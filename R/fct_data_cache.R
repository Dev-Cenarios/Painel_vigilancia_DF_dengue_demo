# R/fct_data_cache.R
# ==================================================================================
# Camada de dados em memoria do painel (compartilhada por todas as sessoes do
# mesmo processo R).
#
# Arquitetura de desempenho:
#
#   DEMONSTRACAO: no lugar do PostgreSQL ha' uma tabela SINAN simulada
#   (R/fct_simulacao.R, gerar_dados_simulados()), que segue o mesmo caminho:
#
#   Tabela simulada --(gerar_dados_simulados)--> DuckDB
#     --(build_app_parquet)--> local/casos.parquet  (so' colunas do app, ja'
#        tratadas, ordenadas por dt_sintomas, com a RA de cada caso
#        pre-calculada -- nada de sf/st_intersects em tempo de sessao)
#
#   Processo R do app (1x por processo, pre-aquecido em run_app(onStart)):
#     casos.parquet -> data.frame em memoria (get_dados())
#     shapes -> sf em EPSG:4326 simplificado (get_dados()$bairros)
#     resultados da visao padrao (ano vigente, filtros padrao) -> cache
#
#   Cada sessao so' filtra indices (vetores inteiros) sobre o data.frame
#   compartilhado; resultados (indices, cards, indicadores por RA, pontos do
#   mapa) ficam num cache de processo (cachem) chaveado por versao dos dados +
#   filtros -- o segundo usuario com o mesmo filtro recebe tudo pronto.
# ==================================================================================

.dengue_env <- new.env(parent = emptyenv())

.dengue_cache <- cachem::cache_mem(max_size = 512 * 1024^2)

local_path <- function(...) {
  file.path(Sys.getenv("PVDENGUE_LOCAL_DIR", "local"), ...)
}

casos_parquet_path <- function() local_path("casos.parquet")

EVOLUCOES <- c(
  "Cura", "Em investigação", "Ignorado",
  "Óbito em investigação", "Óbito pelo agravo", "Óbito por outras causas"
)

EVOLUCAO_CORES <- c(
  "Cura" = "#24d152",
  "Em investigação" = "#AA0DFE",
  "Ignorado" = "#bdbdbd",
  "Óbito em investigação" = "#ff9e42",
  "Óbito pelo agravo" = "#f20f1e",
  "Óbito por outras causas" = "#FE00FA"
)

OBITOS_DENGUE <- c("Óbito pelo agravo", "Óbito em investigação")
CLASSIF_CONFIRMADOS <- c("Dengue", "Dengue com sinais de alarme", "Dengue grave")


# Chave estavel pra comparar codigos de RA vindos do gpkg (double ou texto)
codigo_ra <- function(x) format(x, scientific = FALSE, trim = TRUE)

# ----------------------------------------------------------------------------------
# Cache de resultados (processo inteiro)
# ----------------------------------------------------------------------------------

cached <- function(key_parts, compute) {
  key <- rlang::hash(key_parts)
  value <- .dengue_cache$get(key)
  if (cachem::is.key_missing(value)) {
    value <- compute()
    .dengue_cache$set(key, value)
  }
  value
}

# ----------------------------------------------------------------------------------
# Carga dos dados (1x por processo; recarrega sozinho se o parquet mudar)
# ----------------------------------------------------------------------------------

versao_dados <- function() {
  info <- file.info(casos_parquet_path())
  if (is.na(info$size)) return(NA_character_)
  paste(as.numeric(info$mtime), info$size)
}

ler_parquet <- function(path, ordem = NULL) {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbGetQuery(con, sprintf(
    "SELECT * FROM read_parquet(%s)%s",
    DBI::dbQuoteString(con, path), if (is.null(ordem)) "" else paste(" ORDER BY", ordem)
  ))
}

estratificacao_parquet_path <- function() local_path("estratificacao.parquet")

# Arquivos gerados pela versao atual do ETL (colunas/arquivos que o painel espera)
parquet_atualizado <- function(path) {
  if (!file.exists(estratificacao_parquet_path())) return(FALSE)
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  cols <- DBI::dbGetQuery(
    con, sprintf("DESCRIBE SELECT * FROM read_parquet(%s)", DBI::dbQuoteString(con, path))
  )$column_name
  all(c("ra_code", "ra_origem", "setor_code") %in% cols)
}

# Malha simplificada SEM quebrar a topologia (setores e RAs de exibicao)
#
# Simplificar cada poligono sozinho (st_simplify) faz dois vizinhos cortarem
# a mesma divisa de jeitos diferentes -- aparecem sobreposicoes e frestas.
# Aqui cada divisa e' simplificada UMA vez e compartilhada pelos dois lados:
#   1. fronteiras de todos os setores, quebradas nos encontros de 3+ setores
#      (cada trecho de divisa vira uma linha so');
#   2. Douglas-Peucker em cada trecho (as pontas ficam fixas);
#   3. os poligonos sao remontados a partir dos trechos e devolvidos ao setor
#      que contem cada pedaco.
# As RAs de exibicao sao a uniao dos seus setores (no arquivo original a RA e'
# exatamente a uniao dos setores), entao o contorno da RA casa com os setores.
# ~2 s, uma vez por processo (pre-aquecimento).
MALHA_TOLERANCIA_M <- 6

malha_simplificada <- function() {
  s2_antes <- suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(s2_antes)), add = TRUE)

  setores <- sf::st_read(app_sys("app/www/shapes/setores_df.gpkg"), quiet = TRUE) |>
    sf::st_transform(31985)
  g <- sf::st_geometry(setores)

  bordas <- sf::st_line_merge(sf::st_union(sf::st_boundary(g)))
  trechos <- sf::st_cast(sf::st_sfc(bordas, crs = sf::st_crs(g)), "LINESTRING")
  trechos <- sf::st_simplify(trechos, dTolerance = MALHA_TOLERANCIA_M)

  pedacos <- sf::st_polygonize(sf::st_union(trechos)) |>
    sf::st_collection_extract("POLYGON") |>
    sf::st_cast("POLYGON")
  dono <- vapply(
    sf::st_intersects(sf::st_point_on_surface(pedacos), g),
    function(h) if (length(h)) h[[1]] else NA_integer_, integer(1)
  )
  grupos <- split(seq_along(pedacos), factor(dono, levels = seq_along(g)))
  novas <- lapply(seq_along(g), function(k) {
    p <- grupos[[k]]
    if (length(p) == 1) pedacos[[p]]
    else if (length(p) > 1) sf::st_union(pedacos[p])[[1]]
    else g[[k]]  # (nao acontece na malha atual) mantem o original
  })
  sf::st_geometry(setores) <- sf::st_sfc(novas, crs = sf::st_crs(g))

  # aggregate() de sf une as geometrias de cada grupo
  ras <- stats::aggregate(
    setores["pop"],
    by = list(ra_code = codigo_ra(setores$code_subdistrict)),
    FUN = sum
  )

  list(
    setores = sf::st_transform(setores, 4326),
    ras = sf::st_transform(ras, 4326)
  )
}

# Setores censitarios (Censo 2022): tabela + GeoJSON, enviado ao navegador uma
# vez por sessao (ver inst/app/www/territorio_layer.js)
# (o GeoJSON das RAs e' gerado em carregar_dados(), a partir de carregar_shapes())
carregar_setores <- function(bairros, malha) {
  setores <- malha$setores

  tabela <- data.frame(
    codigo = codigo_ra(setores$code_tract),
    ra_code = codigo_ra(setores$code_subdistrict),
    populacao = setores$pop
  )
  tabela$ra_nome <- bairros$name_subdistrict[match(tabela$ra_code, bairros$ra_code)]

  list(tabela = tabela, geojson = geojson_texto(setores))
}

# GeoJSON (texto) so' com a geometria e o indice da feicao (properties.i),
# para o mapa coropletico em tiles (inst/app/www/territorio_layer.js)
geojson_texto <- function(x) {
  geo <- sf::st_sf(i = seq_len(nrow(x)) - 1L, geometry = sf::st_geometry(x))
  arquivo <- tempfile(fileext = ".geojson")
  on.exit(unlink(arquivo), add = TRUE)
  sf::st_write(geo, arquivo, driver = "GeoJSON", quiet = TRUE,
               layer_options = c("COORDINATE_PRECISION=5", "RFC7946=YES"))
  readChar(arquivo, file.size(arquivo), useBytes = TRUE)
}

carregar_shapes <- function(malha) {
  bairros <- sf::st_read(app_sys("app/www/shapes/bairros_df.gpkg"), quiet = TRUE) |>
    sf::st_transform(4326)
  bairros$ra_code <- codigo_ra(bairros$code_subdistrict)

  # geometria de exibicao: a uniao dos setores simplificados da RA (casa
  # exatamente com a malha de setores; ver malha_simplificada())
  i <- match(bairros$ra_code, malha$ras$ra_code)
  if (!anyNA(i)) sf::st_geometry(bairros) <- sf::st_geometry(malha$ras)[i]

  # Ponto de ancoragem do cluster de cada RA: garantidamente dentro do
  # poligono (centroide pode cair fora em RAs de formato irregular)
  s2_antes <- suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(s2_antes)), add = TRUE)
  ancora <- suppressWarnings(sf::st_coordinates(sf::st_point_on_surface(sf::st_geometry(bairros))))
  bairros$ancora_lng <- ancora[, "X"]
  bairros$ancora_lat <- ancora[, "Y"]

  # Retangulo de cada RA: o clique no cluster enquadra a RA
  caixas <- t(vapply(sf::st_geometry(bairros), function(g) as.numeric(sf::st_bbox(g)), numeric(4)))
  bairros$bbox_w <- caixas[, 1]
  bairros$bbox_s <- caixas[, 2]
  bairros$bbox_e <- caixas[, 3]
  bairros$bbox_n <- caixas[, 4]

  bairros[, c("ra_code", "name_subdistrict", "pop", "ancora_lng", "ancora_lat",
              "bbox_w", "bbox_s", "bbox_e", "bbox_n")]
}

carregar_unidades <- function() {
  if (file.exists(local_path("unidades_cnes.rds"))) {
    readRDS(local_path("unidades_cnes.rds"))
  } else {
    tibble::tibble(
      codigo_cnes = character(), nome_fantasia = character(),
      razao_social = character(), latitude = double(), longitude = double()
    )
  }
}

carregar_dados <- function() {
  path <- casos_parquet_path()

  # Migracao automatica: gera (ou regera) o parquet a partir do dengue.duckdb
  # quando ele nao existe ou foi gerado por uma versao anterior do painel,
  # sem precisar rodar o setup de novo.
  precisa_gerar <- !file.exists(path) || !parquet_atualizado(path)

  # DEMONSTRACAO: sem base local, sorteia a base simulada na primeira
  # abertura (R/fct_simulacao.R) -- nao ha banco de dados por tras
  if (precisa_gerar && !file.exists(local_path("dengue.duckdb"))) {
    gerar_dados_simulados(
      bairros_path = app_sys("app/www/shapes/bairros_df.gpkg"),
      setores_path = app_sys("app/www/shapes/setores_df.gpkg")
    )
    precisa_gerar <- FALSE
  }

  if (precisa_gerar && file.exists(local_path("dengue.duckdb"))) {
    con <- DBI::dbConnect(duckdb::duckdb(), local_path("dengue.duckdb"), read_only = TRUE)
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
    build_app_parquet(
      con, out = path,
      bairros_path = app_sys("app/www/shapes/bairros_df.gpkg"),
      setores_path = app_sys("app/www/shapes/setores_df.gpkg"),
      out_estratificacao = estratificacao_parquet_path()
    )
  }

  if (!file.exists(path)) {
    stop("Base de casos nao encontrada (", path, "). Rode dev/setup_databases.R.")
  }

  versao <- versao_dados()
  casos <- ler_parquet(path, ordem = "id")
  malha <- malha_simplificada()
  bairros <- carregar_shapes(malha)
  setores <- carregar_setores(bairros, malha)

  casos$ra_idx <- match(casos$ra_code, bairros$ra_code)
  casos$setor_idx <- match(casos$setor_code, setores$tabela$codigo)

  # Estratificacao de risco (fixa para a versao dos dados, nao depende dos
  # filtros), alinhada a' ordem das RAs/setores das malhas
  estrat <- if (file.exists(estratificacao_parquet_path())) ler_parquet(estratificacao_parquet_path()) else NULL
  alinhar <- function(nivel, codigos) {
    if (is.null(estrat)) return(NULL)
    e <- estrat[estrat$nivel == nivel, , drop = FALSE]
    e[match(codigos, e$codigo), , drop = FALSE]
  }
  estratificacao <- list(
    ra = alinhar("ra", bairros$ra_code),
    setor = alinhar("setor", setores$tabela$codigo)
  )

  # parquet vem ordenado por dt_sintomas (NULLs no fim): o recorte de periodo
  # vira busca binaria em vez de varrer a base inteira
  n_datados <- sum(!is.na(casos$dt_sintomas))
  dt_ord <- as.numeric(casos$dt_sintomas[seq_len(n_datados)])
  if (is.unsorted(dt_ord)) stop("casos.parquet fora de ordem por dt_sintomas -- regere o parquet.")

  dt_ref <- max(casos$dt_notificacao, na.rm = TRUE)

  list(
    versao = versao,
    casos = casos,
    dt_ord = dt_ord,
    bairros = bairros,
    setores = setores$tabela,
    setores_geojson = setores$geojson,
    bairros_geojson = geojson_texto(bairros),
    estratificacao = estratificacao,
    unidades = carregar_unidades(),
    meta = list(
      dt_max_notificacao = dt_ref,
      n_tot = nrow(casos),
      n_geo = sum(casos$georreferenciado == 1, na.rm = TRUE)
    )
  )
}

#' Dados do painel (memoria do processo). Relê o parquet so' quando ele muda.
#' @noRd
get_dados <- function() {
  atual <- .dengue_env$dados
  if (!is.null(atual) && identical(atual$versao, versao_dados())) {
    return(atual)
  }
  dados <- carregar_dados()
  .dengue_env$dados <- dados
  # resultados antigos nao servem mais pra nova versao dos dados
  .dengue_cache$reset()
  dados
}

# ----------------------------------------------------------------------------------
# Filtros
# ----------------------------------------------------------------------------------

filtros_default <- function() {
  list(
    sexo = filtro_sexo_default,
    idade = filtro_idade_default,
    raca_cor = filtro_raca_cor_default,
    comorbidades = filtro_comorbidades_default,
    populacao_especial = filtro_populacao_especial_default,
    evolucao = filtro_evolucao_default,
    classificacao = filtro_classificacao_default
  )
}

periodo_default <- function(dados) {
  periodo_padrao(dados$meta$dt_max_notificacao)
}

# Periodo padrao do filtro: o ano vigente inteiro -- de 1o de janeiro do ano
# da notificacao mais recente ate' ela
periodo_padrao <- function(fim) {
  c(lubridate::floor_date(fim, "year"), fim)
}

#' Indices (linhas de dados$casos) que passam no periodo + filtros
#' @noRd
calc_idx <- function(dados, periodo, f) {
  cached(list("idx", dados$versao, periodo, f), function() {
    # periodo por busca binaria na parte ordenada por data
    ini <- findInterval(as.numeric(periodo[1]), dados$dt_ord, left.open = TRUE) + 1L
    fim <- findInterval(as.numeric(periodo[2]), dados$dt_ord)
    if (fim < ini) return(integer())
    idx <- ini:fim

    c <- dados$casos
    ok <- c$sexo[idx] %in% f$sexo &
      c$idade[idx] >= f$idade[1] & c$idade[idx] <= f$idade[2] &
      c$raca_cor[idx] %in% f$raca_cor &
      c$evolucao[idx] %in% f$evolucao &
      c$classificacao[idx] %in% f$classificacao

    if (!is.null(f$comorbidades)) {
      ok <- ok & grepl(f$comorbidades, c$comorbidades[idx], fixed = TRUE)
    }
    if (!is.null(f$populacao_especial)) {
      ok <- ok & grepl(f$populacao_especial, c$populacao_especial[idx], fixed = TRUE)
    }

    idx[which(ok)]
  })
}

calc_cards <- function(dados, periodo, f) {
  cached(list("cards", dados$versao, periodo, f), function() {
    idx <- calc_idx(dados, periodo, f)
    c <- dados$casos
    evo <- c$evolucao[idx]
    cls <- c$classificacao[idx]
    inv <- c$situacao[idx] == "Em investigação"
    n_inv <- sum(inv, na.rm = TRUE)
    # Dengue costuma encerrar a investigacao em poucas semanas -- 30 dias aqui
    # cumpre o mesmo papel de alerta que os "6 meses" de TB.
    n_inv_30 <- sum(inv & dados$meta$dt_max_notificacao > (c$dt_sintomas[idx] + 30), na.rm = TRUE)

    list(
      total = length(idx),
      investigacao = n_inv,
      prop_investigacao_30d = if (n_inv > 0) n_inv_30 / n_inv else 0,
      cura = sum(evo == "Cura", na.rm = TRUE),
      obitos = sum(evo %in% OBITOS_DENGUE),
      grave = sum(cls == "Dengue grave", na.rm = TRUE),
      alarme = sum(cls == "Dengue com sinais de alarme", na.rm = TRUE),
      gestante = sum(grepl("Gestante", c$populacao_especial[idx], fixed = TRUE)),
      evolucao = tabulate(match(evo, EVOLUCOES), nbins = length(EVOLUCOES))
    )
  })
}

# Indicadores da estratificacao de risco (metodologia do relatorio): fixos
# para a versao dos dados, nao dependem do periodo nem dos filtros
INDICADORES_ESTRATIFICACAO <- c("estrato", "escore", "I1", "I2", "I3", "I4")

#' Indicador por unidade territorial ("ra" ou "setor"). Indicadores do periodo
#' seguem os filtros e sao contagens via tabulate() sobre a RA/setor de cada
#' caso (pre-calculados no parquet); os da estratificacao vem prontos do ETL.
#' @noRd
calc_ind_territorio <- function(dados, periodo, f, indicador, unidade = "ra") {
  if (indicador %in% INDICADORES_ESTRATIFICACAO) {
    e <- dados$estratificacao[[unidade]]
    if (is.null(e)) stop("Estratificacao nao disponivel -- regere os dados (dev/setup_databases.R).")
    e$ind <- if (indicador == "estrato") e$escore else e[[indicador]]
    return(e)
  }

  cached(list("ind_territorio", dados$versao, periodo, f, indicador, unidade), function() {
    idx <- calc_idx(dados, periodo, f)
    c <- dados$casos
    unidade_idx <- if (unidade == "setor") c$setor_idx else c$ra_idx
    n_unidades <- if (unidade == "setor") nrow(dados$setores) else nrow(dados$bairros)
    conta <- function(sel) tabulate(unidade_idx[idx[sel]], nbins = n_unidades)
    todos <- rep(TRUE, length(idx))
    evo <- c$evolucao[idx]
    cls <- c$classificacao[idx]
    pop <- if (unidade == "setor") dados$setores$populacao else dados$bairros$pop
    pop <- ifelse(!is.na(pop) & pop > 0, pop, NA_real_)

    switch(
      indicador,
      inc = {
        casos <- conta(todos)
        data.frame(casos = casos, ind = 1e5 * casos / pop)
      },
      mort = {
        obitos <- conta(evo %in% OBITOS_DENGUE)
        data.frame(obitos = obitos, ind = 1e5 * obitos / pop)
      },
      let = {
        obitos <- conta(evo %in% OBITOS_DENGUE)
        casos <- conta(todos)
        data.frame(obitos = obitos, casos = casos, ind = obitos / casos)
      },
      prop_cura = {
        curados <- conta(evo == "Cura")
        encerrados <- conta(c$situacao[idx] == "Encerrado")
        data.frame(curados = curados, encerrados = encerrados, ind = curados / encerrados)
      },
      prop_grave = {
        graves <- conta(cls == "Dengue grave")
        confirmados <- conta(cls %in% CLASSIF_CONFIRMADOS)
        data.frame(graves = graves, confirmados = confirmados, ind = graves / confirmados)
      }
    )
  })
}

# Pontos do mapa: TODOS os casos georreferenciados do filtro (sem amostra).
# Vao pro navegador como arrays binarios em base64 (17 bytes por caso) e sao
# agrupados por RA/desenhados por inst/app/www/casos_layer.js -- o JSON do leaflet do
# R levava ~60 s e 57 MB pra base inteira; assim sao ~0,3 s e ~9 MB.
SEXO_CODIGOS <- c("F", "M", "I")

codigo_u8 <- function(x, niveis) {
  i <- match(x, niveis) - 1L
  i[is.na(i)] <- 255L
  jsonlite::base64_enc(as.raw(i))
}

calc_pontos <- function(dados, periodo, f, evolucoes) {
  cached(list("pontos", dados$versao, periodo, f, evolucoes), function() {
    idx <- calc_idx(dados, periodo, f)
    c <- dados$casos
    idx <- idx[c$evolucao[idx] %in% evolucoes & c$georreferenciado[idx] == 1]

    idade <- as.integer(pmin(pmax(floor(c$idade[idx]), 0), 254))
    idade[is.na(idade)] <- 255L

    list(
      n = length(idx),
      lng = jsonlite::base64_enc(writeBin(c$lng[idx], raw(), size = 4, endian = "little")),
      lat = jsonlite::base64_enc(writeBin(c$lat[idx], raw(), size = 4, endian = "little")),
      id = jsonlite::base64_enc(writeBin(as.integer(c$id[idx]), raw(), size = 4, endian = "little")),
      evo = codigo_u8(c$evolucao[idx], EVOLUCOES),
      sexo = codigo_u8(c$sexo[idx], SEXO_CODIGOS),
      raca = codigo_u8(c$raca_cor[idx], filtro_raca_cor_default),
      idade = jsonlite::base64_enc(as.raw(idade)),
      # RA de cada caso (indice em dados$bairros; 255 = RA nao identificada)
      ra = codigo_u8(c$ra_idx[idx], seq_len(nrow(dados$bairros))),
      ras = list(
        nome = dados$bairros$name_subdistrict,
        lng = dados$bairros$ancora_lng,
        lat = dados$bairros$ancora_lat,
        w = dados$bairros$bbox_w,
        s = dados$bairros$bbox_s,
        e = dados$bairros$bbox_e,
        n = dados$bairros$bbox_n
      ),
      nomes = list(evolucao = EVOLUCOES, sexo = SEXO_CODIGOS, raca = filtro_raca_cor_default),
      cores = unname(EVOLUCAO_CORES[EVOLUCOES]),
      evolucoes_presentes = EVOLUCOES[EVOLUCOES %in% c$evolucao[idx]]
    )
  })
}

calc_unidades <- function(dados, periodo, f) {
  cached(list("unidades", dados$versao, periodo, f), function() {
    u <- dados$unidades
    if (nrow(u) == 0) return(u)
    idx <- calc_idx(dados, periodo, f)
    u$n_casos <- tabulate(
      match(dados$casos$cod_unidade_notificadora[idx], u$codigo_cnes),
      nbins = nrow(u)
    )
    u
  })
}

# ----------------------------------------------------------------------------------
# Pre-aquecimento
# ----------------------------------------------------------------------------------

#' Carrega os dados e calcula a visao padrao antes do primeiro usuario chegar
#' @noRd
warm_cache <- function() {
  t0 <- Sys.time()
  ok <- tryCatch({
    dados <- get_dados()
    p <- periodo_default(dados)
    f <- filtros_default()
    calc_cards(dados, p, f)
    calc_ind_territorio(dados, p, f, "inc", "ra")
    calc_pontos(dados, p, f, EVOLUCOES)
    calc_unidades(dados, p, f)
    TRUE
  }, error = function(e) {
    message("[PVDengueDF] Falha ao pre-aquecer o cache: ", conditionMessage(e))
    FALSE
  })
  if (ok) {
    message(sprintf(
      "[PVDengueDF] Cache pre-aquecido em %.1fs (%s casos em memoria)",
      as.numeric(difftime(Sys.time(), t0, units = "secs")),
      format(nrow(.dengue_env$dados$casos), big.mark = ".", decimal.mark = ",")
    ))
  }
  invisible(ok)
}
