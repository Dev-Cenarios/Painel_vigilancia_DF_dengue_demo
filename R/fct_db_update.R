# R/fct_db_update.R
# ==================================================================================
# FUNÇÃO: build_app_parquet()
#
# PROPÓSITO:
#   Fonte única da transformação dos dados brutos (tabela dengue_sinan do DuckDB)
#   para os dois arquivos que o painel carrega em memória:
#     - local/casos.parquet           casos individuais (com RA e setor censitário)
#     - local/estratificacao.parquet  estratificação de risco por setor e por RA
#   Usada por update_dengue_data(), dev/setup_databases.R e pela migração
#   automática em get_dados() (R/fct_data_cache.R), para as rotinas nunca
#   divergirem. Lê direto de dengue_sinan (via dengue_sinan_treated_select()),
#   então funciona com o DuckDB aberto só para leitura.
#
#   "situacao" reflete se o caso já tem EVOLUCAO preenchida (encerrado) ou ainda
#   está em investigação (dengue é doença aguda, evolução em dias/semanas).
# ==================================================================================

build_app_parquet <- function(con,
                              out = file.path("local", "casos.parquet"),
                              bairros_path = bairros_gpkg_path(),
                              setores_path = setores_gpkg_path(),
                              out_estratificacao = file.path(dirname(out), "estratificacao.parquet")) {
  opcionais <- colunas_opcionais(con)
  treated <- dengue_sinan_treated_select(opcionais)

  # Toda a preparacao que antes rodava em cada sessao (conversao de listas em
  # texto, tempo ate encerramento, geometria sf) roda aqui, 1x. So' saem do
  # banco as colunas que o painel usa (nome do paciente, data de nascimento
  # etc. ficam de fora).
  todos <- DBI::dbGetQuery(con, sprintf("
    SELECT
      numero_notificacao,
      dt_sintomas, dt_notificacao, dt_encerramento, dt_obito,
      ano_notificacao,
      idade, sexo, raca_cor,
      coalesce(array_to_string(comorbidades, ', '), '')       AS comorbidades,
      cast(coalesce(n_comorbidades, 0) AS INTEGER)             AS n_comorbidades,
      coalesce(array_to_string(populacao_especial, ', '), '') AS populacao_especial,
      classificacao, evolucao, situacao,
      endereco, bairro, cod_unidade_notificadora, georreferenciado,
      cd_setor_sinan,
      long AS lng,
      lat
    FROM (%s) t", treated))

  # Tempo ate encerramento: casos em aberto contam ate' a notificacao mais
  # recente entre os casos com coordenada (mesma referencia de antes)
  com_coord <- !is.na(todos$lat) & !is.na(todos$lng)
  dt_ref <- max(todos$dt_notificacao[com_coord], na.rm = TRUE)
  todos$tempo_ate_encerramento <- as.integer(ifelse(
    todos$situacao == "Encerrado",
    todos$dt_encerramento - todos$dt_sintomas,
    dt_ref - todos$dt_sintomas
  ))

  territorio <- atribuir_territorios(todos, bairros_path, setores_path)
  todos$ra_code <- territorio$ra_code
  todos$ra_origem <- territorio$ra_origem
  todos$setor_code <- territorio$setor_code
  todos$setor_origem <- territorio$setor_origem

  # Estratificacao usa TODOS os casos com setor (inclusive sem coordenada,
  # quando o CD_SETOR vem da origem) -- mesma base do relatorio
  estratificacao <- calcular_estratificacao(todos, territorio$setores, territorio$ras)

  # Painel: casos com coordenada, ordenados por data dos sintomas (o recorte
  # de periodo vira busca binaria em get_dados())
  casos <- todos[com_coord, setdiff(names(todos), c("cd_setor_sinan", "ano_notificacao")), drop = FALSE]
  casos <- casos[order(casos$dt_sintomas, casos$numero_notificacao, na.last = TRUE), , drop = FALSE]
  casos <- cbind(id = seq_len(nrow(casos)), casos)

  # casos.parquet por ultimo: e' a troca dele que os processos do painel
  # detectam (versao_dados()), e a estratificacao ja' tem que estar pronta
  gravar_parquet(con, estratificacao, out_estratificacao)
  gravar_parquet(con, casos, out)

  invisible(out)
}

# Escreve num arquivo temporario e troca de uma vez so' -- quem estiver lendo
# nunca pega um parquet pela metade.
gravar_parquet <- function(con, df, out) {
  nome <- paste0("tmp_", as.integer(stats::runif(1, 1, 1e9)))
  duckdb::duckdb_register(con, nome, df)
  on.exit(duckdb::duckdb_unregister(con, nome), add = TRUE)
  tmp <- paste0(out, ".tmp")
  DBI::dbExecute(con, sprintf(
    "COPY (SELECT * FROM %s) TO %s (FORMAT parquet, COMPRESSION zstd)",
    nome, DBI::dbQuoteString(con, tmp)
  ))
  if (file.exists(out)) file.remove(out)
  if (!file.rename(tmp, out)) stop("Nao foi possivel gravar ", out)
  invisible(out)
}

# ==================================================================================
# Territorio de cada caso (RA e setor censitario), calculado 1x aqui em vez de
# st_intersects() a cada filtro/indicador em cada sessao.
#
#   RA:    1. poligono da RA onde cai a coordenada ("poligono");
#          2. senao, a RA registrada no SINAN, Regiao_adm ("sinan");
#          3. senao NA -- o painel mostra esses casos num quadro proprio.
#   Setor: 1. CD_SETOR ja' resolvido na origem pela geocodificacao, quando a
#             tabela traz essa coluna ("sinan") -- mesma fonte do relatorio de
#             estratificacao;
#          2. senao, o setor onde cai a coordenada ("poligono");
#          3. senao NA (fica fora da estratificacao, como no relatorio).
# ==================================================================================

atribuir_territorios <- function(casos, bairros_path, setores_path) {
  s2_antes <- sf::sf_use_s2(FALSE)
  on.exit(suppressMessages(sf::sf_use_s2(s2_antes)), add = TRUE)

  bairros <- sf::st_read(bairros_path, quiet = TRUE) |> sf::st_transform(31985)
  setores <- sf::st_read(setores_path, quiet = TRUE) |> sf::st_transform(31985)
  ra_codes <- format(bairros$code_subdistrict, scientific = FALSE, trim = TRUE)
  setor_codes <- format(setores$code_tract, scientific = FALSE, trim = TRUE)

  primeiro <- function(hits) vapply(hits, function(h) if (length(h)) h[[1]] else NA_integer_, integer(1))

  com_coord <- which(!is.na(casos$lat) & !is.na(casos$lng))
  pontos <- sf::st_as_sf(casos[com_coord, c("lng", "lat")], coords = c("lng", "lat"), crs = 4674) |>
    sf::st_transform(31985)

  # RA
  ra_idx <- rep(NA_integer_, nrow(casos))
  ra_idx[com_coord] <- primeiro(sf::st_intersects(pontos, bairros))
  ra_origem <- ifelse(is.na(ra_idx), NA_character_, "poligono")
  ra_sinan <- match(normalizar_nome(casos$bairro), normalizar_nome(bairros$name_subdistrict))
  usa_sinan <- is.na(ra_idx) & !is.na(ra_sinan)
  ra_idx[usa_sinan] <- ra_sinan[usa_sinan]
  ra_origem[usa_sinan] <- "sinan"

  # Setor censitario
  setor_idx <- match(gsub("[^0-9]", "", as.character(casos$cd_setor_sinan)), setor_codes)
  setor_origem <- ifelse(is.na(setor_idx), NA_character_, "sinan")
  falta <- intersect(which(is.na(setor_idx)), com_coord)
  if (length(falta)) {
    setor_idx[falta] <- primeiro(sf::st_intersects(pontos[match(falta, com_coord), ], setores))
    setor_origem[falta] <- ifelse(is.na(setor_idx[falta]), NA_character_, "poligono")
  }

  list(
    ra_code = ra_codes[ra_idx],
    ra_origem = ra_origem,
    setor_code = setor_codes[setor_idx],
    setor_origem = setor_origem,
    setores = data.frame(
      codigo = setor_codes,
      ra_code = format(setores$code_subdistrict, scientific = FALSE, trim = TRUE),
      populacao = setores$pop
    ),
    ras = data.frame(codigo = ra_codes, nome = bairros$name_subdistrict)
  )
}

# ==================================================================================
# Estratificacao de risco -- replica a metodologia do relatorio
# "Estratificação de risco para dengue por setor censitário, DF 2020-2026"
# (scripts 04_indicadores_escore.R e 04c_indicadores_ra.R do projeto
# Dengue_risco), no nivel de setor censitario e de RA:
#
#   Caso provavel = classificacao final diferente de "Descartado" (inclui os
#                   casos ainda em investigacao);
#   Caso grave    = dengue com sinais de alarme OU dengue grave;
#   Obito         = evolucao "Obito pelo agravo" (EVOLUCAO = 2);
#   Ano           = ano de notificacao (NU_ANO; na falta, ano de DT_NOTIFIC);
#   Populacao     = Censo 2022, constante em todos os anos; RA = soma dos setores.
#
#   I1 Magnitude         incidencia media anual de casos provaveis     peso 0,30
#   I2 Recorrencia       n. de anos com incidencia >= 300/100 mil        peso 0,20
#   I3 Situacao recente  incidencia media dos 2 anos mais recentes       peso 0,30
#   I4 Gravidade         incidencia media anual de casos graves          peso 0,20
#
#   Cada indicador normalizado 0-1 (min-max) -> escore ponderado -> estratos
#   pelos quartis do escore (Baixo/Medio/Alto/Muito alto), separadamente para
#   setor e para RA. Calculo bruto, sem suavizacao (decisao do relatorio).
# ==================================================================================

ESTRATIFICACAO_ANO_INICIAL <- 2020
ESTRATIFICACAO_LIMIAR_RECORRENCIA <- 300
ESTRATIFICACAO_PESOS <- c(I1 = 0.30, I2 = 0.20, I3 = 0.30, I4 = 0.20)
ESTRATOS <- c("Baixo", "Médio", "Alto", "Muito alto")

calcular_estratificacao <- function(casos, setores, ras) {
  ano <- casos$ano_notificacao
  anos <- ESTRATIFICACAO_ANO_INICIAL:max(ano, na.rm = TRUE)
  recentes <- utils::tail(anos, 2)

  provavel <- casos$classificacao != "Descartado"
  grave <- casos$classificacao %in% c("Dengue com sinais de alarme", "Dengue grave")
  obito <- casos$evolucao == "Óbito pelo agravo"

  setor_idx <- match(casos$setor_code, setores$codigo)
  ok <- provavel & !is.na(setor_idx) & !is.na(ano) & ano %in% anos
  n_s <- nrow(setores)
  n_a <- length(anos)
  chave <- (setor_idx[ok] - 1L) * n_a + (ano[ok] - anos[1] + 1L)
  por_ano <- function(sel) matrix(tabulate(chave[sel], nbins = n_s * n_a), nrow = n_s, byrow = TRUE)

  casos_sa <- por_ano(rep(TRUE, sum(ok)))
  graves_sa <- por_ano(grave[ok])
  obitos_sa <- por_ano(obito[ok])

  nivel_setor <- indicadores_estratificacao(casos_sa, graves_sa, obitos_sa, setores$populacao, anos, recentes)
  nivel_setor <- cbind(
    data.frame(
      nivel = "setor",
      codigo = setores$codigo,
      nome = paste("Setor", setores$codigo),
      ra_nome = ras$nome[match(setores$ra_code, ras$codigo)]
    ),
    nivel_setor
  )

  # RA = soma dos setores da RA (script 04c)
  grupo <- factor(setores$ra_code, levels = ras$codigo)
  soma <- function(m) rowsum(m, grupo, reorder = TRUE)[levels(grupo), , drop = FALSE]
  pop_ra <- as.numeric(tapply(setores$populacao, grupo, sum, na.rm = TRUE))
  nivel_ra <- indicadores_estratificacao(soma(casos_sa), soma(graves_sa), soma(obitos_sa), pop_ra, anos, recentes)
  nivel_ra <- cbind(
    data.frame(nivel = "ra", codigo = ras$codigo, nome = ras$nome, ra_nome = ras$nome),
    nivel_ra
  )

  resultado <- rbind(nivel_setor, nivel_ra)
  resultado$ano_inicial <- anos[1]
  resultado$ano_final <- utils::tail(anos, 1)
  resultado$anos_recentes <- paste(recentes, collapse = " e ")
  resultado
}

indicadores_estratificacao <- function(casos_sa, graves_sa, obitos_sa, populacao, anos, recentes) {
  pop <- ifelse(!is.na(populacao) & populacao > 0, populacao, NA_real_)
  inc <- casos_sa / pop * 1e5
  inc_grave <- graves_sa / pop * 1e5

  ind <- data.frame(
    populacao = populacao,
    casos_totais = rowSums(casos_sa),
    graves_totais = rowSums(graves_sa),
    obitos_totais = rowSums(obitos_sa),
    I1 = rowMeans(inc),
    I2 = rowSums(inc >= ESTRATIFICACAO_LIMIAR_RECORRENCIA, na.rm = TRUE),
    I3 = rowMeans(inc[, anos %in% recentes, drop = FALSE]),
    I4 = rowMeans(inc_grave)
  )

  normalizar <- function(x) {
    faixa <- range(x, na.rm = TRUE)
    if (!all(is.finite(faixa)) || diff(faixa) == 0) return(rep(NA_real_, length(x)))
    (x - faixa[1]) / diff(faixa)
  }
  ind$e1 <- normalizar(ind$I1)
  ind$e2 <- normalizar(ind$I2)
  ind$e3 <- normalizar(ind$I3)
  ind$e4 <- normalizar(ind$I4)
  p <- ESTRATIFICACAO_PESOS
  ind$escore <- p[["I1"]] * ind$e1 + p[["I2"]] * ind$e2 + p[["I3"]] * ind$e3 + p[["I4"]] * ind$e4

  # Quartis do escore -> estratos. Equivale ao cut(breaks = quartis,
  # include.lowest = TRUE) do relatorio (intervalos fechados a direita), mas
  # nao quebra quando dois quartis coincidem (muitos escores iguais).
  quartis <- stats::quantile(ind$escore, probs = c(0.25, 0.5, 0.75), na.rm = TRUE, names = FALSE)
  estrato <- ESTRATOS[findInterval(ind$escore, quartis, left.open = TRUE) + 1L]
  estrato[is.na(ind$escore)] <- "Sem dados"
  ind$estrato <- estrato
  ind
}

# "Águas Claras" / "AGUAS CLARAS" / "aguas  claras" -> "AGUAS CLARAS"
normalizar_nome <- function(x) {
  x <- toupper(trimws(x))
  x <- chartr("ÁÀÂÃÄÉÈÊËÍÌÎÏÓÒÔÕÖÚÙÛÜÇÑ", "AAAAAEEEEIIIIOOOOOUUUUCN", x)
  trimws(gsub("\\s+", " ", gsub("[^A-Z0-9 ]", " ", x)))
}

bairros_gpkg_path <- function() {
  p <- system.file("app/www/shapes/bairros_df.gpkg", package = "PVDengueDF")
  if (nzchar(p)) p else file.path("inst", "app", "www", "shapes", "bairros_df.gpkg")
}

setores_gpkg_path <- function() {
  p <- system.file("app/www/shapes/setores_df.gpkg", package = "PVDengueDF")
  if (nzchar(p)) p else file.path("inst", "app", "www", "shapes", "setores_df.gpkg")
}

# ==================================================================================
# FUNÇÃO: dengue_sinan_treated_select() / dengue_sinan_treated_sql()
#
# PROPÓSITO:
#   Fonte única da transformação SQL de dengue_sinan (dados brutos do SINAN, como
#   chegam do PostgreSQL) para os dados tratados usados pelo app. Usada tanto por
#   update_dengue_data() quanto por dev/setup_databases.R e build_app_parquet(),
#   para que as rotinas nunca divirjam na forma como os dados são tratados.
#
#   Campos obrigatórios em EXPECTED_DENGUE_COLS (abaixo); CD_SETOR e NU_ANO são
#   opcionais (usados na estratificação quando existirem -- ver
#   colunas_opcionais()). Se a tabela real usar nomes diferentes, ajuste aqui --
#   update_dengue_data() falha com uma mensagem clara se alguma coluna esperada
#   não vier do Postgres, em vez de deixar o DuckDB estourar um "Binder Error".
#
#   Códigos conferidos contra o Dicionário de Dados do SINAN Online
#   (Dengue/Chikungunya, DATASUS, rev. 21/10/2015):
#     CLASSI_FIN  5 Descartado, 8 Inconclusivo, 10 Dengue, 11 Dengue com sinais
#                 de alarme, 12 Dengue grave (os códigos antigos 1/2/3 do SINAN
#                 Net continuam aceitos, mapeados para as mesmas categorias);
#                 vazio = ainda em investigação.
#     EVOLUCAO    1 Cura, 2 Óbito pelo agravo, 3 Óbito por outras causas,
#                 4 Óbito em investigação, 9 Ignorado; vazio = em investigação.
# ==================================================================================

# Colunas que dengue_sinan_treated_select() referencia -- checadas contra o
# resultado do SELECT * antes de rodar a transformação (ver update_dengue_data()).
EXPECTED_DENGUE_COLS <- c(
  "NU_NOTIFIC", "DT_SIN_PRI", "DT_NOTIFIC", "DT_ENCERRA", "DT_OBITO",
  "NM_PACIENT", "DT_NASC", "NU_IDADE_N", "CS_SEXO", "CS_RACA", "CS_GESTANT",
  "HOSPITALIZ", "DIABETES", "HEMATOLOG", "HEPATOPAT", "RENAL", "HIPERTENSA",
  "ACIDO_PEPT", "AUTO_IMUNE", "CLASSI_FIN", "EVOLUCAO", "address",
  "ENDERECO_CNEFE", "Regiao_adm", "NM_LOGRADO", "NM_COMPLEM", "ID_UNIDADE",
  "LATITUDE", "LONGITUDE"
)

OPTIONAL_DENGUE_COLS <- c("CD_SETOR", "NU_ANO")

colunas_opcionais <- function(con, tabela = "dengue_sinan") {
  intersect(OPTIONAL_DENGUE_COLS, DBI::dbListFields(con, tabela))
}

dengue_sinan_treated_sql <- function(opcionais = character()) {
  sprintf("create or replace table dengue_sinan_treated as (%s);", dengue_sinan_treated_select(opcionais))
}

dengue_sinan_treated_select <- function(opcionais = character()) {
  cd_setor <- if ("CD_SETOR" %in% opcionais) "cast(CD_SETOR as varchar)" else "cast(null as varchar)"
  ano <- if ("NU_ANO" %in% opcionais) {
    "coalesce(try_cast(NU_ANO as integer), year(try_cast(DT_NOTIFIC as date)))"
  } else {
    "year(try_cast(DT_NOTIFIC as date))"
  }

  sprintf("
  select
  NU_NOTIFIC as numero_notificacao,
  try_cast(DT_SIN_PRI as date) as dt_sintomas,
  try_cast(DT_NOTIFIC as date) as dt_notificacao,
  try_cast(DT_ENCERRA as date) as dt_encerramento,
  try_cast(DT_OBITO as date) as dt_obito,
  %s as ano_notificacao,
  nm_pacient as nome_paciente,
  try_cast(DT_NASC as date) as dt_nasc,
  case
    when left(cast(NU_IDADE_N as varchar), 1) = '1' then cast(right(cast(NU_IDADE_N as varchar), 3) as integer)/8760
    when left(cast(NU_IDADE_N as varchar), 1) = '2' then cast(right(cast(NU_IDADE_N as varchar), 3) as integer)/365
    when left(cast(NU_IDADE_N as varchar), 1) = '3' then cast(right(cast(NU_IDADE_N as varchar), 3) as integer)/12
    when left(cast(NU_IDADE_N as varchar), 1) = '4' then cast(right(cast(NU_IDADE_N as varchar), 3) as integer)
  end as idade,
  cs_sexo as sexo,
  case
    when CS_RACA = 1 then 'Branca'
    when CS_RACA = 2 then 'Preta'
    when CS_RACA = 3 then 'Amarela'
    when CS_RACA = 4 then 'Parda'
    when CS_RACA = 5 then 'Indígena'
    when CS_RACA = 9 then 'Ignorado'
  end as raca_cor,
  -- Condicoes e comorbidades da ficha de Dengue (equivalente aos AGRAVXXX de TB)
  list_sort(
    list_distinct(
      [
        case when DIABETES = 1 then 'Diabetes' else null end,
        case when HEMATOLOG = 1 then 'Doenças hematológicas' else null end,
        case when HEPATOPAT = 1 then 'Hepatopatias' else null end,
        case when RENAL = 1 then 'Doença renal' else null end,
        case when HIPERTENSA = 1 then 'Hipertensão arterial' else null end,
        case when ACIDO_PEPT = 1 then 'Doença acidopéptica' else null end,
        case when AUTO_IMUNE = 1 then 'Doenças autoimunes' else null end
      ]
    )
  ) as comorbidades,
  array_length(comorbidades) as n_comorbidades,
  -- Populacoes de atencao especial (equivalente aos POP_XXX de TB, adaptado:
  -- dengue nao tem POP_LIBER/POP_RUA no mesmo sentido de TB, mas gestante e
  -- hospitalizacao sao os marcadores de risco relevantes na ficha de Dengue)
  list_sort(
    list_distinct(
      [
        case when CS_GESTANT in (1,2,3,4) then 'Gestante' else null end,
        case when HOSPITALIZ = 1 then 'Hospitalizado' else null end
      ]
    )
  ) as populacao_especial,
  case
    when CLASSI_FIN in (10, 1) then 'Dengue'
    when CLASSI_FIN in (11, 2) then 'Dengue com sinais de alarme'
    when CLASSI_FIN in (12, 3) then 'Dengue grave'
    when CLASSI_FIN = 5 then 'Descartado'
    when CLASSI_FIN = 8 then 'Inconclusivo'
    when CLASSI_FIN is null then 'Em investigação'
    else 'Ignorado'
  end as classificacao,
  case
    when EVOLUCAO = 1 then 'Cura'
    when EVOLUCAO = 2 then 'Óbito pelo agravo'
    when EVOLUCAO = 3 then 'Óbito por outras causas'
    when EVOLUCAO = 4 then 'Óbito em investigação'
    when EVOLUCAO = 9 then 'Ignorado'
    else 'Em investigação'
  end as evolucao,
  -- Dengue nao tem tratamento longo como TB: 'situacao' aqui marca se o caso
  -- ja tem evolucao definida (encerrado) ou ainda esta em investigacao.
  case
    when EVOLUCAO is null then 'Em investigação'
    else 'Encerrado'
  end as situacao,
  address as endereco,
  ENDERECO_CNEFE as endereco_cnefe,
  Regiao_adm as bairro,
  trim(cast(ID_UNIDADE as varchar)) as cod_unidade_notificadora,
  %s as cd_setor_sinan,
  case
  when
    (NM_LOGRADO is null OR NM_LOGRADO = '')
    and (NM_COMPLEM is null or NM_COMPLEM = '')
    then 0
    when
      (regexp_matches(NM_LOGRADO, '\\bSITUACAO\\b', 'i')
      or regexp_matches(NM_LOGRADO, '\\bMORADOR\\b', 'i')
      or regexp_matches(NM_LOGRADO, '\\bDE RUA\\b', 'i')
      or regexp_matches(NM_LOGRADO, '\\bSEM ENDERECO\\b', 'i'))
    then 0
    else 1
    end as georreferenciado,
  LATITUDE AS lat,
  LONGITUDE AS long
  from dengue_sinan", ano, cd_setor)
}

# ==================================================================================
# FUNÇÃO: update_dengue_data() -- VERSAO DE DEMONSTRACAO
#
#   No painel original, baixa a tabela do PostgreSQL. Aqui nao ha banco: o
#   botao "Atualizar dados" sorteia uma NOVA base simulada (outra semente) e
#   regrava os arquivos de local/, exercitando o mesmo fluxo de recarga.
# ==================================================================================

update_dengue_data <- function() {
  tryCatch({
    n <- gerar_dados_simulados(
      seed = sample.int(1e6, 1),
      bairros_path = app_sys("app/www/shapes/bairros_df.gpkg"),
      setores_path = app_sys("app/www/shapes/setores_df.gpkg")
    )
    list(
      success = TRUE,
      message = paste0("✅ Nova base simulada gerada: ", format(n, big.mark = ".", decimal.mark = ","), " notificações fictícias."),
      records_updated = n
    )
  }, error = function(e) {
    list(success = FALSE, message = paste0("❌ Erro ao gerar dados simulados: ", conditionMessage(e)), records_updated = 0)
  })
}
