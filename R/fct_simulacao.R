# R/fct_simulacao.R
# ==================================================================================
# DADOS SIMULADOS -- versao de demonstracao (showcase) do painel.
#
# Nenhum dado real de notificacao e' usado aqui. Esta rotina INVENTA uma tabela
# no mesmo formato da tabela bruta do SINAN geocodificada
# (dados_geolink."DF_geo_Dengue" no painel original), com as mesmas colunas e
# codigos do Dicionario de Dados do SINAN Online, e passa pelo MESMO ETL do
# painel original (dengue_sinan_treated_select() + build_app_parquet()).
#
# Tudo e' sorteado com semente fixa:
#   - quantidade de casos por ano e curva sazonal (pico em marco/abril);
#   - setor censitario de cada caso, com probabilidade proporcional a'
#     populacao do Censo 2022 (malha publica do IBGE) x um "risco" ficticio
#     por RA e por setor;
#   - coordenada: ponto aleatorio dentro do setor sorteado (nao e' endereco);
#   - idade, sexo, raca/cor, comorbidades, gestacao, hospitalizacao,
#     classificacao final e evolucao, com proporcoes plausiveis mas inventadas;
#   - nome, endereco e unidades notificadoras sao textos ficticios.
#
# Os numeros resultantes NAO representam a situacao epidemiologica real do DF.
# ==================================================================================

SIMULACAO_SEMENTE <- 2026

# Casos notificados por ano (ficticios). O ultimo ano e' parcial: vai ate' a
# data de referencia.
SIMULACAO_CASOS_ANO <- c(
  "2020" = 28000, "2021" = 14000, "2022" = 42000, "2023" = 24000,
  "2024" = 65000, "2025" = 16000, "2026" = 22000
)

#' Gera a base simulada completa em `dir` (dengue.duckdb, casos.parquet,
#' estratificacao.parquet e unidades_cnes.rds)
#' @noRd
gerar_dados_simulados <- function(dir = local_path(),
                                  seed = SIMULACAO_SEMENTE,
                                  dt_ref = Sys.Date() - 3,
                                  bairros_path = bairros_gpkg_path(),
                                  setores_path = setores_gpkg_path()) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  t0 <- Sys.time()
  message("[PVDengueDF demo] Gerando dados simulados (semente ", seed, ")...")

  sim <- simular_sinan(seed = seed, dt_ref = as.Date(dt_ref), setores_path = setores_path)

  # unidades notificadoras ficticias (mesmo formato do cache do CNES)
  saveRDS(sim$unidades, file.path(dir, "unidades_cnes.rds"))

  db <- file.path(dir, "dengue.duckdb")
  if (file.exists(db)) file.remove(db)
  con <- DBI::dbConnect(duckdb::duckdb(), db)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  DBI::dbWriteTable(con, "dengue_sinan", sim$sinan, overwrite = TRUE)
  DBI::dbExecute(con, dengue_sinan_treated_sql(colunas_opcionais(con)))

  build_app_parquet(
    con,
    out = file.path(dir, "casos.parquet"),
    bairros_path = bairros_path,
    setores_path = setores_path,
    out_estratificacao = file.path(dir, "estratificacao.parquet")
  )

  message(sprintf(
    "[PVDengueDF demo] %s notificacoes simuladas em %.0fs",
    format(nrow(sim$sinan), big.mark = ".", decimal.mark = ","),
    as.numeric(difftime(Sys.time(), t0, units = "secs"))
  ))
  invisible(nrow(sim$sinan))
}

#' Tabela bruta simulada no formato do SINAN + unidades ficticias
#' @noRd
simular_sinan <- function(seed = SIMULACAO_SEMENTE,
                          dt_ref = Sys.Date() - 3,
                          setores_path = setores_gpkg_path(),
                          casos_ano = SIMULACAO_CASOS_ANO) {
  set.seed(seed)
  s2_antes <- suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(s2_antes)), add = TRUE)

  setores <- sf::st_read(setores_path, quiet = TRUE)
  setores <- setores[!is.na(setores$pop) & setores$pop > 0, ]
  ra_cod <- format(setores$code_subdistrict, scientific = FALSE, trim = TRUE)
  ras <- unique(data.frame(ra_code = ra_cod, ra_nome = setores$name_subdistrict))

  # ---------------------------------------------------------------- tempo ----
  anos <- as.integer(names(casos_ano))
  ano_ref <- as.integer(format(dt_ref, "%Y"))
  anos <- anos[anos <= ano_ref]
  casos_ano <- casos_ano[as.character(anos)]

  dt_sintomas <- do.call(c, lapply(seq_along(anos), function(k) {
    ano <- anos[k]
    dias <- seq(as.Date(sprintf("%d-01-01", ano)), as.Date(sprintf("%d-12-31", ano)), by = "day")
    if (ano == ano_ref) dias <- dias[dias <= dt_ref - 2]
    # curva sazonal: pico entre marco e abril (dia ~95), com um "vale" no
    # inverno seco; a largura do pico varia de ano a ano
    pico <- stats::rnorm(1, 95, 12)
    largura <- stats::runif(1, 32, 48)
    doy <- as.integer(format(dias, "%j"))
    peso <- exp(-0.5 * ((doy - pico) / largura)^2) + 0.04 + 0.03 * exp(-0.5 * ((doy - 340) / 25)^2)
    # o ano parcial so' recebe a fracao da curva que ja' passou
    n <- if (ano == ano_ref) {
      doy_todos <- seq_len(365)
      peso_todos <- exp(-0.5 * ((doy_todos - pico) / largura)^2) + 0.04 + 0.03 * exp(-0.5 * ((doy_todos - 340) / 25)^2)
      round(casos_ano[[k]] * sum(peso) / sum(peso_todos))
    } else {
      casos_ano[[k]]
    }
    sample(dias, n, replace = TRUE, prob = peso)
  }))
  n <- length(dt_sintomas)
  ano_caso <- as.integer(format(dt_sintomas, "%Y"))

  # ------------------------------------------------------------- espaco ----
  # risco ficticio: por RA (fixo), por RA x ano (recorrencia) e por setor
  risco_ra <- stats::setNames(stats::rlnorm(nrow(ras), 0, 0.55), ras$ra_code)
  risco_setor <- stats::rlnorm(nrow(setores), 0, 0.6)
  setor_idx <- integer(n)
  for (ano in unique(ano_caso)) {
    sel <- which(ano_caso == ano)
    risco_ano <- stats::setNames(stats::rlnorm(nrow(ras), 0, 0.45), ras$ra_code)
    prob <- setores$pop * risco_setor * risco_ra[ra_cod] * risco_ano[ra_cod]
    setor_idx[sel] <- sample.int(nrow(setores), length(sel), replace = TRUE, prob = prob)
  }

  # ponto aleatorio dentro do setor sorteado
  xy <- pontos_nos_setores(sf::st_transform(sf::st_geometry(setores), 31985), setor_idx)
  lng <- xy[, 1]
  lat <- xy[, 2]

  # parte dos casos sem endereco/geocodificacao (como na vida real)
  sem_endereco <- stats::runif(n) < 0.03
  sem_coord <- sem_endereco | stats::runif(n) < 0.02 | is.na(lng)
  lng[sem_coord] <- NA_real_
  lat[sem_coord] <- NA_real_
  # sem coordenada, parte ainda tem o setor resolvido na origem
  cd_setor <- format(setores$code_tract[setor_idx], scientific = FALSE, trim = TRUE)
  cd_setor[sem_coord & stats::runif(n) < 0.5] <- NA_character_

  ra_caso <- setores$name_subdistrict[setor_idx]

  # ------------------------------------------------------- demografia ----
  sexo <- sample(c("F", "M", "I"), n, replace = TRUE, prob = c(0.545, 0.453, 0.002))
  idade_anos <- pmin(pmax(round(stats::rgamma(n, shape = 2.6, scale = 14)), 0), 99)
  bebe <- idade_anos == 0
  nu_idade <- ifelse(bebe, 3000 + sample(1:11, n, replace = TRUE), 4000 + idade_anos)
  dt_nasc <- dt_sintomas - round(idade_anos * 365.25 + ifelse(bebe, (nu_idade - 3000) * 30, stats::runif(n, 0, 364)))

  raca <- sample(c(1, 2, 3, 4, 5, 9), n, replace = TRUE, prob = c(0.34, 0.09, 0.012, 0.46, 0.003, 0.095))

  fertil <- sexo == "F" & idade_anos >= 15 & idade_anos <= 45
  gestant <- ifelse(fertil, ifelse(stats::runif(n) < 0.045, sample(1:4, n, replace = TRUE), 5), 6)

  p_com <- function(base) pmin(base * (0.2 + idade_anos / 35), 0.6)
  sim_nao <- function(p) ifelse(stats::runif(n) < p, 1, 2)
  diabetes <- sim_nao(p_com(0.035))
  hipertensa <- sim_nao(p_com(0.06))
  hematolog <- sim_nao(rep(0.004, n))
  hepatopat <- sim_nao(rep(0.003, n))
  renal <- sim_nao(p_com(0.006))
  acido_pept <- sim_nao(rep(0.006, n))
  auto_imune <- sim_nao(rep(0.004, n))
  n_com <- (diabetes == 1) + (hipertensa == 1) + (hematolog == 1) + (hepatopat == 1) +
    (renal == 1) + (acido_pept == 1) + (auto_imune == 1)

  # ------------------------------------------- notificacao e desfecho ----
  dt_notific <- pmin(dt_sintomas + stats::rgeom(n, 0.35), dt_ref)
  dias_desde <- as.numeric(dt_ref - dt_notific)
  # quanto mais recente, maior a chance de ainda estar em investigacao
  p_aberto <- 0.015 + 0.85 * exp(-dias_desde / 25)
  aberto <- stats::runif(n) < p_aberto

  # risco de gravidade cresce com idade e comorbidades
  fator <- (1 + (idade_anos >= 60) * 1.8 + (idade_anos < 2) * 0.8) * (1 + 0.7 * n_com)
  u <- stats::runif(n)
  p_grave <- 0.004 * fator
  p_alarme <- 0.05 * fator
  p_desc <- 0.27
  p_inc <- 0.015
  classi <- ifelse(u < p_desc, 5,
            ifelse(u < p_desc + p_inc, 8,
            ifelse(u < p_desc + p_inc + p_grave, 12,
            ifelse(u < p_desc + p_inc + p_grave + p_alarme, 11, 10))))

  hospitaliz <- ifelse(
    classi == 12 | (classi == 11 & stats::runif(n) < 0.6) | (classi == 10 & stats::runif(n) < 0.015),
    1, 2
  )

  v <- stats::runif(n)
  p_obito <- ifelse(classi == 12, 0.12, ifelse(classi == 11, 0.006, ifelse(classi == 10, 0.00004, 0)))
  evolucao <- ifelse(v < p_obito, 2,
              ifelse(v < p_obito + 0.0006, 3,
              ifelse(v < p_obito + 0.0006 + p_obito * 0.25, 4,
              ifelse(stats::runif(n) < 0.035, 9, 1))))

  dt_encerra <- dt_notific + pmax(1, round(stats::rgamma(n, shape = 2, scale = 10)))
  dt_encerra <- pmin(dt_encerra, dt_ref)
  morte <- evolucao %in% c(2, 3, 4)
  dt_obito <- as.Date(ifelse(morte, as.numeric(dt_sintomas + sample(3:12, n, replace = TRUE)), NA), origin = "1970-01-01")
  dt_obito <- pmin(dt_obito, dt_ref)

  # casos em aberto: sem evolucao, sem encerramento e (quase sempre) sem
  # classificacao final
  evolucao[aberto] <- NA
  dt_encerra[aberto] <- NA
  dt_obito[aberto] <- NA
  classi[aberto & stats::runif(n) < 0.9] <- NA

  # -------------------------------------------------- unidades ficticias ----
  unidades <- simular_unidades(setores, ras, ra_cod)
  # cada caso e' notificado por uma unidade da propria RA (ou, as vezes, de
  # outra RA qualquer -- hospitais de referencia)
  por_ra <- split(seq_len(nrow(unidades)), unidades$ra_code)
  ra_do_caso <- ra_cod[setor_idx]
  id_unidade <- character(n)
  for (r in names(por_ra)) {
    sel <- which(ra_do_caso == r)
    id_unidade[sel] <- unidades$codigo_cnes[por_ra[[r]]][sample.int(length(por_ra[[r]]), length(sel), replace = TRUE)]
  }
  ref <- which(grepl("^Hospital", unidades$nome_fantasia))
  troca <- stats::runif(n) < 0.12
  id_unidade[troca] <- unidades$codigo_cnes[sample(ref, sum(troca), replace = TRUE)]

  # ---------------------------------------------------------- tabela ----
  logradouro <- ifelse(sem_endereco, "SEM ENDERECO", paste("Logradouro fictício", sample(1:999, n, replace = TRUE)))
  endereco <- ifelse(sem_endereco, NA_character_, paste0(logradouro, ", ", ra_caso, " (endereço simulado)"))

  sinan <- data.frame(
    NU_NOTIFIC = sprintf("%07d", seq_len(n)),
    NU_ANO = ano_caso,
    DT_SIN_PRI = dt_sintomas,
    DT_NOTIFIC = dt_notific,
    DT_ENCERRA = dt_encerra,
    DT_OBITO = dt_obito,
    NM_PACIENT = sprintf("PACIENTE SIMULADO %07d", seq_len(n)),
    DT_NASC = dt_nasc,
    NU_IDADE_N = as.integer(nu_idade),
    CS_SEXO = sexo,
    CS_RACA = as.integer(raca),
    CS_GESTANT = as.integer(gestant),
    HOSPITALIZ = as.integer(hospitaliz),
    DIABETES = as.integer(diabetes),
    HEMATOLOG = as.integer(hematolog),
    HEPATOPAT = as.integer(hepatopat),
    RENAL = as.integer(renal),
    HIPERTENSA = as.integer(hipertensa),
    ACIDO_PEPT = as.integer(acido_pept),
    AUTO_IMUNE = as.integer(auto_imune),
    CLASSI_FIN = as.integer(classi),
    EVOLUCAO = as.integer(evolucao),
    address = endereco,
    ENDERECO_CNEFE = endereco,
    Regiao_adm = ra_caso,
    NM_LOGRADO = logradouro,
    NM_COMPLEM = "",
    ID_UNIDADE = id_unidade,
    CD_SETOR = cd_setor,
    LATITUDE = lat,
    LONGITUDE = lng,
    stringsAsFactors = FALSE
  )
  sinan <- sinan[order(sinan$DT_NOTIFIC, sinan$NU_NOTIFIC), ]
  sinan$NU_NOTIFIC <- sprintf("%07d", seq_len(n))
  rownames(sinan) <- NULL

  list(sinan = sinan, unidades = unidades)
}

# Um ponto aleatorio dentro do setor de cada caso (amostragem por rejeicao,
# vetorizada: sorteia no retangulo do setor e fica com os que caem dentro --
# muito mais rapido que sf::st_sample() poligono a poligono). Devolve matriz
# lng/lat (SIRGAS 2000, EPSG 4674).
pontos_nos_setores <- function(geo, setor_idx, max_rodadas = 40) {
  caixas <- t(vapply(geo, function(g) as.numeric(sf::st_bbox(g)), numeric(4)))
  n <- length(setor_idx)
  x <- y <- rep(NA_real_, n)
  falta <- seq_len(n)
  for (r in seq_len(max_rodadas)) {
    if (!length(falta)) break
    cx <- caixas[setor_idx[falta], , drop = FALSE]
    px <- stats::runif(length(falta), cx[, 1], cx[, 3])
    py <- stats::runif(length(falta), cx[, 2], cx[, 4])
    cand <- sf::st_as_sf(data.frame(x = px, y = py), coords = c("x", "y"), crs = sf::st_crs(geo))
    hits <- sf::st_intersects(cand, geo)
    dentro <- vapply(seq_along(falta), function(k) setor_idx[falta[k]] %in% hits[[k]], logical(1))
    x[falta[dentro]] <- px[dentro]
    y[falta[dentro]] <- py[dentro]
    falta <- falta[!dentro]
  }
  if (length(falta)) {
    # setores muito finos: usa um ponto garantidamente interno
    sup <- sf::st_coordinates(suppressWarnings(sf::st_point_on_surface(geo[setor_idx[falta]])))
    x[falta] <- sup[, 1]
    y[falta] <- sup[, 2]
  }
  pts <- sf::st_as_sf(data.frame(x = x, y = y), coords = c("x", "y"), crs = sf::st_crs(geo))
  sf::st_coordinates(sf::st_transform(pts, 4674))
}

# Unidades notificadoras ficticias: ~2 UBS por RA (mais nas RAs populosas) e
# alguns hospitais de referencia. Codigos "SIM...", que nao existem no CNES.
simular_unidades <- function(setores, ras, ra_cod) {
  pop_ra <- tapply(setores$pop, ra_cod, sum)
  geo <- sf::st_transform(sf::st_geometry(setores), 31985)
  linhas <- list()
  k <- 0
  for (i in seq_len(nrow(ras))) {
    r <- ras$ra_code[i]
    n_ubs <- max(1, min(6, round(pop_ra[[r]] / 70000)))
    idx <- which(ra_cod == r)
    escolhidos <- idx[sample.int(length(idx), n_ubs, replace = length(idx) < n_ubs, prob = setores$pop[idx])]
    for (j in seq_along(escolhidos)) {
      k <- k + 1
      linhas[[k]] <- data.frame(
        setor = escolhidos[j], ra_code = r,
        nome_fantasia = sprintf("UBS Fictícia %d - %s", j, ras$ra_nome[i])
      )
    }
  }
  ubs <- do.call(rbind, linhas)
  top <- names(sort(pop_ra, decreasing = TRUE))[1:6]
  hosp <- do.call(rbind, lapply(seq_along(top), function(j) {
    idx <- which(ra_cod == top[j])
    data.frame(
      setor = idx[sample.int(length(idx), 1, prob = setores$pop[idx])], ra_code = top[j],
      nome_fantasia = sprintf("Hospital Fictício de Referência %d - %s", j, ras$ra_nome[ras$ra_code == top[j]])
    )
  }))
  u <- rbind(ubs, hosp)
  pts <- suppressMessages(sf::st_point_on_surface(geo[u$setor]))
  xy <- sf::st_coordinates(sf::st_transform(pts, 4326))
  tibble::tibble(
    codigo_cnes = sprintf("SIM%04d", seq_len(nrow(u))),
    nome_fantasia = u$nome_fantasia,
    razao_social = u$nome_fantasia,
    latitude = xy[, "Y"],
    longitude = xy[, "X"],
    habilitado = TRUE,
    ra_code = u$ra_code
  )
}
