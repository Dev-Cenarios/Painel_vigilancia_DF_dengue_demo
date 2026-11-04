if (FALSE) {
  library(tidyverse)
  library(geobr)
  library(sf)
  library(DBI)
  library(duckdb)
  library(dplyr)

  # Baixar limite do Distrito Federal
  df_shape <- read_municipality(code_muni = 5300108, year = 2022, simplified = FALSE)

  # Conectar ao banco de dados de população
  con <- DBI::dbConnect(duckdb::duckdb(), "local/censo_aux.duckdb", read_only = TRUE)

  pop_setor <- con %>%
    DBI::dbGetQuery("select id_setor_censitario, pessoas from tb_setor_censitario") %>%
    dplyr::as_tibble()

  con %>% DBI::dbDisconnect(shutdown = TRUE)

  # Calcular população total do DF
  pop_total <- pop_setor %>%
    dplyr::summarise(pop = sum(pessoas)) %>%
    dplyr::pull(pop)

  # Preparar dados do município
  df_shape <- df_shape %>%
    dplyr::select(code_muni, name_muni, geom) %>%
    dplyr::mutate(
      pop = pop_total,
      area_km2 = as.numeric(sf::st_area(geom)) / 1e6,
      densidade = pop / (1e6 * area_km2)
    )

  # Salvar como GPKG
  df_shape %>%
    sf::st_write("inst/app/www/shapes/df.gpkg", delete_layer = TRUE)

  cat("✓ Arquivo df.gpkg criado com sucesso!\n")
}
