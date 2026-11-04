if (FALSE) {
  library(tidyverse)
  library(geobr)
  library(sf)
  library(ggplot2)

  # Malha reaproveitada de Cenarios_TB_DF (mesma geografia -- Distrito
  # Federal, Regioes Administrativas dissolvidas a partir dos setores
  # censitarios do Censo 2022). Os arquivos ja vem copiados prontos em
  # inst/app/www/shapes/{bairros_df.gpkg,setores_df.gpkg,df.gpkg} -- rodar
  # este script so' e' necessario se a malha precisar ser regerada (ex.:
  # nova base de populacao).
  #
  # O DF não tem "bairros" no sentido do IBGE usado em read_neighborhood() —
  # a divisão administrativa relevante (Região Administrativa) corresponde ao
  # nível de subdistrito dentro dos setores censitários. Por isso a malha de
  # "bairro" do DF é construída dissolvendo os setores por subdistrito, em vez
  # de usar read_neighborhood() como no Manaus.

  setor_re <- read_census_tract(code_tract = 5300108, year = 2022, simplified = F)

head(setor_re)

  library(duckdb)

  con <- DBI::dbConnect(duckdb::duckdb(), "local/censo_aux.duckdb", read_only=T)

  pop_setor <- con %>% DBI::dbGetQuery("select id_setor_censitario, pessoas from tb_setor_censitario") %>% dplyr::as_tibble()

  head(pop_setor)

  con %>% DBI::dbDisconnect(shutdown=T)

  setor_re <- setor_re %>%
    dplyr::mutate(code_tract = as.character(code_tract)) %>%
    dplyr::left_join(
      pop_setor, by=c("code_tract" = "id_setor_censitario")
    )

  # Dissolve os setores por subdistrito (Região Administrativa).
  # Não mexer manualmente na coluna de geometria dentro do summarise() —
  # o método summarise.sf já une (st_union) a geometria por grupo sozinho,
  # e fazer isso à mão quebra a classe sfc da coluna.
  bair_re <- setor_re %>%
    dplyr::group_by(code_muni, code_subdistrict, name_subdistrict) %>%
    dplyr::summarise(
      pop = sum(pessoas, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      area_km2 = as.numeric(sf::st_area(geom))/1e6,
      densidade = pop/(1e6*area_km2)
    ) %>%
    dplyr::relocate(geom, .after = dplyr::everything())

  setor_re <- setor_re %>%
    dplyr::mutate(area_km2 = as.numeric(sf::st_area(geom))/1e6) %>%
    dplyr::mutate(densidade = pessoas/(1e6*area_km2)) %>%
    dplyr::select(code_tract, code_subdistrict, name_subdistrict, pop=pessoas, area_km2, densidade, geom)

  bair_re %>% sf::st_write("inst/app/www/shapes/bairros_df.gpkg", append = FALSE)
  setor_re %>% sf::st_write("inst/app/www/shapes/setores_df.gpkg", append = FALSE)

}
