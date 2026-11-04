# ==============================================================================
# SCRIPT: gerar_dados_simulados.R  (versao de demonstracao)
#
# Gera em local/ a base SIMULADA que o painel carrega:
#   local/dengue.duckdb          tabela bruta ficticia (formato SINAN) + tratada
#   local/casos.parquet          casos individuais (mesmo ETL do painel real)
#   local/estratificacao.parquet estratificacao de risco por setor e por RA
#   local/unidades_cnes.rds      unidades notificadoras ficticias
#
# Opcional: o painel gera a base sozinho na primeira abertura. Use este
# script para trocar a semente ou a data de referencia.
#
# EXECUTAR a partir da raiz do projeto:  source("dev/gerar_dados_simulados.R")
# ==============================================================================

pkgload::load_all(export_all = TRUE, helpers = FALSE, attach_testthat = FALSE, quiet = TRUE)

gerar_dados_simulados(
  dir = "local",
  seed = 2026,               # mude para sortear outra base
  dt_ref = Sys.Date() - 3    # "data de atualizacao" da base simulada
)
