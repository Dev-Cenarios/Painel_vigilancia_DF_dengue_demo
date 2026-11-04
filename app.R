# Set options here
options(golem.app.prod = TRUE) # TRUE = production mode, FALSE = development mode

library(dplyr)

# Em produção, erros do R não devem ser expostos crus ao usuário final
# (mensagens podem revelar nomes de tabelas/colunas, caminhos de arquivo, etc).
# Deixe o padrão do Shiny (sanitizado); os detalhes completos continuam
# disponíveis nos logs do servidor.
options(shiny.sanitize.errors = TRUE)

# Comment this if you don't want the app to be served on a random port
#options(shiny.port = httpuv::randomPort())

# Detach all loaded packages and clean your environment
#golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

# Document and reload your package
#golem::document_and_reload()

# Carrega o pacote a partir desta pasta (Shiny Server / shiny::runApp()).
# Sem isso, run_app() so' existe se o pacote PVDengueDF estiver instalado.
pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE, quiet = TRUE)

# Run the application
run_app()
