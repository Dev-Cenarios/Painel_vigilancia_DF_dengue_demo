#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  shiny::tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    bs4Dash::bs4DashPage(
      header = bs4Dash::bs4DashNavbar(
        title = NULL,
        status = "primary",
        skin = "dark",
        shiny::column(
          3,
          htmltools::div(
            style = "display: flex; align-items: center; gap: 10px;",
            htmltools::img(src = "www/cenarios_logo_full.png", height = "40px", style = "margin-top: 4px;")
          )
        ),

        htmltools::div(
          style = "flex: 1 1 auto; display: flex; align-items: center; justify-content: center;",
          htmltools::h4(
            "Painel de Vigilância de Dengue",
            style = "color: #FFFFFF; margin: 0; font-weight: 600; white-space: nowrap;"
          ),
          htmltools::span(
            "DEMONSTRAÇÃO · DADOS SIMULADOS",
            title = "Todos os casos deste painel são fictícios, sorteados por computador",
            style = paste(
              "margin-left: 12px; padding: 3px 10px; border-radius: 12px;",
              "background: #F2B705; color: #12346B; font-weight: 700;",
              "font-size: 12px; letter-spacing: .5px; white-space: nowrap;"
            )
          )
        ),

        ### BOTÃO DE ATUALIZAÇÃO DE DADOS

        shiny::column(
  1,
  shiny::actionButton(
    "update_data",
    label = NULL,
    icon = shiny::icon("sync-alt"),
    title = "Gerar nova base simulada",
    class = "btn-sm",
    style = "background-color: #12346B; color: white; border: 2px solid white; padding: 8px 12px;"
  ),
  align = "center"
),

        shiny::column(3,
                      shiny::uiOutput("municipio_1-db_info_ui"),
                      align="right"
                      )
      ),

      sidebar = bs4Dash::bs4DashSidebar(disable = TRUE),

      body = bs4Dash::bs4DashBody(
        shinyjs::useShinyjs(),
        shinytoastr::useToastr(),
        mod_municipio_ui("municipio_1")
      )
    )
  )
}

# Versao dos recursos estaticos (js/css de inst/app/www) para o endereco
# mudar sempre que algum deles mudar (evita cache antigo no navegador)
versao_recursos <- function() {
  arquivos <- list.files(app_sys("app/www"), pattern = "\\.(js|css)$", full.names = TRUE)
  if (length(arquivos) == 0) return("0.0.1")
  format(max(file.mtime(arquivos)), "%Y%m%d%H%M%S")
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  golem::add_resource_path(
    "www",
    app_sys("app/www")
  )

  # CSS de customizacao (cores dos cards, navbar, etc.) embutido inline no HTML
  # em vez de carregado como arquivo externo (.css) -- assim ele chega sempre
  # atualizado a cada carregamento da pagina, sem depender de cache de arquivo
  # do navegador (usuarios que ja visitaram o painel antes de uma mudanca de
  # cor nao ficam presos numa versao antiga em cache).
  inline_css <- paste(readLines(app_sys("app/badges_inline.css"), warn = FALSE), collapse = "\n")

  tags$head(
    golem::favicon(),
    fresh::use_theme(app_theme),
    golem::bundle_resources(
      path = app_sys("app/www"),
      app_title = "Painel de Vigilância de Dengue (demonstração)",
      # versao = data da ultima alteracao dos arquivos de www/: o endereco
      # dos .js/.css muda a cada atualizacao do painel e o navegador nunca
      # fica com uma versao antiga em cache
      version = versao_recursos()
    ),
    htmltools::tags$style(htmltools::HTML(inline_css))
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
