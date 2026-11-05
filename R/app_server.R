#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session, chosen_municipio) {
  # Your application server logic

  sf::sf_use_s2(F)

  # Incrementado após uma atualização de dados bem-sucedida, para esta sessão
  # reler na hora o casos.parquet recém-gerado (as demais percebem sozinhas
  # em até 1 minuto, via reactivePoll em mod_municipio_server()).
  cache_trigger <- shiny::reactiveVal(0)

  # Painel único e independente para o Distrito Federal (sem seletor de
  # município na UI) -- "brasilia" é o identificador interno que
  # mod_municipio.R já espera (main_map_title).
  mod_municipio_server("municipio_1", shiny::reactive("brasilia"), cache_trigger)

  # Aviso de demonstração na abertura
  shiny::showModal(
    shiny::modalDialog(
      title = "Versão de demonstração",
      htmltools::HTML(paste0(
        "<p>Este painel usa <b>exclusivamente dados simulados</b>: todas as ",
        "notificações, pacientes, endereços, coordenadas e unidades ",
        "notificadoras foram <b>sorteados por computador</b> e não ",
        "correspondem a pessoas nem a casos reais.</p>",
        "<p>Os números <b>não representam</b> nenhuma situação ",
        "epidemiológica real. Apenas as malhas territoriais e a população ",
        "(IBGE, Censo 2022) são dados públicos.</p>"
      )),
      easyClose = TRUE,
      footer = shiny::modalButton("Entendi")
    )
  )

  # Botão "Atualizar dados" -- na demonstração, sorteia uma nova base
  # simulada (sem senha: não há banco nem dado sensível). Travado contra
  # execuções concorrentes, já que reescreve os arquivos de local/.
  updating_data <- shiny::reactiveVal(FALSE)

  shiny::observeEvent(input$update_data, {
    shiny::showModal(
      shiny::modalDialog(
        title = "Gerar nova base simulada?",
        htmltools::p(
          "No painel real, este botão baixa as notificações do banco de dados. ",
          "Aqui ele sorteia uma nova base fictícia (leva cerca de um minuto) e ",
          "o painel recarrega sozinho."
        ),
        footer = shiny::tagList(
          shiny::modalButton("Cancelar"),
          shiny::actionButton("confirm_update_data", "Gerar", class = "btn-primary")
        )
      )
    )
  })

  shiny::observeEvent(input$confirm_update_data, {
    if (isTRUE(updating_data())) {
      shiny::removeModal()
      shiny::showNotification("Geração já em andamento.", type = "warning", duration = 4)
      return()
    }

    shiny::removeModal()
    updating_data(TRUE)
    shinyjs::disable("update_data")
    on.exit({
      updating_data(FALSE)
      shinyjs::enable("update_data")
    }, add = TRUE)

    shiny::showNotification(
      "⏳ Sorteando nova base simulada...",
      type = "message",
      duration = NULL,
      id = "update_loading"
    )

    result <- update_dengue_data()
    shiny::removeNotification("update_loading")
    shiny::showNotification(
      result$message,
      type = if (result$success) "message" else "error",
      duration = 6,
      closeButton = TRUE
    )
    if (result$success) cache_trigger(cache_trigger() + 1)
  })

}
