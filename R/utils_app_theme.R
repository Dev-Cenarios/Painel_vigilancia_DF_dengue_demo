#' app_theme
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
# Paleta alinhada ao painel de referência (https://sevsrecife.github.io/modelodashboard/):
# navy escuro (#12346B) como cor "primary" -- usado tanto no cabeçalho quanto
# nos títulos de box (antes "teal"), pra tudo ficar no mesmo tom.
# azul claro (#4A8EFF) como destaque/"info", laranja (#DB400A) para alertas,
# fundo cinza claro (#F4F4F4).
app_theme <- fresh::create_theme(
  fresh::bs4dash_vars(
    navbar_light_color = "#d7e3f0",
    navbar_light_active_color = "#FFF",
    navbar_light_hover_color = "#FFF"
  ),
  fresh::bs4dash_yiq(
    contrasted_threshold = 150,
    text_dark = "#212529",
    text_light = "#FFF"
  ),
  fresh::bs4dash_layout(
    main_bg = "#F4F4F4"
  ),
  fresh::bs4dash_status(
    primary = "#12346B",
    info = "#4A8EFF",
    danger = "#DB400A",
    success = "#198754"
  )
)
