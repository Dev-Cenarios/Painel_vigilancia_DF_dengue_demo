# mod_municipio.R -- Painel de Dengue DF
#
# Porta R/mod_municipio.R de Cenarios_TB_DF para dengue. Mesma arquitetura
# (mapa leaflet + cards + tabela reactable, um unico municipio "brasilia"
# sem seletor), adaptada ao modelo clinico de dengue (doenca aguda, evolucao
# em dias/semanas) em vez de TB (tratamento longo, meses):
#
#   TB (Cenarios_TB_DF)          -> Dengue (este arquivo)
#   ---------------------------     -----------------------------------------
#   dt_diagnostico                  dt_sintomas       (DT_SIN_PRI)
#   dt_inicio_tratamento            dt_obito          (DT_OBITO)
#   forma (Pulmonar/Extrapulmonar)  classificacao     (CLASSI_FIN)
#   desfecho (Cura/Abandono/...)    evolucao          (EVOLUCAO)
#   situacao (aberto ate 6 meses)   situacao          (aberto = sem EVOLUCAO)
#   tipo_entrada (TRATAMENTO)       [sem equivalente -- filtro removido]
#   comorbidades (AGRAVXXX)         comorbidades      (DIABETES, HEMATOLOG, ...)
#   populacao_especial (POP_XXX)    populacao_especial (Gestante, Hospitalizado)
#   box_abandono / box_situacao_rua / box_privado_liberdade
#     -> kpi_grave (Dengue grave) / kpi_alarme (Sinais de alarme) / kpi_gestante
#
# Ver R/fct_db_update.R (dengue_sinan_treated_sql()) para a transformacao
# completa dos campos brutos do SINAN.

# UI ----


filtro_sexo_default <- c("F", "M", "I")
filtro_idade_default <- as.integer(c(0, 120))
filtro_raca_cor_default <- c("Branca", "Preta", "Amarela", "Parda", "Indígena", "Ignorado")
filtro_comorbidades_default <- NULL
filtro_populacao_especial_default <- NULL
filtro_evolucao_default <- c("Cura", "Em investigação", "Óbito em investigação", "Óbito pelo agravo", "Óbito por outras causas", "Ignorado")
# Todas as classificacoes por padrao -- "Em investigação" (CLASSI_FIN vazio)
# e "Ignorado" tambem entram, pra nenhum caso ficar fora do painel sem o
# usuario escolher
filtro_classificacao_default <- c("Dengue", "Dengue com sinais de alarme", "Dengue grave", "Descartado", "Inconclusivo", "Em investigação", "Ignorado")

# O que cada KPI clicavel mostra no mapa (as mesmas regras de calc_cards())
KPI_FILTROS <- list(
  investigacao = list(evolucao = "Em investigação"),
  cura = list(evolucao = "Cura"),
  obitos = list(evolucao = c("Óbito pelo agravo", "Óbito em investigação")),  # OBITOS_DENGUE
  grave = list(classificacao = "Dengue grave"),
  alarme = list(classificacao = "Dengue com sinais de alarme"),
  gestante = list(populacao_especial = "Gestante")
)
KPI_ROTULOS <- c(investigacao = "Em investigação", cura = "Curados", obitos = "Óbitos por dengue",
                 grave = "Dengue grave", alarme = "Sinais de alarme", gestante = "Gestantes")

# Indicadores do mapa territorial (RA ou setor censitario). Os do periodo
# seguem o filtro de data e os demais filtros; os da estratificacao replicam
# o relatorio "Estratificação de risco para dengue por setor censitário" e
# nao dependem dos filtros (ver calcular_estratificacao() em fct_db_update.R).
INDICADORES_MAPA <- list(
  "Indicadores do período (seguem os filtros)" = c(
    "Incidência (casos/população)" = "inc",
    "Mortalidade (óbitos por dengue/população)" = "mort",
    "Letalidade (óbitos por dengue/casos)" = "let",
    "Proporção de cura (curados/casos encerrados)" = "prop_cura",
    "Proporção de casos graves (graves/confirmados)" = "prop_grave"
  ),
  "Estratificação de risco (série histórica)" = c(
    "Estrato de risco (escore composto)" = "estrato",
    "Escore composto" = "escore",
    "I1 — Magnitude (incidência média anual)" = "I1",
    "I2 — Recorrência (anos com incidência ≥ 300)" = "I2",
    "I3 — Situação recente (incidência dos 2 últimos anos)" = "I3",
    "I4 — Gravidade (incidência média de casos graves)" = "I4"
  )
)

# Cores dos estratos -- as mesmas dos mapas do relatório (07_mapas.R)
ESTRATO_CORES <- c(
  "Baixo" = "#55A868", "Médio" = "#DD8452", "Alto" = "#C44E52",
  "Muito alto" = "#8172B2", "Sem dados" = "#BBBBBB"
)

# Controles do painel flutuante do mapa
controles_evolucao <- function(ns) {
  htmltools::tagList(
    htmltools::p("Selecione quais evoluções a serem mostradas no mapa", style = "color: gray; font-size: 14px"),
    reactable::reactableOutput(ns("casos_table"))
  )
}

controles_indicadores <- function(ns, selectize = FALSE) {
  htmltools::tagList(
    shinyWidgets::radioGroupButtons(
      ns("unidade_territorial"),
      label = "Unidade territorial",
      choices = c("Região Administrativa" = "ra", "Setor censitário" = "setor"),
      selected = "ra",
      justified = TRUE,
      status = "primary",
      size = "sm"
    ),
    shinyWidgets::prettyCheckbox(ns("show_indicador_bairro"),
                                 label = "Mostrar indicador no mapa",
                                 status = "info",
                                 icon = shiny::icon("check"),
                                 value = TRUE),
    # no painel flutuante usa o <select> nativo: o menu do selectize seria
    # cortado pela rolagem do painel
    shiny::selectInput(
      ns("indicador_bairro"),
      label = "Indicador",
      choices = INDICADORES_MAPA,
      selected = "inc",
      selectize = selectize,
      width = "100%"
    ),
    shiny::uiOutput(ns("nota_indicador"))
  )
}

reactable_idioma <- reactable::reactableLang(
  sortLabel = "Ordenar {name}",
  filterLabel = "Filtrar {name}",
  searchPlaceholder = "Buscar",
  searchLabel = "Buscar",
  noData = "Nenhum registro encontrado",
  pageNext = "Próxima",
  pagePrevious = "Anterior",
  pageNumbers = "{page} de {pages}",
  pageInfo = "{rowStart}–{rowEnd} de {rows} registros",
  pageSizeOptions = "Mostrar {rows}",
  pageNextLabel = "Próxima",
  pagePreviousLabel = "Anterior",
  pageNumberLabel = "Página {page}",
  pageJumpLabel = "Ir para página",
  pageSizeOptionsLabel = "Registros por página",
  groupExpandLabel = "Abrir grupo",
  detailsExpandLabel = "Mostrar detalhes",
  selectAllRowsLabel = "Selecionar todos",
  selectAllSubRowsLabel = "Selecionar todos do grupo",
  selectRowLabel = "Selecionar linha"
)


#' municipio UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom magrittr %>%
mod_municipio_ui <- function(id) {
  ns <- shiny::NS(id)

  # Data mais recente do conjunto de dados, para o filtro de período abrir já
  # no ano vigente (periodo_padrao()); o usuário pode mudar no filtro de data
  # ou voltar ao ano pelo botão do ano. Vem da memória do processo
  # (get_dados()), sem abrir o banco a cada acesso.
  periodo_max <- tryCatch(get_dados()$meta$dt_max_notificacao, error = function(e) Sys.Date())
  periodo_min <- periodo_padrao(periodo_max)[1]

  ## Filtros avancados (sexo, idade, raca/cor...) ----
  # Caixa que aparece acima do mapa ao clicar em "Mais filtros" (janela de
  # periodo dentro do mapa). Valem para todas as abas.
  filtros_avancados <-
    shiny::fluidRow(
      bs4Dash::column(12,
                      bs4Dash::box(
                        id = ns("box_filtro"),
                        title = "Filtros",
                        width = NULL,
                        status = "primary",
                        solidHeader = F,
                        closable = F,
                        collapsible = F,
                        icon = shiny::icon("sliders"),
                        # seta pra recolher a caixa (volta pelo "Mais filtros")
                        label = shiny::actionButton(
                          ns("fechar_filtros"), label = NULL,
                          icon = shiny::icon("chevron-up"),
                          class = "btn-tool pv-filtro-fechar",
                          title = "Recolher filtros"
                        ),
                        shiny::fluidRow(
                          bs4Dash::column(3,
                                          shinyWidgets::checkboxGroupButtons(
                                            ns("filtro_sexo"),
                                            label = "Sexo",
                                            choiceNames = c("Feminino", "Masculino", "Ignorado"),
                                            choiceValues = c("F", "M", "I"),
                                            selected = filtro_sexo_default,
                                            checkIcon = list(
                                              yes = shiny::icon("check")
                                            ),
                                            status = "info"
                                          ),
                                          shiny::sliderInput(
                                            ns("filtro_idade"),
                                            label = "Idade (anos)",
                                            min = 0,
                                            max = 120,
                                            value = filtro_idade_default,
                                            step = 1
                                          )
                                          ),
                          bs4Dash::column(
                            3,
                            shinyWidgets::pickerInput(
                              ns("filtro_raca_cor"),
                              label = "Raça/cor",
                              choices = c("Branca", "Preta", "Amarela", "Parda", "Indígena", "Ignorado"),
                              selected = filtro_raca_cor_default,
                              multiple = TRUE,
                              options = shinyWidgets::pickerOptions(noneSelectedText = "Selecione raça/cor...",
                                                      noneResultsText = "Nenhum resultado encontrado.",
                                                      deselectAllText = "Limpar seleção",
                                                      selectAllText = "Selecionar tudo",
                                                      actionsBox = TRUE
                              )
                            ),
                            shinyWidgets::pickerInput(
                              ns("filtro_classificacao"),
                              label = "Classificação final",
                              choices = filtro_classificacao_default,
                              selected = filtro_classificacao_default,
                              multiple = T,
                              options = shinyWidgets::pickerOptions(noneSelectedText = "Selecione classificação...",
                                                      noneResultsText = "Nenhum resultado encontrado.",
                                                      deselectAllText = "Limpar seleção",
                                                      selectAllText = "Selecionar tudo",
                                                      actionsBox = TRUE
                              )
                            )
                          ),
                          bs4Dash::column(
                            3,
                            shinyWidgets::pickerInput(
                              ns("filtro_comorbidades"),
                              label = "Comorbidades",
                              choices = c("Diabetes", "Doenças hematológicas", "Hepatopatias",
                                          "Doença renal", "Hipertensão arterial",
                                          "Doença acidopéptica", "Doenças autoimunes"),
                              selected = filtro_comorbidades_default,
                              multiple = TRUE,
                              options = shinyWidgets::pickerOptions(noneSelectedText = "Selecione comorbidade...",
                                                      noneResultsText = "Nenhum resultado encontrado.",
                                                      deselectAllText = "Limpar seleção",
                                                      selectAllText = "Selecionar tudo",
                                                      maxOptions = 1,
                                                      actionsBox = FALSE
                              )
                            ),
                            shinyWidgets::pickerInput(
                              ns("filtro_populacao_especial"),
                              label = "População especial",
                              choices = c("Gestante", "Hospitalizado"),
                              selected = filtro_populacao_especial_default,
                              multiple = TRUE,
                              options = shinyWidgets::pickerOptions(noneSelectedText = "Selecione população específica...",
                                                      noneResultsText = "Nenhum resultado encontrado.",
                                                      deselectAllText = "Limpar seleção",
                                                      selectAllText = "Selecionar tudo",
                                                      maxOptions = 1,
                                                      actionsBox = FALSE
                              )
                            ),
                            bs4Dash::actionButton(
                              ns("filtro_clear"),
                              htmltools::strong("Limpar filtros"),
                              icon = shiny::icon("undo"),
                              width = "100%",
                              status = "secondary"
                            )
                          ),
                          bs4Dash::column(
                            3,
                            shinyWidgets::pickerInput(
                              ns("filtro_evolucao"),
                              label = "Evolução",
                              choices = c(
                                "Cura", "Em investigação",
                                "Óbito em investigação", "Óbito pelo agravo",
                                "Óbito por outras causas", "Ignorado"
                              ),
                              selected = filtro_evolucao_default,
                              multiple = TRUE,
                              options = shinyWidgets::pickerOptions(noneSelectedText = "Selecione evolução...",
                                                                    noneResultsText = "Nenhum resultado encontrado.",
                                                                    deselectAllText = "Limpar seleção",
                                                                    selectAllText = "Selecionar tudo",
                                                                    actionsBox = TRUE
                            )),
                            bs4Dash::actionButton(
                              ns("filtro_update"),
                              htmltools::strong("Aplicar filtros"),
                              icon = shiny::icon("refresh"),
                              width = "100%",
                              status = "success"
                                 ), style="z-index:1002;"
                            )
                        )
                      )
                      ),
      id = ns("box_filtro_row")
    ) |> shinyjs::hidden()

  ## Janela flutuante do periodo (dentro do mapa) ----
  janela_periodo <- htmltools::div(
    class = "pv-janela-periodo",
    htmltools::div(class = "pv-janela-rotulo", shiny::icon("calendar-days"), "Primeiros sintomas"),
    shiny::dateRangeInput(
      ns("periodo"),
      label = NULL,
      start = periodo_min,
      end = periodo_max,
      min = lubridate::ymd("1939-01-01"),
      max = periodo_max,
      format = "dd/mm/yyyy",
      startview = "month",
      language = "pt-BR",
      separator = "até",
      width = "250px"
    ),
    # atalho para o ano vigente inteiro (o mesmo periodo padrao)
    shiny::actionButton(
      ns("ano_vigente"),
      label = format(periodo_max, "%Y"),
      title = paste("Ver todo o ano de", format(periodo_max, "%Y")),
      class = "pv-janela-ano ativo"
    ),
    shiny::actionButton(
      ns("mostrar_filtros"),
      label = "Mais filtros",
      icon = shiny::icon("filter"),
      class = "pv-janela-botao"
    )
  )

  ## Painel flutuante do mapa (evolucao + indicadores territoriais) ----
  # Comportamento em inst/app/www/painel_mapa.js, estilo em
  # inst/app/badges_inline.css (.pv-painel*, .pv-janela-periodo)
  painel_mapa <- htmltools::div(
    class = "pv-painel",
    htmltools::div(
      class = "pv-painel-topo",
      htmltools::tags$button(
        type = "button", class = "pv-painel-alternar",
        title = "Recolher / abrir painel",
        shiny::icon("sliders"),
        htmltools::span(class = "pv-painel-titulo", "Painel do mapa"),
        htmltools::span(class = "pv-painel-seta", shiny::icon("chevron-left"))
      )
    ),
    htmltools::div(
      class = "pv-painel-corpo",
      htmltools::tags$details(
        class = "pv-secao", open = NA,
        htmltools::tags$summary(shiny::icon("chart-bar"), "Distribuição por evolução"),
        htmltools::div(class = "pv-secao-conteudo", controles_evolucao(ns))
      ),
      htmltools::tags$details(
        class = "pv-secao", open = NA,
        htmltools::tags$summary(shiny::icon("layer-group"), "Indicadores territoriais"),
        htmltools::div(class = "pv-secao-conteudo", controles_indicadores(ns))
      )
    )
  )

  ## KPIs ----
  # Faixa compacta de cartoes numa linha so', acima do mapa (estilo .pv-kpi*
  # em inst/app/badges_inline.css). Estrutura fixa; so' os numeros sao
  # outputs, entao o layout nao pisca quando os filtros mudam.
  # Clicar num cartao mostra no mapa so' os casos dele (KPI_FILTROS);
  # clicar de novo, ou em "Casos no periodo", volta a mostrar todos.
  kpi <- function(saida, rotulo, icone, cor, detalhe = NULL) {
    htmltools::div(
      id = ns(paste0("kpi_card_", cor)),
      class = paste("pv-kpi", paste0("pv-kpi-", cor)),
      role = "button", tabindex = "0",
      title = if (cor == "total") "Mostrar todos os casos no mapa" else paste(rotulo, "- mostrar no mapa (clique de novo para ver todos)"),
      onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", ns("kpi_clicado"), cor),
      onkeydown = "if (event.key === 'Enter' || event.key === ' ') { event.preventDefault(); this.click(); }",
      # botao redondo no canto: "ver no mapa" (vira "x" no cartao selecionado)
      htmltools::span(
        class = "pv-kpi-acao", `aria-hidden` = "true",
        shiny::icon(if (cor == "total") "earth-americas" else "map-location-dot", class = "pv-kpi-acao-ver"),
        shiny::icon("xmark", class = "pv-kpi-acao-limpar")
      ),
      htmltools::div(class = "pv-kpi-icone", shiny::icon(icone)),
      htmltools::div(
        class = "pv-kpi-texto",
        htmltools::div(class = "pv-kpi-valor", shiny::textOutput(ns(saida), inline = TRUE)),
        htmltools::div(class = "pv-kpi-rotulo", rotulo),
        if (!is.null(detalhe)) htmltools::div(class = "pv-kpi-detalhe", detalhe)
      )
    )
  }
  kpis <- htmltools::div(
    id = ns("kpis"),
    class = "pv-kpis",
    kpi("kpi_total", "Casos no período", "users", "total"),
    kpi("kpi_investigacao", "Em investigação", "magnifying-glass", "investigacao",
      detalhe = shiny::textOutput(ns("kpi_investigacao_30d"), inline = TRUE)),
    kpi("kpi_cura", "Curados", "heart", "cura"),
    kpi("kpi_obitos", "Óbitos por dengue", "heart-pulse", "obitos"),
    kpi("kpi_grave", "Dengue grave", "triangle-exclamation", "grave"),
    kpi("kpi_alarme", "Sinais de alarme", "bell", "alarme"),
    kpi("kpi_gestante", "Gestantes", "person-pregnant", "gestante")
  )

  htmltools::div(
    class = "pv-abas",
    shiny::tabsetPanel(
      id = ns("abas"),
      type = "pills",

      ### Aba 1: mapa ----
      shiny::tabPanel(
        "Mapa", value = "mapa", icon = shiny::icon("map-location-dot"),
        filtros_avancados,
        kpis,
        shiny::fluidRow(
          bs4Dash::column(12,
            bs4Dash::box(
              # titulo (com o periodo) no proprio cabecalho da caixa, pra
              # sobrar altura pro mapa
              title = shiny::uiOutput(ns("main_map_title"), inline = TRUE),
              width = NULL,
              status = "primary",
              solidHeader = TRUE,
              icon = shiny::icon("map-pin"),
              htmltools::div(
                class = "pv-mapa-wrap",
                # filtros ativos / KPI clicado: etiqueta sobre o mapa (nao
                # empurra o mapa pra baixo)
                shiny::uiOutput(ns("main_map_subtitle"), class = "pv-mapa-filtros"),
                # altura = tela menos cabecalho, abas, KPIs e caixa: o mapa
                # inteiro aparece ao abrir, sem rolar (minimo em .pv-mapa-wrap)
                leaflet::leafletOutput(ns("main_map"), height = "calc(100vh - 350px)"),
                painel_mapa,
                janela_periodo
              )
            )
          )
        )
      ),

      ### Aba 2: estratificacao de risco ----
      shiny::tabPanel(
        "Indicadores", value = "indicadores", icon = shiny::icon("chart-line"),
        shiny::uiOutput(ns("resumo_filtros")),
        shiny::fluidRow(
          bs4Dash::column(12,
            bs4Dash::box(
              title = htmltools::strong("Estratificação de risco"),
              width = NULL,
              status = "primary",
              solidHeader = TRUE,
              icon = shiny::icon("layer-group"),
              shiny::uiOutput(ns("estratificacao_resumo")),
              shiny::fluidRow(
                bs4Dash::column(5, plotly::plotlyOutput(ns("estratificacao_grafico"), height = "760px")),
                bs4Dash::column(7, DT::DTOutput(ns("estratificacao_tabela")))
              )
            )
          )
        )
      ),

      ### Aba 3: tabela de casos ----
      shiny::tabPanel(
        "Tabela de casos", value = "tabela", icon = shiny::icon("table"),
        shiny::uiOutput(ns("resumo_filtros_tabela")),
        shiny::fluidRow(
          bs4Dash::column(12,
            bs4Dash::box(
              title = htmltools::strong("Tabela de casos"),
              width = NULL,
              status = "primary",
              solidHeader = TRUE,
              icon = shiny::icon("table"),
              DT::DTOutput(ns("tabela_casos"))
            )
          )
        )
      )
    )
  )
}

# Paleta por quantil pro mapa de incidencia -- em vez de faixas de valor
# fixas (colorBin com bins manuais), cada cor abrange a mesma quantidade de
# RAs, recalculado a partir dos dados do periodo/filtro atual (mesmo criterio
# de Cenarios_TB_DF). leaflet::colorQuantile() quebra com "'breaks' nao sao
# unicos" sempre que ha' muitas RAs com o mesmo valor -- calcula os pontos de
# corte manualmente e usa so' os unicos, reduzindo o numero de classes em vez
# de dar erro.
pal_quantil <- function(palette, domain, n = 5) {
  domain <- domain[is.finite(domain)]
  if (length(domain) == 0) domain <- 0
  # Muitos zeros (setores sem caso no periodo) colapsam os quantis numa faixa
  # so' -- nesse caso os cortes vem dos valores positivos, e o zero fica na
  # primeira faixa
  positivos <- domain[domain > 0]
  if (mean(domain == 0) > 1 / n && length(unique(positivos)) >= n) {
    domain <- c(0, positivos)
  }
  breaks <- unique(stats::quantile(domain, probs = seq(0, 1, length.out = n + 1), na.rm = TRUE, names = FALSE))
  if (breaks[1] > 0 && any(domain == 0)) breaks[1] <- 0
  if (length(breaks) < 2) {
    breaks <- c(breaks, breaks[length(breaks)] + 1)
  }
  leaflet::colorBin(palette, domain = domain, bins = breaks)
}

fmt_int <- function(x) format(x, big.mark = ".", decimal.mark = ",", trim = TRUE)

fmt_dec <- function(x, digitos = 1) {
  ifelse(is.na(x), "-", formatC(x, format = "f", digits = digitos, big.mark = ".", decimal.mark = ","))
}

fmt_pct <- function(x) {
  ifelse(is.na(x), "-", scales::percent(x, accuracy = 0.01, big.mark = ".", decimal.mark = ","))
}

# Formato dos numeros da legenda (o leaflet usa ponto decimal; por isso
# incidencias vao inteiras, com ponto de milhar, e proporcoes em %)
LEGENDA_INTEIRO <- leaflet::labelFormat(big.mark = ".", digits = 0)
LEGENDA_PCT <- leaflet::labelFormat(suffix = "%", digits = 1, transform = function(x) 100 * x)

# Cores, legenda e linhas do tooltip de um indicador territorial. `tab` tem
# uma linha por unidade (RA ou setor, na ordem da malha) com `nome`,
# `populacao` e as colunas do indicador (calc_ind_territorio()). Serve as duas
# escalas: RA (desenhada pelo leaflet do R) e setor (territorio_layer.js).
especificar_indicador <- function(tab, indicador, periodo) {
  periodo_anos <- if (is.null(periodo)) 1 else pmax(1, as.numeric(periodo[2] - periodo[1]) / 365)
  pop <- c("População (Censo 2022)" = list(fmt_int(tab$populacao)))
  anos_serie <- if (!is.null(tab$ano_inicial)) paste0(tab$ano_inicial[1], "–", tab$ano_final[1]) else ""

  linhas_estrat <- function() {
    list(
      "Estrato" = ifelse(is.na(tab$estrato), "Sem dados", tab$estrato),
      "Escore" = fmt_dec(tab$escore, 4),
      "I1 magnitude" = paste(fmt_dec(tab$I1), "/100 mil"),
      "I2 recorrência" = paste(fmt_int(tab$I2), "ano(s) ≥ 300/100 mil"),
      "I3 situação recente" = paste(fmt_dec(tab$I3), "/100 mil"),
      "I4 gravidade" = paste(fmt_dec(tab$I4), "/100 mil"),
      "População (Censo 2022)" = fmt_int(tab$populacao),
      "Casos prováveis" = paste0(fmt_int(tab$casos_totais), " (", anos_serie, ")"),
      "Óbitos pelo agravo" = fmt_int(tab$obitos_totais)
    )
  }

  switch(
    indicador,
    inc = {
      tab$ind <- tab$ind / periodo_anos
      pal <- pal_quantil("YlOrRd", domain = tab$ind, n = 5)
      list(pal = pal, valores = tab$ind, titulo = "Incidência anual", lab = LEGENDA_INTEIRO,
           linhas = c(list("Casos no período" = fmt_int(tab$casos)), pop,
                      list("Incidência anual" = paste(fmt_dec(tab$ind), "casos/100 mil hab./ano"))))
    },
    mort = {
      tab$ind <- tab$ind / periodo_anos
      pal <- leaflet::colorBin("YlOrRd", domain = tab$ind, bins = c(0, 10, 50, 75, 100, 200, 500, 1000, Inf) / 10)
      list(pal = pal, valores = tab$ind, titulo = "Mortalidade anual por dengue",
           linhas = c(list("Óbitos por dengue no período" = fmt_int(tab$obitos)), pop,
                      list("Mortalidade anual" = paste(fmt_dec(tab$ind), "óbitos/100 mil hab./ano"))))
    },
    let = {
      pal <- leaflet::colorBin("BuPu", domain = tab$ind, bins = c(0, 0.01, 0.025, 0.05, 0.1, 0.15, 0.2, Inf))
      list(pal = pal, valores = tab$ind, titulo = "Letalidade", lab = LEGENDA_PCT,
           linhas = list("Óbitos por dengue no período" = fmt_int(tab$obitos),
                         "Casos de dengue no período" = fmt_int(tab$casos),
                         "Letalidade" = fmt_pct(tab$ind)))
    },
    prop_cura = {
      pal <- leaflet::colorBin("RdYlGn", domain = tab$ind, bins = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, Inf))
      list(pal = pal, valores = tab$ind, titulo = "Prop. Cura", lab = LEGENDA_PCT,
           linhas = list("Curados" = fmt_int(tab$curados),
                         "Casos encerrados no período" = fmt_int(tab$encerrados),
                         "Prop. Cura" = fmt_pct(tab$ind)))
    },
    prop_grave = {
      pal <- leaflet::colorBin("Reds", domain = tab$ind, bins = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, Inf))
      list(pal = pal, valores = tab$ind, titulo = "Prop. Casos graves", lab = LEGENDA_PCT,
           linhas = list("Casos graves" = fmt_int(tab$graves),
                         "Casos confirmados no período" = fmt_int(tab$confirmados),
                         "Prop. Casos graves" = fmt_pct(tab$ind)))
    },
    estrato = {
      estrato <- ifelse(is.na(tab$estrato), "Sem dados", tab$estrato)
      pal <- leaflet::colorFactor(unname(ESTRATO_CORES), levels = names(ESTRATO_CORES), ordered = TRUE)
      list(pal = pal, valores = factor(estrato, levels = names(ESTRATO_CORES)),
           titulo = paste("Estrato de risco", anos_serie), linhas = linhas_estrat())
    },
    escore = {
      pal <- pal_quantil("Purples", domain = tab$escore, n = 4)
      list(pal = pal, valores = tab$escore, titulo = paste("Escore composto", anos_serie), linhas = linhas_estrat())
    },
    I2 = {
      pal <- leaflet::colorNumeric("YlOrRd", domain = c(0, max(tab$I2, 1, na.rm = TRUE)))
      list(pal = pal, valores = tab$I2, titulo = "I2 — anos com incidência ≥ 300", linhas = linhas_estrat())
    },
    {
      # I1, I3, I4: incidencias -- quantis (outliers de setores pequenos nao
      # dominam a cor, mesmo criterio dos mapas do relatorio)
      titulos <- c(I1 = "I1 — Magnitude (/100 mil)", I3 = "I3 — Situação recente (/100 mil)",
                   I4 = "I4 — Gravidade (/100 mil)")
      pal <- pal_quantil(if (indicador == "I4") "Reds" else "YlOrRd", domain = tab[[indicador]], n = 5)
      list(pal = pal, valores = tab[[indicador]], titulo = titulos[[indicador]], lab = LEGENDA_INTEIRO,
           linhas = linhas_estrat())
    }
  )
}

fmt_data <- function(x) ifelse(is.na(x), "-", format(x, "%d/%m/%Y"))

# Popup de um caso no mapa -- montado no servidor so' quando o ponto e'
# clicado, em vez de embutir um popup HTML em cada um dos milhares de pontos.
popup_caso <- function(caso) {
  sprintf(
    "<b>Número de notificação</b>: %s
    <br><b>Data dos primeiros sintomas</b>: %s
    <br><b>Data de notificação</b>: %s
    <br><b>Data de encerramento</b>: %s
    <br><b>Sexo</b>: %s
    <br><b>Idade</b>: %s
    <br><b>Raça/cor</b>: %s
    <br><b>Endereço</b>: %s
    <br><b>Setor censitário</b>: %s
    <br><b>Comorbidades</b>: %s
    <br><b>População especial?</b> %s
    <br><b>Classificação final</b>: %s
    <br><br><b>Evolução</b>: %s",
    htmltools::htmlEscape(caso$numero_notificacao),
    fmt_data(caso$dt_sintomas),
    fmt_data(caso$dt_notificacao),
    fmt_data(caso$dt_encerramento),
    caso$sexo,
    round(caso$idade),
    caso$raca_cor,
    htmltools::htmlEscape(caso$endereco),
    ifelse(is.na(caso$setor_code), "não identificado", caso$setor_code),
    htmltools::htmlEscape(caso$comorbidades),
    htmltools::htmlEscape(ifelse(caso$populacao_especial == "", "Não", caso$populacao_especial)),
    caso$classificacao,
    caso$evolucao
  )
}

# Colunas da tabela de casos (na ordem de exibicao)
TABELA_COLS <- c(
  "numero_notificacao", "dt_notificacao", "dt_sintomas", "dt_obito",
  "idade", "sexo", "raca_cor", "classificacao",
  "dt_encerramento", "evolucao", "tempo_ate_encerramento", "georreferenciado"
)

dt_idioma <- list(
  sProcessing = "Processando...",
  sLengthMenu = "Mostrar _MENU_ registros",
  sZeroRecords = "Nenhum registro encontrado",
  sInfo = "_START_–_END_ de _TOTAL_ registros",
  sInfoEmpty = "0 registros",
  sInfoFiltered = "(filtrado de _MAX_)",
  sThousands = ".",
  sSearch = "Buscar:",
  oPaginate = list(sFirst = "Primeira", sPrevious = "Anterior", sNext = "Próxima", sLast = "Última")
)

# SERVER ----
#' municipio Server Functions
#'
#' @noRd
mod_municipio_server <- function(id, chosen_municipio, cache_trigger = shiny::reactiveVal(0)){
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns

    ## Filtros iniciais -----

    filtro_sexo_active <- shiny::reactiveVal(filtro_sexo_default)
    filtro_idade_active <- shiny::reactiveVal(filtro_idade_default)
    filtro_raca_cor_active <- shiny::reactiveVal(filtro_raca_cor_default)
    filtro_comorbidades_active <- shiny::reactiveVal(filtro_comorbidades_default)
    filtro_populacao_especial_active <- shiny::reactiveVal(filtro_populacao_especial_default)
    filtro_evolucao_active <- shiny::reactiveVal(filtro_evolucao_default)
    filtro_classificacao_active <- shiny::reactiveVal(filtro_classificacao_default)

    evolucao_ids <- dplyr::tibble(evolucao = EVOLUCOES) |>
      dplyr::mutate(evolucao_id = dplyr::row_number())

    ## Reactive values ----

    ### Dados (memoria do processo) ----
    # Confere a cada minuto se o parquet mudou (atualizacao feita por outra
    # sessao/processo); cache_trigger forca a releitura na hora nesta sessao.
    dados_poll <- shiny::reactivePoll(
      60000, session,
      checkFunc = versao_dados,
      valueFunc = get_dados
    )

    dados <- shiny::reactive({
      cache_trigger()
      dados_poll()
      get_dados()
    })

    # Filtros da caixa "Mais filtros" -- os numeros dos KPIs usam estes
    filtros_base <- shiny::reactive({
      list(
        sexo = filtro_sexo_active(),
        idade = filtro_idade_active(),
        raca_cor = filtro_raca_cor_active(),
        comorbidades = filtro_comorbidades_active(),
        populacao_especial = filtro_populacao_especial_active(),
        evolucao = filtro_evolucao_active(),
        classificacao = filtro_classificacao_active()
      )
    })

    # KPI clicado: mapa, indicadores e tabela mostram so' os casos dele (os
    # cartoes continuam com os totais, pra nao zerar os outros)
    kpi_sel <- shiny::reactiveVal(NULL)
    filtros <- shiny::reactive({
      f <- filtros_base()
      s <- if (is.null(kpi_sel())) NULL else KPI_FILTROS[[kpi_sel()]]
      for (n in intersect(names(s), c("evolucao", "classificacao"))) f[[n]] <- intersect(f[[n]], s[[n]])
      if (!is.null(s$populacao_especial)) f$populacao_especial <- s$populacao_especial
      f
    })

    shiny::observeEvent(input$kpi_clicado, {
      k <- input$kpi_clicado
      kpi_sel(if (k == "total" || identical(k, kpi_sel())) NULL else k)
      shiny::updateTabsetPanel(session, "abas", selected = "mapa")
    })
    shiny::observe({
      k <- kpi_sel()
      shinyjs::toggleClass("kpis", "com-selecao", condition = !is.null(k))
      for (cor in c("total", names(KPI_FILTROS))) {
        shinyjs::toggleClass(paste0("kpi_card_", cor), "ativo", condition = identical(cor, k))
      }
    })

    # Digitar uma data no dateRangeInput dispara um evento por tecla --
    # debounce evita recalcular tudo a cada digito.
    periodo <- shiny::reactive({
      p <- as.Date(input$periodo)
      shiny::req(length(p) == 2, !anyNA(p), p[1] <= p[2])
      p
    }) |> shiny::debounce(400)

    # Botao do ano: volta o filtro para o ano vigente inteiro e fica destacado
    # enquanto o periodo for exatamente esse
    shiny::observeEvent(input$ano_vigente, {
      p <- periodo_default(dados())
      shiny::updateDateRangeInput(session, "periodo", start = p[1], end = p[2])
    })
    shiny::observe({
      shinyjs::toggleClass("ano_vigente", "ativo", condition = isTRUE(all(periodo() == periodo_default(dados()))))
    })

    ### Filtered cases (indices sobre dados()$casos) -----

    casos_idx <- shiny::reactive({
      calc_idx(dados(), periodo(), filtros())
    })

    cards <- shiny::reactive({
      calc_cards(dados(), periodo(), filtros_base())
    })

    ### Filtered points ----

    evolucao_sel <- shiny::reactive(reactable::getReactableState("casos_table", "selected"))

    pontos_mapa <- shiny::reactive({
      evolucoes <- evolucao_ids$evolucao[evolucao_sel()]
      calc_pontos(dados(), periodo(), filtros(), evolucoes)
    })

    ### Indicador territorial (RA ou setor censitario) ----

    unidade <- shiny::reactive({
      if (identical(input$unidade_territorial, "setor")) "setor" else "ra"
    })

    # Nome e populacao de cada unidade, na ordem da malha
    unidades_base <- function(d, un) {
      if (un == "setor") {
        data.frame(nome = paste0("Setor ", d$setores$codigo, " — ", d$setores$ra_nome),
                   populacao = d$setores$populacao)
      } else {
        data.frame(nome = d$bairros$name_subdistrict, populacao = d$bairros$pop)
      }
    }

    indicador_territorial <- shiny::reactive({
      d <- dados()
      un <- unidade()
      ind <- input$indicador_bairro
      shiny::req(ind)

      # a estratificacao nao depende de periodo/filtros: nao cria dependencia
      # deles (evita recolorir os 5.418 setores a cada filtro)
      if (ind %in% INDICADORES_ESTRATIFICACAO) {
        p <- NULL
        tab <- calc_ind_territorio(d, NULL, NULL, ind, un)
      } else {
        p <- periodo()
        tab <- calc_ind_territorio(d, p, filtros(), ind, un)
      }
      tab <- cbind(unidades_base(d, un), tab[, setdiff(names(tab), c("nome", "populacao")), drop = FALSE])

      spec <- especificar_indicador(tab, ind, p)
      cores <- spec$pal(spec$valores)
      cores[is.na(spec$valores)] <- "#e5e5e5"
      spec$cores <- cores
      spec$tab <- tab
      spec
    })

    ## ShinyJS ----

    shiny::observeEvent(input$show_indicador_bairro, {
      if (input$show_indicador_bairro) {
        shinyjs::enable("indicador_bairro")
      } else {
        shinyjs::disable("indicador_bairro")
      }
    })

    shiny::observeEvent(input$mostrar_filtros, {
      shinyjs::show("box_filtro_row")
    })

    shiny::observeEvent(input$fechar_filtros, {
      shinyjs::hide("box_filtro_row")
    })

    shiny::observeEvent(input$filtro_update, {

      filtro_sexo_active(input$filtro_sexo)
      filtro_idade_active(as.integer(input$filtro_idade))
      filtro_raca_cor_active(input$filtro_raca_cor)
      filtro_comorbidades_active(input$filtro_comorbidades)
      filtro_populacao_especial_active(input$filtro_populacao_especial)
      filtro_evolucao_active(input$filtro_evolucao)
      filtro_classificacao_active(input$filtro_classificacao)


      shinyjs::hide("box_filtro_row")
    })

    shiny::observeEvent(input$filtro_clear, {


      shinyWidgets::updateCheckboxGroupButtons("filtro_sexo", session = session, selected =filtro_sexo_default)
      shiny::updateSliderInput("filtro_idade", session=session, value = filtro_idade_default)
      shinyWidgets::updatePickerInput(session = session, "filtro_raca_cor", selected = filtro_raca_cor_default)
      shinyWidgets::updatePickerInput(session = session, "filtro_comorbidades", selected = filtro_comorbidades_default)
      shinyWidgets::updatePickerInput(session = session, "filtro_populacao_especial", selected = filtro_populacao_especial_default)
      shinyWidgets::updatePickerInput(session = session, "filtro_evolucao", selected = filtro_evolucao_default)
      shinyWidgets::updatePickerInput(session = session, "filtro_classificacao", selected = filtro_classificacao_default)


      filtro_sexo_active(filtro_sexo_default)
      filtro_idade_active(as.integer(filtro_idade_default))
      filtro_raca_cor_active(filtro_raca_cor_default)
      filtro_comorbidades_active(filtro_comorbidades_default)
      filtro_populacao_especial_active(filtro_populacao_especial_default)
      filtro_evolucao_active(filtro_evolucao_default)
      filtro_classificacao_active(filtro_classificacao_default)
      kpi_sel(NULL)


      shinyjs::hide("box_filtro_row")
    })

    shiny::observeEvent(input$ver_detalhes_casos, {

      num_notific <- input$ver_detalhes_casos$id

      bg_color <- c(
        "Cura" = "#24d152aa",
        "Em investigação" = "#AA0DFEaa",
        "Ignorado" = "#bdbdbdaa",
        "Óbito em investigação" = "#ff9e42aa",
        "Óbito pelo agravo" = "#f20f1eaa",
        "Óbito por outras causas" = "#FE00FAaa"
      )

      casos <- dados()$casos
      caso <- casos[match(num_notific, casos$numero_notificacao), , drop = FALSE]
      shiny::req(nrow(caso) == 1, !is.na(caso$id))

      caso$evolucao_label <- sprintf(
        '<span style="background-color: %s;
             color: white;
             padding: 5px 15px;
             border-radius: 20px;
             display: inline-block;
             font-size: 16px;
             font-weight: bold;">
    %s
</span>', bg_color[[caso$evolucao]], caso$evolucao
      )

      shiny::showModal(
        shiny::modalDialog(
          title = paste0("Ficha de notificação ", num_notific),
          htmltools::div(
            #style="line-height: 3.6;",
            shiny::fluidRow(
              bs4Dash::column(
                4,
                htmltools::HTML(sprintf("<h4><b>Data dos primeiros sintomas</b></h4> <p>%s</p>", format(caso$dt_sintomas, "%d/%m/%Y")))
              ),
              bs4Dash::column(
                4,
                htmltools::HTML(sprintf("<h4><b>Data de notificação</b></h4> <p>%s</p>", format(caso$dt_notificacao, "%d/%m/%Y")))
              ),
              bs4Dash::column(
                4,
                htmltools::HTML(sprintf("<h4><b>Data de óbito</b></h4> <p>%s</p>", ifelse(is.na(caso$dt_obito), "-", format(caso$dt_obito, "%d/%m/%Y"))))
              )
            ),
            shiny::hr(),
            shiny::fluidRow(
              bs4Dash::column(
                4,
                htmltools::HTML(sprintf("<h4><b>Idade</b></h4> <p>%d</p>", as.integer(floor(caso$idade))))
              ),
              bs4Dash::column(
                4,
                htmltools::HTML(sprintf("<h4><b>Sexo</b></h4> <p>%s</p>", caso$sexo))
              ),
              bs4Dash::column(
                4,
                htmltools::HTML(sprintf("<h4><b>Raça/cor</b></h4> <p>%s</p>", caso$raca_cor))
              )
            ),

            shiny::hr(),
            shiny::fluidRow(
              bs4Dash::column(
                6,
                htmltools::HTML(sprintf("<h4><b>Comorbidades</b></h4> <p>%s</p>",
                             htmltools::htmlEscape(ifelse(caso$n_comorbidades == 0, "Nenhuma", caso$comorbidades))))
              ),
              bs4Dash::column(
                6,
                htmltools::HTML(sprintf("<h4><b>População especial</b></h4> <p>%s</p>",
                             htmltools::htmlEscape(ifelse(caso$populacao_especial == "", "Não consta", caso$populacao_especial))))
              )
            ),

            shiny::hr(),
            shiny::fluidRow(
              bs4Dash::column(
                12,
                htmltools::HTML(sprintf("<h4><b>Classificação final</b></h4> <p>%s</p>", caso$classificacao))
              )
            ),

            shiny::hr(),
            shiny::fluidRow(
              bs4Dash::column(
                4,
                htmltools::HTML(sprintf("<h4><b>Evolução</b></h4> <p>%s</p>", caso$evolucao_label))
              ),
              bs4Dash::column(
                4,
                htmltools::HTML(sprintf("<h4><b>Data de encerramento</b></h4> <p>%s</p>", ifelse(is.na(caso$dt_encerramento), "-", format(caso$dt_encerramento, "%d/%m/%Y"))))
              ),
              bs4Dash::column(
                4,
                htmltools::HTML(sprintf("<h4><b>Tempo até encerramento (dias)</b></h4> <p>%s</p>", caso$tempo_ate_encerramento))
              )
            ),

            shiny::hr(),
            shiny::fluidRow(
              bs4Dash::column(
                4,
                htmltools::HTML(sprintf("<h4><b>Região Administrativa</b></h4> <p>%s</p>", htmltools::htmlEscape(caso$bairro)))
              ),
              bs4Dash::column(
                4,
                htmltools::HTML(sprintf("<h4><b>Logradouro</b></h4> <p>%s</p>", htmltools::htmlEscape(caso$endereco)))
              ),
              bs4Dash::column(
                4,
                htmltools::HTML(sprintf("<h4><b>Georreferenciado?</b></h4> <p>%s</p>",
                             ifelse(caso$georreferenciado == 1, "Sim", "Não")))
              )
            )
          ),
          size = "xl",
          footer = shiny::modalButton("Ok")
        )
      )

    }
    )

    ## UI outputs ----



    output$db_info_ui <- shiny::renderUI({

      meta <- dados()$meta

      htmltools::div(
        htmltools::div(style="font-weight: 100", htmltools::HTML(sprintf("<i class=\"fa fa-clock-o\" aria-hidden=\"true\"></i>  Data de atualização dos dados: %s",
                                                   format(meta$dt_max_notificacao, "%d/%m/%Y")))),
        htmltools::div(style="font-weight: 100", htmltools::HTML(sprintf("<i class=\"fa fa-globe\" aria-hidden=\"true\"></i> Total de casos georreferenciados: %s (%s)",
                                           fmt_int(meta$n_geo),
                                           scales::percent(
                                             meta$n_geo/meta$n_tot, accuracy = 0.1, big.mark=".", decimal.mark=","
                                           )
                                           )))
      )
    })

    output$main_map_title <- shiny::renderUI({

      mun <- chosen_municipio()

      location <- dplyr::case_when(
        mun == "brasilia" ~ "Brasília - DF",
        TRUE ~ "Município"
      )

      title <- paste0(
        "Mapa de casos de dengue em ",
        location, ", de ",
        format(periodo()[1], "%d/%m/%Y"), " à ", format(periodo()[2], "%d/%m/%Y")
      )

      htmltools::strong(title)
    })

    # Texto dos filtros ativos (alem do periodo) -- usado no subtitulo do mapa
    # e no resumo das abas Indicadores e Tabela de casos
    texto_filtros <- shiny::reactive({
      modified_sexo <- isTRUE(!identical(filtro_sexo_active(), filtro_sexo_default))
      modified_idade <- isTRUE(!identical(filtro_idade_active(), filtro_idade_default))
      modified_raca_cor <- isTRUE(!identical(filtro_raca_cor_active(), filtro_raca_cor_default))
      modified_comorbidades <- isTRUE(!identical(filtro_comorbidades_active(), filtro_comorbidades_default))
      modified_populacao_especial <- isTRUE(!identical(filtro_populacao_especial_active(), filtro_populacao_especial_default))
      modified_evolucao <- isTRUE(!identical(filtro_evolucao_active(), filtro_evolucao_default))
      modified_classificacao <- isTRUE(!identical(filtro_classificacao_active(), filtro_classificacao_default))

      map_subtitle <- paste0(
        ifelse(any(c(modified_sexo, modified_idade, modified_raca_cor,
                     modified_comorbidades, modified_populacao_especial, modified_evolucao, modified_classificacao)),
               "Filtros: ", ""),
        paste(
          c(ifelse(modified_sexo, paste0("<b>Sexo:</b> ", paste(filtro_sexo_active(), collapse = ", ")), NA),
            ifelse(modified_idade, paste0("<b>Idade:</b> ", paste(filtro_idade_active(), collapse = "-")), NA),
            ifelse(modified_raca_cor, paste0("<b>Raça/cor:</b> ", paste(filtro_raca_cor_active(), collapse = ", ")), NA),
            ifelse(modified_comorbidades, paste0("<b>Comorbidade:</b> ", paste(filtro_comorbidades_active(), collapse = ", ")), NA),
            ifelse(modified_populacao_especial, paste0("<b>População especial:</b> ", paste(filtro_populacao_especial_active(), collapse = ", ")), NA),
            ifelse(modified_evolucao, paste0("<b>Evolução:</b> ", paste(filtro_evolucao_active(), collapse = ", ")), NA),
            ifelse(modified_classificacao, paste0("<b>Classificação:</b> ", paste(filtro_classificacao_active(), collapse = ", ")), NA)) |>
            purrr::discard(is.na),
          collapse = "; "
        )

      )
      # KPI clicado vem na frente
      if (!is.null(kpi_sel())) {
        map_subtitle <- paste0("<b>Mostrando no mapa:</b> ", KPI_ROTULOS[[kpi_sel()]],
                               if (nzchar(map_subtitle)) paste0(" · ", map_subtitle) else "")
      }
      map_subtitle
    })

    output$main_map_subtitle <- shiny::renderUI({
      shiny::req(nzchar(texto_filtros()))
      htmltools::tagList(
        htmltools::span(shiny::icon("filter"), htmltools::HTML(texto_filtros())),
        # com um KPI selecionado: volta a mostrar todos os casos
        if (!is.null(kpi_sel())) {
          htmltools::tags$button(
            type = "button", class = "pv-mapa-voltar",
            onclick = sprintf("Shiny.setInputValue('%s', 'total', {priority: 'event'})", ns("kpi_clicado")),
            shiny::icon("rotate-left"), "Voltar à visualização geral"
          )
        }
      )
    })

    # Resumo do periodo/filtros nas abas sem o mapa (o filtro de data fica
    # na janela flutuante do mapa)
    resumo_filtros_ui <- function() {
      p <- periodo()
      filtros_txt <- texto_filtros()
      htmltools::div(
        class = "pv-resumo-filtros",
        shiny::icon("calendar-days"),
        htmltools::HTML(sprintf(
          "<span>Primeiros sintomas de <b>%s</b> a <b>%s</b>%s</span>",
          format(p[1], "%d/%m/%Y"), format(p[2], "%d/%m/%Y"),
          if (nzchar(filtros_txt)) paste0(" · ", filtros_txt) else " · sem outros filtros"
        )),
        shiny::actionLink(ns("ir_para_mapa"), "Alterar no mapa", icon = shiny::icon("map-location-dot"))
      )
    }
    output$resumo_filtros <- shiny::renderUI(resumo_filtros_ui())
    output$resumo_filtros_tabela <- shiny::renderUI(resumo_filtros_ui())

    shiny::observeEvent(input$ir_para_mapa, {
      shiny::updateTabsetPanel(session, "abas", selected = "mapa")
    })


    ## KPIs ----
    # Todos os cards saem de um unico calculo (cards()), cacheado no processo.

    output$kpi_total <- shiny::renderText(fmt_int(cards()$total))
    output$kpi_investigacao <- shiny::renderText(fmt_int(cards()$investigacao))
    output$kpi_investigacao_30d <- shiny::renderText(paste(
      scales::percent(cards()$prop_investigacao_30d, accuracy = 0.1, big.mark = ".", decimal.mark = ","),
      "há +30 dias"
    ))
    output$kpi_cura <- shiny::renderText(fmt_int(cards()$cura))
    output$kpi_obitos <- shiny::renderText(fmt_int(cards()$obitos))
    output$kpi_grave <- shiny::renderText(fmt_int(cards()$grave))
    output$kpi_alarme <- shiny::renderText(fmt_int(cards()$alarme))
    output$kpi_gestante <- shiny::renderText(fmt_int(cards()$gestante))

    ## Table output ----

    output$casos_table <- reactable::renderReactable({

      evolucao_cont <- evolucao_ids |>
        dplyr::mutate(n = cards()$evolucao) |>
        dplyr::mutate(prop = n/max(1, sum(n)))

      # Render a bar chart with a label on the left. source: https://glin.github.io/reactable/articles/cookbook/cookbook.html
      bar_chart <- function(label, width = "100%", height = "1rem", fill = "#00bfc4", background = NULL) {
        bar <- htmltools::div(style = list(background = fill, width = width, height = height))
        chart <- htmltools::div(style = list(flexGrow = 1, marginLeft = "0.5rem", background = background), bar)
        htmltools::div(style = list(display = "flex", alignItems = "center", fontWeight = 600), label, chart)
      }

      evolucao_cont |>
        reactable::reactable(
        columns = list(
          evolucao = reactable::colDef(name = "Evolução"),
          evolucao_id = reactable::colDef(show=F),
          n = reactable::colDef(
            name = "Frequência",
            cell = function(value, index) {
              evo <- evolucao_cont$evolucao[index]
              prop <- evolucao_cont$prop[index]

              width <- paste0(value/max(1, max(evolucao_cont$n)) * 100, "%")
              bar_chart(paste0(fmt_int(value), " (",
                               scales::percent(prop, big.mark = ".", decimal.mark = ",", accuracy = 0.1), ")"),
                        fill = EVOLUCAO_CORES[[evo]],
                        width = width)
            }
          ),
          prop = reactable::colDef(show=F)
        ),
        defaultSorted = list(n = "desc"),
        defaultPageSize = 15,
        sortable = F,
        selection = "multiple",
        defaultSelected = seq_len(nrow(evolucao_cont)),
        language = reactable_idioma
      )
    })
    # A selecao desta tabela decide quais evolucoes aparecem no mapa: ela tem
    # que continuar ativa mesmo com a secao/painel recolhido
    shiny::outputOptions(output, "casos_table", suspendWhenHidden = FALSE)

    # Tabela de casos com paginacao/ordenacao/filtro no servidor (DT
    # server-side): o navegador so' recebe a pagina visivel (10-100 linhas),
    # nao importa se o filtro tem 5 mil ou 400 mil casos. Badges e link da
    # notificacao sao desenhados em JavaScript, sem funcao R por linha.
    output$tabela_casos <- DT::renderDT({
      d <- dados()
      casos_df <- d$casos[casos_idx(), TABELA_COLS, drop = FALSE]
      casos_df$idade <- as.integer(floor(casos_df$idade))
      casos_df$georreferenciado <- factor(casos_df$georreferenciado, levels = c(0, 1), labels = c("Não ❌", "Sim ✅"))
      for (col in c("sexo", "raca_cor", "classificacao", "evolucao")) {
        casos_df[[col]] <- factor(casos_df[[col]])
      }

      cabecalho <- htmltools::withTags(table(
        class = "display",
        thead(
          tr(
            th(rowspan = 2, "Núm. Notificação"),
            th(colspan = 3, "Datas", style = "text-align:center"),
            th(rowspan = 2, "Idade"),
            th(rowspan = 2, "Sexo"),
            th(rowspan = 2, "Raça/cor"),
            th(rowspan = 2, "Classificação final"),
            th(colspan = 3, "Encerramento", style = "text-align:center"),
            th(rowspan = 2, "Georreferenciado")
          ),
          tr(
            th("Notificação"), th("Primeiros sintomas"), th("Óbito"),
            th("Data de encerramento"), th("Evolução"), th("Período até encerramento")
          )
        )
      ))

      names_js <- jsonlite::toJSON(names(EVOLUCAO_CORES))

      DT::datatable(
        casos_df,
        container = cabecalho,
        rownames = FALSE,
        filter = "top",
        selection = "none",
        class = "compact stripe hover",
        callback = DT::JS(sprintf(
          "table.on('click', 'a.ver-caso', function(e) {
             e.preventDefault();
             Shiny.setInputValue('%s', {id: $(this).attr('data-id')}, {priority: 'event'});
           });",
          ns("ver_detalhes_casos")
        )),
        options = list(
          pageLength = 10,
          lengthMenu = c(10, 25, 50, 100),
          order = list(),
          processing = TRUE,
          scrollX = TRUE,
          language = dt_idioma,
          columnDefs = list(
            list(targets = 0, className = "dt-center", render = DT::JS(
              "function(data, type) {
                 if (type !== 'display' || data === null) return data;
                 return '<a href=\"#\" class=\"ver-caso\" data-id=\"' + data + '\" ' +
                   'style=\"color:#154499;font-weight:bold;text-decoration:underline;\">' + data + '</a>';
               }"
            )),
            list(targets = 9, className = "dt-center", render = DT::JS(sprintf(
              "function(data, type) {
                 if (type !== 'display' || data === null) return data;
                 var cores = %s, nomes = %s, i = nomes.indexOf(data);
                 var cor = i >= 0 ? cores[i] : '#aaaaaaaa';
                 return '<span style=\"background-color:' + cor + ';color:white;padding:5px 15px;' +
                   'border-radius:20px;display:inline-block;font-size:12px;font-weight:bold;\">' + data + '</span>';
               }",
              jsonlite::toJSON(unname(paste0(EVOLUCAO_CORES, "aa"))), names_js
            )))
          )
        )
      ) |>
        DT::formatDate(
          c("dt_notificacao", "dt_sintomas", "dt_obito", "dt_encerramento"),
          method = "toLocaleDateString",
          params = list("pt-BR", list(timeZone = "UTC"))
        ) |>
        DT::formatString("tempo_ate_encerramento", suffix = " dias")
    }, server = TRUE)

    ## Map output ----
    # O mapa base e' desenhado uma vez so'; filtros/indicadores atualizam so'
    # as camadas via leafletProxy (sem recriar tiles, controles nem o widget).

    output$main_map <- leaflet::renderLeaflet({
      # zoom no canto inferior direito (o painel flutuante ocupa o lado
      # esquerdo e a janela de periodo o topo), com um botao de centralizar
      # logo acima: volta pra vista inicial (o DF inteiro)
      leaflet::leaflet(options = leaflet::leafletOptions(zoomControl = FALSE)) |>
        htmlwidgets::onRender("function(el) {
          var map = this, centro = map.getCenter(), zoom = map.getZoom();
          L.control.zoom({ position: 'bottomright', zoomInTitle: 'Aproximar', zoomOutTitle: 'Afastar' }).addTo(map);
          var Centralizar = L.Control.extend({
            options: { position: 'bottomright' },
            onAdd: function() {
              var bar = L.DomUtil.create('div', 'leaflet-bar pv-centralizar');
              var a = L.DomUtil.create('a', '', bar);
              a.href = '#'; a.role = 'button';
              a.title = 'Centralizar no DF'; a.setAttribute('aria-label', 'Centralizar no DF');
              a.innerHTML = '<i class=\"fas fa-crosshairs\"></i>';
              L.DomEvent.disableClickPropagation(bar);
              L.DomEvent.on(a, 'click', function(e) {
                L.DomEvent.preventDefault(e);
                map.flyTo(centro, zoom, { duration: 0.6 });
              });
              return bar;
            }
          });
          new Centralizar().addTo(map);
        }") |>
        # CartoDB.Positron passou a exigir API key nos tiles gratuitos --
        # trocado por Esri.WorldGrayCanvas (visual parecido: cinza claro,
        # minimalista, sem exigir chave/conta).
        leaflet::addProviderTiles("Esri.WorldGrayCanvas", group = 'Ruas') |>
        leaflet::addProviderTiles("Esri.WorldImagery",
                                  options = leaflet::providerTileOptions(opacity = 0.8),
                                  group = 'Satélite') |>
        leaflet::addMapPane("coropletico", zIndex = 350) |>
        leaflet::addMapPane("contornoRA", zIndex = 450) |>
        leaflet::addMapPane("topLayer", zIndex = 500) |>
        leaflet::setView(lng = -47.93, lat = -15.78, zoom = 10) |>
        leaflet::addLayersControl(
          baseGroups = c("Ruas", "Satélite"),
          options = leaflet::layersControlOptions(collapsed = FALSE),
          overlayGroups = c("Casos", "Indicador territorial", "Unidades de saúde (CNES)")
        )
    })

    # leafletProxy so' funciona depois que o widget existe no navegador
    map_ready <- shiny::reactiveVal(FALSE)
    shiny::observeEvent(input$main_map_zoom, map_ready(TRUE), once = TRUE)

    #### Camada: indicador territorial (RA ou setor) ----
    # Desenhada em tiles por territorio_layer.js (a cor nunca some ao arrastar
    # ou dar zoom): geometria de cada unidade enviada 1x por sessao, depois so'
    # cores e valores do tooltip.

    geo_enviada <- shiny::reactiveVal(character())

    enviar_geometria <- function(d, un) {
      if (un %in% shiny::isolate(geo_enviada())) return()
      if (un == "setor") {
        geojson <- d$setores_geojson
        nomes <- paste0("Setor ", d$setores$codigo, " — ", d$setores$ra_nome)
      } else {
        geojson <- d$bairros_geojson
        nomes <- d$bairros$name_subdistrict
      }
      session$sendCustomMessage("pvdengue-territorio-geo", list(unidade = un, geojson = geojson, nomes = nomes))
      geo_enviada(c(shiny::isolate(geo_enviada()), un))
    }

    shiny::observe({
      shiny::req(map_ready())
      d <- dados()
      un <- unidade()

      # deferUntilFlush = FALSE: as chamadas do proxy saem na hora, na mesma
      # ordem das mensagens de territorio_layer.js
      proxy <- leaflet::leafletProxy("main_map", session, deferUntilFlush = FALSE) |>
        leaflet::clearGroup("Distrito Federal") |>
        leaflet::removeControl("legenda_ind")

      if (!isTRUE(input$show_indicador_bairro)) {
        session$sendCustomMessage("pvdengue-coropletico", list(map = ns("main_map"), remover = TRUE))
        proxy |>
          leaflet::addPolygons(
            data = d$bairros,
            weight = 2,
            color = "#2c7fb8",
            fillOpacity = 0.2,
            group = 'Distrito Federal'
          )
        return()
      }

      s <- indicador_territorial()

      enviar_geometria(d, un)
      if (un == "setor") enviar_geometria(d, "ra")  # contorno das RAs por cima
      session$sendCustomMessage("pvdengue-coropletico", list(
        map = ns("main_map"),
        group = "Indicador territorial",
        unidade = un,
        contornoRA = un == "setor",
        cores = unname(s$cores),
        linhas = unname(Map(function(rotulo, valores) list(rotulo, unname(valores)), names(s$linhas), s$linhas))
      ))

      valores_legenda <- s$valores[!is.na(s$valores)]
      if (length(valores_legenda) > 0) {
        proxy |>
          leaflet::addLegend(pal = s$pal, values = valores_legenda, opacity = 0.8, title = s$titulo,
                             labFormat = if (is.null(s$lab)) leaflet::labelFormat() else s$lab,
                             position = "bottomright", layerId = "legenda_ind")
      }
    })

    output$nota_indicador <- shiny::renderUI({
      ind <- input$indicador_bairro
      shiny::req(ind %in% INDICADORES_ESTRATIFICACAO)
      e <- dados()$estratificacao[[unidade()]]
      shiny::req(e)
      htmltools::p(
        style = "color: gray; font-size: 13px;",
        sprintf(
          "Estratificação %s–%s (situação recente: %s), mesma metodologia do relatório de estratificação por setor censitário. Não depende do filtro de data nem dos demais filtros. Estratos = quartis do escore, calculados separadamente para RA e para setor.",
          e$ano_inicial[1], e$ano_final[1], e$anos_recentes[1]
        )
      )
    })

    ## Estratificacao de risco: resumo, grafico e tabela ----
    # Seguem a unidade territorial escolhida no mapa (RA ou setor).

    estratificacao_unidade <- shiny::reactive({
      e <- dados()$estratificacao[[unidade()]]
      shiny::validate(shiny::need(!is.null(e), "Estratificação indisponível -- regere os dados (dev/setup_databases.R)."))
      e$rotulo <- if (unidade() == "setor") paste0(e$ra_nome, " — ", e$codigo) else e$nome
      e
    })

    output$estratificacao_resumo <- shiny::renderUI({
      e <- estratificacao_unidade()
      rotulo_unidade <- if (unidade() == "setor") c("setor", "setores") else c("RA", "RAs")
      resumo <- lapply(names(ESTRATO_CORES), function(est) {
        x <- e[e$estrato == est, , drop = FALSE]
        if (nrow(x) == 0) return(NULL)
        htmltools::div(
          style = sprintf("flex: 1 1 150px; border-left: 5px solid %s; background: #f8f9fa; padding: 6px 10px; border-radius: 4px;", ESTRATO_CORES[[est]]),
          htmltools::div(style = "font-weight: 700;", est),
          htmltools::div(style = "font-size: 13px; color: #444;",
                         sprintf("%s %s · pop. %s", fmt_int(nrow(x)), rotulo_unidade[if (nrow(x) == 1) 1 else 2],
                                 fmt_int(sum(x$populacao, na.rm = TRUE)))),
          htmltools::div(style = "font-size: 13px; color: #444;",
                         sprintf("%s casos prováveis · %s óbitos", fmt_int(sum(x$casos_totais)), fmt_int(sum(x$obitos_totais))))
        )
      })

      htmltools::tagList(
        htmltools::p(
          style = "color: #555; font-size: 14px; margin-bottom: 8px;",
          htmltools::HTML(sprintf(
            "Escore composto de risco por <b>%s</b>, %s–%s: <b>I1</b> magnitude (incidência média anual de casos prováveis, peso 0,30), <b>I2</b> recorrência (anos com incidência ≥ 300/100 mil, 0,20), <b>I3</b> situação recente (incidência média de %s, 0,30) e <b>I4</b> gravidade (incidência média de casos com sinais de alarme ou graves, 0,20), normalizados 0–1 (mín–máx). Estratos = quartis do escore. População do Censo 2022. Não depende dos filtros do painel; para trocar RA/setor use a unidade territorial no mapa.",
            if (unidade() == "setor") "setor censitário" else "Região Administrativa",
            e$ano_inicial[1], e$ano_final[1], e$anos_recentes[1]
          ))
        ),
        htmltools::div(style = "display: flex; flex-wrap: wrap; gap: 8px; margin-bottom: 12px;", resumo)
      )
    })

    output$estratificacao_grafico <- plotly::renderPlotly({
      e <- estratificacao_unidade()
      e <- e[!is.na(e$escore), , drop = FALSE]

      if (unidade() == "setor") {
        # 5.418 setores nao cabem num grafico: 10 de maior escore em cada
        # estrato, como no grafico do relatorio
        e <- do.call(rbind, lapply(split(e, e$estrato), function(x) utils::head(x[order(-x$escore), ], 10)))
        titulo <- "10 setores de maior escore em cada estrato"
      } else {
        titulo <- "Escore composto por Região Administrativa"
      }
      e <- e[order(e$escore), , drop = FALSE]

      hover <- sprintf(
        "<b>%s</b><br>Estrato: %s<br>Escore: %s<br>I1: %s · I2: %s · I3: %s · I4: %s<br>População: %s",
        e$rotulo, e$estrato, fmt_dec(e$escore, 4), fmt_dec(e$I1), fmt_int(e$I2),
        fmt_dec(e$I3), fmt_dec(e$I4), fmt_int(e$populacao)
      )

      plotly::plot_ly(
        x = e$escore,
        y = factor(e$rotulo, levels = unique(e$rotulo)),
        type = "bar",
        orientation = "h",
        marker = list(color = unname(ESTRATO_CORES[e$estrato])),
        text = hover,
        hoverinfo = "text",
        textposition = "none"
      ) |>
        plotly::layout(
          title = list(text = titulo, font = list(size = 14)),
          xaxis = list(title = "Escore composto", range = c(0, 1.02), tickformat = ".1f"),
          yaxis = list(title = "", tickfont = list(size = 10), automargin = TRUE),
          margin = list(t = 40, r = 10, b = 40),
          separators = ",.",
          showlegend = FALSE
        ) |>
        plotly::config(displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d", "select2d"))
    })

    output$estratificacao_tabela <- DT::renderDT({
      e <- estratificacao_unidade()
      e <- e[order(-e$escore, na.last = TRUE), , drop = FALSE]
      tab <- data.frame(
        unidade = if (unidade() == "setor") e$codigo else e$nome,
        ra = e$ra_nome,
        populacao = e$populacao,
        casos = e$casos_totais,
        graves = e$graves_totais,
        obitos = e$obitos_totais,
        I1 = e$I1, I2 = e$I2, I3 = e$I3, I4 = e$I4,
        escore = e$escore,
        estrato = factor(e$estrato, levels = names(ESTRATO_CORES))
      )
      nomes <- c(if (unidade() == "setor") "Setor censitário" else "Região Administrativa", "RA",
                 "População", "Casos prováveis", "Casos graves", "Óbitos",
                 "I1 magnitude", "I2 recorrência", "I3 recente", "I4 gravidade", "Escore", "Estrato")
      if (unidade() == "ra") {
        tab$ra <- NULL
        nomes <- nomes[-2]
      }

      cores_js <- jsonlite::toJSON(as.list(ESTRATO_CORES), auto_unbox = TRUE)

      DT::datatable(
        tab,
        colnames = nomes,
        rownames = FALSE,
        filter = "top",
        selection = "none",
        class = "compact stripe hover",
        options = list(
          pageLength = 15,
          lengthMenu = c(15, 33, 50, 100),
          order = list(),
          scrollX = TRUE,
          language = dt_idioma,
          columnDefs = list(list(
            targets = ncol(tab) - 1, className = "dt-center",
            render = DT::JS(sprintf(
              "function(data, type) {
                 if (type !== 'display' || data === null) return data;
                 var cores = %s;
                 return '<span style=\"background-color:' + (cores[data] || '#bbb') + ';color:white;padding:3px 12px;' +
                   'border-radius:20px;display:inline-block;font-size:12px;font-weight:bold;\">' + data + '</span>';
               }", cores_js))
          ))
        )
      ) |>
        DT::formatRound(c("populacao", "casos", "graves", "obitos", "I2"), digits = 0, mark = ".", dec.mark = ",") |>
        DT::formatRound(c("I1", "I3", "I4"), digits = 1, mark = ".", dec.mark = ",") |>
        DT::formatRound("escore", digits = 4, mark = ".", dec.mark = ",")
    }, server = TRUE)

    #### Camada: casos ----

    pal_evolucao <- leaflet::colorFactor(unname(EVOLUCAO_CORES), domain = names(EVOLUCAO_CORES))

    shiny::observe({
      shiny::req(map_ready())

      pontos <- pontos_mapa()

      proxy <- leaflet::leafletProxy("main_map", session) |>
        leaflet::clearPopups() |>
        leaflet::removeControl("legenda_casos")

      # Todos os casos do filtro, sem amostra: agrupados em clusters que se
      # abrem ao aproximar o zoom, desenhados por inst/app/www/casos_layer.js.
      # Popups vem do servidor no clique (input$main_map_marker_click).
      session$sendCustomMessage(
        "pvdengue-casos",
        c(list(map = ns("main_map"), group = "Casos"), pontos)
      )

      if (pontos$n > 0) {
        proxy |>
          leaflet::addLegend(
            pal = pal_evolucao, values = pontos$evolucoes_presentes,
            opacity = 0.7, title = NULL,
            position = "bottomright",
            layerId = "legenda_casos"
          )
      }
    })

    shiny::observeEvent(input$main_map_marker_click, {
      click <- input$main_map_marker_click
      shiny::req(!is.null(click$id))
      casos <- dados()$casos
      caso <- casos[match(as.integer(click$id), casos$id), , drop = FALSE]
      shiny::req(nrow(caso) == 1, !is.na(caso$id))

      leaflet::leafletProxy("main_map", session) |>
        leaflet::clearPopups() |>
        leaflet::addPopups(lng = click$lng, lat = click$lat, popup = popup_caso(caso))
    })

    #### Camada: unidades de saude (CNES) ----
    # Marcador "X" fixo pra cada unidade notificadora, com contagem de casos
    # no filtro/periodo atual no hover. Mesmo padrao visual de Cenarios_TB_DF.

    shiny::observe({
      shiny::req(map_ready())

      unidades_geo <- calc_unidades(dados(), periodo(), filtros())

      proxy <- leaflet::leafletProxy("main_map", session) |>
        leaflet::clearGroup("Unidades de saúde (CNES)")

      if (nrow(unidades_geo) == 0) return()

      hover_unidade <- sprintf(
        "<b>Unidade notificadora</b>: %s<br><b>%s</b> caso(s) no período selecionado",
        ifelse(
          is.na(unidades_geo$nome_fantasia) | unidades_geo$nome_fantasia == "",
          htmltools::htmlEscape(unidades_geo$razao_social),
          htmltools::htmlEscape(unidades_geo$nome_fantasia)
        ),
        unidades_geo$n_casos
      ) |> lapply(htmltools::HTML)

      proxy |>
        leaflet::addLabelOnlyMarkers(
          lng = unidades_geo$longitude,
          lat = unidades_geo$latitude,
          label = "✕",
          labelOptions = leaflet::labelOptions(
            noHide = TRUE,
            direction = "center",
            textOnly = TRUE,
            style = list(
              "color" = "#000000",
              "font-weight" = "bold",
              "font-size" = "18px",
              "text-shadow" = "-1px -1px 0 #fff, 1px -1px 0 #fff, -1px 1px 0 #fff, 1px 1px 0 #fff"
            )
          ),
          group = 'Unidades de saúde (CNES)',
          options = leaflet::markerOptions(pane = "topLayer")
        ) |>
        leaflet::addCircleMarkers(
          lng = unidades_geo$longitude,
          lat = unidades_geo$latitude,
          radius = 10,
          stroke = FALSE,
          fillOpacity = 0,
          label = hover_unidade,
          group = 'Unidades de saúde (CNES)',
          options = leaflet::pathOptions(pane = "topLayer")
        )
    })

  })
}
