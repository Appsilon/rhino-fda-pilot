box::use(
  dplyr[mutate, filter, select, case_when],
  shiny[
    shinyApp, tagList, tags, includeMarkdown, moduleServer, NS, bootstrapPage,
    textOutput, renderText, actionButton, observe, bindEvent,
  ],
  shinyWidgets[alert],
  teal[ui_teal_with_splash, modules, module, srv_teal_with_splash],
  teal.data[cdisc_data, cdisc_dataset],
  teal.transform[choices_selected, variable_choices, value_choices],
  teal.modules.clinical[tm_g_km],
  admiral[
    derive_vars_merged, exprs, derive_vars_duration, derive_var_merged_cat,
  ],
)

box::use(
  app / logic / adam_data[get_adsl, get_adas, get_adtte, get_adlb],
  app / view / user_guide,
  app / view / demographic_table,
  app / view / km_plot,
  app / view / primary_table,
  app / view / efficacy_table,
  app / view / completion_table,
)

## Important for application to run
# Possible Rhino bug!!
library(teal.modules.clinical)

arm_cat <- function(arm) {
  case_when(
    arm == "Xanomeline High Dose" ~ "ARM A",
    arm == "Xanomeline Low Dose" ~ "ARM C",
    TRUE ~ "ARM B"
  )
}

arm_ref_comp <- list(
  ACTARMCD = list(
    ref = "ARM B",
    comp = c("ARM A", "ARM C")
  ),
  ARM = list(
    ref = "Placebo",
    comp = c("Xanomeline High Dose", "Xanomeline Low Dose")
  )
)

adsl <- get_adsl() |>
  mutate(
    ARM = ARM |> factor(),
    ARMCD = arm_cat(ARM) |> factor(),
    ACTARMCD = ARMCD |> factor()
  )

adtte <- get_adtte() |>
  derive_vars_merged(
    dataset_add = adsl,
    filter_add = SAFFL == "Y" | STUDYID == "CDISCPILOT01",
    by_vars = exprs(STUDYID, USUBJID),
    new_vars = exprs(ARM, ARMCD)
  ) |>
  derive_vars_duration(
    new_var = AVAL,
    start_date = TRTSDT,
    end_date = ADT,
    out_unit = "months",
    new_var_unit = AVALU
  )

adas <- get_adas()
adlb <- get_adlb()

teal_data <- cdisc_data(
  cdisc_dataset("ADSL", adsl),
  cdisc_dataset("ADAS", adas, keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT", "QSSEQ")),
  cdisc_dataset("ADTTE", adtte),
  cdisc_dataset("ADLB", adlb)
)

teal_modules <- modules(
  module(
    label = "App Information",
    server = function(id, datasets) {
      moduleServer(id, function(input, output, session) {

      })
    },
    ui = function(id, ...) {
      includeMarkdown("app/docs/about.md")
    },
    filters = NULL
  ),
  module(
    label = "Usage Guide",
    ui = user_guide$ui,
    server = user_guide$server,
    filters = NULL
  ),
  module(
    label = "Demographic Table",
    ui = demographic_table$ui,
    server = demographic_table$server,
    filters = NULL
  ),
  module(
    label = "KM plot for TTDE",
    ui = km_plot$ui,
    server = km_plot$server,
    filters = c("ADSL", "ADTTE")
  ),
  module(
    label = "Primary Table",
    ui = primary_table$ui,
    server = primary_table$server,
    filters = NULL
  ),
  module(
    label = "Efficacy Table",
    ui = efficacy_table$ui,
    server = efficacy_table$server,
    filters = NULL
  ),
  module(
    label = "Visit Completion Table",
    ui = completion_table$ui,
    server = completion_table$server,
    filters = NULL
  ),
  tm_g_km(
    label = "Kaplan-Meier plot",
    dataname = "ADTTE",
    arm_var = choices_selected(
      variable_choices(adsl, c("ARM", "ARMCD", "ACTARMCD")), "ARM"
    ),
    paramcd = choices_selected(
      choices = value_choices(adtte, "PARAMCD", "PARAM"), "TTDE"
    ),
    arm_ref_comp = arm_ref_comp,
    strata_var = choices_selected(
      variable_choices(adsl, c("SEX")), "SEX"
    ),
    facet_var = choices_selected(
      variable_choices(adsl, c("SEX")), NULL
    ),
    conf_level = choices_selected(c(0.95, 0.9, 0.8), 0.95, keep_order = TRUE),
    pre_output = tags$div(
      alert(
        class = "top-margin",
        tagList(
          tags$b("Important Information:"),
          tags$p(
            "The analyses performed when utilizing subgroups or
            other subsets of the source data sets are considered ",
            tags$b("exploratory.")
          ),
          tags$ul(
            tags$li(
              "Treatment information variables from the",
              tags$b("ADTTE"),
              "data set are excluded from the variable list.
              Use the treatment variables present in the",
              tags$b("ADSL"),
              "set to perform treatment-related filters."
            ),
            tags$li(
              "In rare situations, applying filters with variables from both",
              tags$b("ADSL"), "and", tags$b("ADTTE"),
              "that overlap in content could result in an invalid data subset.
              When possible, select variables with distinct content."
            )
          )
        ),
        status = "info",
        dismissible = TRUE
      )
    )
  )
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tags$div(
    class = "dark color-mode",
    ui_teal_with_splash(
      title = "Pilot 2 Shiny Rhino Application",
      id = ns("teal_wrapper"),
      data = teal_data,
      header = tags$div(
        class = "flex",
        tags$h1(
          "Pilot 2 Shiny Application",
          tags$span(
            class = "text-muted text-smaller text-italic",
            "(using Rhino framework)"
          )
        ),
        tags$div(
          class = "logos-wrapper dark",
          actionButton(
            ns("mode_dark"),
            class = "color-mode-toggle",
            title = "Switch to light mode",
            "☀️"
          ),
          tags$a(
            href = "https://rconsortium.github.io/submissions-wg/",
            target = "_blank",
            tags$img(class = "logo", src = "static/logos/rconsortium_dark.svg")
          )
        ),
        tags$div(
          class = "logos-wrapper light",
          actionButton(
            ns("mode_light"),
            class = "color-mode-toggle",
            title = "Switch to light mode",
            "🌑"
          ),
          tags$a(
            href = "https://rconsortium.github.io/submissions-wg/",
            target = "_blank",
            tags$img(class = "logo", src = "static/logos/rconsortium.svg")
          )
        )
      ),
      footer = tagList(
        tags$p(
          class = "text-muted",
          "Source: R Consortium. Adapted to a Rhino application by Appsilon."
        ),
        tags$div(
          class = "logos-wrapper dark",
          tags$a(
            href = "https://rconsortium.github.io/submissions-wg/",
            target = "_blank",
            tags$img(class = "logo", src = "static/logos/rconsortium_dark.svg")
          ),
          tags$a(
            href = "https://appsilon.com",
            target = "_blank",
            tags$img(class = "logo", src = "static/logos/appsilon_dark.svg")
          )
        ),
        tags$div(
          class = "logos-wrapper light",
          tags$a(
            href = "https://rconsortium.github.io/submissions-wg/",
            target = "_blank",
            tags$img(class = "logo", src = "static/logos/rconsortium.svg")
          ),
          tags$a(
            href = "https://appsilon.com",
            target = "_blank",
            tags$img(class = "logo", src = "static/logos/appsilon.svg")
          )
        )
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    srv_teal_with_splash(id = "teal_wrapper", data = teal_data, modules = teal_modules)

    observe({
      session$sendCustomMessage("toggle_dark", "switch")
    }) |>
      bindEvent(input$mode_dark, input$mode_light, once = FALSE, ignoreInit = TRUE)
  })
}
