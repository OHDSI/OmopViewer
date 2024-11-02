
library(shiny)
library(bslib)
library(palmerpenguins)

aboutTab2 <- function() {
  subtitle <- 'shiny::tagList(
    "This shiny app was generated with ",
    shiny::a(
      "omopViewer",
      href = "https://github.com/oxford-pharmacoepi/omopViewer",
      target = "_blank"),
    shiny::strong("v{as.character(utils::packageVersion("omopViewer"))}")
  )' |>
    glue::glue()
  description <- 'shiny::tagList(
    "omopViewer works only with `summarised_result` objects as defined in ",
    shiny::a(
      "omopgenerics",
      href = "https://github.com/darwin-eu-dev/omopgenerics",
      target = "_blank"),
    "."
  )'
  'shiny::div(
    class = "about",
    shiny::tags$img(
      src = "hds_logo.svg",
      class = "logo-img",
      alt = "Logo",
      height = "auto",
      width = "30%",
      style = "float:right"
    ),
    shiny::tags$p({subtitle}),
    shiny::tags$p({description})
  )' |>
    glue::glue() |>
    styleCode() |>
    paste0(collapse = "\n")
}

link_shiny <- tags$a(
  shiny::icon("github"), "Github",
  href = "https://github.com/oxford-pharmacoepi",
  target = "_blank"
)
link_posit <- tags$a(
  shiny::icon("r-project"), "Posit",
  href = "https://posit.co",
  target = "_blank"
)

# Define the UI
ui <- page_navbar(
  title = tags$span(
    tags$img(
      src = "hds_logo.svg",
      width = "auto",
      height = "46px",
      class = "me-3",
      alt = "logo"
    ),
    "My study"
  ),
  nav_panel(
    title = "Background",
    icon = icon("disease"),
    card(
      card_header("Study background"),
      shiny::p("You can use this section to add some background of your study"),
      tags$img(
        src = "hds_logo.svg",
        width = "auto",
        height = "100px",
        alt = "logo",
        style = "float:left"
      )
    )
  ),
  nav_panel(
    title = "Cohort Characteristics",
    icon = shiny::icon("users-gear"),
    layout_sidebar(
      sidebar = sidebar(
        bslib::accordion(
          open = "Information",
          bslib::accordion_panel(
            title = "Information",
            icon = shiny::icon("circle-info")
          ),
          bslib::accordion_panel(
            title = "Settings",
            shinyWidgets::pickerInput(inputId = "set1", label = "Setting1", choices = unique(penguins$species), multiple = TRUE, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")),
            shinyWidgets::pickerInput(inputId = "set2", label = "Setting2", choices = unique(penguins$species), multiple = FALSE)
          ),
          bslib::accordion_panel(
            title = "grouping",
            shinyWidgets::pickerInput(inputId = "group1", label = "Database name", choices = unique(penguins$species), multiple = TRUE, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")),
            shinyWidgets::pickerInput(inputId = "group2", label = "Cohort name", choices = unique(penguins$species), multiple = TRUE, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")),
            shinyWidgets::pickerInput(inputId = "group3", label = "Sex", choices = unique(penguins$species), multiple = FALSE)
          ),
          bslib::accordion_panel(
            title = "Variables",
            shinyWidgets::pickerInput(inputId = "var1", label = "Variables", choices = unique(penguins$species), multiple = TRUE, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")),
            shinyWidgets::pickerInput(inputId = "var2", label = "Estimates", choices = unique(penguins$species), multiple = FALSE)
          )
        )
      ),
      navset_card_tab(
        nav_panel(
          title = "Raw data",
          card(
            full_screen = TRUE,
            DT::datatable(omopgenerics::emptySummarisedResult())
          )
        ),
        nav_panel(
          title = "Formatted table",
          card(
            full_screen = TRUE,
            card_header(
              popover(
                icon("download"),
                downloadButton("download_gt")
              ),
              class = "text-end"
            ),
            layout_sidebar(
              sidebar = sidebar(
                position = "right",
                shinyWidgets::pickerInput(inputId = "tab1", label = "Group", choices = unique(penguins$species), multiple = TRUE, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")),
                shinyWidgets::pickerInput(inputId = "tab2", label = "Header", choices = unique(penguins$species), multiple = TRUE, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")),
                shinyWidgets::pickerInput(inputId = "tab3", label = "Hide", choices = unique(penguins$species), multiple = TRUE, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
              ),
              gt::gt(dplyr::tibble(a = 1))
            )
          )
        ),
        nav_panel(
          title = "Plot 1",
          card(
            full_screen = TRUE,
            card_header(
              popover(
                icon("download"),
                numericInput(
                  inputId = "download_dpi",
                  label = "dpi",
                  value = 300
                ),
                downloadButton("download_plot")
              ),
              class = "text-end"
            ),
            layout_sidebar(
              sidebar = sidebar(
                position = "right",
                shinyWidgets::pickerInput(inputId = "plot11", label = "Facet", choices = unique(penguins$species), multiple = TRUE, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")),
                shinyWidgets::pickerInput(inputId = "plot12", label = "Colour", choices = unique(penguins$species), multiple = TRUE, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
              ),
              shiny::plotOutput("plot")
            )
          )
        )
      )
    )
  ),
  nav_spacer(),
  nav_item(
    popover(
      icon("circle-info"),
      aboutTab2() |> rlang::parse_expr() |> eval()
    )
  ),
  nav_item(
    input_dark_mode(id = "dark_mode", mode = "light")
  ),
  nav_menu(
    title = "Links",
    align = "right",
    nav_item(link_shiny),
    nav_item(link_posit)
  )
)

# Define the server logic
server <- function(input, output, session) {
  #bs_themer()
  output$plot <- renderPlot({
    ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(displ, hwy, colour = class)) +
      ggplot2::geom_point()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
