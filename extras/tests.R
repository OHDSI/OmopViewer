
library(shiny)
library(bslib)
library(palmerpenguins)

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
  title = "My study",
  nav_panel(
    title = "About",
    icon = icon("circle-info"),
    aboutTab() |> rlang::parse_expr() |> eval() |> card()
  ),
  nav_panel(
    title = "Background",
    icon = icon("disease"),
    card()
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
            icon = shiny::icon("circle-info"),
            p("In this panel you can find information about the characteristics of the individuals")
          ),
          bslib::accordion_panel(
            title = "Settings",
            selectizeInput(inputId = "set1", label = "Setting1", choices = unique(penguins$species), multiple = TRUE, options = list(plugins = "remove_button")),
            selectizeInput(inputId = "set2", label = "Setting2", choices = unique(penguins$species), multiple = FALSE)
          ),
          bslib::accordion_panel(
            title = "Groupping",
            selectizeInput(inputId = "group1", label = "Database name", choices = unique(penguins$species), multiple = TRUE, options = list(plugins = "remove_button")),
            selectizeInput(inputId = "group2", label = "Cohort name", choices = unique(penguins$species), multiple = TRUE, options = list(plugins = "remove_button")),
            selectizeInput(inputId = "group3", label = "Sex", choices = unique(penguins$species), multiple = FALSE)
          ),
          bslib::accordion_panel(
            title = "Variables",
            selectizeInput(inputId = "var1", label = "Variables", choices = unique(penguins$species), multiple = TRUE, options = list(plugins = "remove_button")),
            selectizeInput(inputId = "var2", label = "Estimates", choices = unique(penguins$species), multiple = FALSE)
          )
        )
      ),
      card(
        page_navbar(
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
                  selectizeInput(inputId = "tab1", label = "Group", choices = unique(penguins$species), multiple = TRUE, options = list(plugins = "remove_button")),
                  selectizeInput(inputId = "tab2", label = "Header", choices = unique(penguins$species), multiple = TRUE, options = list(plugins = "remove_button")),
                  selectizeInput(inputId = "tab3", label = "Hide", choices = unique(penguins$species), multiple = TRUE, options = list(plugins = "remove_button"))
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
                  selectizeInput(inputId = "plot11", label = "Facet", choices = unique(penguins$species), multiple = TRUE, options = list(plugins = "remove_button")),
                  selectizeInput(inputId = "plot12", label = "Colour", choices = unique(penguins$species), multiple = TRUE, options = list(plugins = "remove_button"))
                ),
                shiny::plotOutput("plot")
              )
            )
          )
        )
      )
    )
  ),
  nav_spacer(),
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
