
#' Launch a dynamic shiny app wher you can upload results.
#'
#' @return Launches the shiny app.
#' @export
#'
launchDynamicApp <- function() {
  shiny::shinyApp(ui = uiDynamic(), server = serverDynamic)
}

#' Export and launch a static shiny specific to the provided results.
#'
#' @param data List of summarised_result objects to build the shiny app.
#' @param directory Directory to create the shiny.
#' @param launch Whether to launch the shiny app.
#'
#' @return The shiny app will be created in directory.
#' @export
#'
exportStaticApp <- function(data = list(),
                            directory = getwd(),
                            launch = TRUE) {
  # input check
  if (!rlang::is_bare_list(data)) data <- list(data)
  omopgenerics::assertList(data, class = "data.frame")
  omopgenerics::assertCharacter(directory, length = 1)
  omopgenerics::assertLogical(launch, length = 1)

  # create directory if it does not exit
  if (!dir.exists(directory)) {
    cli::cli_inform(c("i" = "Provided directory does not exist, it will be created."))
    dir.create(path = directory, recursive = TRUE)
    cli::cli_inform(c("v" = "directory created: {.pkg {directory}}"))
  }

  # processing data
  cli::cli_inform(c("i" = "Processing data"))
  data <- bindData(data)
  resType <- omopgenerics::settings(data)[["result_type"]] |> unique()
  mes <- "Data processed: {length(resType)} result type{?s} idenfied"
  if (length(resType) == 0) {
    mes <- c("!" = paste0(mes, "."))
  } else {
    mes <- c("v" = paste0(mes, ": {.var {resType}}."))
  }
  cli::cli_inform(mes)

  # create shiny
  cli::cli_inform(c("i" = "Creating shiny from provided data"))

  cli::cli_inform(c("v" = "Shiny created in: {.pkg {directory}}"))

  # launch shiny
  if (launch) {
    cli::cli_inform(c("i" = "Launching shiny"))
    #shiny::shinyAppDir(here::here(directory, "shiny"))
  }

  return(invisible())
}
