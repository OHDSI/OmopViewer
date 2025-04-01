populatePanelDetailsOptions <- function(panelDetails, result) {
  panelDetails |>
    # populate automatic filters
    populateAutomaticFilters() |>
    # populate inputId and outputId names
    populateIds() |>
    # create filter and render function name
    createFunctionNames() |>
    # populate filter and render function name
    populateFunctionNames() |>
    # populate in filter the prefix and the name of the function
    populateInputIds() |>
    # populate <values>
    populateValues(result)
}
populateValues <- function(panelDetails, result) {
  panelDetails |>
    purrr::map(\(pd) {
      # filter result
      res <- result |>
        filterResult(pd$data$result_id, pd$data$result_type)
      # get values
      values <- res |>
        dplyr::select(!c("estimate_type", "estimate_value")) |>
        dplyr::distinct() |>
        omopgenerics::splitAll() |>
        omopgenerics::addSettings() |>
        dplyr::select(!"result_id") |>
        purrr::map(unique)
      values$group <- omopgenerics::groupColumns(res)
      values$strata <- omopgenerics::strataColumns(res)
      values$additional <- omopgenerics::additionalColumns(res)
      values$settings <- omopgenerics::settingsColumns(res)

      # populate filters
      pd$filters <- pd$filters |>
        purrr::map(\(filt) {
          filt$choices <- substituteValues(filt$choices, values)
          filt$selected <- substituteValues(filt$selected, values)
          filt
        })
      pd$content <- pd$content |>
        purrr::map(\(cont) {
          cont$filters <- cont$filters |>
            purrr::map(\(filt) {
              filt$choices <- substituteValues(filt$choices, values)
              filt$selected <- substituteValues(filt$selected, values)
              filt
            })
          cont
        })

      pd
    })
}
substituteValues <- function(x, values) {
  if (is.null(x)) return(x)
  id <- stringr::str_detect(x, "^<.*>$")
  for (k in which(id)) {
    x <- subs(x = x, pat = id, subst = values[[keyWord]])
  }
  x[nchar(x) > 0]
}
populateIds <- function(panelDetails) {
  panelDetails |>
    purrr::imap(\(x, nmp) {
      x$filters <- x$filters |>
        purrr::imap(\(x, nmf) addInputId(x, paste0(nmp, "_", nmf)))
      x$content <- x$content |>
        purrr::imap(\(cont, nmc) {
          cont <- addOutputId(cont, paste0(nmp, "_", nmc))
          cont$filters <- cont$filters |>
            purrr::imap(\(x, nmf) addInputId(x, paste0(nmp, "_", nmc, "_", nmf)))
          cont$download$filters <- cont$download$filters |>
            purrr::imap(\(x, nmf) addInputId(x, paste0(nmp, "_", nmc, "_", nmf)))
          cont$download <- addOutputId(cont$download, paste0(nmp, "_", nmc, "_download"))
          cont
        })
      x
    })
}
addInputId <- function(x, def) {
  if (!is.null(x)) {
    if ("inputId" %in% names(x)) {
      x$input_id <- x$inputId
    } else if ("input_id" %in% names(x)) {
      x$inputId <- x$input_id
    } else {
      x$input_id <- def
      x$inputId <- def
    }
  }
  x
}
addOutputId <- function(x, def) {
  if (!is.null(x) & !"output_id" %in% names(x)) {
    x$output_id <- def
  }
  x
}
createFunctionNames <- function(panelDetails) {
  panelDetails |>
    # filter function name
    purrr::imap(\(x, nm) {
      if (!"filter_function" %in% names(x)) {
        x$filter_function <- paste0("get", formatCamel(paste0(nm, "_data")))
      }
      x
    }) |>
    # render function name
    purrr::imap(\(x, nmp) {
      x$content <- x$content |>
        purrr::imap(\(cont, nmc) {
          if (!"render_function" %in% names(cont)) {
            cont$render_function <- paste0("get", formatCamel(paste0(
              nmp, "_", nmc
            )))
          }
          cont
        })
      x
    })
}
populateFunctionNames <- function(panelDetails) {
  panelDetails |>
    purrr::map(\(pd) {
      filterFunctionName <- paste0(pd$filter_function, "()")
      pd$content <- pd$content |>
        purrr::map(\(cont) {
          renderFunctionName <- paste0(cont$render_function, "()")
          cont$render <- cont$render |>
            substituteFunctionNames(filterFunctionName, renderFunctionName)
          cont$download$render <- cont$download$render |>
            substituteFunctionNames(filterFunctionName, renderFunctionName)
          cont
        })
      pd
    })
}
substituteFunctionNames <- function(x, ffn, rfn) {
  if (is.null(x)) return(x)
  x |>
    stringr::str_replace_all("<filtered_data>", ffn) |>
    stringr::str_replace_all("<rendered_data>", rfn)
}
populateInputIds <- function(panelDetails) {
  panelDetails |>
    purrr::map(\(pd) {
      pd$content <- pd$content |>
        purrr::map(\(cont) {
          # where to find the inputs
          inputsToSubstitute <- c(
            cont$render, cont$download$render, cont$download$filename
          ) |>
            # split in words
            stringr::str_split(pattern = "[[:punct:]&&[^_]]|\\s+") |>
            unlist() |>
            unique() |>
            # find words that start with input$
            purrr::keep(\(x) stringr::str_starts(x, "input\\$")) |>
            rlang::set_names() |>
            # find the id
            purrr::map_chr(\(x) {
              id <- stringr::str_sub(x, start = 7, end = nchar(x))
              if (id %in% names(pd$filters)) {
                nm <- pd$filters[[id]]$input_id
              } else if (id %in% names(cont$filters)) {
                nm <- cont$filters[[id]]$input_id
              } else if (id %in% names(cont$download$filters)) {
                nm <- cont$download$filters[[id]]$input_id
              } else {
                nm <- NULL
                cli::cli_warn("filter {id} not found!")
              }
              paste0("input$", nm)
            })
          for (k in seq_along(inputsToSubstitute)) {
            new <- unname(inputsToSubstitute[k])
            original <- names(inputsToSubstitute)[k] |>
              stringr::str_replace_all(pattern = "\\$", replacement = "\\\\$")
            cont$render <- cont$render |>
              stringr::str_replace_all(pattern = original, replacement = new)
            cont$download$render <- cont$download$render |>
              stringr::str_replace_all(pattern = original, replacement = new)
            cont$download$filename <- cont$download$filename |>
              stringr::str_replace_all(pattern = original, replacement = new)
          }
          cont
        })
      pd
    })
}
populateAutomaticFilters <- function(panelDetails) {
  panelDetails |>
    purrr::map(\(pd) {
      res <- result |>
        filterResult(pd$data$result_id, pd$data$result_type)
      values <- list()
      values$group <- omopgenerics::groupColumns(res)
      values$strata <- omopgenerics::strataColumns(res)
      values$additional <- omopgenerics::additionalColumns(res)
      values$settings <- omopgenerics::settingsColumns(res)

      # create automatic filters
      automaticFilters <- unique(pd$automatic_filters) |>
        purrr::map(\(x) {
          if (!x %in% c("group", "strata", "additional", "settings")) {
            rlang::set_names("main", x)
          } else {
            vals <- values[[x]]
            rlang::set_names(rep(x, length(vals)), vals)
          }
        }) |>
        purrr::flatten_chr()
      # exclude filters
      automaticFilters <- automaticFilters[
        !names(automaticFilters) %in% as.character(unique(pd$exclude_filters))
      ]
      # create filters
      automaticFilters <- automaticFilters |>
        purrr::imap(\(x, nm) {
          list(
            button_type = "pickerInput",
            label = formatTit(nm),
            column = nm,
            column_type = x,
            choices = "choices$",
            selected = "selected$",
            multiple = TRUE
          )
        })
      pd$filters <- c(pd$filters, automaticFilters)
      pd$automatic_filters <- NULL
      pd$exclude_filters <- NULL
      pd
    })
}
