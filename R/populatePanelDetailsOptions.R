populatePanelDetailsOptions <- function(panelDetails, result) {
  panelDetails |>
    # populate automatic filters
    populateAutomaticFilters(result) |>
    # populate inputId and outputId names
    populateIds() |>
    # create filter and render function name
    createFunctionNames() |>
    # populate filter and render function name
    populateFunctionNames() |>
    # populate in filter the prefix and the name of the function
    populateInputIds() |>
    # populate <values>
    populateValues(result) |>
    # populate choices$ and selected$
    populateChoicesSelected() |>
    # populate <prefix> and <panel>
    populatePrefixPanel()
}
populateValues <- function(panelDetails, result) {
  panelDetails |>
    purrr::map(\(pd) {
      # filter result
      res <- filterResult(result, pd$data)
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
          filt$labels <- substituteValues(filt$labels, values)
          filt
        })
      pd$content <- pd$content |>
        purrr::map(\(cont) {
          cont$filters <- cont$filters |>
            purrr::map(\(filt) {
              filt$choices <- substituteValues(filt$choices, values)
              filt$selected <- substituteValues(filt$selected, values)
              filt$labels <- substituteValues(filt$labels, values)
              filt
            })
          cont
        })
      pd
    })
}
substituteValues <- function(x, values) {
  if (is.null(x)) return(x)
  keys <- stringr::str_extract(x, "(?<=<)[^>]+(?=>)") |>
    purrr::keep(\(x) !is.na(x)) |>
    unique()
  for (key in keys) {
    x <- subs(x = x, pat = paste0("<", key, ">"), subst = values[[key]])
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
          if (!"reactive_function" %in% names(cont)) {
            cont$reactive_function <- paste0("get", formatCamel(paste0(
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
          reactiveFunctionName <- paste0(cont$reactive_function, "()")
          cont$observer <- cont$observer |>
            substituteFunctionNames(filterFunctionName, reactiveFunctionName)
          cont$reactive <- cont$reactive |>
            substituteFunctionNames(filterFunctionName, reactiveFunctionName)
          cont$render <- cont$render |>
            substituteFunctionNames(filterFunctionName, reactiveFunctionName)
          cont$download$render <- cont$download$render |>
            substituteFunctionNames(filterFunctionName, reactiveFunctionName)
          cont
        })
      pd
    })
}
substituteFunctionNames <- function(x, ffn, rfn) {
  if (is.null(x)) return(x)
  x |>
    stringr::str_replace_all("<filtered_data>", ffn) |>
    stringr::str_replace_all("<reactive_data>", rfn)
}
populateInputIds <- function(panelDetails) {
  panelDetails |>
    purrr::map(\(pd) {
      pd$content <- pd$content |>
        purrr::map(\(cont) {
          # where to find the inputs
          inputsToSubstitute <- c(
            cont$render, cont$download$render, cont$download$filename,
            cont$observe, cont$reactive
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
            cont$reactive <- cont$reactive |>
              stringr::str_replace_all(pattern = original, replacement = new)
            cont$download$render <- cont$download$render |>
              stringr::str_replace_all(pattern = original, replacement = new)
            cont$download$filename <- cont$download$filename |>
              stringr::str_replace_all(pattern = original, replacement = new)
            cont$observe <- cont$observe |>
              stringr::str_replace_all(pattern = original, replacement = new)
          }
          cont
        })
      pd
    })
}
populateAutomaticFilters <- function(panelDetails, result) {
  panelDetails |>
    purrr::map(\(pd) {
      res <- filterResult(result, pd$data)
      values <- list()
      values$group <- omopgenerics::groupColumns(res)
      values$strata <- omopgenerics::strataColumns(res)
      values$additional <- omopgenerics::additionalColumns(res)
      values$settings <- omopgenerics::settingsColumns(res)

      # create automatic filters
      automaticFilters <- unique(pd$automatic_filters) |>
        purrr::map(\(x) {
          if (!x %in% c("group", "strata", "additional", "settings")) {
            if (x %in% colnames(res)) {
              x <- rlang::set_names("main", x)
            } else if (x %in% values$group) {
              x <- rlang::set_names("group", x)
            } else if (x %in% values$strata) {
              x <- rlang::set_names("strata", x)
            } else if (x %in% values$additional) {
              x <- rlang::set_names("additional", x)
            } else if (x %in% values$settings) {
              x <- rlang::set_names("settings", x)
            } else {
              cli::cli_warn("column: {x} not found!")
              x <- NULL
            }
          } else {
            vals <- values[[x]]
            x <- rlang::set_names(rep(x, length(vals)), vals)
          }
          x
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
populateChoicesSelected <- function(panelDetails) {
  panelDetails |>
    purrr::imap(\(pd, nm) {
      pd$filters <- purrr::map(pd$filters, \(filt) populatecs(filt, nm))
      pd
    })
}
populatecs <- function(filt, nm) {
  if (identical(filt$selected, "selected$")) {
    filt$selected <- paste0("selected$", nm, "_", filt$column)
  }
  if (identical(filt$choices, "choices$")) {
    filt$choices <- paste0("choices$", nm, "_", filt$column)
  }
  filt
}
populatePrefixPanel <- function(panelDetails) {
  panelDetails |>
    purrr::imap(\(pd, nmp) {
      pd$content <- pd$content |>
        purrr::imap(\(cont, nmc) {
          cont$observe <- cont$observe |>
            substitutePrefix(prefix = paste0(nmp, "_", nmc)) |>
            substitutePanel(panel = nmp)
          cont
        })
      pd
    })
}
substitutePrefix <- function(x, prefix) {
  stringr::str_replace_all(x, pattern = "<prefix>", replacement = prefix)
}
substitutePanel <- function(x, panel) {
  stringr::str_replace_all(x, pattern = "<panel>", replacement = panel)
}
