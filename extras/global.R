# Load all packages
library(modules)
library(shiny)
library(dplyr)
library(DT)
library(plotly)
library(shinydashboard)
library(ggplot2)
library(sass)
library(htmlwidgets)
library(bslib)
library(shinyjqui)
library(shinyBS)
library(CohortCharacteristics)
library(visOmopResults)
# library(future)
# library(promises)
# plan(multisession)
# Compile sass to css
sass(
  sass::sass_file("inst/styles/main.scss"),
  cache = NULL,
  options = sass_options(output_style = "compressed"),
  output = "www/css/sass.min.css"
)

# Variables
rdsFolder <- "data/"
dataFolder <- "data"


# Modules loading

# modules for UploadData


# modules for LoadData
uploadData <- use("modules/uploadData.R")
studySelect <- use("modules/studySelect.R")
dataLoad <- use("modules/dataLoad.R")


# old modules -> from template
tableFunction <- use("modules/tableFunction.R")
graphIncidence <- use("modules/graphIncidence.R")
graphCohortOverlap <- use("modules/graphCohortOverlap.R")
graphPrevalence <- use("modules/graphPrevalence.R")
graphSummarisedCharacteristics <- use("modules/graphSummarisedCharacteristics.R")
graphLargeScaleCharacteristics <- use("modules/graphLargeScaleCharacteristics.R")
graphCohortTiming <- use("modules/graphCohortTiming.R")
graphCohortTimingDensity <- use("modules/graphCohortTimingDensity.R")

filterFunction <- use("modules/filterFunction.R")
filterSettingFunction <- use("modules/filterSettingFunction.R")


# Tab pages
source("pages/contact.R")
source("pages/about.R")
source("pages/UploadData.R")
source("pages/LoadData.R")
source("modules/createCard.R")
source("modules/addSharedResources.R")
source("modules/uploadData.R")
source("modules/sidebarMenuDynamic.R")
source("modules/createDynamicTabs.R")

# createCard <- use("modules/createCard.R")
# addSharedResources <- use("modules/addSharedResources.R")


# Then source rest of the pages of the actual PhenotypeR  shiny

