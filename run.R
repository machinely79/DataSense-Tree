

library(readxl)
library(shinythemes)
library(plotly)
library(htmlwidgets)
library(IRdisplay)
library(shinyjs)
library(bslib) 
library(reactable)
library(shiny)
library(dplyr)
library(flexdashboard)
library(tidyr)
library(sparkline)
library(lubridate)
library(tibble)
library(shinyglide) 
library(dataPreparation)
library(shinycssloaders)
library(rpart)
library(RColorBrewer)
library(visNetwork)
library(shinyWidgets)
library(caret)
library(kableExtra)
library(rpart.plot)
library(openxlsx)


options(scipen = 999)

# Options for reactable
options(reactable.theme = reactableTheme(
  color = "hsl(233, 9%, 87%)",
  backgroundColor ="#051524",    
  borderColor =  "hsl(233, 9%, 22%)",
  stripedColor = "#051524",     
  highlightColor =  "#1b3041", "hsl(233, 12%, 24%)",
  inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
  selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
  pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
  pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
  
))

# Options for Spinner
options(spinner.color="#7bc0ce", spinner.color.background="#0f2437", spinner.size=4)

ui <- source("ui.R")$value
server <- source("server.R")$value

shinyApp(ui = ui, server = server)
