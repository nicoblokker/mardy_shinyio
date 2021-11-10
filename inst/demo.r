library(shiny)
library(shinyjs)
library(igraph)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(visNetwork)
library(DT)
source("supp_functions.r")

data <- read.csv("mardy_data.csv", fileEncoding = "UTF-8", )
migration_codebook_english <- read.csv("mardy_codebook.csv", fileEncoding = "UTF-8")

runApp("uiserver.r")
