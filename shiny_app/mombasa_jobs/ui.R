library(shiny)
library(tidyverse)
library(readxl)
library(shinydashboard)
library(scales)
library(DT)
library(shinyWidgets)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

job_opportunities = read_excel(path = "./job_opportunities.xlsx", sheet = "job_opportunities")
cluster_ratios = read_excel(path = "./job_opportunities.xlsx", sheet = "cluster_ratios")
sector_ratios = read_excel(path = "./job_opportunities.xlsx", sheet = "sector_ratios")
org_size_ratios = read_excel(path = "./job_opportunities.xlsx", sheet = "org_size_ratios")
job_category_ratios = read_excel(path = "./job_opportunities.xlsx", sheet = "job_category_ratios")

years <- job_opportunities %>% filter(year>=2010) %>% pull(year)


shinyUI(fluidPage(
    
    useShinydashboard(),

    # Application title
    titlePanel("Job Opportunities in Mombasa County"),

    # Sidebar for filters
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "year_from", label = "Year From", choices = years, selected = 2021),
            selectInput(inputId = "year_to", label = "Year To", choices = years, selected = years[length(years)]),
            selectInput(inputId = "sector", label = "Sector", choices = sector_ratios$sector, multiple = TRUE, selected = sector_ratios$sector[1]),
            selectInput(inputId = "org_size", label = "Org Size", choices = org_size_ratios$size, multiple = TRUE, selected = org_size_ratios$size[1]),
            #selectInput(inputId = "cluster", label = "Cluster", choices = cluster_ratios$cluster, multiple = TRUE, selected =  cluster_ratios$cluster[1]),
            pickerInput(inputId = "cluster", label = "Cluster", choices = cluster_ratios$cluster, selected = cluster_ratios$cluster[1], multiple = TRUE, options =  pickerOptions(actionsBox = TRUE)),
            #selectInput(inputId = "job_category", label = "Job Category", choices = job_category_ratios$job_category, multiple = TRUE)
            pickerInput(inputId = "job_category", label = "Job Category", choices = job_category_ratios$job_category, multiple = TRUE, options =  pickerOptions(actionsBox = TRUE))
        ),

        # Show main results
        mainPanel(
            plotOutput("jobs_plot"),
            
            h3("Summary Stats"),
            fluidRow(
                infoBoxOutput(outputId = "jobs_base"),
                infoBoxOutput(outputId = "jobs_best"),
                infoBoxOutput(outputId = "jobs_worst")
            ),
            
            h3("Jobs Created"),
            dataTableOutput(outputId = "jobs_created")
        )
    )
))
