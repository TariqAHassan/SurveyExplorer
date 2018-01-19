# ----------------------------------------------------------------------------
#
#                                User Interface
#
# ----------------------------------------------------------------------------

library(shiny)
library(readr)
source("tools/support_tools.R")

# ----------------------------------------------------------------------------
# Read in Complete (Cleaned) Survey Data
# ----------------------------------------------------------------------------

survey <- read_csv("data/survey_cln.csv")

# ----------------------------------------------------------------------------
# Define list of Survey Quetions
# ----------------------------------------------------------------------------

# Note: column names that end with _ have been flagged
# as not real survey questions (e.g., country of residence).

survey_col_names <- colnames(survey)
survey_questions <- survey_col_names[!grepl("_$", survey_col_names)]
survey_questions_hrf <- sort(underscore_to_hrf(survey_questions))

# ----------------------------------------------------------------------------
# Compute the Age Range
# ----------------------------------------------------------------------------

age_range <- range(survey$age_, na.rm=TRUE)

# ----------------------------------------------------------------------------
# Define the UI.
# ----------------------------------------------------------------------------

shinyUI(
    fluidPage(
    
    # Application title
    titlePanel("Tech. Workplace Mental Health Survey Explorer"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(
                inputId="analysis_type",
                label="Analysis",
                choices=c("Clustering", "Bar Chart"),
                selected="Clustering"
            ),
            selectInput(
                inputId="region",
                label="Region",
                choices=c("Global", "USA"),
                selected="Global"
            ),
            selectInput(
                inputId="survey_questions",
                label="Survey Questions",
                choices=c('All', survey_questions_hrf),
                selected='All',
                multiple=TRUE
            ),
            sliderInput(
                inputId="age_range",
                label="Age Range",
                min=age_range[1],
                max=age_range[2],
                value=age_range,
                step=1
            )
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
))
