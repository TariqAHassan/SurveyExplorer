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

survey_questions_hrf <- underscore_to_hrf(survey_questions(survey))
abbrevs <- abbreviate(survey_questions_hrf, minlength=1)
survey_questions_hrf <- paste(survey_questions_hrf, " (", abbrevs, ")", sep="")

# ----------------------------------------------------------------------------
# Compute the Age Range
# ----------------------------------------------------------------------------

age_range <- range(survey$age_, na.rm=TRUE)

# ----------------------------------------------------------------------------
# Compute the Age Range
# ----------------------------------------------------------------------------

states <- unique(survey$state_)
states <- sort(states[!is.na(states)])

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
                    choices=c("Clustering", "Summary Statistics"),
                    selected="Clustering"
                ),
                selectInput(
                    inputId="region",
                    label="Region",
                    choices=c("Global", "US"),
                    selected="Global"
                ),
                htmlOutput("us_states"),
                sliderInput(
                    inputId="age_range",
                    label="Age Range",
                    min=age_range[1],
                    max=age_range[2],
                    value=age_range,
                    step=1
                ),
                selectInput(
                    inputId="survey_qs",
                    label="Survey Questions",
                    choices=c("All", survey_questions_hrf),
                    selected="All",
                    multiple=TRUE
                ),
                helpText("Advanced Clustering Features"),
                textInput(
                    inputId="clustering_weights",
                    label="Survey Questions Weighting",
                    value="",
                    placeholder="Weighting"
                ),
                helpText("Above, you can adjust the weight of the survey
                         questions when clustering. For example, to increase
                         the importance of Benifits (B) and reduce the importance
                         of Care Options (CO), you can write a comma-seperated instruction,
                         using the question's abbreviation such as 'B=1.5, CO=0.75'.
                         Default weighting for all questions is 1. This input has absolutley *no* affect
                         when computing summary statistics.")
            ),
            
            # Show a plot of the generated distribution
            mainPanel(
                plotOutput("plot", height="500px")
            )
        )
    )
)
