# ----------------------------------------------------------------------------
#
#                                User Interface
#
# ----------------------------------------------------------------------------

library(shiny)
library(readr)
library(shinyWidgets)
library(shinydashboard)
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


ui <-
    dashboardPage(
    dashboardHeader(title = "Tech. Workplace Mental Health Survey Explorer"),
    dashboardSidebar(
        width = 315, 
        sidebarMenu(
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
            pickerInput(
                inputId="survey_qs",
                label="Survey Questions to Include",
                choices=survey_questions_hrf,
                selected=survey_questions_hrf,
                multiple=TRUE,
                options=list("actions-box"=TRUE, size=10,
                             "selected-text-format" = 'count > 3')
            ),
            helpText("> Full questions located below graph."),
            
            textInput(
                inputId="clustering_weights",
                label="Advanced: Change Question Weighting During Clustering (Default is 1)",
                value="",
                placeholder="Example: B=1.5, CO=0.75"
            ),
            helpText("> This field has *no* affect on summary stats.")
        )
    ),
    dashboardBody(
        # Boxes need to be put in a row (or column)
        box(plotOutput("plot", height="625px"), width=12),
        box(title = "Survey Question Descriptions", 
            solidHeader = TRUE,
            width=12,
            HTML("
            <ul>
                <li><b>Anonymity (A)</b>: Is your anonymity protected if you choose to take advantage of mental health or substance abuse treatment resources?</li>
                <li><b>Benefits (B)</b>: Does your employer provide mental health benefits?</li>
                <li><b>Care Options (CO)</b>:  Do you know the options for mental health care your employer provides?</li>
                <li><b>Coworkers (C)</b>: Would you be willing to discuss a mental health issue with your coworkers?</li>
                <li><b>Mental Health Consequence (MHC)</b>: Do you think that discussing a mental health issue with your employer would have negative consequences?</li>
                <li><b>Mental Health Interview (MHI)</b>: Would you bring up a mental health issue with a potential employer in an interview?</li>
                <li><b>Obs Consequence (OC)</b>: Have you heard of or observed negative consequences for coworkers with mental health conditions in your workplace?</li>
                <li><b>Phys Health Consequence (PHC)</b>: Do you think that discussing a physical health issue with your employer would have negative consequences?</li>
                <li><b>Remote Work (RW)</b>: Do you work remotely (outside of an office) at least 50% of the time?</li>
                <li><b>Seek Help (SH)</b>: Does your employer provide resources to learn more about mental health issues and how to seek help?</li>
                <li><b>Phys Health Interview (PHI)</b>: Would you bring up a physical health issue with a potential employer in an interview?</li>
                <li><b>Supervisor (S)</b>: Would you be willing to discuss a mental health issue with your direct supervisor(s)?</li>
                <li><b>Treatment (T)</b>: Have you sought treatment for a mental health condition?</li>
                <li><b>Mental vs. Physical (MvP)</b>: Do you feel that your employer takes mental health as seriously as physical health?</li>
                <li><b>Wellness Program (WP)</b>: Has your employer ever discussed mental health as part of an employee wellness program?</li>
            </ul>")
        ),
        box(title = "More Information", 
            solidHeader = TRUE,
            width=12,
            HTML("Technical information about how this application works can be found
                  in the README of this project's GitHub repository, located
                 <a href='https://github.com/TariqAHassan/SurveyExplorer'>here</a>.")
            ),
        box(title = "Data Source", 
            solidHeader = TRUE,
            width=12,
            HTML("The full dataset can be obtained
                 <a href='https://www.kaggle.com/osmi/mental-health-in-tech-survey'>here</a>.")
            )
    )
)
