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
        titlePanel("Tech. Workplace Mental Health Survey Explorer",
                   windowTitle="Tech. Mental Health Explorer"),
        
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
                helpText("'Auto' limits the number of states shown to a random subset when displaying
                         Summary Statistics and Region = US. This can be changed by manually selecting
                         the states of interest."),
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
                    label="Survey Questions to Include",
                    choices=c("All", survey_questions_hrf),
                    selected="All",
                    multiple=TRUE
                ),
                helpText("Question descriptions are provided below the chart."),
                textInput(
                    inputId="clustering_weights",
                    label="Survey Questions Weighting (Advanced)",
                    value="",
                    placeholder="Weighting"
                ),
                helpText("Above, you can adjust the weight of the survey
                         questions when clustering. For example, to increase
                         the importance of Benifits (B) and reduce the importance
                         of Care Options (CO), you can write a comma-seperated weighting instruction,
                         using the questions' abbreviations such as this: 'B=1.5, CO=0.75'.
                         The default weighting for all questions is 1.
                         This input field has absolutley *no* affect when computing summary statistics.")
            ),
            
            # Show a plot of the generated distribution
            mainPanel(
                plotOutput("plot", height="675px"),
                br(),
                h3("Survey Question Descriptions"),
                HTML("<ul>
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
                     </ul>"),
                h3("(Very) Technical Details"),
                h4("Data Processing:"),
                p("First, only survey questions for which a binary response is possible are select. Second, to
                  allow for a numerical analysis, 'yes' is mapped to 1 and 'no' is mapped to zero. All other replies, such as 'maybe',
                  are mapped to NA. Third, the data is the aggregated by year, and the mean response of all participants in a
                  given region is computed."),
                h4("Summary Statistics:"),
                p("The summary statistics shown are simply the mean-aggregated results, which come direction out of the data processing procedure."),
                h4("Clustering:"),
                p("The euclidean distance between the mean replies are computed between all countries (yielding an nxn matrix, where n is the number of regions).
                  These distances are then used to learn a Guassian Mixture Model (GMM) to classify the countries / US States.
                  This model is responsible for coloring the points. The position of the points are determined by performing a Principal Component Analysis (PCA).
                  Using the two components which explain the most variance in the (distance) data, this high dimensional space
                  (where each survey question is a dimension) can be summarized on a familiar-looking 2D scatter plot."),
                h4("Additional:"),
                p("In order to be show in either the clustering or summary statistics plots, regions (countries or US States)
                  are required to have at least fifteen observations. Whether or not a region meets this threshold can
                  change as you filter the data (e.g., narrow the range of ages to include)."),
                h3("Data Source"),
                HTML("The full dataset can be obtained <a href='https://www.kaggle.com/osmi/mental-health-in-tech-survey'>here</a>."),
                br(),
                p("")
            )
        )
    )
)
