# ----------------------------------------------------------------------------
#
#                                    Server
#
# ----------------------------------------------------------------------------

library(shiny)
source("tools/support_tools.R")
source("tools/parsers.R")
source("tools/plotting_engine.R")
source("tools/shared_data.R")  # import min_observations

library(shiny)
library(shinyWidgets)
library(shinydashboard)

# ----------------------------------------------------------------------------
# Constants
# ----------------------------------------------------------------------------

ERROR_MESSAGE <- "Insufficient Data. Try Broading your Query or Changing the Analysis."

# ----------------------------------------------------------------------------
# Read in Complete (Cleaned) Survey Data
# ----------------------------------------------------------------------------

survey <- read_csv("data/survey_cln.csv")

# ----------------------------------------------------------------------------
# Summary & Lookup Objects
# ----------------------------------------------------------------------------

region_mapping <- list("Global"="country_", "US"='state_')
states <- unique(survey$state_)
states <- states[!is.na(states)]

survey_questions_underscore <- survey_questions(survey)

# ----------------------------------------------------------------------------
# General Support Functions
# ----------------------------------------------------------------------------


titler <- function(input){
    # Generate the plot's title
    vec <- c(input$region, input$analysis_type, "for",
             input$age_range[1], 'to', input$age_range[2], "year olds")
    return(paste(vec, collapse=" "))
}


state_hander <- function(input){
    # Handle input from the US State selector
    us_states <- input$us_states
    
    validate(
        need(length(input$us_states) > 0, ERROR_MESSAGE)
    )
    return(sort(us_states))
}


allowed_states <- function(input){
    # Get a vector of allowed states, based on the 
    # input (ensures stats with insufficent data are hidden).
    age_filter_survey_df <-
        survey %>%
        filter(age_ >= input$age_range[1],
               age_ <= input$age_range[2],
               !is.na(state_)) %>% 
        group_by(state_) %>% 
        dplyr::summarise(count=n()) %>% 
        filter(count > min_observations) %>% 
        mutate(state_ = as.character(state_))
    
    selected <- unique(age_filter_survey_df$state_)
    return(sort(selected))
}


question_handler <- function(input){
    
    validate(
        need(length(input) > 0, ERROR_MESSAGE)
    )
    return(sort(input))
}


enough_data_for_plotting_checker <- function(region_summary_df, summary_stat){
    # Clustering requires more than one region.
    if (!summary_stat){
        validate(need(nrow(region_summary_df) > 1, ERROR_MESSAGE))
    # Bar plots require at least one region.
    } else {
        validate(need(nrow(region_summary_df) > 0, ERROR_MESSAGE))
    }
}


# ----------------------------------------------------------------------------
# Sever Logic
# ----------------------------------------------------------------------------


shinyServer(
    function(input, output) {
        
        output$us_states <-
            renderUI({
                choices <- allowed_states(input)
                pickerInput(
                    inputId="us_states",
                    label="US States to Include",
                    choices=choices,
                    selected=choices,
                    multiple=TRUE,
                    options=list("actions-box"=TRUE, size=10,
                                 "selected-text-format" = 'count > 3')
                )
            }
        )
        
        output$plot <-
            renderPlot({
                
                summary_stat <- input$analysis_type=="Summary Statistics"
                allowed_states <- state_hander(input=input)
                questions <- question_handler(input=hrf_to_underscore(cln_brackets(input$survey_qs)))
                
                # Ablate weight alterations if Rendering Summary Statistics.
                if (summary_stat) {
                    weights <- NULL
                } else {
                    weights <- weighting_string_parser(input$clustering_weights,
                                                       survey_questions_underscore)
                }
                
                # Filter the data
                region_summary_df <-
                    survey %>%
                    filter(age_ >= input$age_range[1],
                           age_ <= input$age_range[2],
                           state_ %in% allowed_states | is.na(state_)) %>%
                    region_summarizer(
                        region_col=region_mapping[[input$region]],
                        survey_qs=questions,
                        weights=weights)
                
                # Check enough data remains for plotting
                enough_data_for_plotting_checker(region_summary_df, summary_stat)
                
                # Generate Plot
                plot_func <- if (summary_stat) summary_stat_plotter else gmm_plotter
                plot <- plot_func(region_summary_df, title=titler(input))
                
                # Check a plot was actually generated
                validate(
                    need(!is.null(plot), ERROR_MESSAGE)
                )
                
                # Show plot.
                plot
            }
        )
    }
)
