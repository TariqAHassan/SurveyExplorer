# ----------------------------------------------------------------------------
#
#                                    Server
#
# ----------------------------------------------------------------------------

library(shiny)
source("tools/support_tools.R")
source("tools/plotting_engine.R")

# ----------------------------------------------------------------------------
# Read in Complete (Cleaned) Survey Data
# ----------------------------------------------------------------------------

survey <- read_csv("data/survey_cln.csv")

# ----------------------------------------------------------------------------
# Summary & Lookup Objects
# ----------------------------------------------------------------------------

region_mapping <- list("Global"="country_", "US"='state_')
states <- unique(survey$state_)

survey_questions_underscore <- survey_questions(survey)

# ----------------------------------------------------------------------
# Tools to Parse the Clustering Weights
# ----------------------------------------------------------------------

# Generate a list of the form:
# Key = Abbreviation; Value = "Full Name".
# This manipulation is non-trivial.
question_abbrevs_list <-
    survey_questions_underscore %>% 
    underscore_to_hrf() %>% 
    abbreviate(minlength=1) %>%
    name_value_swap() %>% 
    hrf_to_underscore() %>% 
    named_vector_to_list()


weighting_string_parser <- function(string){
    # weighting_string_parser("B=1, C=10, CO=.83")
    
    if (nchar(trimws(string)) == 0){
        return(NULL)
    }
    
    sub_sec <- function(sub){
        split <- strsplit(toupper(trimws(sub)), "=")[[1]]
        abbrev <- question_abbrevs_list[[split[1]]]
        out <- list(split[2]); names(out) <- abbrev
        return(out)
    }
    
    parsed_weights <-
        purrr::map(strsplit(string, ",")[[1]], sub_sec) %>% 
        unlist() %>% 
        named_vector_to_list() %>% 
        lapply(as.numeric)
        
    return(parsed_weights)
}


# ----------------------------------------------------------------------
# General Support Functions
# ----------------------------------------------------------------------


default_handler <- function(input, default, stat_summary=FALSE){
    # Handle cases where `input == 'all'`
    defaults <- c('all', "auto")
    
    if (length(input) == 1){
        if (tolower(input) %in% defaults){
            return(default)
        }
    } else {
        return(input[!(tolower(input) %in% defaults)])
    }
}


titler <- function(input){
    vec <- c(input$region, input$analysis_type, "for",
             input$age_range[1], 'to', input$age_range[2], "year olds")
    return(paste(vec, collapse=" "))
}


# ----------------------------------------------------------------------------
# Sever Logic
# ----------------------------------------------------------------------------


shinyServer(
    function(input, output) {
        
        output$plot <-
            renderPlot({
                
                allowed_states <- default_handler(input$us_states, default=states)
                questions <- default_handler(hrf_to_underscore(cln_brackets(input$survey_qs)),
                                             default=survey_questions_)
                
                region_summary_df <-
                    survey %>%
                    filter(age_ >= input$age_range[1],
                           age_ <= input$age_range[2],
                           state_ %in% allowed_states) %>% 
                    region_summarizer(
                        region_col=region_mapping[[input$region]],
                        survey_qs=questions,
                        weights=weighting_string_parser(input$clustering_weights)
                    )
                
                gmm_plotter(region_summary_df, title=titler(input))
            })
})
