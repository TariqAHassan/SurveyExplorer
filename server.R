# ----------------------------------------------------------------------------
#
#                                    Server
#
# ----------------------------------------------------------------------------

library(shiny)
source("tools/support_tools.R")
source("tools/plotting_engine.R")
source("tools/shared_data.R")  # import min_observations


DEFAULT_VALUES <- c('all', "auto")

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


titler <- function(input){
    vec <- c(input$region, input$analysis_type, "for",
             input$age_range[1], 'to', input$age_range[2], "year olds")
    return(paste(vec, collapse=" "))
}


default_remover <- function(default){
    return(default[!(tolower(default) %in% DEFAULT_VALUES)])
}


state_hander <- function(input, default, summary_stat, top_n=3){
    us_states <- input$us_states
    
    if (tolower(us_states) == 'auto'){
        if (summary_stat){
            out <- head(default, top_n)
        } else {
            out <- default
        }
    } else {
        out <- default_remover(us_states)
    }
    return(sort(out))
}


allowed_states <- function(input){
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


question_handler <- function(input, all){
    # Handle cases where `input == 'all'`.
    if (tolower(input) == 'all'){
        out <- all
    } else {
        out <- input
    }
    return(sort(default_remover(out)))
}

# ----------------------------------------------------------------------------
# Sever Logic
# ----------------------------------------------------------------------------


shinyServer(
    function(input, output) {
        
        output$us_states <-
            renderUI({
                selectInput(
                    inputId="us_states",
                    label="Included US States",
                    choices=c("Auto", allowed_states(input)),
                    selected="Auto",
                    multiple=TRUE
                )
        })
        
        output$plot <-
            renderPlot({
                summary_stat <- input$analysis_type=="Summary Statistics"
                allowed_states <- state_hander(input=input,
                                               default=states,
                                               summary_stat=summary_stat)
                questions <- question_handler(input=hrf_to_underscore(cln_brackets(input$survey_qs)),
                                              all=survey_questions_underscore)
                
                region_summary_df <-
                    survey %>%
                    filter(age_ >= input$age_range[1],
                           age_ <= input$age_range[2],
                           state_ %in% allowed_states | is.na(state_)) %>%
                    region_summarizer(
                        region_col=region_mapping[[input$region]],
                        survey_qs=questions,
                        weights=weighting_string_parser(input$clustering_weights))
                
                plot <- gmm_plotter(region_summary_df, title=titler(input))
                plot
            })
})