# ----------------------------------------------------------------------------
#
#                                    Parsers
#
# ----------------------------------------------------------------------------

source("tools/support_tools.R")

# ----------------------------------------------------------------------------
# Tools to Parse the Clustering Weights Field
# ----------------------------------------------------------------------------


weighting_string_parser <- function(string, survey_questions_underscore){
    #
    #
    #
    
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
    
    # Remove extra whitespace
    string <- trimws(string)
    
    # Exit if `string` is empty.
    if (nchar(string) == 0){
        return(NULL)
    }
    
    sub_sec <- function(sub){
        # Tool to process each subsection (comma-demim part) of `string`.
        split <- strsplit(toupper(trimws(sub)), "=")[[1]]
        # Exit if the weighting does not have a key AND value.
        if (length(split) != 2){
            return(list())
        }
        abbrev <- question_abbrevs_list[[split[1]]]
        out <- list(split[2]); names(out) <- abbrev
        return(out)
    }
    
    # Map `sub_sec` over each of the comma-delim entries in `string`.
    parsed_weights <-
        purrr::map(strsplit(string, ",")[[1]], sub_sec) %>% 
        unlist() 
    
    # If no valid input is present, exit.
    if (!length(parsed_weights)){
        return(NULL)
    } else {
        # Otherwise, finish parsing and return.
        parsed_weights <- 
            parsed_weights %>% 
            named_vector_to_list() %>% 
            lapply(as.numeric)
        return(parsed_weights)
    }
}
