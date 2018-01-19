# ----------------------------------------------------------------------------
#
#                               Support Functions
#
# ----------------------------------------------------------------------------


library(stringi)


# ----------------------------------------------------------------------------
# Underscore <-> HRF
# ----------------------------------------------------------------------------

underscore_to_hrf <- function(vec, correction=c(" Vs ", " vs. ")){
    # Convert a vector of underscore strings
    # into a human readable format (HRF).
    # Example: "my_string" -> "My String".
    
    # Convert vector in manor described above.
    hrf <- stri_trans_totitle(gsub("_", " ", vec))
    
    # Apply the correction post-conversion (if any).
    if (!is.null(correction)){
        hrf <- gsub(correction[1], correction[2], hrf)
    }
    return(hrf)
}


hrf_to_underscore <- function(vec, correction=c("_vs._", "_vs_")){
    # Convert a vector from human readable format (HRF)
    # to an underscore seperated string.
    # Example: "My String" -> "my_string".
    
    # Convert vector in manor described above.
    hrf <- tolower(gsub(" ", "_", vec))
    
    # Apply the correction post-conversion (if any).
    if (!is.null(correction)){
        hrf <- gsub(correction[1], correction[2], hrf)
    }
    return(hrf)
}

# ----------------------------------------------------------------------------
# MISC
# ----------------------------------------------------------------------------

survey_questions <- function(survey, do_sort=TRUE){
    # Get a list of survey questions in the `data_frame`
    # Note: column names that end with _ have been flagged
    # as not real survey questions (e.g., country of residence).
    survey_col_names <- colnames(survey)
    survey_questions <- survey_col_names[!grepl("_$", survey_col_names)]
    if (do_sort){
        survey_questions <- sort(survey_questions)
    }
    return(survey_questions)
}


name_value_swap <- function(named_vector){
    old_names <- names(named_vector)
    new_names <- as.vector(named_vector)
    names(old_names) <- new_names
    return(old_names)
}


named_vector_to_list <- function(named_vecotr){
    # https://stackoverflow.com/a/46251794/4898004
    out <- split(unname(named_vecotr), names(named_vecotr))
    return(out)
}


cln_brackets <- function(vec){
    # Ablate characters in bracets (inclusive).
    # https://stackoverflow.com/a/24173271/4898004
    return(gsub("\\s*\\([^\\)]+\\)", "", vec))
}