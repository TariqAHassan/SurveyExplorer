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









