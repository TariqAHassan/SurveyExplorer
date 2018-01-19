# ----------------------------------------------------------------------------
#
#                                Plotting Engine
#
# ----------------------------------------------------------------------------

library(dplyr)
library(plotly)
library(reshape2)
library(mclust)
library(factoextra)
library(ggplot2)
library(ggpmisc)

# ----------------------------------------------------------------------------
# Data Processing
# ----------------------------------------------------------------------------


get_valid_regions <- function(data_frame, region_col, min_observations){
    # Get region in data_frame that have
    # a min. number of obervations when grouped.
    valid_regions <- 
        data_frame %>% 
        group_by_(region_col) %>% 
        summarise(count=n()) %>% 
        filter(count >= min_observations) %>% 
        pull(region_col)
    return(valid_regions)
}


region_summarizer_engine <- function(data_frame, region_col, survey_qs,
                                     min_observations=15, summary_func=mean){
    # Summarize a region (countries/global or US States).
    #
    # data_frame: a dataframe
    # region_col: one of: country_ (global), state_ (US States).
    # survey_qs: allowed survey questions.
    # min_observations: min. number of required observations. Defaults to 10.
    # summary_func: summary statistic to compute (expected have an `na.rm` param).
    #               Defaults to `mean()`.
    
    summary_func_no_na <- function(x){
        return(summary_func(x, na.rm=TRUE))
    }
    
    # Find thoe region that have `min_observations`.
    valid_regions <- get_valid_regions(data_frame=data_frame,
                                       region_col=region_col,
                                       min_observations=min_observations)
    
    # Add a "region" column
    data_frame['region'] <- data_frame[region_col]
    
    # Generate summary
    region_summary_df <-
        data_frame %>% 
        select(region, survey_qs) %>%
        filter(!is.na(region),
               region %in% valid_regions) %>% 
        group_by(region) %>% 
        summarise_all(funs(summary_func_no_na))
    return(region_summary_df)
}


region_summarizer <- function(data_frame, region_col, survey_qs, weights=NULL, ...){
    #
    #
    # weights: a list of column names (keys), where the values
    #          are scalars on [0, 1] which can be used to use
    #          to reweight the importance of the survey questions.
    #
    # Summarize ---
    region_summary_df <-
        region_summarizer_engine(data_frame=data_frame,
                                 region_col=region_col,
                                 survey_qs=survey_qs,
                                 ...)
    
    # Apply Reweighting to columns ---
    if (is.list(weights)){
        for (c in names(weights)){
            region_summary_df[c] <- weights[[c]] * region_summary_df[c]
        }
    }
    return(region_summary_df)
}

# ----------------------------------------------------------------------------
# Clustering
# ----------------------------------------------------------------------------

gmm_cluster <- function(region_summary_df, region_col,
                        survey_qs, G=1:4, ...){
    # Get a summary of the `data_frame` againt `region_col`,
    # for some subset of survey questions (`survey_qs`).
    # Then generate a GMM model of the distances between this 
    # aggregated data, with `G` clusters.
    
    # region_summary_df: the output of `region_summarizer()`.
    # region_col: one of: country_ (global), state_ (US States).
    # survey_qs: allowed survey questions.
    # G: number of clusters in to use in the model.
    
    # Convert `region_summary_df` into a matrix ---
    region_summary_mtx <-
        region_summary_df %>%
        select(-region) %>%
        as.matrix()
    
    rownames(region_summary_mtx) <- region_summary_df$region
    
    # Compute distances between vectors and compute GMM ---
    region_summary_mtx_dist <- dist(region_summary_mtx)
    gmm_cluster_model <- Mclust(region_summary_mtx_dist, G=G)
    
    return(gmm_cluster_model)
}


gmm_plotter <- function(region_summary_df, region_col, survey_qs,
                        title="Clustering Plot", ...){
    # gmm_plotter(survey, region_col="country_", survey_qs=bool_cols)
    
    # Process Data and Generate the model
    model <- gmm_cluster(region_summary_df=region_summary_df,
                         region_col=region_col,
                         survey_qs=survey_qs,
                         ...)

    # Build the plot
    plot_static <- 
        fviz_mclust(model, ellipse.level=0, repel=TRUE) +
        theme_minimal() +
        ggtitle(title) +
        theme(legend.position="none",
              plot.title=element_text(hjust=0.5),
              plot.subtitle=element_blank())
    
    # Update x and y labels
    plot_static <-
        plot_static +
        labs(x=sub("Dim1 \\(", "Principal Component 1 (Var. Explained = ",
                   plot_static$labels$x),
             y=sub("Dim2 \\(", "Principal Component 2 (Var. Explained = ",
                   plot_static$labels$y)
        )
    
    return(plot_static)
}


# ----------------------------------------------------------------------------
# Bar Plots
# ----------------------------------------------------------------------------









































































