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
library(forcats)
source("tools/support_tools.R")
source("tools/shared_data.R")  # import min_observations

# ----------------------------------------------------------------------------
# Data Processing
# ----------------------------------------------------------------------------


get_valid_regions <- function(data_frame, region_col){
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
                                     summary_func=mean){
    # Summarize a region (countries/global or US States).
    #
    # data_frame: a dataframe
    # region_col: one of: country_ (global), state_ (US States).
    # survey_qs: allowed survey questions.
    # summary_func: summary statistic to compute (expected have an `na.rm` param).
    #               Defaults to `mean()`.
    
    summary_func_no_na <- function(x){
        return(summary_func(x, na.rm=TRUE))
    }
    
    # Find thoe region that have `min_observations`.
    valid_regions <- get_valid_regions(data_frame=data_frame,
                                       region_col=region_col)
    
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
    # region_col: one of: country_ (global), state_ (US States).
    # survey_qs: allowed survey questions.
    # weights: a list of column names (keys), where the values
    #          are scalars which can be used to use to reweight the
    #          importance of the survey questions.
    #
    # Summarize ---
    region_summary_df <-
        region_summarizer_engine(data_frame=data_frame,
                                 region_col=region_col,
                                 survey_qs=survey_qs,
                                 ...)
    
    # Apply Reweighting to columns (if applicable) ---
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

gmm_cluster <- function(region_summary_df, G=1:9){
    # Get a summary of the `data_frame` againt `region_col`,
    # for some subset of survey questions (`survey_qs`).
    # Then generate a GMM model of the distances between this 
    # aggregated data, with `G` clusters.
    
    # region_summary_df: the output of `region_summarizer()`.
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


gmm_plotter <- function(region_summary_df, title="Clustering Plot", ...){
    # gmm_plotter(survey, region_col="country_", survey_qs=bool_cols)
    
    # Process Data and learn the model
    model <- gmm_cluster(region_summary_df=region_summary_df, ...)

    # Build the plot
    static_plot <- 
        fviz_mclust(model, ellipse.level=0, repel=TRUE) +
        theme_minimal() +
        ggtitle(title) +
        theme(legend.position="none",
              plot.title=element_text(hjust=0.5),
              plot.subtitle=element_blank())
    
    # Update x and y labels
    static_plot <-
        static_plot +
        labs(x=sub("Dim1 \\(", "Principal Component 1 (Var. Explained = ",
                   static_plot$labels$x),
             y=sub("Dim2 \\(", "Principal Component 2 (Var. Explained = ",
                   static_plot$labels$y)
        )
    
    return(static_plot)
}


# ----------------------------------------------------------------------------
# Bar Plots
# ----------------------------------------------------------------------------



summary_stat_plotter <- function(region_summary_df, region_subset=NULL,
                                 title="Summary Bar Plot"){
    #
    #
    # region_summary_df <- region_summarizer(survey, region_col="state_", survey_qs=bool_cols)
    # summary_stat_plotter(region_summary_df, region_subset=c("California", "New York"))
    
    # Melt for plotting
    region_summary_df_melt <- 
        region_summary_df %>%
        melt(id="region") %>% 
        rename(question=variable) %>% 
        mutate(question=fct_rev(as.factor(as.character(question))))
    
    # Limit to subset
    if (!is.null(region_subset)){
        region_summary_df_melt <-
            region_summary_df_melt %>% 
            filter(region %in% region_subset)
    }
    
    # Generate the plot
    static_plot <- 
        region_summary_df_melt %>% 
        ggplot(aes(x=question, y=value, fill=region)) +
        geom_bar(stat="identity") +
        facet_wrap(~region) +
        labs(y="\nProportion of Participants Who Responded Yes") +
        coord_flip() +
        ggtitle(title) +
        theme_minimal() +
        scale_y_continuous(breaks=seq(0, 1, 0.5)) +
        theme(legend.position="none",
              axis.title.y=element_blank(),
              panel.spacing=unit(1, "lines"),
              plot.title=element_text(hjust=0.5))
    return(static_plot)
}

