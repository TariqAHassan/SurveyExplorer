# SurveyExplorer

---

This is the code for a R Shiny web app., designed to allow users to explore the result of a 2014 survey
about mental health attitudes in technology companies. The application itself can be found
[here](https://thass.shinyapps.io/SurveyExplorer/). 

## Technical Details 

### Data Processing

First, only survey questions for which a binary response is possible are select. Second, to
allow for a numerical analysis, 'yes' is mapped to 1 and 'no' is mapped to zero. All other replies, such as 'maybe',
are mapped to NA. Third, the data is the aggregated by year, and the mean response of all participants in a
given region is computed.

### Summary Statistics

The summary statistics shown are simply the mean-aggregated results,
which come direction out of the data processing procedure.

### Clustering

The euclidean distance between the mean replies are computed between all countries (yielding an nxn matrix, 
where n is the number of regions). These distances are then used to learn a Gaussian Mixture Model (GMM) to
classify the countries / US States. This model is responsible for coloring the points. The position of the points
are determined by performing a Principal Component Analysis (PCA). Using the two components which explain the most
variance in the (distance) data, this high dimensional space (where each survey question is a dimension) can be summarized
on a familiar-looking 2D scatter plot.

### Additional

In order to be show in either the clustering or summary statistics plots, regions (countries or US States)
are required to have at least fifteen observations. Whether or not a region meets this threshold can
change as you filter the data (e.g., narrow the range of ages to include)

### Data Source

The full dataset can be obtained [here](https://www.kaggle.com/osmi/mental-health-in-tech-survey).

## About

Tariq Hassan, Jan. 2018.
