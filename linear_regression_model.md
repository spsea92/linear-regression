Building a Linear Regression Model to Predict Song’s Valence (Not
finished)
================
Srey Sea
6/25/2020

In this project, I will create a linear regression model to predict a
song’s valence. Caution–the model obtained may or not be a good model
for this prediction. The data is provided by Spotify and a Python script
was created and used to collect the data in a .csv file.

Loading the songs dataset.

``` r
mydata = read.csv("dataset.csv")
attach(mydata)
```

First, parameters, or the independent variables, need to be chosen to be
used in the model.

The dataset has the following parameters:

``` r
library(broom)
names(mydata)
```

    ##  [1] "X"                "song"             "artist"           "danceability"    
    ##  [5] "energy"           "key"              "loudness"         "mode"            
    ##  [9] "speechiness"      "acousticness"     "instrumentalness" "liveness"        
    ## [13] "valence"          "tempo"            "type"             "id"              
    ## [17] "uri"              "track_href"       "analysis_url"     "duration_ms"     
    ## [21] "time_signature"

The method of the **best subsets regression with adjusted
R<sup>2</sup>** will be used to select the independent variables. The
model with the highest adjusted R<sup>2</sup> values and the lowest MSE
value is deemed the best model. Below shows that model \#6 is the best
model, so, danceability, energy, speechiness, acousticness, liveness,
and duration\_ms are the appropriate predictors to be used.

``` r
library(leaps)
n = length(mydata[,1])
mod = regsubsets(cbind(danceability,energy,key,loudness,mode,speechiness,
                       acousticness,instrumentalness,liveness,tempo,
                       duration_ms,time_signature),valence)
summary.mod = summary(mod)
rss = summary.mod$rss
mses = c(rss[1]/(n-2),rss[2]/(n-3),rss[3]/(n-4),rss[4]/(n-5),rss[5]/(n-6),
         rss[6]/(n-7),rss[7]/(n-8),rss[8]/(n-9))
mses
```

    ## [1] 0.03210279 0.02992573 0.02896838 0.02870027 0.02855485 0.02850871 0.02851926
    ## [8] 0.02853920

``` r
summary.mod$adjr2
```

    ## [1] 0.2398019 0.2913548 0.3140250 0.3203740 0.3238174 0.3249102 0.3246602
    ## [8] 0.3241881

``` r
summary.mod$which
```

    ##   (Intercept) danceability energy   key loudness  mode speechiness acousticness
    ## 1        TRUE         TRUE  FALSE FALSE    FALSE FALSE       FALSE        FALSE
    ## 2        TRUE         TRUE   TRUE FALSE    FALSE FALSE       FALSE        FALSE
    ## 3        TRUE         TRUE   TRUE FALSE    FALSE FALSE       FALSE        FALSE
    ## 4        TRUE         TRUE   TRUE FALSE    FALSE FALSE       FALSE         TRUE
    ## 5        TRUE         TRUE   TRUE FALSE    FALSE FALSE        TRUE         TRUE
    ## 6        TRUE         TRUE   TRUE FALSE    FALSE FALSE        TRUE         TRUE
    ## 7        TRUE         TRUE   TRUE  TRUE    FALSE FALSE        TRUE         TRUE
    ## 8        TRUE         TRUE   TRUE  TRUE    FALSE FALSE        TRUE         TRUE
    ##   instrumentalness liveness tempo duration_ms time_signature
    ## 1            FALSE    FALSE FALSE       FALSE          FALSE
    ## 2            FALSE    FALSE FALSE       FALSE          FALSE
    ## 3            FALSE    FALSE FALSE        TRUE          FALSE
    ## 4            FALSE    FALSE FALSE        TRUE          FALSE
    ## 5            FALSE    FALSE FALSE        TRUE          FALSE
    ## 6            FALSE     TRUE FALSE        TRUE          FALSE
    ## 7            FALSE     TRUE FALSE        TRUE          FALSE
    ## 8             TRUE     TRUE FALSE        TRUE          FALSE

Interaction terms should be considered. Using a correlation heatmap (the
tutorial to create a correlation heatmap can be found here)

``` r
mydata2 = mydata[,c(4,5,9,10,12,20)]
cormat = round(cor(mydata2),2)
library(reshape2)
melted_cormat = melt(cormat)
get_upper_tri = function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri = get_upper_tri(cormat)

melted_cormat = melt(upper_tri, na.rm = TRUE)
# Heatmap
library(ggplot2)
ggheatmap = ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))
```

![](linear_regression_model_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

The strongest correlation is between energy and acousticness, so, their
interaction term will be considered.

The current model: **valence** = β<sub>o</sub> +
β<sub>1</sub>**danceability** + β<sub>2</sub>**energy** +
β<sub>3</sub>**speechiness** + β<sub>4</sub>**acousticness** +
β<sub>5</sub>**liveness** + β<sub>6</sub>**duration\_ms** +
β<sub>7</sub>**energy\*acousticness**

Next, a series of F-tests will be performed to determine the independent
variables to be used for the final model.
