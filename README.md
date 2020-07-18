# Building a Linear Regression Model to Predict a Song's Valence

This project is a re-creation of the final project done in my Linear Regression class during the Spring 2020 semester. A detailed report with all the methods used, graphs, and visualizations can be found [here](https://github.com/spsea92/linear-regression/blob/master/linear_regression_model.md). Otherwise, this README will serve as a TLDR of the report.

The goal of this project is to build a linear regression to predict a song's valence from its audio features with the knowledge from my Linear Regression class, and determine if audio features are enough variables to determine a song's valence. Spoiler alert--it's not. The songs and their audio features are from Spotify and were obtained using Spotify's API, [Spotipy](https://https://spotipy.readthedocs.io/en/2.13.0/). The songs are from six playlists from Spotify's **Mood** genre.

## Process and Methods
- **Best subsets regression with adjusted R<sup>2</sup>** for selecting predictor variables
- Series of **partial F-tests** to determine significance of the predictor variables
- **Studentized deleted residuals** to determine outliers
- Determining **highly leverage points** and determining any highly influential points
- **Box-Cox plot** for transformations to satisfy linearity, equal Variance, and normality conditions
- **Shapiro-Wilk test** to ascertain normality condition is met

## Was the model a good model?
The model obtained has an adjusted R<sup>2</sup> value of 0.3245, so, about 32.45% of the variance found in the response variable, valence, is explained by the predictors danceability, energy, duration of the song, and acousticness. Based on the adjusted R<sup>2</sup> alone, we can tell that the model is not a good model to make a prediction about a song’s valence. Thus, audio features alone does not create a good linear regression model to predict a song’s valence.