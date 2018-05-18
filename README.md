A web app made using R and Shiny that allows the user to customize a person using Shiny widgets. The app can predict whether this person would have survived the titanic disaster using machine learning. 

NOTE: requires "shiny", "mice", and "ggplot2" libraries to be installed. Use install.packages in the R console to install them.

I used logistic regression to train the model and make the predictions. 

To run the app, type store these files in a directory called "Titanic-App", use the setwd() function in the R console to change the working directory to the folder containing "Titanic-App", type library(shiny), then type runApp("Titanic-App").
