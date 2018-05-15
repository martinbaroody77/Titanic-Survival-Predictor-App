
library(shiny)
library(mice)

entire_titanic <- read.csv("data/titanic3.csv", stringsAsFactors = FALSE)
titanic_cleaned <- entire_titanic[-(1310), -c(3, 8, 9, 11, 12, 13, 14)]

for (i in 1:nrow(titanic_cleaned)) {
  if (titanic_cleaned$cabin[i] != "") {
    titanic_cleaned$cabin[i] <- substr(titanic_cleaned$cabin[i], 1, 1)
  }
  else {
    titanic_cleaned$cabin[i] <- NA
  }
}

titanic_cleaned$sex<- factor(titanic_cleaned$sex, levels=c("male", "female"))
titanic_cleaned$sex <- as.numeric(titanic_cleaned$sex)
titanic_cleaned$cabin <- factor(titanic_cleaned$cabin, levels=c("A", "B", "C", "D", "E", "F", "G"))
titanic_cleaned$cabin <- as.numeric(titanic_cleaned$cabin)

mimputation <- mice(titanic_cleaned)

average_mimp <- mimputation

for (name in names(mimputation)) {
  if (!is.null(mimputation$imp[[name]])) {
    average_mimp$imp[[name]] <- average_mimp$imp[[name]][, 1]
    for (i in nrow(mimputation$imp[[name]])) {
      average_mimp$imp[[name]][i] <- mean(unlist(mimputation$imp[[name]][i]))
    }
  }
}

titanic_cleaned <- complete(average_mimp)

ui <- fluidPage(
  
  titlePanel("Machine Learning with the Titanic Dataset"),
  
  sidebarLayout(
    
    sidebarPanel(
      p("Click the button below to train the algorithm on a random sample of the data."),
      actionButton("train", "Train"),
      textOutput("accuracy"),
      br(),
      p("For reference, ", a("this", href="http://biostat.mc.vanderbilt.edu/wiki/pub/Main/DataSets/titanic3.xls"), 
        "is the dataset I used.")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      h1("Predict titanic survival!"),
      br(),
      p("Enter information of a passenger and my algorithm will predict whether this passenger 
        would survive the titanic disaster or not."),
      p("Notes: for Class, 1 represents upper class, 3 is lower class. Siblings/Spouses and Parents/Children
        refer to the passenger's siblngs/spouses aboard the titanic and the pasenger's 
        parents/children aboard the titanic respectively."),
      sliderInput("pclass", label="Class", value=1, min=1, max=3),
      selectInput("sex", label="Gender", choices=list("M", "F")),
      sliderInput("age", label="Age", min=0.2, max=90, value=30, step=0.2),
      sliderInput("sibsp", label="Siblings/Spouses", value=0, min=0, max=10),
      sliderInput("parch", label="Parents/Children", value=0, min=0, max=10),
      selectInput("cabin", label="Cabin", choices=list("A", "B", "C", "D", "E", "F", "G")),
      actionButton("survival_button", "Calculate survival!"),
      textOutput("survived")
    )
  )
)

server <- function(input, output) {
  
  v <- reactiveValues(accuracy=NULL, fit=NULL, prediction=NULL)
  
  observeEvent(input$train, {
    
    samples <- sample(nrow(titanic_cleaned), 100)
    test_set <- titanic_cleaned[samples, ]
    train_set <- titanic_cleaned[-(samples), ]
    
    # create a model based on the training data and predict
    
    fit <- glm(survived~pclass+sex+age+sibsp+parch+cabin, data=titanic_cleaned, family=binomial())
    v$fit <- fit
    predictions <- predict(fit, test_set, type="response")
    
    prediction_vector <- vector(length=length(predictions))
    for (i in 1:length(predictions)) {
      if (predictions[i] > 0.55) {
        prediction_vector[i] <- 1
      }
      else {
        prediction_vector[i] <- 0
      }
    }
    
    correctness_vector <- vector(length=length(predictions))
    for (i in 1:length(predictions)) {
      if (prediction_vector[i] == test_set$survived[i]) {
        correctness_vector[i] <- TRUE
      } 
      else {
        correctness_vector[i] <- FALSE
      }
    }
    
    results_df <- data.frame("prediction"=prediction_vector, "actual"=test_set$survived, "correctness"=correctness_vector)
    true_rows <- subset(results_df, correctness==TRUE)
    accuracy <- nrow(true_rows) / nrow(results_df)
    v$accuracy <- accuracy
    
    
  })
  
  output$accuracy <- renderText({
    paste("Accuracy:", v$accuracy)
  })
  
  observeEvent(input$survival_button, {
    if (!is.null(v$fit)) {
      
      sex_int <- 0
      if (input$sex == "M") {
        sex_int <- 1
      }
      else if (input$sex == "F") {
        sex_int <- 2
      }
      
      cabin_int <- 0
      if(input$cabin == "A") {
        cabin_int <- 1
      }
      else if(input$cabin == "B") {
        cabin_int <- 2
      }
      else if(input$cabin == "C") {
        cabin_int <- 3
      }
      else if(input$cabin == "D") {
        cabin_int <- 4
      }
      else if(input$cabin == "E") {
        cabin_int <- 5
      }
      else if(input$cabin == "F") {
        cabin_int <- 6
      }
      else if(input$cabin == "G") {
        cabin_int <- 7
      }
      
      data_to_predict <- data.frame("pclass"=input$pclass, "sex"=sex_int, "age"=input$age, "sibsp"=input$sibsp, 
                                    "parch"=input$parch, "cabin"=cabin_int)
      prediction_val <- predict(v$fit, data_to_predict, type="response")
      if(prediction_val[1] > 0.55) {
        v$prediction <- 1
      }
      else {
        v$prediction <- 0
      }
    }
    
  })
  
  output$survived <- renderText({
    if(is.null(v$prediction)) {
      "You must train the model first"
    }
    else if (v$prediction == 1) {
      "Survived"
    }
    else if (v$prediction == 0) {
      "Died"
    }
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)

