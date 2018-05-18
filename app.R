library(shiny)
library(mice)
library(ggplot2)

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
titanic_cleaned$cabin <- factor(titanic_cleaned$cabin, levels=c("G", "F", "E", "D", "A", "C", "B"))
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

pclass_vec <- c(1, 2, 3)
pclass_avg <- vector(length=3)
for (i in 1:3) {
  specific_pclass <- subset(titanic_cleaned, pclass == i)
  pclass_avg[i] <- mean(specific_pclass$survived)
}
pclass_df <- data.frame("pclass" = pclass_vec, "average_survival" = pclass_avg)
pclass_graph <- (ggplot(pclass_df, aes(x=pclass, y=average_survival)) + geom_point() + geom_smooth(method="lm") + 
                   scale_x_continuous(breaks=c(1, 2, 3), labels = c("Upper", "Middle", "Lower")) + 
                   scale_y_continuous(breaks=c(0.61, 0.25), 
                                      labels=c("Higher chance of surviving", "Lower chance of surviving")) + 
                   labs(x="Passenger Class", y = "Average Survival"))

sex_vec <- 1:2
sex_avg <- vector(length = 2)
for (i in 1:2) {
  specific_sex <- subset(titanic_cleaned, sex == i)
  sex_avg[i] <- mean(specific_sex$survived)
}
sex_df <- data.frame("sex" = sex_vec, "average_survival" = sex_avg)
sex_graph <- (ggplot(sex_df, aes(x=sex, y=average_survival)) + geom_point() + geom_smooth(method="lm") + 
                   scale_x_continuous(breaks=c(1, 2), labels = c("Male", "Female")) + 
                   scale_y_continuous(breaks=c(0.2, 0.73), 
                                      labels=c("Lower chance of surviving", "Higher chance of surviving")) + 
                   labs(x="Gender", y = "Average Survival"))

age_vec <- as.double(levels(factor(titanic_cleaned$age)))
age_avg <- vector(length = length(age_vec))
counter_age <- 1
for (age_ in age_vec) {
  specific_age <- subset(titanic_cleaned, age == age_)
  age_avg[counter_age] <- mean(specific_age$survived)
  counter_age <- counter_age + 1
}
age_df <- data.frame("age" = age_vec, "average_survival" = age_avg)
age_graph <- (ggplot(age_df, aes(x=age, y=average_survival)) + geom_point() + geom_smooth() + 
                scale_y_continuous(breaks=c(0, 1), 
                                   labels=c("Lower chance of surviving", "Higher chance of surviving")) + 
                labs(x="Age", y = "Average Survival"))

sibsp_vec <- c(0, 1, 2, 3, 4, 5, 8)
sibsp_avg <- vector(length = length(sibsp_vec))
counter_sibsp <- 1
for (sibs in sibsp_vec) {
  specific_sibsp <- subset(titanic_cleaned, sibsp == sibs)
  sibsp_avg[counter_sibsp] <- mean(specific_sibsp$survived)
  counter_sibsp <- counter_sibsp + 1
}
sibsp_df <- data.frame("sibsp" = sibsp_vec, "average_survival" = sibsp_avg)
sibsp_graph <- (ggplot(sibsp_df, aes(x=sibsp, y=average_survival)) + geom_point() + geom_smooth() + 
                scale_y_continuous(breaks=c(0, 0.4), 
                                   labels=c("Lower chance of surviving", "Higher chance of surviving")) + 
                labs(x="# Siblings/Spouses", y = "Average Survival"))


parch_vec <- c(0, 1, 2, 3, 4, 5, 6, 9)
parch_avg <- vector(length = length(parch_vec))
counter_parch <- 1
for (par in parch_vec) {
  specific_parch <- subset(titanic_cleaned, parch == par)
  parch_avg[counter_parch] <- mean(specific_parch$survived)
  counter_parch <- counter_parch + 1
}
parch_df <- data.frame("parch" = parch_vec, "average_survival" = parch_avg)
parch_graph <- (ggplot(parch_df, aes(x=parch, y=average_survival)) + geom_point() + geom_smooth() + 
                  scale_y_continuous(breaks=c(0.1, 0.7), 
                                     labels=c("Lower chance of surviving", "Higher chance of surviving")) + 
                  labs(x="# Parents/Children", y = "Average Survival"))


cabin_vec <- c(5, 7, 6, 4, 3, 2, 1)
cabin_avg <- vector(length = length(cabin_vec))
counter_cabin <- 1
for (cab in cabin_vec) {
  specific_cabin <- subset(titanic_cleaned, cabin == cab)
  cabin_avg[counter_cabin] <- mean(specific_cabin$survived)
  counter_cabin <- counter_cabin + 1
}
scale = 1:7
cabin_df <- data.frame("cabin" = scale, "average_survival" = cabin_avg)
cabin_graph <- (ggplot(cabin_df, aes(x=cabin, y=average_survival)) + geom_bar(stat="identity") + 
                  scale_x_continuous(breaks=c(1, 2, 3, 4, 5, 6, 7), 
                                     labels=c("A", "B", "C", "D", "E", "F", "G")) +
                  scale_y_continuous(breaks=c(0.1, 0.7), 
                                     labels=c("Lower chance of surviving", "Higher chance of surviving")) + 
                  labs(x="Cabin", y = "Average Survival"))



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
        refer to the passenger's siblings/spouses aboard the titanic and the pasenger's 
        parents/children aboard the titanic respectively."),
      fluidRow(
        sliderInput("pclass", label="Class", value=1, min=1, max=3),
        actionButton("pclass_button", "Click me to visualize relationship between class and survival")
      ),
      plotOutput("pclass_plot"),
      fluidRow(
        selectInput("sex", label="Gender", choices=list("M", "F")),
        actionButton("sex_button", "Click me to visualize relationship between gender and survival")
      ),
      plotOutput("sex_plot"),
      fluidRow(
        sliderInput("age", label="Age", min=0.2, max=90, value=30, step=0.2),
        actionButton("age_button", "Click me to visualize relationship between age and survival")
      ),
      plotOutput("age_plot"),
      fluidRow(
        sliderInput("sibsp", label="Siblings/Spouses", value=0, min=0, max=10),
        actionButton("sibsp_button", "Click me to visualize relationship between number of siblings/spouses on board
                     and survival")
      ),
      plotOutput("sibsp_plot"),
      fluidRow(
        sliderInput("parch", label="Parents/Children", value=0, min=0, max=10),
        actionButton("parch_button", "Click me to visualize relationship between number of parents/children 
                     on board and survival")
      ),
      plotOutput("parch_plot"),
      fluidRow(
        selectInput("cabin", label="Cabin", choices=list("A", "B", "C", "D", "E", "F", "G")),
        actionButton("cabin_button", "Click me to visualize relationship between the passenger's cabin and survival")
        ),
      plotOutput("cabin_plot"),
      
      
      actionButton("survival_button", "Calculate survival!"),
      textOutput("survived")
      )
    )
  )

server <- function(input, output) {
  
  v <- reactiveValues(accuracy=NULL, fit=NULL, prediction=NULL, pclass_graph=FALSE, sex_graph=FALSE, age_graph=FALSE,
                      sibsp_graph=FALSE, parch_graph = FALSE, cabin_graph=FALSE)
  
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
  
  observeEvent(input$pclass_button, {
    if (v$pclass_graph == FALSE) {
      v$pclass_graph <- TRUE
    }
    else if (v$pclass_graph == TRUE) {
      v$pclass_graph <- FALSE
    }
    
  })
  
  output$pclass_plot <- renderPlot({
    if(v$pclass_graph == TRUE) {
      pclass_graph
    }
  })
  
  observeEvent(input$sex_button, {
    if (v$sex_graph == FALSE) {
      v$sex_graph <- TRUE
    }
    else if (v$sex_graph == TRUE) {
      v$sex_graph <- FALSE
    }
    
  })
  
  output$sex_plot <- renderPlot({
    if(v$sex_graph == TRUE) {
      sex_graph
    }
  })
  
  observeEvent(input$age_button, {
    if (v$age_graph == FALSE) {
      v$age_graph <- TRUE
    }
    else if (v$age_graph == TRUE) {
      v$age_graph <- FALSE
    }
    
  })
  
  output$age_plot <- renderPlot({
    if(v$age_graph == TRUE) {
      age_graph
    }
  })
  
  observeEvent(input$sibsp_button, {
    if (v$sibsp_graph == FALSE) {
      v$sibsp_graph <- TRUE
    }
    else if (v$sibsp_graph == TRUE) {
      v$sibsp_graph <- FALSE
    }
    
  })
  
  output$sibsp_plot <- renderPlot({
    if(v$sibsp_graph == TRUE) {
      sibsp_graph
    }
  })
  
  observeEvent(input$parch_button, {
    if (v$parch_graph == FALSE) {
      v$parch_graph <- TRUE
    }
    else if (v$parch_graph == TRUE) {
      v$parch_graph <- FALSE
    }
    
  })
  
  output$parch_plot <- renderPlot({
    if(v$parch_graph == TRUE) {
      parch_graph
    }
  })
  
  observeEvent(input$cabin_button, {
    if (v$cabin_graph == FALSE) {
      v$cabin_graph <- TRUE
    }
    else if (v$cabin_graph == TRUE) {
      v$cabin_graph <- FALSE
    }
    
  })
  
  output$cabin_plot <- renderPlot({
    if(v$cabin_graph == TRUE) {
      cabin_graph
    }
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
