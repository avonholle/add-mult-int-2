# server.R
# code based on the sim.e.mod.R program

library(mgcv)
library(ggplot2)
library(plyr)
library(reshape2)

# Parameters for function below..................
# ns: number of people in sample
# beta.0: coefficient for intercept
# beta.1: coefficient for x term
# beta.2: coefficient for z term
# beta.3: coefficient for interaction term between x and z

# this function creates a sample for the example on page 449.
created = function(ns, beta.0, beta.1, beta.2, beta.3){
  
  ns = 100
  age = rnorm(ns, mean=50, sd=10)
  male = sample(x=c(0,1), size=ns, 
                replace=TRUE, 
                prob=c(0.5,0.5))

  # Generate logisitic regression outcome values according to params above
  # see http://stats.stackexchange.com/questions/46523/how-to-simulate-artificial-data-for-logistic-regression
  lc.1 = beta.0 + beta.1*age + beta.2*male + beta.3*age*male
  pr.1 = 1/(1+exp(-lc.1))
  y.1 = rbinom(ns, 1, pr.1)
  
  df.1 = data.frame(y=y.1, x1=age, x2=male)
  return(df.1)
}

expit <- function(x) 1/(1+exp(-x))


shinyServer(function(input, output) {
  
  withMathJax()
  
  # take parameters entered with the slider bar and use to simulate data
  ldata <- reactive({
    created(input$n1, input$beta0, input$beta1, input$beta2, input$beta3)
  })
  
  # get a summary of the simulated data
  output$table1 <- renderTable(print(ldata()[1:10,]))
  
  # get 2x2 tables by strata
  # This method doesn't work in putting the object into the server.R
  # #########################################################
  
  # alternate way of getting two by two table
  output$table2alt <- renderTable ({ 
    dcast(ldata(), y ~ x2, value.var="x2", fun.aggregate=length)
  })
  
  # Use simulated data to run logistic regression
  
  # logistic regression without interaction
  m.1 = reactive({
    glm( y ~ x1+x2, data=ldata(), family="binomial")
    }) # model on page 449 
    # original object was 'results1'


  # Output results to a table
  # ############################
  output$summary <- renderTable({
    summary(m.1())
  })
  
  # Make plot
  # plot the log(odds) =  g(x) = beta.0 + beta.1*age + beta.2*male
  # from the logistic regression model.
  # .............................................................
      
  output$oddsplot <- renderPlot({
    
    # predicted probability from model 1
    d1 = cbind(ldata()[,c("x1", "x2")], 
            predict(m.1(), newdata=ldata()[,c("x1", "x2")], 
                    type="link",
                    se=T))
    
    d1 = within(d1, {
        PredictedProb <- plogis(fit)
        LL <- plogis(fit - (1.96 * se.fit))
        UL <- plogis(fit + (1.96 * se.fit))
        x2.f = factor(x2, labels=c("Female", "Male"))
      })
    
    ggplot(d1, aes(x=x1, y=fit, colour=x2.f)) +
      ylab("Log-odds") + 
      xlab("Age") +
      geom_line() +
      scale_colour_discrete("Gender") +
      theme_bw() +
      theme(text = element_text(size=20))
  })
  
  
  # plot the results from model with binomial distribution and linear link (identity)
  # .................................................................................
  
  # binomial distribution with linear link
  m.2 = reactive({
    glm( y ~ x1*x2,
             data=ldata(),
             family=binomial(link="identity"), start=c(expit(input$beta0), 0 , 0, 0)) # model on page 450
  })

  # Output results to a table
  # ############################
  output$summary2 <- renderTable({
    summary(m.2())
  })
  
  # Predicted probability from model 2, linear scale for risk
  # p(age, male) = beta.0 + beta.1*age + beta.2*male + beta.3*age*male
  # plot results
  output$riskplot <- renderPlot({
    d1 = cbind(ldata()[,c("x1", "x2")], 
               predict(m.2(), newdata=ldata()[,c("x1", "x2")], 
                       type="link",
                       se=T))
    d1$x2.f = factor(d1$x2, labels=c("Female", "Male"))
    
    ggplot(d1, aes(x=x1, y=fit, colour=x2.f)) +
      ylab("Risk") + 
      xlab("Age") +
      geom_line() +
      scale_colour_discrete("Gender") +
      theme_bw() +
      theme(text = element_text(size=20))
  })
  
        
  # see http://shiny.rstudio.com/gallery/mathjax.html
  # have to be careful with font sizes.
  output$eqn1 <- renderUI({
    withMathJax(
      helpText('\\( \\text{logit(p) = } \\left(\\frac{p}{1-p}\\right) \\text{ = } \\beta_0 + \\beta_1 \\text{age} + \\beta_2 \\text{male} + \\beta_3 \\text{age} \\times \\text{male}\\)')
    )
    })

  # eqn for binomial distribution, linear link model
  output$eqn2 <- renderUI({
    withMathJax(
      helpText('\\( \\text{Regression model for the risk plot, from page 450: } \\)'),
      helpText('\\( \\text{Risk=} \\pi \\text{(age,male)} = \\beta_0 + \\beta_1 \\text{age} + \\beta_2 \\text{male} + \\beta_3 \\text{age} \\times \\text{male}\\)')
    )
  })
  
    
  # set up text indicating the selected parameters for simulation.
  # .............................................................
  
  output$text0 <- renderUI({
    n = input$n1
    x<-input$beta0
    y<-input$beta1
    z<-input$beta2
    z2 <- input$beta3
    withMathJax(
      sprintf("\\( 
              \\beta_0 = %.02f , 
              \\beta_1 = %.02f ,  
              \\beta_2 = %.02f , 
              \\beta_3 = %.02f ,
              \\text{n} = %d \\)", x, y, z, z2, n)
    )
  })
  
  output$text0i <- renderUI({
    x <- exp(input$beta0)
    y <- exp(input$beta1)
    z <- exp(input$beta2)
    z2 <- exp(input$beta3)
    withMathJax(
      sprintf("\\(
              exp(\\beta_0) = %.02f = \\text{odds of y at x=0 and z=0,}
              \\)",
              x))
    
  })
  
  output$text1i <- renderUI({
    y <- exp(input$beta1)
    withMathJax(
      sprintf("\\(
              exp(\\beta_1) = %.02f = \\text{odds ratio for x=1 vs x=0 with no interaction,}
              \\)",
              y))
    
  })
  
  output$text2i <- renderUI({
    z <- exp(input$beta2)
    withMathJax(
      sprintf("\\(
              exp(\\beta_2) = %.02f = \\text{odds ratio for z=1 vs z=0 with no interaction,}
              \\)",
              z))
    
  })
  
  output$text3i <- renderUI({
    z2 <- exp(input$beta3)
    withMathJax(
      sprintf("\\(
              exp(\\beta_3) = %.02f = \\text{interaction term}
              \\)",
              z2))
    
  })
  
})