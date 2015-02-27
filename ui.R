# ui.R

library(shiny)

shinyUI(pageWithSidebar(
  headerPanel("Demonstration of additive and multiplicative effect modification"),
  sidebarPanel(
    h2("Select parameters for simulation"),
    p("Regression coefficients for logistic regression (defaults based on parameter estimates from page 449)"),
    sliderInput("n1","Sample size", min=50, max=5000, value=100, step=1, format="###", animate=FALSE),
    sliderInput("beta0","Intercept:", min=-7, max=1, value=-5, step=0.5, format="#.#", animate=FALSE),
    sliderInput("beta1","Coefficient for x1:", min=-3, max=1, value=0.05, step=0.05, format="#.#", animate=FALSE),
    sliderInput("beta2","Coefficient for x2", min=-3, max=3, value=1.25, step=0.25, format="#.#", animate=FALSE),
    sliderInput("beta3","Coefficient for interaction between x1 and x2", min=-3, max=3, value=0, step=0.1, format="#.#", animate=FALSE),
    br()
    ),
  
  mainPanel(
    h2("Based on example in the Hosmer and Lemeshow 2013 textbook, 'Applied Logistic Regression'"),
    h2("ISBN: 9780470582473. pages 448-456"),
    tags$a(href="http://www.wiley.com/WileyCDA/WileyTitle/productCd-0470582472.html", "Link to book"),
    h2("Part 1: using example from section 10.9.1, pages 448-451"),
    withMathJax(),
    h3("Model used to simule data, p449"),
    h1(uiOutput("eqn1")),
    br(),
    h3("Current selected coefficients for simulation:"),
    h1(uiOutput("textn")),
    h1(uiOutput("text0")),
    h3("Sample of simulated data"),
    tableOutput("table1"),
    br(),
    h3("Frequencies by x1 and y"),
    tableOutput("table2alt"),
    br(),
    h3("Multiplicative Scale"),
    tags$hr(),
    h3("Summary of logistic regression"),
    tableOutput("summary"),
    br(),
    h3("Plot of log odds by groups"),
    plotOutput("oddsplot"),
    h3("Additive Scale"),
    tags$hr(),
    h3("Summary of data analyzed with binomial distribution regression and linear link instead of logit link"),
    tableOutput("summary2"),
    br(),
    h3("Plot of same data using a generalized regression model with a binomial distribution and linear link (estimate risks instead of log odds)"),
    h1(uiOutput("eqn2")),
    plotOutput("riskplot"),
    p("The results of this demonstration, when set at the defaults for section 10.9.1, show no interaction on the multiplicative scale (first, logistic regression) and interaction on the additive scale (second, linear link regression)")
  )

))