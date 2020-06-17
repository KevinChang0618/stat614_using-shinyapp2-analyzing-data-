library(tidyverse)
library(ggstance)
library(shiny)
df <- read_csv("data/estate.csv")
df %>%
  mutate(AC = as.factor(AC),
         AC = fct_recode(AC, "AC" = "1",
                         "No AC" = "0"),
         AC = fct_rev(AC)) %>%
  mutate(Pool = as.factor(Pool),
         Pool = fct_recode(Pool, "Pool" = "1",
                           "No Pool"= "0"),
         Pool = fct_rev(Pool)) %>%
  mutate(Quality = as.factor(Quality))%>%
  mutate(Style = as.factor(Style))%>%
  mutate(Highway = as.factor(Highway),
         Highway = fct_recode(Highway, "Highway"= "1",
                              "No Highway"= "0"),
         Highway = fct_rev(Highway)) -> df
df$Quality <- factor(df$Quality, levels = c("High", "Medium", "Low"))
##fix quantity & caterogical


ui <- fluidPage(
  titlePanel("EDA of Estate Data"),
  tabsetPanel(
    tabPanel(title = "Univariate Analyses",
             sidebarLayout(
               sidebarPanel = sidebarPanel(
                 varSelectInput("x", "Variable", data=df),
                 checkboxInput("log", "Log"),
                 sliderInput("bin", "Bins", min = 1, max = 100, value = 20),
                 numericInput("null_value", "Null Value", value = 0),
                 tableOutput("p_value")#output dataframe
               ),
               mainPanel = mainPanel(
                 plotOutput("hist_bar")
               )
             )),
    tabPanel(title = "Bivariate Analyses",
             sidebarLayout(
               sidebarPanel = sidebarPanel(
                 varSelectInput("var1", "Variable 1", data= df, selected = "Area"),
                 checkboxInput("log1", "Log"),
                 varSelectInput("var2", "Variable 2", data= df, selected = "Price"),
                 checkboxInput("log2", "Log"),
                 checkboxInput("ols", "OLS!")
               ),
               mainPanel = mainPanel(
                 plotOutput("scatter_box_jit_revbox")
               )
             )),
    tabPanel(title = "Spreadsheet",
             dataTableOutput("table"))
  )
)

server <- function(input, output, session) {
  
  output$hist_bar <- renderPlot({
    pl <- ggplot(df, aes(x= !!input$x))+
          theme_bw()
    if(is.factor(df[[input$x]])){
      pl <- pl+ geom_bar(color="black", fill="white")
    }else{
     if(input$log==TRUE){
        pl <- pl+ geom_histogram(bins = input$bin, color="black", fill="white")+
            scale_x_log10()
      }else{
        pl <- pl+ geom_histogram(bins = input$bin, color="black", fill="white")
      }
    }
    pl
  })
  output$p_value <- renderTable({
    
    
    if(is.numeric(df[[input$x]])){
      df_p = tribble(~"P-value",                       ~"Lower",                     ~"Upper",
                    t.test(df[[input$x]], mu= input$null_value)[[3]][[1]],  t.test(df[[input$x]], mu= input$null_value)[[4]][[1]],  t.test(df[[input$x]], mu= input$null_value)[[4]][[2]]
                    )
    if(input$log==TRUE)
      df_p = tribble(~"P-value",                       ~"Lower",                     ~"Upper",
                     t.test(log2(df[[input$x]]+1), mu= input$null_value)[[3]][[1]],  t.test(log2(df[[input$x]]+1), mu= input$null_value)[[4]][[1]],  t.test(log2(df[[input$x]]+1), mu= input$null_value)[[4]][[2]]
                    )
    }else{
      df_p = tribble(~"data",
                      "Not a numeric"
                    )
    }
    df_p 
  })
  #tab1

  output$scatter_box_jit_revbox <- renderPlot({
    pl <- ggplot(df, aes(x= !!input$var1, y= !!input$var2))+
          theme_bw()
    if(!is.factor(df[[input$var1]]) && !is.factor(df[[input$var2]])){
        pl <- pl+ geom_point()
      if(input$log1==TRUE)
        pl <- pl+ scale_x_log10()
      if(input$log2==TRUE)
        pl <- pl+ scale_y_log10()
      if(input$ols==TRUE)
        pl <- pl+ geom_smooth(method = lm, se = FALSE)
    }else if(is.factor(df[[input$var1]]) && !is.factor(df[[input$var2]])){
        pl <- pl+ geom_boxplot()
      if(is.factor(df[[input$var1]]) && !is.factor(df[[input$var2]]) && input$log2==TRUE)
        pl <- pl+ geom_boxplot()+ scale_y_log10()
    }else if(is.factor(df[[input$var1]]) && is.factor(df[[input$var2]])){
        pl <- pl+ geom_jitter()
    }else if(!is.factor(df[[input$var1]]) && is.factor(df[[input$var2]])){
        pl <- pl+ geom_boxploth()
      if(!is.factor(df[[input$var1]]) && is.factor(df[[input$var2]]) && input$log1==TRUE)
        pl <- pl+ geom_boxploth()+ scale_x_log10()
    }
      pl
  })
  #tab2
  
  output$table <- renderDataTable({df %>% keep(~is.numeric(.))},
                                options = list(pageLength = 10))
  #tab3
}
shinyApp(ui, server)
