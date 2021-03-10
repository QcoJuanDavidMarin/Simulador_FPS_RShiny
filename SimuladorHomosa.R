library(openxlsx)
library(plotly)
library(ggiraph)
library(ggiraphExtra)
library(plyr)
library(neuralnet)
library(shiny)
library(shinydashboard)



datos <- read.xlsx('HOMOSALATO.xlsx', sheet = 'lineal')
cf <- read.xlsx('HOMOSALATO.xlsx', sheet = 'curflujo')



cf2 <- read.xlsx('HOMOSALATO.xlsx', sheet = 'curflujo2')

ui <- dashboardPage(skin = 'yellow',
                    dashboardHeader(title = 'Simulador Homosalato'),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem('FPS Homosalato', tabName = 'reghomo', icon = icon('chart-bar')),
                        menuItem('Curva Flujo', tabName = 'curvaflujo', icon = icon('chart-line')),
                        menuItem('Prediccion curva flujo', tabName = 'rncf', icon = icon('gamepad'))
 
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem('reghomo',
                                box(plotlyOutput('CorplotFPS'), width = 12),
                                checkboxInput('RegLine', 'Linea de regresión')
                          
                        ),
                        tabItem('curvaflujo',
                                box(plotlyOutput(outputId = 'curvFlujo'), width = 12),
                                selectizeInput('curvas', 'Curvas de flujo',multiple = T,
                                            choices = unique(cf2$concentracion)),
                                ),
                        tabItem('rncf',
                                box(plotlyOutput(outputId = 'precurv'), width = 12),
                                box(sliderInput('cant', 'Concentración de Homosalato',
                                    min = min(datos$Homosalato),
                                    max = max(datos$Homosalato),
                                    step = 0.1,
                                    value = mean(cf$logShearRate)))
 
                      )
                    ))
)
  
                    


server <- function(input, output){
 ##################FPS#################################### 
  modelFPS <- lm(datos$FPS ~ datos$Homosalato)
  
  output$CorplotFPS <- renderPlotly({
    LinealHomo <- plot_ly(x = datos$Homosalato, y = datos$FPS, type = 'scatter',
                          mode = 'markers', color = datos$FPS, name = 'FPS Homosalato')
    
    LinealHomo <- LinealHomo %>% layout(title = 'FPF Vs EMH',
                                        xaxis = list(title = '%Homosalato'),
                                        yaxis = list(title = 'FPS'))
      lr <- if(input$RegLine)
        add_trace(LinealHomo,
                  x = datos$Homosalato,
                  y = fitted(lm(datos$FPS ~ datos$Homosalato)),
                  mode = 'lines', name = 'y = 0.64x + 1.63')
        else  LinealHomo 
     

  })
  ######################################################################  
  #################cURVA DE FLUJO
  cf <- read.xlsx('HOMOSALATO.xlsx', sheet = 'curflujo')


    output$curvFlujo <- renderPlotly({
      cf2 <- read.xlsx('HOMOSALATO.xlsx', sheet = 'curflujo2')
      
      curva <- plot_ly(cf2, x = ~logShearRate, y = ~logn, color = ~logn, name = '') %>%
        filter(concentracion %in% input$curvas) %>%
        group_by(concentracion) %>%
        add_markers(symbol = I(15), alpha = 1)  %>% 
        layout(title = 'curva de flujo',
                 xaxis = list(title = 'y(s-1)'),
                 yaxis = list(title = 'n.(Pa-s)')) 

  })
  
    
  
    
    
  output$precurv <- renderPlotly({
    predcf <- read.xlsx('HOMOSALATO.xlsx', sheet = 'predcurvflujo')
    set.seed(123)
    rn <- neuralnet(a290+a291+a292+a293+a294+a295+a296+a297+a298+a299+a300+
                      a301+a302+a303+a304+a305+a306+a307+a308+a309+a310+a311+
                      a312+a313+a314+a315+a316+a317+a318+a319+a320+a321+a322+
                      a323+a324+a325+a326+a327+a328+a329+a330 ~ Homosalato, 
                    data = predcf, hidden = c(4,8,5,3),
                    threshold = 0.01)
    
    canti <- data.frame(Homosalato = input$cant)
    predict <- neuralnet::compute(rn, canti)
    resultado <- data.frame(predict$net.result)
    resultadot <- data.frame(t(resultado))
    resultadot$shear_rate <- cf$logShearRate
    curflujo <- plot_ly(x= resultadot$shear_rate, y = resultadot$t.resultado.,
                        color = resultadot$t.resultado.,
                        name = 'predicción') %>% 
      add_markers(symbol = I(18), alpha = 1)
    curflujo <- curflujo %>% layout(title = 'curva de flujo',
                                        xaxis = list(title = 'y(s-1)'),
                                        yaxis = list(title = 'n.(Pa-s)'))
    
    curflujo
    
    
  })
  
 
}






shinyApp(ui, server)

