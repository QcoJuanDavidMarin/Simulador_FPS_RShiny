library(shiny)
library(plotly)
library(reshape2)
library(DT)
library(shinythemes)
library(openxlsx)
library(shinydashboard)
EMHZnOmod2 <- read.xlsx("ArchivoDatosSimulador.xlsx", sheet = 'Mod2')


ui <- fluidPage(theme = shinythemes::shinytheme("sandstone"),

  sidebarLayout(

    sidebarPanel(box(width =0,h4(strong('Comparación de medias entre pruebas FPS In-vivo e In-vitro'),
                             h6('Juan David Marín, lab: Análisis químico'),
                             img(src = 'aplicacion.png')),
                 sliderInput('octi', h3('Cantidad de EMH'),
                             min = min(EMHZnOmod2$ZnO),
                             max = max(EMHZnOmod2$ZnO),
                             step = 0.1,
                             value = mean(EMHZnOmod2$ZnO), width = 400),
                 sliderInput('cinc', h3('Cantidad de ZnO'),
                             min = min(EMHZnOmod2$ZnO),
                             max = max(EMHZnOmod2$ZnO),
                             step = 0.1,
                             value = mean(EMHZnOmod2$ZnO), width = 400))
                 
                 
      
    ),
    mainPanel(
      tabsetPanel(type = 'tab',
                  tabPanel('',img(src = 'impacto.PNG')),
                  tabPanel('Esk_Pro_Instan_FPS_25', plotlyOutput('FPS25'), 
                           br(),
                           tableOutput('eskpro'),img(src = 'pro.png')),
                       
                  tabPanel('Lbel_defense_total_FPS50',plotlyOutput('FPS50'), 
                           
                           tableOutput('total'),img(src = 'lb50.png')), 
                  
                  tabPanel('LBEL_mousse_matiffiant_FPS15',plotlyOutput('FPS15'),
                           tableOutput('MOUSSE'),img(src = 'mousse.png')),
                  tabPanel('Comportamiento', plotlyOutput('plot3d'),
                           checkboxInput('lm', 'Mostrar superficie de respuesta'),
                           tabItem('FPSMEZCLA',
                                   box(
                                     title = 'FPS Vs EMH y ZnO',
                                     tableOutput('EMHyZnO'), width = 8,
                                     solidHeader = T, collapsible = F,
                                     status = 'success'),
                                   box(img(src = 'ecuaciones.PNG'))
                                  
                           ))
  )
  
  
)))





server <- function(input, output){

 output$FPS25 <- renderPlotly({
    FPS_invitro <- c(29,30,33,29,29,28,30,28,30,29)
    FPS_invivo <- c(25,31.3,25,31.3,31.3,31.3,31.3,25,25,31.3)
    name <- 'FPS'
    Esikapro <- data.frame(FPS_invivo , FPS_invitro, name)
    Esikapro <- melt(Esikapro, id.vars = 'name')
    p<- ggplot(data = Esikapro, aes(x = variable, y = value))+
      geom_boxplot(data = Esikapro, aes(x = variable, y = value, fill = variable, alpha = 0.5))+
      geom_jitter(aes(color = variable, alpha = 1))+
      labs(fill = 'Tecnicas')+
      theme(legend.position = "none")+
      labs(title = 'Comparación FPS in vivo Vs invitro Esk Pro Instan FPS 25',
           x = '', y = 'FPS', grid = F)+
      stat_summary(fun.y = mean, geom = 'point',colour="darkred", size=0.1)+
      stat_summary(fun.y=mean, colour="red", geom="text", show_guide = T, 
                   aes(label=round(..y.., digits=1)))
     
    ggplotly(p)
 
 })
 
 output$eskpro <- renderTable({
   FPS_invitro <- c(29,30,33,29,29,28,30,28,30,29)
   FPS_invivo <- c(25,31.3,25,31.3,31.3,31.3,31.3,25,25,31.3)
  
   Esikapro <- data.frame(FPS_invivo , FPS_invitro)
   Esikapro

   
 })
 
 
 output$FPS50 <- renderPlotly({
   
   FPS_invivo <- c(56,56,56,56,50,56,56,50,50,56)
   FPS_invitro <- c(55,56,57,53,54,56,56,56,56,56)
   name <- 'FPS'
   Esikapro <- data.frame(FPS_invivo , FPS_invitro, name)
   Esikapro <- melt(Esikapro, id.vars = 'name')
   p<- ggplot(data = Esikapro, aes(x = variable, y = value))+
     geom_boxplot(data = Esikapro, aes(x = variable, y = value, fill = variable, alpha = 0.5))+
     geom_jitter(aes(color = variable, alpha = 1))+
     theme(legend.position = "none")+
     labs(title = 'Lbel defense total FPS 50',
          x = '', y = 'FPS')+
     stat_summary(fun.y = mean, geom = 'point',colour="darkred", size=0.1)+
     stat_summary(fun.y=mean, colour="red", geom="text", show_guide = T, 
                  aes(label=round(..y.., digits=1)))
   
   ggplotly(p)
 })
   
   output$total <- renderTable({
     FPS_invivo <- c(56,56,56,56,50,56,56,50,50,56)
     FPS_invitro <- c(55,56,57,53,54,56,56,56,56,56)
     name <- 'FPS'
     Esikapro <- data.frame(FPS_invivo , FPS_invitro, name)
     Esikapro
   
 })
 

 
 output$FPS15 <- renderPlotly({
   FPS_invivo <- c(18.8,18.8,18.8,18.8,18.8,18.8,18.8,15,18.8,18.8)
   FPS_invitro <- c(17,17,17,16,18,18,17,17,18,18)
   name <- 'FPS'
   Esikapro <- data.frame(FPS_invivo , FPS_invitro, name)
   Esikapro <- melt(Esikapro, id.vars = 'name')
   p<- ggplot(data = Esikapro, aes(x = variable, y = value))+
     geom_boxplot(data = Esikapro, aes(x = variable, y = value, fill = variable, alpha = 0.5))+
     geom_jitter(aes(color = variable, alpha = 1))+
     theme(legend.position = "none")+
     labs(title = 'LBEL mousse matiffiant FPS15',
          x = '', y = 'FPS')+
     stat_summary(fun.y = mean, geom = 'point',colour="darkred", size=0.1)+
     stat_summary(fun.y=mean, colour="red", geom="text", show_guide = T, 
                  aes(label=round(..y.., digits=1)))
   
   ggplotly(p)
 })
 
 output$MOUSSE <- renderTable({
   FPS_invivo <- c(18.8,18.8,18.8,18.8,18.8,18.8,18.8,15,18.8,18.8)
   FPS_invitro <- c(17,17,17,16,18,18,17,17,18,18)
   name <- 'FPS'
   Esikapro <- data.frame(FPS_invivo , FPS_invitro, name)
   Esikapro
 })
 

 
 
 #Grafica3D de modelo 2 
 output$plot3d <- renderPlotly({
   lmEMHZnO <- lm(FPS ~ EMH+ZnO+I(EMH^3)+I(EMH*ZnO), data = EMHZnOmod2)
   
   plotEMHZnO <- plot_ly(x = ~EMH, y = ~ZnO, z = ~FPS, data = EMHZnOmod2, 
                         text = ~grupo, type = "scatter3d",
                         mode='markers',
                         color = ~grupo,
                         colors = c("red","blue","green"))
   
   #### Generando las superficie de respuesra
   # graph resolution
   plotresolution <- 0.5
   #Setup Axis
   axis_x <- seq(min(EMHZnOmod2$EMH), max(EMHZnOmod2$EMH), by = plotresolution)
   axis_y <- seq(min(EMHZnOmod2$ZnO), max(EMHZnOmod2$ZnO), by = plotresolution)
   
   #Sample points
   library(reshape2)
   FPS_lm_surface <- expand.grid(EMH = axis_x, ZnO = axis_y, KEEP.OUT.ATTRS = F)
   FPS_lm_surface$FPS <- predict.lm(lmEMHZnO, newdata = FPS_lm_surface)
   FPS_lm_surface <- acast(FPS_lm_surface, ZnO ~ EMH, value.var = 'FPS') 
   
   lrEMHZnO <- if(input$lm)
     plotEMHZnOsurf <- add_trace(p = plotEMHZnO ,
                                 z = FPS_lm_surface,
                                 x = axis_x,
                                 y = axis_y, 
                                 type = 'surface', 
                                 opacity = 0.5,
                                 colorscale = list(c(0, 1), c("gray", "yellow")),
                                 name = 'Ajuste') %>% 
     layout(title = '1.85+(4.15EMH)+(0.480ZnO)-(0.08EMH^3)+(0.103EMH*ZnO)') %>% 
     
     layout(legend = list(orientation = 'h'))
   else plotEMHZnO  
   
 })
 
 
 output$EMHyZnO <- renderValueBox({
    EMHZnOmod2 <- read.xlsx("ArchivoDatosSimulador.xlsx", sheet = 'Mod2')
   lmEMHZnO <- lm(FPS ~ EMH+ZnO+I(EMH^3)+I(EMH*ZnO), data = EMHZnOmod2)
   newdataEMHyZnO <- data.frame(EMH = input$octi , ZnO = input$cinc)
   newdataEMHyZnO
   
   FPSEMHyZnO <- predict(lmEMHZnO, newdataEMHyZnO)
   FPSEMHyZnO
   valueBox(round(FPSEMHyZnO),
            subtitle = '1.85+(4.15EMH)+(0.480ZnO)-(0.08EMH^3)+(0.103EMH*ZnO)',
            icon = icon('atom'),
            color = 'green')
 })
 
}
 
 shinyApp(ui, server)