library(factoextra)
library(FactoMineR)
library(factorEx)
library(rgl)
library(ggplot2)
library(ggbiplot)
library(plotly)
library(shiny)
library(shinydashboard)
library(DT)
library(GGally)
library(psych)
library(Hmisc)
library(MASS)
library(d3heatmap)  
library(cluster)
library(fpc)
library(NbClust)
library(tidyverse)
library(neuralnet)
library(dygraphs)
library(shinythemes)
library(dashboardthemes)

teoricoFPS <- data.frame(Lo = c(290,291,292,293,294,295,296,297,298,299,300,301,302,303,304,305,306,307,308,309,310,311,312,313,314,315,316,317,318,319,320,321,322,323,324,325,326,327,328,329,330,331,332,333,334,335,336,337,338,339,340,341,
                                342,343,344,345,346,347,348,349,350,351,352,353,354,355,356,357,358,359,360,361,362,363,364,365,366,367,368,369,370,371,372,373,374,375,376,377,378,379,380,381,382,383,384,385,386,387,388,389,390,391,392,393,
                                394,395,396,397,398,399,400),
                         num = c(0.000008741,0.000014500,0.000026589,0.000045745,0.000100570,0.000258940,0.000703490,0.001677600,0.003726800,0.006393187,0.009588049,0.013131046,0.017567581,0.021086352,0.023715829,0.026726464,0.028583575,0.028358035,
                                 0.028506361,0.026758044,0.025008033,0.023220870,0.020822301,0.019001980,0.016047196,0.014047820,0.012106600,0.010240148,0.008655035,0.007303116,0.006187458,0.005075764,0.004257950,0.003553161,0.002873215,0.002401862,
                                 0.001968354,0.001608549,0.001330636,0.001264272,0.001229464,0.001207681,0.001201445,0.001161920,0.001120934,0.001098896,0.001071787,0.001046995,0.001011272,0.000996670,0.000960128,0.000939689,0.000912209,0.000880889,
                                 0.000859406,0.000833232,0.000810996,0.000784675,0.000761766,0.000726994,0.000714960,0.000688373,0.000663663,0.000640771,0.000621154,0.000601738,0.000575523,0.000558135,0.000532600,0.000518882,0.000502133,0.000479621,
                                 0.000462253,0.000443592,0.000421025,0.000405759,0.000380740,0.000366804,0.000344847,0.000326106,0.000307698,0.000288918,0.000271046,0.000251022,0.000234043,0.000213866,0.000199164,0.000179991,0.000163282,0.000147280,
                                 0.000129612,0.000115459,0.000101508,0.000086026,0.000072457,0.000060951,0.000050624,0.000040928,0.000033131,0.000026426,0.000020489,0.000015605,0.000011661,0.000008567,0.000006000,0.000004170,0.000002887,0.000001888,
                                 0.000001239,0.000000780,0.000000507))
teoricoUVA <- data.frame(Lo = c(320,321,322,323,324,325,326,327,328,329,330,331,332,333,334,335,336,337,338,339,340,341,342,343,344,345,346,347,348,349,350,351,352,353,354,355,356,357,358,359,360,361,362,363,364,365,366,367,
                                368,369,370,371,372,373,374,375,376,377,378,379,380,381,382,383,384,385,386,387,388,389,390,391,392,393,394,395,396,397,398,399,400),
                         numuva = c(0.0000048,0.0000083,0.0000129,0.0000192,0.0000273,0.0000376,0.0000488,0.0000627,0.0000788,0.0000942,0.0001129,0.0001313,0.0001493,0.0001649,0.0001842,0.0001991,0.0002154,0.0002288,
                                    0.0002413,0.0002509,0.0002599,0.0002769,0.0002925,0.0003073,0.0003203,0.0003341,0.0003456,0.0003554,0.0003684,0.0003743,0.0003834,0.0003905,0.0003953,0.0003978,0.0004022,0.0004014,
                                    0.0004046,0.0004057,0.0004057,0.0004058,0.0004054,0.0004016,0.0003992,0.0003913,0.0003861,0.0003795,0.0003702,0.0003614,0.0003533,0.0003430,0.0003292,0.0003159,0.0003002,0.0002866,
                                    0.0002709,0.0002563,0.0002424,0.0002281,0.0002130,0.0001966,0.0001791,0.0001636,0.0001465,0.0001298,0.0001134,0.0000980,0.0000833,0.0000702,0.0000581,0.0000472,0.0000382,0.0000301,
                                    0.0000233,0.0000176,0.0000130,0.0000095,0.0000068,0.0000048,0.0000032,0.0000021,0.0000013))

ui <- dashboardPage(skin = 'purple',
                    dashboardHeader(title = 'SIMULADOR FPS', titleWidth = 450),
                    dashboardSidebar(h6('Qco Juan David Marín'),
                      img(src = 'logo.png', height = 100, width = 230),
                     sidebarMenu(
                       menuItem('Cargar la data', tabName = 'subirdata', icon = icon('vials')),
                       menuItem('Matriz de correlaciones', tabName = 'corrplots', icon = icon('gamepad')),
                       menuItem('Heatmap', tabName = 'calormap',icon = icon('chart-bar')),
                       menuItem('ACP', tabName = 'acp', icon = icon('arrow-alt-circle-right')),
                       menuItem('Número de clusters sugeridos', tabName = 'numclusters', icon = icon('balance-scale')),
                       menuItem('Selección tipo cluster', tabName = 'kmopam', icon = icon('bullhorn')),
                       menuItem('CORRELACIÓN',icon = icon('braille'),
                                menuSubItem('Agrupamiento', tabName = 'plotcluster', icon = icon('shapes')),
                                menuSubItem('Análisis', tabName = 'analisis', icon = icon('brain'))),
                       # menuItem('AGRUPAMIENTO', tabName = 'plotcluster', icon = icon('shapes')),
                       menuItem('SELECCIONE MODELO', tabName = 'RNA', icon = icon('hand-point-right'),
                                menuSubItem('Modelo1', tabName = 'model1'),
                                menuSubItem('Modelo2', tabName = 'model2')),
                       sliderInput('octinoxato', label = h6('Cantidad de EMH'),min = 0,max = 12, step = 0.1, value = 6),
                       sliderInput('oxidocinc', label = h6('Cantidad de ZnO'),min = 0,max = 12, step = 0.1, value = 6)
                     )

                    ),
                    dashboardBody(
                      tabItems(tabItem('subirdata',
                                       radioButtons(inputId = 'header',
                                                    label = 'Header',
                                                    choices = c('Columns have headers'='Yes',
                                                                'Columns do not have headers'='No'), 
                                                    selected = 'Yes'),
                                       radioButtons('sep', 'Separator',
                                                    c(Comma=',',
                                                      Semicolon=';',
                                                      Tab='\t'),
                                                    ';'),
                                       radioButtons('quote', 'Quote',
                                                    c(None='',
                                                      'Double Quote'='"',
                                                      'Single Quote'="'"),
                                                    '"'),
                                       tags$hr(),
                                       
                                       box(fileInput('file1', 'Seleccione un archivo CSV',
                                                 accept = c('text/csv',
                                                            'text/comma-separated-values',
                                                            'text/tab-separated-values',
                                                            'text/plain',
                                                            '.csv',
                                                            '.tsv')), background = 'purple')), #End file tabitem subirdata
                               tabItem('corrplots',
                                       box(h1('GRÁFICO CORRELACIONES'),background = 'purple', width = 1000),
                                             d3heatmapOutput('plotcorr', width = 1000, height = 600),
                                       checkboxInput('correlaciones', 'Mostrar correlacion entre Bases MQ')), #End file tabitem Corrplots
                               
                               tabItem('calormap',
                                       box(h1('HEATMAP MPs y PRODUCTOS'),background = 'purple', width = 300, height =120),
                                       checkboxInput('clusterheatmap', 'Mostrar Dendogramas'),
                                       d3heatmapOutput('HEATMAP', width = 1000, height = 600),
                                       h2('Base de Datos'),
                                       DT::dataTableOutput('dataset')),#End  tabitem calormap
                              
                                tabItem('acp',
                                       box(h1('ANÁLISIS DE COMPONENTES PRINCIPALES'),background = 'purple', width = 300),
                                       plotlyOutput('graficoACP', height = 600),
                                       br(),
                                       box(checkboxInput('vectores', 'Matriz de loadings'),background = 'light-blue',
                                           width = 3, height = 40, align = 'center'),
                                       br(),
                                       br(),
                                       br(),
                                       h3('Varianza explicada por los componentes Principales'),
                                       verbatimTextOutput('ResumenACP'),
                                       plotOutput('varcp'),
                                       h2('Componentes principales de los datos'),
                                       DT::dataTableOutput('cprepresentation')), #End  tabitem acp
                               tabItem('numclusters',
                                       box(h3('Número de clusters sugeridos por método silhouette (for average silhouette width)'),background = 'purple', width = 250),
                                       plotOutput('silhouette'),
                                       h3('Número de clusters sugeridos por método wss (for total within sum of square)'),
                                       plotOutput('wss')),
                               tabItem('kmopam', # Inico comparaciones de algoritmos kmeas y PAM
                                       box(h3('Evaluación del algoritmo de clasificación'),background = 'purple', width = 10, height = 60),
                                       box(width =  4, height = 4,
                                         title = 'k-means',
                                         tableOutput('valkmeans'),
                                         solidHeader = T, collapsible = T,
                                         status = 'warning'),
                                         box(sliderInput('km',h3('Número de clusters k-means'),
                                                         min = 2,
                                                         max = 12,
                                                         value = 6)),
                                       box(width =  4, height = 4,
                                           title = 'k-medoide',
                                           tableOutput('valmedoides'),
                                           solidHeader = T, collapsible = T,
                                           status = 'danger'),
                                       box(sliderInput('kmedoides',h3('Número de clusters Partitioning Around Medoids'),
                                                       min = 2,
                                                       max = 12,
                                                       value = 6)),# fin comparaciones de algoritmos kmeas y PAM
                                       box(width =  6, height = 4, # Grafica particion kmeans
                                           title = 'Gráfico partición k-means',
                                           plotOutput('partitionkmean'),
                                           solidHeader = T, collapsible = T,
                                           status = 'primary'),
                                       box(width = 6, height= 4, # Grafica particion PAM
                                           title = 'Gráfico partición PAM',
                                           plotOutput('partitionPAM'),
                                           solidHeader = T, collapsible = T,
                                           status = 'info')

                                       ),
                               tabItem('plotcluster',
                                       box(h1('REPRESENTACIÓN CLUSTER 3D'),background = 'purple', width = 100, height = 70),
                                       plotlyOutput('plot3dclus'),
                                       # plotlyOutput('plot3dclus', width = 1090, height = 500),
                                       box(checkboxInput('plotPAM','Mostrar agrupamiento con clusters K-MEANS'),
                                           background = 'light-blue', width = 5, height = 45, align = 'center'),
                                       DT::dataTableOutput('matrizcluster')
                                       ),
                               tabItem('analisis', #####Graficos análisis de los clusters 
                                       plotlyOutput('lineas'),
                                       plotlyOutput('puntos'),
                                       checkboxInput('lineaskmean', 'Análisis Clusters con K-MEANS'),
                                       checkboxInput('puntoskmean', 'Análisis grupos conK-MEANS'),
                                       ),
                               tabItem('model1', ##Ingreso de grafica y cuadors de la prediccion para el modelo 1
                                       
                                   
                                          
                                       # box(#### Grafoco de regresión 3D para el modelo 1
                                       #   plotlyOutput(outputId = 'regresionMD13D'))

                                       box(
                                           dygraphOutput(outputId = 'PLOTTER1'), width = NULL),
                                       checkboxInput('LOC','Mostrar LONGITUD DE ONDA CRITICA'),
                                  
                                     
                                       
                                   
                                       box( ## PREDICCION DEL FPS
                                         title = 'FPS', 
                                         tableOutput('FPSUVB'), width = 2,height = 2,
                                         solidHeader = T, collapsible = T, 
                                         status = 'primary'),
                                       
                                       box(
                                         title = '-IC 95%', ### INTERVALO DE CONFIANZA MENOR
                                         tableOutput('LESSIC'), width = 2,height = 1,
                                         solidHeader = T, collapsible = T, 
                                         status = 'primary'),
                                       
                                       box(
                                         title = '+IC 95%', ### INTERVALO DE CONFIANZA MAYOR
                                         tableOutput('PLUSIC'), width = 2,height = 1,
                                         solidHeader = T, collapsible = T, 
                                         status = 'primary'),
      
                                       
                                       box(
                                         title = 'FPUVA',
                                         tableOutput('FPSUVA'), width = 2,height = 2,
                                         solidHeader = T, collapsible = T, 
                                         status = 'primary'),
                                       box(
                                         title = 'Long. onda crítica',
                                         tableOutput('longcrit'), width = 3, height = 2,
                                         solidHeader = T, collapsible = T, 
                                         status = 'primary'),
                                       br(),
                                       br(),
                                       br(),
                                       br(),
                                       br(),
                                       br(),
                                       br(),
                                       box(#### Grafoco de regresión 3D para el modelo 1
                                         plotlyOutput(outputId = 'regresionMD13D'), collapsible = T, height = 600,
                                         box(title = 'Costos Simulador por Kg',
                                             tableOutput('basessimulamod1'), width = 15)),
                                       #### Incluir las cuadros de los precions y calculo del mod1

                                       # box(title = 'Costos Simulador por Kg',
                                       #     tableOutput('basessimulamod1'), width = 3, background = 'blue', collapsible = T), ## cOSTOS CALCULADOS
                                       box(title = strong('Costos filtros por Kg de formulación'),
                                           DT::dataTableOutput('basesmod1'), collapsible = T), ## cOSTOS CON FPS IN VIVO
                                       
                                      
                                       
                                       ),
                               tabItem('model2',
                                       
                               
                   
                                       box(
                                         dygraphOutput(outputId = 'PLOTTER2'), width = NULL),
                                       checkboxInput('LOC1','Mostrar LONGITUD DE ONDA CRITICA'),
                                       
                                       box(### Prediccion del FPS modelo 2
                                         title = 'FPS', 
                                         tableOutput('FPSUVB2'), width = 2,height = 2,
                                         solidHeader = T, collapsible = T, 
                                         status = 'primary'),
                                       
                                       box(
                                         title = '-IC 95%', ### INTERVALO DE CONFIANZA MENOR
                                         tableOutput('LESSIC2'), width = 2,height = 1,
                                         solidHeader = T, collapsible = T,
                                         status = 'primary'),

                                       box(
                                         title = '+IC 95%', ### INTERVALO DE CONFIANZA MAYOR
                                         tableOutput('PLUSIC2'), width = 2,height = 1,
                                         solidHeader = T, collapsible = T,
                                         status = 'primary'),
                                       
                                       box(
                                         title = 'FPUVA',
                                         tableOutput('FPSUVA2'), width = 2,height = 2,
                                         solidHeader = T, collapsible = T, 
                                         status = 'primary'),
                                       box(
                                         title = 'Long. onda crítica',
                                         tableOutput('longcrit2'), width = 3,height = 2,
                                         solidHeader = T, collapsible = T, 
                                         status = 'primary'),
                                       br(),#### Incluir las cuadros de los precions y calculo del mod2
                                       br(),
                                       br(),
                                       br(),
                                       br(),
                                       br(),
                                       br(),
                                       box(#### Grafoco de regresión 3D para el modelo 2
                                         plotlyOutput(outputId = 'regresionMD23D'),
                                         box(title = 'Costos Simulador por Kg',
                                             tableOutput('basessimulamod2'), width = 25, collapsible = T)),
                                     
                                       box(title = strong('Costos filtros por Kg de formulación'),
                                           DT::dataTableOutput('basesmod2'), collapsible = T),
                                       
                                       
                                       
                                    

                             
                               )
                    )))
              

server <- function(input, output, session){
  
  data_final <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    bases <- read.csv(inFile$datapath, header = (input$header =='Yes'),
                      sep = input$sep, quote= input$quote, stringsAsFactors = F)
    return(bases)
  })

  
  #Graficos de correlaciones
  output$plotcorr <- renderD3heatmap({
    bases <- data_final()
    row.names(bases)<-bases$X
    bases$X<-NULL
    
    dist_bases <- as.matrix(dist(bases), method = "euclidean")
    dist_bases2 <- cor(bases, method = "pearson")

    plot1 <- d3heatmap(dist_bases2, scale = "none",
                       clustering_distance_rows = "euclidean",
                       clustering_distance_cols = "euclidean",
                       clustering_method = "average",cutree_rows = 14,
                       fontsize = 9, cexRow = 0.6)
    if(input$correlaciones)
      plot2<- d3heatmap(dist_bases, scale = "none",
                clustering_distance_rows = "euclidean",
                clustering_distance_cols = "euclidean",
                clustering_method = "average",cutree_rows = 14,
                fontsize = 9, cexRow = 0.7)

      else plot1
  })
  
  
  
  output$HEATMAP <- renderD3heatmap({
    
    bases <- data_final()
    row.names(bases)<-bases$X
    bases$X<-NULL
    d3heatmap(
      scale(bases),
      colors = "Blues",k_row = input$kmedoides, k_col =input$kmedoides, scale = "column",
      distfun = function(x){dist(x, method = "euclidean")},
      hclustfun = function(x){hclust(x, method = "average")},
      cexRow = 0.7, cexCol = 0.5, main = "Impacto Mps", 
      brush_color = "#RRGGBB",

      dendrogram = if(input$clusterheatmap) "both" else "none")
    
   
    })
  #Tabla de la data BASES
  output$dataset <- DT::renderDataTable({
    bases <- data_final()
    row.names(bases)<-bases$X
    bases$X<-NULL
    DT::datatable(bases, filter = 'top',
                  options = list(scrollY=600, 
                                 scrollX=300, 
                                 scroller = TRUE,
                                 deferRender = TRUE,
                                 pageLength = 250))
      

    
  })
  
  #Grafico del ACP con ggbiplot
  output$graficoACP <- renderPlotly({
    bases <- data_final()
    row.names(bases)<-bases$X
    bases$X<-NULL
    acpplot <- prcomp(bases, scale = T)
    p <- ggbiplot(acpplot, obs.scale  = 2,var.scale = 1,
                  alpha = 0, varname.size = 2,var.axes=if(input$vectores) T else F, 
                  color = 'gray',
                  ellipse.prob = 0.95, text =km$cluster)+
      
      geom_point(aes(color = rownames(bases)
      ))
    
    ggplotly(p,dynamicTicks = TRUE)  
      
   
  })
  #Tabla Resumen del ACP 
  output$ResumenACP <- renderPrint({
    bases <- data_final()
    row.names(bases)<-bases$X
    bases$X<-NULL
    acpplot <- prcomp(bases, scale = T, center = T)
    summary(acpplot)
  })
 
  output$varcp <- renderPlot({
    bases <- data_final()
    row.names(bases)<-bases$X
    bases$X<-NULL
    acpplot <- prcomp(bases, scale = T, center = T)
    fviz_screeplot(acpplot, addlebels = TRUE, ylim = c(0,25))
  }) 
  output$cprepresentation <- DT::renderDataTable({
    bases <- data_final()
    row.names(bases)<-bases$X
    bases$X<-NULL
    acpplot <- prcomp(bases, scale = T, center = T)
    DT::datatable(round(data.frame(acpplot$x),2),
                  filter = 'top',
                  extensions = c('Scroller'),
                  options = list(scrollY=600, 
                                 scrollX=300, 
                                 scroller = TRUE,
                                 deferRender = TRUE,
                                 pageLength = 250))
  })   
  
  #Concejo de cuantos clusters con silhouette
output$silhouette <- renderPlot({
  bases <- data_final()
  row.names(bases)<-bases$X
  bases$X<-NULL
  # dataesca <- data.frame(scale(bases))
  fviz_nbclust(bases, kmeans, method = 'silhouette')
})
#Concejo de cuantos clusters con wss
output$wss <- renderPlot({
  bases <- data_final()
  row.names(bases)<-bases$X
  bases$X<-NULL
  # dataesca <- data.frame(scale(bases))
  fviz_nbclust(bases, kmeans, method = 'wss')
  
})
 #Mostrar el dato de la evaluacion del modelo k-means y k-medoides
#kmeans
output$valkmeans <- renderValueBox({
  bases <- data_final()
  row.names(bases)<-bases$X
  bases$X<-NULL
  # dataesca <- data.frame(scale(bases))
  set.seed(123)
  km <- kmeans(bases,centers = input$km,nstart = 25, iter.max = 100)
  dd <- dist(bases, method = 'euclidean')
  km_stats <- cluster.stats(dd, km$cluster)
  km_stats$dunn
  valueBox(round(km_stats$dunn,4),
  subtitle = 'Indice de dunn para K-means',
  icon=icon('fire'),
  color = 'orange')
  
})
# k medoides
output$valmedoides <- renderValueBox({
  bases <- data_final()
  row.names(bases)<-bases$X
  bases$X<-NULL
  # dataesca <- data.frame(scale(bases))
  set.seed(123)
  kmpan <- pam(bases,k =input$kmedoides, metric = 'euclidean')
  dd <- dist(bases, method = 'euclidean')
  kmed_stad <- cluster.stats(dd, kmpan$clustering)
  kmed_stad$dunn
  valueBox(round(kmed_stad$dunn,4),
           subtitle = 'Indice de dunn para PAM',
           icon=icon('fire-alt'),
           color = 'red')

})
#Grafico de particion de k means
output$partitionkmean <- renderPlot({
  bases <- data_final()
  row.names(bases)<-bases$X
  bases$X<-NULL
  # dataesca <- data.frame(scale(bases))
  set.seed(123)
  km <- kmeans(bases,centers = input$km,nstart = 25, iter.max = 100)
  sil.km <- silhouette(km$cluster, dist(bases))
  fviz_silhouette(sil.km)
  
})
#Grafico de particion de PAM
output$partitionPAM <- renderPlot({
  bases <- data_final()
  row.names(bases)<-bases$X
  bases$X<-NULL
  # dataesca <- data.frame(scale(bases))
  set.seed(123)
  kmpan <- pam(bases,k =input$kmedoides, metric = 'euclidean')
  sil.kmPAM <- silhouette(kmpan$cluster, dist(bases))
  fviz_silhouette(sil.kmPAM)
})

output$plot3dclus <- renderPlotly({
  bases <- data_final()
  row.names(bases)<-bases$X
  bases$X<-NULL
  acp <- prcomp(bases, scale = T, center = T)
  # dataesca <- data.frame(scale(bases))
  
  set.seed(123)
  km <- kmeans(bases,centers = input$km,nstart = 25, iter.max = 100)

  set.seed(123)
  kmpan <- pam(bases,k =input$kmedoides, metric = 'euclidean')
 
  plotPAM3D <-plot_ly(x =~acp$x[,1], y = ~acp$x[,2], z= ~acp$x[,3],
                      text = ~paste('Nombre:',rownames(bases), '<br>Cluster:',kmpan$cluster),
                      type = "scatter3d",mode='markers', name = ~kmpan$cluster) %>% 
    layout(title = 'Agrupamiento por PAM') %>% 
    layout(scene = list(xaxis = list(title = 'CP1'),
                        yaxis = list(title = 'CP2'),
                        zaxis = list(title = 'CP3')))

    
    if(input$plotPAM)
      
      plotkm3D <- plot_ly(x =~acp$x[,1], y = ~acp$x[,2], z= ~acp$x[,3], 
                          text = ~paste('Nombre:',rownames(bases), '<br>Cluster:',km$cluster),
                          type = "scatter3d",mode='markers', name = ~km$cluster) %>% 
    layout(title = 'Agrupamiento por K-Meas') %>% 
    layout(scene = list(xaxis = list(title = 'CP1'),
                        yaxis = list(title = 'CP2'),
                        zaxis = list(title = 'CP3')))

    
    else plotPAM3D
  
  
    
})
## Grafico de la representacón de los clusters por las materias primas ###                        PARA HACER ANáLSIS
output$lineas <- renderPlotly({
  bases <- data_final()
  row.names(bases)<-bases$X
  bases$X<-NULL
  bases22 <- data_final()
  set.seed(123)
  kmpan <- pam(bases,input$kmedoides, metric = 'euclidean')
  set.seed(123)
  km <- kmeans(bases,input$km,nstart = 25, iter.max = 100)
  bases$clus <- as.factor(km$cluster)
  bases22$clus <- as.factor(kmpan$clustering)
  ##Cambiar la matriz a la diagonal 
  bases22$clus <- factor(bases22$clus)
  bases_long2 <- gather(bases22, materia_prima, cantidad, X10000196:X10003886, factor_key = T)
  
  bases$clus <- factor(bases$clus)
  bases_long <- gather(bases, materia_prima, cantidad, X10000196:X10003660, factor_key = T)
  
  pp<- ggplot(bases_long2, aes(as.factor(x=materia_prima), y = cantidad, group = clus, 
                              colour = clus, label = bases_long2$X))+
    stat_summary(fun = mean, geom = 'pointrange', size =1)+
    stat_summary(geom = 'line')+
    geom_point()+
    # geom_point(aes(x = X,group = clus))+
    theme(text = element_text(size = 14), legend.position = 'bottom', 
          axis.text.x = element_text(angle = 90, hjust = 1,size = 6))+
    labs(title = 'Representación de los grupos por PAM',
         subtitle = 'Materias primas en los clusters',
         x = '', y = '% Materia prima')
  

  
  p<- ggplot(bases_long, aes(as.factor(x=materia_prima), y = cantidad, group = clus, 
                             colour = clus, label = bases_long2$X))+
    stat_summary(fun = mean, geom = 'pointrange', size =1)+
    stat_summary(geom = 'line')+
    geom_point()+
    # geom_point(aes(x = X,group = clus))+
    theme(text = element_text(size = 14), legend.position = 'bottom', 
          axis.text.x = element_text(angle = 90, hjust = 1,size = 6))+
    labs(title = 'Representación de los grupos por K-MEAN',
         subtitle = 'Materias primas en los clusters',
         x = '', y = '% Materia prima')

  if(input$lineaskmean)
  ggplotly(p) 
  else ggplotly(pp) 
  
})

## Grafico de la representacón de los clusters por los productos bases
output$puntos <- renderPlotly({
     
    bases <- data_final()
    row.names(bases)<-bases$X
    bases$X<-NULL
    bases22 <- data_final()
    set.seed(123)
    kmpan <- pam(bases,input$kmedoides, metric = 'euclidean')
    set.seed(123)
    km <- kmeans(bases,input$km,nstart = 25, iter.max = 100)
    bases$clus <- as.factor(km$cluster)
    bases22$clus <- as.factor(kmpan$clustering)
    ##Cambiar la matriz a la diagonal para PAM
    bases22$clus <- factor(bases22$clus)
    bases_long2 <- gather(bases22, materia_prima, cantidad, X10000196:X10003886, factor_key = T)
    ##Cambiar la matriz a la diagonal para KMEAN
    bases$clus <- factor(bases$clus)
    bases_long <- gather(bases, materia_prima, cantidad, X10000196:X10003886, factor_key = T)

    bb<- ggplot(bases_long2, aes(as.factor(x=materia_prima), y = cantidad, group = clus, 
                                 colour = clus,label = bases_long2$materia_prima))+
      geom_point(aes(x=bases_long2$X,group = clus))+
      theme(text = element_text(size = 14), legend.position = 'bottom', # Cambiar tamaño del texto y la posición de la leyenda
            axis.text.x = element_text(angle = 90, hjust = 1, size = 6))+
      labs(title = 'Representación de los grupos PAM',
           subtitle = 'Materias primas en los clusters',
           x = '', y = '% Materia prima')
    ggplotly(bb) 

    b<- ggplot(bases_long, aes(as.factor(x=materia_prima), y = cantidad, group = clus, 
                                 colour = clus,label = bases_long2$materia_prima))+
     
      geom_point(aes(x=bases_long2$X, group = clus))+
      theme(text = element_text(size = 14), legend.position = 'bottom', # Cambiar tamaño del texto y la posición de la leyenda
            axis.text.x = element_text(angle = 90, hjust = 1, size = 6))+
      labs(title = 'Representación de los grupos K-MEANS',
           subtitle = 'Materias primas en los clusters',
           x = '', y = '% Materia prima')
    ggplotly(b) 
  
  
  if(input$puntoskmean)
    ggplotly(b) 
  else ggplotly(bb) 
  
  
})



output$matrizcluster <- DT::renderDataTable({
  bases <- data_final()
  row.names(bases)<-bases$X
  bases$X<-NULL
  acp <- prcomp(bases, scale = T, center = T)
  # dataesca <- data.frame(scale(bases))
  set.seed(123)
  km <- kmeans(bases,centers = input$km,nstart = 25, iter.max = 100)
  set.seed(123)
  kmpan <- pam(bases,k =input$kmedoides, metric = 'euclidean')
  
  
  clustersKM <- data.frame(km$cluster)
  clusterPAM <-  data.frame(kmpan$clustering)
  if(input$plotPAM)
   
    DT::datatable(clustersKM, 
                  filter = 'top',
                  extensions = c('Scroller'),
                  options = list(scrollY=600, 
                                 scrollX=300, 
                                 scroller = TRUE,
                                 deferRender = TRUE,
                                 pageLength = 250))
  
  
  else 
    DT::datatable(clusterPAM , 
                  filter = 'top',
                  extensions = c('Select'),
                  options = list(scrollY=600, 
                                 scrollX=300, 
                                 scroller = TRUE,
                                 deferRender = TRUE,
                                 pageLength = 250)) 

  
})



#_-------------------Calculo Graficadel modelo 1 -----------------------------####################
output$PLOTTER1 <- renderDygraph({
  
 
  load(file = 'rnMOD1.rda')
  entradas <- data.frame(EMH = input$octinoxato, ZnO = input$oxidocinc)
  

    #FPS
    absorbancias <- neuralnet::compute(rnmod1,entradas)
    absorbanciasDF <- data.frame(absorbancias$net.result)
    absorbanciastDF <- data.frame(t(absorbanciasDF))
    colnames(absorbanciastDF)[1] <- 'Abs'
    absorbanciastDF$lo <- c(290:400)
    matriz <- cbind(teoricoFPS, absorbanciastDF$Abs)
    names(matriz)[3] <- "Abs"
    FPS <- round(mean(matriz$num)/mean((matriz$num*(10^(matriz$Abs*-1)))),0)
    #FPUVA
    teoricoUVA <- cbind(teoricoUVA, as.data.frame(absorbanciastDF$Abs[31:111]))
    names(teoricoUVA)[3] <-"Abs"
    C = 1
    FPFUVA <- round(mean(teoricoUVA$numuva)/mean(teoricoUVA$numuva*(10^(teoricoUVA$Abs*-1*C))),0)
    #LOC
    matriz$pun_medio <- Reduce(function(a,b) 1*((a+b)/2), matriz$Abs,accumulate = T)
    matriz$integral <- Reduce(function(a,b) (a+b), matriz$pun_medio,accumulate = T)
    tot_punt_med <- sum(matriz$pun_medio)*0.90
    tot_punt_med
    LOC <-  matriz %>% 
      filter(integral >= tot_punt_med) %>% 
      summarise_each(funs(
        minimo = min(.)
      ), Lo)
    #Generacion del grafico
    absorbanciastDFdygraph<- data.frame(Lon_Onda = absorbanciastDF$lo,
                                        Absorbancia = absorbanciastDF$Abs)

    p <- dygraph(absorbanciastDFdygraph, main ='ESPECTRO ABSORBANCIA MODELO 1',
                 xlab = 'Longitud de onda',
                 ylab = 'Absorbancia') %>% 
      dyOptions(fillGraph = T, fillAlpha = 0.5, 
                drawPoints = T, pointSize = 1, pointShape = 'dot',
                gridLineColor = NA, drawGrid = FALSE) %>% 
      dyCrosshair(direction = "vertical") %>%
      dyHighlight(highlightCircleSize = 8, highlightSeriesBackgroundAlpha = 0.5,
                  highlightSeriesOpts = list(strokeWidth = 3)) %>% 
      dyRangeSelector() %>% 
      dyLegend(show = 'follow') 
    
    LOCGRAP <- if(input$LOC)
      
      ploc <- dygraph(absorbanciastDFdygraph, main ='ESPECTRO ABSORBANCIA MODELO 1',
                      xlab = 'Longitud de onda',
                      ylab = 'Absorbancia') %>% 
      dyOptions(fillGraph = T, fillAlpha = 0.5, 
                drawPoints = T, pointSize = 1, pointShape = 'dot',
                gridLineColor = NA, drawGrid = FALSE) %>% 
      dyCrosshair(direction = "vertical") %>%
      dyHighlight(highlightCircleSize = 8, highlightSeriesBackgroundAlpha = 0.5,
                  highlightSeriesOpts = list(strokeWidth = 3)) %>% 
      dyRangeSelector() %>% 
      dyAnnotation(x = as.character(LOC), text = as.character(LOC), tooltip = 'LOC', width = 30, height=30) %>% 
      dyShading(from = '290', to = LOC, color = 'lightblue') %>% 
      dyEvent(x = as.character(LOC), 'Longitud onda critica', labelLoc = 'top', color = 'red') %>% 
      dyLegend(show = 'follow') 
    else p
    
})
  

#### Grafico de regresión 3D para el modelo 2----------------------------------------------------------------------------------------------------------------------------------------------------
output$regresionMD23D <- renderPlotly({
  
  EMH<- c(0,2,6,9,12,0,0,0,0,3,6,9,12,7,3,12)
  ZnO<- c(0,0,0,0,0,2,6,9,12,2,6,9,12,2,6,5)
  FPS<- c(2,8,26,35,42,4,5,6,7,17,34,48,62,34,20,49)
  grupo <- c('Octinoxato','Octinoxato','Octinoxato','Octinoxato','Octinoxato','Cinc','Cinc','Cinc','Cinc','MEZCLA','MEZCLA','MEZCLA','MEZCLA','MEZCLA','MEZCLA','MEZCLA')

  EMHZnOmod2 <- data.frame(EMH, ZnO, FPS,grupo)
  entradas <- data.frame(EMH = input$octinoxato, ZnO = input$oxidocinc)
  lmEMHZnOMOD2 <- lm(FPS ~ EMH+ZnO+I(EMH^3)+I(EMH*ZnO), data = EMHZnOmod2)
  prediccion2 <- predict(lmEMHZnOMOD2,entradas, level = 0.95, interval = 'prediction' )
  prediccion2[1]
  plotEMHZnO <- plot_ly(x = entradas$EMH, y = entradas$ZnO, z = prediccion2[1], data = EMHZnOmod2, 
                        type = "scatter3d",
                        # mode='markers',
                        text = ~paste('EMH',input$octinoxato,'<br>ZnO',input$oxidocinc,'<br>FPS',
                                      round(prediccion2[1],2)),
                        name = 'Predicción', 
                        # color = ~grupo,
                        colors = c("lightblue")) 
  
  #### Generando las superficie de respuesra
  # graph resolution
  plotresolution <- 0.5
  #Setup Axis
  axis_x <- seq(min(EMHZnOmod2$EMH), max(EMHZnOmod2$EMH), by = plotresolution)
  axis_y <- seq(min(EMHZnOmod2$ZnO), max(EMHZnOmod2$ZnO), by = plotresolution)
  
  #Sample points
  library(reshape2)
  FPS_lm_surface <- expand.grid(EMH = axis_x, ZnO = axis_y, KEEP.OUT.ATTRS = F)
  FPS_lm_surface$FPS <- predict.lm(lmEMHZnOMOD2, newdata = FPS_lm_surface)
  FPS_lm_surface <- acast(FPS_lm_surface, ZnO ~ EMH, value.var = 'FPS') 
  
  plotEMHZnO <- add_trace(p = plotEMHZnO,
                          z = FPS_lm_surface,
                          x = axis_x,
                          y = axis_y,
                          type = 'surface',
                          opacity = 0.8,
                          colorscale = list(c(0, 1), c( "gray", 'lightblue')),
                          name = 'Ajuste') %>% 
    layout(scene = list(xaxis = list(title = 'EMH'),
                        yaxis = list(title = 'ZnO'),
                        zaxis = list(title = 'FPS'))) %>% 
    layout(legend = list(orientation = 'h'))
  
  plotEMHZnO 
  
})



  #_---------------------Calculo grafico del modelo 2 ---------------------------------#
output$PLOTTER2 <- renderDygraph({
  

    load(file = 'rnMOD2.rda')
    entradas <- data.frame(EMH = input$octinoxato, ZnO = input$oxidocinc)
    
    #FPS
    absorbancias <- neuralnet::compute(rn,entradas)
    absorbanciasDF <- data.frame(absorbancias$net.result)
    absorbanciastDF <- data.frame(t(absorbanciasDF))
    colnames(absorbanciastDF)[1] <- 'Abs'
    absorbanciastDF$lo <- c(290:400)
    matriz1 <- cbind(teoricoFPS, absorbanciastDF$Abs)
    names(matriz1)[3] <- "Abs"
    FPS <- round(mean(matriz1$num)/mean((matriz1$num*(10^(matriz1$Abs*-1)))),0)
    #FPSUVA
    teoricoUVA <- cbind(teoricoUVA, as.data.frame(absorbanciastDF$Abs[31:111]))
    names(teoricoUVA)[3] <-"Abs"
    C = 1
    FPFUVA <- round(mean(teoricoUVA$numuva)/mean(teoricoUVA$numuva*(10^(teoricoUVA$Abs*-1*C))),0)
    #LOC
    matriz1$pun_medio <- Reduce(function(a,b) 1*((a+b)/2), matriz1$Abs,accumulate = T)
    matriz1$integral <- Reduce(function(a,b) (a+b), matriz1$pun_medio,accumulate = T)
    tot_punt_med <- sum(matriz1$pun_medio)*0.90
    
    LOC <-  matriz1 %>%
      filter(integral >= tot_punt_med) %>%
      summarise_each(funs(
        minimo = min(.)
      ), Lo)
    #Generacion del grafico
    
    absorbanciastDFdygraph<- data.frame(Lon_Onda = absorbanciastDF$lo,
                                        Absorbancia = absorbanciastDF$Abs)
    p2 <- dygraph(absorbanciastDFdygraph, main ='ESPECTRO ABSORBANCIA MODELO 2',
                  xlab = 'Longitud de onda',
                  ylab = 'Absorbancia') %>%
      dyOptions(fillGraph = T, fillAlpha = 0.5,colors = 'dodgerblue',
                drawPoints = T, pointSize = 1, pointShape = 'dot',
                gridLineColor = NA, drawGrid = FALSE) %>%
      dyCrosshair(direction = "vertical") %>%
      dyHighlight(highlightCircleSize = 8, highlightSeriesBackgroundAlpha = 0.5,
                  highlightSeriesOpts = list(strokeWidth = 3)) %>%
      dyRangeSelector() %>%
      dyLegend(show = 'follow')
    
    LOCGRAP1 <- if(input$LOC1)
      
      ploc2 <- dygraph(absorbanciastDFdygraph, main ='ESPECTRO ABSORBANCIA MODELO 2',
                       xlab = 'Longitud de onda',
                       ylab = 'Absorbancia') %>%
      dyOptions(fillGraph = T, fillAlpha = 0.5, colors = 'dodgerblue',
                drawPoints = T, pointSize = 1, pointShape = 'dot',
                gridLineColor = NA, drawGrid = FALSE) %>%
      dyCrosshair(direction = "vertical") %>%
      dyHighlight(highlightCircleSize = 8, highlightSeriesBackgroundAlpha = 0.5,
                  highlightSeriesOpts = list(strokeWidth = 3)) %>%
      dyRangeSelector() %>%
      dyAnnotation(x = as.character(LOC), text = as.character(LOC), tooltip = 'LOC', width = 30, height=30) %>%
      dyShading(from = '290', to = LOC, color = 'skyblue') %>%
      dyEvent(x = as.character(LOC), 'Longitud onda critica', labelLoc = 'top', color = 'red') %>%
      dyLegend(show = 'follow')
    else p2
      

  
})

## Mostrar FPS UVB para MODELO 1----------------------------------------------
output$FPSUVB <- renderValueBox({
  EMH<- c(0,2.1,5.8,9.2,11.7,0,0,0,0,3,5.9,8.9,12,7.5,2.9,12,5.95,6.47,11.7)
  ZnO<- c(0,0,0,0,0,2.32,5.76,8.67,11.99,2.37,5.56,8.86,12.07,2.42,6.45,5.22,12.48,5.8,5.22)
  FPS<- c(	2,8,23.7,32.3,39.8,3,4.2,5.1,6,11.9,25.5,39.3,50.0,29,14.5,45.2,29.6,28.0,43.0)
  EMHZnOmod1 <- data.frame(EMH, ZnO, FPS)
  
  entradas <- data.frame(EMH = input$octinoxato, ZnO = input$oxidocinc)
  lmEMHZnOMOD1 <- lm(FPS ~ EMH+ZnO+I(EMH^3)+I(EMH*ZnO), data = EMHZnOmod1)

  prediccion <- predict(lmEMHZnOMOD1,entradas, level = 0.95, interval = 'prediction' )
  prediccion[1]

  #FPS UVB

  valueBox(round(prediccion[1],0), width = 3,
           subtitle = '',
           # icon = icon('angle-double-right'), 
           color = 'light-blue')
})
  ## Mostrar menos intervalo de confianza UVB para MODELO 1---------------------------------------------- 
output$LESSIC <- renderValueBox({
  EMH<- c(0,2.1,5.8,9.2,11.7,0,0,0,0,3,5.9,8.9,12,7.5,2.9,12,5.95,6.47,11.7)
  ZnO<- c(0,0,0,0,0,2.32,5.76,8.67,11.99,2.37,5.56,8.86,12.07,2.42,6.45,5.22,12.48,5.8,5.22)
  FPS<- c(	2,8,23.7,32.3,39.8,3,4.2,5.1,6,11.9,25.5,39.3,50.0,29,14.5,45.2,29.6,28.0,43.0)
  EMHZnOmod1 <- data.frame(EMH, ZnO, FPS)
  
  entradas <- data.frame(EMH = input$octinoxato, ZnO = input$oxidocinc)
  lmEMHZnOMOD1 <- lm(FPS ~ EMH+ZnO+I(EMH^3)+I(EMH*ZnO), data = EMHZnOmod1)
  
  prediccion <- predict(lmEMHZnOMOD1,entradas, level = 0.95, interval = 'prediction' )
  prediccion[2]
  
  #FPS UVB
  
  valueBox(round(prediccion[2],0), width = 3, 
           subtitle = '',
           # icon = icon('angle-double-right'), 
           color = 'light-blue')
})
  
## Mostrar mas intervalo de confianza UVB para MODELO 1---------------------------------------------- 
output$PLUSIC <- renderValueBox({
  EMH<- c(0,2.1,5.8,9.2,11.7,0,0,0,0,3,5.9,8.9,12,7.5,2.9,12,5.95,6.47,11.7)
  ZnO<- c(0,0,0,0,0,2.32,5.76,8.67,11.99,2.37,5.56,8.86,12.07,2.42,6.45,5.22,12.48,5.8,5.22)
  FPS<- c(	2,8,23.7,32.3,39.8,3,4.2,5.1,6,11.9,25.5,39.3,50.0,29,14.5,45.2,29.6,28.0,43.0)
  EMHZnOmod1 <- data.frame(EMH, ZnO, FPS)
  
  entradas <- data.frame(EMH = input$octinoxato, ZnO = input$oxidocinc)
  lmEMHZnOMOD1 <- lm(FPS ~ EMH+ZnO+I(EMH^3)+I(EMH*ZnO), data = EMHZnOmod1)
  
  prediccion <- predict(lmEMHZnOMOD1,entradas, level = 0.95, interval = 'prediction' )
  prediccion[3]
  
  #FPS UVB
  
  valueBox(round(prediccion[3],0), width = 3, 
           subtitle = '',
           # icon = icon('angle-double-right'), 
           color = 'light-blue')
})
  

## Mostrar FPSUVA para MODELO 1
output$FPSUVA <-  renderValueBox({
  load(file = 'rnMOD1.rda')
  entradas <- data.frame(EMH = input$octinoxato, ZnO = input$oxidocinc)
  #FPS
  absorbancias <- neuralnet::compute(rnmod1,entradas)
  absorbanciasDF <- data.frame(absorbancias$net.result)
  absorbanciastDF <- data.frame(t(absorbanciasDF))
  colnames(absorbanciastDF)[1] <- 'Abs'
  absorbanciastDF$lo <- c(290:400)
  matriz <- cbind(teoricoFPS, absorbanciastDF$Abs)
  names(matriz)[3] <- "Abs"
  #FPUVA
  teoricoUVA <- cbind(teoricoUVA, as.data.frame(absorbanciastDF$Abs[31:111]))
  names(teoricoUVA)[3] <-"Abs"
  C = 1
  FPFUVA <- round(mean(teoricoUVA$numuva)/mean(teoricoUVA$numuva*(10^(teoricoUVA$Abs*-1*C))),0) 
  valueBox(FPFUVA, width = 3,
           subtitle = '',
           # icon = icon('angle-double-up'), 
           color = 'light-blue')
})
## Mistrar LOC para MODELO 1
output$longcrit <-  renderValueBox({
load(file = 'rnMOD1.rda')
entradas <- data.frame(EMH = input$octinoxato, ZnO = input$oxidocinc)
absorbancias <- neuralnet::compute(rnmod1,entradas)
absorbanciasDF <- data.frame(absorbancias$net.result)
absorbanciastDF <- data.frame(t(absorbanciasDF))
colnames(absorbanciastDF)[1] <- 'Abs'
absorbanciastDF$lo <- c(290:400)
matriz <- cbind(teoricoFPS, absorbanciastDF$Abs)
names(matriz)[3] <- "Abs"
teoricoUVA <- cbind(teoricoUVA, as.data.frame(absorbanciastDF$Abs[31:111]))
names(teoricoUVA)[3] <-"Abs"
C = 1
matriz$pun_medio <- Reduce(function(a,b) 1*((a+b)/2), matriz$Abs,accumulate = T)
matriz$integral <- Reduce(function(a,b) (a+b), matriz$pun_medio,accumulate = T)
tot_punt_med <- sum(matriz$pun_medio)*0.90
tot_punt_med
LOC <-  matriz %>% 
  filter(integral >= tot_punt_med) %>% 
  summarise_each(funs(
    minimo = min(.)
  ), Lo)
valueBox(LOC, width = 3,
         subtitle = '',
         # icon = icon('angle-double-left'), 
         color = 'light-blue')
})

#### Mostrar tabla del calculo de los costos segun el simulador

output$basessimulamod1 <- renderTable({
  COSTxKgEMH <- 41242
  COSTxKgZnO <- 88845

  entradassimulador <- data.frame(EMH = input$octinoxato, ZnO = input$oxidocinc)
  E<- (entradassimulador[1,][1])/100*COSTxKgEMH
  Zn <-(entradassimulador[1,][2])/100*COSTxKgZnO 
  COSTOSIMULADOR <- data.frame(E,Zn)
  COSTOSIMULADOR<-data.frame(t(COSTOSIMULADOR))
  COSTOSIMULADOR <- COSTOSIMULADOR %>% 
    summarise_each(funs(
      TOTAL = sum(.,na.rm = T)
    ))
  TABLACOSTOS <- data.frame(Costo_EMH = E, 
                            CostoZnO_ZnO = Zn, 
                            Costo_total_xKg = COSTOSIMULADOR)

  (TABLACOSTOS)

})


output$basesmod1 <- DT::renderDataTable({
  
  
  # nombres <- c('EMH', 'ZnO', 'In-vivo', 'Costo_EMH', 'Costo_ZnO', 'Costo_total')
  ESK_PRO_INSTAN_FPS25 <- c(EMH =6.5,ZnO = 3.6,In_vivo = 29, Costo_EMH= 2681,Costo_ZnO = 3198,Costo_total =5879)
  BB_CRAM_FPS30 <- c(EMH =7,ZnO =9,In_vivo =32,Costo_EMH=2887,Costo_ZnO =7996,Costo_total =10883)
  LB_BASE_GOTERO_FPS25 <- c(EMH =7,ZnO =7.2,In_vivo =30,Costo_EMH=2887,Costo_ZnO =6397,Costo_total =9284)
  LB_CLARITE_ANTIMANC_FPS30 <- c(EMH =7.5,ZnO =7.84,In_vivo =36,Costo_EMH=3093,Costo_ZnO =6965,Costo_total =10059)
  TAC_BB_CREAM_M6913O002_FPS30 <- c(EMH =7.5,ZnO =5.4,In_vivo =32.9, Costo_EMH=3093,Costo_ZnO =4798,Costo_total = 7891)
  MOD1bases <- data.frame(rbind(ESK_PRO_INSTAN_FPS25,BB_CRAM_FPS30 ,
                                LB_BASE_GOTERO_FPS25,LB_CLARITE_ANTIMANC_FPS30,
                                TAC_BB_CREAM_M6913O002_FPS30))
  DT::datatable(MOD1bases,
                # filter = 'top',
                # extensions = c('Scroller'),
                options = list(scrollY=250,
                               scrollX=300,
                               scroller = T,
                               deferRender = TRUE,
                               pageLength = 250))
})

#### Grafico de regresión 3D para el modelo 1----------------------------------------------------------------------------------------------------------------------------------------------------
output$regresionMD13D <- renderPlotly({
 
  EMH<- c(0.0,2.1,5.8,9.2,11.7,0.0,0.0,0.0,0.0,3.0,5.9,8.9,12.0,7.5,2.9,12.0,6.0,6.5,11.7)
  ZnO<- c(0.0,0.0,0.0,0.0,0.0,2.3,5.8,8.7,12.0,2.4,5.6,8.9,12.1,2.4,6.5,5.2,12.5,5.8,5.2)
  FPS<- c(2,8,23.7,32.3,39.8,3,4.2,5.1,6,11.9,25.5,39.3,50.0,29,14.5,45.2,29.6,28.0,43.0)
  grupo <- c('Octinoxato','Octinoxato','Octinoxato','Octinoxato','Octinoxato','Cinc','Cinc','Cinc','Cinc','MEZCLA','MEZCLA','MEZCLA','MEZCLA','MEZCLA','MEZCLA','MEZCLA','MEZCLA','MEZCLA','MEZCLA')
  


EMHZnOmod1 <- data.frame(EMH, ZnO, FPS,grupo)
entradas <- data.frame(EMH = input$octinoxato, ZnO = input$oxidocinc)
lmEMHZnOMOD1 <- lm(FPS ~ EMH+ZnO+I(EMH^3)+I(EMH*ZnO), data = EMHZnOmod1)

prediccion <- predict(lmEMHZnOMOD1,entradas, level = 0.95, interval = 'prediction' )
prediccion[1]
plotEMHZnO <- plot_ly(x = entradas$EMH, y = entradas$ZnO, z = prediccion[1], data = EMHZnOmod1, 
                      type = "scatter3d",
                      # mode='markers',
                      text = ~paste('EMH',input$octinoxato,'<br>ZnO',input$oxidocinc,'<br>FPS',
                                      round(prediccion[1],2)),
                      name = 'Predicción',
                      # color = ~grupo,
                      colors = c("lightblue"))
#### Generando las superficie de respuesra
# graph resolution
plotresolution <- 0.5
#Setup Axis
axis_x <- seq(min(EMHZnOmod1$EMH), max(EMHZnOmod1$EMH), by = plotresolution)
axis_y <- seq(min(EMHZnOmod1$ZnO), max(EMHZnOmod1$ZnO), by = plotresolution)

#Sample points
library(reshape2)
FPS_lm_surface <- expand.grid(EMH = axis_x, ZnO = axis_y, KEEP.OUT.ATTRS = F)
FPS_lm_surface$FPS <- predict.lm(lmEMHZnOMOD1, newdata = FPS_lm_surface)
FPS_lm_surface <- acast(FPS_lm_surface, ZnO ~ EMH, value.var = 'FPS') 

plotEMHZnO <- add_trace(p = plotEMHZnO,
                        z = FPS_lm_surface,
                        x = axis_x,
                        y = axis_y,
                        type = 'surface',
                        opacity = 0.8,
                        colorscale = list(c(0, 1), c( "gray", 'lightblue')),
                        name = 'Ajuste') %>% 
  layout(scene = list(xaxis = list(title = 'EMH'),
                      yaxis = list(title = 'ZnO'),
                      zaxis = list(title = 'FPS'))) %>%
  layout(legend = list(orientation = 'h'))
  
plotEMHZnO 
  
})




## -----------------Mostrar FPS UVB para MODELO 2

output$FPSUVB2 <- renderValueBox({
 EMH<- c(0.0,2.1,5.8,9.2,11.7,0.0,0.0,0.0,0.0,3.0,5.9,8.9,12.0,7.5,2.9,11.7)
 ZnO<- c(0.0,0.0,0.0,0.0,0.0,2.3,5.8,8.7,12.0,2.4,5.6,8.9,12.5,2.4,6.5,5.2)
 FPS<- c(2.0,8.4,26.4,34.7,42.1,3.7,5.1,5.9,7.0,17.2,34.0,48.2,62.4,34.5,19.8,49.3)
 EMHZnOmod2 <- data.frame(EMH, ZnO, FPS)
 
 entradas <- data.frame(EMH = input$octinoxato, ZnO = input$oxidocinc)
 lmEMHZnOMOD1 <- lm(FPS ~ EMH+ZnO+I(EMH^3)+I(EMH*ZnO), data = EMHZnOmod2)
 
 prediccion <- predict(lmEMHZnOMOD1,entradas, level = 0.95, interval = 'prediction' )
 prediccion[1]

  #FPS

  valueBox(round(prediccion[1],0), width = 3,
           subtitle = '',
           # icon = icon('arrow-alt-circle-right'), 
           color = 'light-blue')
})
## Mostrar menos intervalo de confianza UVB para MODELO 2---------------------------------------------- 
output$LESSIC2 <- renderValueBox({
  EMH<- c(0.0,2.1,5.8,9.2,11.7,0.0,0.0,0.0,0.0,3.0,5.9,8.9,12.0,7.5,2.9,11.7)
  ZnO<- c(0.0,0.0,0.0,0.0,0.0,2.3,5.8,8.7,12.0,2.4,5.6,8.9,12.5,2.4,6.5,5.2)
  FPS<- c(2.0,8.4,26.4,34.7,42.1,3.7,5.1,5.9,7.0,17.2,34.0,48.2,62.4,34.5,19.8,49.3)
  EMHZnOmod2 <- data.frame(EMH, ZnO, FPS)
  
  entradas <- data.frame(EMH = input$octinoxato, ZnO = input$oxidocinc)
  lmEMHZnOMOD1 <- lm(FPS ~ EMH+ZnO+I(EMH^3)+I(EMH*ZnO), data = EMHZnOmod2)
  
  prediccion <- predict(lmEMHZnOMOD1,entradas, level = 0.95, interval = 'prediction' )
  prediccion[2]
  
  #FPS
  
  valueBox(round(prediccion[2],0), width = 3,
           subtitle = '',
           # icon = icon('arrow-alt-circle-right'), 
           color = 'light-blue')
})
## Mostrar MAS intervalo de confianza UVB para MODELO 2---------------------------------------------- 
output$PLUSIC2 <- renderValueBox({
  EMH<- c(0.0,2.1,5.8,9.2,11.7,0.0,0.0,0.0,0.0,3.0,5.9,8.9,12.0,7.5,2.9,11.7)
  ZnO<- c(0.0,0.0,0.0,0.0,0.0,2.3,5.8,8.7,12.0,2.4,5.6,8.9,12.5,2.4,6.5,5.2)
  FPS<- c(2.0,8.4,26.4,34.7,42.1,3.7,5.1,5.9,7.0,17.2,34.0,48.2,62.4,34.5,19.8,49.3)
  EMHZnOmod2 <- data.frame(EMH, ZnO, FPS)
  
  entradas <- data.frame(EMH = input$octinoxato, ZnO = input$oxidocinc)
  lmEMHZnOMOD1 <- lm(FPS ~ EMH+ZnO+I(EMH^3)+I(EMH*ZnO), data = EMHZnOmod2)
  
  prediccion <- predict(lmEMHZnOMOD1,entradas, level = 0.95, interval = 'prediction' )
  prediccion[3]
  
  #FPS
  
  valueBox(round(prediccion[3],0), width = 3,
           subtitle = '',
           # icon = icon('arrow-alt-circle-right'), 
           color = 'light-blue')
})

## Mostrar FPSUVA2 para MODELO 2

output$FPSUVA2 <-  renderValueBox({
  load(file = 'rnMOD2.rda')
  entradas <- data.frame(EMH = input$octinoxato, ZnO = input$oxidocinc)
  absorbancias <- neuralnet::compute(rn,entradas)
  absorbanciasDF <- data.frame(absorbancias$net.result)
  absorbanciastDF <- data.frame(t(absorbanciasDF))
  colnames(absorbanciastDF)[1] <- 'Abs'
  absorbanciastDF$lo <- c(290:400)
  matriz1 <- cbind(teoricoFPS, absorbanciastDF$Abs)
  names(matriz1)[3] <- "Abs"
  #FPSUVA
  teoricoUVA <- cbind(teoricoUVA, as.data.frame(absorbanciastDF$Abs[31:111]))
  names(teoricoUVA)[3] <-"Abs"
  C = 1
  FPFUVA <- round(mean(teoricoUVA$numuva)/mean(teoricoUVA$numuva*(10^(teoricoUVA$Abs*-1*C))),0)
  
  valueBox(FPFUVA, width = 3,
           subtitle = '',
           # icon = icon('arrow-alt-circle-up'), 
           color = 'light-blue')
  
})


## Mistrar LOC para MODELO 2
output$longcrit2 <-  renderValueBox({
  load(file = 'rnMOD2.rda')
  entradas <- data.frame(EMH = input$octinoxato, ZnO = input$oxidocinc)
  absorbancias <- neuralnet::compute(rn,entradas)
  absorbanciasDF <- data.frame(absorbancias$net.result)
  absorbanciastDF <- data.frame(t(absorbanciasDF))
  colnames(absorbanciastDF)[1] <- 'Abs'
  absorbanciastDF$lo <- c(290:400)
  matriz1 <- cbind(teoricoFPS, absorbanciastDF$Abs)
  names(matriz1)[3] <- "Abs"
  teoricoUVA <- cbind(teoricoUVA, as.data.frame(absorbanciastDF$Abs[31:111]))
  names(teoricoUVA)[3] <-"Abs"
  C = 1
  #LOC
  matriz1$pun_medio <- Reduce(function(a,b) 1*((a+b)/2), matriz1$Abs,accumulate = T)
  matriz1$integral <- Reduce(function(a,b) (a+b), matriz1$pun_medio,accumulate = T)
  tot_punt_med <- sum(matriz1$pun_medio)*0.90
  LOC <-  matriz1 %>%
    filter(integral >= tot_punt_med) %>%
    summarise_each(funs(
      minimo = min(.)
    ), Lo)
  valueBox(LOC, width = 3,
           subtitle = '',
           # icon = icon('arrow-alt-circle-left'), 
           color = 'light-blue')

  
})

#### Mostrar tabla del calculo de los costos segun el simulador

output$basessimulamod2 <- renderTable({
  COSTxKgEMH <- 41242
  COSTxKgZnO <- 88845
  
  entradassimulador <- data.frame(EMH = input$octinoxato, ZnO = input$oxidocinc)
  E<- (entradassimulador[1,][1])/100*COSTxKgEMH
  Zn <-(entradassimulador[1,][2])/100*COSTxKgZnO 
  COSTOSIMULADOR <- data.frame(E,Zn)
  COSTOSIMULADOR<-data.frame(t(COSTOSIMULADOR))
  COSTOSIMULADOR <- COSTOSIMULADOR %>% 
    summarise_each(funs(
      TOTAL = sum(.,na.rm = T)
    ))
  TABLACOSTOS <- data.frame(Costo_EMH = E, 
                            CostoZnO_ZnO = Zn, 
                            Costo_total_xKg = COSTOSIMULADOR)
  
  (TABLACOSTOS)
  
})


output$basesmod2 <- DT::renderDataTable({
  
  
  # nombres <- c('EMH', 'ZnO', 'In-vivo', 'Costo_EMH', 'Costo_ZnO', 'Costo_total')
  CY_STUDIO_LOOK_FPS20 <-  c(EMH = 4, ZnO = 3, In_vivo = 23, Costo_EMH= 1650,Costo_ZnO = 1963, Costo_total = 3613)
  LB_CLARIT_ACLAR_MANC_FPS30 <- c(EMH = 7,ZnO = 6.4,In_vivo= 38,Costo_EMH =2887,Costo_ZnO=5686,Costo_total = 8573)
  CY_GO_NUDE_LIGERA_FPS20 <- c(EMH = 5.96,ZnO = 6.03,In_vivo=34,Costo_EMH=2458,Costo_Zno=5357,Costo_total =7815)
  ESK_PRO_NATURAL_MATE_FPS20 <- c(EMH=5,ZnO = 4.2,In_vivo=29,Costo_EMH=2062,Costo_ZnO=3731,Costo_total= 5794)
  MOD2bases <- data.frame(rbind(CY_STUDIO_LOOK_FPS20,LB_CLARIT_ACLAR_MANC_FPS30,
                                CY_GO_NUDE_LIGERA_FPS20,ESK_PRO_NATURAL_MATE_FPS20))
  

  DT::datatable(MOD2bases,
                # filter = 'top',
                # extensions = c('Scroller'),
                options = list(scrollY=250,
                               scrollX=300,
                               scroller = F,
                               deferRender = TRUE,
                               pageLength = 6))
})





}




shinyApp(ui, server)


# Bibliografia 
#https://rpubs.com/kfhidalgoh/300948
#https://rpubs.com/Joaquin_AR/310338