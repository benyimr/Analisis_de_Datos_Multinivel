#############################################################################################################################
################################################      TABLERO DE CONTROL       ##############################################
################################################ VISUALIZADOR DE DATOS SOL3051 ##############################################         
#############################################################################################################################

#AUTOR:  Benjamín Muñoz
#UPDATE: 30/03/2019


#############################################################################################################################
################################################ PASO 0:  ESPACIO DE TRABAJO ################################################
#############################################################################################################################

#Limpieza del espacio de trabajo y opciones
rm(list=ls())
options(max.print = 2000, scipen = 50)

#Paquetes utilizados
library(car)
library(lme4)
library(lmtest)
library(ggthemes)
library(shiny)
library(shinydashboard)
library(sjlabelled)
library(sjPlot)
library(texreg)
library(tidyverse)



#############################################################################################################################
#################################################### PASO 1:PRELIMINARES ####################################################
#############################################################################################################################

#Base de datos



#############################################################################################################################
################################################ PASO 2: INTERFAZ DE USUARIO ################################################
#############################################################################################################################

interfaz_usuario <- dashboardPage(
  
##FORMATO GENERAL DEL PANEL DE CONTROL
  skin = "red",
  
  title = "Visualizador",
  
  ##TITULO DEL PANEL DE CONTROL
  header = dashboardHeader(title = "Análisis Multinivel", titleWidth = 230, disable = FALSE), #Cierre dashboardHeader

    
  ##BARRA LATERAL DEL PANEL DE CONTROL
  sidebar = dashboardSidebar(disable = FALSE, width = 230, collapsed = FALSE,
                             
                             ###MENU BARRA LATERAL
                             sidebarMenu(id = "Barra ELSOC",
                                         
                                         ####CORRELACIÓN INTRA-CLASE
                                         
                                         menuItem(text = "Correlación Intra-Clase", 
                                                  icon = icon("calculator", lib = "font-awesome", class = "fa-lg"), tabName = "intra_class_corr",
                                                  selected = TRUE), #Cierre menuItem Correlación Intra-Clase
                                         menuItem(text = "Modelo Intercepto Aleatorio", 
                                                  icon = icon("line-chart", lib = "font-awesome", class = "fa-lg"), tabName = "random_intercept",
                                                  selected = FALSE) #Cierre menuItem  Modelo Intercepto Aleatorio
                                         
                               
                             )  #Cierre sidebarMenu
                             ), #Cierre dashboardSidebar
  
  ##AREA CENTRAL DEL TABLERO DE CONTROL
  body = dashboardBody(tabItems(
    
    ###ANÁLISIS DE CORRELACIÓN INTRA-CLASE
    tabItem(tabName = "intra_class_corr",
            h1("Correlación Intra-Clase", align ="left"),   #Título de la sección (tabItem)
            p(h4("En esta sección examinaremos el efecto de la jerarquía y el nivel de correlación entre los grupos. 
                  Para esto, analizaremos la correlación intra-clase por medio de datos simulados. En este ejemplo  
                  podrás manipular el número de observaciones, grupos y los niveles de correlación entre las variables.", 
                 align = "justify")), #Cierre de párrafo
            br(""),
            fluidRow(
              box(width = 9, plotOutput(outputId = "grafico_01", width = "100%", height = 480)),
              box(width = 3, title = "Selecciona", solidHeader = TRUE, status = "warning",
                  sliderInput(inputId = "n_obs", label = "Número de Individuos:",
                              min = 2, max = 1000, value = 100, step = 1, sep = "."),
                  sliderInput(inputId = "n_groups", label = "Número de Grupos:",
                              min = 2, max = 300, value = 50, step = 1, sep = "."),
                  numericInput(inputId = "mu", label = "Gran Media", value = 10, 
                               min = -20, max = 20, step = 0.5),
                  numericInput(inputId = "sigma", label = "Desviación Estándar entre Individuos", value = 1, 
                               min = 0, max = 100, step = 0.5),
                  numericInput(inputId = "sigma_s", label = "Desviación Estándar entre Grupos", value = 1, 
                               min = 0, max = 100, step = 0.5)),
              box(width = 9, plotOutput(outputId = "grafico_02", width = "100%", height = 400)),
              box(width = 9, plotOutput(outputId = "grafico_03", width = "100%", height = 400))
            ),#Cierre de fluidRow de Análisis Univariado:Gráficos
            fluidRow(
              box(width = 3, htmlOutput(outputId = "tabla_01")),
              box(width = 6, htmlOutput(outputId = "tabla_02"))
              )#Cierre de fluidRow de Análisis Correlación Intra-Clase
    ),#Cierre de tabItem de Análisis Correlación Intra-Clase
    
    ###MODELO DE INTERCEPTOS ALEATORIOS
    tabItem(tabName = "random_intercept",
            h1("Modelo de Interceptos Aleatorios", align ="left"),   #Título de la sección (tabItem)
            p(h4("En esta sección examinaremos la relevancia del modelamiento multinivel en el caso de diferencias 
                 grupales en los niveles base por medio del modelo de interceptos aleatorios.", 
                 align = "justify")), #Cierre de párrafo
            br(""),
            fluidRow(
              box(width = 9, htmlOutput(outputId = "tabla_03", width = "100%", height = 480)),
              box(width = 3, title = "Selecciona", solidHeader = TRUE, status = "warning",
                  sliderInput(inputId = "n_obs1", label = "Número de Individuos:",
                              min = 2, max = 1000, value = 100, step = 1, sep = "."),
                  sliderInput(inputId = "n_groups1", label = "Número de Grupos:",
                              min = 2, max = 300, value = 4, step = 1, sep = "."),
                  numericInput(inputId = "V.ind0", label = "Varianza de Interceptos entre Grupos", value = 50, 
                               min = 0, max = 500, step = 0.5),
                  numericInput(inputId = "V.inde", label = "Varianza de Pendientes entre Grupos", value = 0.01, 
                               min = 0, max = 500, step = 0.5),
                  numericInput(inputId = "V.err0", label = "Varianza Residual", value = 100, 
                               min = 0, max = 500, step = 0.5),
                  numericInput(inputId = "COVind0.ind1e", label = "Covarianza Interceptos y Pendientes", value = 0, 
                               min = 0, max = 1, step = 0.01)),
              box(width = 9, plotOutput(outputId = "grafico_04", width = "100%", height = 400)),
              box(width = 9, plotOutput(outputId = "grafico_05", width = "100%", height = 400))
            ),#Cierre de fluidRow de Análisis Univariado:Gráficos
            fluidRow(
              box(width = 9, plotOutput(outputId = "grafico_06", width = "100%", height = 400)),
              box(width = 9, plotOutput(outputId = "grafico_07", width = "100%", height = 400))
            )#Cierre de fluidRow de Análisis Correlación Intra-Clase
    )#Cierre de tabItem de Análisis Correlación Intra-Clase
    
    
    
    ) #Cierre tabItems
  ) #Cierre dashboardBody
)#Cierre de dashboardPage


#############################################################################################################################
###################################################### PASO 3:SERVIDOR ######################################################
#############################################################################################################################

#Desarrollo de Servidor (función) de Análisis Multinivel
servidor <- function(input, output){

###################################################### CORRELACIÓN INTRA-CLASE ######################################################
  
  #FUNCIÓN PARA SIMULACIÓN DE CORRELACIÓN INTRA-CLASE
  random_model <- function(n.groups = 5, n.obs = 4, mu = 10, sigma_s = 2, sigma = 1){
    
    group_eff = rep( rnorm(n.groups, 0, sigma_s), each = n.obs)
    group = rep(seq(1:n.groups), each = n.obs)
    obs_eff = rnorm(n.groups*n.obs, 0, sigma)
    resp = mu + group_eff + obs_eff
    dat = data.frame(group, resp)
    return(dat)
  }
  
  #DATOS SIMULADOS COMO OBJETO REACTIVO
  sim_data <- reactive({
    set.seed(12345)
    random_model(n.groups = input$n_groups, n.obs = input$n_obs, mu = input$mu, 
                 sigma_s = input$sigma_s, sigma = input$sigma)
  })
  
  #MODELO NULO (RESULTADO DE SIMULACIÓN)
  alfa <- reactive({
    lmer(resp ~ 1 + (1|group), data = sim_data())
  })
  alfa_lineal <- reactive({
    lm(resp ~ 1, data = sim_data())
  })
  alfa_interceptos <- reactive({
    as.data.frame(coef(alfa())[1])
  })
  
  #Output N1: Gráfico de Efectos Aleatorios
  output$grafico_01 <- renderPlot({
    plot_model(model = alfa(), type = "re", grid = FALSE, sort.est = "sort.all",
               title = "Efectos Aleatorios (asociados a Interceptos Grupales)") + theme_bw()
  })
  #Output N2: Gráfico de Normalidad de Efectos Aleatorios  
  output$grafico_02 <- renderPlot({
    qqPlot(
      as.data.frame(ranef(alfa()))$condval, 
      ylab ="Efectos Aleatorios", xlab = "Cuantiles Normales",
      main = "Distribución Normal de Efectos Aleatorios (Predichos por el Modelo)")    
  })
  #Output N3: Gráfico de Distribución de Interceptos (Grupos)
  output$grafico_03 <- renderPlot({
    ggplot(data = alfa_interceptos()) + geom_boxplot(mapping = aes(x = "" , y = X.Intercept.)) +
      labs(title = "Distribución de Variable de Resultado por Grupo",
           subtitle = "Promedios Grupales (Interceptos). Basado en Modelo Nulo",
           x = "", y ="Variable de Resultado") + theme_bw()
  })
  #Output N4: Tabla de Modelo Nulo
  output$tabla_01 <- renderText({
    htmlreg(
      alfa(), custom.model.names = "Modelo Nulo", custom.coef.names = "Intercepto",
      custom.gof.names = c("AIC","BIC", "Log-Verosimilitud","Nº Observaciones","Nº Grupos",
                           "Varianza Grupos","Varianza Residual"), caption = ""
    )
  })
  #Output N5: Texto de Análisis de Correlación Intra-Clase
  output$tabla_02 <- renderText({
    paste("La <B>Varianza entre Grupos</B> (between-group variance) es     ",
          round(as.data.frame(VarCorr(alfa()))$vcov[1],3), 
          "mientras que la <B> Varianza Residual </B> (within-group variance) es     ",
          round(as.data.frame(VarCorr(alfa()))$vcov[2],3), 
          ". Por lo tanto, la <B> Varianza Total </B> es     ",
          round(as.data.frame(VarCorr(alfa()))$vcov[1],3)+round(as.data.frame(VarCorr(alfa()))$vcov[2],3),
          "<br>","<hr>",
          "Por lo tanto, la <B> Correlación Intra-Clase </B> es     ",
    round(sjstats::icc(
      alfa()
                 ),3),".",
    "<br>","<hr>",
    "Otro modo de evaluar la pertinencia del modelamiento multinivel es por medio de un test de razones de verosimilitud.
    Se contrasta la ganancia que representa considerar en el modelo la estructura jerárquica de los datos con respecto a
    un modelo especificado de forma idéntica que solo considera la varianza en un único nivel. La <B> Hipótesis Nula </B>
    es que la perturbación aleatoria alrededor de la constante es cero. El estadístico del test de Razón de Verosimilitud es     ",
    round(lrtest(alfa(), alfa_lineal())$Chisq[2],3),
    "y el valor-p asociado es     ", lrtest(alfa(), alfa_lineal())$`Pr(>Chisq)`[2],"."
    )
  })
  

######################################################  MODELO INTERCEPTO ALEATORIO  ######################################################

  #FUNCIÓN PARA SIMULACIÓN DE INTERCEPTOS ALEATORIOS
  re_intercepts <- function(n_groups = 5, n_obs = 4, var_int = 1, var_slop = 1, var_res = 1, cov = 0.01){
    form<-as.formula(c("~ind+(ind|Group)"))
    set.seed(25)
    simdat <-data.frame(Group=factor(rep(1:n_groups,each=n_obs)),ind=rnorm(n_groups*n_obs,100,10))
    
    beta<-c(7,2)
    names(beta)<-c("(Intercept)","ind")
    
    COVind0.ind1e <- cov*(sqrt(var_int*var_slop)) #covariance between reaction norm slopes and intercepts
    
    vcov<-matrix(c(var_int,COVind0.ind1e,COVind0.ind1e,var_slop), 2, 2)
    
    theta<-c((chol(vcov)/sqrt(var_res))[1,1],
             (chol(vcov)/sqrt(var_res))[1,2],
             (chol(vcov)/sqrt(var_res))[2,2])
    
    names(theta)<-c("Group.(Intercept)","Group.ind.(Intercept)","Group.ind")
    
    set.seed(25)
    response<-simulate(form,newdata=simdat,family=gaussian,
                       newparams=list(theta = theta,beta = beta, sigma = sqrt(var_res)))
    simdat$resp<-as.vector(response[,1])
    return(simdat)
  }

  #DATOS SIMULADOS COMO OBJETO REACTIVO
  data_sim <- reactive({
  set.seed(12345)
  re_intercepts(n_groups = input$n_groups1, n_obs = input$n_obs1, var_int = input$V.ind0, 
                var_slop = input$V.inde, var_res = input$V.err0, cov = input$COVind0.ind1e)
  })  
  #MODELOS (RESULTADO DE SIMULACIÓN)
  m0 <- reactive({
    lm(resp ~ 1 + ind, data = data_sim())
  })
  m1 <- reactive({
    lm(resp ~ 1 + ind + as.factor(Group), data = data_sim())
  })
  m2 <- reactive({
    lmer(resp ~ 1 + (1|Group), data = data_sim())
  })
  m3 <- reactive({
    lmer(resp ~ 1 + ind + (1|Group), data = data_sim())
  })
  m4 <- reactive({
    lmer(resp ~ 1 + ind + (ind|Group), data = data_sim())
  })
  #Output N1: Tabla de Modelos
  output$tabla_03 <- renderText({
    htmlreg(
      list(m0(),m1(),m2(),m3(),m4()),custom.model.names = c("MCO","Efectos Fijos","M. Nulo","Intercepto Aleatorio","Pendiente Aleatoria"),
      custom.gof.names = c("R2","R2 Ajustado","Nº Observaciones","Error Cuadrático Medio","AIC","BIC",
                           "Log-Verosimilitud","Nº Grupos","Varianza Grupos","Varianza Residual",
                           "Varianza Grupos V. Independiente","Covarianza Grupos-V. Independiente"), caption = "",
      custom.coef.map = list("(Intercept)"="Intercepto","ind"="V. Independiente")
    )
  })
  #Output N2: Análisis Descriptivo
  output$grafico_04 <- renderPlot({
    ggplot(data = data_sim()) + 
      geom_smooth(mapping = aes(x = ind, y = resp, group = Group), method = "lm", se = FALSE) +
      geom_smooth(mapping = aes(x = ind, y = resp), method = "lm", se = TRUE, colour ="red") +
      labs(title = "Relación Lineal entre Variable Independiente y Variable de Resultado según Grupo",
           subtitle = "Estimación MCO. Línea Roja representa Intercepto Único",
           x = "Variable Independiente", y = "Variable de Resultado") + theme_bw()
      })
  #Output N3: Efectos Aleatorios
  output$grafico_05 <- renderPlot({
    plot_model(m3(), type = "re", terms = "ind", grid = FALSE, sort.est = "sort.all",
               title = "Efectos Aleatorios (basado en M. Interceptos Aleatorios)" ) + theme_bw()
  })
  #Output N4: Efectos de Variable Explicativa
  output$grafico_06 <- renderPlot({
    plot_model(m3(), type ="pred", terms = c("ind","Group"), pred.type = "re",
               title = "Relación entre Variable Explicativa y Resultado (Modelo 3)",
               axis.title = c("Variable Explicativa","Variable de Resultado")) + theme_bw()
  })
  #Output N5: Interceptos Aleatorios
  output$grafico_07 <- renderPlot({
    plot_model(m4(), type ="pred", terms = c("ind","Group"), pred.type = "re",
               title = "Relación entre Variable Explicativa y Resultado (Modelo 4)",
               axis.title = c("Variable Explicativa","Variable de Resultado")) + theme_bw()
  })

}


shinyApp(ui = interfaz_usuario, server = servidor)

