data = read.csv('../data_all.csv',sep = ',')
data <- as.data.frame(data)
data$City <- as.factor(data$City)
data$Customer.type <- as.factor(data$Customer.type)
data$Gender <- as.factor(data$Gender)
data$Product.line <- as.factor(data$Product.line)
data$Payment <- as.factor(data$Payment)
data$Total = as.numeric(data$Total)
data$Rating = as.character(data$Rating)
data$Branch = as.factor(data$Branch)

library(DT)
library(shinythemes)
library(plotly)
library(shiny)

ui <-navbarPage("Super Market Yönetim Paneli",collapsible = TRUE, inverse = TRUE, theme = shinytheme("united"),
                     

    # Application title
    
      tabPanel("Ana Sayfa", 
               fluidPage(
                 tags$h2("Süper Market Satış Kayıtları"),
                 fluidRow(
                   column(6,
                          tags$br(),
                      img(src = "https://images.unsplash.com/photo-1525328437458-0c4d4db7cab4?ixlib=rb-1.2.1&ixid=MnwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8&auto=format&fit=crop&w=1470&q=80", height="88%", width="88%", align="left")
                          ),
                   column(6,
                          tags$h3("Kayıtlar"),
                          DT::dataTableOutput("table"))
                 ),
               fluidRow(
                 tags$p("Tanımlayıcı İstatistikler"),
                 verbatimTextOutput("summary")
               ),
               tags$hr(),    
               
               )),
    
    tabPanel("Mağaza Bilgileri",
             fluidPage(
             tags$h1("Gelir"),
             tags$br(),
             
             plotlyOutput("incomets"),
             tags$br(),
             tags$hr(),
             
             fluidRow(
               column(6,          
                      tags$h2("Alışveriş Ödeme Yöntemleri Dağılımı"),
                      plotlyOutput("paymentpie"),
                      tags$br()
            
               ),
               column(6,
                      tags$h2("Ödeme Yöntemi"),
                      plotlyOutput("incomehist"),
                      tags$br(),
                      
               )
             ),
             
             
             
             
             tags$h2("Gelire Etkisi Bulunan Şehirler"),
             tags$hr(),
             
             plotlyOutput("cityviaincome"),
             tags$br(),
             
             
             tags$h2("Gelire Etkide Bulunan Mağazalar"),
             tags$hr(),
             
             fluidRow(
               column(6,
                      tags$p("Mağaza Şubeleri ~ Gelir"),
                      plotlyOutput("incomeviabranch"),align='center',
                      tags$br(),  
                      ),
               column(6,
                      tags$p("Mağaza Şubeleri"),
                      plotlyOutput("incomeviabranch2"),align='center',
                      tags$br(),   
                      )
             ),
           

             ),
             ),
    
    tabPanel("Müşteriler",
             tags$h1("Müşteri Demografik Bilgileri"),
             fluidPage(
             tags$h2("Cinsiyet Dağılımı"),
             tags$table(
               tags$tr(
                 tags$th(
                   plotlyOutput("genderbar"),align='center',
                 ),
                 tags$th(
                   plotlyOutput("genderpie"),align='center'
                 )
               )
               
             ),
             tags$br(),
             tags$hr(),
             tags$br(),
             
             tags$h2("Satılan Ürün Çeşitleri"),
             fluidRow(
               column(6,          
                      plotlyOutput("producttop"),align='center',
                      tags$br()
                      
               ),
               column(6,
                      plotlyOutput("product"),align='center',
                      tags$br()                 
               )
             ),
             
             
             tags$br(),
             tags$hr(),
             tags$br(),
            
             fluidRow(
               column(6,
                      tags$p("Müşteri Üyelik Durumu"),
                      plotlyOutput("costumertype"),align='center'
               ),     
               column(6,
                      tags$p("Müşteri Alışveriş Deneyimi Puanı"),
                      plotlyOutput("ratingpie"),align='center'
               )
               
             )
             
             
             )
             ))
    
  
        

server <- function(input, output) {
    
  output$producttop <- renderPlotly({
    plot_ly(data, x = ~Product.line, y =data$Quantity, type = 'bar',color = data$Gender)    
    
  })   
  
  
  output$table = DT::renderDataTable({
    data
  })
  
  
    #please run this commend : table(data$Branch)
    output$incomeviabranch2 <- renderPlotly({
        plot_ly(x = c(173,165,170), y =c('A','B','C'), type = 'bar', orientation = 'h',color = c('Store-A','Store-B','Store-C'))
        

    }) 
    
       
    output$incomeviabranch <- renderPlotly({
        plot_ly(data, y = data$gross.income, x =c(1:length(data$gross.income)), text = ~Branch, type = 'scatter', mode = 'markers', size = ~gross.income, color = ~Branch, colors = 'Paired',
                marker = list(opacity = 0.2, sizemode = 'diameter'))        
        
    }) 
    
    output$ratingpie <- renderPlotly({
        plot_ly(data, labels = ~Rating, values = ~Total, type = 'pie')
        
        
    })
    
    
    
    output$cityviaincome <- renderPlotly({
        plot_ly(data, labels = ~City, values = data$gross.income, type = 'pie')
        
    })    
    
    
    output$incomehist <- renderPlotly({
        plot_ly(x= data$gross.income ,type = "histogram",histnorm = "probability",color = data$Payment)        
    })  
    
    
    output$incomets <- renderPlotly({
        plot_ly(data, y = data$gross.income, x =c(1:length(data$gross.income)) , type = 'scatter', mode = 'lines',color = I('green')
               , name = 'Gelir $'       )%>%
        add_trace( y = data$gross.income, x =c(1:length(data$gross.income)))%>%
        layout(showlegend = F, title='Kazanç Zaman Serisi Grafiği',
               xaxis = list(rangeslider = list(visible = T))) %>%layout(
          xaxis = list(zerolinecolor = '#ffff',
                       zerolinewidth = 3,
                       gridcolor = 'ffff'),
          yaxis = list(zerolinecolor = '#ffff',
                       zerolinewidth = 2,
                       gridcolor = 'ffff'),
          plot_bgcolor='#e5ecf6', width = 900)%>% animation_slider(
            hide = T
          )
    })   
    
    
    
    output$costumertype <- renderPlotly({
        plot_ly(data, labels = data$Customer.type, values = data$Quantity, type = 'pie')
        
    })   
    
    
    output$product <- renderPlotly({
        plot_ly(data, labels = data$Product.line, values = data$Quantity, type = 'pie')
        
    })     
    
    output$paymentpie <- renderPlotly({
        plot_ly(data, labels = ~Payment, values = ~Total, type = 'pie')
        
    })    
    
    output$genderpie <- renderPlotly({
        plot_ly(data, labels = ~Gender, values = ~Total, type = 'pie',hole = 0.6)
        
    })
    
    
    output$genderbar <- renderPlotly({
        plot_ly(data, x =data$Gender, y = data$Total, type = 'bar')

        
    })


    output$summary <- renderPrint({
        
        summary(data)
    })

    
    
    } #server end

# Run the application 
shinyApp(ui = ui, server = server)
