
library(shiny)

ui <- fluidPage(
   
   titlePanel(
     h1("")
     ),
   
   sidebarLayout(
      sidebarPanel(
         textInput("textin","please input things")
      ),
      
      mainPanel(
        
        textOutput("textout") ,
        
        verbatimTextOutput("urlout"),
        
        plotly::plotlyOutput("plot")
        
              )
   )
)

server <- function(input, output) {
  
line <- reactive({
  
  key.words <- input$textin
  
  str_c(
    "https://list.tmall.com/search_product.htm?q=",
    key.words,
    "&type=p&spm=a220m.1000858.a2227oh.d100&from=.list.pc_1_searchbutton",
    sep="")
  
})
  
df  <- reactive({
  key.words <- URLencode(input.keywords)
 
  
  GET(url) %>% 
    content(as = "text") %>% 
    read_html() %>% 
    html_nodes(css = ".productShop-name , .productTitle a , .productPrice em") %>%
    html_text %>% 
    as.tibble() -> df
  
  df %<>% 
    mutate(
      group = rep(c("price","name","shop"),NROW(df)/3)
    )
  
  bind_cols(
    df %>%  
      mutate(
        value = gsub("\n","",value)
      ) %>% 
      filter(group=="price") ,
    df %>% 
      mutate(
        value = gsub("\n","",value)
      ) %>% 
      filter(group=="name") ,
    df %>% 
      mutate(
        value = gsub("\n","",value)
      ) %>% 
      filter(group=="shop")
  ) %>% 
    select(value,value1,value2) %>% 
    rename(price = value ,
           name = value1 ,
           shop = value2) 
  
  })

  output$urlout <- renderPrint({
    
    print(line())
    
  })
  
  output$plot <- plotly::renderPlotly({
    
    price.min <- 30
    
    df() %>% 
      ggplot(aes(reorder(name,price),price)) + 
      geom_point(shape = 3 ) + 
      coord_flip() + 
      theme_minimal() -> t
      plotly::ggplotly(t)
    
  })
  
}

shinyApp(ui = ui, server = server)

