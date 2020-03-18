library(shiny)
library(shinydashboard)
library(plotly)
library(rpart)
library(rpart.plot)
library(MASS)
library(readxl)
library(readr)
library(wordcloud)
library(tm)
library(RColorBrewer)
library(maps)
library(mapdata)
library(cowplot)
library(ggplot2)
library(ggmap)
library(ggthemes)
library(widgetframe)
library(Rfacebook)
library(RCurl)

#UI

ui <- dashboardPage(
  dashboardHeader(
    title = "Contents"
  ),
  
  dashboardSidebar(
    sidebarMenu(
        menuItem('Profile', tabName='me',icon = icon("user"), startExpanded = TRUE,
                           menuSubItem('The Data Miner', tabName='visual_resume'),
                           menuSubItem('About me', tabName='about_me')),
        menuItem(" My Interests",tabName="fblikes", icon = icon("facebook")),
        menuItem("Search Engine Text Analytics",tabName="afcase", icon = icon("plane")),
        menuItem("Birth Weight Prediction",tabName="birth", icon = icon("child"))
      )),
  dashboardBody(
    tabItems(
      tabItem(tabName = "visual_resume",h3("The Data Miner"),
             # box(background="blue",
             column(width=6,
                 tags$img(src = 'visualresume.png', height = 600, width = 540)),

             #),br(),
              h4(textOutput("me")),
              br(),br(),
              h4("Let's connect!"),
              h3(strong(a(icon("envelope"),title = "Email", href = "mailto:palak_kaur@icloud.com"))),
              h3(strong(a("",icon("linkedin"),title = "LinkedIn", href = "http://www.linkedin.com/in/palak-kaur"))),
              h3(strong(a(icon("instagram"), title = "Instagram",href = "https://www.instagram.com/thatbrowngirlie/")))
            
              
      ),
      tabItem(tabName = "about_me", h3("Palak Kaur"),
              #textOutput("me"),
              column(width=4,
              box(background="blue",h4("Education"),strong("Master of Business Analytics (with Distinction)"),br(),
                  "Hult International Business School - San Francisco, USA",br(),br(),
                  "Data Mining for Business Using R",br(),
                  "Harvard Summer School - Cambridge, United States",br(),br(),
                  strong("Post Degree Diploma in Business Studies (Marketing)"),br(),
                  "Vancouver Island University - British Columbia, Canada",br(),br(),
                  strong("Bachelor of Science (BSc) in Mathematics & IT"),br(),
                  "Panjab University - Chandigarh, India",width=NULL),
              box(background="blue",h4("Experience"), 
                  strong("Business Data Analyst - ZoomSystems, San Francisco"),br(),
                  "Sept 2019 - Present", br(), br(),
                  strong("Teaching Assistant - Hult International Business School, San Francisco"), br(),
                  "Oct 2019 - Present", br(), br(),
                  strong("Business Analyst Intern - ZoomSystems, San Francisco"),br(),
                  "May 2019 - Aug 2019", br(), br(),
               
                  strong("Statistical Analysis Volunteer - Ganga Library, Washington DC"),br(),
                 "Mar 2019 - Jun 2019", br(),
                 #"Analyzed datasets of Nobel laureates and create statistical reports to be showcased on website (Work from Home).",br(),
                  strong(a( (tags$span(style="color:orange", "Work Sample")), href = "http://www.gangalib.org/fieldspeace.php") ),
                  width=NULL)
              ),
              box(background="blue",h4("Projects"),strong("Air France Business Case using R"),br(),"Explored web-based data to assess various search engines to optimize Return on Advertising. Also implemented text analytics to find
                  frequently searched keywords to detect profitable tokens.",
                  br(),br(),strong("Website User Information analysis using R"),br(),"Analyzed OkCupid user bio descriptions & personal information to offer recommendations to increase website traffic.",
                  br(),br(), strong("Birth Weight Prediction Model using Python"), br(),
                  "Implemented a model with 70% test set accuracy using regression analysis to detect newborn weights.",
                  br(),br(),strong("EDA of World Bank data using Python"),br(), "Analyzed a dataset of Northern Latin American/Caribbean countries to develop a strategy for missing values & outliers & gather useful insights.",
                  br(),br(),strong("Data Management Case"),br(),"Formulated a data strategy for The Vault (a San Francisco based company) to create a competitive advantage with effective use of customer data.",
                  br(),br(),strong("Customer Churn Prediction Model"),br(),"Implemented a model with 78% accuracy using discriminant analysis to predict whether a customer is likely to churn.",
                  width=4),
               column(width=4,
               box(background="blue",h4("Honors and Awards"), strong("Dean's Scholar"),"- Hult International Business School",br(),
                 "Group of select individuals chosen by Dean on basis of academic or professional excellence",br(),br(),
                  strong("Deans's Honor List"),"- Vancouver Island University",br(),"Acacemic Year 2017-18",br(),br(),
                  strong("2 Silver medals in Fencing"),"- Panjab University",br(), "Inter-College Fencing Championship",width=NULL),
               box(background="blue",h4("Social Impact"),
                   strong("Client Support and Center Organization Volunteer for Nanaimo Women Center, BC, Canada"), br(),br(),
                   strong("Bingo Assistant Volunteer for Pacifica Housing, BC, Canada"),br(), "Participated in humanitarian work with a group of 8 elderly people at Pacifica Housing",br(),br(),
                   strong("Canvasser for Canadian Cancer Society"),br(),br(),
                   strong("Volunteer at Wellnessnews Women's Expo, Nanaimo, BC in Feb 2018"), width=NULL)

            )),
    tabItem(tabName = "fblikes", h2("My Interests (on the basis of Facebook likes)"),
            fluidRow(
            column(width=10,
            box(h3(strong("Personality Analysis using Facebook data")),background="blue",br(),br(),
                h3(uiOutput("likes")),br(),actionButton("button", "Click to view more!"),
                br(),
                br(),height=300), box(title="Steps taken:","A new App has been created on the Facebook platform to connect to the Facebook API.",br(),
                                           "Once connected, the dataset of Liked Facebook Pages has been extracted and each page name is made to appear randomly by clicking the action button.",
                                          collapsible=TRUE,collapsed=TRUE)
            )),
        "Data Source: Facebook"
                )
            ,
     tabItem(tabName = "birth",h2("Predicting Infant Birth Weights"),
             h4("Using CART (Classification and Regression Trees) Modeling"),
          fluidRow(
              column(width=6,
                     box(plotOutput("tree"),collapsible = TRUE,
                              status="primary",width=NULL),"Data Source: birthwt from MASS package",
                              title="Decision Tree"),
               column(width=5, 
                     box("The dependent variable of this decision tree is weight of newborn having 2 classes:",br(),
                       "1 = newborn weighs below 2.5 kg; 0 = otherwise", br(),"The root of this tree contains",strong("90%"), "observations from the dataset.",br(),
                       "ptl = # of Premature labours in the past", br(), "lwt = Mother's weight during last menstrual period (in pounds)",br(),
                       "ht = History of Hypertension for mother (0 or 1)",br(),
                       sliderInput("cp",
                                   "Select the proper cp value (to prune or grow the tree):",
                                   min = 0.015,
                                   max = 0.12,
                                   value = 0.05), width=NULL, status="primary"),
                     box(title=strong("Insights"), strong("Most influential variable for this classification is history of premature labour."),br(),
                        strong("The decision tree signals that if a mother has had any number of premature labours and
                              her weight during last menstrual period was below 132 pounds,
                              the baby is likely to weigh less than 2.5 kg, and the 
                              the odds and probability of this event occuring are 2.4 and 70.6% respectively."),width=NULL,
                       collapsible=TRUE, collapsed=TRUE, background = "blue"))
            )),
    tabItem(tabName="afcase",h2("Frequently searched tokens by people looking for airline tickets"),
            h4(strong("Which tokens led to bookings for Air France?")),
            fluidRow(
              column(width=8,
                 checkboxInput("goodtokens", "Show profitable tokens only",FALSE),
                 title="Word Cloud",
                 plotOutput("conditionalwordcloud"),br(),"Data Source: Air France Internet Marketing Case by Kellogg School of Management",br(),br(),
              box(status="primary","A basic example of text analytics being used to find the tokens leading to ticket bookings for Air France.",br(),
               strong("Larger the size of token in the wordcloud, more frequently it appears in the dataset."),br(),
               "CHEAP stands out in the word cloud containing the entire set of keywords (visual representation of text data).",br(),
               "FRANCE stands out in the other (a subset). These are the tokens that resulted in a purchase.",br(),
               "In the subset, CHEAP  is visible but only in the background. It doesn't show up until we decrease the minimum frequency value from slider bar on the right.",width=NULL)
       ),
            column(width=4,br(),br(),
               box("For all tokens:",br(),"Choose:",
                    sliderInput("maxwords1","Maximum number of keywords to be plotted", 
                    min = 1,max = 363, value = 240), 
                    sliderInput("freq1","Minimum frequency of tokens",min=1,max=787,value=266),
                    width=NULL,status="primary"),
              box('For Profitable tokens:',br(),"Choose:",
                    sliderInput("maxwords2","Maximum number of keywords to be plotted",
                    min = 1,max = 89, value = 70),
                    sliderInput("freq2","Minimum frequency of tokens",min=1,max=115,value=50),
                    width=NULL,status="primary"),
              box(background="blue",collapsible = TRUE,collapsed=TRUE, title= strong("Insights"),
                   strong("What does it tell us?"),br(),
                   strong("People looking for cheaper flights explore all available options before making a purchase."),br(),br(),
                   strong("How can this information be used to increase Return on Advertising?"),br(),
                   strong("Focus on the GOOD tokens and invest more in the search engines where these keywords were searched."),width=NULL)
    ))  
 )
                  )) )   #   ,
                    #  checkboxInput("1stcafe", strong("View the world's first Starbucks coffee shop"),FALSE)


#Server
server <- function(input, output) {
output$me <- renderText({"Goal oriented analyst with expertise in programming, 
  interpreting data and summarizing insights to non-technical departments. 
  Highly skilled in writing complex SQL queries, data visualization, text analytics, 
  and supervised machine learning."
    }) 

output$tree <- renderPlot({ 
  data("birthwt")
  cols <- c('low', 'race', 'smoke', 'ht', 'ui')
  birthwt[cols] <- lapply(birthwt[cols], as.factor)
  set.seed(1)  

 train <- sample(1:nrow(birthwt), 0.9* nrow(birthwt))
  test <- birthwt[-train,]
  birth_tree <- rpart(low ~ . - bwt, data = birthwt[train, ], method = 'class', cp=input$cp)
  rpart.plot::rpart.plot(birth_tree, type=1, extra=1, box.palette = c("green","pink"),
                         tweak=1.2, branch.lty=3, shadow.col = "gray", roundint=FALSE)
      },height=400)
output$conditionalwordcloud <- renderPlot({
  if(input$goodtokens == FALSE){
    air_france <- read_excel("air france.xlsx")
    text1 <- air_france$Keyword
    docs1 <- Corpus(VectorSource(text1)) 
    docs1 <- tm_map(docs1, content_transformer(tolower)) 
    docs1 <- tm_map(docs1, removePunctuation)
    docs1 <- tm_map(docs1, stripWhitespace)
    tdm1 <- TermDocumentMatrix(docs1)
    matrx1 <- as.matrix(tdm1) 
    v1 <- sort(rowSums(matrx1),decreasing=TRUE) 
    d1 <- data.frame(word = names(v1),freq=v1)
    wordcloud(d1$word,d1$freq, 
              random.order=FALSE, rot.per=0.35, 
              use.r.layout=FALSE, colors=brewer.pal(8, "Spectral"), scale=c(5,0.5),
              max.words=input$maxwords1,min.freq=input$freq1)
  } else{ 
    air_france <- read_excel("air france.xlsx")
    purchase_sub <- air_france[which(air_france$`Total Volume of Bookings`>0),]
    text2 <- purchase_sub$Keyword
    docs2 <- Corpus(VectorSource(text2)) 
    docs2 <- tm_map(docs2, content_transformer(tolower)) 
    docs2 <- tm_map(docs2, removePunctuation)
    docs2 <- tm_map(docs2, stripWhitespace)
    tdm2 <- TermDocumentMatrix(docs2)
    matrx2 <- as.matrix(tdm2) 
    v2 <- sort(rowSums(matrx2),decreasing=TRUE) 
    d2 <- data.frame(word = names(v2),freq=v2)
    wordcloud(d2$word,d2$freq, 
              random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Spectral"), scale=c(6,0.5),
              max.words=input$maxwords2, min.freq=input$freq2)
    }
   })
output$likes <- renderUI({ 
    input$button
    isolate({
      myrecentfblikes <- read_csv("myrecentfblikes.csv")
      myrecentfblikes <- as.data.frame(myrecentfblikes)
      sample(c(myrecentfblikes$x),24)
    })
  })

}
shinyApp(ui = ui, server = server)