#HW5_Purevjav_Tuul

library(tidyverse)
library(shiny)
library(shinydashboard)
library(stringr)
library(leaflet)


# get district data on King county
#setwd("/home/tuul/shiny")
districts <- read_csv("Washington_School_Districts.csv", show_col_types = FALSE)
districts <- districts %>% 
  filter(County == "King") %>% 
  select(LEACode, Website) %>% 
  drop_na()

# get school data on King county
schools <- read_csv("Washington_State_Public_Schools.csv", show_col_types = FALSE)
schools <- schools %>%
  filter(County == "King") %>%
  select(Latitude,Longitude,SchoolCode, SchoolName, GradeCategory, LowestGrade, HighestGrade, PhysicalAddress, Email, Phone, LEACode, LEAName) %>%
  drop_na()

# left join using merge() to merge school and district
df.school.district <- merge(x=schools, y=districts, by="LEACode", all.x=TRUE)

# put address and SchoolCode into one df
df.obj.address <- paste0(schools$SchoolCode, ",", schools$PhysicalAddress)

# remove whit space
str_replace_all(df.obj.address, fixed(" "), "")
dat = data.frame(Addresses = df.obj.address, stringsAsFactors = FALSE)

# parse address string into street, city and zip code
dat2 = sapply(dat$Addresses, strsplit, ",")
dat2 = data.frame(matrix(unlist(dat2), ncol = 5, byrow = TRUE), stringsAsFactors = FALSE)
dat2$Zip5 = sapply(dat2$X5, function(x) strsplit(x, "-")[[1]][1])
dat2$Zip4 = sapply(dat2$X5, function(x) strsplit(x, "-")[[1]][2])
dat2 = dat2[,-5]
dat2 = dat2[,-6]
colnames(dat2) = c("SchoolCode", "Street", "City", "State", "Zip")
dat2[, 1] <- sapply(dat2[, 1], as.numeric)

# add parsed address to school
schools <- merge(x=df.school.district, y=dat2, by = "SchoolCode", all.x=TRUE)
# remove white space from City column
schools$City <- str_replace_all(schools$City, fixed(" "), "")

# get map data
m.schools <- schools %>%
  select(Latitude, Longitude, Name =`SchoolName`, District=`LEAName`, Address=`PhysicalAddress`, Phone) 

# create pop-up label for each school
labs <- lapply(seq(nrow(m.schools)), 
               function(i) {
                 paste0( "", "Name: ",as.character(m.schools[i, "Name"]), '<br>',
                         "Address: ", as.character(m.schools[i, "Address"]), '<br>',
                         "Phone: ", as.character(m.schools[i, "Phone"]), '<br>',
                         "District: ",as.character(m.schools[i, "District"]), '<br>',
                         '' ) 
               })

# shiny app code
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      textInput("in_city","City",value="Seattle"),
      menuItem("School by City", tabName = "by_city"),
      menuItem("School by Map", tabName = "by_map")
    )
  ),
  dashboardBody(
    tabItems(
      # first page
      tabItem("by_city",
              h2("Schools in ",textOutput("in_city1", inline=TRUE)),
              h3("Please capitalize the first letter of each city name. Popular cities in King county in WA are                  Seattle, Bellevue, Redmond, Kirkland, etc."),
              box(DT::dataTableOutput("t_similar"), width= 500)
      ),
      # second page
      tabItem("by_map",
              h2("Schools in King County on the Map"),
              box(leafletOutput("t_map"), width= 500)
      )
    )
  )
)

server <- function(input, output) {
  # --------------------------------------------------
  # define the name for titling
  # --------------------------------------------------
  # define the name twice to be used twice above
  output$in_city1 <- renderText({
    input$in_city
  })
  
  # --------------------------------------------------
  # table
  # --------------------------------------------------
  output$t_similar <- DT::renderDataTable({
    
    # get city
    in_city <- input$in_city
    
    # output for in_city
    schools %>% 
      filter(City == in_city)  %>% 
      select(Zip,SchoolName, LowestGrade, HighestGrade, PhysicalAddress, LEAName, Website)
  })
  # --------------------------------------------------
  # map
  # --------------------------------------------------
  output$t_map <- renderLeaflet({
    leaflet(m.schools) %>%
      addTiles() %>%
      addAwesomeMarkers(~Longitude, ~Latitude,
                        label = ~lapply(labs, htmltools::HTML),
                        clusterOptions = markerClusterOptions())
  })
}

shinyApp(ui, server)

