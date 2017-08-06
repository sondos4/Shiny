library(shiny)
library(ggplot2)
library(dplyr)
library(shinyjs)


questions = read.csv('data-package/GS_AS2016_DataSet.csv', skip = 1, header = TRUE)
qMapping = read.csv('data-package/QuestionMapping.csv')
names(questions)[1:5] = c('finished', 'country', 'region', 'subreg', 'lang')
names(questions)[10:14] = c('gender','agegroup','nationality', 'iscResidence', 'cResidence')
questions = questions %>% filter(subreg != "#N/A")
questions = questions %>% filter(region != "#N/A")

nbOfResponders = nrow(questions)
byGender = questions %>%
  group_by(gender) %>%
  summarize(percentage = n()/nbOfResponders * 100)

byRegion = questions %>%
  group_by(region) %>%
  summarise(percentage = n()/nbOfResponders * 100)

regSubRegion = questions %>%
  group_by(region, subreg) 

europe = regSubRegion %>%
  filter(region == 'Europe')

africa = regSubRegion %>%
  filter(region == 'Africa')

america = regSubRegion %>%
  filter(region == 'Americas')

oceania = regSubRegion %>%
  filter(region == 'Oceania')

asia = regSubRegion %>%
  filter(region == 'Asia')

bysubRegion = questions %>%
  group_by(subreg) %>%
  summarise(percentage = n()/nbOfResponders * 100)

subregions = unique(bysubRegion$subreg)
regions = unique(byRegion$region)
europe = unique(europe$subreg)
africa = unique(africa$subreg)
america = unique(america$subreg)
oceania = unique(oceania$subreg)
asia = unique(asia$subreg)
europe = append('All', as.character(europe))
africa = append('All', as.character(africa))
america = append('All', as.character(america))
oceania = append('All', as.character(oceania))
asia = append('All', as.character(asia))

regChoices = c('Europe','Africa', 'Americas', 'Oceania','Asia')
regions = append('All', as.character(regions))

fluidPage(
  
  headerPanel(
    h1("Statistics of participants by region / subregion", 
       style = "font-weight: 350; line-height: 1.1; 
       color: #4682B4;")),
  useShinyjs(),
  sidebarPanel(
    radioButtons('groupby', label = 'Region' , choices = regChoices), 
    hidden(
    radioButtons('Europe', label = h3('Europe'),choices = europe)),
    hidden(
    radioButtons('Africa', label = h3('Africa'),choices = africa)),
    hidden(
      radioButtons('Americas', label = h3('Americas'),choices = america)),
    hidden(
      radioButtons('Oceania', label = h3('Oceania'),choices = oceania)),
    hidden(
      radioButtons('Asia', label = h3('Asia'),choices = asia))
  ),
  
  mainPanel(
    textOutput("text2"),
    tags$head(tags$style("#text2{color: #191970;
                                 font-size: 15px;
                                 font-style: bold;
                                font-style: italic;
                                 }")),
    plotOutput('plot1')
  )
)