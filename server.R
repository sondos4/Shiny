library(shiny)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(shinyjs)

function(input, output) {
  
  questions = read.csv('data-package/GS_AS2016_DataSet.csv', skip = 1, header = TRUE)
  qMapping = read.csv('data-package/QuestionMapping.csv')
  names(questions)[1:5] = c('finished', 'country', 'region', 'subreg', 'lang')
  names(questions)[10:14] = c('gender','agegroup','nationality', 'iscResidence', 'cResidence')
  questions = questions %>% filter(subreg != '#N/A')
  questions = questions %>% filter(region != "#N/A")
  
  nbOfResponders = nrow(questions)
  
  byRegion = questions %>%
    group_by(region) %>%
    summarise(total = n())
  
  byRegionPerc = byRegion %>%
    mutate(percentage  = round(total/nbOfResponders* 100, 2), nsmall = 2) 
    
  byGender = questions %>%
    group_by(gender) %>%
    summarise(percentage = round(n()/nbOfResponders * 100, 2), nsmall = 2) #%>%
  
  
  byGenderByReg = questions %>%
    group_by(subreg, gender) %>%
    summarise(percentage = round(n()/nbOfResponders * 100, 2), nsmall = 2) #%>%

  bysubRegion = questions %>%
    group_by(subreg) %>%
    summarise(total = n()) %>%
    arrange(desc(total)) 
  
  bysubRegion10 = bysubRegion %>%
    slice(1:10)
  
  bysubRegionPerc = bysubRegion10 %>%
    mutate(percentage = round(total/nbOfResponders * 100, 2), nsmall = 2)
  
  byAgeGroupByReg = questions %>%
    group_by(subreg, agegroup) %>%
    summarise(percentage = round(n()/nbOfResponders * 100, 2), nsmall=2)
    
  byAgeGroup = questions %>%
    group_by(agegroup) %>%
    summarise(percentage = round(n()/nbOfResponders * 100, 2), nsmall = 2)
  
  groupFun = function(field1, field2, sub){
    if(field1 == 'subreg'){
    totaldf = questions %>%
      filter(subreg == sub)}
    else{
      totaldf = questions %>%
        filter(region == sub)
    }
    total = nrow(totaldf)
    
    df = totaldf%>%
      group_by_(field1, field2) %>%
      summarise(percentage = round(n()/total * 100, 2), nsmall = 2)
    
    df
  }
  
  oldChoice = ''

  observeEvent(input$groupby, {
    output$plot1 <- renderPlot({

      choice1 = input$groupby

      if(choice1 == 'Europe'){
        var = input$Europe
      }
      else if (choice1 == 'Africa'){
        var = input$Africa
      }
      else if(choice1 == 'Americas'){
        var = input$Americas
      }
      else if(choice1 == 'Oceania'){
        var = input$Oceania
      }
      else if(choice1 == 'Asia'){
        var = input$Asia
      }
      if(oldChoice == ''){
        show(choice1)
      }
      else{
        hide(oldChoice)
        show(choice1)
      }

      if(var == 'All'){ 
        dataset2 = groupFun('region','gender', choice1)
        dataset3 = groupFun('region','agegroup', choice1)
        totalParticipants = byRegion$total[byRegion$region == choice1]
        dataset1 = groupFun('region', 'subreg', choice1)
        

        output$text2 <- renderText({ 
          paste(totalParticipants,' participants from', choice1, '\n')
        })
        
        p = ggplot(dataset1, aes(reorder(subreg, +percentage), y=percentage)) +
          geom_bar(stat = 'identity', position = 'identity', width = 0.4) +
          ggtitle("Subregion statistics") +
          coord_flip() +
          geom_text(aes(label =paste0(percentage,"%"), hjust = - 0.2)) +
          xlab('Sub region') +
          theme_bw()
        p1 = ggplot(dataset2, aes(x = gender, y = percentage, fill = gender)) + 
          geom_bar(stat = 'identity', position = 'identity', width=0.2) + 
          ggtitle("Participants' gender distribution in region") + 
          geom_text(aes(label =paste0(percentage,"%"), vjust = - 0.2)) +
          ylab('Percentage') +
          theme_bw()
        p2 = ggplot(dataset3, aes(x = agegroup, y = percentage, fill = agegroup)) + 
          geom_bar(stat = 'identity', position = 'identity', width=0.2) + 
          ggtitle("Participants' age distribution in region") + 
          geom_text(aes(label =paste0(percentage,"%"), vjust = - 0.2)) + 
          ylab('Percentage') +
          theme_bw()
        
        grid.arrange(p, p1, p2, ncol = 1, nrow = 3)
        
      }
      else { 
        dataset2 = groupFun('subreg','gender', var)
        dataset3 = groupFun('subreg', 'agegroup', var)
        totalParticipants = bysubRegion$total[bysubRegion$subreg == var]

        output$text2 <- renderText({ 
          paste(totalParticipants,' participants from', var)
        })
        
        p1 = ggplot(dataset2, aes(x = gender, y = percentage, fill = gender)) + 
          geom_bar(stat = 'identity', position = 'identity', width=0.4) + 
          ggtitle("Participants' gender distribution in subregion") + 
          geom_text(aes(label =paste0(percentage,"%"), vjust = - 0.2)) + 
          ylab('Percentage') +
          theme_bw()
        p2 = ggplot(dataset3, aes(x = agegroup, y = percentage, fill = agegroup)) + 
          geom_bar(stat = 'identity', position = 'identity', width=0.4) + 
          ggtitle("Participants' group distribution in subregion") + 
          geom_text(aes(label =paste0(percentage,"%"), vjust = - 0.2)) + 
          ylab('Percentage')+
          theme_bw()
        
        grid.arrange(p1, p2, ncol = 1, nrow = 2)
      }
    }
    , height=850, width = 1000)
    oldChoice = input$groupby
    show('plot1')
    })

  
}
