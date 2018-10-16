library(DT)
function(input, output) {
  
  # Main page
  output$maxBox = renderInfoBox({
    max_num = max(shape@data$num_dgs, na.rm = T)
    max_zip = max(shape@data$ZIPCODE[shape@data$num_dgs == max_num], na.rm = T)
    infoBox(max_zip, max_num, icon = icon("arrow-up"), color = "green")
  })
  
  output$minBox = renderInfoBox({
    min_num = min(shape@data$num_dgs, na.rm = T)
    min_zip = min(shape@data$ZIPCODE[shape@data$num_dgs == min_num], na.rm = T)
    infoBox(min_zip, min_num, icon = icon("arrow-down"), color = "red")
  })
  
  output$avgBox = renderInfoBox({
    avg_num = mean(shape@data$num_dgs, na.rm = T)
    infoBox("Average", round(avg_num), icon = icon("paw"), color = "yellow")
  })
  
  output$dog_density = renderLeaflet({ leaflet(shape) %>%
      addTiles() %>% 
      setView(-74.00, 40.71, zoom = 11) %>%
      addPolygons(popup =~paste("<b>Zip</b>:", factor(ZIPCODE), "<br/>",
                                "<b>Number of Dogs:</b>", factor(num_dgs), "<br/>",
                                "<b>Population:</b>", factor(POPULAT), "<br/>",
                                #"<b>Dogs per capita (100k):</b>", factor(dgs_pr_), "<br/>",
                                "<b>Breed:</b>", pop_breed, paste0("(",num_breeds,")"), "<br/>",
                                "<b>Name:</b>",factor(pop_name)),
                  label  = ~factor(ZIPCODE),
                  color = ~define_pal(shape@data$num_dgs)(shape@data$num_dgs), 
                  stroke = T) %>%
      addProviderTiles("CartoDB.Positron")
  })
  
  output$percapita = renderPlotly({
    p = shape@data %>%
      group_by(., PO_NAME) %>%
      summarise(., DogsPerCapita = sum(num_dgs)/(sum(POPULAT/100000))) %>%
      filter(., !is.na(DogsPerCapita)) %>%
      select(., PO_NAME, DogsPerCapita) %>%
        ggplot(.,aes(text=sprintf("Area: %s<br> Dogs per Capita (100K): %s", PO_NAME, DogsPerCapita), x = PO_NAME, y = DogsPerCapita)) +
          geom_col(aes(fill = PO_NAME)) +
          theme(legend.position="top",
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank()) +
          labs(fill = "Area") +
          labs(xlab("")) +
          labs(ylab("Dogs per capita (100K)"))
    
    ggplotly(p,tooltip = "text") %>%
      config(displayModeBar = FALSE)
    
    
  })
  
  output$bar_capita = renderPlotly({
   p =  shape@data %>%
      mutate(., Borough = ifelse(Borough == "New York", "Manhattan", Borough)) %>%
      ggplot(., aes(text=sprintf("Zip Code: %s<br> Number of Dogs: %s", ZIPCODE, num_dgs), x = factor(ZIPCODE), y = num_dgs)) +
          geom_col(aes(text = "", fill = Borough )) +
          geom_point(aes(text=sprintf("Dogs per Capita (100K): %s", DogsPerCapita), y = DogsPerCapita, fill = Borough)) +
          scale_x_discrete(breaks=seq(1000, 11697, 3))+ 
          theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
          labs(xlab("Zip Code")) +
          labs(ylab("Dog Count"))
   ggplotly(p,tooltip = "text") %>%
     config(displayModeBar = FALSE)

  })
  
  
  # Breed Density page
  output$breed_density = renderLeaflet({ leaflet(shape) %>%
      addTiles() %>% 
      setView(-74.00, 40.71, zoom = 11) %>%
      addPolygons(popup =~paste("<b>Zip</b>:", factor(ZIPCODE), "<br/>",
                                "<b>Number of Dogs:</b>", factor(num_dgs), "<br/>",
                                "<b>Population:</b>", factor(POPULAT), "<br/>",
                                "<b>Breed:</b>", input$selectedBreed, paste0("(",factor(shape@data[[input$selectedBreed]]),")")),
                                #"<b>Name:</b>", top_names(input$selectedBreed, as.integer(ZIPCODE))),
                  label  = ~factor(ZIPCODE),
                  color = ~define_pal(shape@data[[input$selectedBreed]])(shape@data[[input$selectedBreed]]), 
                  stroke = T) %>%
      addProviderTiles("CartoDB.Positron")
  })
  
  
  output$breed_diversity = renderPlotly({
    df = dogs %>%
      filter(., BreedName != "Unknown") %>%
      group_by(., BreedName) %>%
      summarise(., count = n())
    
    df$BreedName = factor(df$BreedName, levels = df$BreedName[order(df$count)])
    
      ggplot(df, aes(x="", y=count, fill=BreedName))+
        geom_bar(stat = "identity", width = .25) +
        theme(legend.position="none") +
        labs(xlab("Dog Breeds"))
    
  })
  
  output$breed_bar = renderPlotly({
    shape@data %>%
      mutate(., Borough = ifelse(Borough == "New York", "Manhattan", Borough)) %>%
      ggplot(., aes(x = factor(ZIPCODE), y = shape@data[[input$selectedBreed]])) +
      geom_col(aes(fill = Borough )) +
      #geom_point(aes(x = factor(ZIPCODE), y = dgs_pr_, fill = Borough)) +
      scale_x_discrete(breaks=seq(1000, 11697, 3))+ 
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(xlab("Zip Code")) +
      labs(ylab("Dog Count"))
    
  })
  
  # Names page
  output$nameCloud = renderPlot({
    bf = breedFreq(dogs, input$selectedBreed2)
    wordcloud(bf$AnimalName, bf$count, colors=brewer.pal(8, "Dark2"))
  })
  
  output$nameTable = renderDataTable({
    datatable(as.data.frame(breedFreq(dogs, input$selectedBreed2), rownames=FALSE)) %>% 
      formatStyle(1:2, background="skyblue", fontWeight='bold')
    
  })
  
  # Borough page
  output$unique_breed = renderPlotly({
    dogs %>%
      filter(., Borough %in% c("Brooklyn", "Bronx", "Manhattan", "Queens", "Statten Island")) %>%
      group_by(., Borough, BreedName) %>%
      summarise(., num_breeds = n()) %>%
      group_by(., Borough) %>%
      summarise(., num_breeds = n()) %>%
      ggplot(., aes(x = Borough, y = num_breeds)) +
        geom_col(aes(fill = Borough)) 
    
  })
  
  output$top_breeds = renderPlotly({
    dogs %>%
      filter(., BreedName != "Unknown") %>%
      filter(., Borough == input$selectedBorough) %>%
      group_by(., Borough, BreedName) %>%
      summarise(., num_breeds = n()) %>%
      arrange(., desc(num_breeds)) %>%
      group_by(., Borough) %>%
      filter(., row_number() <= 10) %>%
        ggplot(., aes(x = BreedName, y = num_breeds)) +
          geom_col(aes(fill = BreedName), position = "dodge") +
          theme(axis.text.x = element_text(angle = 60, hjust = 1))
   
  })
    
    output$density = renderPlotly({
      dogs %>%
        filter(., Borough %in% c("Brooklyn", "Bronx", "Manhattan", "Queens", "Statten Island")) %>%
        filter(., ZipCode %in% shape@data$ZIPCODE) %>%
        ggplot(., aes(x = ZipCode)) +
        geom_density(aes(color = Borough))

    })
    
    output$zipTable = renderDataTable({
      datatable(as.data.frame(boro_table, rownames=FALSE)) %>% 
        formatStyle(1:3, background="skyblue", fontWeight='bold')
    
    })
  
}