
server = function(input, output) {
  
  output$histogram1 <- renderPlot({
    
    ggplot(a_m, aes(x=Var2, y=value, fill=variable)) +
      geom_histogram(stat="identity")+
      scale_fill_viridis_d(name = "Espèce")+
      theme_minimal()+
      xlab("Année du programme")+
      ylab("Nombre de bêtes transpondées par espèce")+
      guides(fill=guide_legend(title="Espèces")) +
      theme(legend.text = element_markdown())
  })
  
  # Data table
  output$dataTable1 <- renderFormattable({
    
    improvement_formatter <- formatter("span", 
                                       style = x ~ style(font.weight = "bold", 
                                                         color = ifelse(x > 0, "#56bf1d", ifelse(x < 0, "#ff7f7f", "black"))), 
                                       x ~ icontext(ifelse(x>0, "arrow-up", "arrow-down"), x))
    
    formattable(
      transposed_data, align =c("c","c","c","c","c","c","c","c","c"), caption = "Table 1.1: Nombre de chauves-souris marquées depuis le début du programme, par année et par espèce",
      list(
        `Espèces` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
        `2016` = color_bar("#9dcf82"),
        `2017` = color_bar("#9dcf82"),
        `2018` = color_bar("#9dcf82"),
        `2019` = color_bar("#9dcf82"),
        `2020` = color_bar("#9dcf82"),
        `2021` = color_bar("#9dcf82"),
        `2022` = color_bar("#9dcf82"),
        `Moyenne` = color_bar("#7cbebf"),
        `Evolution` = improvement_formatter
        
      )
    ) 
  })
  
  output$dataTable2 <- renderFormattable({
    
    improvement_formatter <- formatter("span", 
                                       style = x ~ style(font.weight = "bold", 
                                                         color = ifelse(x > 0, "#56bf1d", ifelse(x < 0, "#ff7f7f", "black"))), 
                                       x ~ icontext(ifelse(x>0, "arrow-up", "arrow-down"), x)
    )
    
    formattable(
      transposed_data2, align =c("c","c","c","c","c","c","c","c","c"), caption = "Table 1.2: Nombre de chauves-souris dont les marqueurs ont été recontrolés selon les années du programme, par espèce",
      list(
        `Espèces` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
        `2016` = color_bar("#9dcf82"),
        `2017` = color_bar("#9dcf82"),
        `2018` = color_bar("#9dcf82"),
        `2019` = color_bar("#9dcf82"),
        `2020` = color_bar("#9dcf82"),
        `2021` = color_bar("#9dcf82"),
        `2022` = color_bar("#9dcf82"),
        `Moyenne` = color_bar("#7cbebf"),
        `Evolution` = improvement_formatter
        
      )
    ) 
  })
  
  output$dataTable3 <- renderFormattable({
    formattable(b1, caption = "Table 1.3: Nombre de marqueurs retrouvés respectivement 1, 2, 3, 4... fois depuis le début du programme, par espèce")
  })
  
  
  output$histogram2 <- renderPlot({
    
    ggplot(data=d, aes(x=Var3, y=Freq, fill=Var1)) +
      geom_histogram(stat="identity")+
      scale_fill_viridis_d(name = "Espèce")+
      theme_minimal()+
      xlab("Raisons de reprise des marqueurs")+
      ylab("Nombre")+
      guides(fill=guide_legend(title="Espèces")) +
      theme(legend.text = element_markdown())
  })
  
  output$map1 <- renderLeaflet({
    
    paltest <- colorFactor("Set3", MapDataT$COMMUNE)
    
    leaflet(MapDataT) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        color = "black",
        weight = 1,
        fillColor = ~paltest(COMMUNE),
        radius = ~log10(`Nombre de transpondages`) * 3,
        fillOpacity = 1,
        popup = paste("<strong>Lieu-dit:</strong>", MapDataT$LIEU_DIT, "<br>",
                      "<strong>Commune:</strong>", MapDataT$COMMUNE, "<br>",
                      "<strong>Nombre de transpondages:</strong>", MapDataT$`Nombre de transpondages`, "<br>",
                      "<strong>Espèces présentes:</strong>", MapDataT$`Espece(s) présente(s)`))
  })
  
  output$histogram3 <- renderPlot({
    
    ggplot(DataR_subNoZero, aes(x=ESPECE, y=dist)) + 
      geom_boxplot()+
      xlab("Espèce marquée")+
      ylab("Distance parcourue (km)")+
      theme_minimal()
    
  })
  
  output$map2 <- renderLeaflet({
    
    paltest <- colorFactor("Set3", TrajSite$COMMUNE)
    pal2 <- reactive({colorFactor("Set1", TrajSite$ESPECE)})
    
    leaflet() %>%
      addTiles() %>%
      addPolylines(
        data = TrajSite,
        lng = ~st_coordinates(geometry)[, 1],
        lat = ~st_coordinates(geometry)[, 2],
        color = ~pal2()(length(unique(ESPECE))),
        opacity = 0.7,
        weight = 2) %>%
      # popup = paste("ID_PIT: ", ID_PIT, "<br>DATE: ", 
      #               DATE, "<br>COMMUNE: ", COMMUNE, "<br>LIEU_DIT: ", 
      #               LIEU_DIT, "<br>DEPARTEMENT: ", DEPARTEMENT, "<br>ESPECE: ", 
      #               ESPECE, "<br>SEXE: ", SEXE, "<br>AGE: ", AGE)) %>%
      addCircleMarkers(
        data = MapDataT,
        lng = ~longitude,
        lat = ~latitude,
        color = "black",
        weight = 1,
        fillColor = ~paltest(COMMUNE),
        radius = ~log10(`Nombre de transpondages`) * 3,
        fillOpacity = 0.8,
        popup = paste("<strong>LIEU_DIT:</strong>", MapDataT$LIEU_DIT, "<br>",
                      "<strong>COMMUNE:</strong>", MapDataT$COMMUNE, "<br>",
                      "<strong>Nombre de transpondages:</strong>", MapDataT$`Nombre de transpondages`)
      ) 
    
  })
  
  
  
  DataSel <- reactive({subset(Data, Data$LIEU_DIT == input$selection_site)})
  #DataSel$ESPECE <- paste0('*', DataSel$ESPECE, '*')
  
  DataSelT<- reactive({subset(DataSel(), DataSel()$ACTION == "T")})
  DataSelC<- reactive({subset(DataSel(), DataSel()$ACTION == "C")})
  DataSelCMinusT <- reactive({subset(DataSelC(), DataSelC()$ID_PIT%in%DataSelT()$ID_PIT)})
  # D'ou viennent les chiro ?
  DataControlOUT <- reactive({subset(Data, Data$ID_PIT %in% DataSel()$ID_PIT)})
  ## Remove chiro that were transponded first in Site Sel
  DataControlOUT2 <- reactive({subset(DataControlOUT(), !(DataControlOUT()$ID_PIT %in% DataSelT()$ID_PIT))})
  ## Remove controls in Site Sel : I only want the place where they were transpoded
  DataControlOUT3 <- reactive({subset(DataControlOUT2(), DataControlOUT2()$ACTION=="T")})
  
  
  
  
  a <- reactive({as.data.frame(table(DataSelT()$ESPECE,DataSelT()$ANNEE))})
  
  b <- reactive({spread(a(), Var1, Freq)})
  
  
  a_m2 <-  reactive({melt(b())})
  
  output$titre_2_dynamique <- renderUI({
    HTML(paste0(h2("2 Zoom sur le site :"), h2(input$selection_site)))  })
  
  output$sous_titre_dynamique2 <- renderText({
    paste0(nrow(DataSelT()), " chauves-souris ont été marquées sur le site '", input$selection_site, "' depuis ", min(DataSelT()$ANNEE), " sur ", length(unique(DataSelT()$ESPECE)), " espèce(s) (", paste(unique(DataSelT()$ESPECE), collapse = ", "), "). ",
           "Sur ce site ", length(unique(DataSelC()$ID_PIT)), " chauves-souris différentes ont été contrôlées (", nrow(DataSelC()), " contrôles au total), dont ", length(unique(DataControlOUT3()$ID_PIT)), " individu(s) venant d'autre(s) site(s) que ", input$selection_site)
  })
  
  
  output$histogram4 <- renderPlot({
    
    ggplot(data=a_m2(), aes(x=Var2, y=value, fill=variable)) +
      geom_histogram(stat="identity")+
      scale_fill_viridis_d(name = "Espèce")+
      theme_minimal()+
      xlab("Année du programme")+
      ylab(paste("Nombre de bêtes transpondées par \n espèce sur le site '", input$selection_site, "'"))+
      guides(fill=guide_legend(title="Espèces")) +
      theme(legend.text = element_markdown())
    
    
  })
  
  data_1_4 <- reactive({
    
    data_1_4 <- b()
    
    names(data_1_4)[names(data_1_4) == 'Var2'] <- "Année"
    
    # Transposition des données
    transposed_data_1_4 <- data_1_4 %>%
      pivot_longer(cols = -Année, names_to = "Espèces", values_to = "Count") %>%
      pivot_wider(names_from = Année, values_from = Count) %>%     #ajout d'une colonne "Moyenne" et "evolution" 
      mutate(
        Moyenne = round(rowMeans(
          cbind(`2016`, `2017`, `2018`), na.rm = TRUE
        ), 2),
        Evolution = round(ifelse(`2017` != 0, (`2018` - `2016`) / `2016` * 100, NA), 2)
      )
    
    transposed_data_1_4
    
  })
  
  
  
  output$dataTable4 <- renderFormattable({
    
    formattable(
      data_1_4(), align =c("c","c","c","c","c","c","c","c","c"), caption = "Table 2.1: Nombre de chauves-souris dont les marqueurs ont été recontrolés selon les années du programme, par espèce")
  })
  
  
  
  test <- reactive({
    
    test <- as.data.frame(table(DataSelC()$ESPECE, DataSelC()$ANNEE))
    test <- spread(test, Var1, Freq)
    names(test)[names(test) == 'Var2'] <- "Année"
    
    
    # Transposition des données
    transposed_test <- test %>%
      pivot_longer(cols = -Année, names_to = "Espèces", values_to = "Count") %>%
      pivot_wider(names_from = Année, values_from = Count) %>%     #ajout d'une colonne "Moyenne" et "evolution" 
      mutate(
        Moyenne = round(rowMeans(
          cbind(`2017`, `2018`, `2019`, `2020`, `2021`, `2022`), na.rm = TRUE
        ), 2),
        Evolution = round(ifelse(`2017` != 0, (`2022` - `2017`) / `2017` * 100, NA), 2)
      )
    
    transposed_test
    
  })
  
  
  output$dataTable5 <- renderFormattable({
    
    
    test_data <- test()
    
    formattable(
      test_data, align =c("c","c"), caption = paste("Table 2.2: Nombre de chauves-souris (issues de tout le programme) dont les marqueurs ont été recontrolés sur le site '",input$selection_site,"',  selon les années."))
  })
  
  
  
  g <- reactive({
    
    g <- as.data.frame(table(DataControlOUT()$ESPECE, DataControlOUT()$ANNEE, DataControlOUT()$COMMUNE, DataControlOUT()$DEPARTEMENT))
    g <- spread(g, Var1, Freq)
    names(g)[names(g) == 'Var2'] <- "Année de transpondage"
    names(g)[names(g) == 'Var3'] <- "Commune"
    names(g)[names(g) == 'Var4'] <- "Département"
    g <- g[rowSums(g[,-(1:3), drop=FALSE]) > 0, ]
    g
  })
  
  
  output$dataTable6 <- renderDT({
    datatable(g(), caption = paste("Table 2.3: Provenance (et année de transpondage) des chauves-souris dont les marqueurs ont été recontrolés sur le site '",input$selection_site,"',  selon les années."),
              options = list(
                pageLength = 15,
                order=list(list(2,'desc')),
                dom = "Bfrtip", 
                autoWidth = TRUE,
                # scroll :
                scrollY = T, scrollX = F, 
                columnDefs = list(list(targets=0, visible=FALSE)),
                # selection :
                select = list(style = 'os', items = 'row'),
                language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json'),
                columnDefs = list(list(className = 'dt-center', targets = '_all')),initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                  "}")) )
  })
  
  
  
  
  output$titre_3_dynamique <- renderUI({
    HTML(
      paste(h3("Les communes dont certains sites à chauve-souris sont connectées au site '", input$selection_site, "' sont présentées dans la carte ci-dessous :")))
  })
  
  ## Points
  
  # Define a reactive expression to filter the data
  DataSelFiltered <- reactive({
    subset(Data, Data$ID_PIT %in% DataSel()$ID_PIT)
  })
  
  # Define a reactive expression to select columns
  DataSelPoints <- reactive({
    DataSelFiltered() %>%
      select(COMMUNE, LIEU_DIT, DEPARTEMENT, LONG_L93, LAT_L93, ESPECE)
  })
  
  
  # Define a reactive expression to add count
  DataSelPointsWithCount <- reactive({
    dplyr::add_count(DataSelPoints(), LIEU_DIT)
  })
  
  # Define a reactive expression to summarize
  h <- reactive({
    ddply(DataSelPointsWithCount(), .(LIEU_DIT), summarize, ESPECE = paste(unique(ESPECE), collapse=", "))
  })
  
  # Define a reactive expression for the final merged data
  DataSelPointsMerged <- reactive({
    DataSelPointsMerged <- merge(DataSelPointsWithCount(), h(), by="LIEU_DIT", all=T) 
    names(DataSelPointsMerged)[names(DataSelPointsMerged) == 'ESPECE.y'] <- 'Espece(s) présente(s)'
    DataSelPointsMerged
  })
  
  DataSelPointsMerged_sans_duplicates <- reactive({
    DataSelPointsMerged_sans_duplicates <- DataSelPointsMerged() %>% filter(!duplicated(select(., LONG_L93, LAT_L93)))
    names(DataSelPointsMerged_sans_duplicates)[names(DataSelPointsMerged_sans_duplicates) == 'n'] <- 'Nombre de transpondages'
    coordinates(DataSelPointsMerged_sans_duplicates) <- ~ LONG_L93 + LAT_L93
    DataSelPointsMerged_sans_duplicates
  })
  
  DataSelPointsMerged_sf <- reactive({
    DataSelPointsMerged_sans_duplicates <- DataSelPointsMerged_sans_duplicates()
    DataSelPointsMerged_sf <- st_as_sf(DataSelPointsMerged_sans_duplicates, coords = c("LONG_L93", "LAT_L93"), crs=2154) %>%
      st_set_crs(2154) %>%
      st_transform(4326)
    
    DataSelPointsMerged_sf
  })
  
  # Define a reactive expression for the final map data
  MapDataSelPoints <- reactive({
    DataSelPointsMerged_sf <- DataSelPointsMerged_sf()
    st_as_sf(DataSelPointsMerged_sf, coords = c("LONG_L93", "LAT_L93"), crs=4326)
  })
  
  
  
  ## Lines
  
  ########### That's where I actually subset the data of Site Sel from the whole Line Dataset. 
  DataSelLines_subset <-  reactive({  subset(DataSelLines, ID_PIT %in% DataSel()$ID_PIT) })
  DataSel_Small <- reactive({ DataSel() %>% select("DATE", "COMMUNE", "LIEU_DIT", "DEPARTEMENT", "ESPECE", "SEXE", "AGE",  "ID_PIT") })
  
  DataSelLines_final <- reactive({merge(DataSelLines_subset(), DataSel_Small(), by="ID_PIT", all=F)})
  
  ############
  
  output$titre_4_dynamique <- renderText({
    HTML(
      paste(h3("Chauves-souris transpondées sur le site ", input$selection_site, " et retrouvées ailleurs :")) 
    )
  })
  
  output$map3 <- renderLeaflet({
    
    pal <- colorFactor("Set3", MapDataSelPoints()$COMMUNE)
    pal2 <- reactive({colorFactor("Set1", DataSelLines_final()$ESPECE)})
    
    leaflet() %>%
      addTiles() %>%
      # Add lines to the map
      addPolylines(
        data = DataSelLines_final(),
        lng = ~st_coordinates(geometry)[, 1],
        lat = ~st_coordinates(geometry)[, 2],
        # color = wes_palette("IsleofDogs2", n = length(unique(DataSelLines_final()$ESPECE))),
        color = ~pal2()(length(unique(ESPECE))),
        opacity = 1,
        weight = 2,  # Adjust the weight of the lines as needed
        popup = ~paste("DATE: ", DATE, "<br>COMMUNE: ", COMMUNE, "<br>LIEU_DIT: ", LIEU_DIT,
                       "<br>DEPARTEMENT: ", DEPARTEMENT, "<br>ESPECE: ", ESPECE, "<br>SEXE: ", SEXE,
                       "<br>AGE: ", AGE)
      ) %>%
      addCircleMarkers(
        data = MapDataSelPoints(),
        lng = ~st_coordinates(geometry)[, 1],
        lat = ~st_coordinates(geometry)[, 2],
        radius = 5,  # Adjust the radius of the circles as needed
        color = "black",
        fillColor = ~pal(COMMUNE),
        stroke = TRUE,
        fillOpacity = 1,
        weight = 1,
        popup = ~paste("COMMUNE: ", COMMUNE, "<br> LIEU_DIT: ", LIEU_DIT, "<br> DEPARTEMENT: ", DEPARTEMENT, "<br> ESPECE: ", ESPECE.x),
        label = ~ESPECE.x
      )
    
  })
  
  
  DataSelLinesIN <- reactive({subset(DataSelLines_final(), DataSelLines_final()$ID_PIT%in%DataSelT()$ID_PIT)})
  
  
  output$map4 <- renderLeaflet({
    
    if(nrow(DataSelLinesIN())>0){
      
      
      pal <- colorFactor("Set3", MapDataSelPoints()$COMMUNE)
      pal2 <- reactive({colorFactor("Set1", DataSelLinesIN()$ESPECE)})
      
      
      # Create the map
      combined_map <- leaflet() %>%
        addTiles() %>%
        addPolylines(
          data = DataSelLinesIN(),
          lng = ~st_coordinates(geometry)[, 1],
          lat = ~st_coordinates(geometry)[, 2],
          # color = wes_palette("IsleofDogs2", n = length(unique(DataSelLinesIN()$ESPECE))),
          color = ~pal2()(length(unique(ESPECE))),
          opacity = 1, 
          weight = 2,
          label = ~paste("Espèce: ", ESPECE)) %>%
        addCircleMarkers(
          data = MapDataSelPoints(),
          lng = ~st_coordinates(geometry)[, 1],
          lat = ~st_coordinates(geometry)[, 2],
          radius = 5,  # Adjust the radius of the circles as needed
          color = "black",
          fillColor = ~pal(COMMUNE),
          stroke = TRUE,
          fillOpacity = 1,
          weight = 1,
          popup = ~as.character(LIEU_DIT), label = ~as.character(COMMUNE))
    }
  })
  
  
  
  v <- reactive({subset(DataC, DataC$ID_PIT %in% DataSelT()$ID_PIT)})
  
  w <- reactive({subset(v(), v()$LIEU_DIT != input$selection_site)})
  
  
  x <- reactive({
    a <- as.data.frame(table(v()$ESPECE, v()$ANNEE, v()$COMMUNE, v()$DEPARTEMENT))
    b <- spread(a, Var1, Freq)
    names(b)[names(b) == 'Var2'] <- "Année de transpondage"
    names(b)[names(b) == 'Var3'] <- "Commune"
    names(b)[names(b) == 'Var4'] <- "Département"
    c <- b[rowSums(b[, -(1:3), drop = FALSE]) > 0, ]
    return(c)
  })
  
  
  output$dataTable7 <- renderDT({
    datatable(x(), caption = paste("Destination (et année de transpondage) des chauves-souris dont les marqueurs ont été posés sur le site '", input$selection_site, "',  selon les années."),
              options = list(
                pageLength = 15,
                order=list(list(2,'desc')),
                dom = "Bfrtip", 
                autoWidth = TRUE,
                # scroll :
                scrollY = T, scrollX = F, 
                columnDefs = list(list(targets=0, visible=FALSE)),
                # selection :
                select = list(style = 'os', items = 'row'),
                language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json'),
                columnDefs = list(list(className = 'dt-center', targets = '_all')),initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                  "}")) )
  })
  
  
  
  DataSelLinesOUT <- reactive({subset(DataSelLines_final(), !(DataSelLines_final()$ID_PIT %in% DataSelT()$ID_PIT))})
  
  output$titre_5_dynamique <- renderUI({
    HTML(
      paste(h3("Chauves-souris transpondées sur d'autres sites et retrouvées sur le site '", input$selection_site, "'")))
  })
  
  
  pal <- reactive({colorFactor("Set3", MapDataSelPoints()$COMMUNE)})
  pal2 <- reactive({colorFactor("Set1", DataSelLinesOUT()$ESPECE)})
  
  output$map5 <- renderLeaflet({
    
    pal2 <- NULL  
    
    if(nrow(DataSelLinesOUT())>0){
      
      # Create the map
      combined_map <- leaflet() %>%
        addTiles() %>%
        addPolylines(
          data = DataSelLinesOUT(),
          lng = ~st_coordinates(geometry)[, 1],
          lat = ~st_coordinates(geometry)[, 2],
          # color = ~wes_palette("IsleofDogs2", n = length(unique(DataSelLinesOUT()$ESPECE))),
          color = ~pal2()(length(unique(ESPECE))),
          opacity = 1, 
          weight = 2,
          label = ~paste("Espèce: ", ESPECE)) %>%
        addCircleMarkers(data = MapDataSelPoints(),
                         lng = ~st_coordinates(geometry)[, 1],
                         lat = ~st_coordinates(geometry)[, 2],
                         radius = 5,  # Adjust the radius of the circles as needed
                         color = "black",
                         fillColor = ~pal()(COMMUNE),
                         stroke = TRUE,
                         fillOpacity = 1,
                         weight = 1,
                         popup = ~as.character(LIEU_DIT), label = ~paste("Commune: ", as.character(COMMUNE)))
    }
    
  })
  
  
  
}
