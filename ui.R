
ui = dashboardPage(
  dbHeader,
  dashboardSidebar(
    width = 350,
    selectInput("choix_donnees", "Choix du jeu de données",
                choices = c("BDD_CMR_NA_2016-2022_20230105", "BDD_CMR_NA_2016-2021_20220105", "BDD_CMR_NA_2016-2020_20210105"), selected = "BDD_CMR_NA_2016-2022_20230105", multiple=FALSE, selectize=TRUE),
    
    br(), 
    
    sidebarMenu(
      menuItem("Transpondages", tabName = "1", icon = icon("map")),
      menuItem("Autre volet", tabName = "2", icon = icon("briefcase"),
               startExpanded = FALSE,
               menuSubItem("Dossier 1",
                           tabName = "2a"),
               menuSubItem("Dossier 2",
                           tabName = "2b")
      )
      
    )
  ),
  
  dashboardBody(tags$style(HTML(".content-wrapper { background-color: white; }")),
                fluidRow(column(2),
                         column(8,
                                h1("1. Nombre d'individus marqués"),
                                p("Depuis le début du programme de marquage des chiroptères (2016), 13698 marqueurs ont été posés sur 4 espèces (Grand Rhinolophe (Rhinolophus ferrumequinum), Murin à oreilles échancrées (Myotis emarginatus), Rhinolophe euryale (Rhinolophus euryale), Minioptère de Schreibers (Miniopterus schreibersii))"),
                                plotOutput("histogram1") %>% withSpinner(),
                                br(),
                                formattableOutput("dataTable1") %>% withSpinner(),
                                br(),
                                formattableOutput("dataTable2") %>% withSpinner(),
                                br(),
                                formattableOutput("dataTable3") %>% withSpinner(),
                                br(),
                                p("Figure 1.1: Répartition des raisons de reprise des marqueurs, par espèce. MS = Marqueur trouvé au sol, sans information sur son sort; RI = Individu équipé d’une marque et trouvé mort sans information sur la mort; RM = Individu équipé d’une marque, capturé et mort lors de la manipuation ou de la capture"),
                                plotOutput("histogram2") %>% withSpinner(),
                                br(),
                                p("Carte 1.1: Nombre d'individus transpondés par site"),
                                leafletOutput("map1") %>% withSpinner(),
                                p("Attention, le site Blockhaus n’a pas les bonnes coordonnées !!!"),
                                br(),
                                p("Figure 1.2: Distances parcourues par les différents individus marqués ayant changés de sites, par espèce"),
                                plotOutput("histogram3") %>% withSpinner(),
                                br(),
                                leafletOutput("map2") %>% withSpinner(),
                                br(), 
                                selectInput("selection_site", "Choisissez un lieu-dit", choices = unique(sort(Data$LIEU_DIT)), selected = "Grotte de Rancogne"),
                                uiOutput("titre_2_dynamique") %>% withSpinner(),
                                textOutput("sous_titre_dynamique2"),
                                plotOutput("histogram4"),
                                formattableOutput("dataTable4"),
                                formattableOutput("dataTable5"),
                                DTOutput("dataTable6"),
                                uiOutput("titre_3_dynamique"),
                                leafletOutput("map3") %>% withSpinner(),
                                uiOutput("titre_4_dynamique"),
                                leafletOutput("map4") %>% withSpinner(),
                                DTOutput("dataTable7"),
                                uiOutput("titre_5_dynamique"),
                                leafletOutput("map5") %>% withSpinner()),
                         column(2)
                )
  ))