

require(baseballr)
require(dplyr)
require(ggplot2)
require(msm)
require(shiny)
require(shinydashboard)
require(highcharter)


# Map of 2018 MLB Player Names and IDs
master = read.csv("http://crunchtimebaseball.com/master.csv")

master1 = master %>%
  dplyr::filter(mlb_pos == "P") %>%
  dplyr::select(mlb_name, mlb_id, fg_id, mlb_team) %>%
  dplyr::mutate_all(as.character) %>%
  dplyr::mutate(mlb_id = as.numeric(mlb_id))


player_list = master1 %>%
  collect() %>%
  base::split(.$mlb_name) %>%
  purrr::map(~.$mlb_id)


ui = dashboardPage(
  #Header
  dashboardHeader(title = "Statcast Analysis"),
  dashboardSidebar(
    selectInput("Player",
                label = "Player:", 
                choices = player_list, 
                selectize = TRUE,
                selected = NULL),
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard"),
      menuItem("Total Pitches", tabName = "total_pitches"),
      menuItem("Prob of Next", tabName = "pnext"),
      menuItem("Pitch Locations", tabName = "Pitch_Locations"),
      menuItem("Pitch Velocity", tabName = "PitchVelocity")
    )),
  # Body
  dashboardBody(tabItems(
    # First tab content
    tabItem(tabName = "dashboard",
            fluidRow(
              column(12,
                     dataTableOutput('first_pitch')
              )
            )),
    tabItem(tabName = "total_pitches",
            fluidRow(
              column(12,
                     dataTableOutput('total_pitches')
              )
            )),
    tabItem(tabName = "pnext",
            fluidRow(
              column(12,
                     dataTableOutput('pnext')
              )
            )),
    # Second tab content
    tabItem(tabName = "Pitch_Locations",
            fluidRow(
              plotOutput("Pitch_Locations")
            )),
    tabItem(tabName = "PitchVelocity",
            fluidRow(
              plotOutput("pitch_velo")
            )))
  )
)



server <- function(input, output, session) { 
  
  
  # output$selected_var <- renderText({ 
  #   paste("You have selected", input$Player)
  # })

  datasetInput <- reactive({
    scrape_statcast_savant_pitcher(start_date = "2018-03-31", end_date = "2018-10-01", pitcherid = input$Player) %>%
      dplyr::arrange(game_pk, inning, at_bat_number, pitch_number) %>%
      dplyr::mutate(Type = ifelse(game_date < as.Date("2018-07-14"),"First Half", "Second Half")) %>%
      dplyr::mutate(ID = paste0(game_pk, inning, at_bat_number)) %>%
      dplyr::filter(pitch_name != "")
  })
  
  output$first_pitch = renderDataTable({
    
    dataset = datasetInput()
    dataset %>%
      dplyr::rename(Pitch = pitch_name) %>%
      group_by(Pitch) %>%
      dplyr::filter(pitch_number == 1) %>%
      dplyr::count(Pitch) %>%
      ungroup() %>%
      dplyr::rename(Proportion = n) %>%
      dplyr::mutate(`First Pitch %` = paste(round(100 * Proportion/sum(Proportion), 1), "%")) %>%
      dplyr::select(-Proportion)

  })

  output$total_pitches <- renderDataTable({
    
    dataset = datasetInput()
    
    pitcher.table = table(dataset$pitch_name)
    result = prop.table(pitcher.table) %>%
      as.data.frame() %>%
      dplyr::rename(prop = Freq) %>%
      mutate(Proportion = paste(round(100*prop, 1), "%")) %>%
      dplyr::rename(`Pitch Name` = Var1) %>%
      dplyr::select(-prop) 

  })
  
  output$pnext <- renderDataTable({
    
    dataset = datasetInput()
    
    pitcher.matrix = statetable.msm(pitch_name, ID, data = dataset)
    
    transition.matrix = round(t(t(pitcher.matrix) / rep(rowSums(pitcher.matrix), each = ncol(pitcher.matrix))),3)
    x = data.frame(matrix(as.numeric(100*transition.matrix), byrow = F, nrow = ncol(pitcher.matrix)),
                   row.names = dimnames(transition.matrix)[[1]])
    names(x) = row.names(x)
    x
    
    # valueBox(result, subtitle = "Next Pitch Probability")
  })
  
  output$Pitch_Locations <- renderPlot({
    
    
    SZ = data.frame(x = c(-.95,.95,.95,-.95,-.95), z = c(1.6,1.6,3.5,3.5,1.6))
    plate = data.frame(x = c(-0.95, -0.95, 0.95, 0.95, 0, -0.95),
                       z = c(-0.3, 0.1, 0.1, -0.3, -0.6, -0.3))
    LHB = data.frame(x = c(2.5, 2.5, 3, 3, 2.5), z = c(1, 4, 4, 1, 1))
    RHB = data.frame(x = c(-2.5, -2.5, -3, -3, -2.5), z = c(1, 4, 4, 1, 1))
    batter = data.frame(x = c(-2.5, 2.5), y = c(3))
    
    dataset = datasetInput()
    dataset %>%
      dplyr::rename(Pitch = pitch_name) %>%
      group_by(Pitch) %>%
      ggplot(., aes(plate_x, plate_z, col = Pitch)) +
      theme_bw() + facet_wrap(~Pitch) +
      xlab("") + ylab("") +
      theme_void() +
      theme(strip.text = element_text(colour = 1, face = "bold", size = rel(1.2)),
            panel.grid = element_blank(),
            legend.position = "none") + 
      geom_polygon(data = plate, aes(x = x, y = z), col = 1, size = 1.05, fill = "grey") +
      geom_polygon(data = LHB, aes(x = x, y = z), col = "grey", fill = "grey", alpha = 0.5) +
      geom_polygon(data = RHB, aes(x = x, y = z), col = "grey", fill = "grey", alpha = 0.5) +
      geom_point(alpha = 0.3) +
      geom_path(data = SZ, aes(x = x, y = z), col = 1, size = 1.05) 
    
  })
  
  output$pitch_velo <- renderPlot({

    dataset = datasetInput()
    dataset %>%
      dplyr::mutate(Name = paste(player_name, "Pitches", game_year)) %>%
      dplyr::rename(Pitch = pitch_name) %>%
      group_by(Pitch) %>%
      arrange(game_date, at_bat_number, pitch_number) %>%
      dplyr::mutate(counter = row_number(Pitch)) %>%
      ggplot(., aes(counter, release_speed, col = Pitch)) +
      geom_path(alpha = 0.5, size = rel(1.1)) +
      xlab("Number of Pitches Thrown") + ylab("Release Speed (mph)") +
      stat_smooth(method = "loess", se = F, alpha = 1) +
      theme_bw() + facet_wrap(~Name) +
      theme(strip.background = element_rect(fill = "lightblue"),
            strip.text = element_text(colour = 1, face = "bold", size = rel(1.2)),
            legend.position = "top",
            panel.grid = element_blank(),
            legend.title = element_blank())

  })
}


# Run the application 
shinyApp(ui = ui, server = server)
