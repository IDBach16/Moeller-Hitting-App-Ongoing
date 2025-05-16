library(shiny)
library(dplyr)
library(tidyr)

# Load CSV
df <- read.csv("Moeller_2025_Final_Season.csv", stringsAsFactors = FALSE)

# Define swing and PA-ending results
swing_results <- c("Strike Swing and Miss", "Strike In Play", "Strike Foul", "Foul Bunt", "Hit In Play")

pa_results <- c(
  "BB", "Ground Out", "Strike Out", "Fly Out", "Fielders Choice",
  "1B", "2B", "3B", "Error", "Triple", "HR", "HBP", "Line Out", "Sacrfice"
)

ui <- fluidPage(
  titlePanel("0-0 Count: First Pitch Swing Rate by Batter"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("pitch_type", "Select Pitch Type:",
                  choices = c("All", unique(df$PitchType)), selected = "All")
    ),
    
    mainPanel(
      h3("🌀 First Pitch Swing Rate (0-0 Count)"),
      tableOutput("swing_rate_table")
    )
  )
)

server <- function(input, output) {
  output$swing_rate_table <- renderTable({
    # Filter for Moeller batters
    moeller_df <- df %>% filter(BatterTeam == "Moeller")
    
    # Apply pitch type filter
    if (input$pitch_type != "All") {
      moeller_df <- moeller_df %>% filter(PitchType == input$pitch_type)
    }
    
    # Total PAs per batter
    pa_by_batter <- moeller_df %>%
      group_by(Batter) %>%
      summarise(
        total_pas = sum(AtBatResult %in% pa_results, na.rm = TRUE),
        .groups = 'drop'
      )
    
    # First pitch swings (0-0 count)
    first_pitch_swing_by_batter <- moeller_df %>%
      filter(Count == "0 and 0", PitchResult %in% swing_results) %>%
      distinct(Batter, PAofInning) %>%
      group_by(Batter) %>%
      summarise(first_pitch_swing_pas = n(), .groups = 'drop')
    
    # Combine and calculate % swing rate
    swing_rate_by_batter <- pa_by_batter %>%
      left_join(first_pitch_swing_by_batter, by = "Batter") %>%
      mutate(
        first_pitch_swing_pas = replace_na(first_pitch_swing_pas, 0),
        percent_first_pitch_swing = round(100 * first_pitch_swing_pas / total_pas, 1)
      ) %>%
      filter(total_pas > 10) %>%
      arrange(desc(percent_first_pitch_swing))
    
    swing_rate_by_batter
  })
}

shinyApp(ui, server)
