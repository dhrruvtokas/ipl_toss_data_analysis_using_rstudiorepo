# Brief: The following code makes use of R and its several in-built packages to extract meaningful information from the ip datasets.
# These datasets also follows the policy of open data.
# NOTE: It might take a couple of seconds to display the shiny application.

# 1. IMPORT THE RQUIRED LIBRARIES
# For Creating Visualizations
library(plotly)
library(ggplot2)
# For creating a Shiny Application
library(shiny)
# For Creating Data Tables
library(DT)
# For using features like %>%
library(dplyr)
# For creating severalgrids in the same section
library(grid)
library(gridExtra)

# 2. LOADING BOTH OF THE REQUIRED DATASETS (Deliveries and matches)
dataset_first <- read.csv(file = '.\\deliveries.csv', header=T)
dataset_second <- read.csv(file = '.\\matches.csv', header=T)

# Displaying both the datasets
View(dataset_first)
View(dataset_second)

# 3. DATA WRANGLING USING R

# Identifying data type errors
sapply(dataset_first, class)
sapply(dataset_second, class)

# Making sure that both the datasets have same column names so they can be utilized
dataset_temp <- dataset_second
colnames(dataset_temp)[1] <- "match_id"
dataset_temp

# Combining both the datasets into a single dataset
dataset_combine <- merge(dataset_first, dataset_temp, by="match_id", all = TRUE)

# 4. DATA ANALYSIS USING R

# Season Games Analysis

# Number of matches played
matches_played <- dataset_second %>% summarise(matches_played_count = n())
matches_played

# Number of seasons played
total_game_seasons <- length(unique(dataset_second$season))
total_game_seasons

# Season with the most matches played
season_max <- dataset_second %>% group_by(season) %>% summarise(match_count = n()) %>%
  filter(match_count == max(match_count))
season_max

# Game Team Analysis

# Which one is the overall best IPL team
best_team <- dataset_second %>% group_by(winner) %>% summarise(winner_count = n()) %>%
  filter(winner_count == max(winner_count))
best_team

# Which team won by scoring the highest amount of runs
team_max_score = dataset_second[which.max(dataset_second$win_by_runs),] 
team_max_score %>% select('winner','win_by_runs')

# Which team scored the lowest amount of runs
team_min_score <- dataset_second %>% filter(win_by_runs != 0) %>% 
  filter(win_by_runs == min(win_by_runs)) %>%
  select('winner', 'win_by_runs')
team_min_score

# Which team won by taking the most of the wickets
team_wickets_most <- dataset_second %>% filter(win_by_wickets == max(win_by_wickets)) %>%
  select('winner', 'win_by_wickets')
team_wickets_most

# Which team won by taking the least number of wickets
team_wickets_least <- dataset_second %>% filter(win_by_wickets != 0) %>% 
  filter(win_by_wickets == min(win_by_wickets)) %>%
  select('winner', 'win_by_wickets')
team_wickets_least

# Game Toss Analysis : determining if a team will won or not based on the fact that they won the toss

# Verifying Number Of Matches Won By The Toss Winners And Their Rivals

table(dataset_second$winner == dataset_second$toss_winner) 

# TRUE: When the total Number of matches were by the Team that won the toss.
# FALSE: When the total Number of matches were won by the Rival Team 

# Determing The Toss Dependency Factor
games_won_by_toss_winner = 0
games_won_by_opposition = 0

for(i in seq(1,nrow(dataset_second)))
{
  if (dataset_second$toss_winner[i] == dataset_second$winner[i])
    games_won_by_toss_winner=games_won_by_toss_winner+1
  
  else 
    games_won_by_opposition=games_won_by_opposition+1
}

if (games_won_by_toss_winner >= games_won_by_opposition)
{
  print(paste("Winning The Toss Was Related With The Match Outcome"))
  print(paste("Number Of Matches: ", nrow(dataset_second), "||", "Number Of Matches Won By The Toss Winning Team: ", games_won_by_toss_winner))
} else
  
{
  print(paste("Winning The Toss Was Not Related With The Match Outcome"))
  print(paste("Number Of Matches: ", nrow(dataset_second), "||", "Number Of Matches Won By The Rival Team: ", games_won_by_opposition)) 
}

# 5. DATA VISUALISATION USING R SHINY
ui <- fluidPage(
  titlePanel("MAIN DASHBOARD - IPL"),
  navbarPage("Visualisation Panel >>",
    tabPanel("Visualisation 1",
    sidebarLayout(
    sidebarPanel(
    sliderInput("plot_a_title_size", "Select A Size For The Title (Plot A))", min = 10, max = 30,
    value = 18),
    sliderInput("plot_a_x_size", "Select A Size For The X-Axis", min = 10, max = 30,
    value = 12),
    sliderInput("plot_a_y_size", "Select A Size For The Y-Axis", min = 10, max = 30,
    value = 11),
    sliderInput("plot_b_title_size", "Select A Size For The Title (Plot B))", min = 10, max = 30,
    value = 18),
    sliderInput("plot_b_x_size", "Select A Size For The X-Axis", min = 10, max = 30,
    value = 12),
    sliderInput("plot_b_y_size", "Select A Size For The Y-Axis", min = 10, max = 30,
    value = 11),
    sliderInput("plot_c_title_size", "Select A Size For The Title (Plot C))", min = 10, max = 30,
    value = 18),
    sliderInput("plot_c_x_size", "Select A Size For The X-Axis", min = 10, max = 30,
    value = 12),
    sliderInput("plot_c_y_size", "Select A Size For The Y-Axis", min = 10, max = 30,
    value = 11),
    
    selectInput(inputId = "plot_a_color_title", label = "Select A Color For The Plot Title (Plot A)", 
    choices = c("deeppink","aquamarine", "blue", "brown", "deepskyblue", "red", "darkorchid1", "orange", "purple", "green", "grey", "black"), selected = "orange", multiple = FALSE),
    selectInput(inputId = "plot_a_color_x", label = "Select A Color For The X-Axis Label (Plot A)", 
                choices = c("darkorchid1", "orange", "purple", "green","deeppink", "aquamarine", "blue", "brown", "deepskyblue", "red", "grey", "black"), selected = "red", multiple = FALSE),
    selectInput(inputId = "plot_a_color_y", label = "Select A Color For The Y-Axis Label (Plot A)", 
                choices = c("deepskyblue", "red", "darkorchid1", "orange", "purple", "deeppink","aquamarine", "blue", "brown", "green", "grey", "black"), selected = "brown", multiple = FALSE),
    
    selectInput(inputId = "plot_b_color_title", label = "Select A Color For The Title (Plot B)", 
                choices = c("deeppink","aquamarine", "blue", "brown", "deepskyblue", "red", "darkorchid1", "orange", "purple", "green", "grey", "black"), selected = "deepskyblue", multiple = FALSE),
    selectInput(inputId = "plot_b_color_x", label = "Select A Color For The X-Axis Label (Plot B)", 
                choices = c("darkorchid1", "orange", "purple", "green","deeppink", "aquamarine", "blue", "brown", "deepskyblue", "red", "grey", "black"), selected = "green", multiple = FALSE),
    selectInput(inputId = "plot_b_color_y", label = "Select A Color For The Y-Axis Label (Plot B)", 
                choices = c("deepskyblue", "red", "darkorchid1", "orange", "purple", "deeppink","aquamarine", "blue", "brown", "green", "grey", "black"), selected = "darkorchid1", multiple = FALSE),
    
    selectInput(inputId = "plot_c_color_title", label = "Select A Color For The Plot Title (Plot C)", 
                choices = c("deeppink","aquamarine", "blue", "brown", "deepskyblue", "red", "darkorchid1", "orange", "purple", "green", "grey", "black"), selected = "deeppink", multiple = FALSE),
    selectInput(inputId = "plot_c_color_x", label = "Select A Color For The X-Axis Label (Plot C)", 
                choices = c("darkorchid1", "orange", "purple", "green","deeppink", "aquamarine", "blue", "brown", "deepskyblue", "red", "grey", "black"), selected = "deepskyblue", multiple = FALSE),
    selectInput(inputId = "plot_c_color_y", label = "Select A Color For The Y-Axis Label (Plot C)", 
                choices = c("deepskyblue", "red", "darkorchid1", "orange", "purple", "deeppink","aquamarine", "blue", "brown", "green", "grey", "black"), selected = "orange", multiple = FALSE)
    
    ), mainPanel( plotlyOutput("plot_a"), plotlyOutput("plot_b"), plotlyOutput("plot_c")))),
    
    tabPanel("Visualisation 2",
    sidebarLayout(
    sidebarPanel(
    sliderInput("plot_d_title_size", "Select A Size For The Title (Plot A))", min = 10, max = 30,
                  value = 18),
    sliderInput("plot_d_x_size", "Select A Size For The X-Axis", min = 10, max = 30,
                  value = 14),
    sliderInput("plot_d_y_size", "Select A Size For The Y-Axis", min = 10, max = 30,
                  value = 14),
    sliderInput("plot_e_title_size", "Select A Size For The Title (Plot B))", min = 10, max = 30,
                  value = 18),
    sliderInput("plot_e_x_size", "Select A Size For The X-Axis", min = 10, max = 30,
                  value = 14),
    sliderInput("plot_e_y_size", "Select A Size For The Y-Axis", min = 10, max = 30,
                  value = 14),
    
    selectInput(inputId = "plot_d_color_title", label = "Select A Color For The Plot Title (Plot A)", 
                choices = c("deeppink","aquamarine", "blue", "brown", "deepskyblue", "red", "darkorchid1", "orange", "purple", "green", "grey", "black"), selected = "orange", multiple = FALSE),
    selectInput(inputId = "plot_d_color_x", label = "Select A Color For The X-Axis Label (Plot A)", 
                choices = c("darkorchid1", "orange", "purple", "green","deeppink", "aquamarine", "blue", "brown", "deepskyblue", "red", "grey", "black"), selected = "green", multiple = FALSE),
    selectInput(inputId = "plot_d_color_y", label = "Select A Color For The Y-Axis Label (Plot A)", 
                choices = c("deepskyblue", "red", "darkorchid1", "orange", "purple", "deeppink","aquamarine", "blue", "brown", "green", "grey", "black"), selected = "red", multiple = FALSE),
    
    selectInput(inputId = "plot_e_color_title", label = "Select A Color For The Title (Plot B)", 
                choices = c("deeppink","aquamarine", "blue", "brown", "deepskyblue", "red", "darkorchid1", "orange", "purple", "green", "grey", "black"), selected = "darkorchid1", multiple = FALSE),
    selectInput(inputId = "plot_e_color_x", label = "Select A Color For The X-Axis Label (Plot B)", 
                choices = c("darkorchid1", "orange", "purple", "green","deeppink", "aquamarine", "blue", "brown", "deepskyblue", "red", "grey", "black"), selected = "brown", multiple = FALSE),
    selectInput(inputId = "plot_e_color_y", label = "Select A Color For The Y-Axis Label (Plot B)", 
                choices = c("deepskyblue", "red", "darkorchid1", "orange", "purple", "deeppink","aquamarine", "blue", "brown", "green", "grey", "black"), selected = "deepskyblue", multiple = FALSE)
    
    ), mainPanel( plotlyOutput("plot_d"), plotlyOutput("plot_e") ))),
    
    tabPanel("Visualisation 3",
    sidebarLayout(
    sidebarPanel(
    sliderInput("scatter_size_a", "Select A Size For All The Data Points (Innings Progression Of All The Batsmen)", min = 0, max = 10,
    value = 5),
    sliderInput("scatter_size_b", "Select A Size For All The Data Points (Innings Progression Of The Top Batsman)", min = 0, max = 10,
    value = 5),
    sliderInput("scatter_alpha_a", "Select The Transparency Level For All The Data Points (Innings Progression Of All The Batsmen)", min = 0.1, max = 1,
    value = 0.25),
    sliderInput("scatter_alpha_b", "Select The Transparency Level For All The Data Points (Innings Progression Of All The Batsmen)", min = 0.1, max = 1,
    value = 0.25),
    
    sliderInput("plot_f_title_size", "Select A Size For The Title (Scatter Plot A))", min = 10, max = 30,
                value = 22),
    sliderInput("plot_f_x_size", "Select A Size For The X-Axis (Scatter Plot A)", min = 10, max = 30,
                value = 18),
    sliderInput("plot_f_y_size", "Select A Size For The Y-Axis (Scatter Plot A)", min = 10, max = 30,
                value = 18),
    sliderInput("plot_g_title_size", "Select A Size For The Title (Scatter Plot B))", min = 10, max = 30,
                value = 22),
    sliderInput("plot_g_x_size", "Select A Size For The X-Axis (Scatter Plot B)", min = 10, max = 30,
                value = 18),
    sliderInput("plot_g_y_size", "Select A Size For The Y-Axis (Scatter Plot B)", min = 10, max = 30,
                value = 18),
    
    selectInput(inputId = "plot_f_color_title", label = "Select A Color For The Plot Title (Scatter Plot A)", 
                choices = c("deeppink","aquamarine", "blue", "brown", "deepskyblue", "red", "darkorchid1", "orange", "purple", "green", "grey", "black"), selected = "orange", multiple = FALSE),
    selectInput(inputId = "plot_f_color_x", label = "Select A Color For The X-Axis Label (Scatter Plot A)", 
                choices = c("darkorchid1", "orange", "purple", "green","deeppink", "aquamarine", "blue", "brown", "deepskyblue", "red", "grey", "black"), selected = "brown", multiple = FALSE),
    selectInput(inputId = "plot_f_color_y", label = "Select A Color For The Y-Axis Label (Scatter Plot A)", 
                choices = c("deepskyblue", "red", "darkorchid1", "orange", "purple", "deeppink","aquamarine", "blue", "brown", "green", "grey", "black"), selected = "deepskyblue", multiple = FALSE),
    
    selectInput(inputId = "plot_g_color_title", label = "Select A Color For The Title (Scatter Plot A)", 
                choices = c("deeppink","aquamarine", "blue", "brown", "deepskyblue", "red", "darkorchid1", "orange", "purple", "green", "grey", "black"), selected = "darkorchid1", multiple = FALSE),
    selectInput(inputId = "plot_g_color_x", label = "Select A Color For The X-Axis Label (Scatter Plot A)", 
                choices = c("darkorchid1", "orange", "purple", "green","deeppink", "aquamarine", "blue", "brown", "deepskyblue", "red", "grey", "black"), selected = "red", multiple = FALSE),
    selectInput(inputId = "plot_g_color_y", label = "Select A Color For The Y-Axis Label (Scatter Plot A)", 
                choices = c("deepskyblue", "red", "darkorchid1", "orange", "purple", "deeppink","aquamarine", "blue", "brown", "green", "grey", "black"), selected = "aquamarine", multiple = FALSE)
    
    ), mainPanel( plotOutput("plot_f"), plotOutput("plot_g") ))),
    
    tabPanel("Visualisation 4",
    sidebarLayout(
    sidebarPanel(
    sliderInput("plot_h_title_size", "Select A Size For The Plot Title", min = 10, max = 30,
    value = 18),
    sliderInput("plot_h_x_size", "Select A Size For The X-Axis", min = 10, max = 30,
    value = 14),
    sliderInput("plot_h_y_size", "Select A Size For The Y-Axis", min = 10, max = 30,
    value = 14),
    
    selectInput(inputId = "plot_h_color_title", label = "Select A Color For The Plot Title (Scatter Plot A)", 
                choices = c("deeppink","aquamarine", "blue", "brown", "deepskyblue", "red", "darkorchid1", "orange", "purple", "green", "grey", "black"), selected = "deeppink", multiple = FALSE),
    selectInput(inputId = "plot_h_color_x", label = "Select A Color For The X-Axis Label (Scatter Plot A)", 
                choices = c("darkorchid1", "orange", "purple", "green","deeppink", "aquamarine", "blue", "brown", "deepskyblue", "red", "grey", "black"), selected = "orange", multiple = FALSE),
    selectInput(inputId = "plot_h_color_y", label = "Select A Color For The Y-Axis Label (Scatter Plot A)", 
                choices = c("deepskyblue", "red", "darkorchid1", "orange", "purple", "deeppink","aquamarine", "blue", "brown", "green", "grey", "black"), selected = "deepskyblue", multiple = FALSE)
    
    ), mainPanel( plotlyOutput("plot_h") ))),
    
    tabPanel("Visualisation 5",
    sidebarLayout(
    sidebarPanel(
    sliderInput("plot_i_title_size", "Select A Size For The Plot Title", min = 10, max = 30,
    value = 18),
    sliderInput("plot_i_x_size", "Select A Size For The X-Axis", min = 10, max = 30,
    value = 14),
    sliderInput("plot_i_y_size", "Select A Size For The Y-Axis", min = 10, max = 30,
    value = 14),
                 
    selectInput(inputId = "plot_i_color_title", label = "Select A Color For The Plot Title (Scatter Plot A)", 
    choices = c("deeppink","aquamarine", "blue", "brown", "deepskyblue", "red", "darkorchid1", "orange", "purple", "green", "grey", "black"), selected = "brown", multiple = FALSE),
    selectInput(inputId = "plot_i_color_x", label = "Select A Color For The X-Axis Label (Scatter Plot A)", 
    choices = c("darkorchid1", "orange", "purple", "green","deeppink", "aquamarine", "blue", "brown", "deepskyblue", "red", "grey", "black"), selected = "green", multiple = FALSE),
    selectInput(inputId = "plot_i_color_y", label = "Select A Color For The Y-Axis Label (Scatter Plot A)", 
    choices = c("deepskyblue", "red", "darkorchid1", "orange", "purple", "deeppink","aquamarine", "blue", "brown", "green", "grey", "black"), selected = "purple", multiple = FALSE)
                 
    ), mainPanel( plotlyOutput("plot_i") ))),
    
    tabPanel("Visualisation 6",
    sidebarLayout(
    sidebarPanel(
    sliderInput("scatter_size_j", "Select A Size For All The Data Points (Innings Progression Of All The Batsmen)", min = 0, max = 10,
    value = 5),
    sliderInput("scatter_size_k", "Select A Size For All The Data Points (Innings Progression Of The Top Batsman)", min = 0, max = 10,
    value = 5),
    sliderInput("scatter_alpha_j", "Select The Transparency Level For All The Data Points (Innings Progression Of All The Batsmen)", min = 0.1, max = 1,
    value = 0.25),
    sliderInput("scatter_alpha_k", "Select The Transparency Level For All The Data Points (Innings Progression Of All The Batsmen)", min = 0.1, max = 1,
    value = 0.25),
    
    sliderInput("plot_j_title_size", "Select A Size For The Title (Scatter Plot A))", min = 10, max = 30,
    value = 20),
    sliderInput("plot_j_x_size", "Select A Size For The X-Axis (Scatter Plot A)", min = 10, max = 30,
    value = 18),
    sliderInput("plot_j_y_size", "Select A Size For The Y-Axis (Scatter Plot A)", min = 10, max = 30,
    value = 18),
    sliderInput("plot_k_title_size", "Select A Size For The Title (Scatter Plot B))", min = 10, max = 30,
    value = 20),
    sliderInput("plot_k_x_size", "Select A Size For The X-Axis (Scatter Plot B)", min = 10, max = 30,
    value = 18),
    sliderInput("plot_k_y_size", "Select A Size For The Y-Axis (Scatter Plot B)", min = 10, max = 30,
    value = 18),
                 
    selectInput(inputId = "plot_j_color_title", label = "Select A Color For The Plot Title (Scatter Plot A)", 
    choices = c("deeppink","aquamarine", "blue", "brown", "deepskyblue", "red", "darkorchid1", "orange", "purple", "green", "grey", "black"), selected = "deeppink", multiple = FALSE),
    selectInput(inputId = "plot_j_color_x", label = "Select A Color For The X-Axis Label (Scatter Plot A)", 
    choices = c("darkorchid1", "orange", "purple", "green","deeppink", "aquamarine", "blue", "brown", "deepskyblue", "red", "grey", "black"), selected = "deepskyblue", multiple = FALSE),
    selectInput(inputId = "plot_j_color_y", label = "Select A Color For The Y-Axis Label (Scatter Plot A)", 
    choices = c("deepskyblue", "red", "darkorchid1", "orange", "purple", "deeppink","aquamarine", "blue", "brown", "green", "grey", "black"), selected = "orange", multiple = FALSE),
                 
    selectInput(inputId = "plot_k_color_title", label = "Select A Color For The Title (Scatter Plot A)", 
    choices = c("deeppink","aquamarine", "blue", "brown", "deepskyblue", "red", "darkorchid1", "orange", "purple", "green", "grey", "black"), selected = "darkorchid1", multiple = FALSE),
    selectInput(inputId = "plot_k_color_x", label = "Select A Color For The X-Axis Label (Scatter Plot A)", 
    choices = c("darkorchid1", "orange", "purple", "green","deeppink", "aquamarine", "blue", "brown", "deepskyblue", "red", "grey", "black"), selected = "grey", multiple = FALSE),
    selectInput(inputId = "plot_k_color_y", label = "Select A Color For The Y-Axis Label (Scatter Plot A)", 
    choices = c("deepskyblue", "red", "darkorchid1", "orange", "purple", "deeppink","aquamarine", "blue", "brown", "green", "grey", "black"), selected = "brown", multiple = FALSE)
    
    ), mainPanel( plotlyOutput("plot_j"), plotlyOutput("plot_k") ))),
    
    tabPanel("Visualisation 7",
    sidebarLayout(
    sidebarPanel(
    selectInput(inputId = "plot_l_color_team_a", label = "Select A Color For The Plot (Mumbai Indians)", 
    choices = c("deeppink","aquamarine", "blue", "brown", "deepskyblue", "red", "darkorchid1", "orange", "purple", "green", "grey", "black"), selected = "deepskypink", multiple = FALSE),
    selectInput(inputId = "plot_l_color_team_b", label = "Select A Color For The Plot (Delhi Daredevils)", 
    choices = c("darkorchid1", "orange", "purple", "green","deeppink", "aquamarine", "blue", "brown", "deepskyblue", "red", "grey", "black"), selected = "orange", multiple = FALSE),
    selectInput(inputId = "plot_l_color_team_c", label = "Select A Color For The Plot (Rajasthan Royals)", 
    choices = c("deepskyblue", "red", "darkorchid1", "orange", "purple", "deeppink","aquamarine", "blue", "brown", "green", "grey", "black"), selected = "deepskyblue", multiple = FALSE),
    selectInput(inputId = "plot_l_color_team_d", label = "Select A Color For The Plot (Chennai Super Kings)", 
    choices = c("darkorchid1", "orange", "purple", "green","deeppink", "aquamarine", "blue", "brown", "deepskyblue", "red", "grey", "black"), selected = "red", multiple = FALSE),
    selectInput(inputId = "plot_l_color_team_e", label = "Select A Color For The Plot (Kings XI Punjab)", 
    choices = c("deepskyblue", "red", "darkorchid1", "orange", "purple", "deeppink","aquamarine", "blue", "brown", "green", "grey", "black"), selected = "darkorchid1", multiple = FALSE),
    selectInput(inputId = "plot_l_color_team_f", label = "Select A Color For The Plot (Kolkata Knight Riders)", 
    choices = c("darkorchid1", "orange", "purple", "green","deeppink", "aquamarine", "blue", "brown", "deepskyblue", "red", "grey", "black"), selected = "grey", multiple = FALSE),
    selectInput(inputId = "plot_l_color_team_g", label = "Select A Color For The Plot (Sunrisers Hyderabad)", 
    choices = c("deepskyblue", "red", "darkorchid1", "orange", "purple", "deeppink","aquamarine", "blue", "brown", "green", "grey", "black"), selected = "brown", multiple = FALSE),
    selectInput(inputId = "plot_l_color_team_h", label = "Select A Color For The Plot (Royal Challengers Bangalore)", 
    choices = c("darkorchid1", "orange", "purple", "green","deeppink", "aquamarine", "blue", "brown", "deepskyblue", "red", "grey", "black"), selected = "aquamarine", multiple = FALSE),
    
    sliderInput("plot_l_lower_limit", "Select A Lower Limit For All The Plots", min = 100, max = 400,
    value = 100),
    
    sliderInput("plot_l_title_size", "Select A Size For The Title Of All The Plots", min = 10, max = 30,
    value = 16),
    sliderInput("plot_l_x_size", "Select A Size For The X-Axis Of All The Plots", min = 10, max = 30,
    value = 13),
    sliderInput("plot_l_y_size", "Select A Size For The Y-Axis Of All The Plots", min = 10, max = 30,
    value = 13),
    
    selectInput(inputId = "plot_l_color_title", label = "Select A Color For The Title (Scatter Plot A)", 
    choices = c("deeppink","aquamarine", "blue", "brown", "deepskyblue", "red", "darkorchid1", "orange", "purple", "green", "grey", "black"), selected = "deepskyblue", multiple = FALSE),
    selectInput(inputId = "plot_l_color_x", label = "Select A Color For The X-Axis Label (Scatter Plot A)", 
    choices = c("darkorchid1", "orange", "purple", "green","deeppink", "aquamarine", "blue", "brown", "deepskyblue", "red", "grey", "black"), selected = "brown", multiple = FALSE),
    selectInput(inputId = "plot_l_color_y", label = "Select A Color For The Y-Axis Label (Scatter Plot A)", 
    choices = c("deepskyblue", "red", "darkorchid1", "orange", "purple", "deeppink","aquamarine", "blue", "brown", "green", "grey", "black"), selected = "darkorchid1", multiple = FALSE)
    
    ), mainPanel( plotOutput("plot_l") ))),
    
    tabPanel("Dataset",
             mainPanel( DTOutput('tbl_a'), DTOutput('tbl_b')))
    )         
  )

server <- function(session, input, output) {

  # Rendering Data Table
  output$tbl_a = renderDT(
    dataset_first, options = list(pageLength = 5)
  )
  
  output$tbl_b = renderDT(
    dataset_second, options = list(pageLength = 5)
  )
  
  #Rendering Plot
  output$plot_a <- renderPlotly({
    
    
    # Analyzing The Total Number Of Matches Played In Each City
    title_size_plot_a <- as.numeric(input$plot_a_title_size) # To select the size of the plot title
    x_size_plot_a <- as.numeric(input$plot_a_x_size) # To select x-axis size
    y_size_plot_a <- as.numeric(input$plot_a_y_size) # To select y-axis size
    
    title_color_plot_a <- as.character(input$plot_a_color_title) # To select title color
    x_color_plot_a <- as.character(input$plot_a_color_x) # To select x-axis label
    y_color_plot_a <- as.character(input$plot_a_color_y) # To select y-axis label
  
    matches_played_city_plot <- ggplot(dataset_second[which(!is.na(dataset_second$city)),],aes(city,fill= city,rm.na=T)) + geom_bar() + theme_minimal() +
      guides(fill = FALSE) + labs(title="Cities vs Number Of Matches Played", x ="City Where The Match Was Held", y = "Number Of Matches Played In Each City") + theme(plot.title = element_text(hjust = 0.5, color=title_color_plot_a, size=title_size_plot_a, face="bold"),axis.title.x = element_text(color=x_color_plot_a, size=x_size_plot_a, face="bold"),axis.title.y = element_text(color=y_color_plot_a, size=y_size_plot_a, face="bold"),axis.text.x = element_text(angle = 90, hjust = 1))
    ggplotly(matches_played_city_plot)
  })
  
  output$plot_b <- renderPlotly({
    
    # Analyzing The Total Number Of Matches Played By Each Team
    title_size_plot_b <- as.numeric(input$plot_b_title_size) # To select the size of the plot title
    x_size_plot_b <- as.numeric(input$plot_b_x_size) # To select x-axis size
    y_size_plot_b <- as.numeric(input$plot_b_y_size) # To select y-axis size
    
    title_color_plot_b <- as.character(input$plot_b_color_title) # To select title color
    x_color_plot_b <- as.character(input$plot_b_color_x) # To select x-axis label
    y_color_plot_b <- as.character(input$plot_b_color_y) # To select y-axis label
    
    matches_played_team_plot <- ggplot(as.data.frame(table(dataset_second$team2) + table(dataset_second$team1)),aes(reorder(Var1,-Freq),Freq,fill = Var1)) + geom_bar(stat = "identity") + guides(fill=FALSE) + theme_minimal() +
      labs(title="Team vs Matches Played", x ="Teams", y = "Matches Played By Each Team") + theme(plot.title = element_text(hjust = 0.5, color=title_color_plot_b, size=title_size_plot_b, face="bold"),axis.title.x = element_text(color=x_color_plot_b, size=x_size_plot_b, face="bold"),axis.title.y = element_text(color=y_color_plot_b, size=y_size_plot_b, face="bold"),axis.text.x = element_text(angle = 90, hjust = 1))
    ggplotly(matches_played_team_plot)
  })
  
  output$plot_c <- renderPlotly({
    
    # Analyzing The Total Number Of Matches In Each Season
    title_size_plot_c <- as.numeric(input$plot_c_title_size) # To select the size of the plot title
    x_size_plot_c <- as.numeric(input$plot_c_x_size) # To select x-axis size
    y_size_plot_c <- as.numeric(input$plot_c_y_size) # To select y-axis size
    
    title_color_plot_c <- as.character(input$plot_c_color_title) # To select title color
    x_color_plot_c <- as.character(input$plot_c_color_x) # To select x-axis label
    y_color_plot_c <- as.character(input$plot_c_color_y) # To select y-axis label
    
    season_match_count_plot = dataset_second %>% group_by(season) %>% summarise(match_count = n()) %>% ggplot() + geom_bar(aes(season,match_count, fill = season), stat = 'identity') + theme_minimal() + labs(title="Season vs Matches Played", x ="Season (Year) [0.5 indicates the middle of the year]", y = "Number Of Matches Played") + theme(plot.title = element_text(hjust = 0.5, color=title_color_plot_c, size=title_size_plot_c, face="bold"),axis.title.x = element_text(color=x_color_plot_c, size=x_size_plot_c, face="bold"),axis.title.y = element_text(color=y_color_plot_c, size=y_size_plot_c, face="bold"))
    ggplotly(season_match_count_plot)  
  }) 
  
  output$plot_d <- renderPlotly({
    
    # Determining The Best Batsman
    title_size_plot_d <- as.numeric(input$plot_d_title_size) # To select the size of the plot title
    x_size_plot_d <- as.numeric(input$plot_d_x_size) # To select x-axis size
    y_size_plot_d <- as.numeric(input$plot_d_y_size) # To select y-axis size
    
    title_color_plot_d <- as.character(input$plot_d_color_title) # To select title color
    x_color_plot_d <- as.character(input$plot_d_color_x) # To select x-axis label
    y_color_plot_d <- as.character(input$plot_d_color_y) # To select y-axis label
    
    top_runs <- 3000
    top_batsman_df <- dataset_first %>% group_by(batsman)%>% summarise(runs=sum(batsman_runs)) %>% arrange(desc(runs)) %>% filter(runs > top_runs) 
    top_batsman_plot <- top_batsman_df %>% ggplot(aes(reorder(batsman,-runs),runs,fill=batsman)) + geom_bar(stat = "identity") + guides(fill = F) + theme_minimal() +
      labs(title="Top Batsman vs Runs Scored By Each Batsman", x ="Top Batsman", y = "Total Runs Scored By The Players") + theme(plot.title = element_text(hjust = 0.5, color=title_color_plot_d, size=title_size_plot_d, face="bold"),axis.title.x = element_text(color=x_color_plot_d, size=x_size_plot_d, face="bold"),axis.title.y = element_text(color=y_color_plot_d, size=y_size_plot_d, face="bold"),axis.text.x = element_text(angle = 90, hjust = 1))
    ggplotly(top_batsman_plot)
  })
  
  output$plot_e <- renderPlotly({
    
    # Determining The Best Bowler
    title_size_plot_e <- as.numeric(input$plot_e_title_size) # To select the size of the plot title
    x_size_plot_e <- as.numeric(input$plot_e_x_size) # To select x-axis size
    y_size_plot_e <- as.numeric(input$plot_e_y_size) # To select y-axis size
    
    title_color_plot_e <- as.character(input$plot_e_color_title) # To select title color
    x_color_plot_e <- as.character(input$plot_e_color_x) # To select x-axis label
    y_color_plot_e <- as.character(input$plot_e_color_y) # To select y-axis label
    
    top_bowler_df <- dataset_first %>% group_by(bowler) %>% filter(player_dismissed!="") %>% summarise(wickets = length(player_dismissed)) %>% top_n(n=10,wt=wickets) 
    top_bowler_plot <- top_bowler_df %>% ggplot(aes(reorder(bowler,-wickets),wickets,fill = bowler))+geom_bar(stat = "identity") + guides(fill = F) + theme_minimal() +
      labs(title="Top Bowler vs Wickets Taken By Each Bowler", x ="Top Bowler", y = "Total Wickets Taken By Each Bowler") + theme(plot.title = element_text(hjust = 0.5, color=title_color_plot_e, size=title_size_plot_e, face="bold"),axis.title.x = element_text(color=x_color_plot_e, size=x_size_plot_e, face="bold"),axis.title.y = element_text(color=y_color_plot_e, size=y_size_plot_e, face="bold"),axis.text.x = element_text(angle = 90, hjust = 1))
    ggplotly(top_bowler_plot)
  })
  
  output$plot_f <- renderPlot({
    
    # Analyzing The Innings Progression Of All The Batsmen
    scatter_plot_size_a <- as.numeric(input$scatter_size_a) # To select data point size
    alpha_scatter_a <- as.numeric(input$scatter_alpha_a) # To select transparency level of all the datapoints
    
    title_size_plot_f <- as.numeric(input$plot_f_title_size) # To select the size of the plot title
    x_size_plot_f <- as.numeric(input$plot_f_x_size) # To select x-axis size
    y_size_plot_f <- as.numeric(input$plot_f_y_size) # To select y-axis size
    
    title_color_plot_f <- as.character(input$plot_f_color_title) # To select title color
    x_color_plot_f <- as.character(input$plot_f_color_x) # To select x-axis label
    y_color_plot_f <- as.character(input$plot_f_color_y) # To select y-axis label
    
    batsman_innings_progression <- dataset_first %>% group_by(match_id) %>% 
      mutate(cum_run= cumsum(batsman_runs),cum_ball=1:length(match_id))
    batsman_innings_progression_plot <-  batsman_innings_progression %>% ggplot(aes(cum_ball,cum_run,col = as.factor(batsman_runs))) + geom_point(alpha = alpha_scatter_a, size = scatter_plot_size_a) + geom_smooth() + coord_cartesian(ylim = c(0, 120)) + theme_classic() +
      labs(title="Innings Progression Of All The Batsmen", x ="Balls", y = "Runs") + theme(plot.title = element_text(hjust = 0.5, color=title_color_plot_f, size = title_size_plot_f, face = "bold"),axis.title.x = element_text(color=x_color_plot_f, size=x_size_plot_f, face="bold"),axis.title.y = element_text(color=y_color_plot_f, size=y_size_plot_f, face="bold"))
    batsman_innings_progression_plot
  })
  
  output$plot_g <- renderPlot({
    
    # Analyzing The Innings Progression Of The Top Batsman
    scatter_plot_size_b <- as.numeric(input$scatter_size_b) # To select data point size
    alpha_scatter_b <- as.numeric(input$scatter_alpha_b) # To select transparency level of all the datapoints
    
    title_size_plot_g <- as.numeric(input$plot_g_title_size) # To select the size of the plot title
    x_size_plot_g <- as.numeric(input$plot_g_x_size) # To select x-axis size
    y_size_plot_g <- as.numeric(input$plot_g_y_size) # To select y-axis size
    
    title_color_plot_g <- as.character(input$plot_g_color_title) # To select title color
    x_color_plot_g <- as.character(input$plot_g_color_x) # To select x-axis label
    y_color_plot_g <- as.character(input$plot_g_color_y) # To select y-axis label
    
    top_batsman_name <- "V Kohli"
    top_batsman_innings_progression <- dataset_first %>% filter(batsman==top_batsman_name) %>% group_by(match_id) %>% 
      mutate(cum_run= cumsum(batsman_runs),cum_ball=1:length(match_id))
    top_batsman_innings_progression_plot <-  top_batsman_innings_progression %>% ggplot(aes(cum_ball,cum_run,col = as.factor(batsman_runs))) + geom_point(alpha = alpha_scatter_b, size = scatter_plot_size_b) + geom_smooth() + coord_cartesian(ylim = c(0, 120)) + theme_classic() +
      labs(title="Top Batsman (V Kohli) - Innings Progression", x ="Balls", y = "Runs") + theme(plot.title = element_text(hjust = 0.5, color=title_color_plot_g, size = title_size_plot_g, face = "bold"),axis.title.x = element_text(color=x_color_plot_g, size=x_size_plot_g, face="bold"),axis.title.y = element_text(color=y_color_plot_g, size=y_size_plot_g, face="bold"))
    top_batsman_innings_progression_plot
  })
  
  output$plot_h <- renderPlotly({
    
    # Determining The Best IPL Team (Based on maximum victories)
    title_size_plot_h <- as.numeric(input$plot_h_title_size) # To select the size of the plot title
    x_size_plot_h <- as.numeric(input$plot_h_x_size) # To select x-axis size
    y_size_plot_h <- as.numeric(input$plot_h_y_size) # To select y-axis size
    
    title_color_plot_h <- as.character(input$plot_h_color_title) # To select title color
    x_color_plot_h <- as.character(input$plot_h_color_x) # To select x-axis label
    y_color_plot_h <- as.character(input$plot_h_color_y) # To select y-axis label
    
    ip_best_team <- dataset_second %>% group_by(winner) %>% summarise(winner_count = n())
    ip_best_team_plot = ggplot(ip_best_team) + geom_bar(aes(winner,winner_count, fill = winner), stat = 'identity') + coord_flip() + theme_minimal() + labs(title="IPL Teams Vs Number Of Wins", x ="Winner", y = "Number Of Victories") + theme(plot.title = element_text(hjust = 0.5, color=title_color_plot_h, size=title_size_plot_h, face="bold"),axis.title.x = element_text(color=x_color_plot_h, size=x_size_plot_h, face="bold"),axis.title.y = element_text(color=y_color_plot_h, size=y_size_plot_h, face="bold"))
    ggplotly(ip_best_team_plot)
  })
  
  output$plot_i <- renderPlotly({
    
    # Determining Whether Winning A Toss Helped A Team Or Not
    title_size_plot_i <- as.numeric(input$plot_i_title_size) # To select the size of the plot title
    x_size_plot_i <- as.numeric(input$plot_i_x_size) # To select x-axis size
    y_size_plot_i <- as.numeric(input$plot_i_y_size) # To select y-axis size
    
    title_color_plot_i <- as.character(input$plot_i_color_title) # To select title color
    x_color_plot_i <- as.character(input$plot_i_color_x) # To select x-axis label
    y_color_plot_i <- as.character(input$plot_i_color_y) # To select y-axis label
    
    toss_winning_count = c(games_won_by_toss_winner,games_won_by_opposition)
    ipl_teams = c("Toss Winners Won The Match" , "Rivals Won The Match")
    toss_win_lose_df = data.frame(ipl_teams,toss_winning_count, stringsAsFactors = F)
    toss_win_loss_plot = ggplot(toss_win_lose_df) + geom_bar(aes(ipl_teams,toss_winning_count, fill = ipl_teams), stat = 'identity') + theme_minimal() + labs(title="Toss Analysis: Toss Winners-Rivals vs Toss Won Count", x ="IPL Teams", y = "Number Of Tosses Won") + theme(plot.title = element_text(hjust = 0.5, color=title_color_plot_i, size=title_size_plot_i, face="bold"),axis.title.x = element_text(color=x_color_plot_i, size=x_size_plot_i, face="bold"),axis.title.y = element_text(color=y_color_plot_i, size=y_size_plot_i, face="bold"))
    ggplotly(toss_win_loss_plot)
  })
  
  output$plot_j <- renderPlotly({
    
    # Analyzing The Total Number Of Matches Won By Teams When They Chose To Bat First vs Runs Won By Teams
    scatter_plot_size_j <- as.numeric(input$scatter_size_j) # To select data point size
    alpha_scatter_j <- as.numeric(input$scatter_alpha_j) # To select transparency level of all the datapoints
    
    title_size_plot_j <- as.numeric(input$plot_j_title_size) # To select the size of the plot title
    x_size_plot_j <- as.numeric(input$plot_j_x_size) # To select x-axis size
    y_size_plot_j <- as.numeric(input$plot_j_y_size) # To select y-axis size
    
    title_color_plot_j <- as.character(input$plot_j_color_title) # To select title color
    x_color_plot_j <- as.character(input$plot_j_color_x) # To select x-axis label
    y_color_plot_j <- as.character(input$plot_j_color_y) # To select y-axis label
    
    matches_won_batting_plot <- ggplot(dataset_second[dataset_second$win_by_runs!=0,],aes(id,win_by_runs,col= winner )) + geom_point(alpha = alpha_scatter_j, size = scatter_plot_size_j) +
      scale_y_continuous(breaks=c(0,30,60,90,120)) +
      geom_hline(yintercept = mean(dataset_second[dataset_second$win_by_runs!=0,]$win_by_runs),col="blue") + theme_classic() +
      labs(title="Matches Won By Teams When They Chose To Bat First vs Runs", x ="Matches Won By Teams", y = "Runs Scored By Teams") + theme(plot.title = element_text(hjust = 0.5, color=title_color_plot_j, size=title_size_plot_j, face="bold"),axis.title.x = element_text(color=x_color_plot_j, size=x_size_plot_j, face="bold"),axis.title.y = element_text(color=y_color_plot_j, size=y_size_plot_j, face="bold"))
    ggplotly(matches_won_batting_plot)
  })
  
  output$plot_k <- renderPlotly({
    
    # Analyzing The Total Number Of Matches Won By Teams When They Chose To Bowl First vs Wickets Taken By Teams
    scatter_plot_size_k <- as.numeric(input$scatter_size_k) # To select data point size
    alpha_scatter_k <- as.numeric(input$scatter_alpha_k) # To select transparency level of all the datapoints
    
    title_size_plot_k <- as.numeric(input$plot_k_title_size) # To select the size of the plot title
    x_size_plot_k <- as.numeric(input$plot_k_x_size) # To select x-axis size
    y_size_plot_k <- as.numeric(input$plot_k_y_size) # To select y-axis size
    
    title_color_plot_k <- as.character(input$plot_k_color_title) # To select title color
    x_color_plot_k <- as.character(input$plot_k_color_x) # To select x-axis label
    y_color_plot_k <- as.character(input$plot_k_color_y) # To select y-axis label
    
    matches_won_bowling_plot <- ggplot(dataset_second[dataset_second$win_by_wickets!=0,],aes(id,win_by_wickets,col= winner )) + geom_point(alpha = alpha_scatter_k, size = scatter_plot_size_k) +
      scale_y_continuous(breaks=c(2,4,6,8,10)) +
      geom_hline(yintercept = mean(dataset_second[dataset_second$win_by_wickets!=0,]$win_by_wickets),col="blue") + theme_classic() +
      labs(title="Matches Won By Teams When They Chose To Bowl First vs Runs", x ="Matches Won By Teams", y = "Wickets Taken By Teams") + theme(plot.title = element_text(hjust = 0.5, color=title_color_plot_k, size=title_size_plot_k, face="bold"),axis.title.x = element_text(color=x_color_plot_k, size=x_size_plot_k, face="bold"),axis.title.y = element_text(color=y_color_plot_k, size=y_size_plot_k, face="bold"))
    ggplotly(matches_won_bowling_plot)
  })
  
  output$plot_l <- renderPlot({
    
    # Determining The Relationship Between The Matches
    team_a_color <- as.character(input$plot_l_color_team_a) # To select the team color
    team_b_color <- as.character(input$plot_l_color_team_b) # To select the team color
    team_c_color <- as.character(input$plot_l_color_team_c) # To select the team color
    team_d_color <- as.character(input$plot_l_color_team_d) # To select the team color
    team_e_color <- as.character(input$plot_l_color_team_e) # To select the team color
    team_f_color <- as.character(input$plot_l_color_team_f) # To select the team color
    team_g_color <- as.character(input$plot_l_color_team_g) # To select the team color
    team_h_color <- as.character(input$plot_l_color_team_h) # To select the team color
    
    lower_limit <- as.numeric(input$plot_l_lower_limit) # To select the lower limit for all the plots
    
    title_size_plot_l <- as.numeric(input$plot_l_title_size) # To select the size of the plot title
    x_size_plot_l <- as.numeric(input$plot_l_x_size) # To select x-axis size
    y_size_plot_l <- as.numeric(input$plot_l_y_size) # To select y-axis size
    
    title_color_plot_l <- as.character(input$plot_l_color_title) # To select title color
    x_color_plot_l <- as.character(input$plot_l_color_x) # To select x-axis label
    y_color_plot_l <- as.character(input$plot_l_color_y) # To select y-axis label
    
    dataset_second_final <- dataset_second 
    dataset_second_final[which(as.character(dataset_second_final$team2)==as.character(dataset_second_final$winner)),"loser"]<- dataset_second_final[which(as.character(dataset_second_final$team2)==as.character(dataset_second_final$winner)),"team1"]
    dataset_second_final[which(as.character(dataset_second_final$team1)==as.character(dataset_second_final$winner)),"loser"]<- dataset_second_final[which(as.character(dataset_second_final$team1)==as.character(dataset_second_final$winner)),"team2"]
    
    game_data <- dataset_second_final[dataset_second_final$win_by_runs!=0,]
    relationship <- function(x,y = team_a_color ){
      matches_win_lose_data <- game_data[game_data$winner==x|game_data$loser==x,]
      matches_win_lose_data[matches_win_lose_data$loser==x,"win_by_runs"] <- -matches_win_lose_data[matches_win_lose_data$loser==x,"win_by_runs"]
      ggplot(matches_win_lose_data,aes(1:nrow(matches_win_lose_data),win_by_runs)) + geom_area(fill = y) + geom_ribbon(aes(ymin=-5, ymax=5),fill="red",alpha=0.4) + geom_ribbon(aes(ymin=-15, ymax=15), fill="cyan", alpha=0.18) +
        guides(fill=FALSE) + scale_alpha(guide = 'none') + coord_cartesian(ylim = c(-lower_limit, lower_limit)) + theme_minimal() + labs(title=x, x ="Matches", y = "Runs") + theme(plot.title = element_text(hjust = 0.5, color=title_color_plot_l, size=title_size_plot_l, face="bold"),axis.title.x = element_text(color=x_color_plot_l, size=x_size_plot_l, face="bold"),axis.title.y = element_text(color=y_color_plot_l, size=y_size_plot_l, face="bold"))}
    
    team_mumbai_indians <- relationship("Mumbai Indians") # Relationship function will be called and the values will be passed into it
    team_delhi_daredevils <- relationship("Delhi Daredevils",team_b_color) # Relationship function will be called and the values will be passed into it
    team_rajasthan_royals <- relationship("Rajasthan Royals",team_c_color) # Relationship function will be called and the values will be passed into it
    team_chennai_super_kings <- relationship("Chennai Super Kings",team_d_color) # Relationship function will be called and the values will be passed into it
    team_kings_xi_punjab <- relationship("Kings XI Punjab",team_e_color) # Relationship function will be called and the values will be passed into it
    team_kolkata_knight_riders <- relationship("Kolkata Knight Riders",team_f_color) # Relationship function will be called and the values will be passed into it
    team_sunrisers_hyderabad <- relationship("Sunrisers Hyderabad",team_g_color) # Relationship function will be called and the values will be passed into it
    team_royal_challengers_bangalore <- relationship("Royal Challengers Bangalore",team_h_color) # Relationship function will be called and the values will be passed into it
    matches_grid <- grid.arrange(team_mumbai_indians, team_delhi_daredevils, team_rajasthan_royals, team_chennai_super_kings, team_kings_xi_punjab, team_kolkata_knight_riders, team_sunrisers_hyderabad, team_royal_challengers_bangalore, ncol=4, top=textGrob("Relationship Between Matches Of Each Team", gp=gpar(fontsize=30,font=30, col="deeppink")) )
    matches_grid
  })
    
  }

# Calling Shiny Function To Deploy The R Code
shinyApp(ui, server)


