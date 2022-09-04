library(XML)
library(RCurl)
library(rlist)
library(dplyr)
library(stringr)
library(ggplot2)
library(fuzzyjoin)
library(gtExtras)
library(gt)
library(shiny)
library(gtable)
library(purrr)
library(stringdist)  

scrape_old <- function(years) {
  first_year <- min(years)
  theurl <- getURL(paste0("https://www.pro-football-reference.com/years/", first_year, "/fantasy.htm"),.opts = list(ssl.verifypeer = FALSE) )
  tables <- readHTMLTable(theurl)
  tables <- list.clean(tables, fun = is.null, recursive = FALSE)$fantasy
  tables$Season <- first_year
  
  years <- years[years != first_year]
  scrape_df <- data.frame(matrix(nrow = 0, ncol = ncol(tables))) %>% `colnames<-`(names(tables))
  scrape_df <- rbind(scrape_df, tables)
  for (year in years) {
    theurl <- getURL(paste0("https://www.pro-football-reference.com/years/", year, "/fantasy.htm"),.opts = list(ssl.verifypeer = FALSE) )
    tables <- readHTMLTable(theurl)
    tables <- list.clean(tables, fun = is.null, recursive = FALSE)$fantasy
    tables$Season <- year
    scrape_df <- rbind(scrape_df, tables)
  }
  
  return(scrape_df)
}

pull_projections <- function(positions = c('qb', 'rb', 'wr', 'te', 'k', 'dst')) {
  pos <- positions[1]
  theurl <- getURL(paste0("https://www.fantasypros.com/nfl/projections/", pos, ".php"), .opts = list(ssl.verifypeer = FALSE) )
  tables <- readHTMLTable(theurl)
  tables <- list.clean(tables, fun = is.null, recursive = FALSE)$data
  colnames(tables) <- c('PLAYER', 'PASS_ATT', 'PASS_COMP', 'PASS_YDS', 'PASS_TDS', 'PASS_INTS', 'RUSH_ATT', 'RUSH_YRD', 'RUSH_TDS', 'FL', 'FPTS')
  tables$POS <- pos
  positions <- positions[positions != positions[1]]
  
  scrape_df <- data.frame(matrix(nrow = 0, ncol = ncol(tables))) %>% `colnames<-`(names(tables))
  scrape_df <- rbind(scrape_df, tables)
  
  
  for (pos in positions) {
    theurl <- getURL(paste0("https://www.fantasypros.com/nfl/projections/", pos, ".php"), .opts = list(ssl.verifypeer = FALSE) )
    tables <- readHTMLTable(theurl)
    tables <- list.clean(tables, fun = is.null, recursive = FALSE)$data
    if (pos %in% c('wr', 'rb')) {
      colnames(tables) <- c('PLAYER', 'RUSH_ATT', 'RUSH_YRD', 'RUSH_TDS', 'REC', 'REC_YRD', 'REC_TDS', 'FL', 'FPTS')
    }
    if (pos %in% c('te')) {
      colnames(tables) <- c('PLAYER', 'REC', 'REC_YRD', 'REC_TDS', 'FL', 'FPTS')
    }
    if (pos %in% c('dst')) {
      tables <- tables %>% dplyr::rename('PLAYER' = 'Player')
    }
    if (pos %in% c('k')) {
      tables <- tables %>% dplyr::rename('PLAYER' = 'Player')
    }
    tables$POS <- pos
    
    if (!( all(colnames(tables) %in% colnames(scrape_df)) & all(colnames(scrape_df) %in% colnames(tables)) )) {
      not_in_scrape <- setdiff(colnames(tables), colnames(scrape_df))
      not_in_scrape <- data.frame(matrix(nrow = nrow(scrape_df), ncol = length(not_in_scrape))) %>% `colnames<-`(not_in_scrape) 
      not_in_tables <- setdiff(colnames(scrape_df), colnames(tables))
      not_in_tables <- data.frame(matrix(nrow = nrow(tables), ncol = length(not_in_tables))) %>% `colnames<-`(not_in_tables)
      
      tables <- cbind(tables, not_in_tables)
      scrape_df <- cbind(scrape_df, not_in_scrape)
    }
    scrape_df <- rbind(scrape_df, tables)
  }
  
  return(scrape_df)
}

get_closest_names <- function(x) {
  return(in_a_not_b[amatch(x, table = in_a_not_b, maxDist = 4)])
}

get_p_val <- function(x, mean, sd) {
  p <- pnorm( (x -  mean) / sd,  lower.tail = T)
  return(p)
}

get_pick_choices <- function(player_sheet, draft_number, team) {
  next_pick_draft_n <- 10
  needed <- get_team_needs(team, player_sheet)
  player_sheet <- player_sheet %>% filter(is.na(status)) %>% mutate(next_pick_draft_n = draft_number + next_pick_draft_n) %>% 
    mutate(prob_picked_by_next_round = round(get_p_val(next_pick_draft_n, adp, st_dev_adp), digits = 4)) 
  player_sheet <- player_sheet %>% arrange(desc(value)) %>% dplyr::select(-next_pick_draft_n, -st_dev_adp, -pos_rank, -fantasy_pt, -status) %>%
    mutate(needed = as.factor(ifelse(pos %in% needed, 'Yes', 'No'))) %>% group_by(pos) %>% arrange(desc(prob_picked_by_next_round), by.group = T ) %>%
    mutate(estimated_max_value_pos_next_rd = sum((cumprod(dplyr::lag(prob_picked_by_next_round, default = 1)) * (1 - prob_picked_by_next_round)) * value )) %>% 
    ungroup() %>% arrange(desc(value))
  player_sheet <- player_sheet %>%
    mutate(current_value = value - estimated_max_value_pos_next_rd) %>% 
    dplyr::select(id, player, pos, current_value, needed, adp, prob_picked_by_next_round) %>%
    filter(needed == 'Yes') %>%
    arrange(desc(current_value))
  
  ran_val <- range(player_sheet[1:20,]$current_value)
  ret <- player_sheet[1:20,]
  
  return(ret)
  
}



update_player_sheet <- function(player_sheet, player_pick_id, team, round) {
  player_pick_id <- as.numeric(player_pick_id)
  player_sheet <- player_sheet %>% mutate(status = case_when(id == player_pick_id ~ as.character(team),
                                                             TRUE ~ as.character(status))) %>%
    mutate(round = case_when(id == player_pick_id ~ as.numeric(round),
                             TRUE ~ as.numeric(round)))
  
  return(player_sheet)
}

get_current_team <- function(team, player_sheet) {
  current_team_on_sheet <- player_sheet %>% filter(status == team) %>% dplyr::select(player, pos, fantasy_pt, value)
  current_team <- data.frame(matrix(nrow = 0, ncol = 3)) %>% `colnames<-`(c('player', 'pos', 'fantasy_pt'))
  for (position in c('qb', 'rb', 'wr', 'te', 'dst', 'k')) {
    ordered_pos <- current_team_on_sheet %>% filter(pos == position) %>% arrange(desc(fantasy_pt)) %>% `colnames<-`(c('player', 'pos', 'fantasy_pt', 'VOR'))
    
    ## overfull position cases
    
    if (position == 'rb' & nrow(ordered_pos) > 4) {
      n_extra <- nrow(ordered_pos) - 4
      ordered_pos$pos <- c('rb1', 'rb2', 'rb3', 'rb4', paste0('wr_rb', 1:n_extra))
    }
    
    if (position == 'wr' & nrow(ordered_pos) > 4) {
      n_extra <- nrow(ordered_pos) - 4
      ordered_pos$pos <- c('rb1', 'rb2', 'rb3', 'rb4', paste0('wr_rb', 1:n_extra))
    }
    
    
    
    ## normal cases
    
    if (position == 'qb' & nrow(ordered_pos) < 3) {
      missing_slots <- 2 - nrow(ordered_pos)
      missing_slots <- data.frame(matrix(nrow = missing_slots, ncol = 4)) %>% `colnames<-`(c('player', 'pos', 'fantasy_pt', 'VOR'))
      ordered_pos <- rbind(ordered_pos, missing_slots)
      ordered_pos$pos <- c('qb1', 'qb2') 
    }
    if (position == 'rb' & nrow(ordered_pos) < 5) {
      missing_slots <- 4 - nrow(ordered_pos)
      missing_slots <- data.frame(matrix(nrow = missing_slots, ncol = 4)) %>% `colnames<-`(c('player', 'pos', 'fantasy_pt', 'VOR'))
      ordered_pos <- rbind(ordered_pos, missing_slots)
      ordered_pos$pos <- c('rb1', 'rb2', 'rb3', 'rb4')
    }
    if (position == 'wr' & nrow(ordered_pos) < 5) {
      missing_slots <- 4 - nrow(ordered_pos)
      missing_slots <- data.frame(matrix(nrow = missing_slots, ncol = 4)) %>% `colnames<-`(c('player', 'pos', 'fantasy_pt', 'VOR'))
      ordered_pos <- rbind(ordered_pos, missing_slots)
      ordered_pos$pos <- c('wr1', 'wr2', 'wr3', 'wr4')
    }
    if (position == 'te' & nrow(ordered_pos) < 3) {
      missing_slots <- 2 - nrow(ordered_pos)
      missing_slots <- data.frame(matrix(nrow = missing_slots, ncol = 4)) %>% `colnames<-`(c('player', 'pos', 'fantasy_pt', 'VOR'))
      ordered_pos <- rbind(ordered_pos, missing_slots)
      ordered_pos$pos <- c('te1', 'te2')
    }
    if (position == 'dst' & nrow(ordered_pos) < 2) {
      missing_slots <- 1 - nrow(ordered_pos)
      missing_slots <- data.frame(matrix(nrow = missing_slots, ncol = 4)) %>% `colnames<-`(c('player', 'pos', 'fantasy_pt', 'VOR'))
      ordered_pos <- rbind(ordered_pos, missing_slots)
      ordered_pos$pos <- c('dst1')
    }
    if (position == 'k' & nrow(ordered_pos) < 2) {
      missing_slots <- 1 - nrow(ordered_pos)
      missing_slots <- data.frame(matrix(nrow = missing_slots, ncol = 4)) %>% `colnames<-`(c('player', 'pos', 'fantasy_pt', 'VOR'))
      ordered_pos <- rbind(ordered_pos, missing_slots)
      ordered_pos$pos <- c('k1')
    }
    current_team <- rbind(current_team, ordered_pos) 
  }
  if (nrow(current_team) < 18) {
    missing_slots <- 18 - nrow(current_team)
    next_flex_number <- 4 - missing_slots + 1
    missing_slots <- data.frame(matrix(nrow = missing_slots, ncol = 4)) %>% `colnames<-`(c('player', 'pos', 'fantasy_pt', 'VOR'))
    missing_slots$pos <- paste0('wr_rb', next_flex_number:4)
    current_team <- rbind(current_team, missing_slots)
  }
  return(current_team)
}

get_team_needs <- function(team, player_sheet) {
  current_team <- get_current_team(team, player_sheet)
  starting_posit <- c('rb1', 'rb2', 'wr1', 'wr2', 'te1')
  current_team_starters <- current_team %>% filter(pos %in% starting_posit)
  needs <- current_team_starters$pos[is.na(current_team_starters$player)]
  filled_positions <- current_team_starters$pos[!is.na(current_team_starters$player)]
  if (length(needs) == 0) {
    needs <- current_team$pos[is.na(current_team$player)]
  }
  if (length(needs) == 0 & length(filled_positions) %in% c(5, 6)) {
    starting_posit <- c('qb1', 'rb1', 'rb2', 'wr1', 'wr2', 'te1', 'wr3', 'rb3')
    current_team_starters <- current_team %>% filter(pos %in% starting_posit)
    needs <- current_team_starters$pos[is.na(current_team_starters$player)]
  }
  needs <- gsub('[0-9]+', '', needs)
  needs <- unique(unlist(str_split(needs, '_')))
  return(needs)
}

get_final_report <- function(player_sheet) {
  report <- data.frame(matrix(nrow = 0, ncol = 3)) %>% `colnames<-`(c('Team', 'Predict Starter Points', 'Predict Bench Points'))
  for (team in unique(player_sheet$status)) {
    stat_starter <- get_current_team(team, player_sheet) %>% filter(pos %in% c('qb1', 'rb1', 'rb2', 'wr1', 'wr2', 'wr_rb1', 'te1', 'dst', 'k')) 
    stat_bench <- get_current_team(team, player_sheet) %>% filter(!(pos %in% c('qb1', 'rb1', 'rb2', 'wr1', 'wr2', 'wr_rb1', 'te1', 'dst', 'k')))
    starter_predicted_pt <- sum(stat_starter$fantasy_pt)
    stat_bench_predicted_pt <- sum(stat_bench$fantasy_pt)
    add <- data.frame(team = team, starter_pred_pt = starter_predicted_pt, stat_bench_predicted_pt = stat_bench_predicted_pt) %>% 
      `colnames<-`(c('Team', 'Predict Starter Points', 'Predict Bench Points'))
    report <- rbind(report, add)
  }
  return(report)
}

get_player_status <- function(player_sheet, play, team, draft_number) {
  next_pick_draft_n <- 10
  needed <- get_team_needs(team, player_sheet)
  player_sheet <- player_sheet %>% filter(is.na(status)) %>% mutate(next_pick_draft_n = draft_number + next_pick_draft_n) %>% 
    mutate(prob_picked_by_next_round = round(get_p_val(next_pick_draft_n, adp, st_dev_adp), digits = 4)) 
  player_sheet <- player_sheet %>% arrange(desc(value))  %>%
    mutate(needed = as.factor(ifelse(pos %in% needed, 'Yes', 'No'))) %>% group_by(pos) %>% arrange(desc(prob_picked_by_next_round), by.group = T ) %>%
    mutate(estimated_max_value_pos_next_rd = sum((cumprod(dplyr::lag(prob_picked_by_next_round, default = 1)) * (1 - prob_picked_by_next_round)) * value )) %>% 
    ungroup() %>% arrange(desc(value))
  player_sheet <- player_sheet %>%
    mutate(current_value = value - estimated_max_value_pos_next_rd) %>% 
    dplyr::select(id, player, pos, current_value, needed, adp, prob_picked_by_next_round, status) %>%
    filter(grepl(play, player))

  ret <- player_sheet[1:ifelse(nrow(player_sheet) >= 5, 5, nrow(player_sheet)),]
  
  return(ret)
}

projected <- pull_projections()
colnames(projected) <- c('player', 'pass_att', 'pass_comp', 'pass_yrd', 'pass_td', 'pass_int', 'rush_att', 'rush_yrd', 'rush_td', 'fl', 'fantasy_pt', 'pos',
                         'rec', 'rec_yrd', 'rec_td', 'fg', 'fga', 'xpt', 'sack', 'int', 'fr', 'ff', 'tf', 'safety', 'pa', 'yrds_against')
projected <- projected %>% mutate(team = gsub(".*\\s", "", player)) %>% mutate(player = gsub("\\s*\\w*$", "", player)) %>% 
  dplyr::select(-c(fl, xpt, fr, ff, tf, safety, pa, yrds_against, fg, fga, int, sack))

projected <- projected %>% dplyr::select(player, pos, fantasy_pt) %>% 
  mutate(player = ifelse(is.na(word(player, 1, 2)), player, word(player, 1, 2)))


theurl <- getURL(paste0("https://fantasyfootballcalculator.com/adp"),.opts = list(ssl.verifypeer = FALSE) )
projected_pick_number <- readHTMLTable(theurl)
projected_pick_number <- data.frame(projected_pick_number) %>% dplyr::rename('player' = 'NULL.Name', 'st_dev_adp' = 'NULL.Std.Dev', 'adp' = 'NULL.Overall') %>%
  dplyr::select(player,st_dev_adp,adp) %>% mutate(player = ifelse(is.na(word(player, 1, 2)), player, word(player, 1, 2)))  %>%
  mutate(player = str_remove(player,' Defense')) %>%
  mutate(player = str_remove(player, ' Rams')) %>%
  mutate(player = str_remove(player, ' Chargers')) %>%
  mutate(player = str_replace(player, 'LA', "Los Angeles"))


in_a_not_b <- setdiff(projected$player, projected_pick_number$player)
in_b_not_a <- setdiff(projected_pick_number$player, projected$player)

projected_pick_number <- projected_pick_number %>% 
  mutate(player = case_when(player %in% in_b_not_a ~ get_closest_names(player)[1],
                            T ~ player))
projected <- projected %>%
  left_join(projected_pick_number, by = c("player")) %>% mutate(adp = ifelse(is.na(adp), 1000, adp), st_dev_adp = ifelse(is.na(st_dev_adp), 1, st_dev_adp)) 

projected_use <- projected %>% mutate(across(c(fantasy_pt,st_dev_adp, adp), as.numeric)) %>%
  filter(!(is.na(fantasy_pt) | is.na(pos) | pos == '')) %>% arrange(pos, desc(fantasy_pt)) %>%
  group_by(pos) %>% 
  mutate(pos_rank = rank(-fantasy_pt, ties.method = "first")) %>% ungroup() %>% 
  mutate(starter = case_when(pos == 'qb' ~ pos_rank <= 11,
                             pos == 'rb' ~ pos_rank <= 23,
                             pos == 'wr' ~ pos_rank <= 23,
                             pos == 'dst' ~ pos_rank <= 11,
                             pos == 'k' ~ pos_rank <= 11,
                             pos == 'te' ~ pos_rank <= 12)) %>%
  mutate(fantasy_points_starter = case_when(starter == T ~ fantasy_pt)) %>% group_by(pos) %>%
  mutate(mean_starter_pos = min(fantasy_points_starter, na.rm = T), value = fantasy_pt - mean_starter_pos) %>% ungroup() %>%
  dplyr::select(-c(mean_starter_pos, fantasy_points_starter, starter)) %>%
  mutate(status = NA, round = NA) %>% arrange(desc(adp))

projected_use$id <- nrow(projected_use):1

teams <- c('t1', 't2', 't3',' t4', 't5', 't6', 't7', 't8', 't9', 't10')


# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
  conditionalPanel("output$i < 180",
                   titlePanel("Fantasy Drafter"),
                    textOutput("pick_round"),
                    textOutput("team"),
                    numericInput("pickid", "Pick ID:", 1, min = 1, max = 400),
                    actionButton("submit", "Sumbit Pick"),
                    textInput('searchid', label = 'Insert player name', value = "Lamar Jackson"), 
                    actionButton("search", "Search Player"),
                    gt_output("player_search"),
                    gt_output("suggest"),
                    gt_output("current_team")
      ),
  conditionalPanel("output$i > 180",
                   gt_output("final_rep")
  )
)
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  player_sheet <- reactiveValues(data=projected_use)
  i <- reactiveValues(data=1)
  team_id <- reactiveValues(data=1)
  team <- reactiveValues(data = 't1')
  player_search <- reactiveValues(data = get_player_status(projected_use, 'Lamar Jackson', 't1', 1))
  current <- reactiveValues(data = get_current_team('t1', projected_use))
  suggest <- reactiveValues(data = get_pick_choices(projected_use, 1, 't1'))
  final_rep <- reactiveValues(data = NULL)
  snake_driection <- reactiveValues(data = 'up')
  
  observeEvent(input$submit, {
    if (i$data > 180) {
      final_rep$data <- get_final_report(player_sheet$data) %>% gt()
    }
    if (i$data < 180) {
      player_sheet$data <- update_player_sheet(player_sheet$data, input$pickid, team$data)
      if (snake_driection$data == 'up') {
        team_id$data <- team_id$data + 1
      }
      if (snake_driection$data == 'down') {
        team_id$data <- team_id$data - 1
      }
      if (team_id$data == 11) {
        snake_driection$data <- 'down'
        team_id$data = 10
      }
      if (team_id$data == 0) {
        snake_driection$data <- 'up'
        team_id$data = 1
      }
      team$data <- teams[team_id$data]
      i$data <- i$data + 1
      current$data <- get_current_team(team$data, player_sheet$data)
      suggest$data <- get_pick_choices(player_sheet$data, i$data, team$data)
      updateNumericInput(session, "pickid", value = suggest$data$id[suggest$data$current_value == max(suggest$data$current_value)][1] )
      player_search$data <- get_player_status(player_sheet$data, input$searchid, team$data, i$data) 
    }
  })
  
  observeEvent(input$search, {
    player_search$data <- get_player_status(player_sheet$data, input$searchid, team$data, i$data) 
  })
  
  output$player_search <- render_gt(player_search$data %>% gt() %>% data_color(
    columns = c('current_value'),
    colors = scales::col_numeric(
      palette = c("red", "yellow", "green"),
      domain = c(-70, 70)
    )
  ) %>% tab_header(title = paste('Search Player Results')))
  
  output$suggest <- render_gt(suggest$data  %>% gt() %>% data_color(
    columns = c(current_value),
    colors = scales::col_numeric(
      palette = c("red", "yellow", "green"),
      domain = c(-90, 90)
    )
  ) %>% data_color(
    columns = c(needed),
    colors = scales::col_factor(
      palette = c("red", "green"),
      domain = c('No', 'Yes')
    )
  ) %>% tab_header(title = paste(team$data, 'Suggested Picks')))
  output$current_team <- render_gt(current$data  %>% gt()  %>% data_color(
    columns = c('VOR'),
    colors = scales::col_numeric(
      palette = c("red", "yellow", "green"),
      domain = c(min(player_sheet$data$value), max(player_sheet$data$value))
    )
  ) %>% tab_header(title = paste(team$data, 'Current Team')))
  output$team <- reactive({team$data})
  output$pick_round <- reactive({paste('Pick Number:', i$data, '    ', 'Round Number:', ceiling(i$data/10))})
  output$final_rep <- render_gt(final_rep$data)
  output$i <- reactive({i$data})
}

# Run the application 
shinyApp(ui = ui, server = server)
