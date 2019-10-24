library(rvest)

# Player Stats

#Create holding dfs
master_links <- data.frame()
players2020 <- data.frame()
stats <- data.frame()

# Loop thru alphabet (manually add 'x' if any X names join)
for (letter in c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'y', 'z')){
  
  url <- sprintf("https://www.basketball-reference.com/players/%s/", letter)
  webpage <- read_html(url)
  
  new_players <- webpage %>%
    html_nodes("table") %>%
    html_table(fill = TRUE)
  
  new_players <- data.frame(new_players)
  
  players2020 <- rbind(players2020, new_players)
  
  letter_links <- webpage %>%
    html_nodes("table") %>%
    html_nodes("tr") %>%
    html_nodes("a") %>%
    html_attr("href")
  
  save <- grep(".html", letter_links)
  letter_links <- letter_links[save]
  letter_links <- data.frame(letter_links, stringsAsFactors = F)
  letter_links$To <- new_players$To
  master_links <- rbind(master_links, letter_links)
  
}

master_links$letter_links <- substr(master_links$letter_links, 1, nchar(master_links$letter_links) - 5)

links2020 <- as.list(master_links[master_links$To == 2020, 1])
players2020 <- as.list(players2020[players2020$To == 2020, 1])

# Loop thru players
for (link in links2020){
  OGLink <- paste("https://www.basketball-reference.com", link, "/gamelog/2020", sep = "")
  
  OGA <- read_html(OGLink)
  tbls <- html_nodes(OGA, "table")
  
  # Scrape game log
  gamelog <- OGA %>%
    html_nodes("table") %>%
    .[8] %>%
    html_table(fill = TRUE)
  
  # Add to dataframe
  stats <- rbind(stats, as.data.frame(gamelog))
}

# Organize data
stats$Player <- players2020
stats$H.A <- 'A'
stats[which(is.na(stats$Var.6)), 'H.A'] <- 'H'
stats <- stats[, -6]
stats <- stats[, c(30, 1:6, 31, 7:29)]
colnames(stats)[31] <- 'Diff'

##Team Stats
team_links <- webpage %>%
  html_nodes(".division") %>%
  html_nodes("a") %>%
  html_attr("href")
