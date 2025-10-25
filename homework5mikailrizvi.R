# Analyzes player statistics from Houston Dash, Gotham FC, and Orlando Pride
# https://fbref.com/en/

# Load required libraries
library(ggplot2)

# Read in the three team datasets, skip the first two lines of the sheets since they are useless.
houston <- read.csv("HoustonDashSheet1.csv", skip = 2)
gotham <- read.csv("GothamFCSheet1.csv", skip = 2)
orlando <- read.csv("OrlandoPrideSheet1.csv", skip = 2)

# Add team names to each dataset
houston$Team <- "Houston Dash"
gotham$Team <- "Gotham FC"
orlando$Team <- "Orlando Pride"

# Combine all three datasets into one
all_players <- rbind(houston, gotham, orlando)

# Clean the data: remove rows with missing player names and squad totals
all_players <- all_players[!is.na(all_players$Player) & all_players$Player != "" & all_players$Player != "Squad Total" & all_players$Player != "Opponent Total", ]

# Extract numeric age from the Age column, use the sub function so that I could get rid of the -xxx for the ages since it wasn't needed
all_players$Age_Numeric <- as.numeric(sub("-.*", "", all_players$Age))

# Convert goals and minutes to numeric, used global sub this time so that for all values in this row I could get rid of the comma. I used sub and gsub a few times during this
all_players$Gls <- as.numeric(all_players$Gls)
all_players$Min <- as.numeric(gsub(",", "", all_players$Min))

# Remove players with missing age or minutes data
all_players <- all_players[!is.na(all_players$Age_Numeric) & !is.na(all_players$Min) & all_players$Min > 0, ]

# ===== RESEARCH QUESTION 1: Distribution of Player Ages =====
ggplot(all_players, aes(x = Age_Numeric)) + geom_histogram(binwidth = 2, fill = "blue", color = "black") + labs(title = "Distribution of Ages for NWSL Players", subtitle = "Houston Dash, Gotham FC, and Orlando Pride (2024 Season)", x = "Age (Years)", y = "Number of Players") + theme_minimal() + theme(plot.title = element_text(face = "bold", size = 14))

# ===== RESEARCH QUESTION 2: Minutes Played vs Goals Scored =====
# Select one random player who scored at least one goal
players_with_goals <- all_players[all_players$Gls > 0, ]
random_player <- players_with_goals[sample(nrow(players_with_goals), 1), ]

# Get the player name from the first column
player_name <- as.character(random_player[1, 1])

# Create indicator for which player to highlight
all_players$is_highlighted <- as.character(all_players[, 1]) == player_name

# Get the highlighted player data
highlighted_data <- all_players[all_players$is_highlighted, ]
not_highlighted_data <- all_players[!all_players$is_highlighted, ]

# Create scatterplot with highlighted player
ggplot(all_players, aes(x = Min, y = Gls)) + geom_point(data = not_highlighted_data, color = "blue", size = 2, alpha = 0.6) + geom_point(data = highlighted_data, color = "red", size = 4) + annotate("text", x = highlighted_data$Min, y = highlighted_data$Gls, label = player_name, vjust = -1, hjust = 0.5, size = 4, fontface = "bold", color = "red") + labs(title = "Relationship Between Minutes Played and Goals Scored", subtitle = "NWSL Players from Houston Dash, Gotham FC, and Orlando Pride (2024 Season)", x = "Minutes Played", y = "Goals Scored") + theme_minimal() + theme(plot.title = element_text(face = "bold", size = 14))

#CITING SOURCES: I had some help from claude to explain why the dashes werent working for age, and learned how to implement sub and gsub
#Along with this, I ran into an error that says NAS introduced by coercion. This was because some of my values couldnt be converted into numbers. Thats why I used asnumeric. 

