#Krishna Soni
#Introduction to Analytics
#2/8/25

# Load Required Libraries
library(dplyr)    # For data manipulation
library(ggplot2)  # For data visualization

# Step 1: Read the Dataset
# Read in the ball dataset from the specified file path
ball_data <- read.csv("C:/Users/krish/OneDrive/Desktop/Project 5/ball-dataset.csv")\

# Step 2: Create Frequency Tables
# Create a frequency table for the colors of the balls
freq_color <- ball_data %>%
  group_by(color) %>%          # Group data by 'color'
  summarise(counts = n()) %>%  # Count occurrences of each color
  ungroup()                    # Ungroup the data frame

ls()  # Lists all objects in the current environment

# Create a frequency table for the labels of the balls
freq_label <- ball_data %>%
  group_by(label) %>%         # Group data by 'label'
  summarise(counts = n()) %>%  # Count occurrences of each label
  ungroup()                   # Ungroup the data frame

ls()  # Lists all objects in the current environment

# Display Frequency Tables
print(freq_color)  # Print counts of each color
print(freq_label)  # Print counts of each label

# Step 3: Create Bar Charts
# Bar chart for the counts of ball colors
ggplot(freq_color, aes(x=color, y=counts, fill=color)) +
  geom_bar(stat="identity") +                                # Create a bar chart
  labs(title="Counts of Different Ball Colors", x="Color", y="Counts") +  # Add title and axis labels
  theme_minimal()                                           # Apply a minimal theme for better visualization

# Bar chart for the counts of ball labels
ggplot(freq_label, aes(x=label, y=counts, fill=label)) +
  geom_bar(stat="identity") +                                # Create a bar chart
  labs(title="Counts of Different Ball Labels", x="Label", y="Counts") +  # Add title and axis labels
  theme_minimal()                                           # Apply a minimal theme for better visualization

# Step 4: Calculate Probabilities
# Probability of drawing a green ball
# P(Green) = Number of green balls / Total number of balls
prob6_result <- freq_color$counts[freq_color$color == "green"] / sum(freq_color$counts)

# Probability of drawing a blue or a red ball
# P(Blue or Red) = (Number of blue balls + Number of red balls) / Total number of balls
prob7_result <- (freq_color$counts[freq_color$color == "blue"] +
                   freq_color$counts[freq_color$color == "red"]) / sum(freq_color$counts)

# Probability of drawing a ball with an A or C label
# P(A or C) = (Number of A labels + Number of C labels) / Total number of labels
prob8_result <- (freq_label$counts[freq_label$label == "A"] +
                   freq_label$counts[freq_label$label == "C"]) / sum(freq_label$counts)
# Load necessary libraries
library(dplyr)

# Probability of drawing a yellow ball with the label D
# Calculate the probability of drawing a yellow ball with label "D"
prob9_result <- sum(ball_data$color == "yellow" & ball_data$label == "D") / nrow(ball_data)

# Probability of drawing a blue ball followed by a red ball without replacement
# P(Blue then Red) = P(Blue) * P(Red | Blue)
prob11_result <- (freq_color$counts[freq_color$color == "blue"] / sum(freq_color$counts)) * 
  (freq_color$counts[freq_color$color == "red"] / (sum(freq_color$counts) - 1))

# Probability of drawing four green balls in a row without replacement
# Assuming prob_color is defined
prob12_result <- prod((freq_color$counts[freq_color$color == "green"] - (0:3)) / 
                        (sum(freq_color$counts) - (0:3)))


# Probability of drawing a red ball followed by a B label ball without replacement
# P(Red then B) = P(Red) * P(B | Red)
prob13_result <- (freq_color$counts[freq_color$color == "red"] / sum(freq_color$counts)) *
  (freq_label$counts[freq_label$label == "B"] / (sum(freq_color$counts) - 1))

# Probability of drawing a yellow ball or a ball with a D label
prob10_result <- (sum(ball_data$color == "yellow") + sum(ball_data$label == "D")) / nrow(ball_data)

#Print all calculated probabilities
cat("Probability of drawing a green ball:", prob6_result, "\n")
cat("Probability of drawing either a blue or red ball:", prob7_result, "\n")
cat("Probability of drawing a ball with A or C label:", prob8_result, "\n")
cat("Probability of drawing a yellow ball with D label:", prob9_result, "\n")
cat("Probability of drawing either a yellow ball or a ball with D label:", prob10_result, "\n")
cat("Probability of drawing a blue ball then a red ball:", prob11_result, "\n")
cat("Probability of drawing four green balls in a row:", prob12_result, "\n")
cat("Probability of drawing a red ball followed by a ball with B label:", prob13_result, "\n")

# Load required library
library(ggplot2)

# Step 1: Create outcomes Data Frame for flipping a biased coin four times
coin_outcomes <- expand.grid(first = c("H", "T"),
                             second = c("H", "T"),
                             third = c("H", "T"),
                             fourth = c("H", "T"))

# Step 2: Calculate probabilities assuming P(H) = 0.6 and P(T) = 0.4
# Assuming a coin with P(H) = 0.6, calculate probability of 3 heads
prob18_result <- choose(4, 3) * (0.6)^3 * (0.4)^1  # P(3H, 1T)


# Step 3: Calculate probabilities of getting a certain number of heads
num_heads_prob <- table(rowSums(coin_outcomes == "H")) / nrow(coin_outcomes)

# Step 4: Convert the table to a data frame for plotting
probability_data <- data.frame(num_heads = as.numeric(names(num_heads_prob)), 
                               probability = as.numeric(num_heads_prob))

# Check the contents of the probability_data
print(probability_data)

# Step 5: Create a bar chart to visualize the probabilities
ggplot(probability_data, aes(x = num_heads, y = probability)) +
  geom_bar(stat = "identity") +                                 # Use bar heights corresponding to probability
  labs(title = "Probability of Number of Heads in 4 Flips", 
       x = "Number of Heads", 
       y = "Probability") + 
  theme_minimal()  # Use a minimal theme for better aesthetics

# Optional Challenge (Soccer Games)
# Define winning probabilities for soccer games
p_home_win <- 0.75  # Probability of winning a game at home
p_away_win <- 0.50  # Probability of winning a game away

# Calculate probability of winning exactly 10 home games
prob22_result <- dbinom(10, size = 10, prob = p_home_win)

# Calculate probability of winning more than one game (1 - P(0 or 1 wins))
prob23_result <- 1 - pbinom(1, size=10, prob = p_home_win)

# Calculate the number of ways to win exactly 3 home games out of 5
# This will give the combinations of choosing 3 wins from 5 home games
prob24_result <- choose(5, 3)  # Which is 10


# Print soccer game probabilities
cat("Probability of winning exactly 10 home games:", prob22_result, "\n")
cat("Probability of winning more than one game:", prob23_result, "\n")
cat("Different ways to pick 3 home games out of 5:", prob24_result, "\n")


install.packages("testthat") # do this line
library(testthat)
test_file("project5_tests.R")

