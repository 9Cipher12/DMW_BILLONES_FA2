
#1 ======================
install.packages("tidyverse")
library(tidyverse)
library (dplyr)

load("C:/Users/Cipher/Desktop/CRISTEL_DMW/ml_pay.rdata")
# Check the objects in the environment
ls()
ls

#1.1 :  Import
mlb_raw <- as_tibble(ml_pay)
print(mlb_raw)
colnames(mlb_raw)
View(ml_pay)

#	How many rows and columns does the data have?
dim(mlb_raw)

#	Does this match up with the data description given above?
# Check the column names of the dataset
column_names <- names(mlb_raw)
print(column_names)

#1.2 : Tidy
# Create mlb_aggregate tibble
library(dplyr)

mlb_aggregate <- mlb_raw %>%
  select(`Team.name.2014`, payroll, matches("^X\\d{4}\\.pct$")) %>%
  mutate(payroll_aggregate = payroll, 
         pct_wins_aggregate = rowSums(across(matches("^X\\d{4}\\.pct$")))) %>%
  select(-payroll, -matches("^X\\d{4}\\.pct$")) %>%
  rename(team = `Team.name.2014`)

# Create mlb_total with columns team, payroll_aggregate, pct_wins_aggregate
mlb_total <- mlb_aggregate %>%
  select(team, payroll_aggregate, pct_wins_aggregate)

print(mlb_total)

mlb_yearly <- mlb_raw %>%
  pivot_longer(cols = starts_with("p"), names_to = "year", values_to = "pct_wins") %>%
  mutate(year = as.integer(gsub("\\D", "", year))) %>%
  select(`Team.name.2014`, year, pct_wins) %>%
  left_join(select(mlb_raw, `Team.name.2014`, payroll, avgwin), by = "Team.name.2014") %>%
  arrange(`Team.name.2014`, year)


print(mlb_aggregate)
print(mlb_yearly)
#Check number of rows
nrow(mlb_aggregate)
nrow(mlb_yearly)

#1.3
library(dplyr)

mlb_aggregate_computed <- mlb_total %>%
  group_by(team) %>%
  summarise(
    payroll_aggregate_computed = sum(payroll_aggregate),
    pct_wins_aggregate_computed = sum(pct_wins_aggregate)
  )

# Print mlb_aggregate_computed to check the result
print(mlb_aggregate_computed)

#__________________________________________________________

# Join mlb_aggregate and mlb_aggregate_computed
mlb_aggregate_joined <- inner_join(mlb_aggregate, mlb_aggregate_computed, by = "team")

# Print mlb_aggregate_joined to check the result
print(mlb_aggregate_joined)

#Scatter Plots
install.packages("gridExtra")
library(gridExtra)
install.packages("ggplot2")
library(ggplot2)

payroll_scatter_plot <- ggplot(mlb_aggregate_joined, aes(x = payroll_aggregate, y = payroll_aggregate_computed)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(x = "Payroll Aggregate", y = "Computed Payroll Aggregate", title = "Payroll Aggregate vs. Computed Payroll Aggregate")

# Scatter plot for pct_wins_aggregate
pct_wins_plot <- ggplot(mlb_aggregate_joined, aes(x = pct_wins_aggregate, y = pct_wins_aggregate_computed)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(x = "Percentage Wins Aggregate", y = "Computed Percentage Wins Aggregate", title = "Percentage Wins Aggregate vs. Computed Percentage Wins Aggregate")

# Arrange plots side by side
combined_plots <- grid.arrange(payroll_scatter_plot, pct_wins_plot, ncol = 2)

# Display the combined plots
print(combined_plots)


#2

#2.1
library(ggplot2)

#2.1.1
# Convert Team.name.2014 to factor for correct ordering in facet_wrap
mlb_yearly$Team.name.2014 <- factor(mlb_yearly$Team.name.2014, levels = unique(mlb_yearly$Team.name.2014))

# Plot payroll as a function of year, faceting by team
payroll_plot <- ggplot(mlb_yearly, aes(x = year, y = payroll)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Team.name.2014, scales = "free_y") +
  labs(x = "Year", y = "Payroll", title = "Payroll as a Function of Year for Each Team")

# Add a red dashed horizontal line for the mean payroll across years for each team
payroll_plot <- payroll_plot +
  geom_hline(aes(yintercept = mean(payroll), color = "Mean Payroll"), linetype = "dashed") +
  scale_color_manual(values = "red", guide = FALSE)

# Display the plot
print(payroll_plot)

#2.1.2
library(dplyr)

# Identify the three teams with the greatest payroll_aggregate_computed
top_three_teams <- mlb_aggregate_computed %>%
  arrange(desc(payroll_aggregate_computed)) %>%
  head(3)

# Print a table of the top three teams and their payroll_aggregate_computed
print(top_three_teams)

#2.1.3
library(dplyr)

# Calculate payroll figures for 1998 and 2014
mlb_payroll_1998_2014 <- mlb_raw %>%
  select(Team.name.2014, p1998, p2014) %>%
  rename(team = Team.name.2014, payroll_1998 = p1998, payroll_2014 = p2014)

# Calculate pct_increase
mlb_payroll_1998_2014 <- mlb_payroll_1998_2014 %>%
  mutate(pct_increase = ((payroll_2014 - payroll_1998) / payroll_1998) * 100)

# Identify the three teams with the greatest percentage increase in payroll
top_three_increase <- mlb_payroll_1998_2014 %>%
  arrange(desc(pct_increase)) %>%
  head(3)

# Print a table of the top three teams with their payroll figures from 1998 and 2014, and pct_increase
print(top_three_increase)

#2.1.4


#2.2.1
pct_wins_plot <- ggplot(mlb_yearly, aes(x = year, y = pct_wins)) +
  geom_point() +                    # Add points
  geom_hline(aes(yintercept = mean(pct_wins), color = "Average"), linetype = "dashed") +  # Add average line
  facet_wrap(~ Team.name.2014, scales = "free_y") +  # Facet by team
  labs(x = "Year", y = "Percentage Wins", title = "Percentage Wins vs. Year") +  # Labels
  theme_bw() +                      # White background theme
  theme(legend.position = "bottom") # Legend position

# Display the plot
print(pct_wins_plot)
#2.2.2
library(dplyr)

# Three teams with the greatest pct_wins_aggregate_computed
top_three_teams <- mlb_aggregate_computed %>%
  arrange(desc(pct_wins_aggregate_computed)) %>%
  slice_head(n = 3)

print(top_three_teams)
#2.2.3
library(dplyr)


# Calculate the standard deviation of pct_wins for each team
team_pct_wins_sd <- mlb_yearly %>%
  group_by(Team.name.2014) %>%
  summarise(pct_wins_sd = sd(pct_wins, na.rm = TRUE)) %>%
  ungroup()

# Identify the three teams with the most erratic pct_wins across years
top_three_erratic_teams <- team_pct_wins_sd %>%
  arrange(desc(pct_wins_sd)) %>%
  slice_head(n = 3)

# Print a table of these teams along with pct_wins_sd
print(top_three_erratic_teams)

#2.3
library(ggplot2)
install.packages("ggrepel")
library(ggrepel)

# Create scatter plot with labels
scatter_plot <- ggplot(mlb_aggregate, aes(x = payroll_aggregate, y = pct_wins_aggregate, label = team)) +
  geom_point() +  # Scatter plot
  geom_text_repel(size = 3, box.padding = unit(0.5, "lines")) +  # Add labels with ggrepel
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add least squares line
  labs(x = "Payroll Aggregate", y = "Percentage Wins Aggregate", title = "Percentage Wins vs. Payroll") +
  theme_minimal()  # Optional: Customize the theme if needed

# Print the scatter plot
print(scatter_plot)

#2.4
library(dplyr)

# Assuming you have a tibble named mlb_aggregate_computed containing columns:
# team, pct_wins_aggregate_computed, and payroll_aggregate_computed

# Calculate efficiency (wins per unit of payroll)
mlb_efficiency <- mlb_aggregate_computed %>%
  mutate(efficiency = pct_wins_aggregate_computed / payroll_aggregate_computed)

# Identify the top three teams with the greatest efficiency
top_three_efficiency <- mlb_efficiency %>%
  top_n(3, efficiency) %>%
  arrange(desc(efficiency))  # Arrange in descending order of efficiency

# Print a table of the top three teams along with their efficiency,
# pct_wins_aggregate_computed, and payroll_aggregate_computed
print(top_three_efficiency)



