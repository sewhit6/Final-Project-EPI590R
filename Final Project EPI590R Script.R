## EPI 590R Final Project - Script 

packages_to_install <- c("tidytuesdayR", "gtsummary", "tidyverse", "here", "readr")
#install.packages(packages_to_install)
lapply(packages_to_install, require, character.only=TRUE)

here::here()
# my project directory is "/Users/swhiteside/Final Project EPI590R"
getwd()
# my working directory is "/Users/swhiteside/Final Project EPI590R"
squirrel_data <- read_csv(here("squirrel_data.csv"))

# reading dataset
descriptive_table <- tbl_summary(
  squirrel_data,
  by = "Primary Fur Color",
  include = c("Age", "Shift", "Running", "Chasing", "Climbing", "Foraging"),
  missing_text = "Missing") |> 
  add_p(test = list (all_continuous() ~ "t.test",
                     all_categorical() ~ "chisq.test")) |> 
  add_overall(last=TRUE) |> 
  bold_labels() |> 
  modify_footnote(update = everything() ~ NA) |> 
  modify_header(label = "**Variable**", p.value = "**P**") |> 
  modify_caption(caption = "Table 1. Descriptive Statistics")
descriptive_table 

# Using here to save table as a csv 
descriptive_table_df <- as.data.frame(descriptive_table)
write_csv(descriptive_table_df, here("descriptive_table.csv"))

# specifying inline_text used in the quarto document 
inline_text(descriptive_table, variable="Age", column = "Black", level = "Juvenile") #8 (8%)
inline_text(descriptive_table, variable="Age", column = "Gray", level = "Juvenile") #256 (11%)
inline_text(descriptive_table, variable="Age", column = "Cinnamon", level = "Juvenile") # 58 (15%)
inline_text(descriptive_table, variable="Age", column = "Black", level = "Adult") #92 (92%)
inline_text(descriptive_table, variable="Age", column = "Gray", level = "Adult") #2125 (89%)
inline_text(descriptive_table, variable="Age", column = "Cinnamon", level = "Adult") #326 (85%)

# creating a binary function
binary_func <- function(data, cols) {
  data[cols] <- lapply(data[cols], function(x) ifelse(x, 1, 0))
  data}
squirrel_data <- binary_func(squirrel_data, c("Tail flags", "Tail twitches"))

# specifying levels for age of squirrels
squirrel_data$Age <- factor(
  ifelse(squirrel_data$Age == "Adult", 1,
         ifelse(squirrel_data$Age == "Juvenile", 0, NA)),
  levels = c(0, 1),
  labels = c("Juvenile", "Adult"))

# creating regression table 
table2 <- tbl_uvregression(
  data = squirrel_data,
  y = Age,
  include = c(`Tail flags`, `Tail twitches`),
  method = glm,
  method.args = list(family = binomial()),
  exponentiate = TRUE) |> 
  modify_header(label ~ "**Predictors**") |>
  modify_caption("**Table 2. Binomial Logistic Regression - Tail Flags and Twitches as Predictors of Squirrel Age**")
table2

# creating inline text for regression text
inline_text(table2, variable = "Tail flags") # OR
inline_text(table2, variable = "Tail twitches")

# creating figure (barplot)
barplot(table(squirrel_data$`Primary Fur Color`),
        col = c("black", "firebrick", "gray"),
        main = "Distribution of Primary Fur Color",
        ylab = "Count",
        xlab = "Primary Fur Color")

# Calculating how many squirrels in each primary fur color
table(squirrel_data$`Primary Fur Color`)["Black"]
table(squirrel_data$`Primary Fur Color`)["Cinnamon"]
table(squirrel_data$`Primary Fur Color`)["Gray"]
