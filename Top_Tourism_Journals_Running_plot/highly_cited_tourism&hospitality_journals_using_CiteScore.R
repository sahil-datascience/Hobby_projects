
#########################
#### Create a Running plot of Tourism and Hospitality Journals from Elsevier Scopus 
#### source list and based on Cite Score metric
#########################

# Load the necessary libraries
library(tidyverse)
library(readxl)

# Load the data
# These sets needs to be in your working directory

data_2013 <- read_xlsx("Scopus Source List/2013.xlsx") %>% 
        select(`Source title`, `CiteScore`) %>%
        rename(Journal = `Source title`, CiteScore_2013 = `CiteScore`)

data_2014 <- read_xlsx("Scopus Source List/2014.xlsx") %>% 
        select(`Source title`, `CiteScore`) %>%
        rename(Journal = `Source title`, CiteScore_2014 = `CiteScore`)

data_2015 <- read_xlsx("Scopus Source List/2015.xlsx") %>% 
        select(`Source title`, `CiteScore`) %>%
        rename(Journal = `Source title`, CiteScore_2015 = `CiteScore`)

data_2016 <- read_xlsx("Scopus Source List/2016.xlsx") %>% 
        select(`Source title`, `CiteScore`) %>%
        rename(Journal = `Source title`, CiteScore_2016 = `CiteScore`)

data_2017 <- read_xlsx("Scopus Source List/2017.xlsx") %>% 
        select(`Source title`, `CiteScore`) %>%
        rename(Journal = `Source title`, CiteScore_2017 = `CiteScore`)

data_2018 <- read_xlsx("Scopus Source List/2018.xlsx") %>% 
        select(`Source title`, `CiteScore`) %>%
        rename(Journal = `Source title`, CiteScore_2018 = `CiteScore`)

data_2019 <- read_xlsx("Scopus Source List/2019.xlsx") %>% 
        select(`Source title`, `CiteScore`) %>%
        rename(Journal = `Source title`, CiteScore_2019 = `CiteScore`)

data_2020 <- read_xlsx("Scopus Source List/2020.xlsx") %>% 
        select(`Source title`, `CiteScore`) %>%
        rename(Journal = `Source title`, CiteScore_2020 = `CiteScore`)

data_2021 <- read_xlsx("Scopus Source List/2021.xlsx") %>% 
        select(`Source title`, `CiteScore`) %>%
        rename(Journal = `Source title`, CiteScore_2021 = `CiteScore`)

data_2022 <- read_xlsx("Scopus Source List/2022.xlsx") %>% 
        select(`Source title`, `CiteScore`) %>%
        rename(Journal = `Source title`, CiteScore_2022 = `CiteScore`)

data_2023 <- read_xlsx("Scopus Source List/2023.xlsx") %>% 
        select(`Source title`, `CiteScore`) %>%
        rename(Journal = `Source title`, CiteScore_2023 = `CiteScore`)

# Merge the data
data <- data_2013 %>% 
        full_join(data_2014, by = "Journal") %>%
        full_join(data_2015, by = "Journal") %>%
        full_join(data_2016, by = "Journal") %>%
        full_join(data_2017, by = "Journal") %>%
        full_join(data_2018, by = "Journal") %>%
        full_join(data_2019, by = "Journal") %>%
        full_join(data_2020, by = "Journal") %>%
        full_join(data_2021, by = "Journal") %>%
        full_join(data_2022, by = "Journal") %>%
        full_join(data_2023, by = "Journal")

#### Make the Running Bar Chart ####

# Load necessary libraries
library(gganimate)

# Check the structure of the data
glimpse(data)

# Convert Cite Score columns to numeric
data <- data %>%
        mutate(across(starts_with("CiteScore_"), as.numeric))

# Verify the class of the variables
glimpse(data)

# Reshape the data from wide to long format
data_long <- data %>%
        pivot_longer(
                cols = starts_with("CiteScore_"),
                names_to = "Year",
                names_prefix = "CiteScore_",
                values_to = "CiteScore"
        )

# Convert Year to integer
data_long <- data_long %>%
        mutate(Year = as.integer(Year))

# Create the running race bar chart with rank on Y-axis and Cite Score on X-axis
# Filter for top 15 journals per year
top_journals_filtered <- data_long %>%
        group_by(Year) %>%
        top_n(15, wt = CiteScore) %>%
        ungroup() %>%
        arrange(Year, desc(CiteScore)) %>%
        group_by(Year) %>%
        mutate(Rank = row_number(),
               CiteScoreLabel = paste0(" ", round(CiteScore, 2)))


# Create the race bar chart
p <- ggplot(top_journals_filtered, aes(x = Rank, y = CiteScore, fill = Journal)) +
        geom_bar(stat = "identity") +
        geom_text(aes(label = CiteScoreLabel), position = position_stack(vjust = 0.5), size = 5) +
        geom_text(aes(y = CiteScore, label = Journal), hjust = -0.1, size = 5) +
        scale_x_reverse(breaks = 1:15) +
        coord_flip() +
        scale_y_continuous(limits = c(0,40), expand = expansion(mult = c(0.05, 0.15))) +
        labs(title = 'Top Highly Cited Tourism & Hospitality Research Journals: {closest_state}', 
             x = 'Rank', 
             y = 'CiteScore') +
        theme_minimal() +
        theme(legend.position = "none",
              plot.title = element_text(size = 35, face = "bold"),
              axis.title.x = element_text(size = 18),
              axis.title.y = element_text(size = 18),
              axis.text.x = element_text(size = 14),
              axis.text.y = element_text(size = 14),
              plot.margin = margin(20, 20, 20, 20)  # Add margins (top, right, bottom, left)
              ) +
        transition_states(Year, transition_length = 20, state_length = 5) + # Transition states
        ease_aes('linear') +
        enter_fade() + 
        exit_fade() +
        annotate("text", x = 14, y = 40 * 0.8, label = "Data Source: CiteScore from Scopus", size = 5, hjust = 0) +
        annotate("text", x = 15, y = 40 * 0.8, label = "© Sahil Sharma", size = 5, hjust = 0) + 
        geom_label(aes(x = 8, y = 40 * 0.9, label = as.character(Year)), 
                   size = 8, color = "white", fill = "blue", fontface = "bold", alpha = 0.05, label.padding = unit(0.5, "lines"))

# Render the animation with larger canvas
animate(p, nframes = 600, fps = 30, width = 1200, height = 800, end_pause = 10,
        renderer = gifski_renderer("top_journals_race.gif"))
        
        
        
        
        
        
        
        