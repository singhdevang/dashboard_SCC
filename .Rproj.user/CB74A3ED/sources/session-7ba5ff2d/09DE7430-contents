# Load necessary libraries
library(readxl)
library(tidyverse)
library(lubridate)
library(ggplot2)

# Define variables for the workstream name and time frame
workstream_names <- c("Workstream 2 - Community", "Workstream 4 - Acute")
start_month_year <- "February 2023"
end_month_year <- "November 2023"
health_board_trusts <- NULL


process_data <- function(workstream_names = NULL, start_month_year, end_month_year, health_board_trusts = NULL) {
  # Read the data from an Excel file
  df <- read_excel("C:/Users/De122459/OneDrive - NHS Wales/National SCC Project Planning/National SCC project planning.xlsx")
  
  # Data cleaning and transformation
  new_df <- df %>%
    # Conditionally filter by 'Workstream' if the argument is not NULL
    {
      if(!is.null(workstream_names)) {
        filter(., Workstream %in% workstream_names)
      } else {
        .
      }
    } %>%
    # Conditionally filter by 'Health Board / Trust' if the argument is not NULL
    {
      if(!is.null(health_board_trusts)) {
        filter(., `Health Board / Trust` %in% health_board_trusts)
      } else {
        .
      }
    } %>%
    
    # Select columns containing "202" in their names
    select(contains("202")) %>% 
    # Replace empty or "NO Report received" values with NA
    mutate(across(everything(), ~if_else(. == "" | . == "NO Report received" | . == "No report received", NA_character_ , .))) %>% 
    # Rename columns to shorter names and parse numbers
    rename("February 2023"= "Monthly Progress Score - February 2023 (MSSW only)",
           "March 2023" = "Monthly Progress Score - March 2023 (MSSW ONLY)",
           "April 2023" = "Monthly Progress Score - April 2023",
           "May 2023" = "Monthly Progress Score - May 2023",
           "June 2023" = "Monthly Progress Score - June 2023",
           "July 2023" = "Monthly Progress Score - July 2023",
           "August 2023" = "Monthly Progress Score - August 2023",
           "September 2023" = "Monthly Progress Score - September 2023",
           "October 2023" = "Monthly Progress Score - October 2023 (Due 21/11/23)",
           "November 2023" = "Monthly Progress Score - November 2023 (Due 13th Dec)",
           "December 2023" = "Monthly Progress Score - December 2023 (due 17th January)",
           "January 2024" = "Monthly Progress Score - January 2024",
           "February 2024" = "Monthly Progress Score - February 2024") %>% 
    # Parse numeric values
    mutate(across(everything(), ~parse_number(.)))
  
  # Define the start and end dates
  start_date <- as.Date(paste0("01 ", start_month_year), format = "%d %B %Y")
  end_date <- as.Date(paste0("01 ", end_month_year), format = "%d %B %Y")
  
  # Generate a sequence of month-years between the start and end dates
  date_sequence <- seq.Date(start_date, end_date, by = "month")
  month_year_sequence <- format(date_sequence, "%B %Y")
  
  # Prepare column names for comparison
  month_year_cols <- names(new_df)
  
  # Filter the columns that are in the sequence
  relevant_cols <- month_year_cols[month_year_cols %in% month_year_sequence]
  
  # Pivot the data frame to long format
  long_df <- new_df %>%
    select(all_of(relevant_cols)) %>%
    pivot_longer(cols = everything(),
                 names_to = "Month-Year",
                 values_to = "Score",
                 values_drop_na = FALSE) %>%
    mutate(`Month-Year` = factor(`Month-Year`, levels = month_year_sequence, ordered = TRUE)) %>%
    arrange(`Month-Year`, Score)
  
  return(long_df)
}
loong_df <- process_data(workstream_names, start_month_year, end_month_year, health_board_trusts)



plot_box_plot <- function(df, workstream_names){
  # Handle case when no workstream is selected
  if (is.null(workstream_names) || length(workstream_names) == 0) {
    workstream_title <- "ALL Workstreams"
  } else {
    workstream_title <- if(length(workstream_names) > 1) paste(workstream_names, collapse = ", ") else workstream_names
  }
  
  # Plotting
  p <- ggplot(df, aes(x = `Month-Year`, y = `Score`)) +
    geom_boxplot(na.rm = TRUE, fill = '#4A7986', color = 'darkgray') +
    scale_x_discrete() +
    scale_y_continuous(breaks = seq(0,5, by = 0.5), limits = c(0,5))+
    labs(
      title = paste("Box Plot of Monthly Progress Scores for", workstream_title),
      subtitle = paste("(", start_month_year, "-", end_month_year, ")"),  
      caption = "Source: SCC National Planning File"
    ) + 
    theme_minimal(base_family = "sans") +
    theme(
      plot.title = element_text(size = 12, hjust = 0.5, color = "#1b5768"),
      plot.subtitle = element_text(size = 10, hjust = 0.5, color = "#1b5768", margin = margin(b = 10)),  # Style the subtitle
      plot.caption = element_text(size = 7, hjust = 1, color = "gray"),
      axis.text.x = element_text(angle = 90, hjust = 1, color = "darkgray"),
      axis.text.y = element_text(angle = 0, hjust = 1, color = "darkgray"),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(), 
      axis.line.x = element_line(color = "gray"),
      axis.line.y = element_line(color = "gray"),
      axis.ticks.x = element_line(color = "gray"),
      axis.ticks.y = element_line(color = "gray"),
      axis.ticks.length = unit(0.1, "cm"),
      axis.ticks.margin = unit(0.2, "cm"),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.caption.position = "plot"
    ) 
  
  # Print the plot
  print(p)
  
}

# Define a new function for plotting the bar graph
plot_line_graph <- function(df, workstream_names) {
  # Prepare data for Bar Graph
  count_data <- df %>%
    group_by(`Month-Year`) %>%
    summarize(Count = sum(!is.na(Score)), .groups = 'drop')  # Use .groups = 'drop' to avoid group_by drop warning
  
  # Handle case when no workstream is selected
  if (is.null(workstream_names) || length(workstream_names) == 0) {
    workstream_title <- "ALL Workstreams"
  } else {
    workstream_title <- if(length(workstream_names) > 1) paste(workstream_names, collapse = ", ") else workstream_names
  }
  
  # Determine range for y-axis
  max_count <- max(count_data$Count, na.rm = TRUE)
  upper_limit <- ceiling(max_count / 10) * 10  # Round up to the nearest 10
  y_breaks <- seq(0, upper_limit, by = max(1, upper_limit / 10))  # Ensure at least one break if max_count is very low
  
  # Line Graph
  p_line <- ggplot(count_data, aes(x = `Month-Year`, y = Count, group = 1)) +
    geom_line(color = "#4A7986", size = 1) +  # Set bar width and fill color
    geom_point(color = "#4A7896", size = 3, shape = 21, fill = "white", stroke = 0) +
    scale_x_discrete() +
    scale_y_continuous(breaks = y_breaks, limits = c(0, upper_limit)) +
    labs(
      title = paste("Number of Progress Scores Received per Month by", workstream_title),
      subtitle = paste("(", start_month_year, "-", end_month_year, ")"),
      caption = "Source: SCC National Planning File"
    ) +
    theme_minimal(base_family = "sans") +
    theme(
      plot.title = element_text(size = 12, hjust = 0.5, color = "#1b5768"),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "#1b5768", margin = margin(b = 10)),
      plot.caption = element_text(size = 8, hjust = 1, color = "darkgray"),
      axis.text.x = element_text(angle = 90, hjust = 1, color = "darkgray"),
      axis.text.y = element_text(angle = 0, hjust = 1, color = "darkgray"),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.line.x = element_line(colour = "gray"),
      axis.line.y = element_blank(),
      axis.ticks.x = element_line(color = "gray"),
      axis.ticks.y = element_blank(),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white", color = NA)
    )
  
  # Print the line graph
  print(p_line)
}




