library(readxl)
library(tidyverse)
library(lubridate)
library(ggplot2)



# Define variables for the workstream name and time frame
workstream_names <- c("Workstream 2 - Community", "Workstream 4 - Acute")
start_month_year <- "February 2023"
end_month_year <- "December 2023"
health_board_trusts <- NULL
filepath <- "C:/Users/De122459/OneDrive - NHS Wales/National SCC Project Planning/National SCC project planning.xlsx"

process_and_clean_data <- function(filepath, workstream_names = NULL, start_month_year, end_month_year, health_board_trusts = NULL) {
  filepath <- "C:/Users/De122459/OneDrive - NHS Wales/National SCC Project Planning/National SCC project planning.xlsx"
  df <- read_excel(filepath)
  
  # Data cleaning and filtering
  df <- df %>%
    
    # Conditionally filter by 'Workstream' if the argument is not NULL
    {
      if(!is.null(workstream_names)) {
        filter(., `Workstream` %in% workstream_names)
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
    select(contains("202"), "Unique ID") %>%
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
           "January 2024" = "Monthly Progress Score - January 2024 (due 13th Feb 2024)",
           "February 2024" = "Monthly Progress Score - February 2024 (due 13th march 2024)") %>% 
    # Parse numbers from specific columns
    mutate(
      across(
        .cols = matches("202") ,
        .fns = ~parse_number(.)
      )
    )
  
  
  
  
  # Generate the complete sequence of month-years between start and end
  date_range <- seq(as.Date(paste0("01 ", start_month_year), format = "%d %B %Y"),
                    as.Date(paste0("01 ", end_month_year), format = "%d %B %Y"),
                    by = "month")
  
  month_year_sequence <- format(date_range, "%B %Y")
  
  # Filter the relevant columns directly
  relevant_cols <- intersect(names(df), month_year_sequence)
  
  # Filter and pivot if necessary for the plot functions
  df <- df %>%
    select(all_of(relevant_cols), `Unique ID`) %>% 
    pivot_longer(cols = -`Unique ID`, names_to = "Month-Year", values_to = "Score") %>%
    mutate(`Month-Year` = as.character(`Month-Year`), # Ensure Month-Year is treated as character
           Score = as.numeric(Score)) %>% # Convert Score to numeric explicitly
    #values_drop_NA = FALSE %>%
    mutate(`Month-Year` = factor(`Month-Year`, levels = month_year_sequence, ordered = TRUE)) %>%
    arrange(`Unique ID`, `Month-Year`)
  
  
  return(df)
}


plot_box_plot <- function(df, workstream_names, health_board_trusts = NULL) {
  # Handle case when no workstream is selected
  if (is.null(workstream_names) || length(workstream_names) == 0) {
    workstream_title <- "ALL Workstreams"
  } else {
    workstream_title <- if(length(workstream_names) > 1) paste(workstream_names, collapse = ", ") else workstream_names
  }
  
  # Handle case when no Health Board/Trust is selected
  if (is.null(health_board_trusts) || length(health_board_trusts) == 0) {
    health_board_trust_title <- ""
  } else {
    health_board_trust_title <- paste("[", paste(health_board_trusts, collapse = ", "), "]", sep = "")
  }
  
  # Plotting
  p <- ggplot(df, aes(x = `Month-Year`, y = `Score`)) +
    geom_boxplot(na.rm = TRUE, fill = '#4A7986', color = 'darkgray') +
    scale_x_discrete() +
    scale_y_continuous(breaks = seq(0,5, by = 0.5), limits = c(0,5))+
    labs(
      title = paste("Box Plot of Monthly Progress Scores for", workstream_title, health_board_trust_title),
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


# Define a new function for plotting the line graph
plot_line_graph <- function(df, workstream_names, health_board_trusts = NULL) {
  # Prepare data for line Graph
  count_data <- df %>%
    group_by(`Month-Year`) %>%
    summarize(Count = sum(!is.na(Score)), .groups = 'drop')  # Use .groups = 'drop' to avoid group_by drop warning
  
  # Handle case when no workstream is selected
  if (is.null(workstream_names) || length(workstream_names) == 0) {
    workstream_title <- "ALL Workstreams"
  } else {
    workstream_title <- if(length(workstream_names) > 1) paste(workstream_names, collapse = ", ") else workstream_names
  }
  
  # Handle case when no Health Board/Trust is selected
  if (is.null(health_board_trusts) || length(health_board_trusts) == 0) {
    health_board_trust_title <- ""
  } else {
    health_board_trust_title <- paste("[", paste(health_board_trusts, collapse = ", "), "]", sep = "")
  }
  
  # Determine range for y-axis
  max_count <- max(count_data$Count, na.rm = TRUE)
  upper_limit <- ceiling(max_count / 10) * 10  # Round up to the nearest 10
  y_breaks <- seq(0, upper_limit, by = max(1, upper_limit / 10))  # Ensure at least one break if max_count is very low
  
  # Line Graph
  p_line <- ggplot(count_data, aes(x = `Month-Year`, y = Count, group = 1)) +
    geom_line(color = "#4A7986", size = 0.7) +  # Set bar width and fill color
    geom_point(color = "#4A7896", size = 2, shape = 21, fill = "white", stroke = 0.3) +
    scale_x_discrete() +
    scale_y_continuous(breaks = y_breaks, limits = c(0, upper_limit)) +
    labs(
      title = paste("Number of Progress Scores Received per Month by", workstream_title, health_board_trust_title),
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

# Function to create line chart in IC guidelines 
create_line_chart_by_id <- function(df, unique_id) {
  
  # Filter long data for the specific unique ID, excluding NA values for Score
  filtered_data <- filter(df, `Unique ID` == unique_id)
  
  # Calculate median, excluding NA
  # median_score <- median(as.numeric(filtered_data$Score), na.rm = TRUE)
  
  # Create the run chart using ggplot2 with specified customizations
  p <- ggplot(filtered_data, aes(x = `Month-Year`, y = Score, group = unique_id)) + # Median line
    #geom_line(y = median_score, color = "#D89F3E", size = 1) + 
    geom_line(color = "#4A7986", size = 0.7) +  # Solid line for connecting data points
    geom_point(color = "#4A7986", size = 2, shape = 21, fill = "white", stroke = 0.3) + # Dot markers
    scale_x_discrete() +  # Use discrete scale for Month-Year factor
    scale_y_continuous(breaks = seq(0, 5, by = 0.5), limits = c(0, 5)) +  # Set y-axis breaks and limits
    labs(
      title = paste("Line Chart of Monthly Progress Score of", unique_id),
      caption = "Source: SCC National Planning File"
    ) +
    theme_minimal(base_family = "sans") +
    theme(
      plot.title = element_text(size = 14, hjust = 0.5, color = "#1B5768"),
      plot.caption = element_text(size = 8, hjust = 1, color = "gray"),
      axis.text.x = element_text(angle = 90, hjust = 1, color = "darkgray"),
      axis.text.y = element_text(angle = 0, hjust = 1, color = "darkgray", size = 9),
      axis.title.x = element_blank(),  # Remove x-axis label
      axis.title.y = element_blank(),  # Remove y-axis label
      panel.grid = element_blank(),  # Remove gridlines
      panel.background = element_rect(fill = "white", colour = NA),
      plot.caption.position = "plot",
      axis.line.x = element_line(color = "gray"),  # Add x-axis line
      axis.ticks.x = element_line(color = "gray"),  # Add x-axis tick marks
      axis.ticks.length = unit(0.1, "cm"),  # Set the length of the tick marks
      axis.ticks.margin = unit(0.2, "cm")  # Adjust the distance between ticks and labels
      
    )
  return(p)
}



# Unified function for preprocessing and plotting with dynamic title and automatic pattern prefixing
plot_scc_sessions <- function(session_type) {
  # Hard-coded path to the Excel file
  file_path <- "C:/Users/De122459/OneDrive - NHS Wales/Contact List/SCC Master Distribution List .xlsx"
  
  # Automatically prepend '^' to the session_type to match from the start
  session_pattern <- paste0("^", session_type)
  
  # Preprocess and transform the data
  df <- read_excel(file_path)
  
  df1 <- df %>% 
    select(4, 6, 19:38) %>% 
    filter(!(Organisation %in% c("Public Health Wales", "IHI", "RSM", "NHS Wales Executive", "Improvement Cymru"))) %>% 
    mutate(across(3:22, ~ifelse(grepl("^[Yy]", .) | grepl("^[Yy]$", .), "Y", NA)), 
           `Coaching Call (Jan 2024)` = ifelse(!is.na(`Jan Lead CC`) | !is.na(`Jan CC...20`), "Y", NA),
           `Learning Session 4 (Nov 2023)` = ifelse(!is.na(`LS4 Day 2`) | !is.na(`LS4 Cl Ex`) | !is.na(`LS4 Day 1 (AB, CTM, HD)`), "Y", NA),
           `Coaching Call (Oct 2023)` = ifelse(!is.na(`Oct CC`) | !is.na(`Oct Lead CC`), "Y", NA),
           `Learning Session 3 (Sep 2023)` = ifelse(!is.na(`LS3`) | !is.na(`LS3 Cl Ex`), "Y", NA),
           `Coaching Call (Aug 2023)` = ifelse(!is.na(`Aug CC`) | !is.na(`Aug Lead CC`), "Y", NA),
           `Coaching Call (July 2023)` = ifelse(!is.na(`11`) | !is.na(`July Lead CC`), "Y", NA),
           `Learning Session 2 (Jun 2023)` = `LS2`,
           `Coaching Call (May 2023)` = `May CC`,
           `Coaching Call (Apr 2023)` = `Apr CC`,
           `Learning Session 1.2 (Mar 2023)` = `LS 1.2`,
           `Coaching Call (Feb 2023)` = `Feb CC`,
           `Coaching Call (Jan 2023)` = `Jan CC...37`,
           `Learning Session 1 (Nov 2022)` = `LS 1`) %>%
    select(-c(`Jan Lead CC`, `Jan CC...20`, `LS4 Day 2`, `LS4 Cl Ex`, `LS4 Day 1 (AB, CTM, HD)`,
              `Oct CC`, `Oct Lead CC`, `LS3`, `LS3 Cl Ex`, `Aug CC`, `Aug Lead CC`,
              `11`, `July Lead CC`, `LS2`, `May CC`, `Apr CC`, `LS 1.2`, `Feb CC`, `Jan CC...37`, `LS 1`, `Workstream`, `Organisation`))
  
  session_order <- c("Coaching Call (Jan 2024)", "Learning Session 4 (Nov 2023)", "Coaching Call (Oct 2023)",
                     "Learning Session 3 (Sep 2023)", "Coaching Call (Aug 2023)", "Coaching Call (July 2023)", 
                     "Learning Session 2 (Jun 2023)", "Coaching Call (May 2023)", "Coaching Call (Apr 2023)", 
                     "Learning Session 1.2 (Mar 2023)", "Coaching Call (Feb 2023)", "Coaching Call (Jan 2023)", 
                     "Learning Session 1 (Nov 2022)")
  
  
  df2 <- df1 %>%
    pivot_longer(cols = everything(), names_to = "SCC Session", values_to = "value") %>%
    filter(value == "Y") %>%
    group_by(`SCC Session`) %>%
    summarise(count = n()) %>%
    mutate(`SCC Session` = factor(`SCC Session`, levels = session_order)) %>%
    arrange(`SCC Session`) %>%
    mutate(`SCC Session` = fct_rev(`SCC Session`))
  
  # Dynamically set the title based on session_type
  title <- ifelse(session_type == "Coaching Call", "Total attendance at Coaching Calls",
                  ifelse(session_type == "Learning Session", "Total Attendance at Learning Sessions",
                         "Total Attendance"))
  
  # Filter data based on the modified session_pattern for plotting
  filtered_data <- df2 %>%
    filter(grepl(session_pattern, `SCC Session`))
  
  # Generate plot
  p <- ggplot(filtered_data, aes(x = `SCC Session`, y = count, group = 1)) +
    geom_line(color = "#4A7986", size = 0.7) +
    geom_point(color = "#4A7986", size = 2, shape = 21, fill = "white", stroke = 0.3) +
    scale_y_continuous(breaks = seq(0, max(filtered_data$count, na.rm = TRUE) + 10, by = 10), limits = c(0, max(filtered_data$count, na.rm = TRUE) + 10)) +
    labs(title = title, caption = "Source: SCC Master Distribution List") +
    theme_minimal(base_family = "sans") +
    theme(
      plot.title = element_text(size = 14, hjust = 0.5, color = "#1B5768"),
      plot.caption = element_text(size = 8, hjust = 1, color = "gray"),
      axis.text.x = element_text(angle = 90, color = "gray"),
      axis.text.y = element_text(color = "darkgray"),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.caption.position = "plot",
      axis.line.x = element_line(color = "gray"),  
      axis.ticks.x = element_line(color = "gray"),  
      axis.ticks.length = unit(0.1, "cm"),  
      axis.ticks.margin = unit(0.2, "cm")  
    )
  
  return(p)
}

plot_scc_sessions("Learning Session")




library(readxl)
library(tidyverse)
library(likert)

plot_likert <- function(scc_session) {
  # Assuming the path and reading of the Excel file works outside this environment
  df <- read_excel("C:/Users/De122459/OneDrive - NHS Wales/Desktop/Psychological Safety SCC/PSCC.xlsx")
  
  df1 <- df |>
    filter(`SCC Session` == scc_session) |>
    select(-1) # Assuming this selects the relevant columns after filtering
  
  num_responses <- nrow(df1)
  
  # Mapping responses to numerical values
  response_mapping <- setNames(1:7, c("Strongly Disagree", "Disagree", "Somewhat Disagree", "Neutral", "Somewhat Agree", "Agree", "Strongly Agree"))
  df1_numerical <- data.frame(lapply(df1, function(x) response_mapping[as.character(x)]))
  df1_factors <- data.frame(lapply(df1_numerical, factor, levels = 1:7))
  levels_labels <- c("Strongly Disagree", "Disagree", "Somewhat Disagree", "Neutral", "Somewhat Agree", "Agree", "Strongly Agree")
  df1_labeled <- data.frame(lapply(df1_factors, function(x) factor(x, levels = 1:7, labels = levels_labels)))
  colnames(df1_labeled) <- gsub("\\.", " ", colnames(df1_labeled))
  likert_data <- likert(df1_labeled)
  
  # Define the dynamic title including the number of responses in brackets
  dynamic_title <- sprintf("Psychological Safety Analysis of %s [n = %s]", scc_session, num_responses)
  
  # Define custom colors for the plot
  custom_colors <- c("#D89F39", "#DFB15F", "#E9C98F", "#BEBEBE", "#81ADB9", "#5E97A6", "#4A7986")
  
  # Plot the likert data with customizations
  p <- plot(likert_data) + 
    theme_minimal(base_family = "sans") + # Use a minimal theme as a base
    labs(title = dynamic_title) + # Set title with the number of responses included
    theme(plot.title = element_text(color = "#4A7896", hjust = 0.5, size = 13),
          panel.border = element_blank(), # Remove the box around the plot
          panel.background = element_blank(), # Make background transparent
          axis.line = element_blank(),
          panel.grid = element_blank(),
          axis.title.x = element_blank()
    )+
    scale_fill_manual(values = custom_colors) +
    guides(fill = guide_legend(title = NULL))
  
  return(p) # Display the plot
}

# Call the function with a specific session
plot_likert("Learning Session 4 (Nov 2023)")
