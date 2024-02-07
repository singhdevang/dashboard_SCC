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
           "January 2024" = "Monthly Progress Score - January 2024 (due 13th Feb 2024)",
           "February 2024" = "Monthly Progress Score - February 2024 (due 13th march 2024)") %>% 
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
  
  # Determine range for y-axis
  max_count <- max(count_data$Count, na.rm = TRUE)
  upper_limit <- ceiling(max_count / 10) * 10  # Round up to the nearest 10
  y_breaks <- seq(0, upper_limit, by = max(1, upper_limit / 10))  # Ensure at least one break if max_count is very low
  
  # Line Graph
  p_line <- ggplot(count_data, aes(x = `Month-Year`, y = Count, group = 1)) +
    geom_line(color = "#4A7986", size = 1) +  # Set bar width and fill color
    geom_point(color = "#4A7896", size = 3, shape = 21, fill = "white", stroke = 1) +
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

# Define variables for the workstream name and time frame
Workstream_names <- "Workstream 2 - Community"
Start_month_year <- "February 2023"
End_month_year <- "March 2023"
Health_board_trusts <- NULL




# Function to filter,rename,select relevant columns, replace blank cells/ No report received cells with NA and extracting numbers from string
# Wrote two separate function for data cleaning and manipulation to make it easier for me
clean_and_rename <- function(Workstream_names = NULL, Start_month_year, End_month_year, Health_board_trusts = NULL) {
  
  # Read Excel file
  df <- read_excel("C:/Users/De122459/OneDrive - NHS Wales/National SCC Project Planning/National SCC project planning.xlsx")
  
  # Data cleaning and transformation
  df <- df %>%
    # Conditionally filter by 'Workstream' if the argument is not NULL
    {
      if(!is.null(Workstream_names)) {
        filter(., Workstream %in% Workstream_names)
      } else {
        .
      }
    } %>%
    # Conditionally filter by 'Health Board / Trust' if the argument is not NULL
    {
      if(!is.null(Health_board_trusts)) {
        filter(., `Health Board / Trust` %in% Health_board_trusts)
      } else {
        .
      }
    } 
  
  
  
  df<-df |> rename("February 2023"= "Monthly Progress Score - February 2023 (MSSW only)",
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
                   "February 2024" = "Monthly Progress Score - February 2024 (due 13th march 2024)"
  )
  
  
  
  
  
  #Select the specific fields or ones that have 202 in them, this should pick up future ones as well!
  df<- df |> select(c("Unique ID", "Health Board / Trust", "Project Title (inline with Monthly Reporting)")| contains("202"))
  
  # Replace blank cells and "NO Report received" with NA
  df[df == "" | df == "NO Report received"] <- NA
  
  
  
  for (col in colnames(df)[4:length(colnames(df))]) {
    df[[col]] <- str_extract(df[[col]], "\\d+\\.*\\d*")
  }
  
  
  
  month_year_cols <-colnames(df)[4:length(colnames(df))]
  
  # Generate the complete sequence of month-years between start and end
  full_sequence <- seq(as.Date(paste0("01 ", Start_month_year), format = "%d %B %Y"),
                       as.Date(paste0("01 ", End_month_year), format = "%d %B %Y"),
                       by = "month")
  full_sequence_formatted <- format(full_sequence, "%B %Y")
  
  # Filter the columns that fall within the generated sequence
  selected_month_cols <- month_year_cols[month_year_cols %in% full_sequence_formatted]
  
  # Include other necessary columns
  id_and_project_cols <- c("Unique ID", "Health Board / Trust", "Project Title (inline with Monthly Reporting)")
  cols_to_keep <- c(id_and_project_cols, selected_month_cols)
  
  # Filter and pivot the data frame to long format
  long_df <- df %>%
    select(all_of(cols_to_keep)) %>%
    pivot_longer(cols = all_of(selected_month_cols),
                 names_to = "Month-Year",
                 values_to = "Score",
                 #keeps the column name even if it is NA
                 values_drop_na = FALSE) %>% 
    #make score numeric to include NA values
    mutate(Score = as.numeric(Score))
  
  # Convert 'Month-Year' to an ordered factor based on the full sequence
  long_df$`Month-Year` <- factor(long_df$`Month-Year`, levels = full_sequence_formatted, ordered = TRUE)
  
  return(long_df)
}

loong_df <- clean_and_rename(Workstream_names, Start_month_year, End_month_year, Health_board_trusts)


# Function to create run chart in IC guidelines with a loop to make all 18 charts on the basis of unique ID
create_run_chart_by_id <- function(df, unique_id) {
  
  # Filter long data for the specific unique ID, excluding NA values for Score
  filtered_data <- filter(df, `Unique ID` == unique_id)
  
  # Calculate median, excluding NA
  # median_score <- median(as.numeric(filtered_data$Score), na.rm = TRUE)
  
  # Create the run chart using ggplot2 with specified customizations
  p <- ggplot(filtered_data, aes(x = `Month-Year`, y = Score, group = unique_id)) + # Median line
    #geom_line(y = median_score, color = "#D89F3E", size = 1) + 
    geom_line(color = "#4A7986", size = 1) +  # Solid line for connecting data points
    geom_point(color = "#4A7986", size = 3, shape = 21, fill = "white", stroke = 1) + # Dot markers
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
                  ifelse(session_type == "Learning Session", "Total attendance at Learning Sessions",
                         "Total Attendance"))
  
  # Filter data based on the modified session_pattern for plotting
  filtered_data <- df2 %>%
    filter(grepl(session_pattern, `SCC Session`))
  
  # Generate plot
  p <- ggplot(filtered_data, aes(x = `SCC Session`, y = count, group = 1)) +
    geom_line(color = "#4A7986", size = 1) +
    geom_point(color = "#4A7986", size = 2, shape = 21, fill = "white", stroke = 1) +
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
  
  # Mapping responses to numerical values
  response_mapping <- setNames(1:7, c("Strongly Disagree", "Disagree", "Somewhat Disagree", "Neutral", "Somewhat Agree", "Agree", "Strongly Agree"))
  df1_numerical <- data.frame(lapply(df1, function(x) response_mapping[as.character(x)]))
  df1_factors <- data.frame(lapply(df1_numerical, factor, levels = 1:7))
  levels_labels <- c("Strongly Disagree", "Disagree", "Somewhat Disagree", "Neutral", "Somewhat Agree", "Agree", "Strongly Agree")
  df1_labeled <- data.frame(lapply(df1_factors, function(x) factor(x, levels = 1:7, labels = levels_labels)))
  colnames(df1_labeled) <- gsub("\\.", " ", colnames(df1_labeled))
  likert_data <- likert(df1_labeled)
  
  # Define the dynamic title
  dynamic_title <- sprintf("Psychological Safety Analysis of %s", scc_session)
  # Define custom colors for the plot
  custom_colors <- c("#D89F39", "#DFB15F", "#E9C98F", "#BEBEBE", "#81ADB9", "#5E97A6", "#4A7986")
  
  
  # Plot the likert data with customizations
  p <- plot(likert_data) + 
    theme_minimal() + # Use a minimal theme as a base
    labs(title = dynamic_title, x = NULL) + # Set title, remove x-axis title
    theme(plot.title = element_text(color = "#4A7896", hjust = 0.5, size = 13), # Change title color
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

# Call the function and check the column names
plot_likert("Learning Session 4")

