### This script includes:
### 1) Distribution plots
### 2) Grouped Boxplots
### 3) Lineages Through Time 


# Load required library
library(ggplot2)
library(tidyr)
library(dplyr)
library(glue)
library(tidyverse)

#define output directory 
outfile<-"/Users/juanbolanos/Desktop/Phylogenetisc_Paper/Dornburg-Review/Final-Data/scripts/Transition_Counter_Plots_AlexShared/Distribution_Plots_And_LTT_CombinedScript/output"

# Define the file paths
PreAlpha <- "/Users/juanbolanos/Desktop/Phylogenetisc_Paper/Dornburg-Review/Final-Data/scripts/Transition_Counter_Plots_AlexShared/Distribution_Plots_And_LTT_CombinedScript/input/PreAlpha_Grouped_COMBINED-Transition-Count-03-21-2024-10-30.txt" 
Alpha <- "/Users/juanbolanos/Desktop/Phylogenetisc_Paper/Dornburg-Review/Final-Data/scripts/Transition_Counter_Plots_AlexShared/Distribution_Plots_And_LTT_CombinedScript/input/Alpha_Grouped_COMBINED-Transition-Count-03-21-2024-10-30.txt" 
Delta <- "/Users/juanbolanos/Desktop/Phylogenetisc_Paper/Dornburg-Review/Final-Data/scripts/Transition_Counter_Plots_AlexShared/Distribution_Plots_And_LTT_CombinedScript/input/Delta_Grouped_COMBINED-Transition-Count-03-21-2024-10-30.txt" 
Omicron <- "/Users/juanbolanos/Desktop/Phylogenetisc_Paper/Dornburg-Review/Final-Data/scripts/Transition_Counter_Plots_AlexShared/Distribution_Plots_And_LTT_CombinedScript/input/Omicron_Grouped_COMBINED-Transition-Count-03-21-2024-10-30.txt" 
Combined <- "/Users/juanbolanos/Desktop/Phylogenetisc_Paper/Dornburg-Review/Final-Data/scripts/Transition_Counter_Plots_AlexShared/Distribution_Plots_And_LTT_CombinedScript/input/All_Data_Grouped_COMBINED-Transition-Count-03-26-2024-13-44.txt"  

#Subsample Data
one_to_one<-"/Users/juanbolanos/Desktop/Phylogenetisc_Paper/Dornburg-Review/Final-Data/scripts/Transition_Counter_Plots_AlexShared/Distribution_Plots_And_LTT_CombinedScript/input/Bootstrap/Boostrapped_1-to-1_ALLDATA-Transition-Count-02-05-2025-09-57.txt"
one_to_five<-"/Users/juanbolanos/Desktop/Phylogenetisc_Paper/Dornburg-Review/Final-Data/scripts/Transition_Counter_Plots_AlexShared/Distribution_Plots_And_LTT_CombinedScript/input/Bootstrap/Boostrapped_1-to-5_ALLDATA-Transition-Count-02-05-2025-09-57.txt"
two_to_three<-"/Users/juanbolanos/Desktop/Phylogenetisc_Paper/Dornburg-Review/Final-Data/scripts/Transition_Counter_Plots_AlexShared/Distribution_Plots_And_LTT_CombinedScript/input/Bootstrap/Boostrapped_2-to-3_ALLDATA-Transition-Count-02-05-2025-09-57.txt"


#campus bias
three_to_two<-"/Users/juanbolanos/Desktop/Phylogenetisc_Paper/Dornburg-Review/Final-Data/scripts/Transition_Counter_Plots_AlexShared/Distribution_Plots_And_LTT_CombinedScript/input/Bootstrap/Boostrapped_3_campus-to-2_ALLDATA-Transition-Count-02-20-2025-21-06.txt"
five_to_one<-"/Users/juanbolanos/Desktop/Phylogenetisc_Paper/Dornburg-Review/Final-Data/scripts/Transition_Counter_Plots_AlexShared/Distribution_Plots_And_LTT_CombinedScript/input/Bootstrap/Boostrapped_5_campus-to-1_offCampus_ALLDATA-Transition-Count-02-20-2025-21-06.txt"

#################################
######    histogram        ######
#################################
plotBEASTTransitions<-function(file_path){
# Read the file into a character vector
lines <- readLines(file_path)

# Locate "Histogram" section start and next section start (if any)
hist_start <- grep("^Histogram", lines)

# Identify the next section (e.g., "Lineages through time") to find the histogram's end
next_section <- grep("^[A-Za-z ]+", lines[(hist_start + 1):length(lines)]) 

# Determine the end of histogram section
if (length(next_section) > 0) {
  hist_end <- hist_start + next_section[1] - 2  # Adjust index to exclude the next section header
} else {
  hist_end <- length(lines)  # If no next section, assume the rest is the histogram
}
endofsection<-hist_end+ hist_start
# Read histogram data
hist_data <- read.table(text = lines[(hist_start + 1): endofsection], 
                        header = TRUE, sep = "\t", check.names = FALSE, fill = TRUE)

# Remove blank columns (columns with all NAs or empty names)
hist_data <- hist_data %>% select(where(~!all(is.na(.))))  # Drops empty columns

# Convert from wide to long format
hist_long <- hist_data %>%
  pivot_longer(cols = -Transition, names_to = "Value", values_to = "Frequency") %>%
  mutate(Value = as.numeric(Value), Frequency = as.numeric(Frequency))

# Plot the histogram
gg<-ggplot(hist_long, aes(x = Value, y = Frequency, fill = Transition)) +
  geom_col(position = "dodge") +
  scale_x_continuous(breaks = seq(0, max(hist_long$Value, na.rm = TRUE), by = 50)) +
  labs(title = "Transition Histogram", x = "Transition States", y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "bottom")
  print(gg)
  
  pdf(glue("{outfile}/Histogram.pdf"), width = 13.33, height = 7.33)
  print(gg)
  dev.off()
  return(hist_long)
  
  }

#usage
AllHistogramData<-plotBEASTTransitions(two_to_three)
AllHistogramData<-plotBEASTTransitions(Combined)

#################################
######   ltt plot          ######
#################################
    
plotBEASTLTT<-function(file_path){  # Feed in a file path to the transitioncounteroutput
# Read the file into a character vector
lines <- readLines(file_path)
#find start line
ltt_start <- grep("^Lineages", lines)

# Identify the next section (e.g., "Lineages through time") to find the histogram's end
lttnext_section <- grep("^Introduction", lines)


endoflttsection<-lttnext_section -1
# Read histogram data
ltt_data <- read.table(text = lines[(ltt_start + 1): endoflttsection], 
                        header = TRUE, sep = "\t", check.names = FALSE, fill = TRUE)

# Remove blank columns (columns with all NAs or empty names)
ltt_data <- ltt_data %>% select(where(~!all(is.na(.))))  # Drops empty columns

# Convert from wide to long format
ltt_long <- ltt_data %>%
  pivot_longer(cols = -Transition, names_to = "Time", values_to = "Value") %>%
  mutate(Time = as.numeric(Time), Value = as.numeric(Value))

# Filter for Off_Campus and On_Campus
ltt_filtered <- ltt_long %>%
  filter(Transition %in% c("Off_Campus", "On_Campus"))

# Plot
gg<-ggplot(ltt_filtered, aes(x = Time, y = Value, color = Transition)) +
  geom_line(size = 1) +
  geom_point(size = 1.5, alpha = 0.7) + 
  labs(title = "Transition Trends Over Time", x = "Time", y = "Value") +
  theme_minimal() +
  theme(legend.position = "bottom")
  print(gg)
  pdf(glue("{outfile}/LTT.pdf"), width = 13.33, height = 7.33)
  print(gg)
  dev.off()
  }
  
#usage
AllLTTData<-plotBEASTLTT(two_to_three)



#################################
###### individual boxplot ######
#################################
plotBEASTBoxpPlots<-function(file_path){  # Feed in a file path to the transitioncounteroutput
  # Read the file into a character vector
  lines <- readLines(file_path)
  
  # Locate "Histogram" section start and next section start (if any)
  hist_start <- grep("^Histogram", lines)
  
  # Identify the next section (e.g., "Lineages through time") to find the histogram's end
  next_section <- grep("^[A-Za-z ]+", lines[(hist_start + 1):length(lines)]) 
  
  # Determine the end of histogram section
  if (length(next_section) > 0) {
    hist_end <- hist_start + next_section[1] - 2  # Adjust index to exclude the next section header
  } else {
    hist_end <- length(lines)  # If no next section, assume the rest is the histogram
  }
  endofsection<-hist_end+ hist_start
  # Read histogram data
  hist_data <- read.table(text = lines[(hist_start + 1): endofsection], 
                          header = TRUE, sep = "\t", check.names = FALSE, fill = TRUE)
  
  # Remove blank columns (columns with all NAs or empty names)
  hist_data <- hist_data %>% select(where(~!all(is.na(.))))  # Drops empty columns
  
  # Convert from wide to long format
  hist_long <- hist_data %>%
    pivot_longer(cols = -Transition, names_to = "Value", values_to = "Frequency") %>%
    mutate(Value = as.numeric(Value), Frequency = as.numeric(Frequency))%>%
    filter(Frequency > 0)  # Remove zero values to avoid log issues
  
  print(hist_long)
  # Plot
  gg <- ggplot(hist_long, aes(x = Transition, y = Frequency, fill = Transition)) +
    geom_boxplot() +  # Replace geom_line/geom_point with geom_boxplot
    labs(title = "Transitions Dynamics across Sars-Cov_2 Groups", x = "Groups", y = "Value") +
    #theme_minimal() +
    theme(legend.position = "bottom")
  
  print(gg)
  pdf(glue("{outfile}/Boxplots.pdf"), width = 9.33, height = 4.33)
  print(gg)
  dev.off()
}

plotBEASTBoxpPlots(one_to_one)
plotBEASTBoxpPlots(one_to_five)
plotBEASTBoxpPlots(two_to_three)
plotBEASTBoxpPlots(Combined)



#################################
######   grouped boxplot ######
#################################
# Function to extract histogram data from a file
extract_histogram_data <- function(file_path, file_label) {
  library(tidyverse)
  lines <- readLines(file_path)
  
  # Locate "Histogram" section start
  hist_start <- grep("^Histogram", lines)
  
  # Identify the next section (e.g., "Lineages through time") to determine the end
  next_section <- grep("^[A-Za-z ]+", lines[(hist_start + 1):length(lines)])
  
  # Determine end of histogram section
  if (length(next_section) > 0) {
    hist_end <- hist_start + next_section[1] - 2  # Adjust index to exclude the next section header
  } else {
    hist_end <- length(lines)  # If no next section, assume rest is the histogram
  }
  
  endofsection <- hist_end + hist_start
  
  # Read histogram data
  hist_data <- read.table(text = lines[(hist_start + 1):endofsection], 
                          header = TRUE, sep = "\t", check.names = FALSE, fill = TRUE)
  
  # Remove blank columns
  hist_data <- hist_data %>% select(where(~ !all(is.na(.))))
  
  # Convert to long format
  hist_long <- hist_data %>%
    pivot_longer(cols = -Transition, names_to = "Value", values_to = "Frequency") %>%
    mutate(File = file_label,  # Use provided label instead of file name
           Value = as.numeric(Value), 
           Frequency = as.numeric(Frequency)) %>% 
    filter(Frequency > 0)  # Remove zero values to avoid log issues
  
  return(hist_long)
}


# List your files and custom labels
files <- c(Combined,three_to_two,five_to_one)  # Replace with actual paths
file_labels <- c('Combined','3/2','5/1')  # Custom labels


# Extract and combine data from all files
all_hist_data <- map2_dfr(files, file_labels, extract_histogram_data)


# Manually specify order of groups (Transitions) within each dataset
all_hist_data <- all_hist_data %>%
  mutate(Transition = factor(Transition, levels = c("On_Campus=>Off_Campus", 
                                                    "Off_Campus=>On_Campus", 
                                                    "On_Campus=>On_Campus", 
                                                    "Off_Campus=>Off_Campus"))) %>%
  mutate(File = factor(File, levels = c('Combined','3/2','5/1')))


all_hist_data

# Plot grouped boxplots (X-axis grouped by File)
gg<-ggplot(all_hist_data, aes(x = File, y = Value, fill = Transition)) +
  geom_boxplot() +
  #scale_y_log10()+
  labs(title = "Grouped Boxplots of Histogram Data", x = "Dataset", y = "Value") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")

print(gg)
pdf(glue("{outfile}/GroupedBoxplots.pdf"), width = 12.33, height = 4.33)
print(gg)
dev.off()


