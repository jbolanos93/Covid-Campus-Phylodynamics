#this sciopt
#1) graph of the off the on / on to off rates for main text that combines the campus divisions as "on"


#Ridgeline plots 
#install.packages("ggridges")  # Install if not already installed
library(ggridges)
library(ggplot2)
library(tidyr)
library(dplyr)


#set working dir 
setwd("/Users/juanbolanos/Desktop/Phylogenetisc_Paper/Dornburg-Review/Final-Data/scripts/Ridge_Line_Violin_Plot_Tracer_Frequencies/")

#read csv here. 
df<-read.csv("/Users/juanbolanos/Desktop/Phylogenetisc_Paper/Dornburg-Review/Final-Data/scripts/Ridge_Line_Violin_Plot_Tracer_Frequencies/input/Geo_Location_Transition_Rates_On_vs_Off.csv")


colnames(df)

# Exclude "North->Off-Campus_2"
#df <- df %>% select(-"North..Off.Campus_2")

# Convert selected data to long format
df_long <- df %>%
  pivot_longer(cols = -Sample, names_to = "Category", values_to = "Value")

#Clean this up to have more intuitive naming conventions
df_long <- df_long %>%
  mutate(Category = case_when(
    Category %in% c("Center..Off.Campus", "East..Off.Campus", "Martin..Off.Campus",
                    "North..Off.Campus_1", "North..Off.Campus_2") ~ "OntoOffCampus",
    Category == "Off.Campus..South" ~ "OfftoOnCampus",
    TRUE ~ Category  # Keep other categories unchanged
  ))
  
#lets restrict ourselves to the quantiles of the rate distribution, calculate in case we want to talk about it
df_summary <- df_long %>%
  group_by(Category) %>%
  summarise(Q3 = quantile(Value, 0.75, na.rm = TRUE))  
  
# Filter the dataset to only include values up to Q3 for plotting
df_filtered <- df_long %>%
  group_by(Category) %>%
  filter(Value <= quantile(Value, 0.75, na.rm = TRUE))

#lets use a violin plot to plot the truncated distributions
ggplot(df_filtered, aes(x = Category, y = Value, fill = Category)) +
  geom_violin(trim = FALSE, alpha = 0.5) +  # Violin plot showing truncated data
  geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.3) +  # Faint boxplot for context
  geom_point(data = df_summary, aes(x = Category, y = Q3), size = 3, color = "black") +  # Q3 marker
  labs(title = "Distribution of Evolutionary Transition Rates (Capped at 75th Percentile)",
       x = "Transition Type",
       y = "Estimated Rate") +
  theme_minimal() +
  scale_fill_manual(values = c("OntoOffCampus" = "blue", "OfftoOnCampus" = "Yellow"))
  
# Perform Wilcoxon Test
test_result <- wilcox.test(Value ~ Category, data = df_long)
print(test_result)


# 
# ##################
# ###original stuff
# ##################
# 
# #TEst to see how plot shows. . 
# # Create the ridge plot for Kernel Density Estimation
# ggplot(df_long, aes(x = Value, y = Category, fill = Category)) +
#   geom_density_ridges(scale = 0.90, alpha = 0.8, rel_min_height = 0.11) +
#   theme_minimal() +
#   xlim(-.02, 1.0)+
#   labs(title = "Kernal Density Estimation of Transition Rates",
#        x = "Value",
#        y = "Transition") +
#   theme(legend.position = "none")
# 
# ### joining all on campus distributions ###
# KDE<-density(c(df$North..Off.Campus_1,df$Martin..Off.Campus,df$East..Off.Campus,df$Center..Off.Campus))
# plot(KDE,main = "KDE all on campus")
# 
# 
# # Convert KDE object to dataframe
# kde_df <- data.frame(x = KDE$x, y = KDE$y)
# 
# # Create a dataframe for Off.Campus..South density
# south_kde <- density(df$Off.Campus..South)
# south_kde_df <- data.frame(x = south_kde$x, y = south_kde$y, Region = "Off Campus -> On Campus")
# 
# # Add a "Region" column to kde_df to distinguish it
# kde_df$Region <- "Combined On Campus"
# 
# 
# # Combine the two dataframes
# combined_data <- rbind(
#   kde_df[, c("x", "y", "Region")],
#   south_kde_df
# )
# 
# unique(combined_data$Region)
# 
# #plot KDE on top of each other. 
# plot(KDE,main="KDE")
# lines(south_kde,col="red")
# legend(x="topright",
#        legend = c("Off Campus->On Campus","Combined On Campus->Off Campus"),
#        col=c("red",'black'), lwd=1, lty=c(1,1,1), 
#        pch=c(NA,NA), cex = 0.5 )
# 
# 
# ## plot with ridges ## 
# 
# #pdf("density_ridges.pdf", width =7, height = 3)
# 
# 
# ggplot(combined_data, aes(x = x, y = Region, height = y, fill = Region)) +
#   geom_density_ridges(
#     stat = "identity",  # Use precomputed densities
#     alpha = 0.7,        # Transparency
#     scale = 0.9         # Adjust overlap between ridges
#   ) +
#   scale_fill_manual(values = c("Combined On Campus" = "#4ECDC4", "Off Campus South" = "#FF6B6B")) +
#   labs(
#     title = "Combined On vs Off Campus",
#     x = "Value",
#     y = "Density"
#   ) +
#   xlim(-0.06, 0.75)+
#   theme_ridges() +
#   theme(legend.position = "none")
# 
# #dev.off()
# 
