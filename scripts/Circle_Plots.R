
################################
#### ChordDiagram ################
################################
# Load the circlize library

library(phyloseq)
library(phytools)
library(phylobase)
#BiocManager::install("ComplexHeatmap")
ChordDiagramPlot<-function(a_csv,DiagramTitle){
  library(circlize)
  library(glue)
  
  
  
  # Define your data
  ChordData<-read.csv(a_csv)
  #ChordData
  
  #Save file 
  # Define the file path for saving the PDF
  pdf_file<-glue("/Users/juanbolanos/Desktop/Phylogenetisc_Paper/Dornburg-Review/Final-Data/scripts/Visuals-CirclePlot-R/output/{DiagramTitle}_chord_diagram.pdf")
  
  
  # Save the chord diagram to a PDF file
  pdf(pdf_file,width = 16, height = 14)
  
  #chordDiagram(ChordData)
  
  grid.col = c(Off_Campus = "red", 
               South="black",
               Center = "green", 
               Martin = "yellow",
               North = "blue",
               East="orange")
  
  chordDiagram(ChordData,
               grid.col=grid.col)
  
  # Add title to the chord diagram
  title(DiagramTitle, cex = 1, outer = FALSE)
  
  

  
  dev.off()
  
  #emphasize small transitions
  #ChordData[ChordData < 100000000000] = "#00000000"
  #ChordData(data, grid.col = grid.col, transparency = 0) 
}


#working version of the fnction, the funcitn below is the one mosified that I can tweak and break. 
ChordDiagramPlotGrouped<-function(a_csv,DiagramTitle){
  library(circlize)
  library(glue)
  library(ComplexHeatmap)

  
  
  
  # Define your data
  ChordData<-read.csv(a_csv)
  #ChordData
  
  #Save file 
  # Define the file path for saving the PDF
  pdf_file<-glue("/Users/juanbolanos/Desktop/Phylogenetisc_Paper/Dornburg-Review/Final-Data/scripts/Visuals-CirclePlot-R/output/{DiagramTitle}_GROUPED_chord_diagram.pdf")
  
  
  # Save the chord diagram to a PDF file
  pdf(pdf_file)
  
  #chordDiagram(ChordData)

  
  grid.col = c(Off_Campus = "orange", 
               On_Campus="blue")
  
  
  # discrete
  lgd_points = Legend(at = c("label1", "label2"), type = "points", 
                      legend_gp = gpar(col = 2:3), title_position = "topleft", 
                      title = "Track1")
  
  chordDiagram(ChordData,grid.col=grid.col,annotationTrack = c("name", "grid"))
  
  #draw(lgd_points)
  # Add title to the chord diagram
  title(DiagramTitle, cex = 1, outer = FALSE)
  
  
  
  
  dev.off()
  
  chordDiagram(ChordData,grid.col=grid.col,annotationTrack = c("name", "grid"))
  #lgd_lines = Legend(at = c("On Campus", "Off Campus"), type = "lines", legend_gp = gpar(col = c(4,7), lwd = 2), title_position = "topleft", title = "Track2")
  lgd_lines <- Legend(
  labels = c("Off Campus", "On Campus"),  # Labels for the legend
  legend_gp = gpar(fill = c("darkorange", "blue")),  # Colors for the legend
  title = "Transitions",  # Title for the legend
  title_gp = gpar(fontsize = 12, fontface = "bold"),  # Style for title
  labels_gp = gpar(fontsize = 10)  # Style for labels
)
  
  #draw(lgd_lines, x = unit(4, "mm"), y = unit(4, "mm"), just = c("left", "bottom"))
  
  # Draw the legend
  draw(legend)
  print("Done")
  
  #emphasize small transitions
  #ChordData[ChordData < 100000000000] = "#00000000"
  #ChordData(data, grid.col = grid.col, transparency = 0) 
}


#Tweaked version of the functino., 
ChordDiagramPlotGrouped_v2<-function(a_csv,DiagramTitle){
  library(circlize)
  library(glue)
  library(ComplexHeatmap)
  
  
  
  
  # Define your data
  ChordData<-read.csv(a_csv)
  #ChordData
  
  #Save file 
  # Define the file path for saving the PDF
  #pdf_file<-glue("/Users/juanbolanos/Desktop/Phylogenetisc_Paper/Dornburg-Review/Final-Data/scripts/Visuals-CirclePlot-R/output/{DiagramTitle}_GROUPED_chord_diagram.pdf")
  
  
  # Save the chord diagram to a PDF file
  #pdf(pdf_file)
  # Define the file path for saving the PNG
  png_file <- glue("/Users/juanbolanos/Desktop/Phylogenetisc_Paper/Dornburg-Review/Final-Data/scripts/Visuals-CirclePlot-R/output/{DiagramTitle}_GROUPED_chord_diagram.png")
  
  # Save the chord diagram to a PNG file
  png(filename = png_file, width = 3000, height = 2400, res = 300)
  #chordDiagram(ChordData)
  
  
  grid.col = c(Off_Campus = "orange", 
               On_Campus="blue")
  
  
  # discrete
  #lgd_points = Legend(at = c("label1", "label2"), type = "points", legend_gp = gpar(col = 2:3), title_position = "topleft", title = "Track1")
  
  chordDiagram(ChordData,grid.col=grid.col,annotationTrack = c("name", "grid"))
  lgd_lines <- Legend(
    labels = c("Off Campus", "On Campus"),  # Labels for the legend
    legend_gp = gpar(fill = c("darkorange", "blue")),  # Colors for the legend
    title = "Transitions",  # Title for the legend
    title_gp = gpar(fontsize = 12, fontface = "bold"),  # Style for title
    labels_gp = gpar(fontsize = 10)  # Style for labels
  )
  
  #draw(lgd_lines, x = unit(4, "mm"), y = unit(4, "mm"), just = c("left", "bottom"))
  
  # Draw the legend
  # Draw the legend at the top-left position
  draw(lgd_lines, x = unit(0.05, "npc"), y = unit(0.80, "npc"), just = c("left", "top"))
  
  #draw(lgd_points)
  # Add title to the chord diagram
  title(DiagramTitle, cex = 1, outer = FALSE,line = -1)
  
  
  
  
  dev.off()
  
  chordDiagram(ChordData,grid.col=grid.col,annotationTrack = c("name", "grid"))
  #lgd_lines = Legend(at = c("On Campus", "Off Campus"), type = "lines", legend_gp = gpar(col = c(4,7), lwd = 2), title_position = "topleft", title = "Track2")
  lgd_lines <- Legend(
    labels = c("Off Campus", "On Campus"),  # Labels for the legend
    legend_gp = gpar(fill = c("darkorange", "blue")),  # Colors for the legend
    title = "Transitions",  # Title for the legend
    title_gp = gpar(fontsize = 12, fontface = "bold"),  # Style for title
    labels_gp = gpar(fontsize = 10)  # Style for labels
  )
  
  #draw(lgd_lines, x = unit(4, "mm"), y = unit(4, "mm"), just = c("left", "bottom"))
  
  # Draw the legend
  draw(lgd_lines, x = unit(0.05, "npc"), y = unit(0.80, "npc"), just = c("left", "top"))
  title(DiagramTitle, cex = 1, outer = FALSE,line = -1)
  print("Done")
  
  #emphasize small transitions
  #ChordData[ChordData < 100000000000] = "#00000000"
  #ChordData(data, grid.col = grid.col, transparency = 0) 
}
#?Legend()
#Type of legends. The value can be one of grid, points, lines and boxplot.

ChordDiagramPlotGrouped_v2(AllData,"Complete SARS-CoV-2 Transition Patterns\nBetween On-Campus and Off-Campus Populations")
#help with funciton
?chordDiagram()


ChordDiagramPlotGrouped_PDF_v2 <- function(a_csv, DiagramTitle) {
  library(circlize)
  library(glue)
  library(ComplexHeatmap)
  
  # Read data
   ChordData<-read.csv(a_csv)
  # PDF output settings
  pdf_file <- glue("/Users/juanbolanos/Desktop/Phylogenetisc_Paper/Dornburg-Review/Final-Data/scripts/Visuals-CirclePlot-R/output/{DiagramTitle}_GROUPED_chord_diagram.pdf")
  
  # Set up PDF device
  pdf(file = pdf_file, width = 10, height = 8)  # 10x8 inches (equivalent to 3000x2400 pixels at 300dpi)
  
  # Set up color scheme
  grid.col = c(Off_Campus = "orange", On_Campus = "blue")
  
  # Create chord diagram
  #chordDiagram(ChordData, grid.col = grid.col, annotationTrack = c("name", "grid"))
  chordDiagram(ChordData, grid.col = grid.col, annotationTrack = 'grid')
  # Create legend
  lgd_lines <- Legend(
    labels = c("Off Campus", "On Campus"),
    legend_gp = gpar(fill = c("darkorange", "blue")),
    title = "Transitions",
    title_gp = gpar(fontsize = 12, fontface = "bold"),
    labels_gp = gpar(fontsize = 10)
  )
  
  # Add legend and title
  draw(lgd_lines, x = unit(0.05, "npc"), y = unit(0.80, "npc"), just = c("left", "top"))
  
  #Removind tittel for final editing, uncomment if you wnat tittle. 
  #title(DiagramTitle, cex = 1, line = -1)
  title("", cex = 1, line = -1)
  
  # Close PDF device
  dev.off()
  
  print("Done")
}


#Individual Groups
#PreAlpha
# PreAlpha_csv="/Users/juanbolanos/Desktop/Phylogenetisc_Paper/Dornburg-Review/Final-Data/scripts/Visuals-CirclePlot-R/input/PreAlpha_Output_TransitionCounts_circlPlot-2024-03-19.csv"
# ChordDiagramPlot(PreAlpha_csv,"PreAlpha")
# 
# #Alpha
# Alpha_csv="/Users/juanbolanos/Desktop/Phylogenetisc_Paper/Dornburg-Review/Final-Data/scripts/Visuals-CirclePlot-R/input/Alpha_Output_TransitionCounts_circlPlot-2024-03-19.csv"
# ChordDiagramPlot(Alpha_csv,"Alpha")
# 
# #Delta
# Delta_csv="/Users/juanbolanos/Desktop/Phylogenetisc_Paper/Dornburg-Review/Final-Data/scripts/Visuals-CirclePlot-R/input/Delta_Output_TransitionCounts_circlPlot-2024-03-19.csv"
# ChordDiagramPlot(Delta_csv,"Delta")
# 
# #Omicron
# Omicorn_csv="/Users/juanbolanos/Desktop/Phylogenetisc_Paper/Dornburg-Review/Final-Data/scripts/Visuals-CirclePlot-R/input/Omicron_Output_TransitionCounts_circlPlot-2024-03-19.csv"
# ChordDiagramPlot(Omicorn_csv,"Omicron")






################################
#### Grouped Buildings ########
#### as on_Campus      ########
################################
# #PreALpha
# PreAlpha_csv="/Users/juanbolanos/Desktop/Phylogenetisc_Paper/Dornburg-Review/Final-Data/scripts/Visuals-CirclePlot-R/input/Grouped/PreAlphaGrouped_Output_TransitionCounts_circlPlot-2024-03-21.csv"
# ChordDiagramPlotGrouped(PreAlpha_csv,"PreAlpha_Grouped")
# 
# #Alpha
# Alpha_csv="/Users/juanbolanos/Desktop/Phylogenetisc_Paper/Dornburg-Review/Final-Data/scripts/Visuals-CirclePlot-R/input/Grouped/AlphaGrouped_Output_TransitionCounts_circlPlot-2024-03-21.csv"
# ChordDiagramPlotGrouped(Alpha_csv,"Alpha_Grouped")
# 
# #Delta
# Delta="/Users/juanbolanos/Desktop/Phylogenetisc_Paper/Dornburg-Review/Final-Data/scripts/Visuals-CirclePlot-R/input/Grouped/DeltaGrouped_Output_TransitionCounts_circlPlot-2024-03-21.csv"
# ChordDiagramPlotGrouped(Delta,"Delta_Grouped")
# 
# #Omicron
# Omicron="/Users/juanbolanos/Desktop/Phylogenetisc_Paper/Dornburg-Review/Final-Data/scripts/Visuals-CirclePlot-R/input/Grouped/OmicronGrouped_Output_TransitionCounts_circlPlot-2024-03-21.csv"
# ChordDiagramPlotGrouped(Omicron,"Omicron_Grouped")
# 
# #AllData
AllData="/Users/juanbolanos/Desktop/Phylogenetisc_Paper/Dornburg-Review/Final-Data/scripts/Visuals-CirclePlot-R/input/AllDataGrouped_Output_TransitionCounts_circlPlot-2024-04-15.csv"
ChordDiagramPlotGrouped_v2(AllData,"Complete SARS-CoV-2 Transition Patterns Between On-Campus and Off-Campus Populations")
ChordDiagramPlotGrouped_PDF_v2(AllData,"Complete SARS-CoV-2 Transition Patterns Between On-Campus and Off-Campus Populations")






#hypothetical data
#Balanced Transtitions
hyp="/Users/juanbolanos/Desktop/Phylogenetisc_Paper/Dornburg-Review/Final-Data/scripts/Visuals-CirclePlot-R/input/Hypothetical-AllData.csv"
ChordDiagramPlotGrouped_v2(hyp,"Hypothetical Balanced Transition Patterns\nBetween On-Campus and Off-Campus Populations")
ChordDiagramPlotGrouped_PDF_v2()
#Off campus dominating
hyp="/Users/juanbolanos/Desktop/Phylogenetisc_Paper/Dornburg-Review/Final-Data/scripts/Visuals-CirclePlot-R/input/Hypothetical-OnCampus-Dominating.csv"
ChordDiagramPlotGrouped_v2(hyp,"Dominant On-Campus Transition Patterns\nBetween On-Campus and Off-Campus Populations")
ChordDiagramPlotGrouped_PDF_v2()

#using the V2 diagram for fig 4
#Pre Alpha
PreAlpha="/Users/juanbolanos/Desktop/Phylogenetisc_Paper/Dornburg-Review/Final-Data/scripts/Visuals-CirclePlot-R/input/Grouped/PreAlphaGrouped_Output_TransitionCounts_circlPlot-2024-03-21.csv"
ChordDiagramPlotGrouped_v2(PreAlpha,"Pre Alpha\nComplete SARS-CoV-2 Transition Patterns Between On-Campus and Off-Campus Populations")
ChordDiagramPlotGrouped_PDF_v2(PreAlpha,"Pre Alpha\nComplete SARS-CoV-2 Transition Patterns Between On-Campus and Off-Campus Populations")

#Alpha
Alpha="/Users/juanbolanos/Desktop/Phylogenetisc_Paper/Dornburg-Review/Final-Data/scripts/Visuals-CirclePlot-R/input/Grouped/AlphaGrouped_Output_TransitionCounts_circlPlot-2024-03-21.csv"
ChordDiagramPlotGrouped_v2(Alpha,"Alpha\nComplete SARS-CoV-2 Transition Patterns Between On-Campus and Off-Campus Populations")
ChordDiagramPlotGrouped_PDF_v2(Alpha,"Alpha\nComplete SARS-CoV-2 Transition Patterns Between On-Campus and Off-Campus Populations")

#Delta
Delta="/Users/juanbolanos/Desktop/Phylogenetisc_Paper/Dornburg-Review/Final-Data/scripts/Visuals-CirclePlot-R/input/Grouped/DeltaGrouped_Output_TransitionCounts_circlPlot-2024-03-21.csv"
ChordDiagramPlotGrouped_v2(Delta,"Delta\nComplete SARS-CoV-2 Transition Patterns Between On-Campus and Off-Campus Populations")
ChordDiagramPlotGrouped_PDF_v2(Delta,"Delta\nComplete SARS-CoV-2 Transition Patterns Between On-Campus and Off-Campus Populations")

#Omicron
Omicron="/Users/juanbolanos/Desktop/Phylogenetisc_Paper/Dornburg-Review/Final-Data/scripts/Visuals-CirclePlot-R/input/Grouped/OmicronGrouped_Output_TransitionCounts_circlPlot-2024-03-21.csv"
ChordDiagramPlotGrouped_v2(Omicron,"Omicron\nComplete SARS-CoV-2 Transition Patterns Between On-Campus and Off-Campus Populations")
ChordDiagramPlotGrouped_PDF_v2(Omicron,"Omicron\nComplete SARS-CoV-2 Transition Patterns Between On-Campus and Off-Campus Populations")

#Bootstrap Data 
#1 to 1
bootstrap="/Users/juanbolanos/Desktop/Phylogenetisc_Paper/Dornburg-Review/Final-Data/scripts/Visuals-CirclePlot-R/input/Bootstrap/Bootstrap_1_to_1_Output_TransitionCounts_circlPlot-2025-02-05.csv"
#ChordDiagramPlotGrouped(bootstrap,"Bootstrap_1_to_1_")
ChordDiagramPlotGrouped_PDF_v2(bootstrap,"Bootstrap_1_to_1_")


#1 to 5
bootstrap="/Users/juanbolanos/Desktop/Phylogenetisc_Paper/Dornburg-Review/Final-Data/scripts/Visuals-CirclePlot-R/input/Bootstrap/Bootstrap_1_to_5_Output_TransitionCounts_circlPlot-2025-02-05.csv"
#ChordDiagramPlotGrouped(bootstrap,"Bootstrap_1_to_1_")
ChordDiagramPlotGrouped_PDF_v2(bootstrap,"Bootstrap_1_to_5_")


#1 to 3
bootstrap="/Users/juanbolanos/Desktop/Phylogenetisc_Paper/Dornburg-Review/Final-Data/scripts/Visuals-CirclePlot-R/input/Bootstrap/Bootstrap_2_to_3_Output_TransitionCounts_circlPlot-2025-02-05.csv"
#ChordDiagramPlotGrouped(bootstrap,"Bootstrap_1_to_1_")
ChordDiagramPlotGrouped_PDF_v2(bootstrap,"Bootstrap_3_to_3_")


################################
#### Histogram ################
################################
#"/Users/juanbolanos/Desktop/Phylogenetisc_Paper/Dornburg-Review/Final-Data/scripts/Visuals-CirclePlot-R/Histogram_section.csv"
# Read the CSV file skipping the first row
# HistData <- read.csv("/Users/juanbolanos/Desktop/Phylogenetisc_Paper/Dornburg-Review/Final-Data/scripts/Visuals-CirclePlot-R/input/Histogram_section.csv", skip = 1)
# 
# # Exclude the 'Transition' column
# HistData <- HistData[, -1]
# 
# # Convert non-numeric values to NA
# HistData <- apply(HistData, 2, function(x) ifelse(grepl("[^0-9.]", x), NA, as.numeric(x)))
# 
# # Remove rows with NA values
# HistData <- na.omit(HistData)
# 
# # Set up colors for each category
# colors <- rainbow(ncol(HistData))
# 
# # Set up plotting parameters
# par(mar = c(5, 4, 4, 2) + 0.1)
# 
# # Create a single histogram with different colors for each category
# barplot(HistData, beside = TRUE, col = colors, main = "Combined Histogram", xlab = "Values", ylab = "Frequency", legend.text = colnames(HistData))



