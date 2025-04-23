source("Data_functions.R")
library(ggplot2)
library(gridExtra)
library(grid)
library(reshape2)

# Custom function for plotting
plot_heatmap <- function(data, title_xaxis_yaxis) {
  # Replace "<5" with NA for plotting purposes
  data_with_na <- data
  data_with_na[data_with_na == "<5"] <- NA
  
  # Melt the data for ggplot, excluding NFWHM temporarily from the heatmap
  melted_data <- melt(data_with_na, id.vars = c("SD", "NFWHM"))
  
  # Create a separate label column to reintroduce "<5" in the text
  melted_data$label <- as.character(melted_data$value)
  
  # Loop over each variable and SD, and if the original value was "<5", reassign it to the label column
  for (i in seq_len(nrow(melted_data))) {
    # Find corresponding original value in the data frame
    var <- as.character(melted_data$variable[i])
    sd_val <- melted_data$SD[i]
    
    # Check if the value in the original data was "<5"
    if (!is.na(sd_val) && data[which(data$SD == sd_val), var] == "<5") {
      melted_data$label[i] <- "<5"
    }
  }
  
  # Convert the values to numeric for plotting (ignoring NA)
  melted_data$value <- as.numeric(melted_data$value)
  
  # Change the x-axis labels to reflect NFWHM
  NFWHM_labels <- as.character(data$NFWHM[!is.na(data$NFWHM)])
  
  # Heatmap using ggplot2
  ggplot(melted_data, aes(x = variable, y = SD, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient(low = "lightgreen", high = "darkred", na.value = "white", 
                        limits = c(5, 60),   # Set lower limit to 5 and upper limit to 50
                        breaks = c(5,25,45,60),  # Custom legend breaks
                        name = "SS") +  
    geom_text(aes(label = label), color = "black", size = 4, fontface = "bold") +  # Display "<5" for NA entries
    labs(title = title_xaxis_yaxis[1],
         x = title_xaxis_yaxis[2],
         y = title_xaxis_yaxis[3]) +
    scale_x_discrete(labels = NFWHM_labels) +  # Set x-axis labels to NFWHM values
    theme_minimal() +
    # Increase the font size of the plot
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          plot.title = element_text(size = 16, hjust = 0.5)) 
}

#SPM

# Update your data frame with NFWHM as the last column
Angle_SPM <- data.frame(
  SD = c(15, 10, 5, NA, NA),
  V1 = c(23, 12, 5, NA, NA),
  V2 = c(29, 15, 6, NA, NA),
  V3 = c(33, 17, 6, NA, NA),
  V4 = c(36, 18, 7, NA, NA),
  V5 = c(38, 19, 7, NA, NA),
  NFWHM = c(5, 15, 25, 35,45)  # NFWHM values
)




Angle_SnPM <- data.frame(
  SD = c(15, 10, 5, NA, NA),
  V1 = c(22, 12, 5, NA, NA),  # Replace "<5" with NA for plotting
  V2 = c(28, 14, 6, NA, NA),
  V3 = c(32, 17, 6, NA, NA),
  V4 = c(34, 17, 6, NA, NA),
  V5 = c(36, 17, 6, NA, NA),
  NFWHM = c(5, 15, 25, 35, 45)  # NFWHM values
)




Angle_IWT <- data.frame(
  SD = c(15, 10, 5, NA, NA),
  V1 = c(15, 8, "<5", NA, NA),  # Replace "<5" with NA for plotting
  V2 = c(22, 11, "<5", NA, NA),
  V3 = c(25, 13, "<5", NA, NA),
  V4 = c(28, 14, "<5", NA, NA),
  V5 = c(31, 15, 6, NA, NA),
  NFWHM = c(5, 15, 25, 35, 45)  # NFWHM values
)


#TWT

Angle_TWT <- data.frame(
  SD = c(15, 10, 5, NA, NA),
  V1 = c(9, 5,"<5", NA, NA),  # Replace "<5" with NA for plotting
  V2 = c(15, 8, "<5", NA, NA),
  V3 = c(19, 10, "<5", NA, NA),
  V4 = c(22, 11, "<5", NA, NA),
  V5 = c(26, 13, "<5", NA, NA),
  NFWHM = c(5, 15, 25, 35, 45)  # NFWHM values
)

#ERL
Angle_ERL <- data.frame(
  SD = c(15, 10, 5, NA, NA),
  V1 = c(22, 11, 8, NA, NA),
  V2 = c(28, 14, 6, NA, NA),
  V3 = c(32, 16, 6, NA, NA),
  V4 = c(35, 17, 6, NA, NA),
  V5 = c(37, 17, 6, NA, NA),
  NFWHM = c(5, 15, 25, 35,45)  # NFWHM values
)

#IATSE

Angle_IATSE <- data.frame(
  SD = c(15, 10, 5, NA, NA),
  V1 = c(23, 12, 5, NA, NA),
  V2 = c(31, 15, 6, NA, NA),
  V3 = c(35, 17, 6, NA, NA),
  V4 = c(38, 18, 6, NA, NA),
  V5 = c(39, 19, 7, NA, NA),
  NFWHM = c(5, 15, 25, 35,45)  # NFWHM values
)






MF_TWT <- data.frame(
  SD = c(50, 40, 30, NA, NA),
  V1 = c(14,10,6, NA, NA),  # Replace "<5" with NA for plotting
  V2 = c(24,15,10, NA, NA),
  V3 = c(32,21,12, NA, NA),
  V4 = c(35,23,12, NA, NA),
  V5 = c(39,24,15, NA, NA),
  NFWHM = c(5, 15, 25, 35, 45)  # NFWHM values
)

MF_IWT <- data.frame(
  SD = c(50, 40, 30, NA, NA),
  V1 = c(22,15,9, NA, NA),  # Replace "<5" with NA for plotting
  V2 = c(39,25,16, NA, NA),
  V3 = c(47,30,18, NA, NA),
  V4 = c(55,35,21, NA, NA),
  V5 = c(59,39,23, NA, NA),
  NFWHM = c(5, 15, 25, 35, 45)  # NFWHM values
)

MF_SPM <- data.frame(
  SD = c(50, 40, 30, NA, NA),
  V1 = c(15,11,8, NA, NA),  # Replace "<5" with NA for plotting
  V2 = c(16,11,8, NA, NA),
  V3 = c(15,10,7, NA, NA),
  V4 = c(14,10,7, NA, NA),
  V5 = c(12,9,6, NA, NA),
  NFWHM = c(5, 15, 25, 35, 45)  # NFWHM values
)


MF_SnPM <- data.frame(
  SD = c(50, 40, 30, NA, NA),
  V1 = c(16,11,8, NA, NA),  # Replace "<5" with NA for plotting
  V2 = c(16,11,8, NA, NA),
  V3 = c(15,10,7, NA, NA),
  V4 = c(14,10,7, NA, NA),
  V5 = c(13,9,7, NA, NA),
  NFWHM = c(5, 15, 25, 35, 45)  # NFWHM values
)

#ERL
MF_ERL <- data.frame(
  SD = c(15, 10, 5, NA, NA),
  V1 = c(16,11,8, NA, NA),  # Replace "<5" with NA for plotting
  V2 = c(18,11,9, NA, NA),
  V3 = c(16,11,8, NA, NA),
  V4 = c(15,11,8, NA, NA),
  V5 = c(13,10,7, NA, NA),
  NFWHM = c(5, 15, 25, 35,45)  # NFWHM values
)

#IATSE

MF_IATSE <- data.frame(
  SD = c(15, 10, 5, NA, NA),
  V1 = c(18,12,9, NA, NA),  # Replace "<5" with NA for plotting
  V2 = c(21,15,10, NA, NA),
  V3 = c(20,15,10, NA, NA),
  V4 = c(20,15,10, NA, NA),
  V5 = c(20,14,10, NA, NA),
  NFWHM = c(5, 15, 25, 35,45)  # NFWHM values
)




# Plot all the datasets using the function
# Assume plot_heatmap is a custom function for generating heatmaps.
# Replace the function calls with your actual plot_heatmap code.
plot1 <- plot_heatmap(Angle_SPM, c("", "", "SD"))
plot2 <- plot_heatmap(Angle_SnPM, c("", "", "SD"))
plot3 <- plot_heatmap(Angle_IWT, c("", "", "SD"))
plot4 <- plot_heatmap(Angle_TWT, c("", "", "SD"))
plot5 <- plot_heatmap(Angle_ERL, c("", "", "SD"))
plot6 <- plot_heatmap(Angle_IATSE, c("", "NFWHM", "SD"))


plot7 <- plot_heatmap(MF_SPM, c("", "", "SD"))
plot8 <- plot_heatmap(MF_SnPM, c("", "", "SD"))
plot9 <- plot_heatmap(MF_IWT, c("", "", "SD"))
plot10 <- plot_heatmap(MF_TWT, c("", "", "SD"))
plot11 <- plot_heatmap(MF_ERL, c("", "", "SD"))
plot12 <- plot_heatmap(MF_IATSE, c("", "NFWHM", "SD"))

# Create a title for each column
col1_title <- textGrob("Hip Felxion Angle", gp=gpar(fontsize=14, fontface="bold"))
col2_title <- textGrob("Muscle Force", gp=gpar(fontsize=14, fontface="bold"))

# Create a nullGrob to act as the spacer between columns
spacer <- nullGrob()


# Create row titles
row1_title <- textGrob("SPM", rot=90, gp=gpar(fontsize=12, fontface="bold"))
row2_title <- textGrob("SnPM", rot=90, gp=gpar(fontsize=12, fontface="bold"))
row3_title <- textGrob("IWT", rot=90, gp=gpar(fontsize=12, fontface="bold"))
row4_title <- textGrob("TWT", rot=90, gp=gpar(fontsize=12, fontface="bold"))
row5_title <- textGrob("ERL", rot=90, gp=gpar(fontsize=12, fontface="bold"))
row6_title <- textGrob("IATSE", rot=90, gp=gpar(fontsize=12, fontface="bold"))

# Arrange the plots in two columns and four rows
# Arrange the plots with increased space between columns using a spacer
# Arrange the plots with row titles and increased space between columns
grid.arrange(
  spacer, col1_title, spacer, col2_title,          # Column titles and spacer
  row1_title, plot1, spacer, plot7,                # Row 1 with row title and spacer
  row2_title, plot2, spacer, plot8,                # Row 2 with row title and spacer
  row3_title, plot3, spacer, plot9,                # Row 3 with row title and spacer
  row4_title, plot4, spacer, plot10,                # Row 4 with row title and spacer
  row5_title, plot5, spacer, plot11,                # Row 5 with row title and spacer
  row6_title, plot6, spacer, plot12,                # Row 6 with row title and spacer
  ncol=4,                                          # Four columns (row titles, left plot, spacer, right plot)
  layout_matrix = rbind(
                        c(1, 2, 3, 4),             # Titles row
                        c(5, 6, 7, 8),             # First row of plots with row title
                        c(9, 10, 11, 12),          # Second row of plots with row title
                        c(13, 14, 15, 16),         # Third row of plots with row title
                        c(17, 18, 19, 20),         # Fourth row of plots with row title
                        c(21, 22, 23, 24),        # Fifth row of plots with row title
                        c(25, 26, 27, 28)         # Sixth row of plots with row title
  
                        ),        
  heights = c(0.15, 1, 1, 1, 1, 1, 1),                   # Control the height: less space for titles
  widths = c(0.1, 1, 0.15, 1)                      # Control the width: space for row titles and columns
)



MF <- MF_data("both")
Angele <- Angle_data("both")

#plot these two plot using ggplot2 y axis is value and x is the index and use three colors, two for columns and one for difference between two columns
# Use the melt function from the reshape2 package to convert the data to long format

####################################
#SPM

# Update your data frame with NFWHM as the last column
Angle_SPM <- data.frame(
  SD = c(15, 10, 5, NA, NA, NA, NA, NA, NA, NA),
  V1 = c(23, 12, 5, NA, NA, NA, NA, NA, NA, NA),
  V2 = c(29, 15, 6, NA, NA, NA, NA, NA, NA, NA),
  V3 = c(33, 17, 6, NA, NA, NA, NA, NA, NA, NA),
  V4 = c(36, 18, 7, NA, NA, NA, NA, NA, NA, NA),
  V5 = c(38, 19, 7, NA, NA, NA, NA, NA, NA, NA),
  V6 = c(38, 19, 7, NA, NA, NA, NA, NA, NA, NA),
  V7 = c(39, 18, 7, NA, NA, NA, NA, NA, NA, NA),
  V8 = c(35, 17, 6, NA, NA, NA, NA, NA, NA, NA),
  V9 = c(33, 16, 6, NA, NA, NA, NA, NA, NA, NA),
  V10 = c(31, 16, 6, NA, NA, NA, NA, NA, NA, NA),
  NFWHM = seq(5,95,10)  # NFWHM values
)








Angle_SnPM <- data.frame(
  SD = c(15, 10, 5, NA, NA, NA, NA, NA, NA, NA),
  V1 = c(22, 12, 5, NA, NA, NA, NA, NA, NA, NA),  # Replace "<5" with NA for plotting
  V2 = c(28, 14, 6, NA, NA, NA, NA, NA, NA, NA),
  V3 = c(32, 17, 6, NA, NA, NA, NA, NA, NA, NA),
  V4 = c(34, 17, 6, NA, NA, NA, NA, NA, NA, NA),
  V5 = c(36, 17, 6, NA, NA, NA, NA, NA, NA, NA),
  V6 = c(35, 17, 6, NA, NA, NA, NA, NA, NA, NA),
  V7 = c(34, 16, 6, NA, NA, NA, NA, NA, NA, NA),
  V8 = c(33, 16, 6, NA, NA, NA, NA, NA, NA, NA),
  V9 = c(32, 16, 6, NA, NA, NA, NA, NA, NA, NA),
  V10 = c(31, 15, 6, NA, NA, NA, NA, NA, NA, NA),
  NFWHM = seq(5,95,10)  # NFWHM values
)




Angle_IWT <- data.frame(
  SD = c(15, 10, 5, NA, NA, NA, NA, NA, NA, NA),
  V1 = c(15, 8, "<5", NA, NA, NA, NA, NA, NA, NA),  # Replace "<5" with NA for plotting
  V2 = c(22, 11, "<5", NA, NA, NA, NA, NA, NA, NA),
  V3 = c(25, 13, "<5", NA, NA, NA, NA, NA, NA, NA),
  V4 = c(28, 14, "<5", NA, NA, NA, NA, NA, NA, NA),
  V5 = c(31, 15, 6, NA, NA, NA, NA, NA, NA, NA),
  V6 = c(33, 16, 6, NA, NA, NA, NA, NA, NA, NA),
  V7 = c(35, 17, 6, NA, NA, NA, NA, NA, NA, NA),
  V8 = c(35, 17, 6, NA, NA, NA, NA, NA, NA, NA),
  V9 = c(35, 17, 6, NA, NA, NA, NA, NA, NA, NA),
  V10 = c(35, 17, 6, NA, NA, NA, NA, NA, NA, NA),
  NFWHM = seq(5,95,10)  # NFWHM values
)


#TWT

Angle_TWT <- data.frame(
  SD = c(15, 10, 5, NA, NA, NA, NA, NA, NA, NA),
  V1 = c(9, 5,"<5", NA, NA, NA, NA, NA, NA, NA),  # Replace "<5" with NA for plotting
  V2 = c(15, 8, "<5", NA, NA, NA, NA, NA, NA, NA),
  V3 = c(19, 10, "<5", NA, NA, NA, NA, NA, NA, NA),
  V4 = c(22, 11, "<5", NA, NA, NA, NA, NA, NA, NA),
  V5 = c(26, 13, "<5", NA, NA, NA, NA, NA, NA, NA),
  V6 = c(29, 14, 6, NA, NA, NA, NA, NA, NA, NA),
  V7 = c(31, 15, 6, NA, NA, NA, NA, NA, NA, NA),
  V8 = c(33, 16, 6, NA, NA, NA, NA, NA, NA, NA),
  V9 = c(34, 17, 6, NA, NA, NA, NA, NA, NA, NA),
  V10 = c(34, 17, 6, NA, NA, NA, NA, NA, NA, NA),
  NFWHM = seq(5,95,10)  # NFWHM values
)

#ERL
Angle_ERL <- data.frame(
  SD = c(15, 10, 5, NA, NA, NA, NA, NA, NA, NA),
  V1 = c(22, 11, 8, NA, NA, NA, NA, NA, NA, NA),
  V2 = c(28, 14, 6, NA, NA, NA, NA, NA, NA, NA),
  V3 = c(32, 16, 6, NA, NA, NA, NA, NA, NA, NA),
  V4 = c(35, 17, 6, NA, NA, NA, NA, NA, NA, NA),
  V5 = c(37, 17, 6, NA, NA, NA, NA, NA, NA, NA),
  V6 = c(36, 17, 6, NA, NA, NA, NA, NA, NA, NA),
  V7 = c(35, 17, 6, NA, NA, NA, NA, NA, NA, NA),
  V8 = c(34, 16, 6, NA, NA, NA, NA, NA, NA, NA),
  V9 = c(33, 16, 6, NA, NA, NA, NA, NA, NA, NA),
  V10 = c(32, 16, 6, NA, NA, NA, NA, NA, NA, NA),
  NFWHM = seq(5,95,10)  # NFWHM values
)

#IATSE

Angle_IATSE <- data.frame(
  SD = c(15, 10, 5, NA, NA, NA, NA, NA, NA, NA),
  V1 = c(23, 12, 5, NA, NA, NA, NA, NA, NA, NA),
  V2 = c(31, 15, 6, NA, NA, NA, NA, NA, NA, NA),
  V3 = c(35, 17, 6, NA, NA, NA, NA, NA, NA, NA),
  V4 = c(38, 18, 6, NA, NA, NA, NA, NA, NA, NA),
  V5 = c(39, 19, 7, NA, NA, NA, NA, NA, NA, NA),
  V6 = c(40, 20, 7, NA, NA, NA, NA, NA, NA, NA),
  V7 = c(41, 20, 7, NA, NA, NA, NA, NA, NA, NA),
  V8 = c(42, 20, 7, NA, NA, NA, NA, NA, NA, NA),
  V9 = c(42, 20, 7, NA, NA, NA, NA, NA, NA, NA),
  V10 = c(41, 20, 7, NA, NA, NA, NA, NA, NA, NA),
  NFWHM = seq(5,95,10)  # NFWHM values
)

plot1 <- plot_heatmap(Angle_SPM, c("", "", "SD"))
plot2 <- plot_heatmap(Angle_SnPM, c("", "", "SD"))
plot3 <- plot_heatmap(Angle_IWT, c("", "", "SD"))
plot4 <- plot_heatmap(Angle_TWT, c("", "", "SD"))
plot5 <- plot_heatmap(Angle_ERL, c("", "", "SD"))
plot6 <- plot_heatmap(Angle_IATSE, c("", "NFWHM", "SD"))

# Create a title for the column
col_title <- textGrob("Hip Felxion Angle", gp=gpar(fontsize=14, fontface="bold"))
# Create row names
row1_title <- textGrob("SPM", rot=90, gp=gpar(fontsize=12, fontface="bold"))
row2_title <- textGrob("SnPM", rot=90, gp=gpar(fontsize=12, fontface="bold"))
row3_title <- textGrob("IWT", rot=90, gp=gpar(fontsize=12, fontface="bold"))
row4_title <- textGrob("TWT", rot=90, gp=gpar(fontsize=12, fontface="bold"))
row5_title <- textGrob("ERL", rot=90, gp=gpar(fontsize=12, fontface="bold"))
row6_title <- textGrob("IATSE", rot=90, gp=gpar(fontsize=12, fontface="bold"))

# Arrange the plots in two columns and 6 rows
grid.arrange(
  col_title,          # Column title
  row1_title, plot1,  # Row 1 with row title
  row2_title, plot2,  # Row 2 with row title
  row3_title, plot3,  # Row 3 with row title
  row4_title, plot4,  # Row 4 with row title
  row5_title, plot5,  # Row 5 with row title
  row6_title, plot6,  # Row 6 with row title
  ncol=2,             # Two columns (row titles, plots)
  layout_matrix = rbind(
    c(1, 1),           # Column title
    c(2, 3),           # First row of plots with row title
    c(4, 5),           # Second row of plots with row title
    c(6, 7),           # Third row of plots with row title
    c(8, 9),           # Fourth row of plots with row title
    c(10, 11),         # Fifth row of plots with row title
    c(12, 13)          # Sixth row of plots with row title
  ),
  heights = c(0.15, 1, 1, 1, 1, 1, 1),  # Control the height: less space for titles
  widths = c(0.1, 1) 
)


######################################
Data_plot <- function(dataset,TITLE){
  cont_size <- dim(dataset)[1]
  # Create a data frame with the two-sample data
  plot_data <- data.frame(
    x_values = rep(0:(cont_size - 1), 3),  # Repeat x_values for 3 lines
    y_values = c((dataset[, 1] - dataset[, 2]), #first - second column
                 dataset[, 1], dataset[, 2]),  # Combine all y-values
    legend = factor(rep(c("Pulse", colnames(dataset)[1], colnames(dataset)[2]), 
                        each = dim(dataset)[1]))  # Control factor levels
  )
  
  # Use setNames to create color_values dynamically
  color_values <- setNames(c("black", "cadetblue", "tomato"),
                           c("Pulse", 
                             colnames(dataset)[1], 
                             colnames(dataset)[2]))
  
  
  
  
  # Create the plot using ggplot
  ggplot(plot_data, aes(x = x_values, y = y_values, color = legend)) +
    geom_line(linewidth = 1.5) +  # Plot lines for each group
    labs(title = TITLE, x = "Index", y = "Value") +  # Labels
    scale_color_manual(values = color_values) +  # Line colors
    theme_minimal() +  # Use a minimal theme
    theme(plot.title = element_text(hjust = 0.5)) +  # Center the title
    theme(legend.position = "bottom")+
    #increase the font size of the labels and axis numbers
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          plot.title = element_text(size = 16),
          legend.text = element_text(size = 12),
          legend.title = element_blank())
  
  
}

MF <- MF_data("both")
Angele <- Angle_data("both")

plot2<- Data_plot(MF, "Muscle Force")
plot1<- Data_plot(Angele, "Hip Flexion Angle")
#plot plot1 and 2 in a same row
grid.arrange(plot1, plot2, ncol=2)



Angle_IATSE <- data.frame(
  SD = c(15, 10, 5, NA, NA),
  V1 = c(22, 12, 5, NA, NA),
  V2 = c(28, 14, 6, NA, NA),
  V3 = c(30, 15, 6, NA, NA),
  V4 = c(32, 16, 6, NA, NA),
  V5 = c(34, 16, 6, NA, NA),
  NFWHM = c(5, 15, 25, 35,45)  # NFWHM values
)


Angle_ERL <- data.frame(
  SD = c(15, 10, 5, NA, NA),
  V1 = c(20, 10, 8, NA, NA),
  V2 = c(25, 13, 7, NA, NA),
  V3 = c(28, 15, 6, NA, NA),
  V4 = c(29, 15, 7, NA, NA),
  V5 = c(30, 15, 7, NA, NA),
  NFWHM = c(5, 15, 25, 35,45)  # NFWHM values
)

#split screen into two row
par(mfrow=c(2,1))

plot_heatmap(Angle_ERL,c("ERL","NFWHM","SD"))
plot_heatmap(Angle_IATSE,c("IATSE","NFWHM","SD"))
