library(dplyr)
library(ggplot2)
library(purrr)
library(scales)


# Overview:
# This script reads in the results from the LWF model summary files, filters the data by habitat type,
# and creates bar plots comparing the model performance metrics (CV PDE, Validation AUC, and Validation PDE) for each habitat type.

################################################################################

# Set working directory to where the Model Summary files are stored
setwd("C:/Path/To/Your/Model_Summary_Files")

# File Input
# Code to read in multiple Model Summary files and combine them, extracting Validation.AUC, and Validation.PDE
ModSumm_list <- list.files(path = getwd(), pattern = "*.csv$", full.names = TRUE, recursive = FALSE)
hab_results <- data.frame()
# Read the first file to initialize the data frame
hab_results <- read.csv(ModSumm_list[1], header = FALSE)
# Add a row to the data frame called 'LWF' that will be either 'Y' or 'N' 
# if the filename contains '_noLWF' then 'N' otherwise 'Y'
# Add row to hab_results
hab_results <- rbind(hab_results, c("LWF", ifelse(grepl("_noLWF", ModSumm_list[1]), "N", "Y")))

# Loop through the remaining files and bind the second column to hab_results
for(i in ModSumm_list[2:length(ModSumm_list)]){
  print(i)
  ModSum_df <- read.csv(i, header = FALSE)
  ModSum_df <- rbind(ModSum_df, c("LWF", ifelse(grepl("_noLWF", i), "N", "Y")))
  hab_results <- cbind(hab_results, ModSum_df[,2])
}

# Make the first column the row names
rownames(hab_results) <- hab_results$V1
# Remove the first column (V1) as it is now the row names
hab_results <- hab_results[,-1]

# Transpose the data frame to have responses as rows and metrics as columns
# str(hab_results)
hab_results_t <- as.data.frame(t(hab_results))
rownames(hab_results_t) <- NULL


# Create a vector of unique response labels
habs <- unique(hab_results_t$`Response Variable`)
str(habs)

# Create a lookup table for short names and labels
lookup_table <- data.frame(
  ShortName = c("C_Bare", "LC_OthBranCol", "LC_OthBran", "LC_Stagh", "LC_Ipal"),
  Label = c("Bare", "Branching + Columnar", "Branching", "Staghorn", "Isopora palifera")
)

# Remove columns and Rename the metric columns for clarity
hab_results_t_new <- hab_results_t %>%
  select(Response = `Response Variable`, Validation.AUC = `AUC`, 
         Validation.PDE = `Validation Percent Deviance Explained`, LWM = `LWF`)

hab_results_t_new$ShortName <- lookup_table[match(hab_results_t_new$Response, lookup_table$ShortName), "Label"]

# Convert Validation.AUC and Validation.PDE to numeric
hab_results_t_new$Validation.AUC <- as.numeric(hab_results_t_new$Validation.AUC)
hab_results_t_new$Validation.PDE <- as.numeric(hab_results_t_new$Validation.PDE)

# Format Validation.AUC and Validation.PDE to 3 decimal places
hab_results_t_new$Validation.AUC <- format(round(hab_results_t_new$Validation.AUC, 3), nsmall = 3)
hab_results_t_new$Validation.PDE <- format(round(hab_results_t_new$Validation.PDE, 3), nsmall = 3)

# Convert Validation.AUC and Validation.PDE to numeric (need to redo this for plotting)
hab_results_t_new$Validation.AUC <- as.numeric(hab_results_t_new$Validation.AUC)
hab_results_t_new$Validation.PDE <- as.numeric(hab_results_t_new$Validation.PDE)

################################################################################
# Combine results for all taxa into a single plot

# Make sure the Shortnames are factored and ordered correctly
hab_results_t_new$ShortName  <- factor(hab_results_t_new$ShortName,levels = c("Bare", "Branching + Columnar",
                                                              "Branching", "Staghorn",
                                                              "Isopora palifera"))

# Validation AUC
ggplot(hab_results_t_new, aes(ShortName, Validation.AUC, fill = LWM)) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, .25, .5, .75, 1)) +
  coord_cartesian(ylim = c(.5, 1)) + #Zoom to upper section of graph
  geom_bar(stat="identity", position = "dodge") +
  geom_text(aes(label = Validation.AUC), fontface = "bold", vjust = 1.5,
            position = position_dodge(.9), size = 4) +
  scale_fill_grey(start = .4, end = .7) +
  theme_bw() +
  labs(x = "\n Taxa Type", y = "Validation AUC\n", title = "\n Validation AUC Comparison \n")


ggplot(hab_results_t_new, aes(ShortName, Validation.PDE, fill = LWM)) +
  scale_y_continuous(limits = c(0, 45), breaks = c(0, 10, 20, 30, 40)) +
  geom_bar(stat="identity", position = "dodge") + 
  geom_text(aes(label = Validation.PDE), fontface = "bold", vjust = 1.5,
            position = position_dodge(.9), size = 4) +
  scale_fill_grey(start = .4, end = .7) +
  theme_bw() +
  labs(x = "\n Taxa Type", y = "Validation PDE\n", title = "\n Validation PDE Comparison \n")  
# Save the plots
ggsave("Validation_AUC_Comparison.png", width = 8, height = 6)
ggsave("Validation_PDE_Comparison.png", width = 8, height = 6)

# Save the combined results to a CSV file
write.csv(hab_results_t_new, "Combined_LWF_Results.csv", row.names = FALSE)
# Save the workspace
save.image("LWF_Results_Workspace.RData")

# End of script
################################################################################

