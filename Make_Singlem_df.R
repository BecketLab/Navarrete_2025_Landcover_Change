library(tidyverse)
library(zCompositions)
library(vegan)
library(pairwiseAdonis)
library(stringr)
library(ggplot2)
library(ggvegan)
library(dplyr)
library(ggforce)
library(purrr)


setwd("~/BecketLab_R/Soil/Profiling/data/singleM/profile.tsv")


samdf <- read.delim("~/BecketLab_R/Soil/Profiling/data/soil_metadata_2.txt", 
                    sep = "\t", 
                    header = T, 
                    stringsAsFactors=FALSE)

# Get a list of all SingleM profile output files files in the directory
file_list <- list.files("~/BecketLab_R/Soil/Profiling/data/singleM/profile.tsv/",pattern="\\_profile.tsv$")

# Use purrr::map() to read all files into a list of dataframes
data_list <- purrr::map(file_list, ~read.delim(.x, header = TRUE) %>% dplyr::select(taxonomy, coverage))
                        
names(data_list) <- file_list

############################################################## Process the data
# Use purrr::reduce() to join dataframes by shared variable Name
joined_data <- purrr::reduce(data_list, full_join, by = "taxonomy")

# Extract data_list names
sam_names <- names(data_list)
# Only keep the sam_names text to the left of the second "_"
sam_names <- str_extract(names(data_list), "^[^_]+_[^_]+")
# Rename Cov columns to data_list names
names(joined_data)[grep("coverage", names(joined_data))] <- sam_names

# Replace NA with 0 in the entire data frame
joined_data[is.na(joined_data)] <- 0


# move first column to be last column
joined_data <- joined_data[c(2:ncol(joined_data), 1)]

# Split the last column into a list
split_data <- strsplit(as.character(joined_data[, ncol(joined_data)]), "; ")

# Determine the maximum number of splits to create equal length vectors
max_length <- max(sapply(split_data, length))

# Standardize the length of each vector
standardized_splits <- lapply(split_data, function(x) {
  c(x, rep(NA, max_length - length(x)))
})

# Convert the list to a data frame
split_df <- as.data.frame(do.call(rbind, standardized_splits))

# Combine with the original data frame (excluding the last column)
joined_data <- cbind(joined_data[, -ncol(joined_data)], split_df)

# Names of the last 8 columns to be renamed
new_column_names <- c("Root", "Domain", "Phylum", "Class", "Order", "Family", "Genus", "Species")

# Rename the last 8 columns
names(joined_data)[(ncol(joined_data)-7):ncol(joined_data)] <- new_column_names

# Replace NA in 'Phylum' column with "p__"
joined_data$Phylum[is.na(joined_data$Phylum)] <- "p__unclassified"

# Replace NA in 'Class' column with "c__"
joined_data$Class[is.na(joined_data$Class)] <- "c__unclassified"

# Replace NA in 'Order' column with "o__"
joined_data$Order[is.na(joined_data$Order)] <- "o__unclassified"

# Replace NA in 'Family' column with "f__"
joined_data$Family[is.na(joined_data$Family)] <- "f__unclassified"

# Replace NA in 'Genus' column with "g__"
joined_data$Genus[is.na(joined_data$Genus)] <- "g__unclassified"

# Replace NA in 'Species' column with "s__"
joined_data$Species[is.na(joined_data$Species)] <- "s__unclassified"

###
save(joined_data, file = "~/BecketLab_R/Soil/Profiling/data/Singlem_df.RData")
##############








