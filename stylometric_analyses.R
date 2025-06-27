# stylometric_analyses

# read configuration file
config <- read.csv("Analysis_configuration.csv", stringsAsFactors = F)

# define variables for the analysis
my_corpus <- config$value[which(config$feature == "my_corpus")]
my_distance <- config$value[which(config$feature == "my_distance")]
MFW_base <- as.numeric(config$value[which(config$feature == "MFW_base")])
MFW_evolution_analysis <- as.logical(config$value[which(config$feature == "MFW_evolution_analysis")])
word_importance_analysis <- as.logical(config$value[which(config$feature == "word_importance_analysis")])
word_removal_analysis <- as.logical(config$value[which(config$feature == "word_removal_analysis")])
words_to_remove <- unlist(strsplit(config$value[which(config$feature == "words_to_remove")], " "))

# load libraries
library(stylo)
library(magrittr)
library(ggplot2)
library(tidyverse)
library(ggpubr)

# Cleaning function
clean_texts <- function(Kolimo_texts){
  for(i in 1:length(Kolimo_texts)){
    Kolimo_string <- Kolimo_texts[i]
    ##substitute old S
    Kolimo_string <- gsub("ſ", "s", Kolimo_string)
    ##substitute umlauts and double S
    Kolimo_string <- gsub("ä", "ae", Kolimo_string)
    Kolimo_string <- gsub("ö", "oe", Kolimo_string)
    Kolimo_string <- gsub("ü", "ue", Kolimo_string)
    Kolimo_string <- gsub("ß", "ss", Kolimo_string)
    
    ##substitute line breaks
    Kolimo_string <- gsub("([a-z])-\\s+([a-z])", "\\1\\2", Kolimo_string)
    Kolimo_string <- gsub("([a-z])¬\\s+([a-z])", "\\1\\2", Kolimo_string)
    
    ##delete multiple spaces
    Kolimo_string <- gsub(pattern = " +", replacement = " ", x = Kolimo_string)
    
    ##save new version
    Kolimo_texts[i] <- Kolimo_string
    print(i)
  }
  return(Kolimo_texts)
}

# splitting function
text_process <- function(my_texts, length_limit = 5000){
  texts_split <- split(my_texts, ceiling(seq_along(my_texts)/length_limit))
  texts_split <- texts_split[-length(texts_split)]
  print(length(texts_split))
  return(texts_split)
} 

# create dir to store results
analysis_dir <- paste("results_", my_corpus, "_MFWbase", MFW_base, "/", sep = "")
dir.create(analysis_dir)

# list all files in corpus
all_files <- list.files(my_corpus, pattern = ".txt", full.names = T)

# read and process them
all_texts <- lapply(all_files, readLines) %>% 
  lapply(function(x) paste(x, collapse = " ")) %>% 
  lapply(clean_texts) %>%
  stylo::txt.to.words.ext(corpus.lang = "German")
names(all_texts) <- gsub(paste(my_corpus,"/|.txt", sep = ""), "", all_files)

# remove the pronouns
if(length(words_to_remove)>0){
  
  for(i in 1:length(all_texts)){
    
    all_texts[[i]] <- all_texts[[i]][-which(all_texts[[i]] %in% words_to_remove)]
    
  }
  
}

my_candidates <- names(all_texts)[2:length(all_texts)] %>%
  strsplit(split = "_") %>%
  sapply(function(x) x[1])

# Process by splitting into 5,000-word chunks

all_texts_split <- list()
for(i in 2:length(all_texts)){
  all_texts_split[[i-1]] <- text_process(all_texts[[i]])
  names(all_texts_split[[i-1]]) <- paste(my_candidates[i-1], 1:length(all_texts_split[[i-1]]), sep = "_")
}

all_texts_split <- unlist(all_texts_split, recursive = F)
parsed_corpus <- c(all_texts[1], all_texts_split)

# first analysis with base MFW
stylo_results <- stylo(gui = F, 
                       parsed.corpus = parsed_corpus, 
                       corpus.lang = "German",
                       mfw.min = MFW_base,
                       mfw.max = MFW_base,
                       distance.measure = my_distance
)

# get frequency table
my_frequencies <- read.csv("table_with_frequencies.txt", sep = " ")
my_frequencies <- t(my_frequencies)

# get evolution of distances with varying MFW
if(MFW_evolution_analysis){
  
  all_distances <- list()
  for(my_fmw in 2:(MFW_base-length(words_to_remove))){
    
    stylo_results <- stylo(gui = F, 
                           frequencies = my_frequencies, 
                           corpus.lang = "German",
                           mfw.min = my_fmw,
                           mfw.max = my_fmw,
                           distance.measure = my_distance
    )
    
    # get distances per candidate
    my_distances <- stylo_results$distance.table[1,2:dim(stylo_results$distance.table)[1]]
    my_candidates <- names(my_distances) %>% strsplit(split = "_") %>% sapply(function(x) x[1])
    all_distances[[my_fmw]] <- tapply(my_distances, my_candidates, mean)
    
  }
  
  unlink("*EDGES.csv")
  
  # Convert data to a data frame
  df <- as.data.frame(do.call(rbind, all_distances[-1]))
  
  # Add an index column
  df$MFW <- 2:(nrow(df)+1)
  
  # Reshape the data for ggplot
  df_long <- gather(df, key = "Candidate", value = "Distance", -MFW)
  
  if(length(words_to_remove) == 0){
    my_title <- paste("Distances evolution\nDistance:", my_distance)
  }else{
    my_title <- paste("Distances evolution after removal of:", paste(words_to_remove, collapse = " "), "\nDistance:", my_distance)
  }
  
  # Plot using ggplot
  p1 <- ggplot(df_long, aes(x = MFW, y = Distance, color = Candidate)) +
    geom_line() +
    scale_color_manual(values = c("red", "blue", "green"))
  
  if(length(words_to_remove) == 0){
    my_filename <- paste(analysis_dir, "Distances_", gsub("dist.", "", my_distance), ".png", sep = "")
  }else{
    my_filename <- paste(analysis_dir, "Distances_", gsub("dist.", "", my_distance), "_remove(", paste(words_to_remove, collapse = "_"), ").png", sep = "")
  }
  
  ggsave(p1, filename = my_filename, width = 16, height = 9, scale = 0.5)
  
}

# importance of MFW
if(word_importance_analysis){
  
  # get basic distances
  stylo_results <- stylo(gui = F, 
                         parsed.corpus = parsed_corpus, 
                         corpus.lang = "German",
                         mfw.min = MFW_base-length(words_to_remove),
                         mfw.max = MFW_base-length(words_to_remove),
                         distance.measure = my_distance
  )
  
  my_distances <- stylo_results$distance.table[1,2:dim(stylo_results$distance.table)[1]]
  my_candidates <- names(my_distances) %>% strsplit(split = "_") %>% sapply(function(x) x[1])
  basic_distances <- tapply(my_distances, my_candidates, mean)
  
  # get analyzed words
  my_words <- stylo_results$features.actually.used
  my_words
  
  # get all frequencies
  all_freqs <- read.csv("table_with_frequencies.txt", sep = " ")
  all_freqs <- all_freqs[1:(MFW_base-length(words_to_remove)),]
  
  # get evolution of distances with varying MFW
  all_distances <- list()
  for(my_word in 1:(MFW_base-length(words_to_remove))){
    
    all_freqs_tmp <- all_freqs[-my_word,]
    
    stylo_results <- stylo(gui = F, 
                           frequencies = t(all_freqs_tmp), 
                           corpus.lang = "German",
                           mfw.min = MFW_base-length(words_to_remove)-1,
                           mfw.max = MFW_base-length(words_to_remove)-1,
                           distance.measure = my_distance
    )
    
    # get distances per candidate
    my_distances <- stylo_results$distance.table[1,2:dim(stylo_results$distance.table)[1]]
    my_candidates <- names(my_distances) %>% strsplit(split = "_") %>% sapply(function(x) x[1])
    all_distances[[my_word]] <- tapply(my_distances, my_candidates, mean)
    
  }
  
  unlink("*EDGES.csv")
  
  # Convert data to a data frame
  df <- as.data.frame(do.call(rbind, all_distances))
  df$klein <- df$klein - basic_distances[1]
  df$salten <- df$salten - basic_distances[2]
  df$schnitzler <- df$schnitzler - basic_distances[3]
  df$importance <- df$klein - df$salten
  df$word <- as.factor(my_words)
  
  # Convert 'word' column into a factor with levels ordered by 'importance'
  df$word <- factor(df$word, levels = df$word[order(df$importance, decreasing = TRUE)])
  
  df <- df[order(df$importance),]
  # limit to 50 words
  if(MFW_base > 50){
    df_2 <- df[(length(df$word)-49):length(df$word),]
  }else{
    df_2 <- df
  }
    
  # Create the bar plot
  if(MFW_base > 50){
    p1 <- ggplot(df_2, aes(x = word, y = importance)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      labs(x = "Word", y = "Klein") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
  }else{
    p1 <- ggplot(df_2, aes(x = word, y = importance)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      labs(x = "Word", y = "Salten                       Klein         ") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
  }
  
  if(length(words_to_remove) == 0){
    my_filename <- paste(analysis_dir, "Importance_", gsub("dist.", "", my_distance), ".png", sep = "")
  }else{
    my_filename <- paste(analysis_dir, "Importance_", gsub("dist.", "", my_distance), "_remove(", paste(words_to_remove, collapse = "_"), ").png", sep = "")
  }
  
  ggsave(p1, filename = my_filename, width = 16, height = 9, scale = 35/70)
  
  ## word usages
  
  my_limit <- length(which(df$importance > 0))
  
  all_freqs <- all_freqs[which(rownames(all_freqs) %in% levels(df$word)[1:my_limit]),]
  
  word_ordered <- levels(df$word)[1:my_limit]
  
  df_zscores <- as.data.frame(t(apply(all_freqs, 1, scale)))
  colnames(df_zscores) <- colnames(all_freqs)
  
  # Reshape data into longer format
  df_long <- df_zscores %>%
    rownames_to_column(var = "Group") %>%
    pivot_longer(cols = -Group, names_to = "Variable") %>%
    separate(Variable, into = c("Factor", "Index"), sep = "_") %>%
    select(-Index)
  
  # Create boxplot
  p1 <- ggplot(df_long, aes(x = factor(Group, levels = word_ordered), y = value, fill = Factor)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_boxplot() +
    labs(x = "Word",
         y = "z-score",
         fill = "Group")
  
  if(length(words_to_remove) == 0){
    my_filename <- paste(analysis_dir, "Zscores_", gsub("dist.", "", my_distance), ".png", sep = "")
  }else{
    my_filename <- paste(analysis_dir, "Zscores_", gsub("dist.", "", my_distance), "_remove(", paste(words_to_remove, collapse = "_"), ").png", sep = "")
  }
  
  ggsave(p1, filename = my_filename, width = 16, height = 9, scale = MFW_base/90)
  
}

# word removal analysis

if(length(words_to_remove)>0 & word_removal_analysis){
  
  # 1. analysis without removing words
  
  # read and process files
  all_texts <- lapply(all_files, readLines) %>% 
    lapply(function(x) paste(x, collapse = " ")) %>% 
    lapply(clean_texts) %>%
    stylo::txt.to.words.ext(corpus.lang = "German")
  names(all_texts) <- gsub(paste(my_corpus,"/|.txt", sep = ""), "", all_files)
  
  my_candidates <- names(all_texts)[2:length(all_texts)] %>%
    strsplit(split = "_") %>%
    sapply(function(x) x[1])
  
  # Process by splitting into 5,000-word chunks
  
  all_texts_split <- list()
  for(i in 2:length(all_texts)){
    all_texts_split[[i-1]] <- text_process(all_texts[[i]])
    names(all_texts_split[[i-1]]) <- paste(my_candidates[i-1], 1:length(all_texts_split[[i-1]]), sep = "_")
  }
  
  all_texts_split <- unlist(all_texts_split, recursive = F)
  parsed_corpus <- c(all_texts[1], all_texts_split)
  
  # first analysis with base MFW
  stylo_results <- stylo(gui = F, 
                         parsed.corpus = parsed_corpus, 
                         corpus.lang = "German",
                         mfw.min = MFW_base,
                         mfw.max = MFW_base,
                         distance.measure = my_distance
  )
  
  # get distances per candidate
  my_distances <- stylo_results$distance.table[1,2:dim(stylo_results$distance.table)[1]]
  my_candidates <- names(my_distances) %>% strsplit(split = "_") %>% sapply(function(x) x[1])
  my_df <- data.frame(my_distances, my_candidates, words = paste(MFW_base, "MFW"))
  my_df <- my_df %>%
    filter(my_candidates %in% c("salten", "klein"))
  full_df <- my_df
  
  # 2. analysis removing words
  
  # list all files in corpus
  all_files <- list.files(my_corpus, pattern = ".txt", full.names = T)
  
  # read and process them
  all_texts <- lapply(all_files, readLines) %>% 
    lapply(function(x) paste(x, collapse = " ")) %>% 
    lapply(clean_texts) %>%
    stylo::txt.to.words.ext(corpus.lang = "German")
  names(all_texts) <- gsub(paste(my_corpus,"/|.txt", sep = ""), "", all_files)
  
  # remove the pronouns
  for(i in 1:length(all_texts)){
    
    all_texts[[i]] <- all_texts[[i]][-which(all_texts[[i]] %in% words_to_remove)]
    
  }
  
  my_candidates <- names(all_texts)[2:length(all_texts)] %>%
    strsplit(split = "_") %>%
    sapply(function(x) x[1])
  
  # Process by splitting into 5,000-word chunks
  
  all_texts_split <- list()
  for(i in 2:length(all_texts)){
    all_texts_split[[i-1]] <- text_process(all_texts[[i]])
    names(all_texts_split[[i-1]]) <- paste(my_candidates[i-1], 1:length(all_texts_split[[i-1]]), sep = "_")
  }
  
  all_texts_split <- unlist(all_texts_split, recursive = F)
  parsed_corpus <- c(all_texts[1], all_texts_split)
  
  # stylo analysis
  stylo_results <- stylo(gui = F, 
                         parsed.corpus = parsed_corpus, 
                         corpus.lang = "German",
                         mfw.min = MFW_base-length(words_to_remove),
                         mfw.max = MFW_base-length(words_to_remove),
                         distance.measure = my_distance
  )
  
  # get distances per candidate
  my_distances <- stylo_results$distance.table[1,2:dim(stylo_results$distance.table)[1]]
  my_candidates <- names(my_distances) %>% strsplit(split = "_") %>% sapply(function(x) x[1])
  
  my_df <- data.frame(my_distances, my_candidates, words = paste(MFW_base-length(words_to_remove), " MFW\n(excluding ", paste(words_to_remove, collapse = ", "), ")", sep = ""))
  my_df <- my_df %>%
    filter(my_candidates %in% c("salten", "klein"))
  
  full_df <- rbind(full_df, my_df)
  full_df$words <- factor(full_df$words, levels = sort(unique(full_df$words), decreasing = T))
  
  colnames(full_df)[2] <- "Candidates"
  
  full_df %>%
    group_by(words) %>%
    summarise(shapiro.test(my_distances)$p.value)
  
  # Make Plot
  p1 <- ggplot(full_df, aes(x = words, y = my_distances, color = Candidates)) +
    geom_boxplot() +
    xlab("Words used") +
    ylab("Distances from Mutzenbacher") +  
    stat_compare_means(method = "wilcox.test", paired = FALSE, method.args = list(alternative = "greater"))
  
  my_filename <- paste(analysis_dir, "DistancesComparison_", gsub("dist.", "", my_distance), "_remove(", paste(words_to_remove, collapse = "_"), ").png", sep = "")
  
  ggsave(p1, filename = my_filename, width = 16, height = 9, scale = 0.5)
  
}

# word removal analysis (single graph)

if(length(words_to_remove)==0 & word_removal_analysis){
  
  # 1. analysis without removing words
  
  # first analysis with base MFW
  stylo_results <- stylo(gui = F, 
                         parsed.corpus = parsed_corpus, 
                         corpus.lang = "German",
                         mfw.min = MFW_base,
                         mfw.max = MFW_base,
                         distance.measure = my_distance
  )
  
  # get distances per candidate
  my_distances <- stylo_results$distance.table[1,2:dim(stylo_results$distance.table)[1]]
  my_candidates <- names(my_distances) %>% strsplit(split = "_") %>% sapply(function(x) x[1])
  my_df <- data.frame(my_distances, my_candidates, words = paste(MFW_base, "MFW"))
  my_df <- my_df %>%
    filter(my_candidates %in% c("salten", "klein"))
  full_df <- my_df
  
  colnames(full_df)[2] <- "Candidates"
  
  full_df %>%
    summarise(shapiro.test(my_distances)$p.value)
  
  # Make Plot
  p1 <- ggplot(full_df, aes(x = words, y = my_distances, color = Candidates)) +
    geom_boxplot() +
    xlab("Words used") +
    ylab("Distances from Mutzenbacher") +  
    stat_compare_means(method = "wilcox.test", paired = FALSE, method.args = list(alternative = "greater"))
  
  my_filename <- paste(analysis_dir, "DistancesComparison_", gsub("dist.", "", my_distance), "_NOremove.png", sep = "")
  
  ggsave(p1, filename = my_filename, width = 16, height = 9, scale = 0.5)
  
}
