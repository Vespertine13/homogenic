# this R code can be used to check if files R alike
print("-------------------------- Litias --------------------------")
# rm(list= ls())

print("Loading libraries...")
suppressPackageStartupMessages(library(tidyverse))
library(digest)
library(glue)
library(reshape2)
print("Done")

# source functions
print("Sourcing functions...")
source("funcs.R")
print("Done")
print("------------------------------------------------------------")

# paths to different folders
source("config.R")
print("Current folders")

provide_folders()

print("------------------------------------------------------------")

print("Creating df...")

for(i in 1:length(folders)){
    assign(paste0(extract_letter(folders[i]),"_files"),
           list.files(get(folders[i]), recursive = TRUE))
}

files_lst_names <- paste0(extract_letter(folders), "_files")
totfiles <- c()
for(name in files_lst_names){
    totfiles <- c(totfiles, get(name))
}
uniqfiles <- unique(totfiles)
df <- data.frame(files = uniqfiles)
for(i in folders){
    df[[i]] <- NA
}

print("done")


print("Calculating hash values...")
df <- fill_hash(df, folders)

print("Done")

print("Calculating statistics...")
df$max <- NA
df$n_max <- NA
df$new_file <- NA
df$broken_file <- NA

for(i in 1:nrow(df)){
    hash_set <- df[i, folders] %>% as.character()
    df$max[i] <- max_hash(hash_set)
    df$n_max[i] <- n_max_hash(hash_set)
    df$new_file[i] <- check_new_file(hash_set)
    df$broken_file[i] <- check_broken_file(hash_set)
}

for(i in 1:length(folders)){
    df[[extract_letter(folders[i])]] <- get_match(df[[folders[i]]], df$max)
    df[[extract_letter(folders[i])]][df$broken_file] <- FALSE
}

plot_df <- df[,nchar(colnames(df)) == 1]
plot_df$files <- df$files
print("Done")


print("Generating Shell Commands...")

for(i in 1:length(folders)){
    df[[paste0("shell_cmd_",extract_letter(folders[i]))]] <- NA
    df[[paste0("target_folder_",extract_letter(folders[i]))]] <- NA
}


idx <- grep(pattern = "folder_", colnames(df))
for(i in 1:nrow(df)){
    folder_source <- sample(folders[which(df[i, idx] == df$max[i])], 1)
    source <- paste0(get(paste0(folder_source)), df$files[i])
    source_windows <- str_replace_all(source, pattern = "/", replacement = "\\\\")
    for(n in 1:length(folders)){
        if(df[[folders[n]]][i] != df$max[i]){
            target <- paste0(get(folders[n]), df$files[i])
            target_windows <- str_replace_all(target, pattern = "/", replacement = "\\\\")
            df[[paste0("shell_cmd_",extract_letter(folders[n]))]][i] <- glue('copy /y "{source_windows}" "{target_windows}"')
            df[[paste0("target_folder_",extract_letter(folders[n]))]][i] <- dirname(target)
        }
    }
}

all_shells <- paste0("shell_cmd_",extract_letter(folders))
df[df$broken_file,all_shells] <- NA

print("Done")

n_broken <- sum(df$broken_file)
print(glue("Number of broken files: {n_broken}"))
if(sum(df$broken_file) >0){print(df$files[df$broken_file])}

n_new <- sum(df$new_file)
print(glue("Number of new files: {n_new}"))

if(sum(df$new_file) >0){print(df$files[df$new_file])}


total_cmd <- sum(!is.na(df[ ,all_shells]))
print(glue("Number of suggested commands: {total_cmd}"))

for(i in 1:length(all_shells)){
    if(sum(!is.na(df[[all_shells[i]]])) >0){
        print(df[[all_shells[i]]][!is.na(df[[all_shells[i]]])])
    }
}

# i am here
# n <- 1
# i <- 28
# j <- extract_letter(folders[n])
# df %>% colnames()
# 
# !dir.exists(df[[paste0("target_folder_", j)]][i])
# current_dir <- df[[paste0("target_folder_", j)]][i]
# dir.exists(current_dir)
# 
# current_dir <- "c:/Users/ekb/folder_a/what"
# dirname_windows <- str_replace_all(current_dir, pattern = "/", replacement = "\\\\")
# 
# shell(glue('mkdir "{dirname_windows}"'))


print("------------------------------------------------------------")
