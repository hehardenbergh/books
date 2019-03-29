
################################

# ------------------------
# THE HIERARCHY OF GENDER 
# ------------------------
# Author: Eve Kraicer and Dr. Andrew Piper
# Date: August 2018

################################

# This script runs all of the calculations for the paper.
# This includes interpreting the raw data, testing significance, and reporting values.
# This script also builds tables and figures.
# To run, change wd to a directory with nodes_ALL, edges_ALL, hierarchy_metadata.csv, and hierarchy_adj.R 

# REQUIREMENTS

library(epiR)
library(stats)
library(igraph)

library(dplyr)
library(bazar)
library(reshape2)
library(stringr)
library(splitstackshape)
library(rapport)

library(ggplot2)
library(RColorBrewer)

wd <- '~/Desktop/hierarchy/hierarchy_data' # set working directory
num_func  <- function(x){as.numeric(as.character(x))}
source(paste(wd, '/hierarchy_adj.R', sep = ''))

setwd(wd)

# ------------------------
# SECTION I: DATA
# ------------------------

epi_add  <- function(pos,tested,se,sp){
  epi  <- (epi.prev(pos,tested,se_2, sp_2)$tp[[1]])*.01
  add  <- num_func(round((tested*epi)-pos))
  return(add)
}

# 1: DATA & DATA PREP
# = = = = = = = = = = = 

# metadata
metadata.df <- data.frame(read.csv('hierarchy_metadata.csv'))
# subset by works with main character identified and by women
meta_mc.df <- metadata.df[metadata.df$mc_calc == 'Y',]
meta_w.df  <- metadata.df[metadata.df$ag == 'F',]

# tokens, nodes and edges
tokens_files <- list.files('tokens_ALL')
nodes_list <- c('nodes_YA', 'nodes_NYT', 'nodes_MY', 'nodes_SF', 'nodes_BS', 'nodes_ROM', 'nodes_PW')
edges_list <- c('edges_YA', 'edges_NYT', 'edges_MY', 'edges_SF', 'edges_BS', 'edges_ROM', 'edges_PW')


# 1b: TABLE ONE
# - - - - - - - - 
# gather info for table 1
data_info <- NULL
for (i in 1:nlevels(metadata.df$genre)){
  files <- list.files('nodes_ALL')
  meta_sub <- metadata.df[metadata.df$genre == levels(metadata.df$genre)[i],]
  titles  <- files[files %in% meta_sub$ID]
  author <- str_split_fixed(titles, "_", 3)
  no_authors <- length(unique(author[,2]))
  genre <- levels(metadata.df$genre)[i]
  novels <- length(titles)
  perc_w <- (length(which(titles %in% meta_w.df$ID))/novels)*100
  temp_info <- data.frame(cbind(genre, novels, no_authors, perc_w))
  data_info <- data.frame(rbind(data_info, temp_info))
}
# build and order table 1 
table_1 <- data_info[order(num_func(data_info$perc_w)),]


# 1c: FIGURE ONE
# - - - - - - - - 

# DATA
# calculates the mentions for each character by rank position 
setwd(wd)
files <- list.files('nodes_ALL')
setwd(paste(getwd(),('nodes_ALL'),sep = "/"))

# run twice turning off / on condition
# once on all works (mentions) 
# once on only works with main character identified by gender (mentions_sub)

mentions <- NULL
# mentions_sub <- NULL
for(j in 1:length(files)){ 
  work  <- files[j]
  # only files where main character gender was identified 
  # if (work %in% meta_mc.df$ID){
  nodes <- data.frame(read.csv(files[j]))
  m_counts <- data.frame(nodes[,2]) # only pull mentions
  extra <- as.data.frame(matrix(c(rep.int(NA,length(200))),nrow=200)) # add columns
  colnames(extra) <- colnames(m_counts) #give same column names
  mentions_bind <- rbind(m_counts, extra) #bind
  mentions <- cbind(mentions, mentions_bind[1:150,])
  # mentions_sub <- cbind(mentions_sub, mentions_bind[1:150,])
  # }
}

# count up sums for each rank position for both cases 
rank_m <- NULL
rank_m_sub <- NULL
for (i in 1:nrow(mentions)){
  mean <- mean(num_func(mentions[i,]), na.rm=TRUE)
  sum <- sum(mentions[i,], na.rm=TRUE)
  log_sum <- log(sum)
  rank <- i
  temp <- data.frame(cbind(rank,mean,sum,log_sum))
  rank_m <- data.frame(rbind(rank_m,temp))
}
for (i in 1:nrow(mentions_sub)){
  mean <- mean(num_func(mentions_sub[i,]), na.rm=TRUE)
  sum <- sum(mentions_sub[i,], na.rm=TRUE)
  log_sum <- log(sum)
  rank <- i
  temp <- data.frame(cbind(rank,mean,sum,log_sum))
  rank_m_sub <- data.frame(rbind(rank_m_sub,temp))
}

# BUILD  
# run on all works 
figure_1 <- ggplot(data=rank_m, aes(x=rank, y=mean)) +
  geom_point()+
  ggtitle("Mean Mentions by Rank")+
  labs(x="Character Rank by Mentions", y="Mean Mentions")+
  theme_bw()+
  geom_vline(xintercept = 20, linetype = "dashed") +
  theme(plot.title = element_text(size = 13, family = "Times New Roman", face = "bold"),
        text = element_text(size = 11, family = "Times New Roman"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11))


# 1d: FIGURE TWO
# - - - - - - - - 

# DATA
# calculates raw counts of M/ W/ ? by rank position 
setwd(wd)
files <- list.files('nodes_ALL')
setwd(paste(getwd(),('nodes_ALL'),sep = "/"))

# run four times turning off / on condition
# once on all works (raw) 
# once on only works with main character identified by gender (raw_sub)
# once on all works by women (raw_w) 
# once on only works by women with main character identified by gender (raw_sub_w)

# raw <- NULL
# raw_sub <- NULL
# raw_w  <- NULL
raw_sub_w <- NULL
for(i in 1:length(files)){ 
  work  <- files[i]
  # only files where top pair was calculated 
  if (work %in% meta_mc.df$ID){
    # only files written by women authors 
    if (work %in% meta_w.df$ID){
      nodes <- data.frame(read.csv(files[i]))
      genders <- data.frame(nodes[,3]) 
      # add columns to cbind different books 
      extra <- as.data.frame(matrix(c(rep.int(NA,length(200))),nrow=200)) 
      colnames(extra) <- colnames(genders)
      genders_bind <- rbind(genders, extra) 
      # raw <- data.frame(cbind(raw, genders_bind[1:150,]))
      # raw_sub <- data.frame(cbind(raw_sub, genders_bind[1:150,]))
      # raw_w <- data.frame(cbind(raw_w, genders_bind[1:150,]))
      raw_sub_w  <- data.frame(cbind(raw_sub_w, genders_bind[1:150,]))
    }
  }
}

# clean vectors to normalize factors --> characters
clean_func  <- function(df){
  v <- df[,1]
  clean <- gsub(3, 'M', v)
  clean <- gsub(2, 'F', clean)
  clean <- gsub(1, '?', clean)
  return(clean)
}
raw[,1] <- clean_func(raw)
raw_sub[,1] <- clean_func(raw_sub)
raw_w[,1] <- clean_func(raw_w)
raw_sub_w[,1] <- clean_func(raw_sub_w)

# count up sums for each rank position
rank_func  <- function(x){
  ranks <- NULL
  for (i in 1:nrow(x)){
    wr <- length(which(x[i,] == 'F')) # how many women
    mr <- length(which(x[i,] == 'M')) # how many men
    q  <- length(which(x[i,] == '?')) # how many q
    all <- mr + wr + q
    # adjust women and men 
    women  <- wr + epi_add(wr,all,se_2, sp_2)
    men  <- mr + epi_add(mr,all,se_2m, se_2m)
    temp <- data.frame(women,men,q,all) #cbind for storage
    ranks <-data.frame(rbind(ranks,temp))
  }
  ranks$rank <- 1:150
  ranks$cumulative_q  <- cumsum(ranks$q)/cumsum(ranks$all)
  return(ranks)
}

ranks_all <- rank_func(raw)
ranks_sub  <- rank_func(raw_sub)
ranks_w  <- rank_func(raw_w)                   
ranks_sub_w  <- rank_func(raw_sub_w)   

# basic counts by subsets
# how many unknown overall?
perc_unknown10 <- sum(ranks_all$q[1:10])/sum(ranks_all$all[1:10])
perc_unknown20 <- sum(ranks_all$q[1:20])/sum(ranks_all$all[1:20])

# BUILD 
# run on all works
figure_2 <- ggplot(data=ranks_all, aes(x=rank, y=cumulative_q)) +
  geom_line()+
  ggtitle("% Unassigned Characters by Mentions Rank")+
  labs(x="Character Rank by Mentions", y="Cumulative Percent Unassigned")+
  theme_bw() + 
  geom_vline(xintercept = 20, linetype = "dashed") + 
  theme(plot.title = element_text(size = 13, family = "Times New Roman", face = "bold"),
        text = element_text(size = 11, family = "Times New Roman"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11))

# ------------------------
# SECTION II: VISIBILITY
# ------------------------

# bootstrapper 
# x is column on df to be sampled, y is df 
boot_func <- function(x,y){
  sample <- as.character(sample(x, nrow(y), replace = TRUE))
  return(sample)
}

# adjustments
# for all
epi_func <- function(vector,w,m,q,se,sp){
  pos <- length(which(as.character(vector) == w))
  tested <- length(which(as.character(vector) == w | as.character(vector) == m | as.character(vector) == q)) 
  epi <- epi.prev(pos, tested, se, sp)
  adj_perc <- epi$tp[[1]]*.01
  adj_add <- round((tested*adj_perc)-pos)
  return(adj_add)
}

# for main chars
epi_func2 <- function(vector,w,m,se,sp){
  pos <- length(which(as.character(vector) == w))
  tested <- length(which(as.character(vector) == w | as.character(vector) == m)) # original sample set 
  epi <- epi.prev(pos, tested, se, sp)
  # find percentage increase of women for true prevalence of women as main character
  adj_perc <- epi$tp[[1]]*.01
  # how many more women is that? 
  adj_add <- round((tested*adj_perc)-pos)
  return(adj_add)
}

# 1: TOP 20 & ALL 
# = = = = = = = = = 

# build master table with genders of top pairs from each document

# run twice for conditionals 
# once for all 
# once for only characters in the top twnety 

# chars_all  <- NULL
chars_20 <- NULL
for (i in 1:nlevels(metadata.df$genre)){
  setwd(wd)
  files <- list.files('nodes_ALL')
  setwd(paste(getwd(),('nodes_ALL'),sep = "/"))
  #loop through each file
  meta_sub <- metadata.df[metadata.df$genre == levels(metadata.df$genre)[i],]
  titles  <- files[files %in% meta_sub$ID]
  # loop through each file
  chars <- NULL
  for(j in 1:length(titles)){ 
    work <- titles[j]
    nodes <- data.frame(read.csv(titles[j]))
    # genders  <- as.character(nodes[,3])
    # for 20 chars run these lines 
    # make sure work has 20 chars
    if (nrow(nodes) >= 20){
      genders <- as.character(nodes[1:20,3])
    } else {
      genders <- as.character(nodes[,3])}
    ag <- as.character(metadata.df$ag[(which(metadata.df$ID %in% work))])
    genre <- levels(metadata.df$genre)[i]
    temp_chars <- data.frame(cbind(work,genders,ag,genre))
    chars <- data.frame(rbind(temp_chars,chars))
  }
  chars_20  <- data.frame(rbind(chars_20, chars))
  # chars_all <- data.frame(rbind(chars_all, chars))
}

# 1a: ADJUSTMENTS
# - - - - - - - - - 

# how many M / ? need to be replaced with Fs?
men_rep2  <- epi_func(chars_20$genders, 'M', 'F', '?', se_2m, sp_2m)
q_rep2  <- (epi_func(chars_20$genders, 'F', 'M', '?', se_2, sp_2))-abs(men_rep2)

# randomly replace M 
rep_m20 <- sample(which(chars_20$gender == 'M'), men_rep2)
chars_20$adj_all <- replace(as.character(chars_20$gender), rep_m20, 'F')
# randomly replace ?
rep_q20 <- sample(which(chars_20$adj_all == '?'), q_rep2)
chars_20$adj_all <- replace(as.character(chars_20$adj_all), rep_q20, 'F')


# BOOTSTRAP 
# calculate the percentage of 'F' with bootstrapping 
# boot_chars_20 <- vector()
boot_chars_all  <- vector()
for (j in 1:10000){  
  sample <- boot_func(chars_20$adj_all, chars_20)
  # sample  <- boot_func(chars_all$adj_all, chars_all)
  f_perc <- length(which(sample == 'F'))/length(sample)
  boot_chars_20[j] <- f_perc
  # boot_chars_all[j] <- f_perc
}

# 1b: REPORTED VALUES
# - - - - - - - - - - 
length(which(chars_20$genders == 'F')) # raw women
length(which(chars_20$adj_all == 'F')) # adjusted women 
length(which(chars_20$adj_all == 'F')) / nrow(chars_20) # % women 
mean(boot_chars_20) # bootstrap mean - top 20 
mean(boot_chars_all) # bootstrap mean - all 

# by genre and ag 
chars_func  <- function(x){
  results <- NULL
  for (i in 1:nlevels(x)){
    sub <- chars_20[which(as.character(x) == levels(x)[i]),]
    genre <- levels(x)[i]
    f <- vector()
    for (j in 1:10000){  
      sample <- boot_func(sub$adj_all, sub)
      f[j] <- length(which(sample == 'F'))
    }
    all  <- nrow(sub)
    temp <- data.frame(cbind('f'= mean(f), 'all'= all, genre))
    results <- data.frame(rbind(results,temp))
  }
  results$f_perc  <- num_func(results$f)/num_func(results$all)
  return(results)
}

# by genre and author gender 
boot_chars_genre <- chars_func(chars_20$genre)

# remove romance
chars_20xROM <- chars_20[which(chars_20$genre != 'ROM'),]
boot_chars_ag <- chars_func(chars_20xROM$ag)
boot_chars_ag_wROM <- chars_func(chars_20$ag)

# 1d: SIGNIFICANCE TESTING
# - - - - - - - - - - - - - 
chisq_20  <- t(data.frame(cbind('women'=num_func(boot_chars_genre$f), 'all'=num_func(boot_chars_genre$all))))
chisq.test(chisq_20)

chisq_20_ag  <- t(data.frame(cbind('women'=num_func(boot_chars_ag$f[1:2]), 'all'=num_func(boot_chars_ag$all[1:2]))))
chisq.test(chisq_20_ag)
fisher.test(chisq_20_ag)

# 1e: TABLE TWO
# - - - - - - - - 
table_2  <- data.frame(cbind('genre'=as.character(boot_chars_genre$genre), 'percent_w'=num_func(boot_chars_genre$f_perc)*100))
table_2 <- table_2[order(num_func(table_2$percent_w)),]


# 1f: FIGURE THREE & FOUR
# - - - - - - - - - - - - -

# DATA 
# from ranks_sub, update the first and second character based on perspective calcs
ranks_sub[1,1:2]  <- c(623, 617)
ranks_sub[2,1:2]  <- c(470, 768)
ranks_sub_w[1,1:2]  <- c(321,194)
ranks_sub_w[2,1:2]  <- c(191,324)

# get correct percentages of women 
ranks_sub$perc_w  <- ranks_sub$w/ranks_sub$all
ranks_sub_w$perc_w  <- ranks_sub_w$w/ranks_sub_w$all

# BUILD 
# figure 3 
# uses only works with main character identified 
figure_3 <- ggplot(data=ranks_sub, aes(rank)) + 
  geom_line(aes(y = women, colour = "Women")) + 
  geom_line(aes(y = men, colour = "Men")) +
  scale_color_manual(values=c("darkorange2", "mediumpurple4")) +
  ggtitle("Men and Women by Rank Position (All Authors)")+
  labs(x="Rank of Character", y="") +
  labs(color="Character Gender") +
  theme_bw() +
  theme(plot.title = element_text(size = 13, family = "Times New Roman", face = "bold"),
        text = element_text(size = 11, family = "Times New Roman"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11)) +
  geom_vline(xintercept = 20, linetype = "dashed") 

# figure 4 
figure_4 <- ggplot(data=ranks_sub_w, aes(rank)) + 
  geom_line(aes(y = women, colour = "Women")) + 
  geom_line(aes(y = men, colour = "Men")) +
  scale_color_manual(values=c("darkorange2", "mediumpurple4")) +
  ggtitle("Men and Women by Rank Position (Women Authors)")+
  labs(x="Rank of Character", y="") +
  labs(color="Character Gender") +
  theme_bw() +
  theme(plot.title = element_text(size = 13, family = "Times New Roman", face = "bold"),
        text = element_text(size = 11, family = "Times New Roman"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11)) +
  geom_vline(xintercept = 20, linetype = "dashed") 


# 2: MAIN CHARACTER & TOP PAIRS
# = = = = = = = = = = = = = = = = = = 

# 2a: DATA PREP
# - - - - - - - -

# build master table with genders of top pairs from each document 
setwd(wd)
files <- list.files('nodes_ALL')
token_files  <- list.files('tokens_ALL')
top_chars2 <- NULL
#loop through each file
for(i in 1:length(files)){ 
  setwd('~/Desktop/hierarchy/hierarchy_data/nodes_ALL')
  print(i)
  work <- files[i]
  genre <- as.character(metadata.df$genre[(which(metadata.df$ID %in% work))])
  pov  <- as.character(metadata.df$pov[(which(metadata.df$ID %in% work))])
  ag <- as.character(metadata.df$ag[(which(metadata.df$ID %in% work))])
  nodes <- data.frame(read.csv(work))
  if (pov == '3p'){
    first <- as.character(nodes[1,3])
  }
  else if (pov == '1p'){
    setwd('~/Desktop/hierarchy/hierarchy_data/tokens_ALL')
    if (work %in% token_files){
      tokens <- read.csv(work, sep = ',')
      dep_null <- NULL
      for (i in 2:nlevels(factor(tokens$characterId))){
        sub <- tokens[factor(tokens$characterId) == levels(factor(tokens$characterId))[i],]
        if (nrow(sub) > 30){
          char_name <- as.character(sub$lemma)[1]
          charID <- sub$characterId[1]
          no_occurs <- nrow(sub)
          subj_occurs  <-  length(sub$deprel[sub$deprel == "null"])
          subj_ratio <- subj_occurs/no_occurs
          subj_adj_ratio <- log(subj_occurs)/log(no_occurs)
          gender <- num_func(sub$characterGender)[1]
          temp <- data.frame(char_name, charID, no_occurs, subj_ratio, subj_adj_ratio, gender)
          dep_null <- rbind(dep_null, temp)
        }
      }
      if (is.null(dep_null)) {dep_null$gender  <- 'error_1'} else {
        dep_null <- dep_null[order(-dep_null$subj_adj_ratio),]
        # insert rule on removing chars named Mr. or Mrs.
        remove <- which(as.character(dep_null$char_name) == "Mr." & as.character(dep_null$char_name) == "Mrs." & as.character(dep_null$char_name) == "Ms.")
        if (length(remove) > 0){
          dep_null <- dep_null[-remove, ]
        }
      } 
      first  <- dep_null$gender[1]
    } else {
      first  <- 'error_2'
    }
  }
  second <- as.character(nodes[2,3])
  temp_pairs <- data.frame(cbind(work,first,second,ag,genre,pov))
  top_chars2 <- data.frame(rbind(temp_pairs,top_chars2))
}


# 2b: ADJUSTMENTS
# - - - - - - - - -

# calculate how many to add based on validation 
adj_3p <- epi_func(top_chars2$first, 'F', 'M', se_1, sp_1)
adj_1p <- epi_func(top_chars2$first, 1, 2, se_1b, sp_1b)
all_adj <- epi_func(top_chars2$second, 'F', 'M', se_1, sp_1)

# replace protagonists in 3p books
rep_first <- sample(which(top_chars2$first == 'M'), adj_3p)
top_chars2$adj_first <- replace(as.character(top_chars2$first), rep_first, 'F')
# for protagonists in 1p books 
rep_firstb <- sample(which(top_chars2$first == 2), adj_1p)
top_chars2$adj_first <- replace(as.character(top_chars2$adj_first), rep_firstb, 1)

# change all indicators from numeric --> characters 
top_chars2$first  <- gsub(1, 'F', top_chars2$first)
top_chars2$first  <- gsub(2, 'M', top_chars2$first)
top_chars2$adj_first  <- gsub(1, 'F', top_chars2$adj_first)
top_chars2$adj_first  <- gsub(2, 'M', top_chars2$adj_first)
# replace
rep_second <- sample(which(top_chars2$second == 'M'), all_adj)
top_chars2$adj_second <- replace(as.character(top_chars2$second), rep_second, 'F')

# clean all errors & subset
top_chars3  <- top_chars2[which(top_chars2$adj_first != 'error_M'),]
top_chars3  <- top_chars3[which(top_chars3$adj_first != 0),]
top_chars_1p  <- top_chars3[top_chars3$pov == '1p',]
top_chars_3p  <- top_chars3[top_chars3$pov == '3p',]

# calculate top pairs by genre and author gender 
tp_func  <- function(x,y){
  results <- NULL
  for (i in 1:nlevels(x)){
    sub <- top_chars3[which(as.character(x) == levels(x)[i]),]
    sub  <- sub[sub$genre != y,]
    group <- levels(x)[i]
    titles <- nrow(sub)
    temp  <- NULL
    for (i in 1:1000){
      # shuffle second char column
      sub$sec_samp <- sample(sub$adj_second, nrow(sub))
      sub$adj_tp  <- as.character(paste(sub$adj_first, sub$sec_samp, sep=''))
      sample <- boot_func(sub$adj_tp, sub)
      f  <- length(which(sample == 'FF'|sample == 'FM'))
      m  <- length(which(sample == 'MM'|sample == 'MF'))
      mm <- length(which(sample == 'MM'))
      ff <- length(which(sample == 'FF'))
      f_perc <- f/length(sample)
      m_perc  <- m/length(sample)
      mm_perc <- mm/length(sample)
      ff_perc <- ff/length(sample)
      mm_m  <- mm/m
      ff_f  <- ff/f
      vectors  <- data.frame(cbind(f,m,mm,ff,mm_m,ff_f, m_perc,f_perc,mm_perc,ff_perc))
      temp  <- rbind(temp,vectors)
    }
    mean_f <- mean(temp$f)
    mean_m <- mean(temp$m)
    mean_mm  <- mean(temp$mm)
    mean_ff  <- mean(temp$ff)
    mean_f_perc <- mean(temp$f_perc) 
    mean_m_perc <- mean(temp$m_perc)
    mean_ff_perc <- mean(temp$ff_perc)
    mean_mm_perc <- mean(temp$mm_perc)
    mean_ff_f  <- mean(temp$ff_f)
    mean_mm_m  <- mean(temp$mm_m)
    temp_tp <- data.frame(cbind(titles, group, mean_f, mean_m, mean_mm, mean_ff, 
                                mean_m_perc, mean_f_perc, mean_mm_perc, mean_ff_perc, mean_ff_f, mean_mm_m))
    results <- data.frame(rbind(results, temp_tp))
  }
  return(results)
}

boot_tp_genre  <- tp_func(top_chars3$genre, 'NA')
boot_tp_ag  <- tp_func(top_chars3$ag, 'ROM')

# 2c: TABLE 3 & 4
# - - - - - - - -  
table_3 <- data.frame(cbind('women'=num_func(boot_tp_genre$mean_f), 'perc_women'=num_func(boot_tp_genre$mean_f_perc)), row.names = boot_tp_genre$group)

table_4  <- data.frame(cbind('genre'=as.character(boot_tp_genre$group), 
                             'perc_same_gender'=(num_func(boot_tp_genre$mean_ff)+num_func(boot_tp_genre$mean_mm))/num_func(boot_tp_genre$titles),
                             'ratio'=num_func(boot_tp_genre$mean_mm)/num_func(boot_tp_genre$mean_ff)))

table_4 <- table_4[order(num_func(table_4$ratio)),]

# 2d: REPORTED VALUES
# requires boot_func 
# - - - - - - - - - - 

# empty vectors 
boot_mc  <- vector()
boot_mc_1p <- vector()
boot_mc_3p  <- vector()

# all main characters
for (i in 1:10000){
  sample <- boot_func(top_chars3$adj_first, top_chars3)
  sample_perc <- length(which(sample == 'F'))/length(sample)
  boot_mc[i]  <- sample_perc
}
mean_mc  <- mean(boot_mc)

# 1p main characters (homodiegetic)
for (i in 1:10000){
  sample <- boot_func(top_chars_1p$adj_first, top_chars_1p)
  sample_perc <- length(which(sample == 'F'))/length(sample)
  boot_mc_1p[i]  <- sample_perc
}
mean_mc_1p  <- mean(boot_mc_1p)

# 3p main character (heterodiegetic)
for (i in 1:10000){
  sample <- boot_func(top_chars_3p$adj_first, top_chars_3p)
  sample_perc <- length(which(sample == 'F'))/length(sample)
  boot_mc_3p[i]  <- sample_perc
}
mean_mc_3p  <- mean(boot_mc_3p)

# second character
boot_sc  <- vector()
for (i in 1:10000){
  sample <- boot_func(top_chars3$adj_second, top_chars3)
  sample_perc <- length(which(sample == 'F'))/length(sample)
  boot_sc[i]  <- sample_perc
}
mean_sc  <- mean(boot_sc)

# second character in books by women 
boot_sc_w  <-  vector()
for (i in 1:10000){
  sub  <- top_chars3[top_chars3$ag == 'F',]
  sample <- boot_func(sub$adj_second, sub)
  sample_perc <- length(which(sample == 'F'))/length(sample)
  boot_sc_w[i]  <- sample_perc
}
mean_sc_w  <- mean(boot_sc_w)

# how many ff given f and mm given m? 
ff_f <- sum(num_func(boot_tp_genre$mean_ff))/sum(num_func(boot_tp_genre$mean_f))
mm_m  <- sum(num_func(boot_tp_genre$mean_mm))/sum(num_func(boot_tp_genre$mean_m))

# for author gender
# how many for each by women 
ff_pairs_w <- num_func(boot_tp_ag$mean_ff[1])
mm_pairs_w <- num_func(boot_tp_ag$mean_mm[1])
# by men 
ff_pairs_m <- num_func(boot_tp_ag$mean_ff[2])
mm_pairs_m <- num_func(boot_tp_ag$mean_mm[2])

# 2e: SIGNIFICANCE TESTING
# - - - - - - - - - - - - - 

# by genre
chsq_mc <- t(cbind(num_func(boot_tp_genre$mean_f), (num_func(boot_tp_genre$title))))[,1:7]
chisq.test(chsq_mc) 

# bu author gender 
chsq_mc_ag  <- t(cbind(num_func(boot_tp_ag$mean_f[1:2]), (num_func(boot_tp_ag$title[1:2]))))
chisq.test(chsq_mc_ag)
fisher.test(chsq_mc_ag)

# mm/ff by genre 
chsq_tp <- t(cbind(num_func(boot_tp_genre$mean_mm), (num_func(boot_tp_genre$mean_ff))))[,1:7]
chisq.test(chsq_tp)
mm_pairs/ff_pairs

# top pairs by author gender 
ft_tp_ag_w  <- data.frame(c(ff_pairs_w,ff_pairs_w+mm_pairs_w), c(ff_pairs_m, ff_pairs_m+mm_pairs_m))
ft_tp_ag_m  <- data.frame(c(mm_pairs_m, mm_pairs_m+ff_pairs_m), c(mm_pairs_w,ff_pairs_w+mm_pairs_w))
fisher.test(ft_tp_ag_w)
fisher.test(ft_tp_ag_m)
sum(num_func(chsq_tp_ag))


# -------------------------
# SECTION III: CONNECTIVITY
# -------------------------

# anova test
anova_func<-function(x,y,z){
  df2 <- data.frame(cbind(x, as.character(y), as.character(z))) #make df
  colnames(df2) <- c("m", "genre", "ag") #rename for measures
  df2$m2 <- num_func(df2$m)#turn to numeric
  p <- summary(aov(m2 ~ ag + genre + ag:genre, data=df2))[[1]][['Pr(>F)']] #pvalue
  f <- summary(aov(m2 ~ ag + genre + ag:genre, data=df2))[[1]][['F value']] #fscore
  summary  <- data.frame(rbind('p-value'=as.character(p[1:3]),'f'=as.character(f[1:3])))
  colnames(summary) <- c('ag', 'genre', 'ag:genre')
  return(summary)
}

# antagonism labelling pairs 
pair_func  <- function(x,y){
  pair <- vector()
  for (j in 1:nrow(y)){
    c1 <- as.character(x[which(as.character(x$NAME) == as.character(y$NAME.1[j])),3])
    c2 <- as.character(x[which(as.character(x$NAME) == as.character(y$NAME.2[j])),3])
    pair[j] <- paste(c1,c2, sep = '')
  }
  return(pair)
}


# 1: ASSORTATIVITY
# = = = = = = = = =

# 1a: adjustments 
# - - - - - - - - - - - - - 

# loop to to get adjustments on nodes lists 
setwd(wd)
files <- list.files('nodes_ALL')
setwd(paste(getwd(),('nodes_ALL'),sep = "/"))
bootstrap_files <- NULL
for (j in 1:length(files)){
  work <- as.character(files[j])
  nodes <- read.csv(files[j])
  pos <- length(which(nodes$GENDER == 'F'))
  tested <- nrow(nodes)
  if (pos < 1) {adj_add <- 0} else {
    epi <- epi.prev(pos, tested, se_2, sp_2)
    # find percentage increase of women for true prevalence of women as main character
    adj_perc <- epi$tp[[1]]*.01
    # how many more women is that? 
    adj_add <- round((tested*adj_perc)-pos)
  }
  temp <- data.frame(cbind(work,'adj_add'=num_func(adj_add), 'no_chars'=tested))
  bootstrap_files <- data.frame(rbind(temp,bootstrap_files))
}
bootstrap_files$adj_add <- num_func(bootstrap_files$adj_add)
bootstrap_files$work <- as.character(bootstrap_files$work)

# split into files that need to be bootstrapped and those that do not
files_noboot <- bootstrap_files$work[which(bootstrap_files$adj_add > 0)]
files_boot <- bootstrap_files$work[which(bootstrap_files$adj_add <= 0)]


# 1b: CALCULATE ASSORTATIVITY
# - - - - - - - - - - - - - - 

# calculate assortativity three ways - no bootstrap, bootstrap, and a control w permutations
# no bootstrap
setwd(wd)
files_n <- list.files("nodes_ALL")
files_e <- list.files("edges_ALL")
assort1 <- NULL
for (i in 1:length(files_e)){
  work <- files_e[i]
  if (work %in% files_noboot){
    genre <- as.character(top_chars$code[(which(top_chars$work %in% work))])
    if (is.null(genre)) {genre  <- 'catch'}
    ag <- as.character(metadata.df$ag[(which(metadata.df$ID %in% work))])
    # nodes 
    setwd(paste((wd),('nodes_ALL'),sep = "/"))
    nodes <- read.csv(files_n[i])
    setwd(paste((wd),('edges_ALL'),sep = "/"))
    # edges 
    edges <- read.csv(files_e[i])
    edge1 <- data.frame(source=edges$NAME.1, target=edges$NAME.2) 
    #build graphs
    g <- graph.data.frame(edge1, directed=FALSE, vertices=NULL)#graph
    gw <- graph.adjacency(get.adjacency(g),mode=c("undirected"), weighted=TRUE)
    #basic information
    no_nodes <- length(V(gw))
    no_edges <- length(E(gw)) 
    no_ints <- length(E(g))
    #assortativity
    char_list <- sort(as_ids(V(g)), decreasing = F)
    nodes <- nodes[order(nodes$NAME),]
    nodes <- nodes[as.character(nodes$NAME) %in% char_list,]
    #give gender labels to vertices
    V(g)$name <- as_ids(V(g))
    V(gw)$name <- as_ids(V(gw))
    # atleast one man, and not zero women 
    V(g)$gender <- as.character(nodes$GENDER[match(V(g)$name,nodes$NAME)])
    V(gw)$gender <- as.character(nodes$GENDER[match(V(gw)$name,nodes$NAME)])
    g_clean <- delete_vertices(g, which(V(g)$gender == "?"))
    gw_clean <- delete_vertices(gw, which(V(gw)$gender == "?"))
    # measures on all edges
    assort_int_all <- assortativity.nominal(g_clean, as.factor(V(g_clean)$gender))
    assort_rel_all <- assortativity.nominal(gw_clean, as.factor(V(gw_clean)$gender))
    # measures on subsets using helper function
    assort_int_20 <- assort_func(g_clean, g_clean, 20)
    assort_rel_20 <- assort_func(g_clean, gw_clean, 20)
    assort_int_10 <- assort_func(g_clean, g_clean, 10)
    assort_rel_10 <- assort_func(g_clean, gw_clean, 10)
    temp.df <- data.frame(work, genre, ag, no_nodes, no_edges, no_ints, assort_int_all, assort_rel_all, 
                          assort_int_10, assort_rel_10, assort_int_20, assort_rel_20)
    assort1 <- rbind(assort1, temp.df)
  }
}

# bootstrap 
setwd(wd)
files_n <- list.files('nodes_ALL')
files_e <- list.files('edges_ALL')
assort2 <- NULL
for (i in 1:length(files_e)){
  work <- files_n[i]
  if (work %in% files_boot){
    genre <- as.character(top_chars$code[(which(top_chars$work %in% work))])
    ag <- as.character(metadata.df$ag[(which(metadata.df$ID %in% work))])
    setwd(paste((wd),('edges_ALL'),sep = "/"))
    edges <- read.csv(files_e[i])
    edge1 <- data.frame(source=edges$NAME.1, target=edges$NAME.2) 
    g <- graph.data.frame(edge1, directed=FALSE, vertices=NULL)
    gw <- graph.adjacency(get.adjacency(g),mode=c("undirected"), weighted=TRUE)
    #basic information
    no_nodes <- length(V(gw))
    no_edges <- length(E(gw)) 
    no_ints <- length(E(g))
    # fix nodes 
    setwd(paste((wd),('nodes_ALL'),sep = "/"))
    nodes <- read.csv(files_n[i])
    #assortativity
    char_list <- sort(as_ids(V(g)), decreasing = F)
    nodes <- nodes[order(nodes$NAME),]
    nodes <- nodes[as.character(nodes$NAME) %in% char_list,]
    #give gender labels to vertices
    V(g)$name <- as_ids(V(g))
    V(gw)$name <- as_ids(V(gw))
    # calculate replacement using epi_prev based on # of nodes
    # find percentage increase of women for true prevalence of women as main character
    # how many more women is that? 
    adj_add <- as.character(bootstrap_files$adj_add[(which(bootstrap_files$work %in% work))])
    # empty vectors 
    int_all <- vector()
    int_20 <- vector()
    int_10 <- vector()
    rel_all <- vector()
    rel_20 <- vector()
    rel_10 <- vector()
    for (j in 1:1000){
      # randomly sample the number of added women from the number of non-women and replace integers with 'F's
      # for first char
      rep <- sample(which(nodes$GENDER != 'F'), adj_add)
      nodes$adj_GENDER <- replace(as.character(nodes$GENDER), rep, 'F')
      # for permutation turn these two lines off
      V(g)$gender <- as.character(nodes$adj_GENDER[match(V(g)$name,nodes$NAME)])
      V(gw)$gender <- as.character(nodes$adj_GENDER[match(V(gw)$name,nodes$NAME)])
      g_clean <- delete_vertices(g, which(V(g)$gender == "?"))
      gw_clean <- delete_vertices(gw, which(V(gw)$gender == "?"))
      # measures on all edges
      int_all[j] <- assortativity.nominal(g_clean, as.factor(V(g_clean)$gender))
      rel_all[j] <- assortativity.nominal(gw_clean, as.factor(V(gw_clean)$gender))
      # measures on top 20 edges using helper function 
      int_20[j] <- assort_func(g_clean, g_clean, 20)
      rel_20[j] <- assort_func(g_clean, gw_clean, 20)
      int_10[j] <- assort_func(g_clean, g_clean, 10)
      rel_10[j] <- assort_func(g_clean, gw_clean, 10)
    }
    assort_int_all <- mean(int_all)
    assort_int_20 <- mean(int_20)
    assort_int_10 <- mean(int_10)
    assort_rel_all <- mean(rel_all)
    assort_rel_20 <- mean(rel_20)
    assort_rel_10 <- mean(rel_10)
    temp.df <- data.frame(work, genre, ag, no_nodes, no_edges, no_ints, assort_int_all, assort_rel_all, 
                          assort_int_10, assort_rel_10, assort_int_20, assort_rel_20)
    assort2 <- rbind(assort2, temp.df)
  }
}

assort_ALL <- data.frame(rbind(assort1,assort2))

# control 
boot_assort_mean <- NULL
for (i in 1:1000){
  setwd(wd)
  files_n <- list.files("nodes_ALL")
  files_e <- list.files("edges_ALL")
  boot_assort <- NULL
  for (i in 1:length(files_e)){
    work <- files_n[i]
    setwd(paste((wd),('edges_ALL'),sep = "/"))
    edges <- read.csv(files_e[i])
    edge1 <- data.frame(source=edges$NAME.1, target=edges$NAME.2) 
    g <- graph.data.frame(edge1, directed=FALSE, vertices=NULL)#graph
    gw <- graph.adjacency(get.adjacency(g),mode=c("undirected"), weighted=TRUE)
    # fix nodes 
    setwd(paste((wd),('nodes_ALL'),sep = "/"))
    nodes <- read.csv(files_n[i])
    adj_add <- bootstrap_files$adj_add[(which(bootstrap_files$work %in% work))]
    if (adj_add > 1){
      rep <- sample(which(nodes$GENDER != 'F'), adj_add)
      nodes$adj_GENDER <- replace(as.character(nodes$GENDER), rep, 'F')
    } else {nodes$adj_GENDER <- nodes$GENDER}
    # subset 
    char_list <- sort(as_ids(V(g)), decreasing = F)
    nodes <- nodes[nodes$NAME %in% char_list,]
    # randomly shuffle
    gender_orig <- as.character(nodes$adj_GENDER)
    gender_permute <- sample(gender_orig, length(gender_orig))
    V(g)$gender <- gender_permute
    V(gw)$gender <- gender_permute
    g_clean <- delete_vertices(g, which(V(g)$gender == "?"))
    gw_clean <- delete_vertices(gw, which(V(gw)$gender == "?"))
    # measures on all edges
    assort_int_all <- assortativity.nominal(g_clean, as.factor(V(g_clean)$gender))
    assort_rel_all <- assortativity.nominal(gw_clean, as.factor(V(gw_clean)$gender))
    # measures on subsets using helper function
    assort_int_20 <- assort_func(g_clean, g_clean, 20)
    assort_rel_20 <- assort_func(g_clean, gw_clean, 20)
    assort_int_10 <- assort_func(g_clean, g_clean, 10)
    assort_rel_10 <- assort_func(g_clean, gw_clean, 10)
    temp <- data.frame(work, assort_int_all, assort_rel_all, 
                       assort_int_10, assort_rel_10, assort_int_20, assort_rel_20)
    boot_assort <- rbind(boot_assort, temp)
  }
  int_all <- mean(boot_assort$assort_int_all, na.rm=T)
  int_20 <- mean(boot_assort$assort_int_20,na.rm=T)
  int_10 <- mean(boot_assort$assort_int_10,na.rm=T)
  rel_all <- mean(boot_assort$assort_rel_all,na.rm=T)
  rel_20 <- mean(boot_assort$assort_rel_20,na.rm=T)
  rel_10 <- mean(boot_assort$assort_rel_10,na.rm=T)
  boot_temp <- data.frame(int_all,int_20,int_10,rel_all,rel_20,rel_10)
  boot_assort_mean <-rbind(boot_assort_mean, boot_temp)
}

# 1c: REPORTED VALUES
# - - - - - - - - - - 

# remove infinite values from _10 sets 
rel_10_finite <- lapply(assort_ALL, function(x) x[is.finite(x)])$assort_rel_10
int_10_finite <- lapply(assort_ALL, function(x) x[is.finite(x)])$assort_int_10

# 
# for relationships
mean(assort_ALL$assort_rel_all, na.rm = TRUE) < quantile(boot_assort_mean$rel_all, 0.05) # false
mean(assort_ALL$assort_rel_20, na.rm = TRUE) < quantile(boot_assort_mean$rel_20, 0.05) # false
mean(rel_10_finite) < quantile(boot_assort_mean$rel_10, 0.05) # true, but between .01 and .05 

# for interactions
mean(assort_ALL$assort_int_all, na.rm = TRUE) < min(boot_assort_mean$int_all) # true
mean(assort_ALL$assort_int_20, na.rm = TRUE) < min(boot_assort_mean$int_20) # true
mean(int_10_finite) < min(boot_assort_mean$int_10)

# 1d: SIGNIFICANCE TESTING (table 5 generated from this section)
# - - - - - - - - - - - - -

# calculate anova values on three subsets for genre
assort_anovas  <- NULL # all 
assort_anova_xROM  <- NULL # no ROM
assort_anova_xOUT  <- NULL # no ROM and no YA
# remove NA's and non finite rows
assort_xALL  <- assort_ALL[is.finite(rowSums(assort_ALL[,7:12])),]
assort_xROM  <- assort_xALL[assort_xALL$genre != 'ROM',]
assort_xOUT  <- assort_xALL[assort_xALL$genre != 'ROM' & assort_xALL$genre != 'YA',]
for (i in 7:12){
  df  <- assort_xALL
  # df  <- assort_xROM
  # df  <- assort_xOUT
  label  <- colnames(assort_xALL)[i]
  # label  <- colnames(assort_xROM)[i]
  # label  <- colnames(assort_xOUT)[i]
  measure <- df[i]
  n  <- nrow(df)
  aov <- anova_func(measure, df$genre, df$ag)
  mean  <- mean(measure[,1], na.rm = TRUE)
  temp  <- cbind(label,aov,mean,n)
  assort_anovas  <- rbind(assort_anovas, temp)
  # assort_anova_xROM  <- rbind(assort_anova_xROM, temp)
  # assort_anova_xOUT  <- rbind(assort_anova_xOUT, temp)
}

# calculate t.tests for author gender 
assort_tt_xROM  <- NULL
for (i in 7:12){
  df  <- assort_xROM 
  label  <- colnames(assort_xROM)[i]
  df$measure <- df[i]
  n  <- nrow(df)
  tt  <- t.test(df$measure[df$ag == 'F',], df$measure[df$ag == 'M',], na.action = na.omit)
  t  <- num_func(tt[1])
  p  <- num_func(tt[3])
  m1  <- num_func(tt[[5]][1])
  sd1  <- sd(df$measure[df$ag == 'F',])
  m2  <- num_func(tt[[5]][2])
  sd2  <- sd(df$measure[df$ag == 'M',])
  temp  <- data.frame(cbind(label,t, p, m1, sd1, m2, sd2))
  assort_tt_xROM  <- data.frame(rbind(assort_tt_xROM, temp))
}


# 1e: FIGURES 5 & 6
# - - - - - - - - - 

genres  <- c('YA', 'NYT', 'BS', 'PW', 'MY', 'SF', 'ROM')
assort_ALL2  <- assort_ALL[assort_ALL$genre %in% genres,]

# figure 5
assort_rel_bp <- melt(assort_ALL2, id = "genre", measure = c("assort_rel_all", "assort_rel_20", "assort_rel_10"))
figure_5 <- ggplot(assort_rel_bp, aes(variable, value, position = "dodge", fill = genre)) +
  geom_boxplot ()+
  ggtitle(label = "Assortativity of Relationships") +
  scale_y_continuous(name = "Assortativity Score") + 
  scale_x_discrete(name = "Genre", labels = c("All", "Top20", "Top10")) +
  labs(fill="Genre") +
  theme_bw() +
  theme(plot.title = element_text(size = 13, family = "Times New Roman", face = "bold"),
        text = element_text(size = 11, family = "Times New Roman"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11)) +
  scale_fill_brewer(palette = "Set3")

# figure 6
assort_int_bp <- melt(assort_ALL2, id = "genre", measure = c("assort_int_all", "assort_int_20", "assort_int_10"))
figure_6 <- ggplot(assort_int_bp, aes(variable, value, position = "dodge", fill = genre)) +
  geom_boxplot() +
  ggtitle(label = "Assortativity of Interactions") +
  scale_y_continuous(name = "Assortativity Score") + 
  scale_x_discrete(name = "Genre", labels = c("All", "Top20", "Top10")) +
  theme_bw() +
  labs(fill='Genre') +
  theme(plot.title = element_text(size = 13, family = "Times New Roman", face = "bold"),
        text = element_text(size = 11, family = "Times New Roman"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11)) +
  scale_fill_brewer(palette = "Set3")


# 2: ANTAGONISM
# = = = = = = = = 

# 2a: CALCULATE ANTAGONISM
# - - - - - - - - - - - - -

# no bootstrap
setwd(wd)
files_n <- list.files('nodes_ALL')
files_e <- list.files('edges_ALL')
ant1 <- NULL
for (i in 1:length(files_e)){
  work <- files_n[i]
  if (work %in% files_noboot){
    genre <- as.character(metadata.df$genre[(which(metadata.df$ID %in% work))])
    ag <- as.character(metadata.df$ag[(which(metadata.df$ID %in% work))])
    if (is.null(genre)) {genre  <- 'catch'}
    # nodes 
    setwd(paste((wd),('nodes_ALL'),sep = "/"))
    nodes <- read.csv(work)
    nodes <- nodes[(nodes$GENDER != "?"),] 
    nodes_20 <- nodes[1:20,]
    # edges 
    setwd(paste((wd),('edges_ALL'),sep = "/"))
    edges <- read.csv(work)
    edges_20 <- edges[edges$NAME.1 %in% nodes_20$NAME & edges$NAME.2 %in% nodes_20$NAME,1:4]  
    edges_20$pair <- pair_func(nodes_20,edges_20)
    mm <- edges_20[edges_20$pair == "MM",] #subset by mm interactions
    # mixed <- edges_20[edges_20$pair == "MF" | edges_20$pair == "FM",]
    ww <- edges_20[edges_20$pair == "FF",]
    mm.sent <- (sum(mm$X..POS))/(sum(mm$X..NEG))
    # mixed.sent <- (sum(mixed$X..POS))/(sum(mixed$X..NEG))
    ww.sent <- (sum(ww$X..POS))/(sum(ww$X..NEG))
    temp.df <- data.frame(work, genre, ag, mm.sent, ww.sent)  
    ant1 <- rbind(ant1, temp.df)
  } 
}

# bootstrap
setwd(wd)
files_n <- list.files('nodes_ALL')
files_e <- list.files('edges_ALL')
ant2 <- NULL
for (i in 1:length(files_e)){
  work <- files_n[i]
  if (work %in% files_boot){
    ag <- as.character(metadata.df$ag[(which(metadata.df$ID %in% work))])
    genre <- as.character(top_chars$code[(which(top_chars$work %in% work))])
    if (is.null(genre)) {genre  <- 'catch'}
    # nodes 
    setwd(paste((wd),('nodes_ALL'),sep = "/"))
    nodes <- read.csv(files_n[i])
    nodes <- nodes[(nodes$GENDER != "?"),] #subsets nodes without ?
    nodes_20 <- nodes[1:20,] #subsets to top 20 
    # edges 
    setwd(paste((wd),('edges_ALL'),sep = "/"))
    edges <- read.csv(files_e[i])
    edges_20 <- edges[edges$NAME.1 %in% nodes_20$NAME & edges$NAME.2 %in% nodes_20$NAME,1:4]  
    # how many more women?
    adj_add <- bootstrap_files$adj_add[(which(bootstrap_files$work %in% work))]
    # if it's possible to add that many women 
    if (adj_add < length(which(nodes_20$GENDER != 'F'))){
      # empty vectors
      mm_sent <- vector()
      ww_sent <- vector()
      for (j in 1:10){
        # randomly sample the number of added women from the number of non-women and replace integers with 'F's
        # for first char
        rep <- sample(which(nodes_20$GENDER != 'F'), adj_add)
        nodes_20$adj_GENDER <- replace(as.character(nodes_20$GENDER), rep, 'F')
        edges_20$pair <- pair_func(nodes_20,edges_20)
        mm <- edges_20[edges_20$pair == "MM",] 
        mixed <- edges_20[edges_20$pair == "MF" | edges_20$pair == "FM",]
        ww <- edges_20[ edges_20$pair == "FF",]
        mm_sent[j] <- (sum(mm$X..POS))/(sum(mm$X..NEG))
        ww_sent[j] <- (sum(ww$X..POS))/(sum(ww$X..NEG))
      }
      mean_mm_sent <- mean(mm_sent)
      mean_ww_sent <- mean(ww_sent)
    } else {
      mean_mm_sent <- 'error'
      mean_ww_sent <- 'error'
    }
    temp.df <- data.frame(work, genre, ag, mean_mm_sent, mean_ww_sent)  
    ant2 <- rbind(ant2, temp.df)
  }
}

colnames(ant2)  <- colnames(ant1)
ant_ALL <- data.frame(rbind(ant1, ant2))
ant_ALL  <- ant_ALL[which(ant_ALL$ww.sent != 'error' & ant_ALL$mm.sent != 'error'),]

# 2b: REPORTED VALUES 
# - - - - - - - - - - 

# means 
mean(num_func(ant_sent2$value[ant_sent2$variable == 'ww.sent']), na.rm = TRUE) # mean sentiment of women
mean(num_func(ant_sent2$value[ant_sent2$variable == 'mm.sent']), na.rm = TRUE) # mean sentiment of men

# difference within a genre for BS and NYT
mean(num_func(ant_sent2$value[ant_sent2$genre == 'BS' & ant_sent2$variable == 'ww.sent']), na.rm = TRUE)-
  mean(num_func(ant_sent2$value[ant_sent2$genre == 'BS' & ant_sent2$variable == 'mm.sent']), na.rm = TRUE)
mean(num_func(ant_sent2$value[ant_sent2$genre == 'NYT' & ant_sent2$variable == 'ww.sent']), na.rm = TRUE)- 
  mean(num_func(ant_sent2$value[ant_sent2$genre == 'NYT' & ant_sent2$variable == 'mm.sent']), na.rm = TRUE)

# 2c: SIGNIFICANCE TESTING
# - - - - - - - - - - - - -  

# anova
# for genre across sentiment types
ant_sent_ag <- melt(ant_ALL, id = "ag", measure = c("ww.sent", "mm.sent"), na.action=na.omit, na.rm=TRUE)
ant_sent_genre  <- melt(ant_ALL, id = "genre", measure = c("ww.sent", "mm.sent"), na.action=na.omit, na.rm=TRUE)
ant_sent  <- data.frame(cbind('genre'=ant_sent_genre$genre,ant_sent_ag), row.names = NULL)

# for sentiment types
ant_sent2  <- ant_sent[ant_sent$value != 'error',]
ant_sent2  <- ant_sent2[is.finite(num_func(ant_sent2$value)),]
anova_sent <- summary(aov(value ~ variable, data=ant_sent2)) 

# for influence of author genre / genre
ant_sent3  <- ant_sent2[ant_sent2$ag == 'M' | ant_sent2$ag == 'F',]
anova_func(ant_sent2$value, ant_sent2$genre, ant_sent2$ag)


# 3: BALANCE
# = = = = = = = = 

# 3a: CALCULATE BALANCE
setwd(wd)
files_n <- list.files('nodes_ALL')
files_e <- list.files('edges_ALL')
triples_ALL <- NULL
for (i in 1:length(files_n)) {
  setwd(paste((wd),('nodes_ALL'),sep = "/"))
  node <- read.csv(files_n[i])
  setwd(paste((wd),('edges_ALL'),sep = "/"))
  edge <- read.csv(files_e[i])
  work <- files_e[i]
  #prepare for igraph
  edge1 <- edge[,1:2]
  #make a "graph object"
  g <- graph.data.frame(edge1, directed=FALSE, vertices=NULL)
  #make a second that collapses all interactions into *unique* interactions w edge weights
  #set attributes
  V(g)$name <- as_ids(V(g))
  V(g)$gender <-as.character(node$GENDER[match(V(g)$name,node$NAME)])
  #remove unknown genders
  g_clean<-delete_vertices(g, which(V(g)$gender == "?")) 
  if (length(V(g_clean)$gender) > 10) {
    #first extract top 10 most connected characters
    top10 <- names(sort(degree(g_clean), decreasing = T)[1:10])
    non.top10<-names(sort(degree(g_clean), decreasing = T)[11:length(V(g_clean))])
    #reduce graph to these characters
    g.sub<-delete_vertices(g_clean, v=non.top10)
    #get cliques of size 3
    c <- cliques(g.sub, min = 3, max = 3)
    #turn cliques into a table of triples
    if (length(c) > 1){
      triples<-NULL
      for (m in 1:length(c)){
        temp<-names(unlist(c[[m]]))
        triples<-(rbind(triples, temp))
      }
      row.names(triples)<-c(1:nrow(triples))
      triples<-as.data.frame(triples)
      colnames(triples)<-c("name1", "name2", "name3")
      edge.balance<-edge[,1:4]
      #set gender code for each row 
      trio.type<-vector()
      balance.type<-vector()
      for (k in 1:nrow(triples)){        
        c1<-as.character(node[which(as.character(node$NAME) == as.character(triples$name1[k])),3])
        c2<-as.character(node[which(as.character(node$NAME) == as.character(triples$name2[k])),3])
        c3<-as.character(node[which(as.character(node$NAME) == as.character(triples$name3[k])),3])
        trio.gender<-paste(c1,c2,c3, sep="")
        if (trio.gender == "MMM"){
          trio.type<-append(trio.type, "0.w")
        } else if (trio.gender == "FFF") {
          trio.type<-append(trio.type, "3.w")
        } else if (str_count(trio.gender, "F") == 2){
          trio.type<-append(trio.type, "2.w")
        } else {
          trio.type<-append(trio.type, "1.w")
        }
        #now get balance
        #smooth values
        edge.balance$X..POS<-edge.balance$X..POS+.0001
        edge.balance$X..NEG<-edge.balance$X..NEG+.0001
        #find pairs
        pair1<-edge.balance[as.character(edge.balance$NAME.1) == as.character(triples$name1[k]) | as.character(edge.balance$NAME.2) == as.character(triples$name1[k]),]
        pair1<-pair1[as.character(pair1$NAME.2) == as.character(triples$name2[k]) | as.character(pair1$NAME.1) == as.character(triples$name2[k]),]
        pair2<-edge.balance[as.character(edge.balance$NAME.1) == as.character(triples$name2[k]) | as.character(edge.balance$NAME.2) == as.character(triples$name2[k]),]
        pair2<-pair2[as.character(pair2$NAME.2) == as.character(triples$name3[k]) | as.character(pair2$NAME.1) == as.character(triples$name3[k]),]
        pair3<-edge.balance[as.character(edge.balance$NAME.1) == as.character(triples$name3[k]) | as.character(edge.balance$NAME.2) == as.character(triples$name3[k]),]
        pair3<-pair3[as.character(pair3$NAME.2) == as.character(triples$name1[k]) | as.character(pair3$NAME.1) == as.character(triples$name1[k]),]
        #establish book sentiment ratio
        s<-sum(edge.balance$X..POS)/sum(edge.balance$X..NEG)
        #find pair ratios
        sum1<-sum(pair1$X..POS)/sum(pair1$X..NEG)
        sum2<-sum(pair2$X..POS)/sum(pair2$X..NEG)
        sum3<-sum(pair3$X..POS)/sum(pair3$X..NEG)
        sum.all<-c(sum1, sum2, sum3)
        #+++
        if (sum1 > s & sum2 > s & sum3 > s){
          balance.type<-append(balance.type,"b")
          #---
        } else if (sum1 < s & sum2 < s & sum3 < s){
          balance.type<-append(balance.type,"u")
        } else if (length(which(sum.all < s)) == 2){
          balance.type<-append(balance.type,"u")
        } else {
          balance.type<-append(balance.type,"b")
        }
      }
      genre <- as.character(metadata.df$genre[(which(top_chars$work %in% work))][1])
      if (is.null(genre)) {genre  <- 'catch'}
      ag <- as.character(metadata.df$ag[(which(metadata.df$ID %in% work))][1]) 
      temp<-data.frame(work, ag, genre, trio.type, balance.type)
      triples_ALL<-data.frame(rbind(triples_ALL,temp))
    }
  }
}

# 3b: REPORTED VALUES 
# - - - - - - - - - - 

sum(triangles.sum) # total triangles

# by balance type
length(which(triples_ALL$balance.type == "b")) # balanced
length(which(triples_ALL$balance.type == "u")) # unbalanced

# by gender type 
length(which(triples_ALL$trio.type=="3.w")) # www
length(which(triples_ALL$trio.type=="2.w")) # wwm
length(which(triples_ALL$trio.type=="1.w")) # wmm
length(which(triples_ALL$trio.type=="0.w")) # mmm 

# balanced / unbalanced by genre
bu_genre <- NULL
for (i in 1:nlevels(triples_ALL$genre)){
  genre  <- levels(triples_ALL$genre)[i]
  b <- length(which(triples_ALL$genre == genre & triples_ALL$balance.type == "b"))
  u <- length(which(triples_ALL$genre == genre & triples_ALL$balance.type == "u"))
  b_u  <- b/u
  temp <- data.frame(cbind(b,u, b_u))
  rownames(temp)  <- genre
  bu_genre  <- rbind(bu_genre, temp)
}

sum(num_func(bu_genre$b))/sum(num_func(bu_genre$u)) # overall ratio of b / u 

# by triangle type
bu_type <- NULL
for (i in 1:nlevels(triples_ALL$trio.type)){
  type  <- levels(triples_ALL$trio.type)[i]
  b <- length(which(triples_ALL$trio.type == type & triples_ALL$balance.type == "b"))
  u <- length(which(triples_ALL$trio.type == type & triples_ALL$balance.type == "u"))
  b_u  <- b/u
  temp <- data.frame(cbind(b,u, b_u))
  rownames(temp)  <- type
  bu_type  <- rbind(bu_type, temp)
}

# author gender
triples_ag<-triples_ALL[triples_ALL$ag != "X" & triples_ALL$ag != "T" & triples_ALL$genre != "ROM",]
bu_ag <- NULL
for (i in 1:nlevels(triples_ag$ag)){
  type  <- levels(triples_ag$ag)[i]
  b <- length(which(triples_ag$ag == type & triples_ag$balance.type == "b"))
  u <- length(which(triples_ag$ag == type & triples_ag$balance.type == "u"))
  b_u  <- b/u
  temp <- data.frame(cbind(b,u, b_u))
  rownames(temp)  <- type
  bu_ag  <- rbind(bu_ag, temp)
}

# 3c: SIGNIFICANCE TESTING 
# - - - - - - - - - - - - -

chisq.test(t(bu_genre[1:2])) # not sig 
chisq.test(t(bu_ag[1:2,1:2])) # not sig 
fisher.test(data.frame(rbind(bu_type[1,1:2], bu_type[4,1:2]))) # not sig 

# 3d: FIGURE 8
# - - - - - - -

figure_8 <- ggplot(triples_ALL, aes(balance.type, ..count..)) + 
  geom_bar(aes(fill = trio.type), position = "dodge") +
  ggtitle(label = "Balanced and Unbalanced, by Gender Type") +
  ylab("Number of Triangles") +
  theme_bw() +
  scale_x_discrete(name = "Gender Type", labels = c("Balanced", "Unbalanced")) +
  scale_fill_manual(values=c("darkorange2","tan1", "thistle3", "mediumpurple3"), labels = c("MMM", "WMM", "WWM", "WWW")) +
  labs(fill="Triangle Type") +
  theme(plot.title = element_text(size = 13, family = "Times New Roman", face = "bold"),
        text = element_text(size = 11, family = "Times New Roman"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11))
