# uploads ####
options(scipen = 999)
library(gdata)
# for crawling Twitter data 
library(academictwitteR)
# library(rtweet)
library(corpus)
library(quanteda)
library(udpipe)
library(stopwords)
library(corpustools)
library(quanteda.textstats)
library(dplyr)
library(tidyverse)
library(lubridate)
library(tokenizers)
library(tidytext)
library(stringi)
library(readtext)
library(parallel)
library(stringr)
library(widyr)
library(irlba)
library(furrr)
library(stm)
library(slider)
library(ggthemes)
library("stminsights")
library("gridExtra")

"%nin%" <- Negate("%in%")

setwd("C:/Users/rocpa/OneDrive/Desktop/CNT/twitter_unsupervised/from_server/16_11_2022/")

load("df_20_22.Rdata")
df_20_22 <- df_20_22[df_20_22$selected_2 == 1,]
# df_20_22 <- rename(df_20_22, original_language = text)
# df_20_22 <- rename(df_20_22, text = en_text)
# save(df_20_22,file="df_20_22.Rdata")

df_20_22$text <- str_replace_all(df_20_22$text,"\\b's\\b","") 
df_20_22$text <- str_replace_all(df_20_22$text,"\\b-\\b","_") 

key_de <- read.xls("keywords_cnt.xls",sheet = "lemma_de", encoding = "latin1")[,1]
key_de <- paste0("\\b",key_de,"\\b")
lemma_de <- read.xls("keywords_cnt.xls",sheet = "lemma_de", encoding="latin1" )[,2]
# lemma_de <- paste0("\\b",lemma_de,"\\b")
names(lemma_de) <- key_de
df_20_22[df_20_22$country == "Germany",]$text <- str_replace_all(df_20_22[df_20_22$country == "Germany",]$text,lemma_de) 

key_it <- read.xls("keywords_cnt.xls",sheet = "lemma_it", encoding = "latin1")[,1]
key_it <- paste0("\\b",key_it,"\\b")
lemma_it <- read.xls("keywords_cnt.xls",sheet = "lemma_it", encoding="latin1" )[,2]
# lemma_de <- paste0("\\b",lemma_de,"\\b")
names(lemma_it) <- key_it
df_20_22[df_20_22$country == "Italy",]$text <- str_replace_all(df_20_22[df_20_22$country == "Italy",]$text,lemma_it) 

#

# engsing <- read.xls("keywords_cnt.xls",sheet = "eng_sing", encoding = "latin1")[,1]
# engsing <- paste0("\\b",engsing,"\\b")
# engsinglem <- read.xls("keywords_cnt.xls",sheet = "eng_sing", encoding = "latin1")[,2]
# names(engsinglem) <- engsing
# df_20_22$text <- str_replace_all(df_20_22$text,engsinglem) 


# # English
eng <- read.xls("keywords_cnt.xls",sheet = "lemma_eng", encoding = "latin1")[,1]
eng <- paste0("\\b",eng,"\\b")
lemma_eng <- read.xls("keywords_cnt.xls",sheet = "lemma_eng", encoding="latin1" )[,2]
# lemma_de <- paste0("\\b",lemma_de,"\\b")
names(lemma_eng) <- eng
df_20_22$text <- str_replace_all(df_20_22$text,lemma_eng)

# # compounds

cmpd <- read.xls("keywords_cnt.xls",sheet = "compound_dfm", encoding = "latin1")[1:2397,1]
cmpd <- paste0("\\b",cmpd,"\\b")
cmpdlinked <- read.xls("keywords_cnt.xls",sheet = "compound_dfm", encoding="latin1" )[1:2397,2]
names(cmpdlinked) <- cmpd
df_20_22$text <- str_replace_all(df_20_22$text,cmpdlinked)

#  
#compond_dfm <- read.xls("keywords_cnt.xls",sheet = "compound_dfm", encoding = "latin1")[,1]
rem_dfm <- read.xls("keywords_cnt.xls",sheet = "rem_dfm", encoding = "latin1")[,1]
#
bigrams <- read.csv("bigrams_4.csv",sep=";") 
trigrams_can <- trigrams %>% filter(str_starts(trigram,"can"))

write.csv(bigrams_be,file="bigrams_be.csv",row.names = F)



bigrams_tx  <- df_20_22[,-4] %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)
bigrams_tx  %>% dplyr::count(bigram, sort = TRUE)
bigrams_separate  <- bigrams_tx  %>% separate(bigram,c("word1","word2"),sep=" ")
bigrams_filtered  <- bigrams_separate  %>%
  filter(!word1 %in% stopwords_en) %>%
  filter(!word2 %in% stopwords_en)
bigrams_filtered <- bigrams_filtered  %>%  dplyr::count(word1, word2, sort = TRUE)
bigrams_united <- bigrams_filtered  %>% unite(bigram, word1, word2, sep = " ")
bigrams_united  <- bigrams_united$bigram
bigrams_united <- unique(bigrams_united)

write.csv(bigrams_united,"bigrams3_fin.csv",row.names= F)

trigrams_tx <- df_20_22[,-4] %>% unnest_tokens(trigram, text, token = "ngrams", n = 3)
trigrams_tx %>% dplyr::count(trigram, sort = TRUE)
trigrams_separate  <- trigrams_tx %>% separate(trigram,c("word1","word2","word3"),sep=" ")
trigrams_filtered <- trigrams_separate  %>%
  filter(!word1 %in% stopwords_en) %>%
  filter(!word2 %in% stopwords_en) %>%
  filter(!word3 %in% stopwords_en)
trigrams_filtered  <- trigrams_filtered %>%  dplyr::count(word1, word2,word3, sort = TRUE)
trigrams_united  <- trigrams_filtered  %>% unite(trigram, word1, word2,word3, sep = " ")
trigrams_united <- trigrams_united$trigram
trigrams_united <- unique(trigrams_united)
# 
write.csv(trigrams_united,"trigrams3_fin.csv",row.names= F)

quadrigrams_tx  <- df_20_22[,-4] %>% unnest_tokens(quadrigram, text, token = "ngrams", n = 4)
quadrigrams_tx  %>% dplyr::count(quadrigram, sort = TRUE)
quadrigrams_separate  <- quadrigrams_tx  %>% separate(quadrigram,c("word1","word2","word3","word4"),sep=" ")
quadrigrams_filtered  <- quadrigrams_separate  %>%
  filter(!word1 %in% stopwords_en) %>%
  filter(word2 == "and") %>%
  filter(!word3 %in% stopwords_en) %>%
  filter(!word4 %in% stopwords_en)
quadrigrams_filtered <- quadrigrams_filtered  %>%  dplyr::count(word1, word2,word3,word4, sort = TRUE)
quadrigrams_united <- quadrigrams_filtered  %>% unite(quadrigrams, word1, word2,word3,word4, sep = " ")
# quadrigrams_united  <- quadrigrams_united$quadrigram
# quadrigrams_united <- unique(quadrigrams_united)

write.csv(quadrigrams_united,"quadrigrams3_fin.csv",row.names= F)


##
# zukunftspaket
unique(stringr::str_extract_all(df_20_22[df_20_22$selected_2 == 1 & df_20_22$country == "Italy",]$text,
        "\\b[:alnum:]*\\-?[:alnum:]*\\-?[:alnum:] sized supplier\\-?[:alnum:]*\\-?[:alnum:]*\\b"))

wordouble <- unique(stringr::str_extract_all(df_20_22[df_20_22$country=="Italy",]$original_language,
                                     "\\b\\-?[:alnum:]*s\\b"))

wordouble <- unlist(wordouble)
wordouble <- unique(wordouble)
wordouble
write.csv(wordouble,file="digital.csv",row.names = F)

txt <- df_20_22 %>% filter(str_detect(text,"\\bstimulus_package\\b") & str_detect(text,"\\bclimate_protection\\b"))

# sector # social
compond_dfm <- c(wordouble,compond_dfm)
compond_dfm <- unique(compond_dfm)
compond_dfm <- compond_dfm[!is.na(compond_dfm)]



tstcheck <- df_20_22 %>% filter(str_detect(original_language,regex("cigo")))
tstcheck$text

textfreq <- textstat_frequency(dfm_it_en)
textstat_frequency(dfm_it_en) %>% subset(feature %in% "dc_reliefs_3") 
ck <- textstat_frequency(dfm_de) %>% subset(feature %in% key_de) 

##

dfm_de_en <- tokens(corpus(df_20_22[df_20_22$country == "Germany",]),
                remove_punct = TRUE, remove_numbers = TRUE,remove_url = TRUE) %>%
 # tokens_wordstem() %>%
 # tokens_compound(phrase(c(compond_dfm))) %>%
  tokens_remove(c("http*","@*","€","+","|","faq","=",stopwords("en"),stopwords_en,
                  rem_dfm)) %>%
  dfm()

dfm_de_ennopol <- dfm_subset(dfm_de_en, actor %in% c("TU","TA"))
save(dfm_de_ennopol,file="dfm_de_ennopol.Rdata")

dfm_de_en
topfeatures(dfm_de_en,50)
save(dfm_de_en,file="dfm_de_en.Rdata")
#
dfm_it_en <- tokens(corpus(df_20_22[df_20_22$country == "Italy",]),
                 remove_punct = TRUE, remove_numbers = TRUE,remove_url = TRUE) %>%
  # tokens_wordstem() %>%
 # tokens_compound(phrase(c(compond_dfm))) %>%
  tokens_remove(c("http*","@*","€","+","|","faq","l","il","=",stopwords("en"),stopwords_en,
                  rem_dfm)) %>%
  dfm()

dfm_it_ennpol <- dfm_subset(dfm_it_en,actor %in% c("TU","TA"))
save(dfm_it_ennpol,file="dfm_it_ennpol.Rdata")

dfm_it_en
topfeatures(dfm_it_en,50)

save(dfm_it_en,file="dfm_it_en.Rdata")



# Results ####
de_policies <- c(
  "corona_aid",
  "corona_emergency_aid",
  "economic_stabilization_fund",
  "stimulus_package",
  "social_protection_package",
  "social_protection_package_2",
  "social_protection_package_3",
  "bridging_aid",
  "corona_tax_assistance",
  "bridging_aid_2",
  "november_aid",
  "december_aid",
  "november_december_aid",
  "bridging_aid_3",
  "new_start_aid",
  "bridging_aid_3_plus",
  "bridging_aid_4",
  "new_start_aid_plus",
  "new_start_aid22",
  "recoveryplan",
  "digital_pact",
  "next_generation",
  "darp",
  "reacteu",
  "esf"
)

it_policies <- c(
  "dc_aiuti",
  "dc_cureitaly",
  "dc_liquidity",
  "dc_relaunch",
  "dc_august",
  "dc_reliefs",
  "dc_reliefs_2",
  "dc_reliefs_3",
  "dc_reliefs_4",
  "dc_supports",
  "dc_supports_2",
  "dc_supports_3",
  "next_generation",
  "nextgenerationeu",
  "recoveryplan",
  "pnrr",
  "reacteu",
  "esf"
)



folder <- "data_new/save_14/"
# sample

load("df_20_22.Rdata")
df_20_22 <- df_20_22[df_20_22$selected_2 == 1,]

ggplot(df_20_22,aes(x = actor)) + geom_bar() +
  geom_text(aes(label = ..count..), stat = "count", vjust = 0) +
  facet_wrap(~country) +
  theme_bw()
ggsave(paste0(folder,"results/sample.jpg"),width = 5, height = 4)

# Extraction

load(paste0(folder,"stm_de30cn.Rdata"))
# load("data_new/stm_it20cnnoint.Rdata")
# load("df_20_22.Rdata")
load(paste0(folder,"dfm_de_en.Rdata"))
# dfm_it_en <- dfm_subset(dfm_it_en,actor %in% c("TU","TA"))
stm_m <- stm_de30cn
stm_m
stm_m$settings$call
numm <- 30
tpreg <- "cn"
stm_df <- quanteda::convert(dfm_de_en,to = "stm") 
#stm_df_it <- quanteda::convert(dfm_it_en,to = "stm")
# df_20_22 <- df_20_22[df_20_22$selected_2  == 1,]
dfb  <- df_20_22[df_20_22$country == "Italy" & df_20_22$selected_2 == 1,]
dfb <- df_20_22[df_20_22$country == "Germany" & df_20_22$selected_2 == 1,]
titleplot <- unique(dfb$country)

# to make the dataframe on drive: top probability words, frex, text with findthoughts
# probability and frex
# tb <- labelTopics(stm_m,1:numm,20)
# df <- tibble(topic = tb$topicnums,prob = tb$prob,frex = tb$frex)

sg <- sageLabels(stm_m,10)
sg_prob <- tibble(topic = 1:numm,sg$marginal$prob)
sg_frex <- tibble(topic = 1:numm,sg$marginal$frex)
sg_prob_pol <- tibble(topic = 1:numm,sg$covnames[[1]],sg$cov.betas[[1]]$problabels) # POL
sg_prob_ta <- tibble(topic = 1:numm,sg$covnames[[2]],sg$cov.betas[[2]]$problabels) # TA
sg_prob_tu <- tibble(topic = 1:numm,sg$covnames[[3]],sg$cov.betas[[3]]$problabels) # TU
sg_frex_pol <- tibble(topic = 1:numm,sg$covnames[[1]],sg$cov.betas[[1]]$frexlabels) # POL
sg_frex_ta <- tibble(topic = 1:numm,sg$covnames[[2]],sg$cov.betas[[2]]$frexlabels) # TA
sg_frex_tu <- tibble(topic = 1:numm,sg$covnames[[3]],sg$cov.betas[[3]]$frexlabels) # TU

# longsgde25 <- cbind(sg_prob,sg_frex,sg_prob_pol,sg_prob_ta,sg_prob_tu,sg_frex_pol,sg_frex_ta,sg_frex_tu)
# write.csv(it20noint,file="data_new/it20noint.csv",row.names = F, col.names=T,   fileEncoding = "UTF-8")




# For Findthoughts: it uses the whole dataset, 
# it takes the text corresponding to number row corresponding to text in Findthoughts. 
# Plotquotes shows the text corresponding to a document, i.e. a row in dfm
# I have checked, it works

# select dataframe (to use both languages)

# run findthoughts for each topic in the model (i in 1:number(topics), here 30),
# it attaches text and other document info

thoughts <- list()
for (i in 1:numm){ # 
  # thg_det <- findThoughts(stm_m, texts = df_de$text,n = 3, topics =i)$docs[[1]]
  thought_text = list()
  dates <- findThoughts(stm_m, texts = dfb$text,n = 20, topics =i)$index[[1]] # 
  for (n in dates) {
    txx <-  print(c(paste0(" ACT: ", dfb[n,]$actor,
                           " ACTsj: ", dfb[n,]$user_username," TXT: ",dfb[n,]$text," DATE: ", dfb[n,]$created_at,
                           " TWID: ",dfb[n,]$tweet_id," CNID ",dfb[n,]$conversation_id)))
    thought_text[[n]] <- txx
    thought_textfin <- do.call(rbind.data.frame, thought_text)
  }
  thoughts[[i]] <- thought_textfin
  
 }
bind_rows(thoughts)
thoughts <- do.call(rbind.data.frame, lapply(thoughts,'[[',1))
colnames(thoughts) = c("1", "2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20") # columns where texts go
thoughts <- cbind(topic = 1:numm,thoughts)


#
# short_report <- cbind(sg_prob,sg_frex,thoughts)
# write.csv(short_report,file=paste("data_new/shortrep_",titleplot,numm,".csv"), row.names = F, col.names=T,  sep=";",  fileEncoding = "UTF-8")
long_report <- cbind(sg_prob,sg_frex,sg_prob_pol,sg_prob_ta,sg_prob_tu,sg_frex_pol,sg_frex_ta,sg_frex_tu,thoughts)
write.csv(long_report,file=paste0(folder,"results/longrep_",titleplot,tpreg,numm,".csv"),row.names = F, col.names=T,  sep=";",  fileEncoding = "UTF-8")

# labelTopics (model withouth content)
# df <- cbind(df,thoughts) # combine information from prob&frex words and texts for topic
# # save locally the dataframe as csv, as the one in Drive
# write.table(df,file=paste("data_new/",titleplot,tpreg,numm,".csv"),row.names = F, col.names=T,  sep=";",  fileEncoding = "UTF-8") # 


# plot
# stm_m <- stm_it20cn
# dfb <- dfb_it
# stm_df <- stm_df_it
# titleplot <- unique(dfb$country)
td_gamma <- tidy(stm_m, matrix = "gamma")
ID_row <- names(stm_df$documents) # the name of documents gets lost, the row number is reported
td_gamma <- cbind(td_gamma,ID_row) # Here I map each document to its name via row, I checked with content, it works
td_gamma <- cbind(td_gamma,dfb) # merge the gamma matrix with the dataframe via ID, so the variables to sort documents can be used
# td_gamma2 <- merge(td_gamma, by =  rownames())


# tidy global topic proportion

top_terms <- tidy(stm_m) %>%
  arrange(beta) %>%
  group_by(topic) %>%
  filter(term %nin% it_policies) %>%
  top_n(7, beta) %>%
  arrange(-beta)%>%
  select(topic, term) %>%
  summarise(terms = list(unique(term))) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest()

gamma_terms <- td_gamma %>%
  group_by(topic) %>%
 # filter(word %nin% it_policies) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  left_join(top_terms, by = "topic") %>%
  mutate(gamma = paste0("(",round(gamma,4)*100,"%)")) 
# %>%
#     mutate(topic = paste(topic,gamma),
#      topic = reorder(topic, gamma))

# SANKEY
library(networkD3)
library(dplyr)

tidystm <- tidy(stm_m)  %>% group_by(term,topic) %>%  summarise(beta = mean(beta))
# tidystm <- rename(tidystm, actor = y.level)

a <- tidystm %>% filter(term %in% it_policies)
a$topic <- as.character(a$topic)
gamma_terms$terms <- as.character(gamma_terms$terms)

# a$topic <- paste0("Topic ",as.character(a$topic))

# a[a$topic == 1,]$topicint <-  "1: first phase"
# a[a$topic == 2,]$topicint <-  "2: emergence credit"
# a[a$topic == 6,]$topicint <-  "3: small-business-enterprise"
# a[a$topic == 4,]$topicint <-  "4: liquidity"
# a[a$topic == 5,]$topicint <-  "5: stability"
# a[a$topic == 7,]$topicint <-  "7: employment equity"
# a[a$topic == 8,]$topicint <-  "8: target groups"
# a[a$topic == 12,]$topicint <-  "12: social confrontations"

for (i in c(1:30)){
  
  a[a$topic == i,]$topic <- paste(a[a$topic == i,]$topic,gamma_terms[gamma_terms$topic == i,]$gamma,":", gamma_terms[gamma_terms$topic == i,]$terms)
  
}


# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(a$term), 
         as.character(a$topic)) %>% unique()
)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
a$IDsource <- match(a$term, nodes$name)-1 
a$IDtarget <- match(a$topic, nodes$name)-1


# Make the Network
sankeyNetwork(Links = a, Nodes = nodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "beta", NodeID = "name",
              fontSize=16,
              sinksRight =FALSE)
saveNetwork(sn, "sn.html")
library(webshot)
webshot::install_phantomjs()
webshot("sn.html","sn.png", vwidth = 1000, vheight = 900)










# gamma_terms %>%
#   # top_n(20, gamma) %>%
#   ggplot(aes(topic, gamma, label = terms, fill = topic)) +
#   geom_col(show.legend = FALSE) +
#   geom_text(hjust = 0.32, nudge_y = 0.00005, size = 8, # 0.0005
#             family = "IBMPlexSans") +
#   coord_flip() +
#   scale_y_continuous(expand = c(0,0),
#                      limits = c(0, 0.10) , 
#                      labels = scales::percent_format() ) +
#   ggtitle(titleplot) +
#   theme_tufte(base_family = "IBMPlexSans", ticks = FALSE) +
#   theme(plot.title = element_text(size = 20,
#                                   family="IBMPlexSans-Bold"),
#         plot.subtitle = element_text(size = 13),
#         axis.text.y = element_text(size = 15)) +
#   labs(x = NULL, y = expression(gamma))
# ggsave(paste0(folder,"results/",titleplot,"_glob",numm,".jpg"),width = 27,height = 12)


# average expected topic (gamma probability) for actors
# td_gamma %>%  group_by(actor,topic) %>% summarize(gamma = mean(gamma)) %>%
#   arrange(desc(gamma)) %>%
#   top_n(7, gamma) %>%
#   ggplot(aes(factor(topic),gamma,fill = factor(topic)),label = terms) + geom_col(show.legend = FALSE) +
#   facet_wrap(~actor, ncol=1) +
#   xlab("Topic") + ylab("Avg expect. proportion") +
#   coord_flip() +
#   ggtitle(td_gamma$country) + theme_bw()

td_gamma %>%
  group_by(actor,topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
 # top_n(7, gamma) %>%
 # filter(actor == "POL") %>%
  ggplot(aes(reorder(factor(topic), gamma),gamma, fill = factor(topic))) + 
  geom_col(show.legend = FALSE) +
  # geom_text(hjust = 0.32, nudge_y = 0.00005, size = 8, # 0.0005
  #           family = "IBMPlexSans") +
  facet_wrap(~ actor, scales = "free_x", 
  labeller = labeller(actor = c("TU" = "Trade Unions","TA" = "Trade Associations", "POL" = "Political Actors"))) +
  xlab("Topic") + ylab("Avg expect. proportion by actors") +
  coord_flip() +
  ggtitle(titleplot) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        strip.text.x = element_text(size = 15))
ggsave(paste0(folder,"/results/",titleplot,"_actor",numm,".jpg"),width = 11,height = 5)

#
td_gamma %>%
  group_by(actor,topic) %>%
  summarise(gamma = mean(gamma))  %>%
  ggplot(aes(x = actor,y = gamma, fill = factor(topic), labels = factor(topic))) +
  geom_bar(position="fill", stat="identity")  

td_gamma %>%
  group_by(actor,topic) %>%
   summarise(gamma = mean(gamma))  %>%
  ggplot(aes(x = topic,y = gamma, fill = factor(topic), labels = factor(topic))) +
  geom_bar( stat="identity") + facet_wrap(~ actor)

 

#
tidystm <- tidy(stm_m)
tidystm <- rename(tidystm, actor = y.level)


top_termsact <- tidystm %>%
  arrange(beta) %>%
  group_by(topic,actor) %>%
  top_n(3, beta) %>%
  arrange(-beta)  %>%
  select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest()

for (i in 1:numm) {
  

tidystm %>% filter(topic == i) %>%
    filter(term %nin% it_policies) %>%
  group_by(actor) %>%
  arrange(-beta) %>%
  top_n(10,beta) %>%
  ggplot(aes(reorder(term,beta),beta,fill = actor)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ actor, scales = "free",
             labeller = labeller(actor = c("TU" = "Trade Unions","TA" = "Trade Associations", "POL" = "Political Actors"))) +         
  coord_flip() +
  ggtitle(paste("Topic: ",i)) +
 xlab("") +
  ylab("Probability words per actor") +
  theme(axis.title.x = element_blank()) +
  theme_bw()
ggsave(paste0(folder,"/results/",titleplot,"_wordactor",i,".jpg"),width = 11,height = 5) 
  
} 




# gamma_termsact <- td_gamma %>%
#   group_by(topic) %>%
#   summarise(gamma = mean(gamma)) %>%
#   arrange(desc(gamma)) %>%
#   left_join(top_terms, by = "topic") %>%
#   mutate(topic = paste0("Topic ", topic),
#          topic = reorder(topic, gamma))

gamma_termsact <- td_gamma %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  left_join(top_termsact, by = "topic") %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))


# gamma_termsact %>%
#   group_by(actor,topic) %>%
#   summarise(gamma = mean(gamma)) %>%
#   #  arrange(desc(gamma)) %>%
#   top_n(7, gamma) %>%
#   # filter(actor == "POL") %>%
#   ggplot(aes(reorder(factor(topic), gamma),gamma, fill = factor(topic))) + 
#   geom_col(show.legend = FALSE) +
#   # geom_text(hjust = 0.32, nudge_y = 0.00005, size = 8, # 0.0005
#   #           family = "IBMPlexSans") +
#   facet_wrap(~ actor, ncol=1, scales = "free_y", 
#              labeller = labeller(actor = c("TU" = "Trade Unions","TA" = "Trade Associations", "POL" = "Political Actors"))) +
#   xlab("Topic") + ylab("Avg expect. proportion by actors") +
#   coord_flip() +
#   ggtitle(titleplot) +
#   theme_bw() +
#   theme(axis.text.y = element_text(size = 15),
#         axis.text.x = element_text(size = 15),
#         strip.text.x = element_text(size = 15))


gamma_termsact %>%
  # top_n(20, gamma) %>%
  ggplot(aes(topic, gamma, label = terms, fill = topic)) +
  geom_col(show.legend = FALSE) +
  geom_text(hjust = 0.32, nudge_y = 0.00005, size = 8, # 0.0005
            family = "IBMPlexSans") +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.10) , 
                     labels = scales::percent_format() ) +
  ggtitle(titleplot) +
  facet_wrap(~ actor,dir="v") +
  theme_tufte(base_family = "IBMPlexSans", ticks = FALSE) +
  theme(plot.title = element_text(size = 20,
                                  family="IBMPlexSans-Bold"),
        plot.subtitle = element_text(size = 13),
        axis.text.y = element_text(size = 15)) +
  labs(x = NULL, y = expression(gamma))


td_gamma %>%
  group_by(actor,topic) %>%
  summarise(gamma = mean(gamma)) %>%
  #  arrange(desc(gamma)) %>%
  top_n(7, gamma) %>%
  # filter(actor == "POL") %>%
  ggplot(aes(reorder(factor(topic), gamma),gamma, fill = factor(topic))) + 
  geom_col(show.legend = FALSE) +
  # geom_text(hjust = 0.32, nudge_y = 0.00005, size = 8, # 0.0005
  #           family = "IBMPlexSans") +
  facet_wrap(~ actor, ncol=1, scales = "free_y", 
             labeller = labeller(actor = c("TU" = "Trade Unions","TA" = "Trade Associations", "POL" = "Political Actors"))) +
  xlab("Topic") + ylab("Avg expect. proportion by actors") +
  coord_flip() +
  ggtitle(titleplot) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        strip.text.x = element_text(size = 15))

# Correlation topic

corrtop <- topicCorr(stm_m, method = "huge")
plot(corrtop)

# TIME EVOLUTION

stm_df_it <- quanteda::convert(dfm_it_en,to = "stm")
stm_df_de <- quanteda::convert(dfm_de_en,to = "stm")

prep_it <- estimateEffect(1:numm ~ actor * s(datenum), stm_m, metadata = stm_df$meta, uncertainty = "Global")
prep_de <- estimateEffect(1:numm ~ actor * s(datenum), stm_m, metadata = stm_df$meta, uncertainty = "Global")

# prep_itnomod <- estimateEffect(1:25 ~ actor * s(datenum), stm_m, metadata = stm_df_it$meta, uncertainty = "Global")
# prep_denomod <- estimateEffect(1:25 ~ actor * s(datenum), stm_m, metadata = stm_df$meta, uncertainty = "Global")
# plot.estimateEffect(prep,covariate="datenum",topics=prep$topics[1],method="continuous",ylim=c(-0.07,0.3),linecol="blue",printlegend = F)


# ntp <- 1
# tt <- "Companies Recovery"
# par(bty="n",lwd=3,xaxt="n")
# plot.estimateEffect(prep,covariate="datenum",topics=prep$topics[ntp],method="continuous",
#                     moderator="actor",moderator.value="TA",ylim=c(-0.07,0.3),linecol="blue",printlegend = F)
# plot.estimateEffect(prep,covariate="datenum",topics=prep$topics[ntp],method="continuous",
#                     moderator="actor",moderator.value="TU",ylim=c(-0.07,0.3),linecol="green",printlegend=F,add=T)
# plot.estimateEffect(prep,covariate="datenum",topics=prep$topics[ntp],method="continuous",
#                     moderator="actor",moderator.value="POL",linecol="red",printlegend = F,add=T)
# par(xaxt="s")
# axis(1,at=c(18290,18414,18718,18841,18996,19250,19357), # when date is an integer (datenum), it reports name associated with date that are universal.
#      # Here it is mapped so that date appears in format yyyy-dd-mm, same as created_at in dataframe
#      labels=c("JAN 2020","JUN 2020","JAN 2021","AUG 2021","JAN 2022","SEP 2022","DEC 2022"),las=1)
# legend("top",legend=c("Trade\nAssociations","Trade\nUnions","Political\nActors"),
#        col=c("blue","green","red"), ncol = 3, lty = 1,bty = "n")
# # legend(0, .08,legend= c("TA", "TU"), lwd = 2, col = c("blue","green"))
# title(paste("Germany topic ",ntp,":",tt))

# estimateeffects



effects_de <- get_effects(estimates = prep_de,
                           variable = 'datenum',
                           type = 'continuous') 

effects_it <- get_effects(estimates = prep_it,
                          variable = 'datenum',
                          type = 'continuous',
                          moderator = NULL) 

effects_int_it <- get_effects(estimates = prep_it,
                           variable = 'datenum',
                           type = 'continuous',
                           moderator = 'actor',
                           modval = "POL") %>%
  bind_rows(
    get_effects(estimates = prep_it,
                variable = 'datenum',
                type = 'continuous',
                moderator = 'actor',
                modval = "TA") %>%
      
      bind_rows(
        get_effects(estimates = prep_it,
                    variable = 'datenum',
                    type = 'continuous',
                    moderator = 'actor',
                    modval = "TU")
  )
  )

effects_int_de <- get_effects(estimates = prep_de,
                              variable = 'datenum',
                              type = 'continuous',
                              moderator = 'actor',
                              modval = "POL") %>%
  bind_rows(
    get_effects(estimates = prep_de,
                variable = 'datenum',
                type = 'continuous',
                moderator = 'actor',
                modval = "TA") %>%
      
      bind_rows(
        get_effects(estimates = prep_de,
                    variable = 'datenum',
                    type = 'continuous',
                    moderator = 'actor',
                    modval = "TU")
      )
  )

ita.labs <- c("Topic 1:", "Topic 2:", "Topic 3:","Topic 4:","Topic 5: Equity employment","Topic 6:",
              "Topic 7: Liquidity","Topic 8:","Topic 9: Emergency Recovery","Topic 10:","Topic 11: EU funding",
              "Topic 12:","Topic 13:","Topic 14:","Topic 15: Social confrontations","Topic 16: Human capital",
              "Topic 17:","Topic 18: Sustainable transitions","Topic 19:","Topic 20:")
names(ita.labs) <- c("1", "2", "3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20")

deu.labs <- c("Topic 1: Companies support", "Topic 2:", "Topic 3: EU funding","Topic 4: Federal support",
              "Topic 5:","Topic 6: Digital education","Topic 7: Tax relief","Topic 8:","Topic 9: Families support",
              "Topic 10:","Topic 11:","Topic 12: Sectors affected","Topic 13:","Topic 14:","Topic 15:",
              "Topic 16:",
              "Topic 17: EU","Topic 18: Sustainable transitions","Topic 19:","Topic 20:")
names(deu.labs) <- c("1", "2", "3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20")


efc <- effects_int_de
tt <- "Germany"
lbs <- ita.labs

# time individual country

for (i in c(1:numm)) {
  

  effects_int_de %>%  filter(topic == i) %>%
 # mutate(moderator = as.factor(moderator)) %>%
  # filter(moderator == "TU") %>%
  ggplot(aes(x = value, y = proportion, color = moderator # ,
             #   group = moderator, fill = moderator
  )) +
  geom_line() +
  # geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2)  +
  scale_x_continuous(breaks = c(18290,18414,18628,18841,18993,19250,19357),
                     labels = c("18290" = "JAN 2020","18414" = "JUN 2020","18628" = "JAN 2021","18841" = "AUG 2021",
                                "18993" = "JAN 2022","19250" = "SEP 2022","19357" = "DEC 2022")) +
  ggtitle(paste(titleplot, "Topic: ",i)) + 
  ylab("Expected Proportion") +
 # geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2)  +
 # facet_wrap(~ topic_f,labeller=labeller(topic_f = lbs), scales = "free") +  # c("6" ="6: Digital Education","9" = "9: Families"))) +
   scale_color_manual(labels = c("Political\nActors","Trade\nAssociations","Trade\nUnions"),
                     values = c("red","green","blue")) +
  theme_light() +
  theme(axis.text.x = element_text(vjust = 0.6,angle=45), axis.title.x = element_blank(),
        strip.background = element_rect(fill="beige"), 
        strip.text = element_text(color = "black",size = 12),
        legend.position = "bottom") 
#plotly::ggplotly(pl)
ggsave(paste0(folder,"results/",i,titleplot,"_time.jpg"),width = 12,height = 7)

}



### interaction
  for (i in c(1:25)) {
    
# effects_int_it$topic_f = factor(effects_int_it$topic, levels=c('5',"10","17","8"))   
    
efc %>% filter(topic == i) %>% # filter(topic_f == c(5,10,17,8)) %>%
 mutate(moderator = as.factor(moderator)) %>%
# filter(moderator == "TU") %>%
  ggplot(aes(x = value, y = proportion, color = moderator # ,
         #   group = moderator, fill = moderator
             )) +
  geom_line() +
 # geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2)  +
  scale_x_continuous(breaks = c(18290,18414,18628,18841,18993,19250,19357),
   labels = c("18290" = "JAN 2020","18414" = "JUN 2020","18628" = "JAN 2021","18841" = "AUG 2021",
              "18993" = "JAN 2022","19250" = "SEP 2022","19357" = "DEC 2022")) +
  ggtitle(paste(tt)) + 
      ylab("Expected Proportion") +
  facet_wrap(~ topic, scales = "free" ) + #, labeller=labeller(topic_f = lbs)) +  # c("6" ="6: Digital Education","9" = "9: Families"))) +
  scale_color_manual(labels = c("Political\nActors","Trade\nAssociations","Trade\nUnions"),
                     values = c("red","green","blue")) +
 theme_light() +
  theme(axis.text.x = element_text(vjust = 0.6,angle=45), axis.title.x = element_blank(),
        strip.background = element_rect(fill="beige"), 
        strip.text = element_text(color = "black",size = 12),
        legend.position = "bottom") 
    ggsave(paste0(folder,"results/",tt,i,"_timeINT.jpg"),width = 12,height = 7)
  }
# + labs(y = 'Topic Proportion',
#  ggsave(paste("pic_DE/DE_",ntp,"_",tt,".jpg"))
#  plotly::ggplotly(pl)

# pointestimate

effects_pe <- get_effects(estimates = prep_de,
                       variable = 'actor',
                       type = 'pointestimate')


effects_pe %>% 
  # filter(topic == 18) %>%
 # mutate(topic = as.factor(topic)) %>%
 mutate(topic = factor(topic,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"))) %>% 
  ggplot(aes(x = topic, y = proportion, color = topic)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, size = 1) +
 # scale_x_continuous(breaks=c(1:20,1)) +
  geom_point(size = 3) +   coord_flip() + 
  facet_wrap(~ value,dir="v", labeller = labeller(value = c("POL" = "Political Actors","TA" = "Trade Associations", "TU" = "Trade Unions" ))) +
 #  geom_text(aes(label= topic, color = "black")) +
 labs(x = 'Topic', y = 'Topic Proportion') +
  theme_light() +
  guides(color= "none")
ggsave(paste0("data_new/","Germany","_pointestimate",numm,".jpg"),width = 12,height = 25)

# BETA BY actor

tidystm <- tidy(stm_de20cn)
tidystm <- rename(tidystm, actor = y.level)
tt <- "Germany"

tidystm$topic_f = factor(tidystm$topic, levels=c('7','3','18','6','9','17')) 

 tidystm %>% group_by(topic, actor) %>% top_n(5,beta)%>%
  filter(topic_f %in% c('7','3','18','6','9','17')) %>% 
  ggplot(aes(x = reorder(term, beta), beta, fill = topic_f)) +
  # ggplot(aes(beta, term, fill = topic)) +
  geom_col() +
  facet_wrap(~  actor, # scales = "free", 
             labeller = labeller(
               actor = c("POL" = "Political Actors","TU" = "Trade Unions","TA" = "Trade Associations"))) +
  coord_flip() +
  ggtitle(tt) +
  # scale_fill_manual(values = c('9' = "blue",'11' = "red",'18' = "purple",
  #                              '5' = "orange",'15' = "darkgreen",'16' = "pink"),
  #                   labels = c('9' = "9: Emergency Recovery",'11' = "11: EU funding",
  #                              '18' = "18: Sustainable transitions",
  #                              '5' = "5: Equity employment",
  #                              '15' = "15: Social confrontations",'16' = "16: Human capital"),
  #                   name = "Topics") +
   scale_fill_manual(values = c('7' = "blue",'3' = "red",'18' = "purple",
                                '6' = "orange",'9' = "darkgreen",'17' = "pink"),
                     labels = c('7' = "7: Tax relief",'3' = "3: EU funding",
                                '18' = "18: Climate protection",
                                '6' = "6: Digital education",
                                '9' = "9: Families support",'17' = "17: EU"),
                     name = "Topics") +
  ylab("Top 5 words actors for topic") + 
  theme_bw() + 
  theme(axis.title.y = element_blank(),plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")
ggsave(paste0("data_new/",tt,"_topbeta.jpg"),width = 10,height = 8)


# tf-idf

library(ggstance)
library(ggthemes)


tff_words <- df_20_22 %>% filter(datenum >= 18414 & datenum <= 18628) %>%
  filter(country == "Italy") %>%
  unnest_tokens(word, text) %>%
  count(actor, word, sort = TRUE) %>%
  ungroup()

tff_words <- tff_words %>%
  bind_tf_idf(word, actor, n) 

plot_tff <- tff_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  mutate(actor = factor(actor, levels = c("POL",
                                            "TA",
                                            "TU")))

plot_tff <- plot_tff %>% group_by(actor) %>% 
  top_n(10, tf_idf) %>% 
  mutate(word = reorder(word, tf_idf))

ggplot(plot_tff, aes(tf_idf, word, fill = actor, alpha = tf_idf)) +
  geom_barh(stat = "identity", show.legend = FALSE) +
  labs(# title = "Highest tf-idf words in Classic Physics Texts",
       y = NULL, x = "tf-idf") +
  facet_wrap(~actor, ncol = 2, scales = "free") +
  theme_tufte(base_family = "Arial", base_size = 13, ticks = FALSE) +
  scale_alpha_continuous(range = c(0.6, 1)) +
  scale_x_continuous(expand=c(0,0)) +
#  scale_fill_viridis(end = 0.6, discrete=TRUE) +
  theme(strip.text=element_text(hjust=0))












# Between countries
require(quanteda)
require(quanteda.textstats)
require(quanteda.textplots)



dfm_en <- tokens(corpus(df_20_22),
                    remove_punct = TRUE, remove_numbers = TRUE,remove_url = TRUE) %>%
  # tokens_wordstem() %>%
  # tokens_compound(phrase(c(compond_dfm))) %>%
  tokens_remove(c("http*","@*","€","+","|","faq","=",stopwords("en"),stopwords_en,
                  rem_dfm)) %>%
  dfm()
tstat_key <- textstat_keyness(dfm_subset(dfm_en,actor == "TU"), 
                              target = dfm_subset(dfm_en,actor == "TU")$country == "Italy")
textplot_keyness(tstat_key)

##

# Combine plots
library(magick)

DE15act <- image_read(paste0(folder,"results/15Germany_time.jpg"))
DE15word <- image_read(paste0(folder,"results/Germany_wordactor15.jpg"))
print(DE15act)
print(DE15word)

grid.arrange(DE15act,DE15word)

img <- c(DE15word,DE15act)
img <- image_scale(img, "300x300")

# Combine 2 plots

tidystm <- tidy(stm_m)
tidystm <- rename(tidystm, actor = y.level)

prep <- estimateEffect(1:numm ~ actor * s(datenum), stm_m, metadata = stm_df$meta, uncertainty = "Global")

effects_int <- get_effects(estimates = prep,
                              variable = 'datenum',
                              type = 'continuous',
                              moderator = 'actor',
                              modval = "POL") %>%
  bind_rows(
    get_effects(estimates = prep,
                variable = 'datenum',
                type = 'continuous',
                moderator = 'actor',
                modval = "TA") %>%
      
      bind_rows(
        get_effects(estimates = prep,
                    variable = 'datenum',
                    type = 'continuous',
                    moderator = 'actor',
                    modval = "TU")
      )
  )



for (i in c(1:numm)) {
  
tm <-  effects_int %>%  filter(topic == i) %>%
    # mutate(moderator = as.factor(moderator)) %>%
    # filter(moderator == "TU") %>%
    ggplot(aes(x = value, y = proportion, color = moderator # ,
               #   group = moderator, fill = moderator
    )) +
    geom_line() +
    # geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2)  +
    scale_x_continuous(breaks = c(18290,18414,18628,18841,18993,19250,19357),
                       labels = c("18290" = "JAN 2020","18414" = "JUN 2020","18628" = "JAN 2021","18841" = "AUG 2021",
                                  "18993" = "JAN 2022","19250" = "SEP 2022","19357" = "DEC 2022")) +
    ggtitle(paste(titleplot, "Topic: ",i)) + 
    ylab("Expected Proportion") +
    # geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2)  +
    # facet_wrap(~ topic_f,labeller=labeller(topic_f = lbs), scales = "free") +  # c("6" ="6: Digital Education","9" = "9: Families"))) +
    scale_color_manual(labels = c("Political\nActors","Trade\nAssociations","Trade\nUnions"),
                       values = c("red","green","blue")) +
    theme_light() +
    theme(axis.text.x = element_text(vjust = 0.6,angle=45), axis.title.x = element_blank(),
          strip.background = element_rect(fill="beige"), 
          strip.text = element_text(color = "black",size = 12),
          legend.position = "bottom") 
  #plotly::ggplotly(pl)
 # ggsave(paste0(folder,"results/",i,titleplot,"_time.jpg"),width = 12,height = 7)
  

wd <-  tidystm %>% filter(topic == i) %>%
    filter(term %nin% de_policies) %>%
    group_by(actor) %>%
    arrange(-beta) %>%
    top_n(7,beta) %>%
    ggplot(aes(reorder(term,beta),beta,fill = actor)) +
    geom_col(show.legend = FALSE) +
  scale_y_continuous(label = scales::percent ) +
  #  scale_y_continuous(breaks = ~ c(min(.x), max(.x))) +
    facet_wrap(~ actor, scales = "free",
               labeller = labeller(actor = c("TU" = "Trade Unions","TA" = "Trade Associations", "POL" = "Political Actors"))) +         
    coord_flip() +
   # ggtitle(paste("Topic: ",i)) +
    xlab("") +
    ylab("Probability words per actor") +
    theme(axis.title.x = element_blank()) +
    theme_bw()
#  ggsave(paste0(folder,"/results/",titleplot,"_wordactor",i,".jpg"),width = 11,height = 5) 
  
cm <- grid.arrange(tm,wd,ncol = 2)
ggsave(cm,file = paste0(folder,"/results/",titleplot,"comb_",i,".jpg"),width = 14, height = 3.5)


} 











