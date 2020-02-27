library(tm)
library(ggplot2)
library(RColorBrewer)
library(wordcloud)
library(SentimentAnalysis)
library(tidyr)
library(dplyr)
library(devtools)
library(radarchart)
library(memery)
library(magik)
library(stringr)        
library(tidytext)
library(dplyr)
library(reshape2)
library(igraph)
library(ggraph)
library(wordcloud2)
library(harrypotter)


Sys.setlocale("LC_ALL","English")
path=list.files("C:/Users/F1/Desktop/R/Harry Potter/txt")
name<-c(path[6], path[1], path[7], path[3], path[5], path[4], path[2])

harry<-list(philosophers_stone, chamber_of_secrets, prisoner_of_azkaban,
         goblet_of_fire, order_of_the_phoenix,
         half_blood_prince, deathly_hallows)

str(harry)
names(harry)<-name
str(harry)
a<-paste(harry," ")
texta<- VCorpus(VectorSource(a))
str(texta)
texta <- tm_map(texta, removeWords, stopwords("english"))
texta <- tm_map(texta,removePunctuation)
texta <- tm_map(texta, removeNumbers)
texta <- tm_map(texta, tolower)   
texta <- tm_map(texta, removeWords, stopwords("english"))
texta <- tm_map(texta, stripWhitespace)
texta <- tm_map(texta, PlainTextDocument)
dtma <- DocumentTermMatrix(texta)
dtma$dimnames$Terms
dtma$dimnames$Docs
str(dtma)
for (i in 1:length(dtma$dimnames$Docs)){
  dtma$dimnames$Docs[i]<-name[i]
}
freqa<-colSums(as.matrix(dtma))
freqa
head(table(freqa),500)
findFreqTerms(dtma,lowfreq = 1000)
texta <- tm_map(texta, removeWords, my_stop)
dtma <- DocumentTermMatrix(texta)
for (i in 1:length(dtma$dimnames$Docs)){
  dtma$dimnames$Docs[i]<-name[i]
}
freqa<-colSums(as.matrix(dtma))
findFreqTerms(dtma,lowfreq = 1000)
findFreqTerms(dtma,lowfreq = 700, highfreq = 1000)
texta <- tm_map(texta, removeWords, my_stop_2)
dtma <- DocumentTermMatrix(texta)
for (i in 1:length(dtma$dimnames$Docs)){
  dtma$dimnames$Docs[i]<-name[i]
}
freqa<-colSums(as.matrix(dtma))
findFreqTerms(dtma,lowfreq = 700)
findFreqTerms(dtma,lowfreq = 500, highfreq = 700)
texta <- tm_map(texta, removeWords, my_stop_3)
dtma <- DocumentTermMatrix(texta)
for (i in 1:length(dtma$dimnames$Docs)){
  dtma$dimnames$Docs[i]<-name[i]
}
freqa<-colSums(as.matrix(dtma))
findFreqTerms(dtma,lowfreq = 500)
findFreqTerms(dtma,lowfreq = 300, highfreq = 500)
texta <- tm_map(texta, removeWords, my_stop_4)
dtma <- DocumentTermMatrix(texta)
for (i in 1:length(dtma$dimnames$Docs)){
  dtma$dimnames$Docs[i]<-name[i]
}
freqa<-colSums(as.matrix(dtma))
findFreqTerms(dtma,lowfreq = 400)
findFreqTerms(dtma,lowfreq = 200, highfreq = 400)
texta <- tm_map(texta, removeWords, my_stop_5)
dtma <- DocumentTermMatrix(texta)
for (i in 1:length(dtma$dimnames$Docs)){
  dtma$dimnames$Docs[i]<-name[i]
}
freqa<-colSums(as.matrix(dtma))
findFreqTerms(dtma,lowfreq = 200)
findFreqTerms(dtma,lowfreq = 150, highfreq = 200)
texta <- tm_map(texta, removeWords, my_stop_6)
dtma <- DocumentTermMatrix(texta)
for (i in 1:length(dtma$dimnames$Docs)){
  dtma$dimnames$Docs[i]<-name[i]
}
freqa<-colSums(as.matrix(dtma))
findFreqTerms(dtma,lowfreq = 150)
findFreqTerms(dtma,lowfreq = 100, highfreq = 150)


wfb<-data.frame(word=names(freqa), freq=freqa)

wfb_1<-wfb%>%filter(freq>=150)
wfb_1<-wfb_1%>%mutate_if(is.factor, as.character)
str(wfb_1)

wfb_1<-wfb%>%filter(freq>=150)
wfb_1<-wfb_1%>%mutate_if(is.factor, as.character)
str(wfb_1)



test<-wfb%>%filter((freq>=200)&(freq<2500))
test
wordcloud2(test, size=.5, shape="cardioid", 
           color=rep_len( c("yellow","orange"), nrow(wfb_1)),
           backgroundColor="black")

wfb_2<-wfb%>%filter(freq>=500)
wfb_2<-wfb_2%>%mutate_if(is.factor, as.character)
str(wfb_2)


wfb_3<-wfb%>%filter((freq>=150)&(freq<500))
wfb_3<-wfb_3%>%mutate_if(is.factor, as.character)
str(wfb_3)

for (i in 10:100){
wordcloud2(wfb_3, size=i/10, figPath = "triangle.png", 
           color="orange",
           backgroundColor="black")
}

wordcloud2(wfb_2, size=1, shape="circle", 
           color=rep_len( c("yellow","orange"), nrow(wfb_1)),
           backgroundColor="black",
           fontFamily="Harry Potter")

letterCloud(wfb_2, wordSize=.5, word = "H",
            color="orange", backgroundColor="black")


wfb<-data.frame(word=names(freqps), freq=freqps)
wfb<-wfb%>%filter((freq>=50))
wfb<-wfb%>%mutate_if(is.factor, as.character)
str(wfb)

p <- ggplot(subset(wfb, freq>=70), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity", colour='black',
           fill=c("orange")) +
    labs(x = "words", y = "quantity") +
  ggtitle("Harry Potter and the sorcerer stone") + 
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank())+
  coord_flip()
p


#NRC analysis

b<-wfb_p1 %>%
  inner_join(get_sentiments("nrc"))

wfb_p1<-data.frame(word=names(freqa), freq=freqa)
wfb_p1<-wfb_p1%>%mutate_if(is.factor, as.character)


nrc_plot <- b %>%
  filter((word!='harry')&(word!='laughing'))%>%
  group_by(sentiment) %>%
  summarise(word_count = n()) %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, word_count)) %>%
  ggplot(aes(sentiment, word_count, fill = -word_count)) +
  geom_col() +
  guides(fill = FALSE)+
  labs(x = NULL, y = "Word Count") +
  ggtitle("Harry Potter NRC Sentiment") +
  theme_dark()+
  coord_flip()

nrc_plot

img <- "harry_castle.jpg"
lab <- ""
meme(img, lab, "meme_nrc.jpg", inset = nrc_plot ,col="black")
nrc_meme <- image_read("meme_nrc.jpg")
plot(nrc_meme)

  
str(b)
b<-b%>%arrange(desc(freq))
str(b)
head(b)

k <- b %>%
    filter((word!='harry')&(word!='laughing')&(word!='happily'))%>% 
    group_by(sentiment) %>%
    arrange(sentiment, desc(freq))%>%
    slice(seq_len(25)) %>%
    ungroup()

kl<-b %>%
    filter((word!='harry')&(word!='laughing')&(word!='happily'))%>% 
    group_by(sentiment) %>%
    arrange(sentiment, desc(freq))%>%
    slice(seq_len(10)) %>%
    ungroup()

l<-kl %>%
    distinct(word,sentiment) %>%
    ggplot(aes(x = word, fill = sentiment)) +
    facet_grid(~sentiment) +
    geom_bar()+
    theme(panel.grid.major.x = element_blank(),
          axis.text.y = element_text(colour="white"),
          axis.text.x = element_blank(),
          plot.background = element_rect(fill="black"))+
    xlab(NULL) + ylab(NULL) +
    coord_flip()
  
l
  k1<- k %>%
    ggplot(aes(word, 1, label = word, fill = sentiment )) +
    geom_point(color = "transparent") +
    geom_label_repel(force = 1,nudge_y = .5,  
                     direction = "y",
                     box.padding = 0.05,
                     segment.color = "transparent",
                     size = 4) +
    facet_grid(~sentiment) +
    theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
          axis.title.x = element_text(size = 8),
          panel.grid = element_blank(), panel.background =element_rect(colour="lightblue", fill="lightblue"),
          panel.border = element_rect("lightblue", fill = NA),
          strip.text.x = element_text(size = 30),
          legend.box.background=element_rect(colour ="lightblue",fill = "lightblue"),
          plot.background = element_rect(fill="black")) +
    xlab(NULL) + ylab(NULL) +
    ggtitle("Harry Potter NRC sentiment") +
    coord_flip()
k1

  k_1 <- b %>%
    filter((word!='harry')&(word!='laughing')&(word!='happily')) %>%
    filter((sentiment=='positive')|(sentiment=='negative'))%>%
    group_by(sentiment) %>%
    arrange(sentiment, desc(freq))%>%
    slice(seq_len(25)) %>%
    ungroup()
  
str(k_1)  
  
  k_2<-k_1 %>%
    ggplot(aes(word, 1, label = word, fill = sentiment )) +
    geom_point(color = "transparent") +
    geom_label_repel(force = 1,nudge_y = .5,  
                     direction = "y",
                     box.padding = 0.5,
                     segment.color = "transparent",
                     size = 3) +
    facet_grid(~sentiment) +
    theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
          axis.title.x = element_text(size = 8),
          panel.grid = element_blank(),
          legend.background=element_rect(colour="lightblue"),
          panel.background =element_rect(colour="lightblue", fill="lightblue"),
          panel.border = element_rect(size=.1,"lightblue", fill = NA),
          strip.text.x = element_text(size = 10),
          legend.box.background=element_rect(colour ="lightblue",fill = "lightblue"),
          plot.background = element_rect(fill="black")) +
    xlab(NULL) + ylab(NULL) +
    ggtitle("Harry Potter BING sentiment") +
    coord_flip()
  
  k_2
  
# Bing analysis
  
  wfb_p1<-data.frame(word=names(freqa), freq=freqa)
  wfb_p1<-wfb_p1%>%filter((freq>=100))
  wfb_p1<-wfb_p1%>%mutate_if(is.factor, as.character)
  
  str(wfb_p1)
  
  
  par(bg="black")
  wfb_p1 %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE) %>%
    acast(word ~ sentiment, value.var = "n", fill = 0) %>%
    comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                     scale=c(1.5,.5),
                     max.words = 100)
  str(a)
  wfb_p1<-data.frame(word=names(freqa), freq=freqa)
  str(wfb_p1)
  
  img2 <- "harry_castle.jpg"
  lab2 <- ""
  meme(img2, lab2, "meme_bing.jpg", inset = a)
  y <- image_read("meme_bing.jpg")
  plot(y)
  
  

  bing <- wfb_p1 %>%
  inner_join(get_sentiments("bing"))
  
  wfb_p1<-data.frame(word=names(freqa), freq=freqa)
  wfb_p1<-wfb_p1%>%filter((freq>=100))
  wfb_p1<-wfb_p1%>%mutate_if(is.factor, as.character)  
  
  
  View(bing)
  bing_plot <- bing %>%
    group_by(sentiment) %>%
    summarise(word_count = n()) %>%
    ungroup() %>%
    mutate(sentiment = reorder(sentiment, word_count)) %>%
    ggplot(aes(sentiment, word_count, fill = sentiment)) +
    geom_col() +
    guides(fill = FALSE) +
    labs(x = NULL, y = "Word Count") +
    scale_y_continuous(limits = c(0, 3000)) +
    ggtitle("Harry Potter Bing Sentiment") +
    coord_flip()
  
  bing_plot
  
  img1 <- "harry_castle.jpg"
  lab1 <- ""
  meme(img1, lab1, "meme_bing.jpg", inset = bing_plot)
  x <- image_read("meme_bing.jpg")
  plot(x)
  
 
  #bigrams

path=list.files("C:/Users/F1/Desktop/R/Harry Potter/txt")
name<-c(path[6], path[1], path[7], path[3], path[5], path[4], path[2])
harry<-list(philosophers_stone, chamber_of_secrets, prisoner_of_azkaban,
            goblet_of_fire, order_of_the_phoenix,
            half_blood_prince, deathly_hallows)
  
all_together<-paste(philosophers_stone, chamber_of_secrets, prisoner_of_azkaban,
                    goblet_of_fire, order_of_the_phoenix,
                    half_blood_prince, deathly_hallows)
 
s<-data.frame(text=all_together)
s<-s%>%unnest_tokens(bigram, text, token = "ngrams", n = 2)
bigrams_separated <- s %>%
    separate(bigram, c("word1", "word2"), sep = " ")
  bigrams_filtered <- bigrams_separated %>%
    filter(!word1 %in% my_stop_6) %>%
    filter(!word1 %in% stopwords("english")) %>%
    filter(!word2 %in% my_stop_6)%>%
    filter(!word2 %in% stopwords("english"))%>%
    filter(word2!=word1)
  bigrams_united <- bigrams_filtered %>%
    unite(bigram, word1, word2, sep = " ")
  n_bigrams<-bigrams_united %>% 
    count(bigram, sort = TRUE)

  
  head(n_bigrams, 20)
  pl<- ggplot(subset(n_bigrams, n>=100), aes(x = reorder(bigram, -n),
                                                y = n, 
                                            fill=-n)) +
    geom_bar(stat = "identity") + 
    labs(x = NULL, y = NULL)+
    ggtitle("Harry Potter bigram analysis")+
    theme(axis.text=element_text(size = 10, colour="white",
                                 face='bold'),
          legend.background=element_rect(colour="lightblue"),
          panel.background =element_rect(colour="lightblue", fill="lightblue"),
          panel.border = element_rect(size=.1,"lightblue", fill = NA),
          strip.text.x = element_text(size = 20),
          legend.box.background=element_rect(colour ="lightblue",fill = "lightblue"),
          plot.background = element_rect(fill="black")) +
    coord_flip()
pl

wordcloud2(n_bigrams[2:100,], size=.8, shape="circle", 
           color="random-light",
           backgroundColor="black")


bigram_func<-function(txt){
  f_s<-data.frame(text=txt)
  f_s<-s%>%unnest_tokens(bigram, text, token = "ngrams", n = 2)
  bigrams_separated <- f_s %>%
    separate(bigram, c("word1", "word2"), sep = " ")
  bigrams_filtered <- bigrams_separated %>%
    filter(!word1 %in% my_stop_6) %>%
    filter(!word1 %in% stopwords("english")) %>%
    filter(!word2 %in% my_stop_6)%>%
    filter(!word2 %in% stopwords("english"))%>%
    filter(word2!=word1)
  f_bigrams_united <- f_bigrams_filtered %>%
    unite(bigram, word1, word2, sep = " ")
  f_n_bigrams<-bigrams_united %>% 
    count(bigram, sort = TRUE)
  return (f_n_bigrams)
  }

plot_bigrams<-function(n_bigrams,k){
  pl<- ggplot(subset(n_bigrams, n>=k), aes(x = reorder(bigram, -n),
                                      y = n, 
                                      fill=-n)) +
    geom_bar(stat = "identity") + 
    labs(x = NULL, y = NULL)+
    ggtitle("Harry Potter bigram analysis")+
    theme(axis.text=element_text(size = 20, colour="white",
                                 face='bold'),
          legend.background=element_rect(colour="lightblue"),
          panel.background =element_rect(colour="lightblue", fill="lightblue"),
          panel.border = element_rect(size=.1,"lightblue", fill = NA),
          strip.text.x = element_text(size = 20),
          legend.box.background=element_rect(colour ="lightblue",fill = "lightblue"),
          plot.background = element_rect(fill="black")) +
    coord_flip() 
  return (pl)
}

pl1<-plot_bigrams(bigram_func(harry[[1]]),10)
pl1
pl1<-plot_bigrams(bigram_func(harry[[2]]),10)
pl1
pl1<-plot_bigrams(bigram_func(harry[[3]]),15)
pl1
pl1<-plot_bigrams(bigram_func(harry[[4]]),20)
pl1
pl1<-plot_bigrams(bigram_func(harry[[5]]),20)
pl1
pl1<-plot_bigrams(bigram_func(harry[[6]]),20)
pl1
pl1<-plot_bigrams(bigram_func(harry[[7]]),15)
pl1


a<-bigram_func(harry[[1]])%>%mutate(names=name[1])
b<-bigram_func(harry[[2]])%>%mutate(names=name[2])
c<-bigram_func(harry[[3]])%>%mutate(names=name[3])
d<-bigram_func(harry[[4]])%>%mutate(names=name[4])
e<-bigram_func(harry[[5]])%>%mutate(names=name[5])
f<-bigram_func(harry[[6]])%>%mutate(names=name[6])
g<-bigram_func(harry[[7]])%>%mutate(names=name[7])

all<-rbind(a,b,c,d,e,f,g)


bigram_tf_idf <- all %>%
  count(names, bigram) %>%
  bind_tf_idf(bigram, names, n) %>%
  arrange(desc(tf_idf))%>%
  top_n(20) %>%
  ggplot(aes(bigram, tf_idf, fill = names)) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  coord_flip()
bigram_tf_idf


library(tidyverse)
library(tidytext)

har<-harry%>%
  set_names(name) %>%
  map_df(as_tibble, .id = "book") %>%
  mutate(book = factor(book, levels = harry)) %>%
  filter(!is.na(value)) %>%
  group_by(book) %>%
  mutate(chapter = row_number(book)) %>%
  unnest_tokens(word, value)


har %>% 
  inner_join(get_sentiments("afinn")) %>%
  group_by(book, chapter) %>%
  summarize(score = sum(score)) %>%
  ggplot(aes(chapter, score, fill = book)) +
  geom_col() +
  facet_wrap(~ book, scales = "free_x") +
  labs(title = "Emotional arc of Harry Potter books",
       subtitle = "AFINN sentiment dictionary",
       x = "Chapter",
       y = "Emotional score") +
  theme(legend.position = "none",
        plot.background = element_rect(fill="black"))

