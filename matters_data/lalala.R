setwd("~/Documents/Github/dmp_hk/matters_data")
Sys.setlocale(category = 'LC_ALL','en_US.UTF-8')

library(quanteda)
library(stm)
library(readr)
library(igraph)
library(showtext)
library(sysfonts)
library(wordcloud)
library(stringr)


# EDA
data <- read.csv("comprehensive.csv", encoding = 'utf-8')
data$seg_words <- as.character(data$seg_words)
corpus <- corpus(data$seg_words, 
                 docvars=data)
doc.features <- dfm(corpus, 
                    remove=stopwords(language = "zh", source = "misc"), 
                    stem=T, remove_punct=T, what="fastestword")
topfeatures(doc.features, 20)


#process text data
out <- convert(doc.features, to = "stm", docvars = data)
summary(out$meta$MAT)
out$meta$date <- as.Date(out$meta$createdAt, format="%Y-%m-%d")
hist(out$meta$date, breaks="month")


out$meta$date <- as.numeric(out$meta$date)
out$meta$MAT <- as.numeric(out$meta$MAT)

stm.out = stm(out$documents, out$vocab, K=30, 
              prevalence = ~ MAT + s(date), 
              data=out$meta, init.type="Spectral")

save(stm.out, file="STM_matters.RData")


# plot topics
load("STM_matters.RData")
labelTopics(stm.out)


font_add("Heiti TC Medium", regular='/System/Library/Fonts/STHeiti Medium.ttc')
plot.STM(stm.out, n=10, family = 'Heiti TC Medium')


thoughts9 <- findThoughts(stm.out, out$meta$seg_words, topics=9, n=10)$docs[[1]]
thoughts3 <- findThoughts(stm.out, out$meta$seg_words, topics=3, n=10)$docs[[1]]
thoughts10 <- findThoughts(stm.out, out$meta$seg_words, topics=10, n=10)$docs[[1]]
findThoughts(stm.out, out$meta$seg_words, topics=18, n=10)

par(mfrow = c(1, 2), mar = c(1, 1, 2, 1))
par(family = 'Heiti TC Medium')
plotQuote(thoughts3, width = 40, main = "Topic 3")
#plotQuote(thoughts9, width = 30, main = "Topic 9")
plotQuote(thoughts10, width = 40, main = "Topic 10")



# estimate effect
#out$meta$date <- as.numeric(out$meta$date)
##out$meta$MAT <- as.factor(out$meta$MAT)
prep <- estimateEffect(c(1:30) ~ MAT + s(date), stm.out, metadata = out$meta)
plot.estimateEffect(prep, covariate = "MAT", xlim = c(-.1, .1),
                    method="difference", cov.value1=1, cov.value2=0)
findThoughts(stm.out, out$meta$seg_words, topics=18, n=10)
findThoughts(stm.out, out$meta$seg_words, topics=20, n=10)
findThoughts(stm.out, out$meta$seg_words, topics=3, n=10)
findThoughts(stm.out, out$meta$seg_words, topics=16, n=10)


plot(prep, covariate = "date", method="continuous", model = z, topic=3, xaxt = "n", xlab = "Time")
monthseq <- seq(from=as.Date("2017-12-01"), to=as.Date("2019-10-01"), by = "3 month")
monthnames <- months(monthseq)
l <- c(17500, 17600, 17700, 17800, 17900, 18000, 18100, 18200)
axis(1, at=l, labels=monthnames)


plot(prep, covariate = "date", method="continuous", model = z, topic=10, xaxt = "n", xlab = "Time")
monthseq <- seq(from=as.Date("2017-12-01"), to=as.Date("2019-10-01"), by = "3 month")
monthnames <- months(monthseq)
l <- c(17500, 17600, 17700, 17800, 17900, 18000, 18100, 18200)
axis(1, at=l, labels=monthnames)


plot.estimateEffect(prep, covariate = 'date',
                    method= "continuous", topics=23)

labelTopics(stm.out, c(5,8))
mod.out.corr <- topicCorr(stm.out)
plot(mod.out.corr)


# semantic coherence
dataSelect <- selectModel(out$documents, out$vocab, K = 10,
                          prevalence =~ MAT + s(date), max.em.its= 75,
                          data=out$meta, runs = 10, seed = 1234)

plotModels(dataSelect, pch=c(1,2,3,4), legend.position="bottomright")
selectedmodel <- dataSelect$runout[[1]]


# kmeans
set.seed(01234)
kmeans.results.20 <- kmeans(doc.features, 
                            centers = 20, nstart = 10) 
head(kmeans.results.20$cluster)
table(kmeans.results.20$cluster)
head(data$seg_words[kmeans.results.20$cluster==1])
head(data$seg_words[kmeans.results.20$cluster==2])
head(data$seg_words[kmeans.results.20$cluster==3])
kmeans.results.20$withinss

# wordcloud
par(family=('Heiti TC Light'))
cluster11 <- colSums(doc.features[kmeans.results.20$cluster==11,])
wordcloud(names(cluster11), cluster11, max.words=50)

cluster2 <- colSums(doc.features[kmeans.results.20$cluster==2,])
wordcloud(names(cluster2), cluster2, max.words=50)


#Hierarchical clustering (agglomerative in R)
# get distances on normalised dfm
doc.features <- dfm_trim(doc.features, min_termfreq = 4)
DistMat <- dist(as.matrix(dfm_weight(doc.features, scheme="prop")))
# hiarchical clustering the distance object
Cluster <- hclust(DistMat)
# label with document names
Cluster$labels <- docnames(doc.features)
#label with first part of string
Cluster$labels <- sapply(data$seg_words, 
                         function (x){
                           str_c(str_split(x, " ")[1:4], 
                                 collapse=" ")})
# plot as a dendrogram
plot(Cluster)

#You can also do this for words
WordDistMat <- dist(t(as.matrix(doc.features[1:10])))
# hiarchical clustering the distance object
Cluster <- hclust(WordDistMat)
# label with document names
Cluster$labels <- colnames(doc.features[1:10])
#label with first part of string
plot(Cluster)
