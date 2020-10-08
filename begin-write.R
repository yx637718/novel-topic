install.packages("rvest")
devtools::install_github("nstrayer/datadrivencv")
setwd()
# run ?datadrivencv::use_datadriven_cv to see more details
datadrivencv::use_datadriven_cv(
  full_name = "XY",
  data_location = "https://docs.google.com/spreadsheets/d/1MBHEekmtanrcC7p7xVkBahd6qppRkO1Rc75WN3wlA90/edit?usp=sharing",
  pdf_location = "https://github.com/nstrayer/cv/raw/master/strayer_cv.pdf",
  html_location = "nickstrayer.me/cv/",
  source_location = "https://github.com/nstrayer/cv"
)


library(rvest)
library(dplyr)
url0 <- "C:\\Users\\Sibyl\\Desktop\\为开头写一个故事 - 搜索结果 - 知乎.html"
web <- read_html(url0, encoding = "UTF-8")
id <- web %>% html_nodes('div.List-item h2 meta') %>% html_attr("itemprop")
content <- web %>% html_nodes('div.List-item h2 meta') %>% html_attr("content")
id.url <- grepl("url",id)
id.content <- grepl("name",id)
df <- cbind.data.frame(url = content[id.url], question = content[id.content]) 

# outtext <- paste0("[",df$question,"]","(",df$url,")")


# library(tm)
library(quanteda)
# devtools::install_github("quanteda/quanteda.corpora")

# docs <- Corpus(VectorSource(text))
# docs <- tm_map(docs, stripWhitespace)
# toSpace <- content_transformer(function (x , pattern ) gsub(pattern, "", x))
# docs <- tm_map(docs, toSpace, "？|?|?|,|，")
inspect(docs)
# docs <- tm_map(docs, toSpace, "为开头写个故事|为开头写一个故事|开头写一个|写一个故事|故事|一个|开头|写|结尾")
# docs <- tm_map(docs, toSpace, "我是|试|我的|xxx|一篇|里")

# inspect(docs)

# 【|】|「|」|［|］
# docs <- tm_map(docs, removeWords, c("写一个故事", "为开头写个故事","为开头写一个故事")) 

# docs <- corpus(docs)



# corp <- quanteda.corpora(docs)
# corp <- quanteda.corpora::download(url = "https://www.dropbox.com/s/37ojd5knz1qeyul/data_corpus_chinesegovreport.rds?dl=1")

# Chinese stopwords
ch_stop <- stopwords("zh", source = "misc")

# tokenize
ch_toks <- docs %>% 
  tokens(remove_punct = T) %>%
  tokens_remove(pattern = ch_stop)

# construct a dfm
ch_dfm <- dfm(ch_toks)
tf <- topfeatures(ch_dfm, n = 100)
tf2 <- data.frame(word=names(tf), freq=tf)

# plot a word cloud
set.seed(100)

# to set the font correctly for macOS
textplot_wordcloud(ch_dfm, min_count = 2, random_order = FALSE,
                   rotation = .25, max_words = 100,
                   min_size = 0.5, max_size = 2.8,
                   font =  "YouYuan",
                   color = RColorBrewer::brewer.pal(8, "Dark2"))
tstat_col <- textstat_collocations(ch_toks, size = 2, min_count = 1, tolower = TRUE)
head(tstat_col, 10)

tstat_col <- textstat_collocations(ch_toks, size = 3, min_count = 1, tolower = TRUE)
head(tstat_col, 10)


##################
https://quanteda.io/articles/pkgdown/quickstart_cn.html#--2

# df结构见上篇文章
text <- df[,2]
head(text)

# step-1: corpus()：创建语料库格式的object
docs <- corpus(text)
head(docs)
# step-2: 获取中文停用词
ch_stop <- stopwords("zh", source = "misc")
head(ch_stop)

# step-3: 去除停用词后进行分词
ch_toks <- docs %>% 
  tokens(remove_punct = T) %>%
  tokens_remove(pattern = ch_stop)

head(ch_toks)

# step-4: 创建dfm（文档特征矩阵）
ch_dfm <- dfm(ch_toks)
head(ch_dfm)
topfeatures(ch_dfm)

# step-5: 创建fcm（特征共现矩阵）
fcmat_china1 <- fcm(ch_toks, context = "window")
head(fcmat_china1)

topfeatures(fcmat_china1["皇后", ])

tstat_col <- textstat_collocations(ch_toks, size = 2, min_count = 20, tolower = TRUE)
head(tstat_col, 20)

# step-6: clean "text" object
text.clean <- gsub("写|一个|一篇|故事|开头|试|里|请|我|x|结尾|你|的", "", text)
docs <- corpus(text.clean)

# step-7: repeat step-3 & step-4 
# tokenize
ch_toks <- docs %>% 
  tokens(remove_punct = T) %>%
  tokens_remove(pattern = ch_stop) 
ch_dfm <- dfm(ch_toks,stem = TRUE)
topfeatures(ch_dfm)

# step-8: get wordcloud
#get font
extrafont::font_import() 
extrafont::loadfonts(device = "win") 
png(file="plot_cloud.png",res=300, width = 1000, height = 1000, units = "px")
textplot_wordcloud(ch_dfm, min_count = 1, random_order = F,
                   rotation = .25, max_words = 100,
                   min_size = 2, max_size = 2.8,
                   font =  "YouYuan",
                   color = RColorBrewer::brewer.pal(8, "Paired"))
dev.off()

# 聚类
dfmat_sotu <- dfm_trim(ch_dfm, min_termfreq = 1, min_docfreq = 1)
?dfm_trim
#分层聚类 -  在归一化dfm上计算距离
tstat_dist <- textstat_dist(dfm_weight(dfmat_sotu, scheme = "prop"))

# 聚类分析文本距离
a <- as.dist(tstat_dist)
a <- a[!is.na(a)]
pres_cluster <- hclust(a)
?hclust
# 按文档名标注
pres_cluster$labels <- docnames(dfmat_sotu)
# 绘制树状图
plot(pres_cluster, xlab = "", sub = "",
     main = "Euclidean Distance on Normalized Token Frequency")


set.seed(100)
if (require(topicmodels)) {
  my_lda_fit20 <- LDA(convert(dfmat_sotu, to = "topicmodels"), k = 20)
  get_terms(my_lda_fit20, 7)
}
