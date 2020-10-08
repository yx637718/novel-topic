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
url0 <- "C:\\Users\\Sibyl\\Desktop\\Ϊ��ͷдһ������ - ������� - ֪��.html"
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
# docs <- tm_map(docs, toSpace, "��|?|?|,|��")
inspect(docs)
# docs <- tm_map(docs, toSpace, "Ϊ��ͷд������|Ϊ��ͷдһ������|��ͷдһ��|дһ������|����|һ��|��ͷ|д|��β")
# docs <- tm_map(docs, toSpace, "����|��|�ҵ�|xxx|һƪ|��")

# inspect(docs)

# ��|��|��|��|��|��
# docs <- tm_map(docs, removeWords, c("дһ������", "Ϊ��ͷд������","Ϊ��ͷдһ������")) 

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

# df�ṹ����ƪ����
text <- df[,2]
head(text)

# step-1: corpus()���������Ͽ��ʽ��object
docs <- corpus(text)
head(docs)
# step-2: ��ȡ����ͣ�ô�
ch_stop <- stopwords("zh", source = "misc")
head(ch_stop)

# step-3: ȥ��ͣ�ôʺ���зִ�
ch_toks <- docs %>% 
  tokens(remove_punct = T) %>%
  tokens_remove(pattern = ch_stop)

head(ch_toks)

# step-4: ����dfm���ĵ���������
ch_dfm <- dfm(ch_toks)
head(ch_dfm)
topfeatures(ch_dfm)

# step-5: ����fcm���������־���
fcmat_china1 <- fcm(ch_toks, context = "window")
head(fcmat_china1)

topfeatures(fcmat_china1["�ʺ�", ])

tstat_col <- textstat_collocations(ch_toks, size = 2, min_count = 20, tolower = TRUE)
head(tstat_col, 20)

# step-6: clean "text" object
text.clean <- gsub("д|һ��|һƪ|����|��ͷ|��|��|��|��|x|��β|��|��", "", text)
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

# ����
dfmat_sotu <- dfm_trim(ch_dfm, min_termfreq = 1, min_docfreq = 1)
?dfm_trim
#�ֲ���� -  �ڹ�һ��dfm�ϼ������
tstat_dist <- textstat_dist(dfm_weight(dfmat_sotu, scheme = "prop"))

# ��������ı�����
a <- as.dist(tstat_dist)
a <- a[!is.na(a)]
pres_cluster <- hclust(a)
?hclust
# ���ĵ�����ע
pres_cluster$labels <- docnames(dfmat_sotu)
# ������״ͼ
plot(pres_cluster, xlab = "", sub = "",
     main = "Euclidean Distance on Normalized Token Frequency")


set.seed(100)
if (require(topicmodels)) {
  my_lda_fit20 <- LDA(convert(dfmat_sotu, to = "topicmodels"), k = 20)
  get_terms(my_lda_fit20, 7)
}