

f_weibo_followtags <- function(cH=ch0,
                               type='my',
                               hisID='lijian001',
                               myID='chenyibo',
                               ftype=0){
  # type my或者his
  # hisID 对方的ID
  # myID 我的ID，用于过滤
  # ftype 0:全部关注 1:共同关注
  # ftype 0:全部关注 1:互相关注
  require(RJSONIO)
  require(RCurl)
  require(XML)
  
  if(type == 'my'){
    follow_df <- f_weibo_myfollow(cH=cH, ftype=ftype)
    hisID <- myID
  }
  if(type == 'his'){
    follow_df <- f_weibo_hisfollow(cH=cH, hisID=hisID, myID=myID, ftype=0)
  }
  # 循环读取数据
  raw_data <- NULL
  nrow_follow_df <- nrow(follow_df)
  for(i in seq_len(nrow_follow_df)){
    href_i <- follow_df$following_href[i]
    theiurl <- paste('http://weibo.com', href_i, sep='')
    theiget <- getURL(theiurl, curl=cH, .encoding='UTF-8')
    hisinfo <- '^.*<script>STK && STK.pageletM && STK.pageletM.view\\((\\{\"pid\":\"pl_profile_hisInfo\".+?\\})\\)</script>.*$'
    histags <- '^.*<script>STK && STK.pageletM && STK.pageletM.view\\((\\{\"pid\":\"pl_content_tags\".+?\\})\\)</script>.*$'
    if(grepl(hisinfo, theiget)){
      a1 <- gsub(hisinfo, '\\1', theiget)
      a1 <- fromJSON(a1)[['html']]
      if(grepl('标签', a1)){
        raw_data <- c(raw_data, a1)
      }
    }
    if(grepl(histags, theiget)){
      a1 <- gsub(histags, '\\1', theiget)
      a1 <- fromJSON(a1)[['html']]
      if(grepl('标签', a1)){
        raw_data <- c(raw_data, a1)
      }
    }
    cat('tags 进度 ', i/nrow_follow_df, '\n')
    Sys.sleep(runif(n=1,min=0.001,max=0.01))
  }
  a11 <- htmlParse(raw_data, encoding='UTF-8')
  a111 <- getNodeSet(a11, path='//a//span[@class="S_func1"]|//span//a[@target="_blank"]')
  following_tags <- iconv(sapply(a111, xmlValue), 'UTF-8', 'UTF-8')
  
  save(following_tags, file=paste('weibo_saved_followtags_', hisID,'.RData', sep=''))
  return(following_tags)
}


f_weibo_app_followtags <- function(tags=following_tags,
                                   hisID='chenyibo',
                                   cnt_words=100,scale_a=6,scale_b=1){
  require(wordcloud)
  
  words_df <- table(tags)
  words_df2 <- data.frame(tag=names(words_df), cnt=as.numeric(words_df))
  words_df3 <- words_df2[order(-words_df2$cnt, words_df2$tag), ]
  cnt_words <- min(nrow(words_df3), cnt_words)
  words_df4 <- words_df3[seq_len(cnt_words), ]
  # clusters <- kmeans(words_df4$cnt, 20)
  # words_df4$cnt <- as.numeric(as.factor(clusters$centers[clusters$cluster, 1]))
  
  png(paste('weibo_follow_tags_', hisID, '_', Sys.Date(), '.png', sep=''),width=600,height=600)
  set.seed(17)
  wordcloud(words=words_df4$tag, freq=words_df4$cnt, scale=c(scale_a, scale_b), 
            max.words=cnt_words, min.freq=0, random.order=T, random.color=T, 
            # colors=rev(grey((seq_len(cnt_words)-1)*0.6/(cnt_words-1))), 
            colors=terrain.colors(cnt_words, alpha=0.75), 
            rot.per=0, font=2)
  dev.off()
}

