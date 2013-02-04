

# 微博关键词及时间分布
f_weibo_app_content <- function(hisID='chenyibo', 
                                scale_a=4, scale_b=1, 
                                cutday='2012-04-01', 
                                equal_length=F, 
								sogou_file='I:\\0_yibo\\2_bigbigdata\\2_apply_3_network_analysis\\1_weibo\\1_content\\SogouLabDic.dic', 
                                mydic=NULL, 
                                cnt_words=100){
  load(paste('weibo_saved_content_', hisID, '.RData', sep=''))
  pkgs <- installed.packages()[, 1]
  if(!'rJava' %in% pkgs){
    install.packages('rJava', 
                     repos='http://mirrors.ustc.edu.cn/CRAN/')
  }
  if(!'Rwordseg' %in% pkgs){
    install.packages('Rwordseg', 
                     repos='http://R-Forge.R-project.org')
  }
  if(!'wordcloud' %in% pkgs){
    install.packages('wordcloud', 
                     repos='http://mirrors.ustc.edu.cn/CRAN/')
  }
  
  require(Rwordseg)
  require(wordcloud)
  weibo_data <- weibo_content$weibo_data[order(
    -as.numeric(as.POSIXlt(weibo_content$weibo_data$weibo_time))), ]
  weibo_data_all <- sapply(strsplit(weibo_data$weibo_content, '//'), '[', 1)
  weibo_data_all <- gsub('@[^@]+ ', ' ', weibo_data_all)
  flag <- min(which(as.POSIXlt(weibo_data$weibo_time) <= as.POSIXlt(cutday)))
  weibo_data_1 <- weibo_data_all[flag:nrow(weibo_data)]
  weibo_data_2 <- weibo_data_all[(flag-1):1]
  if(equal_length){
    weibo_data_1 <- weibo_data_1[1:min(length(weibo_data_2),length(weibo_data_1))]
    weibo_data_2 <- weibo_data_2[1:min(length(weibo_data_2),length(weibo_data_1))]
  }
  
  if(!is.null(mydic)){
    installDict(mydic)
  }
  # 分词
  f_fenci <- function(input=weibo_data){
    weibo_data <- input[input != '' & !is.na(input)]
    words <- unlist(segmentCN(weibo_data))
    words <- words[!words  %in% c('转发','回复')]
    
    # 统计词频
    words_freq <- sort(table(words), dec=T)
    words_names <- names(words_freq)
    words_length <- nchar(words_names)
    words_df <- data.frame(words_names=words_names, words_freq=words_freq, words_length=words_length)
    
    # 加载搜狗实验室的词频文件
    SogouLabDic <- readLines(sogou_file)
    SogouLabDic <- paste(iconv(SogouLabDic, 'GBK', 'UTF-8'), ' ')
    SogouLabDic <- data.frame(do.call(rbind, strsplit(SogouLabDic, '\t')), stringsAsFactors=F)
    names(SogouLabDic)[1] <- 'words_names'
    SogouLabDic_match <- SogouLabDic[SogouLabDic[,1] %in% words_df$words_names, ]
    SogouLabDic_match$X2 <- as.numeric(SogouLabDic_match$X2)
    
    words_df2 <- merge (words_df, SogouLabDic_match, by='words_names', all.x=T)
    
    # 匹配不到的扔掉
    words_df3 <- words_df2[!is.na(words_df2$X2), ]
    words_df3 <- words_df3[grep('^[NV],',words_df3$X3), ]
    words_df3$words_freq2 <- words_df3$words_freq * log(max(words_df3$X2)/words_df3$X2)
    
    return(words_df3)
  }
  
  words_df3 <- f_fenci(weibo_data_all)
  words_df1 <- f_fenci(weibo_data_1)
  words_df2 <- f_fenci(weibo_data_2)
  words_1 <- as.character(words_df1[order(-words_df1$words_freq2)[1:9], 1])
  words_2 <- as.character(words_df2[order(-words_df2$words_freq2)[1:9], 1])
  
  png(paste('weibo_content_', hisID, '_', Sys.Date(), '.png', sep=''),width=1000,height=1000)
  par(mfrow=c(2,2), mar=c(1,1,3,1), yaxt='n', 
      cex.main=2.5, cex.axis=2)
  weibo_time <- strptime(substr(weibo_content$weibo_data$weibo_time, 12, 16), format='%H:%M')
  hist(weibo_time, breaks='hours', 
       main=paste('TA主要在神马时间发微博？', sep=''), 
       col='darkgray', border='white', xlab=NULL, ylab=NULL)
  weibo_time <- strptime(weibo_content$weibo_data$weibo_time, format='%Y-%m-%d %H:%M')
  hist(weibo_time, breaks='months', 
       main=paste('TA的微博是不是越发越勤快？', sep=''), 
       col='darkgray', border='white', xlab=NULL, ylab=NULL)
  cnt_words <- min(nrow(words_df3), cnt_words)
  words_df4 <- words_df3[order(-words_df3$words_freq2), c('words_names','words_freq2')]
  words_df4 <- words_df4[seq_len(cnt_words), ]
  clusters <- kmeans(words_df4$words_freq2, 10)
  words_df4$words_freq3 <- as.numeric(as.factor(clusters$centers[clusters$cluster, 1]))
  wordcloud(words=words_df4$words_names, freq=words_df4$words_freq3, scale=c(scale_a, scale_b), 
            max.words=cnt_words, min.freq=0, random.order=F, 
            colors=rev(grey((seq_len(cnt_words)-1)*0.6/(cnt_words-1))), 
            rot.per=0, font=2)
  plot(0, 0, type='n', xlim=c(0,100), ylim=c(0,100), axes=F)
  text(0, 50, paste(weibo_content$nick, '\n', 
                    '在', cutday, '之前的\n', 
                    length(weibo_data_1), '条微博的关键词是：\n', 
                    paste(c(words_1[1:3], ''), collapse=';'), '\n', 
                    paste(c(words_1[4:6], ''), collapse=';'), '\n', 
                    paste(c(words_1[7:9]), collapse=';'), '\n\n', 
                    '在', cutday, '之后的\n', 
                    length(weibo_data_2), '条微博的关键词是：\n', 
                    paste(c(words_2[1:3], ''), collapse=';'), '\n', 
                    paste(c(words_2[4:6], ''), collapse=';'), '\n', 
                    paste(c(words_2[7:9]), collapse=';'), 
                    sep=''), 
       cex=3, font=2, adj=0, col=grey(0.01))
  dev.off()
}

