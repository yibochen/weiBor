

f_weibo_followsns <- function(cH=ch0,
                              type='my',
                              hisID='lijian001',
                              myID='chenyibo',
                              ftype=1){
  # type my或者his
  # hisID 对方的ID
  # myID 我的ID，用于过滤
  # ftype 0:全部关注 1:互相关注
  require(RJSONIO)
  require(RCurl)
  require(XML)
  
  if(type == 'my'){
    follow_df <- f_weibo_myfollow(cH=cH, ftype=ftype)
    hisID <- myID
  }
  if(type == 'his'){
    follow_df <- f_weibo_hisfollow(cH=cH, hisID=hisID, myID=myID, ftype=ftype)
  }
  
  # 循环读取数据
  sns_df <- NULL
  nrow_follow_df <- nrow(follow_df)
  for(i in seq_len(nrow_follow_df)){
    hisid_i <- gsub('^/', '', follow_df$following_href[i])
    if(type == 'my'){
      new_df <- f_weibo_hisfollow(cH=cH, hisID=hisid_i, myID=myID, ftype=1)
    }
    if(type == 'his'){
      new_df <- f_weibo_hisfollow(cH=cH, hisID=hisid_i, myID=myID, ftype=ftype)
    }
    if(!is.null(new_df)){
      new_df <- cbind(follow_df$following_nick[i], follow_df$following_id[i], new_df[, 1:2])
      sns_df <- rbind(sns_df, new_df)
    }
    cat('sns 进度 ', i/nrow_follow_df, '\n')
    Sys.sleep(runif(n=1,min=0.001,max=0.01))
  }
  names(sns_df) <- c('nick_0', 'id_0', 'nick_1', 'id_1')
  sns_df$nick_0 <- as.character(sns_df$nick_0)
  sns_df$id_0 <- as.character(sns_df$id_0)
  sns_df <- sns_df[sns_df$id_1 %in% follow_df$following_id, ]
  
  save(sns_df, file=paste('weibo_saved_snsdf_', hisID,'.RData', sep=''))
  return(sns_df)
}


f_weibo_app_followsns <- function(hisID='chenyibo',
                                  sns_df=sns_df,
                                  topk=3){
  require(igraph)
  
  people <- unique(data.frame(id=c(sns_df[, 2], sns_df[, 4]), 
                              name=c(sns_df[, 1], sns_df[, 3])))
  gg <- graph.data.frame(d=sns_df[, c(2, 4)], directed=T, vertices=people)
  is.simple(gg)
  gg2 <- simplify(gg, remove.loops=T, remove.multiple=T)
  is.simple(gg2)
  
  # 子群划分
  com <- walktrap.community(gg2, steps=5)
  subgroup <- split(com$names, com$membership)
  V(gg2)$sg <- com$membership
  
  # 图形的参数，这个需要设计一下  ="=
  V(gg2)$degree <- degree(gg2, mode='in')
  V(gg2)$betweenness <- betweenness(gg2)
  top_d <- quantile(V(gg2)$degree, (length(V(gg2))-topk)/length(V(gg2)))
  top_b <- quantile(V(gg2)$betweenness, (length(V(gg2))-topk)/length(V(gg2)))
  V(gg2)$size <- 2
  V(gg2)$label <- NA
  V(gg2)$labelcex <- 1
  # V(gg2)$framecolor <- 'SkyBlue2'
  # V(gg2)$vertexcolor <- 'SkyBlue2'
  V(gg2)$vertexcolor <- rainbow(max(V(gg2)$sg), alpha=0.75)[V(gg2)$sg]
  V(gg2)$framecolor <- rainbow(max(V(gg2)$sg), alpha=0.75)[V(gg2)$sg]
  V(gg2)[degree>=top_d | betweenness>=top_b]$size <- 6
  V(gg2)[degree>=top_d | betweenness>=top_b]$label <- V(gg2)[degree>=top_d | betweenness>=top_b]$name
  V(gg2)[degree>=top_d]$vertexcolor <- 'red'
  V(gg2)[degree>=top_d]$framecolor <- 'red'
  V(gg2)[betweenness>=top_b]$vertexcolor <- 'green'
  V(gg2)[betweenness>=top_b]$framecolor <- 'green'
  V(gg2)[degree>=top_d & betweenness>=top_b]$vertexcolor <- 'gold'
  V(gg2)[degree>=top_d & betweenness>=top_b]$framecolor <- 'gold'
  
  png(paste('weibo_followsns_', hisID, '_', Sys.Date(), '.png', sep=''),width=1000,height=1000)
  par(mar=c(0,0,0,0))
  set.seed(17)
  plot(gg2,
       layout=layout.fruchterman.reingold,
       vertex.size=V(gg2)$size,
       vertex.label=V(gg2)$label,
       vertex.label.cex=V(gg2)$labelcex,
       vertex.label.color=1,
       vertex.label.dist=0,
       vertex.color=V(gg2)$vertexcolor,
       vertex.frame.color=V(gg2)$framecolor,
       edge.color=grey(0.8),
       edge.arrow.size=0.5,
       edge.arrow.width=0.5
  )
  dev.off()
}

