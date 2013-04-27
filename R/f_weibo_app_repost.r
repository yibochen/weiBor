

# 微博传播
f_weibo_app_repost <- function(hisID='xiaonan', weibo_repost=weibo_repost, 
                               topk=5){
  require(igraph)
  
  repost <- weibo_repost$result_df[, c('rootmid','rootname','repostmid','repostname')]
  people <- unique(data.frame(id=c(repost$rootmid,repost$repostmid), name=c(repost$rootname,repost$repostname)))
  gg <- graph.data.frame(d=repost[c('rootmid','repostmid')], directed=T, vertices=people)
  is.simple(gg)
  gg2 <- simplify(gg, remove.loops=T, remove.multiple=F)
  is.simple(gg2)
  
  # 图形的参数，这个需要设计一下  ="=
  V(gg2)$degree <- degree(gg2, mode='out')
  V(gg2)$betweenness <- betweenness(gg2)
  top_d <- quantile(V(gg2)$degree, (length(V(gg2))-topk)/length(V(gg2)))
  top_b <- quantile(V(gg2)$betweenness, (length(V(gg2))-topk)/length(V(gg2)))
  V(gg2)$size <- 1
  V(gg2)$label <- NA
  V(gg2)$labelcex <- 1
  V(gg2)$framecolor <- 'SkyBlue2'
  V(gg2)$vertexcolor <- 'SkyBlue2'
  V(gg2)[degree>=top_d | betweenness>=top_b]$framecolor <- 'gold'
  V(gg2)[degree>=top_d | betweenness>=top_b]$vertexcolor <- 'gold'
  V(gg2)[1]$framecolor <- 'red'
  V(gg2)[1]$vertexcolor <- 'red'
  V(gg2)[degree>=top_d | betweenness>=top_b]$size <- 5
  V(gg2)[1]$size <- 7
  V(gg2)[degree>=top_d | betweenness>=top_b]$label <- V(gg2)[degree>=top_d | betweenness>=top_b]$name
  
  png(paste('weibo_repost_', hisID, '_', Sys.Date(), '.png', sep=''),width=600,height=600)
  par(mar=c(0,0,0,0))
  set.seed(14)
  plot(gg2,
       layout=layout.auto,
       vertex.size=V(gg2)$size,
       vertex.label=V(gg2)$label,
       vertex.label.cex=V(gg2)$labelcex,
       vertex.label.color=1,
       vertex.label.dist=0.5,
       vertex.color=V(gg2)$vertexcolor,
       vertex.frame.color=V(gg2)$framecolor,
       edge.color='darkgrey',
       edge.arrow.size=0.5,
       edge.arrow.width=1
  )
  dev.off()
}

