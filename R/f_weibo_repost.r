

f_weibo_repost_oneshot <- function(cH=ch0,
                                   oneshot_url='http://weibo.com/2043157342/zcdFaeuD1'){
  # 先看看有多少转发
  the1url <- paste(oneshot_url, '?type=repost&page=1', sep='')
  the1get <- getURL(the1url, curl=cH, .encoding='UTF-8')
  
  weibodetail <- '^.*<script>STK && STK.pageletM && STK.pageletM.view\\((\\{\"pid\":\"pl_content_weiboDetail\".+?\\})\\)</script>.*$'
  a1 <- gsub(weibodetail, '\\1', the1get)
  a1 <- fromJSON(a1)[['html']]
  if(grepl('还没有人转发', a1)){
    return(NULL)
  } else{
    a11 <- htmlParse(a1, encoding='UTF-8')
    # 分页
    a112 <- getNodeSet(a11, path='//div[@class="W_pages W_pages_comment"]//a|//div[@class="W_pages_minibtn"]//a')
    if(is.null(a112)){
      page_cnt <- 1
    } else{
      page_cnt <- max(as.numeric(sapply(a112, xmlValue)), na.rm=T)
    }
    
    raw_data <- NULL
    
    for(pg_i in seq_len(page_cnt)){
      theiurl <- paste(oneshot_url, '?type=repost&page=', pg_i, sep='')
      theiget <- getURL(theiurl, curl=cH, .encoding='UTF-8')
      weibodetail <- '^.*<script>STK && STK.pageletM && STK.pageletM.view\\((\\{\"pid\":\"pl_content_weiboDetail\".+?\\})\\)</script>.*$'
      a1 <- gsub(weibodetail, '\\1', theiget)
      a1 <- fromJSON(a1)[['html']]
      raw_data <- c(raw_data, a1)
      cat(pg_i, '\n')
      Sys.sleep(runif(n=1,min=0.001,max=0.01))
    }
    
    a11 <- htmlParse(raw_data, encoding='UTF-8')
    # a111 <- getNodeSet(a11, path='//a[@class="S_link2 WB_time"]')
    # root_time <- as.POSIXlt(sapply(a111, xmlValue)[1])
    a113 <- getNodeSet(a11, path='//div[@node-type="feed_list"]//dl[@class="comment_list S_line1 clearfix WB_feed_type "]//dd//div//a[@action-type="feed_list_forward"]|
                       //div[@node-type="feed_list"]//dl[@class="comment_list W_linecolor clearfix"]//dd//a[@action-type="feed_list_forward"]')
    repost_repost <- iconv(sapply(a113, xmlValue), 'UTF-8', 'UTF-8')
    repost_repost <- as.numeric(gsub('[^0-9]','',repost_repost))
    repost_info <- iconv(sapply(a113, xmlGetAttr, 'action-data'), 'UTF-8', 'UTF-8')
    
    rootmid <- gsub('^.*rootmid=([^&]+)&.*$', '\\1', repost_info)
    rootname <- gsub('^.*rootname=([^&]+)&.*$', '\\1', repost_info)
    rootuid <- gsub('^.*rootuid=([^&]+)&.*$', '\\1', repost_info)
    rooturl <- gsub('^.*rooturl=([^&]+)&.*$', '\\1', repost_info)
    reposturl <- gsub('^.*&url=([^&]+)&.*$', '\\1', repost_info)
    repostmid <- gsub('^.*&mid=([^&]+)&.*$', '\\1', repost_info)
    repostname <- gsub('^.*&name=([^&]+)&.*$', '\\1', repost_info)
    repostuid <- gsub('^.*&uid=([^&]+)&.*$', '\\1', repost_info)
    
    output <- unique(as.data.frame(cbind(rootmid,rootname,rootuid,rooturl,repost_repost,
                                         reposturl,repostmid,repostname,repostuid), stringsAsFactors=F))
    return(output)
  }
}

f_weibo_repost_path <- function(cH=ch0, hisID='xiaonan', 
                                root_url='http://weibo.com/2043157342/zcdFaeuD1'){
  require(RJSONIO)
  require(RCurl)
  require(XML)
  
  root_df <- f_weibo_repost_oneshot(cH=cH, oneshot_url=root_url)
  if(is.null(root_df)){
    return(NULL)
    cat('there are no repost for this weibo!!!', '\n')
  } else{
    repost_df <- NULL
    lost_df <- NULL
    for(repost_url in root_df$reposturl[!is.na(root_df$repost_repost)]){
      idx <- match(repost_url,root_df$reposturl)
      new_df <- f_weibo_repost_oneshot(cH=cH, oneshot_url=repost_url)
      if(!is.null(new_df)){
        if(nrow(new_df) > 0){
          new_df$rootmid <- root_df$repostmid[idx]
          new_df$rootname <- root_df$repostname[idx]
          new_df$rootuid <- root_df$repostuid[idx]
          new_df$rooturl <- root_df$reposturl[idx]
          repost_df <- rbind(repost_df, new_df)
        }
      }
      cnt_delta <- ifelse(!is.null(new_df), nrow(new_df), 0) - as.numeric(root_df$repost_repost[idx])
      cat(cnt_delta, idx, ' have to be positive, perfect to be zero. \n')
      lost_df <- rbind(lost_df, data.frame(cnt_delta, idx))
      cat('进度', idx/nrow(root_df), '\n')
    }
    root_df2 <- root_df[!root_df$reposturl %in% repost_df$reposturl, ]
    result_df <- rbind(root_df2, repost_df)
    weibo_repost <- list(result_df=result_df, lost_df=lost_df)
    save(weibo_repost, file=paste('weibo_saved_repost_', hisID,'.RData', sep=''))
    return(weibo_repost)
  }
}

