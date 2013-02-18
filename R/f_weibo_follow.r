

f_weibo_myfollow <- function(cH=ch0,
                             ftype=0){
  # ftype 0:全部关注 1:互相关注
  require(RJSONIO)
  require(RCurl)
  require(XML)
  
  the0url <- 'http://weibo.com/'
  the0get <- getURL(the0url, curl=cH, .encoding='UTF-8')
  oid <- gsub('^.*\\[\'oid\'\\] = \'([^\']+)\';.*$', '\\1', the0get)
  
  # 先看一下有多少页
  page_cnt <- 1
  the1url <- paste('http://weibo.com/', oid, '/myfollow?page=', 1, '&ftype=', ftype, sep='')
  the1get <- getURL(the1url, curl=cH, .encoding='UTF-8')
  idx <- gregexpr('page=[0-9]+', the1get)[[1]]
  page_num <- substring(the1get, idx, idx-1+attr(idx,"match.length"))
  page_num <- max(as.numeric(gsub('page=([0-9]+)', '\\1', page_num)), na.rm=T)
  if(page_num > 0 & !is.na(page_num)){
    page_cnt <- page_num
  }
  
  # 循环读取数据
  raw_data <- NULL
  for(pg_i in seq_len(page_cnt)){
    theiurl <- paste('http://weibo.com/', oid, '/myfollow?page=', pg_i, '&ftype=', ftype, sep='')
    theiget <- getURL(theiurl, curl=cH, .encoding='UTF-8')
    myfollow <- '^.*<script>STK && STK.pageletM && STK.pageletM.view\\((\\{\"pid\":\"pl_relation_myfollow\".+?\\})\\)</script>.*$'
    a1 <- gsub(myfollow, '\\1', theiget)
    a1 <- fromJSON(a1)[['html']]
    raw_data <- c(raw_data, a1)
    cat('follow 进度 ', pg_i/page_cnt, '\n')
    Sys.sleep(runif(n=1,min=0.001,max=0.01))
  }
  
  a11 <- htmlParse(raw_data, encoding='UTF-8')
  a111 <- getNodeSet(a11, path='//a[@class="S_func1"]')
  # following_nick <- iconv(sapply(a111, xmlValue), 'UTF-8', 'UTF-8')
  following_nick <- iconv(sapply(a111, xmlGetAttr, 'title'), 'UTF-8', 'UTF-8')
  following_id <- gsub('id=(.+)', '\\1', iconv(sapply(a111, xmlGetAttr, 'usercard'), 'UTF-8', 'UTF-8'))
  following_href <- iconv(sapply(a111, xmlGetAttr, 'href'), 'UTF-8', 'UTF-8')
  a112 <- getNodeSet(a11, path='//span[@class="connlink"]')
  following_type <- iconv(sapply(a112, xmlValue), 'UTF-8', 'UTF-8')
  
  output <- unique(as.data.frame(cbind(following_nick,following_id,following_href,following_type), stringsAsFactors=F))
  return(output)
}


f_weibo_hisfollow <- function(cH=ch0,
                              hisID='lijian001',
                              myID='chenyibo',
                              ftype=0){
  # hisID 对方的ID
  # myID 我的ID，用于过滤
  # ftype 0:全部关注 1:共同关注
  require(RJSONIO)
  require(RCurl)
  require(XML)
  
  the0url <- paste('http://weibo.com/', hisID, sep='')
  the0get <- getURL(the0url, curl=cH, .encoding='UTF-8')
  oid <- gsub('^.*\\[\'oid\'\\] = \'([^\']+)\';.*$', '\\1', the0get)
  
  # 先看一下有多少页
  page_cnt <- 1
  the1url <- paste('http://weibo.com/', oid, '/follow?page=', 1, '&tag=', ftype, sep='')
  the1get <- getURL(the1url, curl=cH, .encoding='UTF-8')
  if(grepl('还没有共同关注的人', the1get)){
    cat('还没有共同关注的人!!!!!!把ftype设成0试试看吧')
    return(NULL)
  }
  idx <- gregexpr('page=[0-9]+', the1get)[[1]]
  page_num <- substring(the1get, idx, idx-1+attr(idx,"match.length"))
  page_num <- max(as.numeric(gsub('page=([0-9]+)', '\\1', page_num)), na.rm=T)
  if(page_num > 0 & !is.na(page_num)){
    page_cnt <- page_num
  }
  
  # 循环读取数据
  raw_data <- NULL
  for(pg_i in seq_len(page_cnt)){
    theiurl <- paste('http://weibo.com/', oid, '/follow?page=', pg_i, '&tag=', ftype, sep='')
    theiget <- getURL(theiurl, curl=cH, .encoding='UTF-8')
    hisfollow <- '^.*<script>STK && STK.pageletM && STK.pageletM.view\\((\\{\"pid\":\"pl_relation_hisFollow\".+?\\})\\)</script>.*$'
    a1 <- gsub(hisfollow, '\\1', theiget)
    a1 <- fromJSON(a1)[['html']]
    raw_data <- c(raw_data, a1)
    cat('follow 进度 ', pg_i/page_cnt, '\n')
    Sys.sleep(runif(n=1,min=0.001,max=0.01))
  }
  
  a11 <- htmlParse(raw_data, encoding='UTF-8')
  a111 <- getNodeSet(a11, path='//a[@class="W_f14 S_func1"]')
  following_nick <- iconv(sapply(a111, xmlValue), 'UTF-8', 'UTF-8')
  following_id <- gsub('id=(.+)', '\\1', iconv(sapply(a111, xmlGetAttr, 'usercard'), 'UTF-8', 'UTF-8'))
  following_href <- iconv(sapply(a111, xmlGetAttr, 'href'), 'UTF-8', 'UTF-8')
  delete <- which(following_href == paste('/', myID, sep=''))
  if(length(delete) > 0){
    following_nick <- following_nick[-delete]
    following_id <- following_id[-delete]
    following_href <- following_href[-delete]
  }
  a112 <- getNodeSet(a11, path='//div[@class="addbtn mbspace"]')
  following_type <- iconv(sapply(a112, xmlValue), 'UTF-8', 'UTF-8')
  
  output <- unique(as.data.frame(cbind(following_nick,following_id,following_href,following_type), stringsAsFactors=F))
  return(output)
}

