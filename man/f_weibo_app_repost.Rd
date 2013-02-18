\name{f_weibo_app_repost}
\alias{f_weibo_app_repost}
\title{微博转发的初步分析：微博传播路径}
\usage{
  f_weibo_app_repost(hisID='xiaonan', weibo_repost=weibo_repost, topk=5)
}
\arguments{
  \item{hisID}{用于文件命名}
  \item{weibo_repost}{之前抓取到的微博转发数据}
  \item{topk}{关注前topk个节点}
}
\details{
  运行完成后，会在工作路径下生成weibo_repost_hisID_时间.png文件
}
\examples{
load('weibo_saved_repost_xiaonan.RData')
f_weibo_app_repost(hisID='xiaonan', weibo_repost=weibo_repost)
}

