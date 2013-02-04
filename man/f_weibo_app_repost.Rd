\name{f_weibo_app_repost}
\alias{f_weibo_app_repost}
\title{微博转发的初步分析：微博传播路径}
\usage{
  f_weibo_app_repost(hisID='xiaonan', topk=5)
}
\arguments{
  \item{hisID}{对方的ID，与之前保存的RData文件中的hisID一致}
  \item{topk}{关注前topk个节点}
}
\details{
  运行完成后，会在工作路径下生成weibo_repost_hisID_时间.png文件
}
\examples{
f_weibo_app_repost('xiaonan')
}

