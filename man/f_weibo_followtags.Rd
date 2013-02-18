\name{f_weibo_followtags}
\alias{f_weibo_followtags}
\title{抓取关注对象标签}
\usage{
  f_weibo_followtags(cH=ch0, type='my', hisID='lijian001', myID='chenyibo', ftype=0)
}
\arguments{
  \item{cH}{之前登录获得的CurlHandle}
  \item{type}{取'my'则抓取对象为登录账号，取'his'则抓取对象非登录账号}
  \item{hisID}{对方的ID，当type为'my'时可以不指定}
  \item{myID}{我的ID，用于过滤及文件命名，必须指定}
  \item{ftype}{关注类型。取0则为全部关注，取1则为互相关注或共同关注（视type取值而定）}
}
\value{
  \item{following_tags}{标签向量}
}
\details{
  运行完成后，会在工作路径下生成weibo_saved_followtags_hisID.RData文件，用于后续分析，其中数据集命名为following_tags
}
\examples{
weibo_followtags <- f_weibo_followtags(cH=ch0, type='my', hisID='lijian001', myID='chenyibo', ftype=0)
head(weibo_followtags)
}

