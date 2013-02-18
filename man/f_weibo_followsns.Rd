\name{f_weibo_followsns}
\alias{f_weibo_followsns}
\title{抓取关注对象之间关注关系}
\usage{
  f_weibo_followsns(cH=ch0, type='my', hisID='lijian001', myID='chenyibo', ftype=1)
}
\arguments{
  \item{cH}{之前登录获得的CurlHandle}
  \item{type}{取'my'则抓取对象为登录账号，取'his'则抓取对象非登录账号}
  \item{hisID}{对方的ID，当type为'my'时可以不指定}
  \item{myID}{我的ID，用于过滤及文件命名，必须指定}
  \item{ftype}{关注类型。取0则为全部关注，取1则为互相关注或共同关注（视type取值而定）}
}
\value{
  \item{sns_df}{数据框，字段分别为：昵称0、ID0、昵称1、ID1，表示有向的关注关系}
}
\details{
  运行完成后，会在工作路径下生成weibo_saved_snsdf_hisID.RData文件，用于后续分析，其中数据集命名为sns_df
}
\examples{
weibo_followsns <- f_weibo_followsns(cH=ch0, type='my', hisID='lijian001', myID='chenyibo', ftype=1)
head(weibo_followsns)
}

