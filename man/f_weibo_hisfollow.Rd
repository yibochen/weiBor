\name{f_weibo_hisfollow}
\alias{f_weibo_hisfollow}
\title{抓取某账号的关注列表}
\usage{
  f_weibo_hisfollow(cH=ch0, hisID='lijian001', myID='chenyibo', ftype=0)
}
\arguments{
  \item{cH}{之前登录获得的CurlHandle}
  \item{hisID}{对方的ID}
  \item{myID}{我的ID，用于过滤}
  \item{ftype}{关注类型。取0则为全部关注，取1则为共同关注}
}
\value{
  \item{following_nick}{关注对象的昵称}
  \item{following_id}{关注对象的数字ID}
  \item{following_href}{关注对象的页面后缀}
  \item{following_type}{关注对象的类型}
}
\details{
  运行过程中会打印抓取情况。保持网络畅通，否则容易前功尽弃
}
\examples{
weibo_hisfollow <- f_weibo_hisfollow(cH=ch0, hisID='lijian001', myID='chenyibo', ftype=0)
head(weibo_hisfollow)
}

