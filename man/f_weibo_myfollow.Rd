\name{f_weibo_myfollow}
\alias{f_weibo_myfollow}
\title{抓取登录账号关注列表}
\usage{
  f_weibo_myfollow(cH=ch0, ftype=0)
}
\arguments{
  \item{cH}{之前登录获得的CurlHandle}
  \item{ftype}{关注类型。取0则为全部关注，取1则为互相关注}
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
weibo_myfollow <- f_weibo_myfollow(cH=ch0, ftype=0)
head(weibo_myfollow)
}

