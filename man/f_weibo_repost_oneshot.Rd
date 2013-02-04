\name{f_weibo_repost_oneshot}
\alias{f_weibo_repost_oneshot}
\title{抓取单条微博转发记录}
\usage{
  f_weibo_repost_oneshot(cH=ch0, oneshot_url='http://weibo.com/2043157342/zcdFaeuD1')
}
\arguments{
  \item{cH}{之前登录获得的CurlHandle}
  \item{oneshot_url}{目标微博原po的链接}
}
\value{
  \item{rootmid}{原po的mid}
  \item{rootname}{原po主的昵称}
  \item{rootuid}{原po主的uid}
  \item{rooturl}{原po的链接}
  \item{repost_repost}{该条转发带来的转发数（页面显示值）}
  \item{reposturl}{该条转发的链接}
  \item{repostmid}{该条转发的mid}
  \item{repostname}{该条转发的昵称}
  \item{repostuid}{该条转发的uid}
}
\details{
  运行过程中会打印抓取情况。保持网络畅通，否则容易前功尽弃
}
\examples{
weibo_repost_oneshot <- f_weibo_repost_oneshot(cH=ch0, oneshot_url='http://weibo.com/2043157342/zcdFaeuD1')
head(weibo_repost_oneshot)
}

