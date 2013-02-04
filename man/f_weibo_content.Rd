\name{f_weibo_content}
\alias{f_weibo_content}
\title{抓取微博内容}
\usage{
  f_weibo_content(cH=ch0, N=200, hisID='chenyibo', is_e=F)
}
\arguments{
  \item{cH}{之前登录获得的CurlHandle}
  \item{N}{想要获取的微博条数，概数}
  \item{hisID}{对方的ID}
  \item{is_e}{是否企业版微博}
}
\value{
  \item{hisID}{对方的ID}
  \item{nick}{对方的昵称}
  \item{weibo_data}{微博内容，数据框，两个字段（微博文本内容及微博发布时间）}
}
\details{
  运行完成后，会在工作路径下生成weibo_saved_content_hisID.RData文件，用于后续分析，其中数据集命名为weibo_content
}
\examples{
weibo_content1 <- f_weibo_content(cH=ch0, N=100, hisnick='chenyibo', is_e=F)
head(weibo_content1$weibo_data)

weibo_content2 <- f_weibo_content(cH=ch0, N=100, hisnick='eeocomcn', is_e=T)
head(weibo_content2$weibo_data)
}

