\name{f_weibo_login}
\alias{f_weibo_login}
\title{微博登录}
\usage{
  f_weibo_login(name='****', pwd='****')
}
\arguments{
  \item{name}{用户名，通常是邮箱}
  \item{pwd}{密码}
}
\value{
  返回CurlHandle，用于后续的页面抓取
}
\details{
  建议注册个马甲用用
}
\examples{
ch0 <- f_weibo_login('myemail', 'mypwd')
}

