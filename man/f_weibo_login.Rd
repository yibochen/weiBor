\name{f_weibo_login}
\alias{f_weibo_login}
\title{微博登录}
\usage{
  f_weibo_login(name='****', pwd='****', cookie_file=NULL)
}
\arguments{
  \item{name}{用户名，通常是邮箱}
  \item{pwd}{密码}
  \item{cookie_file}{cookie文件，在无法模拟登陆时使用}
}
\value{
  返回CurlHandle，用于后续的页面抓取
}
\details{
  建议注册个马甲用用
}
\examples{
ch0 <- f_weibo_login('myemail', 'mypwd')
ch0 <- f_weibo_login(cookie_file='C:/Users/ASUS/Desktop/cookies.txt')
}

