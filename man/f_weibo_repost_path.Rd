\name{f_weibo_repost_path}
\alias{f_weibo_repost_path}
\title{抓取微博转发记录}
\usage{
  f_weibo_repost_path(cH=ch0, hisID='xiaonan', root_url='http://weibo.com/2043157342/zcdFaeuD1')
}
\arguments{
  \item{cH}{之前登录获得的CurlHandle}
  \item{hisID}{自定义对方的昵称，用于文件命名}
  \item{root_url}{目标微博原po的链接}
}
\value{
  \item{result_df}{数据框，结构与f_weibo_repost_oneshot得到的结果一致}
  \item{lost_df}{数据框，记录实际抓取结果与页面显示结果不一致的情况，暂时没用}
}
\details{
  运行完成后，会在工作路径下生成weibo_saved_repost_hisID.RData文件，用于后续分析，其中数据集命名为weibo_repost
}
\examples{
weibo_repost <- f_weibo_repost_path(cH=ch0, hisID='xiaonan', root_url='http://weibo.com/2043157342/zcdFaeuD1')
head(weibo_repost$result_df)
}

