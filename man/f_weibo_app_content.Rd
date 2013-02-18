\name{f_weibo_app_content}
\alias{f_weibo_app_content}
\title{微博内容的初步分析：微博关键词及时间分布}
\usage{
  f_weibo_app_content(hisID='chenyibo', weibo_content=weibo_content, scale_a=4, scale_b=1, 
                      cutday='2012-04-01', equal_length=F, 
					  sogou_file='I:\\0_yibo\\2_bigbigdata\\2_apply_3_network_analysis\\1_weibo\\1_微博正文', 
                      mydic=NULL, cnt_words=100)
}
\arguments{
  \item{hisID}{用于文件命名}
  \item{weibo_content}{之前抓取到的微博数据}
  \item{scale_a,scale_b}{词云的参数，调节字体大小}
  \item{cutday}{日期分隔点，用于比较该日期前后的关键词变化}
  \item{equal_length}{是否要求上述分隔点前后的微博数目一致。若取值为T，则会按照最少记录数进行截取。默认为F}
  \item{sogou_file}{搜狗词库文件的路径}
  \item{mydic}{自定义词典路径}
  \item{cnt_words}{词云中展示的关键词个数的上限}
}
\details{
  运行完成后，会在工作路径下生成weibo_content_hisID_时间.png文件
}
\examples{
load('weibo_saved_content_eeocomcn.RData')
f_weibo_app_content(hisID='eeocomcn', weibo_content, cutday='2013-01-01')
load('weibo_saved_content_chenyibo.RData')
f_weibo_app_content(hisID='chenyibo', weibo_content, cutday='2012-04-01')
}

