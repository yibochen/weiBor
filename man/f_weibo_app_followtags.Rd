\name{f_weibo_app_followtags}
\alias{f_weibo_app_followtags}
\title{文本向量词云}
\usage{
  f_weibo_app_followtags(tags=following_tags, hisID='chenyibo',
                         cnt_words=100, scale_a=6, scale_b=1)
}
\arguments{
  \item{tags}{目标变量，文本向量}
  \item{hisID}{对方的ID，用于文件命名}
  \item{cnt_words}{词云中展示的关键词个数的上限}
  \item{scale_a,scale_b}{词云的参数，调节字体大小}
}
\details{
  运行完成后，会在工作路径下生成weibo_follow_tags_hisID_时间.png文件
}
\examples{
load('weibo_saved_followtags_chenyibo.RData')
f_weibo_app_followtags(tags=following_tags, hisID='chenyibo', cnt_words=100, scale_a=6, scale_b=1)
}

