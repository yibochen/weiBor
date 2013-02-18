\name{f_weibo_app_followsns}
\alias{f_weibo_app_followsns}
\title{关注关系的可视化}
\usage{
  f_weibo_app_followsns(hisID='chenyibo', sns_df=sns_df, topk=3)
}
\arguments{
  \item{hisID}{对方的ID，用于文件命名}
  \item{sns_df}{关注关系的数据集}
  \item{topk}{关注前topk个节点}
}
\details{
  运行完成后，会在工作路径下生成weibo_followsns_hisID_时间.png文件
}
\examples{
load('weibo_saved_snsdf_chenyibo.RData')
f_weibo_app_followsns(hisID='chenyibo', sns_df=sns_df, topk=3)
}

