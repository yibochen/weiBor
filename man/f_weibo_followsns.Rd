\name{f_weibo_followsns}
\alias{f_weibo_followsns}
\title{ץȡ��ע����֮���ע��ϵ}
\usage{
  f_weibo_followsns(cH=ch0, type='my', hisID='lijian001', myID='chenyibo', ftype=1)
}
\arguments{
  \item{cH}{֮ǰ��¼��õ�CurlHandle}
  \item{type}{ȡ'my'��ץȡ����Ϊ��¼�˺ţ�ȡ'his'��ץȡ����ǵ�¼�˺�}
  \item{hisID}{�Է���ID����typeΪ'my'ʱ���Բ�ָ��}
  \item{myID}{�ҵ�ID�����ڹ��˼��ļ�����������ָ��}
  \item{ftype}{��ע���͡�ȡ0��Ϊȫ����ע��ȡ1��Ϊ�����ע��ͬ��ע����typeȡֵ������}
}
\value{
  \item{sns_df}{���ݿ��ֶηֱ�Ϊ���ǳ�0��ID0���ǳ�1��ID1����ʾ����Ĺ�ע��ϵ}
}
\details{
  ������ɺ󣬻��ڹ���·��������weibo_saved_snsdf_hisID.RData�ļ������ں����������������ݼ�����Ϊsns_df
}
\examples{
weibo_followsns <- f_weibo_followsns(cH=ch0, type='my', hisID='lijian001', myID='chenyibo', ftype=1)
head(weibo_followsns)
}
