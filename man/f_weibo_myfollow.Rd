\name{f_weibo_myfollow}
\alias{f_weibo_myfollow}
\title{ץȡ��¼�˺Ź�ע�б�}
\usage{
  f_weibo_myfollow(cH=ch0, ftype=0)
}
\arguments{
  \item{cH}{֮ǰ��¼��õ�CurlHandle}
  \item{ftype}{��ע���͡�ȡ0��Ϊȫ����ע��ȡ1��Ϊ�����ע}
}
\value{
  \item{following_nick}{��ע������ǳ�}
  \item{following_id}{��ע���������ID}
  \item{following_href}{��ע�����ҳ���׺}
  \item{following_type}{��ע���������}
}
\details{
  ���й����л��ӡץȡ������������糩ͨ����������ǰ������
}
\examples{
weibo_myfollow <- f_weibo_myfollow(cH=ch0, ftype=0)
head(weibo_myfollow)
}
