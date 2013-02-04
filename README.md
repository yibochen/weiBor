weiBor
======
demo

require(weiBor)

ch0 <- f_weibo_login('myemail', 'mypwd')

weibo_content1 <- f_weibo_content(cH=ch0, N=100, hisID='chenyibo', is_e=F)
weibo_content2 <- f_weibo_content(cH=ch0, N=100, hisID='eeocomcn', is_e=T)
head(weibo_content1$weibo_data)
head(weibo_content2$weibo_data)
f_weibo_app_content('chenyibo', cutday='2013-01-20')
# 需要用到搜狗词库：http://www.sogou.com/labs/dl/w.html

weibo_repost <- f_weibo_repost_path(cH=ch0, hisID='xiaonan', root_url='http://weibo.com/2043157342/zcdFaeuD1')
head(weibo_repost$result_df)
f_weibo_app_repost('xiaonan')

