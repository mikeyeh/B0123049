Facebook粉絲團分析朱立倫
================

此程式為計算朱立倫於2016/01/01至2016/04/10的PO文數計算以及讚數、留言數、分享數的資料分析統計

讀取朱立倫粉絲團資料
--------------------

為朱立倫自2016/01/01到做作業當天的PO文數，每隔5天計算一次PO文數，最後再計算出總PO文數。

``` r
if (!require('Rfacebook')){
    install.packages("Rfacebook")
    library(Rfacebook)
}
```

``` r
token<-'EAACEdEose0cBAFNlnBcrEaj2wj5FbxlJ07Fa6vKFDVwSl815EJwiLXycADPtD5nO1hU9nhbN0myyW1AZBJ4RyZCVbOMdXWIZCoIhklYAJjZAZC82mUSSrqbRW10S5Uaz2qqspliIjaZC8o2wBZB3x2iHsYmIACjp9PBxHYcIy5bDwZDZD'

FBData = GET(paste0("https://graph.facebook.com/v2.5/llchu?fields=posts{meesages,likes.summary(true)}&acess_token=",token))

totalPage<-NULL
lastDate<-Sys.Date()
DateVectorStr<-as.character(seq(as.Date("2016-01-01"),lastDate,by="5 days"))
for(i in 1:(length(DateVectorStr)-1)){
    tempPage<-getPage("cherngs.y", token,
                      since = DateVectorStr[i],until = DateVectorStr[i+1])
    totalPage<-rbind(totalPage,tempPage)
}
```

    ## 2 posts 1 posts 5 posts 4 posts 7 posts 1 posts 3 posts 4 posts 3 posts 2 posts 3 posts 1 posts 4 posts 3 posts 1 posts 1 posts 5 posts 1 posts 9 posts 2 posts

``` r
nrow(totalPage)
```

    ## [1] 62

2016/01/01至2016/04/10共有62篇文章數!

每日發文分析
------------

為朱立倫在2016/01/01至2016/04/10之中，每天的PO文數分析，並將PO文數最多的天數做排行。

``` r
totalPage$datetime <- as.POSIXct(totalPage$created_time, format = "%Y-%m-%dT%H:%M:%S+0000", tz = "GMT")


totalPage$dateTPE <- format(totalPage$datetime, "%Y-%m-%d", tz = "Asia/Taipei") 

totalPage$weekdays <-weekdays(as.Date(totalPage$dateTPE))

postnumber <- totalPage$id

PostCount<-aggregate(postnumber~dateTPE,totalPage,length)


library(knitr)

kable(head(PostCount[order(PostCount$postnumber,decreasing = T),]))
```

|     | dateTPE    |  postnumber|
|-----|:-----------|-----------:|
| 39  | 2016-04-01 |           5|
| 11  | 2016-01-23 |           3|
| 40  | 2016-04-03 |           3|
| 1   | 2016-01-01 |           2|
| 5   | 2016-01-14 |           2|
| 6   | 2016-01-16 |           2|

每日讚數
--------

因每日的文章數不一樣多，先選出當日最高讚數的文章，再把每個當日最高讚數的文章下去做排行。

``` r
tablelikes <- NULL

for(i in unique(totalPage$dateTPE)){
  
  selectTPE<-subset(totalPage,dateTPE==i)
  
  tablelikes <- rbind(tablelikes,selectTPE[order(selectTPE$likes_count , decreasing=T)[1],c("dateTPE","type","likes_count")])
  
}

finaloutput <- tablelikes[order(tablelikes$likes_count,decreasing = T),c("dateTPE","type","likes_count")]

kable(head(finaloutput,digits = 0))
```

|     | dateTPE    | type  |  likes\_count|
|-----|:-----------|:------|-------------:|
| 14  | 2016-01-24 | photo |         90859|
| 12  | 2016-01-16 | photo |         61992|
| 50  | 2016-03-21 | photo |         54909|
| 43  | 2016-03-07 | photo |         50350|
| 24  | 2016-02-10 | photo |         48752|
| 41  | 2016-03-09 | photo |         47668|

每日留言數
----------

因每日的文章數不一樣多，先選出當日最高留言數的文章，再把每個當日最高留言數的文章下去做排行。

``` r
tablecomments <- NULL

for(i in unique(totalPage$dateTPE)){
  
  selectTPE<-subset(totalPage,dateTPE==i)
  
  tablecomments <- rbind(tablecomments,selectTPE[order(selectTPE$comments_count,decreasing=T),c("dateTPE","type","comments_count")])
  
}

finaloutputcomments <- tablecomments[order(tablecomments$comments_count,decreasing = T),c("dateTPE","type","comments_count")]

kable(head(finaloutputcomments,digits = 0))
```

|     | dateTPE    | type  |  comments\_count|
|-----|:-----------|:------|----------------:|
| 7   | 2016-01-13 | photo |             2116|
| 6   | 2016-01-14 | photo |             1173|
| 5   | 2016-01-14 | photo |             1043|
| 46  | 2016-03-24 | photo |             1022|
| 10  | 2016-01-19 | photo |              979|
| 29  | 2016-02-11 | photo |              951|

每日分享數
----------

因每日的文章數不一樣多，先選出當日最高分享數的文章，再把每個當日最高分享數的文章下去做排行。

``` r
tableshares <- NULL

for(i in unique(totalPage$dateTPE)){
  
  selectTPE<-subset(totalPage,dateTPE==i)
  
  tableshares <- rbind(tableshares,selectTPE[order(selectTPE$shares_count,decreasing=T),c("dateTPE","type","shares_count")])
  
}

finaloutputshares <- tableshares[order(tableshares$shares_count,decreasing = T),c("dateTPE","type","shares_count")]

kable(head(finaloutputshares,digits = 0))
```

|     | dateTPE    | type   |  shares\_count|
|-----|:-----------|:-------|--------------:|
| 46  | 2016-03-24 | photo  |           1820|
| 7   | 2016-01-13 | photo  |           1582|
| 25  | 2016-02-06 | status |            954|
| 41  | 2016-03-09 | photo  |            408|
| 9   | 2016-01-21 | link   |            395|
| 6   | 2016-01-14 | photo  |            364|
