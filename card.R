library(XML)
library(rvest)
library(stringr)
library(dplyr)
library(rjson)
library(plyr)

#读取网页的函数，避免暂时连接不上导致无法读取
read_Url <- function(url) {
  out <- tryCatch({
    url %>% as.character() %>% read_html() 
  },
  error=function(cond) {
    message(paste("URL does not seem to exist:", url))
    message("Here's the original error message:")
    message(cond)
    return(NA)
  })    
  return(out)
}

#更新于2018-4-19
#路径信息
wk0<-"E://practice//R//lovelive//lovelive"  #此为保存的工作路径
wk_icon<-"E://practice//R//lovelive//lovelive//icon" #此为保存的icon路径
wk_card<-"E://practice//R//lovelive//lovelive//card" #此为保存的card路径

setwd(wk0)

#成员资料
data_idol<-data.frame(idol=factor(c("Nishikino Maki","Hoshizora Rin","Koizumi Hanayo",
                                    "Kousaka Honoka","Sonoda Umi","Minami Kotori",
                                    "Yazawa Nico","Ayase Eli","Toujou Nozomi",
                                    "Kurosawa Ruby","Kunikida Hanamaru","Tsushima Yoshiko",
                                    "Takami Chika","Sakurauchi Riko","Watanabe You",
                                    "Ohara Mari","Kurosawa Dia","Matsuura Kanan")),
                      team=factor(c(rep("μ's",9),rep("Aqours",9))),
                      subteam=factor(c("BiBi","Lily White","Printemps",
                                       "Printemps","Lily White","Printemps",
                                       "BiBi","BiBi","Lily White",
                                       "Cyaron","Azalea","Guilty Kiss",
                                       "Cyaron","Guilty Kiss","Cyaron",
                                       "Guilty Kiss","Azalea","Azalea")),
                      grade=factor(rep(c(rep(1,3),rep(2,3),rep(3,3)),2)))
str(data_idol)


###############
#爬虫部分
N_Card<-1548 #目前更新至编号为1548号

CardID <- idol<-rarity<-attribution<-grade<-NULL
Card_skill<-img_URL<-img_card_URL<-list()
ins<-NULL
for(m in c("Smile","Pure","Cool")){
  for(n in c("non_idolized","idolized")){
    ins<-c(ins,paste(m,"_",n,sep=""))
  }
}
for(i in ins){
  txt<-paste(ins,"<-NULL")
  eval(parse(text=txt))
}

start.Time<-Sys.time()
#开始爬虫
for(CardID0 in 1:N_Card){
  #进度条
  now.Time<-Sys.time()
  duration<-difftime(now.Time,start.Time,units = "secs") %>% as.numeric()
  duration0<-floor(duration*100)/100
  ato.time<-duration*(N_Card-CardID0)/CardID0
  stop.time<-now.Time+ato.time
   
  print(paste(">>>> ",floor(CardID0/N_Card*100000)/1000,"% >>>> 已花费时间",duration,"sec >>>> 预计完成时间",stop.time,sep=""))
  
  #爬取基本信息
  URL_Card0<-"https://schoolido.lu/cards/"
  web0<-read_Url(paste(URL_Card0,(as.character(CardID0)),sep=""))
  
  #爬取三围属性
  for(m in c("Smile","Pure","Cool")){
    for(n in c("non_idolized","idolized")){
      txt0<-paste(m,"_",n,"0",sep="")
      txt<-paste(txt0,"<-web0 %>% html_nodes(\".statistics_",n,"_maximum\") %>% html_nodes(\".text-",m,"\") %>% html_text() %>% str_replace_all(\"[:space:]\",\"\")",sep="")
      eval(parse(text=txt)) #Cool_idolized0<-web0 %>% html_nodes(\".statistics_idolized_maximum\") %>% html_nodes(\".text-Cool\") %>% html_text() %>% str_replace_all(\"[:space:]\",\"\")
      
      txt<-paste("if(length(",txt0," )== 0){",txt0,"<-NA}",sep="")
      eval(parse(text=txt))#if(length(Cool_idolized0==0){Cool_idolized0<-NA}
    }
  }
  
  #爬取卡牌信息，包括人物、稀有度、卡牌颜色等
  idol0<-(web0 %>% html_nodes("strong") %>% html_text())[1]
  rarity0<-web0 %>% html_nodes("title") %>% html_text() %>% str_extract(pattern="\\w{1,}$")
  
  detail.info<-web0 %>% html_nodes("small")%>% html_text()%>% str_replace_all(pattern="\n","") %>% paste(.,collapse = " ")
  attribution0<-detail.info %>% str_extract(pattern="(クール)|(スマイル)|(ピュア)")
  
  attribution0<-if_else(attribution0=="スマイル",
                        "Smile",
                        if_else(attribution0=="ピュア",
                                "Pure",
                                if_else(attribution0=="クール","Cool","All","All"),
                                "All"),
                        "All")
  grade0<-detail.info %>% str_extract(pattern="Year: \\w{1,}") %>% str_extract(pattern="\\w{1,}$")
  
  #######
  #查找图片 img
  
     #icon图片
  img_URL0<-web0 %>% html_nodes("a") %>% html_nodes(".pull-right") %>% html_nodes("img") %>% 
              html_attr("src") %>%
              str_extract_all(pattern=paste(".*",CardID0,".*",sep="")) %>% 
              unlist() %>% paste("http:",.,sep="")
  
  
  if(length(img_URL0)==1){img_URL0<-c(img_URL0,img_URL0)}
  if(length(img_URL0)==0){img_URL0<-c(NA,NA)}
 
  img_URL0<-list(img_URL0)
  img_URL<-c(img_URL,img_URL0)
  
  #下载icon图片
  setwd(wk_icon)
  picName<-paste(CardID0,"_",c("unidolized","idolized"),"_",str_extract(idol0,"\\w{1,}$"),".png",sep="")
  curl::curl_download(img_URL0[[1]][1],destfile = picName[1] )
  curl::curl_download(img_URL0[[1]][2],destfile = picName[2] )
  
    #爬取Card立绘
  setwd(wk_card)
  
  img_card_URL0<-web0 %>% html_nodes(".card_images") %>% html_nodes("a") %>% html_attr("href") %>%
                    paste("http:",.,sep="")
  
  if(length(img_card_URL0)==1){img_card_URL0<-c(img_card_URL0,img_card_URL0)}
  img_card_URL0<-list(img_card_URL0)
  img_card_URL<-c(img_card_URL,img_card_URL0)
  #下载立绘
  picName<-paste(CardID0,"_",c("unidolized","idolized"),"_",str_extract(idol0,"\\w{1,}$"),".png",sep="")
  curl::curl_download(img_card_URL0[[1]][1],destfile = picName[1] )
  curl::curl_download(img_card_URL0[[1]][2],destfile = picName[2] )
  
  #重新设置工作路径
  setwd(wk0)
  
  ##############
  
  
  ins<-NULL
  for(m in c("Smile","Pure","Cool")){
    for(n in c("non_idolized","idolized")){
      ins<-c(ins,paste(m,"_",n,sep=""))
    }
  }
  for(j in c("CardID","idol","rarity","attribution","grade",ins)){
    txt0<-paste(j,"0",sep="")
    txt<-paste("if(length(",txt0," )== 0){",txt0,"<-NA}",sep="")
    eval(parse(text=txt))
    txt<-paste(j,"<-c(",j,",",txt0,")",sep="")
    eval(parse(text=txt))
  }#检查并合并全部信息
  
  ################
  #爬取skill信息  
  
  URL_Card1<-"https://db.loveliv.es/card/number/"
  web1<-read_Url(paste(URL_Card1,(CardID0),sep=""))
  #爬取技能信息
  if(idol0 %in% data_idol$idol & rarity0 %in% c("SR","SSR","UR")){
    Card_skill0.matrix<-web1 %>% html_nodes(".active")%>% html_nodes("td") %>% html_text %>% 
      matrix(.,ncol=3,byrow=T)
    Card_skill0<-data.frame(Card_skill0.matrix[-1,]);names(Card_skill0)<-Card_skill0.matrix[1,]
  } else {
    Card_skill0<-NA
  }
  
  Card_skill<-c(Card_skill,list(Card_skill0))
  #################
}

Card_Total_names<-c("CardID","idol","rarity","attribution","grade",ins)
paste(Card_Total_names,collapse = ",")

Card_Total.info<-data.frame(CardID,idol,rarity,attribution,grade,Smile_non_idolized,Smile_idolized,Pure_non_idolized,Pure_idolized,Cool_non_idolized,Cool_idolized)

save(Card_Total.info,"Card_Total.info.RData")

write.csv(Card_Total.info,"Card_Total.info.csv")

save(Card_skill,file="Card_skill.RData")
