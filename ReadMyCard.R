###############################
#                             #
#      read my card           #
#                             #
###############################

myCard<-readLines("E:\\LL\\submember-18-4-19.sd")
myCard<-JsonToDataframe(myCard)
names(myCard)<-c("ID","Mezame","SkillLevel","MaxCost")
myCard$ID<-as.numeric(myCard$ID)
  
myCard.List<-Card_update[myCard$ID]

################################
#                              #
#    coding the skill          #
#                              #
################################

#######################################################################
#                                                                     #
# 技能发动条件：     时间、note、combo、perfect                       #
#                    得分、回血次数、                                 #
#                    星星perfect、星星、                              #
#                    双押双perfect、回血值、                          #
#                    连锁发动(?)                                      #
#                                                                     #
#                                                                     #
#                                                                     #
#  技能效果：        判定、体力、分数、                               #
#                    技能发动率提升(Skill Boost)、                    #
#                    重复上一技能(Encore)                             #
#                    perfect分数增加（%）、                           #
#                    perfect分数增加(分)   (Perfect Score Up)         #
#                    combo fever（%）、combo fever（分）              #
#                    属性值同步(Mirror)（彩虹大队）                   #
#                    技能等级上升、                                   #
#                    属性值上升(Appeal Boost)                         #
#                                                                     #
#######################################################################


skill_encoding<-function(skill){
  #触发条件
  #skill<-skill_effect0
  trigger<- ifelse(str_detect(skill,"^PERFECT"),"P",
            ifelse(str_detect(skill,"^リズムアイコン"),"N",
            ifelse(str_detect(skill,"^コンボ"),"C",
            ifelse(str_detect(skill,"^[0-9]{1,}秒"),"T",
            ifelse(str_detect(skill,"^スコア"),        "Score",
            ifelse(str_detect(skill,"^スターアイコンPERFECT"),        "StarPefect",
            ifelse(str_detect(skill,"^自身を除く\\S{3,10}の特技がすべて発動する"),        "Skill",
            "other" #施工未完
            )))))))
  
  #触发概率
  probability<-(str_extract(skill,"\\d{1,}%の確率で") %>% 
                  str_extract(.,pattern="\\d{1,}") %>% as.numeric)/100
  
  #触发周期
  
    #时间
    T_T<-str_extract(skill,"^\\d{1,}") %>% as.numeric()
    #combo
    T_C<-str_extract(skill,"^コンボ\\d{1,}を達成") %>%
      str_extract(.,pattern="\\d{1,}")%>% as.numeric
    #note
    T_N<-str_extract(skill,"リズムアイコン\\d{1,}個ごと") %>%
      str_extract(.,pattern="\\d{1,}")%>% as.numeric
    #perfect
    T_P<-str_extract(skill,"PERFECTを\\d{1,}回達成する") %>%
      str_extract(.,pattern="\\d{1,}")%>% as.numeric
    #score
    T_Score<-str_extract(skill,"スコア\\d{1,}達成") %>%
      str_extract(.,pattern="\\d{1,}")%>% as.numeric
    #star perfect
    T_StarPerfect<-str_extract(skill,"スターアイコンPERFECT\\d{1,}回") %>%
      str_extract(.,pattern="\\d{1,}")%>% as.numeric
    #skill(触发人物)
    Skill_team<-str_extract(skill,"自身を除く\\S{1,}の特技がすべて発動する") %>%
      str_extract(.,pattern="(μ's|Aqours)")
    Skill_subteam<-str_extract(skill,"自身を除く\\S{1,}の特技がすべて発動する") %>%
      str_extract(.,pattern="(1年生|2年生|3年生)")
  
  #effect
    #技能效果
    
    effect<-ifelse(str_detect(skill,"スコアが\\d{1,}増える$"),                         "Score Up",  #分
            ifelse(str_detect(skill,"体力が\\d{1,}回復する$"),                         "Healer",    #奶
            ifelse(str_detect(skill,"判定が\\S{1,}秒(間少し)?強化される$"),            "Perfect Lock",#判
            ifelse(str_detect(skill,"属性Pが\\S{1,}UPする$"),                          "Appeal Boost",
            ifelse(str_detect(skill,"PERFECT時のタップSCOREが\\d{1,}増える$"),         "Perfect Score Up",
            ifelse(str_detect(skill,"他の特技の発動確率が\\S{1,}倍になる$"),           "Skill Boost",
            ifelse(str_detect(skill,"同じ属性Pになる$"),                               "Mirror",
            ifelse(str_detect(skill,"直前に発動した特技リピート以外の特技効果を発動$"),"Encore",
            "other"
                                                                             ))))))))
    #Healer
    Healer<-str_extract(skill,"体力が\\d{1,}回復する$") %>%
      str_extract(.,pattern="\\d{1,}")%>% as.numeric
    #Perfect Lock
    PerfectLock<-str_extract(skill,"判定が((\\d{1,})(.\\d{1,})?)秒(間少し)?強化される$") %>%
      str_extract(.,pattern="((\\d{1,})(.\\d{1,})?)")%>% as.numeric*1000#单位为毫秒
    #Score Up
    ScoreUp<-ifelse(effect=="Healer",Healer * 480,
                                    str_extract(skill,"スコアが\\d{1,}増える$") %>%
                                      str_extract(.,pattern="\\d{1,}")%>% as.numeric)
    #Appeal Boost
    AppealBoost_effect<-((str_extract(skill,"属性Pが\\S{1,}UPする$") %>%
                                            str_extract(.,pattern="((\\d{1,})(.\\d{1,})?)")%>% as.numeric)/100+1)
    AppealBoost_duration.effect<-(str_extract(skill,"((\\d{1,})(.\\d{1,})?)秒間(μ's|Aqours)(1年生|2年生|3年生)の属性Pが\\d{1,}%UPする$") %>%
                                             str_extract(.,pattern="^((\\d{1,})(.\\d{1,})?)")%>% as.numeric)*1000
    AppealBoost_team<-str_extract(skill,"((\\d{1,})(.\\d{1,})?)秒間(μ's|Aqours)(1年生|2年生|3年生)の属性Pが\\d{1,}%UPする$") %>%
      str_extract(.,pattern="(μ's|Aqours)")
    AppealBoost_subteam<-str_extract(skill,"((\\d{1,})(.\\d{1,})?)秒間(μ's|Aqours)(1年生|2年生|3年生)の属性Pが\\d{1,}%UPする$") %>%
      str_extract(.,pattern="(1年生|2年生|3年生)")
    
    
    #Perfect Score Up
    PerfectScoreUp<-str_extract(skill,"PERFECT時のタップSCOREが\\d{1,}増える$") %>%
      str_extract(.,pattern="\\d{1,}")%>% as.numeric
    #Skill Boost
    SkillBoost<-str_extract(skill,"他の特技の発動確率が\\S{1,}倍になる$") %>%
      str_extract(.,pattern="((\\d{1,})(.\\d{1,})?)") %>% as.numeric
    
    #Mirror  #彩虹队、
    #Encore
    #无
  
  xxx<-list(trigger=trigger,probability=probability,
         T_T=T_T,T_C=T_C,T_N=T_N,T_P=T_P,T_Score=T_Score,T_StarPerfect=T_StarPerfect,
         Skill_team=Skill_team,Skill_subteam=Skill_subteam,
         effect=effect,
         
         Healer=Healer,ScoreUp=ScoreUp,PerfectLock=PerfectLock,
         
         AppealBoost_effect=AppealBoost_effect,
         AppealBoost_duration.effect=AppealBoost_duration.effect,
         AppealBoost_team=AppealBoost_team,
         AppealBoost_subteam=AppealBoost_subteam,
         PerfectScoreUp=PerfectScoreUp,
         SkillBoost=SkillBoost)
  
  
  return(xxx)
  
}





myCard.List[[1]]$Card_skill
howMany_myCard<-length(myCard.List)

raw_myCard<-list()
for(i in 1:length(myCard.List)){
  card_skill0<-myCard.List[[i]]$Card_skill
  skill_level0<-myCard[i,"SkillLevel"] %>% as.numeric()
  skill_effect0<-card_skill0[skill_level0,"Effect"] %>% as.character()
  skill0<-skill_encoding(skill_effect0) %>% unlist
  
  
  #raw attribution
  for(j in c("Smile","Pure","Cool")){
    txt<-paste(j,"0<-myCard.List[[i]]$",j,myCard[i,"Mezame"],sep="")
    eval(parse(text=txt))
  }
  
  raw_myCard0<-c(myCard[i,],myCard.List[[i]][2:5],Smile=Smile0,Pure=Pure0,Cool=Cool0,
                 skill=skill_effect0,skill0) %>% 
                unlist %>% t %>% data.frame(.,stringsAsFactors=F)
  raw_myCard<-bind_rows(raw_myCard,raw_myCard0)
}
rm(list=(ls()%>%str_extract_all(".*0$") %>% unlist)[1:8])
  