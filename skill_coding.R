#######################################################################
#                                                                     #
# 技能发动条件：     时间、note、combo、perfect                       #
#                    得分、回血次数、                                 #
#                    星星perfect、星星、                              #
#                    双押双perfect、回血值、                          #
#                    连锁发动                                         #
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


#触发条件


Skill_Coding<-function(data_Card_skill){
  data_Card_skill$trigger<- ifelse(str_detect(data_Card_skill$skill,"^PERFECT"),"P",
                                   ifelse(str_detect(data_Card_skill$skill,"^リズムアイコン"),"N",
                                          ifelse(str_detect(data_Card_skill$skill,"^コンボ"),"C",
                                                 ifelse(str_detect(data_Card_skill$skill,"^[0-9]{1,}秒"),   "T",
                                                        ifelse(str_detect(data_Card_skill$skill,"^スコア"),        "Score",
                                                               ifelse(str_detect(data_Card_skill$skill,"^スターアイコンPERFECT"),        "StarPefect",
                                                                      ifelse(str_detect(data_Card_skill$skill,"^自身を除く\\S{3,10}の特技がすべて発動する"),        "Skill",
                                                                             "other" #施工未完
                                                                      )))))))
  
  
  
  
  #触发概率
  data_Card_skill$probability<-(str_extract(data_Card_skill$skill,"\\d{1,}%の確率で") %>% 
                                  str_extract(.,pattern="\\d{1,}") %>% as.numeric)/100
  
  #触发周期
  
  #时间
  data_Card_skill$T_T<-str_extract(data_Card_skill$skill,"^\\d{1,}") %>% as.numeric()
  #combo
  data_Card_skill$T_C<-str_extract(data_Card_skill$skill,"^コンボ\\d{1,}を達成") %>%
    str_extract(.,pattern="\\d{1,}")%>% as.numeric
  #note
  data_Card_skill$T_N<-str_extract(data_Card_skill$skill,"リズムアイコン\\d{1,}個ごと") %>%
    str_extract(.,pattern="\\d{1,}")%>% as.numeric
  #perfect
  data_Card_skill$T_P<-str_extract(data_Card_skill$skill,"PERFECTを\\d{1,}回達成する") %>%
    str_extract(.,pattern="\\d{1,}")%>% as.numeric
  #score
  data_Card_skill$T_Score<-str_extract(data_Card_skill$skill,"スコア\\d{1,}達成") %>%
    str_extract(.,pattern="\\d{1,}")%>% as.numeric
  #star perfect
  data_Card_skill$T_StarPerfect<-str_extract(data_Card_skill$skill,"スターアイコンPERFECT\\d{1,}回") %>%
    str_extract(.,pattern="\\d{1,}")%>% as.numeric
  #skill(触发人物)
  data_Card_skill$Skill_team<-str_extract(data_Card_skill$skill,"自身を除く\\S{1,}の特技がすべて発動する") %>%
    str_extract(.,pattern="(μ's|Aqours)")
  data_Card_skill$Skill_subteam<-str_extract(data_Card_skill$skill,"自身を除く\\S{1,}の特技がすべて発動する") %>%
    str_extract(.,pattern="(1年生|2年生|3年生)")
  
  
  
  
  
  
  #技能效果
  
  data_Card_skill$effect<-ifelse(str_detect(data_Card_skill$skill,"スコアが\\d{1,}増える$"),                         "Score Up",
                                 ifelse(str_detect(data_Card_skill$skill,"体力が\\d{1,}回復する$"),                         "Healer",
                                        ifelse(str_detect(data_Card_skill$skill,"判定が\\S{1,}秒(間少し)?強化される$"),            "Perfect Lock",
                                               ifelse(str_detect(data_Card_skill$skill,"属性Pが\\S{1,}UPする$"),                          "Appeal Boost",
                                                      ifelse(str_detect(data_Card_skill$skill,"PERFECT時のタップSCOREが\\d{1,}増える$"),         "Perfect Score Up",
                                                             ifelse(str_detect(data_Card_skill$skill,"他の特技の発動確率が\\S{1,}倍になる$"),           "Skill Boost",
                                                                    ifelse(str_detect(data_Card_skill$skill,"同じ属性Pになる$"),                               "Mirror",
                                                                           ifelse(str_detect(data_Card_skill$skill,"直前に発動した特技リピート以外の特技効果を発動$"),"Encore",
                                                                                  "other"
                                                                           ))))))))
  
  #Healer
  data_Card_skill$Healer<-str_extract(data_Card_skill$skill,"体力が\\d{1,}回復する$") %>%
    str_extract(.,pattern="\\d{1,}")%>% as.numeric
  #Perfect Lock
  data_Card_skill$PerfectLock<-str_extract(data_Card_skill$skill,"判定が((\\d{1,})(.\\d{1,})?)秒(間少し)?強化される$") %>%
    str_extract(.,pattern="((\\d{1,})(.\\d{1,})?)")%>% as.numeric*1000#单位为毫秒
  #Score Up
  data_Card_skill$ScoreUp<-ifelse(data_Card_skill$effect=="Healer",data_Card_skill$Healer * 480,
                                  str_extract(data_Card_skill$skill,"スコアが\\d{1,}増える$") %>%
                                    str_extract(.,pattern="\\d{1,}")%>% as.numeric)
  #Appeal Boost
  data_Card_skill$AppealBoost_effect<-((str_extract(data_Card_skill$skill,"属性Pが\\S{1,}UPする$") %>%
                                          str_extract(.,pattern="((\\d{1,})(.\\d{1,})?)")%>% as.numeric)/100+1)
  data_Card_skill$AppealBoost_t.effect<-(str_extract(data_Card_skill$skill,"((\\d{1,})(.\\d{1,})?)秒間(μ's|Aqours)(1年生|2年生|3年生)の属性Pが\\d{1,}%UPする$") %>%
                                           str_extract(.,pattern="^((\\d{1,})(.\\d{1,})?)")%>% as.numeric)*1000
  data_Card_skill$AppealBoost_team<-str_extract(data_Card_skill$skill,"((\\d{1,})(.\\d{1,})?)秒間(μ's|Aqours)(1年生|2年生|3年生)の属性Pが\\d{1,}%UPする$") %>%
    str_extract(.,pattern="(μ's|Aqours)")
  data_Card_skill$AppealBoost_subteam<-str_extract(data_Card_skill$skill,"((\\d{1,})(.\\d{1,})?)秒間(μ's|Aqours)(1年生|2年生|3年生)の属性Pが\\d{1,}%UPする$") %>%
    str_extract(.,pattern="(1年生|2年生|3年生)")
  
  
  #Perfect Score Up
  data_Card_skill$PerfectScoreUp<-str_extract(data_Card_skill$skill,"PERFECT時のタップSCOREが\\d{1,}増える$") %>%
    str_extract(.,pattern="\\d{1,}")%>% as.numeric
  #Skill Boost
  data_Card_skill$SkillBoost<-str_extract(data_Card_skill$skill,"他の特技の発動確率が\\S{1,}倍になる$") %>%
    str_extract(.,pattern="((\\d{1,})(.\\d{1,})?)") %>% as.numeric
  
  #Mirror  #彩虹队、
  #Encore
  #无
  
  return(data_Card_skill)
}
