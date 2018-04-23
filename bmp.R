#bmp

  #
  p_rate<-0.6

#main info



main_bmp.json<-read_Url("http://r.llsif.win/maps.json") %>% html_text()
main_bmp.df<-JsonToDataframe(main_bmp.json)

#main_bmp$live_setting_id<-as.numeric(main_bmp$live_setting_id)
sort(main_bmp.df$live_setting_id)
str(main_bmp.df)
save(main_bmp.df,file="main_bmp_df.RData")

#主键为live_setting_id


  #main_bmp:
  #attribute_icon:颜色

# 查询具体谱面的地址
bmp_all=NULL

bmplist<-sort(main_bmp.df$live_setting_id)

updateBMPinfo<-function(bmplist){
  bmp_all<-NULL
  startTime=Sys.time()
  
        for(i in 1:length(bmplist)){
          i_bmp<-bmplist[i]
          address0<-main_bmp.df$notes_setting_asset[main_bmp.df$live_setting_id==i_bmp]
          name0<-main_bmp.df$name[main_bmp.df$live_setting_id==i_bmp]
          attribute0<-main_bmp.df$attribute_icon_id[main_bmp.df$live_setting_id==i_bmp]
          difficulty0<-main_bmp.df$difficulty_text[main_bmp.df$live_setting_id==i_bmp]
          
          
          bmp.df0<-read_Url(paste("http://a.llsif.win/live/json/",address0,sep="")) %>% html_text() %>% JsonToDataframe()
          
          bmp.list0<-list(list(live_setting_id=i_bmp,
                             address=address0,
                             name=name0,
                             attribute=attribute0,
                             difficulty=difficulty0,
                             bmp=bmp.df0))
          
          names(bmp.list0)<-paste("live_",as.character(i_bmp),sep="")
          bmp_all<-c(bmp_all,bmp.list0)
          progressbar<-progress_bar(start.Time=startTime,i,length(bmplist))
        }
  return(bmp_all)
}

bmp_all<-updateBMPinfo(sort(as.numeric(main_bmp.df$live_setting_id)))

save(bmp_all,
     file=paste("bmp_all-",(Sys.Date() %>% as.character()),".RData",sep=""))

write(main_bmp.json,file="main_bmp.json")
rm(bmplist);rm(main_bmp.json)
