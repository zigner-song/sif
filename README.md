# sif
get the icons, kanons and bmps of sif

完成全部下载（编号1~1548的卡面）亲测一共8531.91秒

# 文件说明：
目前已更新了3个脚本文件

1. <code>package.R</code>  加载所需要的功能包
2. <code>card.R</code>  爬取所需要的卡牌信息，以及立绘
3. <code>bmp.R</code>  爬取谱面信息和谱面，以json格式以及R中的list格式(.RData)保存在本地
4. <code>ReadMyCard.R</code>  读取本地卡组信息，队伍需要以json格式<b>预先</b>保存在本地，建议使用工具[LLHelper]<http://llhelper.com/llnewautounit>编辑
5. <code>skill_coding.R</code>  编码技能信息





# 使用方法：
1. 下载package.R,card.R文件，并使用记事本或者notepad++等工具打开，修改其中card.R中的路径(wk0,wk_icon,wk_card)三个变量，改为你自定义的文件路径。请先保证磁盘空间足够。
2. 使用本地的工具运行card.R脚本，可自行下载R(<https://www.r-project.org/>)   是免费的！！！
3. 下载相应的package：
    所用的package列表如下：
      XML   rvest   stringr   dplyr   rjson   plyr
      
     安装方法非常容易，以XML为例，只需在console界面运行一句 
     `install.packages("XML")`
     <br/>即可

     
4. 先运行package.R脚本，再运行其他所需要的脚本


如果有资源有时间有钱，会放在服务器上，然后用户就不需要再安装R以及相应的package了

# 目前已完成：
SIF全部卡牌信息（立绘、小图、属性、技能）的获取 √


# 尚未完成：
1. 卡牌去框立绘的获取（不知道哪里有静态的的去框图可以下啊！！！）
2. 谱面的自动下载(√已完成) 及 可视化
3. 重抽样法(?)模拟打歌得分（方案是你给出9张卡，我自动模拟打100次，计算得分，支持新技能，支持判卡对P率的修正）
4. 自动配宝石工具
5. 自动组卡工具
6. 利用shiny制作便于使用的工具


联系方式: 
zigner.song@gmail.com

QQ: 596655168
