# sif
get the icons, kanons and bmps of sif

完成全部下载（编号1~1548的卡面）亲测一共8531.91秒

# 使用方法：
1. 下载card.R文件，并使用记事本或者notepad++等工具打开，修改其中的路径(wk0,wk_icon,wk_card)三个变量，改为你自定义的文件路径。请先保证磁盘空间足够。
2. 使用本地的工具运行card.R脚本，可自行下载R(https://www.r-project.org/)   是免费的！！！
3. 下载相应的package：
    所用的package列表如下：
      XML   rvest   stringr   dplyr   rjson   plyr
      
     安装方法非常容易，以XML为例，只需在console界面运行一句 
     ```r
     install.packages("XML")
     ```
     即可

     
4. 运行card.R脚本


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
