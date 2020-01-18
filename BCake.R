library(plotrix)

candle = function(pos)
{
  x=pos[1]
  y=pos[2]
  rect(x,y,x+.2,y+2,col="red")
  polygon(c(x+.05,x-.1,x+.1,x+.3,x+.15,x+0.05), c(y+2,y+2.3,y+2.6,y+2.3,y+2,y+2),col="#EECF08")
  
}

plot(c(0,10), c(0,10),type="n", bty="n",xaxt="n",yaxt="n", main="Happy B Day! D! for your 30 years old!", xlab="God's love always! - Yohan, Nov. 21, 2019",ylab="")
draw.ellipse(5,2,col="#73EA71",a=4.4,b=1.7,border=1)
draw.ellipse(5,2,col="#BAE5FF",a=4,b=1.4,border=1)
rect(1,2,9,5,col="#BAE5FF",border="#BAE5FF")
lines(c(1,1),c(2,5))
lines(c(9,9),c(2,5))
draw.ellipse(5,5,col="#FCD2DE",a=4,b=1.4)

# for(i in runif(30)*7+1.3){
#   for(j in runif(30)*2+3.5){
#     candle(c(i,j))
#   }
# }

candle(c(2.5,4.5))
candle(c(4,4.5))
candle(c(7,5.2))

