library(gdata)

find_clear <- function (x,pat) {
    
    if (is.character(x)|is.factor(x)) 
        gsub(pat,"",x) 
    else x

}

format_date <- function (x) {
    
    if (is.character(x)|is.factor(x)) 
        paste(substr(x,3,4),"/",substr(x,1,2),sep="")
    else x

}

#setwd("d:\\Programming\\R\\sberbank-newyear\\data")
currencies = c("Новогодний_руб","Новогодний_доллСША","Новогодний_евро")
resfilename <- "..\\res.csv"

rub = vector(length=9)
usd = vector(length=9)
eur = vector(length=9)
for (file in list.files()) {
	d = read.xls(file,skip=8,row.names=c(),header=F,sheet=1)[,c(3,4,5)]
	rub=rbind(rub,as.vector(unlist(d[c(3,6,8),])))
    d = read.xls(file,skip=7,row.names=c(),header=F,sheet=2)[,c(3,4,5)]
    usd=rbind(usd,as.vector(unlist(d[c(3,6,8),])))
    d = read.xls(file,skip=8,row.names=c(),header=F,sheet=3)[,c(3,4,5)]
    eur=rbind(eur,as.vector(unlist(d[c(3,6,8),])))
}

rub = data.frame(rub[-1,])
usd = data.frame(usd[-1,])
eur = data.frame(eur[-1,])
names(rub) = c("m3_10","m6_10","y_10","m3_30","m6_30","y_30","m3_100","m6_100","y_100")
names(usd) = c("m3_2","m6_2","y_2","m3_10","m6_10","y_10","m3_100","m6_100","y_100")
names(eur) = c("m3_2","m6_2","y_2","m3_10","m6_10","y_10","m3_100","m6_100","y_100")

#write.table(adata2,file=resfilename,sep=",")

temp1 = find_clear(list.files(),".xls")
temp2 = find_clear(temp1,"nys")
dates = format_date(temp2)

row.names(rub) <- dates
row.names(usd) <- dates
row.names(eur) <- dates

plot(rub$m3_10,type="b",xlab="Date",xaxt='n',ylab="Deposit interest rate",ylim=c(0,max(rub)),col="red",pch=19)
axis(1,at=seq(length(dates)),labels=dates,las=3)
lines(usd$m3_2,col="green")
lines(eur$m3_2,col="blue")

maxrub = apply(rub,1,max)
minrub = apply(rub,1,min)
xs = seq(length(dates))

arrows(xs,minrub,xs,maxrub,code=3,angle=90,length=0.05,col="red")
legend("topleft",c("RUB (optim and min/max envelope)","USD","EUR"),fill=c("red","green","blue"))

