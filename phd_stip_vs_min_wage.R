
data=read.csv('Stipend.data.csv')
hours=40

data$min_gross=(data$min/100*hours*52)
data$min_takehome=sapply(1:length(data$min),function(i) data$min_gross[i]-max(c(0,data$min_gross[i]-data$personal_allow[i]))*0.2-
                      max(c(0,data$min_gross[i]-data$NI_thresh[i]*52))*0.12)
par(mfrow=c(2,1))

#plot absolute 
plot(data$year2,data$stipend,ylim=c(min(c(data$min_takehome[!is.na(data$min_takehome)],
                                          data$stipend[!is.na(data$stipend)])),
                                    max(c(data$min_takehome[!is.na(data$min_takehome)],
                                          data$stipend[!is.na(data$stipend)]))),
     xaxt='n',cex=1.3,pch=19,
     ylab='Take home pay (£)',xlab='',main=paste('Minimum wage job vs UKRI standard PhD \n (' ,hours ,'hours of work/week)'))
par(new=T)

plot(data$year2,data$min_takehome,pch=17,col=2,ylim=c(min(c(data$min_takehome[!is.na(data$min_takehome)],
                                                            data$stipend[!is.na(data$stipend)])),
                                                      max(c(data$min_takehome[!is.na(data$min_takehome)],
                                                            data$stipend[!is.na(data$stipend)]))),
     cex=1.3,xlab='tax year',ylab='',xaxt='n')

loc=min(c(data$min_takehome[!is.na(data$min_takehome)],
          data$stipend[!is.na(data$stipend)]))-(
max(c(data$min_takehome[!is.na(data$min_takehome)],
      data$stipend[!is.na(data$stipend)]))-min(c(data$min_takehome[!is.na(data$min_takehome)],
                                                 data$stipend[!is.na(data$stipend)])))/5
  
  
  
text(x=data$year2,y=loc,labels=paste(data$year1,data$year2,sep='/'),srt=45, xpd=TRUE)

legend(col=1:2,bty='n',x='topleft',legend=c('PhD stipend','minimum wage'),pch=c(19,17))
################################################### real terms adjustment
plot(data$year2,data$stipend*data$inflation,ylim=c(min(c((data$min_takehome*data$inflation)[!is.na(data$min_takehome)],
                                                         (data$stipend*data$inflation)[!is.na(data$stipend)])),
                                                   max(c((data$min_takehome*data$inflation)[!is.na(data$min_takehome)],
                                                         (data$stipend*data$inflation)[!is.na(data$stipend)])))
       ,
     xaxt='n',cex=1.3,pch=19,
     ylab='Take home pay  (2020 £)',xlab='',main=paste('Minimum wage job vs UKRI standard PhD (2020 prices)\n (' ,hours ,'hours of work/week)'))
par(new=T)

plot(data$year2,data$min_takehome*data$inflation,pch=17,col=2,ylim=c(min(c((data$min_takehome*data$inflation)[!is.na(data$min_takehome)],
                                                                           (data$stipend*data$inflation)[!is.na(data$stipend)])),
                                                                     max(c((data$min_takehome*data$inflation)[!is.na(data$min_takehome)],
                                                                           (data$stipend*data$inflation)[!is.na(data$stipend)]))),
     cex=1.3,xlab='tax year',ylab='',xaxt='n')

loc=min(c((data$min_takehome*data$inflation)[!is.na(data$min_takehome)],
          (data$stipend*data$inflation)[!is.na(data$stipend)]))-(
            max(c((data$min_takehome*data$inflation)[!is.na(data$min_takehome)],
                  (data$stipend*data$inflation)[!is.na(data$stipend)]))-min(c((data$min_takehome*data$inflation)[!
                                                              is.na(data$min_takehome)],
                                                             (data$stipend*data$inflation)[!is.na(data$stipend)])))/5

text(x=data$year2,y=loc,labels=paste(data$year1,data$year2,sep='/'),srt=45, xpd=TRUE)
