#ukri standard stipend- uses GDP deflator?
stipend=rev(c(15609,15285,15009,14777,14553,14296,14057,13863,13726,13590,13590,13590))
#inflation adjustment from Bank of England 
infl_adj=rev(c(1000/1.042,1000,1015,1041,1076,1114,1134,1145,1172,1208,1246,1311)/1000)
plot(stipend*infl_adj)

#minimum wage in pence per hour
min=c(608,619,631,650,670,720,750,783,821,872,891,950)
hours=40

#20% income tax over this 
personal_allow=c(12570,12570,12500,12500,11850,11500,10600,10000,9440,8105,7475,6475)
#12% NI over this
NI_thresh=c(190,184,183,166,162,157,155,153,149,146,136,110)


length(min);length(stipend);length(personal_allow);length(NI_thresh)
#plot(personal_allow,NI_thresh)

min_gross=(min/100*hours*52)
min_takehome=sapply(1:length(min),function(i) min_gross[i]-max(c(0,min_gross[i]-personal_allow[i]))*0.2-
                      max(c(0,min_gross[i]-NI_thresh[i]*52))*0.12)
par(mfrow=c(2,1))

#plot absolute 
plot(stipend,ylim=c(min(c(min_takehome,stipend)),max(c(min_takehome,stipend))),
     xaxt='n',cex=1.3,pch=19,
     ylab='Take home pay (£)',xlab='',main=paste('Minimum wage job vs UKRI standard PhD \n (' ,hours ,'hours of work/week)'))
par(new=T)

plot(min_takehome,pch=17,col=2,ylim=c(min(c(min_takehome,stipend)),
                                     max(c(min_takehome,stipend))),
     cex=1.3,xlab='tax year',ylab='',xaxt='n')

loc=min(c(min_takehome,stipend))-
  ((max(c(min_takehome,stipend)))-min(c(min_takehome,stipend)))/6.6
text(x=1:12,y=loc,labels=paste('20',10:21,'/',11:22,sep=''),srt=45, xpd=TRUE)

legend(col=1:2,bty='n',x='topleft',legend=c('PhD stipend','minimum wage'),pch=c(19,17))
################################################### real terms adjustment

plot(stipend*infl_adj,ylim=c(min(c(min_takehome*infl_adj,stipend*infl_adj)),max(c(min_takehome*infl_adj,stipend*infl_adj))),
     xaxt='n',cex=1.3,pch=19,
     ylab='Take home pay (2020 £)',xlab='',main=paste('Minimum wage job vs UKRI standard PhD (2020 prices) \n (',hours ,'hours of work/week)'))
par(new=T)

plot(min_takehome*infl_adj,pch=17,col=2,ylim=c(min(c(min_takehome*infl_adj,stipend*infl_adj)),
                                     max(c(min_takehome*infl_adj,stipend*infl_adj))),
     cex=1.3,xlab='tax year',ylab='',xaxt='n')


loc=min(c(min_takehome*infl_adj,stipend*infl_adj))-
((max(c(min_takehome*infl_adj,stipend*infl_adj)))-min(c(min_takehome*infl_adj,stipend*infl_adj)))/6.6
text(x=1:12,y=loc,labels=paste('20',10:21,'/',11:22,sep=''),srt=45, xpd=TRUE)
legend(col=1:2,bty='n',x='topright',legend=c('PhD stipend','minimum wage'),pch=c(19,17))

