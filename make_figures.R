args <- commandArgs(trailingOnly=TRUE)
out <- if(length(args)) args[1] else 'figures'
dir.create(out,showWarnings=FALSE,recursive=TRUE)
read.csv <- function(file, ...) utils::read.csv(file, fileEncoding='UTF-8-BOM', ...)
navy <- '#183047'; teal <- '#007C83'; orange <- '#B85B22'; grey <- '#52616E'; pale <- '#F1F5F7'
canvas <- function(title,sub){par(mar=c(0,0,0,0),family='sans',xpd=NA);plot.new();plot.window(c(0,12),c(0,7));}
box <- function(x,y,w,h,head,body,accent=teal){rect(x,y,x+w,y+h,col=pale,border='#CAD4DB',lwd=1);segments(x,y+h,x+w,y+h,col=accent,lwd=3);text(x+.18,y+h-.36,head,adj=c(0,.5),font=2,cex=1.12,col=navy);text(x+.18,y+h-.83,body,adj=c(0,1),cex=1.00,col=grey)}
export <- function(name,fn){
 reg <- c(fig1_silard_red_queen_mechanism='Fig1',fig2_competition_review_intensity='Fig2',fig3_msca_qualification='Fig3',fig4_score_and_decision_patterns='Fig4',fig5_szilard_metrics='Fig5',fig6_escape_routes='Fig6',figS1_nsf_pca_stability='FigS1',figS2_portfolio_costs='FigS2')
 name <- reg[[name]]
 cairo_ps(file.path(out,paste0(name,'.eps')),width=12,height=7,pointsize=14,onefile=FALSE,family='Arial');fn();dev.off()
 svg(file.path(out,paste0(name,'.svg')),width=12,height=7,pointsize=14,family='Arial');fn();dev.off()
 png(file.path(out,paste0(name,'.png')),width=7200,height=4200,res=600,pointsize=14,type='cairo',family='Arial');fn();dev.off()
}

title <- function(main,...){if(grepl('^[ab]  ',main)) graphics::title(main,...)}
mtext <- function(text,side=3,outer=FALSE,...){if(side!=3 || grepl('Rubric and rank',text)) graphics::mtext(text,side=side,outer=outer,...)}
export('fig1_silard_red_queen_mechanism',function(){
 canvas('The proposed Szilard-Red Queen mechanism','Hypothesised dynamics and separate administrative illustrations')
 heads <- c('Competition\npressure','Applicant\nadaptation','Excess\nexcellence','Selection\ncompression','Possible\nscientific\nconsequences')
 bodies <- c('Demand relative\nto award capacity','Content and\npresentation','Above scientific\nthreshold; number\nexceeds awards','Crowded scores or\nweak distinctions','Displaced time,\nnovelty or diversity')
 xs <- c(.4,2.78,5.16,7.54,9.92)
 for(i in 1:5){rect(xs[i],3.35,xs[i]+1.75,5.55,col=pale,border='#CAD4DB');text(xs[i]+.875,5.08,heads[i],font=2,cex=.99,col=navy);text(xs[i]+.875,4.15,bodies[i],cex=.91,col=grey);if(i<5)arrows(xs[i]+1.82,4.55,xs[i+1]-.08,4.55,length=.09,lty=2,lwd=1.7,col=orange)}
 rect(.4,2.0,4.5,2.8,col='#FCF1E9',border=orange)
 text(2.45,2.49,'Later competitive requirements',font=2,col=orange,cex=1)
 text(2.45,2.18,'Can change at fixed demand / award capacity',col=grey,cex=.84)
 arrows(3.45,3.32,3.45,2.85,length=.10,lty=2,col=orange,lwd=1.7)
 segments(4.5,2.4,4.85,2.4,lty=2,col=orange,lwd=1.7);segments(4.85,2.4,4.85,3.05,lty=2,col=orange,lwd=1.7);segments(4.85,3.05,4.1,3.05,lty=2,col=orange,lwd=1.7);arrows(4.1,3.05,4.1,3.3,length=.08,lty=2,col=orange,lwd=1.7)
 text(8.35,2.65,'Downstream arrows mean "may contribute".',col=grey,cex=.98)
 text(8.35,2.2,'Excess qualification and compression are\nneither necessary nor sufficient for feedback.',col=grey,cex=.93)
 rect(.4,.45,11.67,1.52,border=NA,col='#E8F4F3')
 text(.65,1.25,'Observed illustrations',adj=0,font=2,col=teal,cex=.94)
 text(.65,.92,'NSF: pressure and written-review activity',adj=0,col=navy,cex=.88)
 text(.65,.59,'MSCA: qualification versus initial selection',adj=0,col=navy,cex=.88)
 text(6.6,1.25,'NIH: decision mapping',adj=0,col=navy,cex=.88)
 text(6.6,.92,'Taiwan: recorded ties',adj=0,col=navy,cex=.88)
 text(6.6,.59,'No dashed arrow is empirically established here.',adj=0,col=grey,cex=.86)
})
export('fig2_competition_review_intensity',function(){
 d <- read.csv(file.path(out,'../data','nsf_baseline_matrix_and_scores.csv'))
 par(mar=c(5.8,5.1,4.1,1.3),family='sans',col.axis=grey,col.lab=navy)
 plot(100*d$p,d$review_intensity,xlim=c(21,46),ylim=c(3.22,4.43),type='n',xlab='Awards / evaluated proposals (%)',ylab='Written external reviews / evaluated proposal',cex.lab=1.12,axes=FALSE)
 abline(h=seq(3.3,4.3,.2),col='#E1E7EB');axis(1,at=seq(20,45,5));axis(2,at=seq(3.3,4.3,.2),las=1);graphics::box(bty='l',col='#CAD4DB')
 points(100*d$p,d$review_intensity,pch=21,bg=ifelse(d$unit %in% c('NSF_BIO','NSF_OIA'),teal,'#93A7B7'),col='white',cex=1.7)
 labels <- sub('NSF_','',d$unit)
 dx <- c(.18,.3,.28,.28,.3,.3,-.85,-.85,.3,-.65)
 dy <- c(.065,.03,.045,.015,.04,-.055,.055,-.02,.05,-.06)
 text(100*d$p+dx,d$review_intensity+dy,labels,cex=.9,col=navy)
 title('Success rates do not describe the whole review process',adj=0,col.main=navy,cex.main=1.3,line=2)
 mtext('NSF FY2023: ten directorates and offices; preserved working baseline',side=3,adj=0,col=grey,cex=.95,line=.55)
 rect(32,3.92,45.5,4.36,col=pale,border=NA)
 text(32.4,4.26,'BIO  24.06% selected  |  4.151 reviews/proposal',adj=0,cex=.94,col=navy)
 text(32.4,4.14,'OIA  24.33% selected  |  3.637 reviews/proposal',adj=0,cex=.94,col=navy)
 text(32.4,4.02,'BIO review intensity is 14.1% higher.',adj=0,cex=.94,col=teal,font=2)
 mtext('Activity counts are not hours, evaluation quality or a causal estimate of excess burden.',side=1,line=4.7,adj=0,cex=.9,col=grey)
})
export('fig5_szilard_metrics',function(){
 canvas('A Szilard metric family','A transparent profile of pressure, burden and selection; no universal total score')
 box(.4,1.4,3.5,4.3,'SI-P   Competition pressure','')
 box(4.25,1.4,3.5,4.3,'SI-B   Procedural burden','')
 box(8.1,1.4,3.5,4.3,'SI-S   Selection profile','')
 text(.58,4.83,'Nonselection fraction',adj=0,col=grey,cex=1)
 text(.58,4.23,expression(CP == 1-frac(A,N)),adj=0,col=navy,cex=1.15)
 text(.58,3.55,'Demand / announced capacity',adj=0,col=grey,cex=.98)
 text(.58,3.08,expression(frac(N,K)),adj=0,col=navy,cex=1.15)
 text(.58,2.42,'Workforce exposure also needs\ndistinct applicants and workforce.',adj=c(0,1),col=grey,cex=.93)
 text(.58,1.72,'CP and applications/award are redundant.',adj=0,col=grey,cex=.81)
 text(4.43,4.83,'NSF written-review activity proxy',adj=0,col=grey,cex=.94)
 text(4.43,4.23,expression(RI == frac(R,N)),adj=0,col=navy,cex=1.15)
 text(4.43,3.55,'Person-hours needed',adj=0,col=grey,cex=1)
 text(4.43,3.13,'Applicant preparation\nReviewer work\nAdministration and repetition',adj=c(0,1),col=grey,cex=1)
 text(4.43,1.72,'Gross effort is not avoidable cost.',adj=0,col=grey,cex=.9)
 text(8.28,4.83,'Qualified initially unselected share',adj=0,col=grey,cex=.92)
 text(8.28,4.23,expression(frac(Q-A,Q)),adj=0,col=navy,cex=1.15)
 text(8.28,3.55,'Recorded concentration\nScore ties and dispersion',adj=c(0,1),col=grey,cex=.98)
 text(8.28,2.68,'Decision discrimination\nDisagreement and decision sensitivity',adj=c(0,1),col=grey,cex=.91)
 text(8.28,1.72,'Saturation alone is not compression.',adj=0,col=grey,cex=.86)
 text(.4,.86,'N evaluated applications; A awards or initial selections; Q qualified applications; K precommitted capacity.',adj=0,cex=.94,col=grey)
 text(.4,.45,'Match population, year and stage before combining indicators. Country coverage must be demonstrated.',adj=0,cex=.94,col=teal)
})
export('fig6_escape_routes',function(){
 canvas('Institutional escape routes','Hypothesised functions and trade-offs; no optimal allocation weights are implied')
 text(.55,5.65,'Mechanism',adj=0,font=2,col=navy);text(3.6,5.65,'Function sought',adj=0,font=2,col=teal);text(7.65,5.65,'Risk to evaluate',adj=0,font=2,col=orange)
 mech<-c('Stable core support','Targeted competition','Protected early career','High-risk or novelty channel','Partial lottery')
 good<-c('Continuity outside repeated contests','Selection where comparison adds value','Time to build an independent programme','Room for exploratory ideas and failure','Less fine ranking above a quality threshold')
 bad<-c('Entrenchment; hidden internal contests','Priority conformity; repeated application costs','Eligibility cliffs; another selection boundary','Strategic novelty claims; hard accountability','Screening costs; lost useful ranking information')
 for(i in 1:5){y<-5.14-(i-1)*.73;rect(.4,y-.33,11.6,y+.31,col=if(i%%2) pale else 'white',border=NA);text(.55,y,mech[i],adj=0,cex=1,col=navy);text(3.6,y,good[i],adj=0,cex=.91,col=grey);text(7.65,y,bad[i],adj=0,cex=.86,col=grey)}
 rect(.4,.65,11.6,1.52,col='#E8F4F3',border=NA)
 text(.6,1.23,'Across the portfolio: shorter or staged applications, simpler procedures and reusable material',adj=0,col=teal,font=2,cex=1)
 text(.6,.87,'Evaluate retained selection information, entry responses, delay and costs displaced into institutions.',adj=0,col=grey,cex=.97)
})

export('fig3_msca_qualification',function(){
 d<-read.csv(file.path(out,'../data','msca_msca_reconstructed.csv'))
 d<-d[d$unit=='HE_PF_TOTAL' & d$year %in% c(2024,2025),];d<-d[order(d$year),]
 par(mfrow=c(1,2),mar=c(5,4.5,4,1),oma=c(1.8,0,2,0),family='sans',col.axis=grey,col.lab=navy)
 values<-rbind(d$A,d$Q-d$A,d$N-d$Q)
 b<-barplot(values/1000,names.arg=d$year,col=c(teal,'#9CCDC8','#D6DEE4'),border=NA,ylim=c(0,20),ylab='Evaluated proposals (thousands)',las=1,cex.names=1.05)
 text(b,d$N/1000+.48,format(d$N,big.mark=',',trim=TRUE),font=2,col=navy,cex=1)
 text(b,(d$A+(d$Q-d$A)/2)/1000,format(d$Q-d$A,big.mark=',',trim=TRUE),font=2,col=navy,cex=1)
 text(b,d$A/2000,format(d$A,big.mark=',',trim=TRUE),col='white',cex=.9)
 title('a  Same cohort, different categories',adj=0,col.main=navy,cex.main=1.1)
 legend('topleft',c('Below reported threshold','Qualified, initially unselected','Initially recommended'),fill=c('#D6DEE4','#9CCDC8',teal),bty='n',cex=.78,inset=c(-.03,-.01))
 b<-barplot(rbind(100*d$Q/d$N,100*d$A/d$N),beside=TRUE,names.arg=d$year,col=c('#9CCDC8',teal),border=NA,ylim=c(0,112),ylab='Share of evaluated proposals (%)',las=1,cex.names=1.05)
 val<-rbind(100*d$Q/d$N,100*d$A/d$N)
 text(b,val+4,sprintf('%.2f%%',val),col=navy,cex=.94)
 title('b  Qualification versus initial selection',adj=0,col.main=navy,cex.main=1.08)
 legend('bottom',c('Above threshold','Initially recommended'),fill=c('#9CCDC8',teal),horiz=TRUE,bty='n',cex=.78,inset=c(0,-.22),xpd=NA)
 mtext('MSCA Postdoctoral Fellowships: 2024 and 2025 calls',outer=TRUE,side=3,line=.35,adj=.04,col=navy,font=2,cex=1.2)
 mtext('Administrative threshold 70/100; initial recommendations are not final awards or all-source support.',outer=TRUE,side=1,line=.3,adj=.04,col=grey,cex=.82)
})

export('fig4_score_and_decision_patterns',function(){
 n<-read.csv(file.path(out,'../data','nih_nih_anchor_metrics.csv'))
 tw<-read.csv(file.path(out,'../data','taiwan_taiwan_pre_post_metrics.csv'));tw<-tw[tw$metric=='Proposal share in tie groups >=5',]
 par(mfrow=c(1,2),mar=c(5.2,4.3,4,1.2),oma=c(1.6,0,2,0),family='sans',col.axis=grey,col.lab=navy)
 plot(NA,xlim=c(0,30),ylim=c(.4,2.65),xlab='Source percentile rank',ylab='',yaxt='n',axes=FALSE)
 axis(1,at=seq(0,30,5));axis(2,at=c(2,1),labels=n$year,las=1);graphics::box(bty='l',col='#CAD4DB')
 for(i in 1:2){y<-3-i;segments(n$p90_crossing_percentile[i],y,n$p10_crossing_percentile[i],y,col='#9BAFBC',lwd=5);points(c(n$p90_crossing_percentile[i],n$p10_crossing_percentile[i]),c(y,y),pch=c(17,15),col=grey,cex=1.05);points(n$p50_crossing_percentile[i],y,pch=19,col=teal,cex=1.5);text(n$p50_crossing_percentile[i],y+.22,sprintf('50%%: %.2f',n$p50_crossing_percentile[i]),cex=.85,col=teal);text(15,y-.27,sprintf('Transition width: %.2f points',n$transition_width_90_to_10[i]),cex=.82,col=grey)}
 title('a  NIH decision mapping',adj=0,col.main=navy,cex.main=1.15)
 legend('topright',c('90% crossing','50% crossing','10% crossing'),pch=c(17,19,15),col=c(grey,teal,grey),bty='n',cex=.75)
 v<-100*c(tw$pre_share,tw$post_share);b<-barplot(v,names.arg=c('2015-2019','2020-2023'),col=c('#9BAFBC',teal),border=NA,ylim=c(0,68),ylab='Proposals in tie groups of at least five (%)',las=1,cex.names=.9)
 text(b,v+3,sprintf('%.2f%%',v),col=navy,font=2,cex=1.1);text(b,6,c('392 / 1,071','471 / 954'),col='white',cex=.9)
 title('b  Taiwan recorded concentration',adj=0,col.main=navy,cex.main=1.1)
 mtext('Rubric and rank correction changed in 2020',side=3,line=.3,col=orange,cex=.8)
 mtext('Different measurement problems in two funding systems',outer=TRUE,side=3,line=.35,adj=.04,col=navy,font=2,cex=1.2)
 mtext('Descriptive comparisons; the NIH segment is not a confidence interval and the Taiwan contrast is not causal.',outer=TRUE,side=1,line=.25,adj=.04,col=grey,cex=.79)
})

export('figS1_nsf_pca_stability',function(){
 d<-read.csv(file.path(out,'../data','nsf_baseline_matrix_and_scores.csv'));d<-d[order(d$rank),]
 boot<-read.csv(file.path(out,'../data','pca_bootstrap_bootstrap_rank_summary.csv'));boot<-boot[match(d$unit,boot$unit),]
 par(mfrow=c(1,2),mar=c(4.8,4.6,3.8,1),oma=c(1.8,0,2,0),family='sans',col.axis=grey,col.lab=navy)
 y<-10:1;lab<-sub('NSF_','',d$unit)
 plot(NA,xlim=c(-2.35,2.1),ylim=c(.4,10.6),yaxt='n',xlab='Preserved PC1 score',ylab='',axes=FALSE);axis(1);axis(2,at=y,labels=lab,las=1);abline(v=0,col='#AABAC4');segments(0,y,d$PC1,y,col='#9CCDC8',lwd=8);points(d$PC1,y,pch=19,col=teal,cex=.9);text(d$PC1,y+.25,sprintf('%.3f',d$PC1),cex=.73,col=navy);graphics::box(bty='l',col='#CAD4DB');title('a  Original exploratory component',adj=0,col.main=navy,cex.main=1.05)
 plot(NA,xlim=c(.6,10.4),ylim=c(.4,10.6),yaxt='n',xlab='Rank across composition resamples',ylab='',axes=FALSE);axis(1,at=1:10);axis(2,at=y,labels=lab,las=1);abline(v=1:10,col='#E8EDF0');segments(boot$all_rank_p025,y,boot$all_rank_p975,y,col='#9BAFBC',lwd=4);points(boot$point_rank,y,pch=19,col=teal,cex=1.2);graphics::box(bty='l',col='#CAD4DB');title('b  All 1,000 existing resamples',adj=0,col.main=navy,cex.main=1.05)
 mtext('NSF FY2023: preserved scores and rank sensitivity',outer=TRUE,side=3,line=.35,adj=.04,col=navy,font=2,cex=1.2)
 mtext('Ranges are 2.5th-97.5th resample percentiles, not population confidence intervals. All 52 contrast-axis cases are retained.',outer=TRUE,side=1,line=.25,adj=.04,col=grey,cex=.77)
})

export('figS2_portfolio_costs',function(){
 d<-read.csv(file.path(out,'../data','pure_costs_pure_mechanism_costs.csv'));d<-d[d$scenario=='central_central_equal',]
 p<-read.csv(file.path(out,'../data','portfolio_values_portfolio_sensitivity.csv'));p<-p[p$scenario=='central_central_equal',]
 cols<-c('preparation_hours','review_hours','administration_procedural_hours')
 rows<-rbind(d[d$mechanism=='competitive',cols],p[p$mix=='equal_five',cols],d[d$mechanism=='partial_lottery',cols],p[p$mix=='reference_70_15_5_5_5',cols],d[d$mechanism=='core_refugium',cols])
 labels<-c('Pure competitive','Equal five','Lottery-style','70/15/5/5/5','Pure core')
 par(mar=c(5.8,8,4.3,1.5),family='sans',col.axis=grey,col.lab=navy)
 b<-barplot(t(as.matrix(rows))/1000,horiz=TRUE,names.arg=labels,col=c('#9CCDC8',teal,'#9BAFBC'),border=NA,las=1,xlim=c(0,460),xlab='Assumed procedural hours (thousands)',cex.names=.95)
 totals<-rowSums(rows)/1000;text(totals+7,b,format(round(totals,1),trim=TRUE),adj=0,col=navy,font=2,cex=.94)
 legend('topright',c('Preparation','Written review','Administration / procedures'),fill=c('#9CCDC8',teal,'#9BAFBC'),bty='n',cex=.88)
 title('What a cost-only portfolio model counts',adj=0,col.main=navy,cex.main=1.3,line=2.1)
 mtext('Central effort and administration; equal unit sizes; common synthetic budget and horizon',side=3,line=.5,adj=0,col=grey,cex=.9)
 mtext('Scientific benefits are absent. Lower modelled cost does not establish a better funding system.',side=1,line=4.5,adj=0,col=orange,cex=.9)
})
