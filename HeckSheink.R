#re-create Heckman Scheinkman exercise with NLSY

library(foreign)
library(readstata13)
library(data.table)
library(ivpack)
library(xtable)

setwd("~/Documents/CurrResearch/SkillBundling")

nlsy <- read.dta13("yearly_03.dta", convert.factors = F)
 
nlsy <- data.table(nlsy)
setkeyv(nlsy,c("id","year"))


nlsy <- nlsy[ age>=25 & age<=45,]
nlsy[ , rwage := rwage/100] #convert from cents to dollars

#setup sequence number:
nlsy[ rwage>0, seqno:= year-min(year), by=id]
nlsy[ , hs := grade>=12]
nlsy[ , lths := grade<12]
nlsy[ , univ := grade>=16]

#subdivide sample of wages
nlsy[ , next7.lwage  := shift(lwage,7 ,type="lead"), by=id]
nlsy[ , last7.lwage  := shift(lwage,7 ,type="lag" ), by=id]
nlsy[ , next14.lwage := shift(lwage,14,type="lead"), by=id]
nlsy[ , last14.lwage := shift(lwage,14,type="lag" ), by=id]

nlsy[           seqno<7 , lwage0 := lwage]
nlsy[ seqno>=7 & seqno<14, lwage1 := lwage]
nlsy[ seqno>14& seqno<21, lwage2 := lwage]

nlsy[           seqno<7 , lwage1 := next7.lwage]
nlsy[ seqno>=14& seqno<21, lwage1 := last7.lwage]

nlsy[ seqno>=7 & seqno<14, lwage0 := last7.lwage]
nlsy[ seqno>=14& seqno<21, lwage0 := last14.lwage]

nlsy[          seqno<7 , lwage2 := next14.lwage]
nlsy[ seqno>=7& seqno<14, lwage2 := next7.lwage]

#for levels
nlsy[ , next7.rwage  := shift(rwage,7 ,type="lead"), by=id]
nlsy[ , last7.rwage  := shift(rwage,7 ,type="lag" ), by=id]
nlsy[ , next14.rwage := shift(rwage,14,type="lead"), by=id]
nlsy[ , last14.rwage := shift(rwage,14,type="lag" ), by=id]

nlsy[           seqno<7 , rwage0 := rwage]
nlsy[ seqno>=7 & seqno<14, rwage1 := rwage]
nlsy[ seqno>=14& seqno<21, rwage2 := rwage]

nlsy[           seqno<7 , rwage1 := next7.rwage]
nlsy[ seqno>=14& seqno<21, rwage1 := last7.rwage]

nlsy[ seqno>=7 & seqno<14, rwage0 := last7.rwage]
nlsy[ seqno>=14& seqno<21, rwage0 := last14.rwage]

nlsy[          seqno<7 , rwage2 := next14.rwage]
nlsy[ seqno>=7& seqno<14, rwage2 := next7.rwage]


#generage skills stuff:
nlsy[ , afqt := asvab_sec02 + asvab_sec03 + asvab_sec04 + asvab_sec05/2]
#renumber asvab:
nlsy[, asvab_sec05:= asvab_sec01]
nlsy[, asvab_sec01:= asvab_sec02]
nlsy[, asvab_sec02:= asvab_sec08]
nlsy[, asvab_sec06:= asvab_sec09]
nlsy[, asvab_sec07:= asvab_sec10]

nlsy[, enterage := min(age -(year-1980)) ,by=id]

sd20 <- array(1.,9)
for( seci in seq(1,7)){
	sd20[seci] = nlsy[ enterage==20, var(eval(as.name(paste0("asvab_sec0",seci))),na.rm = T)^.5]
}
sd20[8] = nlsy[ enterage==20, var(rosenberg_score,na.rm = T)^.5]
sd20[9] = nlsy[ enterage==20, var(rotter_score,na.rm = T)^.5]

nlsy[ , asvab_sec01 := (asvab_sec01-mean(asvab_sec01,na.rm=T))*sd20[1]/var(asvab_sec01,na.rm=T)^.5, by=enterage]
nlsy[ , asvab_sec02 := (asvab_sec02-mean(asvab_sec02,na.rm=T))*sd20[2]/var(asvab_sec02,na.rm=T)^.5 , by=enterage]
nlsy[ , asvab_sec03 := (asvab_sec03-mean(asvab_sec03,na.rm=T))*sd20[3]/var(asvab_sec03,na.rm=T)^.5 , by=enterage]
nlsy[ , asvab_sec04 := (asvab_sec04-mean(asvab_sec04,na.rm=T))*sd20[4]/var(asvab_sec04,na.rm=T)^.5 , by=enterage]
nlsy[ , asvab_sec05 := (asvab_sec05-mean(asvab_sec05,na.rm=T))*sd20[5]/var(asvab_sec05,na.rm=T)^.5 , by=enterage]
nlsy[ , asvab_sec06 := (asvab_sec06-mean(asvab_sec06,na.rm=T))*sd20[6]/var(asvab_sec06,na.rm=T)^.5 , by=enterage]
nlsy[ , asvab_sec07 := (asvab_sec07-mean(asvab_sec07,na.rm=T))*sd20[7]/var(asvab_sec07,na.rm=T)^.5 , by=enterage]

nlsy[ , rosenberg_score := (rosenberg_score-mean(rosenberg_score,na.rm=T))*sd20[8]/var(rosenberg_score,na.rm=T)^.5 , by=enterage]
nlsy[ , rotter_score    := (rotter_score   -mean(rotter_score,na.rm=T))   *sd20[9]/var(rotter_score   ,na.rm=T)^.5 , by=enterage]

nlsy[ , afqt:= (afqt-mean(afqt,na.rm=T))/var(afqt,na.rm=T)^.5 , by=enterage]

# verbal skills 
pc_verbal = prcomp( subset(nlsy,select=c("asvab_sec03","asvab_sec04")))
nlsy[ , skill_verbal := pc_verbal$x[,1]]
if( nlsy[,cor(skill_verbal,asvab_sec03)]<0 ){
	nlsy[ , skill_verbal := -(skill_verbal - mean(skill_verbal,na.rm=T))/var(skill_verbal,na.rm=T)^.5]
}else{
	nlsy[ , skill_verbal := (skill_verbal - mean(skill_verbal,na.rm=T))/var(skill_verbal,na.rm=T)^.5]
}

# math skills
pc_math  = prcomp( subset(nlsy,select=c("asvab_sec01","asvab_sec02")))
nlsy[ , skill_math := pc_math$x[,1]]
if( nlsy[,cor(skill_math,asvab_sec01)]<0 ){
	nlsy[ , skill_math := -(skill_math - mean(skill_math,na.rm=T))/var(skill_math,na.rm=T)^.5]
}else{
	nlsy[ , skill_math := (skill_math - mean(skill_math,na.rm=T))/var(skill_math,na.rm=T)^.5]
}

# social skills
pc_social  = prcomp( subset(nlsy,select=c("rosenberg_score","rotter_score")))
nlsy[ , skill_social := pc_social$x[,1]]
if( nlsy[,cor(skill_social,rosenberg_score)]<0 ){
	nlsy[ , skill_social := -(skill_social - mean(skill_social,na.rm=T))/var(skill_social,na.rm=T)^.5]
}else{
	nlsy[ , skill_social := (skill_social - mean(skill_social,na.rm=T))/var(skill_social,na.rm=T)^.5]
}

nlsy[  ,switch_occHL :=  (occ_1d<4 & shift(occ_1d)>=4) | (occ_1d>=4 & shift(occ_1d)<4), by=id]
nlsy[  ,anyswitchHL :=  any( switch_occHL==1,na.rm=T ), by=id]

nlsy[  ,startL :=  any(occ_1d<4 & seqno==7,na.rm=T), by=id]
nlsy[  ,endL   :=  any(occ_1d<4 & seqno==14,na.rm=T), by=id]

nlsy[  ,startH :=  any(occ_1d>=4 & seqno==7,na.rm=T) , by=id]
nlsy[  ,endH   :=  any(occ_1d>=4 & seqno==14,na.rm=T), by=id]
nlsy[  ,anyswitchHL_H2L :=  any( (occ_1d>=4 & shift(occ_1d)<4),na.rm=T ) , by=id]
nlsy[  ,anyswitchHL_L2H :=  any( (occ_1d<4 & shift(occ_1d)>=4),na.rm=T ), by=id]


nlsy[  ,anyswitch :=  any( switch_occ==1,na.rm=T ), by=id]


#
# main regressions ################
#

#wage levels instead of logs. We are measuring prices, not returns
summary(nlsy[ seqno>=7 & seqno<14, lm(rwage~rwage0+shift(rwage0)+shift(rwage0,2)+skill_verbal+skill_math+skill_social + lths + univ)])
summary(nlsy[ seqno>=7 & seqno<14, ivreg(rwage~rwage0+shift(rwage0)+shift(rwage0,2)+skill_verbal+skill_math+skill_social  + lths + univ| 
										 	rwage2+shift(rwage2)+shift(rwage2,2)+skill_verbal+skill_math+skill_social  + lths + univ)])

#on occupation stayers, only
#nlsy[ , anyswitch:= any(switch_occ==1), by=id]
summary(nlsy[ seqno>=7 & seqno<14 & anyswitchHL==F, lm(rwage~rwage0+shift(rwage0)+shift(rwage0,2)+skill_verbal+skill_math+skill_social + lths + univ)])
summary(nlsy[ seqno>=7 & seqno<14 & anyswitchHL==F, ivreg(rwage~rwage0+shift(rwage0)+shift(rwage0,2)+skill_verbal+skill_math+skill_social  + lths + univ|
															rwage2+shift(rwage2)+shift(rwage2,2)+skill_verbal+skill_math+skill_social + lths + univ)])

#separate for occupations:
OLSHS_occH <- nlsy[occ_1d<4  & seqno>=7 & seqno<14, lm(rwage~rwage0+shift(rwage0)+shift(rwage0,2)+skill_verbal+skill_math+skill_social + lths + univ)]
OLSHS_occL <- nlsy[occ_1d>=4 & seqno>=7 & seqno<14, lm(rwage~rwage0+shift(rwage0)+shift(rwage0,2)+skill_verbal+skill_math+skill_social + lths + univ)]
HS_occH <- nlsy[ occ_1d<4  &seqno>=7 & seqno<14, ivreg(rwage~rwage0+shift(rwage0)+shift(rwage0,2)+skill_verbal+skill_math+skill_social  + lths + univ| 
													 	rwage2+shift(rwage2)+shift(rwage2,2)+skill_verbal+skill_math+skill_social  + lths + univ)]
HS_occL <- nlsy[ occ_1d>=4 &seqno>=7 & seqno<14, ivreg(rwage~rwage0+shift(rwage0)+shift(rwage0,2)+skill_verbal+skill_math+skill_social  + lths + univ| 
													   	rwage2+shift(rwage2)+shift(rwage2,2)+skill_verbal+skill_math+skill_social  + lths + univ)]

#encompassing model:
OLSHS_occHL <- nlsy[ seqno>=7 & seqno<14, lm(rwage~rwage0+shift(rwage0)+shift(rwage0,2)+skill_verbal+skill_math+skill_social + lths + univ + 
										   	I(occ_1d<4)*(rwage0+shift(rwage0)+shift(rwage0,2)+skill_verbal+skill_math+skill_social + lths + univ))]
OLSHS_occ <- nlsy[ seqno>=7 & seqno<14, lm(rwage~rwage0+shift(rwage0)+shift(rwage0,2)+skill_verbal+skill_math+skill_social + lths + univ + 
											 	factor(occ_1d)*(rwage0+shift(rwage0)+shift(rwage0,2)+skill_verbal+skill_math+skill_social + lths + univ))]
OLSHS     <- nlsy[ seqno>=7 & seqno<14, lm(rwage~rwage0+shift(rwage0)+shift(rwage0,2)+skill_verbal+skill_math+skill_social + lths + univ)]

IVHS      <- nlsy[ seqno>=7 & seqno<14, ivreg(rwage~rwage0+shift(rwage0)+shift(rwage0,2)+skill_verbal+skill_math+skill_social + lths + univ | 
											 	rwage2+ shift(rwage2)+shift(rwage2,2) +skill_verbal+skill_math+skill_social + lths + univ)]

IVHS_occHL  <- nlsy[ seqno>=7 & seqno<14, ivreg(rwage~rwage0+shift(rwage0)+shift(rwage0,2)+skill_verbal+skill_math+skill_social + lths + univ +
											  	I(occ_1d<4)*(rwage0+shift(rwage0)+shift(rwage0,2)+skill_verbal+skill_math+skill_social + lths + univ)| 
											 	rwage2+ shift(rwage2)+shift(rwage2,2) +skill_verbal+skill_math+skill_social + lths + univ+ 
											  	I(occ_1d<4)*(rwage2+shift(rwage2)+shift(rwage2,2)+skill_verbal+skill_math+skill_social + lths + univ) )]

IVHS_stayHL    <- nlsy[ seqno>=7 & seqno<14 & anyswitchHL==F, ivreg(rwage~rwage0+shift(rwage0)+shift(rwage0,2)+skill_verbal+skill_math+skill_social + lths + univ | 
											  	rwage2+ shift(rwage2)+shift(rwage2,2) +skill_verbal+skill_math+skill_social + lths + univ)]

IVHS_stay    <- nlsy[ seqno>=7 & seqno<14 & anyswitch   ==F, ivreg(rwage~rwage0+shift(rwage0)+shift(rwage0,2)+skill_verbal+skill_math+skill_social + lths + univ | 
												rwage2+ shift(rwage2)+shift(rwage2,2) +skill_verbal+skill_math+skill_social + lths + univ)]

IVHS_switch  <- nlsy[ seqno>=7 & seqno<14 & anyswitch   ==T, ivreg(rwage~rwage0+shift(rwage0)+shift(rwage0,2)+skill_verbal+skill_math+skill_social + lths + univ | 
											   	rwage2+ shift(rwage2)+shift(rwage2,2) +skill_verbal+skill_math+skill_social + lths + univ)]


IVHS_stayHL_occHL<- nlsy[ seqno>=7 & seqno<14 & anyswitchHL==F, ivreg(rwage~rwage0+shift(rwage0)+shift(rwage0,2)+skill_verbal+skill_math+skill_social + lths + univ +
											  	I(occ_1d<4)*(rwage0+shift(rwage0)+shift(rwage0,2)+skill_verbal+skill_math+skill_social + lths + univ)| 
											  	rwage2+ shift(rwage2)+shift(rwage2,2) +skill_verbal+skill_math+skill_social + lths + univ+ 
											  	I(occ_1d<4)*(rwage2+shift(rwage2)+shift(rwage2,2)+skill_verbal+skill_math+skill_social + lths + univ) )]

IVHS_stayHL_occHLalpha<- nlsy[ seqno>=7 & seqno<14 & anyswitchHL==F, ivreg(rwage~rwage0+shift(rwage0)+shift(rwage0,2)+skill_verbal+skill_math+skill_social + lths + univ +
											  	I(occ_1d<4)*(rwage0+shift(rwage0)+shift(rwage0,2))| 
											  	rwage2+ shift(rwage2)+shift(rwage2,2) +skill_verbal+skill_math+skill_social + lths + univ+ 
											  	I(occ_1d<4)*(rwage2+shift(rwage2)+shift(rwage2,2)) )]

IVHS_stayHL_occHLbeta<- nlsy[ seqno>=7 & seqno<14 & anyswitchHL==F, ivreg(rwage~rwage0+shift(rwage0)+shift(rwage0,2)+skill_verbal+skill_math+skill_social + lths + univ +
											  	I(occ_1d<4)*(skill_verbal+skill_math+skill_social + lths + univ)| 
											  	rwage2+ shift(rwage2)+shift(rwage2,2) +skill_verbal+skill_math+skill_social + lths + univ+ 
											  	I(occ_1d<4)*(skill_verbal+skill_math+skill_social + lths + univ) )]

IVHS_switchHL    <- nlsy[ seqno>=7 & seqno<14 & anyswitchHL==T, ivreg(rwage~rwage0+shift(rwage0)+shift(rwage0,2)+skill_verbal+skill_math+skill_social + lths + univ | 
												rwage2+ shift(rwage2)+shift(rwage2,2) +skill_verbal+skill_math+skill_social + lths + univ)]

IVHS_switchHL_occHL<- nlsy[ seqno>=7 & seqno<14 & anyswitchHL==T, ivreg(rwage~rwage0+shift(rwage0)+shift(rwage0,2)+skill_verbal+skill_math+skill_social + lths + univ +
												I(occ_1d<4)*(rwage0+shift(rwage0)+shift(rwage0,2)+skill_verbal+skill_math+skill_social + lths + univ)| 
												rwage2+ shift(rwage2)+shift(rwage2,2) +skill_verbal+skill_math+skill_social + lths + univ+ 
												I(occ_1d<4)*(rwage2+shift(rwage2)+shift(rwage2,2)+skill_verbal+skill_math+skill_social + lths + univ) )]


IVHS_occ  <- nlsy[ seqno>=7 & seqno<14, ivreg(rwage~rwage0+shift(rwage0)+shift(rwage0,2)+skill_verbal+skill_math+skill_social + lths + univ +
												factor(occ_1d)*(rwage0+shift(rwage0)+shift(rwage0,2)+skill_verbal+skill_math+skill_social + lths + univ)| 
												rwage2+ shift(rwage2)+shift(rwage2,2) +skill_verbal+skill_math+skill_social + lths + univ+ 
												factor(occ_1d)*(rwage2+shift(rwage2)+shift(rwage2,2)+skill_verbal+skill_math+skill_social + lths + univ) )]

IVHS_stay_occ  <- nlsy[ seqno>=7 & seqno<14 & anyswitch==F, ivreg(rwage~rwage0+shift(rwage0)+shift(rwage0,2)+skill_verbal+skill_math+skill_social + lths + univ +
											  	factor(occ_1d)*(rwage0+shift(rwage0)+shift(rwage0,2)+skill_verbal+skill_math+skill_social + lths + univ)| 
											  	rwage2+ shift(rwage2)+shift(rwage2,2) +skill_verbal+skill_math+skill_social + lths + univ+ 
											  	factor(occ_1d)*(rwage2+shift(rwage2)+shift(rwage2,2)+skill_verbal+skill_math+skill_social + lths + univ) )]

IVHS_switch_occ<- nlsy[ seqno>=7 & seqno<14 & anyswitch==T, ivreg(rwage~rwage0+shift(rwage0)+shift(rwage0,2)+skill_verbal+skill_math+skill_social + lths + univ +
											  	factor(occ_1d)*(rwage0+shift(rwage0)+shift(rwage0,2)+skill_verbal+skill_math+skill_social + lths + univ)| 
											  	rwage2+ shift(rwage2)+shift(rwage2,2) +skill_verbal+skill_math+skill_social + lths + univ+ 
											  	factor(occ_1d)*(rwage2+shift(rwage2)+shift(rwage2,2)+skill_verbal+skill_math+skill_social + lths + univ) )]


IVHS_stay_occalpha<- nlsy[ seqno>=7 & seqno<14 & anyswitch==F, ivreg(rwage~rwage0+shift(rwage0)+shift(rwage0,2)+skill_verbal+skill_math+skill_social + lths + univ +
											  	factor(occ_1d)*(rwage0+shift(rwage0)+shift(rwage0,2))| 
											  	rwage2+ shift(rwage2)+shift(rwage2,2) +skill_verbal+skill_math+skill_social + lths + univ+ 
											  	factor(occ_1d)*(rwage2+shift(rwage2)+shift(rwage2,2)) )]

IVHS_stay_occbeta  <- nlsy[ seqno>=7 & seqno<14 & anyswitch==F, ivreg(rwage~rwage0+shift(rwage0)+shift(rwage0,2)+skill_verbal+skill_math+skill_social + lths + univ +
											  	factor(occ_1d)*(skill_verbal+skill_math+skill_social + lths + univ)| 
											  	rwage2+ shift(rwage2)+shift(rwage2,2) +skill_verbal+skill_math+skill_social + lths + univ+ 
											  	factor(occ_1d)*(skill_verbal+skill_math+skill_social + lths + univ) )]

alphabeta0_HL <- waldtest( IVHS_stayHL_occHL,IVHS_stayHL, test="F"  )
beta0_HL      <- waldtest( IVHS_stayHL_occHL, IVHS_stayHL_occHLalpha, test="F"  )
alpha0_HL     <- waldtest( IVHS_stayHL_occHL,IVHS_stayHL_occHLbeta, test="F"  )

alphabeta0 <- waldtest( IVHS_stay_occ,IVHS_stay, test="F"  )
beta0      <- waldtest( IVHS_stay_occ,IVHS_stay_occalpha, test="F"  )
alpha0     <- waldtest( IVHS_stay_occ,IVHS_stay_occbeta, test="F"  )

alphabeta_test <- cbind(c(alphabeta0_HL$F[2], alphabeta0_HL$`Pr(>F)`[2],beta0_HL$F[2], beta0_HL$`Pr(>F)`[2],alpha0_HL$F[2], alpha0_HL$`Pr(>F)`[2]),
						  c(alphabeta0$F[2], alphabeta0$`Pr(>F)`[2],beta0$F[2], beta0$`Pr(>F)`[2],alpha0$F[2], alpha0$`Pr(>F)`[2]))

rnames <- c("F\ stat",     "P\ Val",
			"F\ stat\ ",   "P\ Val\ ",
			"F\ stat\ \ ", "P\ Val\ \ ")
rownames(alphabeta_test) <-rnames
colnames(alphabeta_test) <- c("2 Occupation", "6 Occupation")
rowtitles <- list( pos=list(0,2,4), command=c("\\hline  Observed and unobserved & &  \\\\ \n",
											  "\\hline \\hline   Observed       & &  \\\\  \n", 
											  "\\hline \\hline   Unobserved     & &  \\\\  \n")  )
tab_alphabetatest <- xtable(alphabeta_test, digits=2, 
					   align="l|ll", caption=paste0("Weak price-equalization test \\label{weaktestH0}"))

print(tab_alphabetatest,include.rownames=T, hline.after= c(nrow(tab_alphabetatest)), 
	add.to.row=rowtitles, file="HS_p0stayer.tex"  )
	

waldtest( IVHS_switchHL_occHL,IVHS_switchHL, test="F"  )
waldtest( IVHS_switch_occ,IVHS_switch, test="F"  )


# Combinations of origin destination for movers:
IVHS_switchH2L<- nlsy[ seqno>=7 & seqno<14 & anyswitchHL_H2L==T , ivreg(rwage~rwage0+shift(rwage0)+shift(rwage0,2)+skill_verbal+skill_math+skill_social + lths + univ +
																			(rwage0+shift(rwage0)+shift(rwage0,2)+skill_verbal+skill_math+skill_social + lths + univ)| 
																			rwage2+ shift(rwage2)+shift(rwage2,2) +skill_verbal+skill_math+skill_social + lths + univ+ 
																			(rwage2+shift(rwage2)+shift(rwage2,2)+skill_verbal+skill_math+skill_social + lths + univ) )]
IVHS_switchL2H<- nlsy[ seqno>=7 & seqno<14 & anyswitchHL_L2H==T , ivreg(rwage~rwage0+shift(rwage0)+shift(rwage0,2)+skill_verbal+skill_math+skill_social + lths + univ +
																		  	(rwage0+shift(rwage0)+shift(rwage0,2)+skill_verbal+skill_math+skill_social + lths + univ)| 
																		  	rwage2+ shift(rwage2)+shift(rwage2,2) +skill_verbal+skill_math+skill_social + lths + univ+ 
																		  	(rwage2+shift(rwage2)+shift(rwage2,2)+skill_verbal+skill_math+skill_social + lths + univ) )]

IVHS_switchH2L_occHL<- nlsy[ seqno>=7 & seqno<14 & anyswitchHL_H2L==T , ivreg(rwage~rwage0+shift(rwage0)+shift(rwage0,2)+skill_verbal+skill_math+skill_social + lths + univ +
																		  	I(occ_1d<4)*(rwage0+shift(rwage0)+shift(rwage0,2)+skill_verbal+skill_math+skill_social + lths + univ)| 
																		  	rwage2+ shift(rwage2)+shift(rwage2,2) +skill_verbal+skill_math+skill_social + lths + univ+ 
																		  	I(occ_1d<4)*(rwage2+shift(rwage2)+shift(rwage2,2)+skill_verbal+skill_math+skill_social + lths + univ) )]

IVHS_switchL2H_occHL<- nlsy[ seqno>=7 & seqno<14 & anyswitchHL_L2H==T , ivreg(rwage~rwage0+shift(rwage0)+shift(rwage0,2)+skill_verbal+skill_math+skill_social + lths + univ +
																			  	I(occ_1d<4)*(rwage0+shift(rwage0)+shift(rwage0,2)+skill_verbal+skill_math+skill_social + lths + univ)| 
																			  	rwage2+ shift(rwage2)+shift(rwage2,2) +skill_verbal+skill_math+skill_social + lths + univ+ 
																			  	I(occ_1d<4)*(rwage2+shift(rwage2)+shift(rwage2,2)+skill_verbal+skill_math+skill_social + lths + univ) )]

IVHS_switchL2H_occHLalpha<- nlsy[ seqno>=7 & seqno<14 & anyswitchHL_L2H==T, ivreg(rwage~rwage0+shift(rwage0)+shift(rwage0,2)+skill_verbal+skill_math+skill_social + lths + univ +
																	 	I(occ_1d<4)*(rwage0+shift(rwage0)+shift(rwage0,2))| 
																	 	rwage2+ shift(rwage2)+shift(rwage2,2) +skill_verbal+skill_math+skill_social + lths + univ+ 
																	 	I(occ_1d)*(rwage2+shift(rwage2)+shift(rwage2,2)) )]

IVHS_switchL2H_occHLbeta  <- nlsy[ seqno>=7 & seqno<14 & anyswitchHL_L2H==T, ivreg(rwage~rwage0+shift(rwage0)+shift(rwage0,2)+skill_verbal+skill_math+skill_social + lths + univ +
																	  	I(occ_1d<4)*(skill_verbal+skill_math+skill_social + lths + univ)| 
																	  	rwage2+ shift(rwage2)+shift(rwage2,2) +skill_verbal+skill_math+skill_social + lths + univ+ 
																	  	I(occ_1d<4)*(skill_verbal+skill_math+skill_social + lths + univ) )]

IVHS_switchH2L_occHLalpha<- nlsy[ seqno>=7 & seqno<14 & anyswitchHL_H2L==T, ivreg(rwage~rwage0+shift(rwage0)+shift(rwage0,2)+skill_verbal+skill_math+skill_social + lths + univ +
																	  	I(occ_1d<4)*(rwage0+shift(rwage0)+shift(rwage0,2))| 
																	  	rwage2+ shift(rwage2)+shift(rwage2,2) +skill_verbal+skill_math+skill_social + lths + univ+ 
																	  	I(occ_1d<4)*(rwage2+shift(rwage2)+shift(rwage2,2)) )]

IVHS_switchH2L_occHLbeta  <- nlsy[ seqno>=7 & seqno<14 & anyswitchHL_H2L==T, ivreg(rwage~rwage0+shift(rwage0)+shift(rwage0,2)+skill_verbal+skill_math+skill_social + lths + univ +
																	   	I(occ_1d<4)*(skill_verbal+skill_math+skill_social + lths + univ)| 
																	   	rwage2+ shift(rwage2)+shift(rwage2,2) +skill_verbal+skill_math+skill_social + lths + univ+ 
																	   	I(occ_1d<4)*(skill_verbal+skill_math+skill_social + lths + univ) )]

alphabeta0_H2L <- waldtest( IVHS_switchH2L_occHL, IVHS_switchH2L, test="F"  )
beta0_H2L      <- waldtest( IVHS_switchH2L_occHL, IVHS_switchH2L_occHLalpha, test="F"  )
alpha0_H2L     <- waldtest( IVHS_switchH2L_occHL, IVHS_switchH2L_occHLbeta, test="F"  )

alphabeta0_L2H <- waldtest( IVHS_switchL2H_occHL, IVHS_switchL2H, test="F"  )
beta0_L2H      <- waldtest( IVHS_switchL2H_occHL, IVHS_switchL2H_occHLalpha, test="F"  )
alpha0_L2H     <- waldtest( IVHS_switchL2H_occHL, IVHS_switchL2H_occHLbeta, test="F"  )

alphabeta_strongtest <- cbind(c(alphabeta0_H2L$F[2], alphabeta0_H2L$`Pr(>F)`[2],beta0_H2L$F[2], beta0_H2L$`Pr(>F)`[2],alpha0_H2L$F[2], alpha0_H2L$`Pr(>F)`[2]),
							  c(alphabeta0_L2H$F[2], alphabeta0_L2H$`Pr(>F)`[2],beta0_L2H$F[2], beta0_L2H$`Pr(>F)`[2],alpha0_L2H$F[2], alpha0_L2H$`Pr(>F)`[2]))

rnames <- c("F\ stat",     "P\ Val",
			"F\ stat\ ",   "P\ Val\ ",
			"F\ stat\ \ ", "P\ Val\ \ ")
rownames(alphabeta_strongtest) <-rnames
colnames(alphabeta_strongtest) <- c("High -> Low", "Low -> High")
rowtitles <- list( pos=list(0,2,4), command=c("\\hline  Observed and unobserved & &  \\\\ \n",
											  "\\hline \\hline   Observed       & &  \\\\  \n", 
											  "\\hline \\hline   Unobserved     & &  \\\\  \n")  )
tab_alphabeta_strongtest <- xtable(alphabeta_strongtest, digits=2, 
							align="l|ll", caption=paste0("Strong price-equalization test \\label{strongtestH0}"))

print(tab_alphabeta_strongtest,include.rownames=T, hline.after= c(nrow(tab_alphabeta_strongtest)), 
	  add.to.row=rowtitles, file="HS_p0switcher.tex"  )


# Recover skill price series ------------

IVHS_t      <- nlsy[ seqno>=7 & seqno<14, ivreg(rwage~rwage0+shift(rwage0)+shift(rwage0,2) + year*(rwage0+shift(rwage0)+shift(rwage0,2)) +
													skill_verbal+skill_math+skill_social + lths + univ +age + I(age^2)| 
											  	rwage2+ shift(rwage2)+shift(rwage2,2) + year*(rwage2+ shift(rwage2)+shift(rwage2,2)) +
													skill_verbal+skill_math+skill_social + lths + univ +age + I(age^2))]

