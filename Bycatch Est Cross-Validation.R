#Create Training and Test Data Sets for DGN
#Training Data includes n-1 samples
#Test Data includes *one* set
# 15 November 2016

 rm(list=ls())
 setwd("c:/carretta/noaa/net_mort/data")
 start.time <- date()
 
 library(randomForest)
 library(grid)
 library(gridExtra)
 library(rfPermute)
 
 # read LIFE table to calculate proportion of species that were DEAD or INJURED
 # used to calculate M&SI levels from total bycatch estimates
 
 life = read.csv("Lifedata.txt")
 life = life[which(substr(life$TripNumber, 1, 2)=="DN"),]  # only drift gillnet records
 # "DD" codes need to be treated as "DS" for analysis
 replace.DD=which(life$SpCd=="DD")
 life$SpCd[replace.DD]="DS"
  
# set initial species code
 spcode="BA"
 trees=500
 table.offset = 3
# import variable importance file to initialize
 load(paste(spcode, "rfPermute_Signif_Vars.RData"))
# assign most recent 5-yr period for SAR bycatch pooling
 most.recent.5yrs = year.range[(length(year.range)-4):length(year.range)]
 min.yr = min(year.range)
 max.yr = max(year.range)
 obs.Year <- seq(min.yr, max.yr, 1)
# define analysis periods
 per1 = seq(min.yr,2000,1)
 per2 = seq(2001,max.yr,1)
 per3 = most.recent.5yrs
# character names for 3 analysis periods
 p1.name = "1990-2000"
 p2.name = paste("2001-", max.yr, sep="")
 p3.name = paste(min(per3), "-", max(per3), sep="")
 
# DGN effort summary
  fishery.year = seq(1990,2015,1)
  effort.DGN <- c(4078,4778,4379,5442,4248,3673,3392,3039,3353,2634,1936,1665,1630,1467,1084,1075,1433,1241,1103,761,492,435,445,470,409,361)
  obs.sets.DGN <- c(178,470,596,728,759,572,421,692,587,526,444,339,360,298,223,225,266,204,149,101,59,85,83,175,97,74)
  unobs.sets.DGN = effort.DGN-obs.sets.DGN
  obs.cov = round(obs.sets.DGN/effort.DGN, 2)
 
 p1.index = match(per1, fishery.year)
 p2.index = match(per2, fishery.year)
 p3.index = match(per3, fishery.year)
 
# bootstrap function
 bootstrp.CV = function(x) { iter=9999
                             mean.boot = matrix(NA, iter) 
                             
                             for (i in 1:iter) { b = sample(x, length(x), replace=T)
                                                 mean.boot[i]=mean(b)  }
                             
                             boot.CV = sd(mean.boot)/mean(x) 
                             boot.CV                          }
 
# Loop through all species codes
 
 for (spp in 1:length(response.codes))  {
 
 spcode = response.codes[spp]
 
 load(paste(spcode, "rfPermute_Signif_Vars.RData"))
 
 if (spcode=="aSP") sp.name = "UNID. CORMORANT"
 if (spcode=="aVE") sp.name = "UNID. BIRD"
 if (spcode=="BA") sp.name = "MINKE WHALE"
 if (spcode=="BD") sp.name = "BAIRD'S BEAKED WHALE"
 if (spcode=="BP") sp.name = "FIN WHALE"
 if (spcode=="CC") sp.name = "LOGGERHEAD TURTLE"
 if (spcode=="CM") sp.name = "GREEN TURTLE"
 if (spcode=="DC") sp.name = "LEATHERBACK TURTLE"
 if (spcode=="DL") sp.name = "LONG-BEAKED COMMON DOLPHIN"
 if (spcode=="DS") sp.name = "SHORT-BEAKED COMMON DOLPHIN"
 if (spcode=="ER") sp.name = "GRAY WHALE"
 if (spcode=="EJ") sp.name = "STELLER'S SEA LION"
 if (spcode=="fGL") sp.name = "NORTHERN FULMAR"
 if (spcode=="GG") sp.name = "RISSO'S DOLPHIN"
 if (spcode=="GM") sp.name = "SHORT-FINNED PILOT WHALE"
 if (spcode=="KB") sp.name = "PYGMY SPERM WHALE"
 if (spcode=="LB") sp.name = "N. RIGHT WHALE DOLPHIN"
 if (spcode=="LO") sp.name = "PAC. WHITE-SIDED DOLPHIN"
 if (spcode=="LV") sp.name = "OLIVE RIDLEY TURTLE"
 if (spcode=="MA") sp.name = "N ELEPHANT SEAL"
 if (spcode=="MC") sp.name = "HUBB'S BEAKED WHALE"
 if (spcode=="MN") sp.name = "HUMPBACK WHALE"
 if (spcode=="MT") sp.name = "STEJNEGER'S BEAKED WHALE"
 if (spcode=="OO") sp.name = "KILLER WHALE"
 if (spcode=="PD") sp.name = "DALL'S PORPOISE"
 if (spcode=="PM") sp.name = "SPERM WHALE"
 if (spcode=="PU") sp.name = "UNID. PINNIPED"
 if (spcode=="SC") sp.name = "STRIPED DOLPHIN"
 if (spcode=="TT") sp.name = "BOTTLENOSE DOLPHIN"
 if (spcode=="UC") sp.name = "UNID. CETACEAN"
 if (spcode=="UM") sp.name = "UNID. MESOPLODON"
 if (spcode=="UW") sp.name = "UNID. WHALE"
 if (spcode=="UT") sp.name = "UNID. TURTLE"
 if (spcode=="ZC") sp.name = "CA SEA LION"
 if (spcode=="ZI") sp.name = "CUVIER'S BEAKED WHALE"
 if (spcode=="ZU") sp.name = "UNID. ZIPHIID"
 if (spcode=="beaked.whales") sp.name = "ALL BEAKED WHALES"
 if (spcode=="all.delphinoids") sp.name = "ALL DELPHINOIDS"
 
 spcol = which(names(analysis.data) %in% spcode)
 vars = signif.vars
 
# if length(signif.vars<=1) use variable pooling
# pool species with small sample sizes based on var results for larger taxonomic groups
# pinniped variables include combined significant variables for ZC and MA
 
 if (spcode%in%c("SC","TT","UC", "GG", "OO", "UC", "LO", "PU", "UO", "EJ", "UT", "LV", "CM", "aSP", "aVE")) vars = c(names(data[,use.vars]))
 if (spcode%in%c("UM","MT","BD","MC", "ZU", "KB", "ZI")) vars = c("lon","n.ping") # "use all.beaked.whales" vars
 if (spcode%in%c("BA", "BP", "ER", "MN", "UW")) vars = c("lat", "lon", "days")  # default set of vars based on season/area
 
 var.cols = which(names(analysis.data) %in% vars)
 obs.bycatch = tapply(data[,which(names(data)==spcode)], data$year, sum)
 year = data$year

 bycatch.all.set.obs = data[,which(names(data) %in% spcode)]
 bycatch.per1 = bycatch.all.set.obs[which(data$year %in% per1)]
 bycatch.per2 = bycatch.all.set.obs[which(data$year %in% per2)]
 bycatch.per3 = bycatch.all.set.obs[which(data$year %in% per3)]
 
 mean.predictions = data.frame()
 all.tree.predictions = data.frame()
 test.data.input = data.frame()
 tree.names = paste("T", seq(1:trees), sep="")

# BEGIN RANDOM FOREST MODELS
 
  set.seed(123)
  
 for (i in nrow(analysis.data):1) {
    
  training.data = analysis.data[-i,]
  test.data = analysis.data[i,]
  bycatch = training.data[,which(names(training.data)%in%spcode)]
  
  forest.model = randomForest(y=training.data[,spcol], x=training.data[,var.cols], ntree=trees, replace=T)
  predict.bycatch = predict(forest.model, test.data[,var.cols], predict.all=T)
  all.trees.pred = round(predict.bycatch$individual, 4) 
  
# all n tree predictions for test.data
  all.tree.predictions = rbind(all.tree.predictions, all.trees.pred)
  predicted = signif(predict.bycatch$aggregate, 6)
  mean.predictions = rbind(predicted, mean.predictions)
  
  test.data.temp = cbind(test.data$lat, test.data$lon, test.data[,var.cols], test.data[,spcol], predicted, all.trees.pred)
  test.data.input = rbind(test.data.temp, test.data.input)
  
  if (i %in% seq(1000,nrow(analysis.data),1000)) print(i)         }

#################### END RANDOM FOREST MODELS AND PREDICTION ###########################
  
# conf.interval functions
  
  L95 = function(x) { L95=quantile(x, 0.025) }
  U95 = function(x) { U95=quantile(x, 0.975) }
  
  mean.predictions = cbind(year, test.data.input)
  names(mean.predictions)=c("year", "lat", "lon", names(training.data[,var.cols]), spcode, "predicted", tree.names)
  indiv.tree.cols = which(names(mean.predictions) %in% tree.names)
  
  sum.all.tree.predictions = apply(all.tree.predictions, 2, sum)
  sum.all.tree.predictions
  
  # mean predicted bycatch rate by year (and confidence limits)
  bpue.annual.predicted = tapply(mean.predictions$predicted, mean.predictions$year, mean)
  
  # Estimated bycatch by year (bpue.annual.predicted * unobs.sets.DGN[match(obs.Year, fishery.year)]) + obs.bycatch
  annual.estimate = (bpue.annual.predicted * unobs.sets.DGN[match(obs.Year, fishery.year)])
  total.bycatch = annual.estimate + obs.bycatch
  
  sm = which(annual.estimate<1)
  med = which(annual.estimate>=1 & annual.estimate<10)
  lg = which(annual.estimate>=10)
  
  annual.estimate[sm] = round(annual.estimate[sm], 1)
  annual.estimate[med] = round(annual.estimate[med], 1)
  annual.estimate[lg] = round(annual.estimate[lg], 1)
  
  sm = which(total.bycatch<1)
  med = which(total.bycatch>=1 & total.bycatch<10)
  lg = which(total.bycatch>=10)
  
  total.bycatch[sm] = round(total.bycatch[sm],1)
  total.bycatch[med] = round(total.bycatch[med], 1)
  total.bycatch[lg] = round(total.bycatch[lg],1)
  
  obs.estimated = as.matrix(rbind(obs.bycatch, annual.estimate))
  
  # Calculate level of Mortality + Serious Injury using rbinom variate based on fraction of 
  # entanglements from observer data that were DEAD or seriously-injured
  
  life.index = match(life$TripNumber, analysis.data$trip)
  year.entangled = analysis.data$year[life.index]
  life.new = cbind(year.entangled, life)
  
  life.data.sp = life.new[life$SpCd==spcode,]
  dead = length(which(life.data.sp$Cond%in%c("D","I","U")))
  prob.msi = dead / nrow(life.data.sp)
  if (spcode=="MN") prob.msi = 0.25
  if (spcode=="beaked.whales") prob.msi = 32/33
  if (spcode=="all.delphinoids") prob.msi = 1
  if (spcode%in%c("DD", "DS", "DL", "GG", "LO", "LB", "TT", "PD", "SC", "UD", "UC", "OO", "GM")) prob.msi=1
  if (spcode%in%c("DD", "DS", "DL", "GG", "LO", "LB", "TT", "PD", "SC", "UD", "UC", "OO", "GM","all.delphinoids")) sp.group="delphinoid"
  if (spcode%in%c("BA","BP","ER","PM","ZC","EJ","PU","MA","CC","CM","DC","UT","aVE","aSP","fGL","UW","UC")) sp.group="other"
  if (spcode%in%c("beaked.whales")) sp.group="beakers"
  
  # This section distills *observed* MS&I (mortality and serious injury)
  # cases by year for use in prorating total bycatch to MSI
  
  life.index = match(life$TripNumber, analysis.data$trip)
  year.entangled = analysis.data$year[life.index]
  life.new = cbind(year.entangled, life, rep(1, nrow(life)))
  names(life.new)[ncol(life.new)]="Number"
  
  life.data.sp = life.new[life$SpCd==spcode,]
  if(spcode!=c("beaked.whales","all.delphinoids")) tab = table(life.new$year.entangled, life.new$SpCd==spcode & life.new$Cond!="A")
  
  if (sp.group=="delphinoid") msi.per.yr = obs.bycatch
  if (sp.group=="beakers") msi.per.yr = as.numeric(c(obs.bycatch[1:5], 5, obs.bycatch[7:length(year.range)]))
  if (spcode%in%c("LV","MN")) msi.per.yr = rep(0, length(year.range))
  if (sp.group=="other") msi.per.yr = tab[,2]
  
  # END section
  
  # calculate individual year uncertainty
  
  CV.annual.TREE = data.frame()
  CV.annual.RATIO = data.frame()
  CV.model = data.frame()
  CV.total.bycatch = data.frame()
  msi = data.frame()
  CV.msi = data.frame()
  
  for (j in 1:length(obs.Year)) { annual.data = mean.predictions[which(mean.predictions$year==obs.Year[j]),] 
  annual.trees = annual.data[,indiv.tree.cols]
  annual.sum.pred = apply(annual.trees, 2, sum)
  
  CV.model = sd(annual.sum.pred)/mean(annual.sum.pred)
  CV.temp.TREE = (CV.model * annual.estimate[j]) / total.bycatch[j]
  CV.temp.TREE = signif(CV.temp.TREE, 2)
  
  CV.temp.RATIO = bootstrp.CV(bycatch.all.set.obs[which(data$year==obs.Year[j])])
  CV.annual.RATIO = rbind(CV.annual.RATIO, CV.temp.RATIO)
  CV.annual.TREE = rbind(CV.annual.TREE, CV.temp.TREE)
  
  if (prob.msi==1) temp.msi = total.bycatch[j]
  if (prob.msi==1) sd.temp.msi = 0
  
  if (prob.msi<1 & prob.msi>0) sd.temp.msi = sd(apply(annual.data[,indiv.tree.cols], 2, mean) * unobs.sets.DGN[j] * rbinom(trees, sum(obs.bycatch), prob=prob.msi)/sum(obs.bycatch))
  if (prob.msi<1 & prob.msi>0) temp.msi = apply(annual.data[,indiv.tree.cols], 2, mean) * unobs.sets.DGN[j]  * rbinom(trees, sum(obs.bycatch), prob=prob.msi)/sum(obs.bycatch) + msi.per.yr[j] 
  if (spcode=="MN") temp.msi = apply(annual.data[,indiv.tree.cols], 2, mean) * unobs.sets.DGN[j]  * rbinom(trees, 4, prob=prob.msi)/4 + msi.per.yr[j]
  if (prob.msi==0) temp.msi = 0
  
  msi.temp=mean(temp.msi)
  if(msi.temp==0) CV.temp.msi=0
  if (msi.temp>0) CV.temp.msi = sd.temp.msi/mean(temp.msi)
  if(prob.msi==1) CV.temp.msi=CV.temp.TREE 
  if (annual.estimate[j]==0) CV.temp.msi = 0
  CV.msi = rbind(CV.msi, CV.temp.msi)
  msi = rbind(msi, msi.temp) 
  
  }
  
  CV.annual.TREE = unlist(CV.annual.TREE)
  CV.annual.RATIO = unlist(CV.annual.RATIO)
  CV.msi = unlist(CV.msi)
  msi=unlist(msi)
  msi = round(msi, 1)
  CV.annual.RATIO = signif(CV.annual.RATIO, 2)
  CV.annual.TREE = signif(CV.annual.TREE, 2)
  CV.msi = signif(CV.msi, 2)
  
  all.ann.predicted = rbind(obs.bycatch, total.bycatch, CV.annual.TREE, obs.sets.DGN[match(obs.Year, fishery.year)], obs.cov[match(obs.Year, fishery.year)])
  ratio.est = round(obs.bycatch * 1/obs.cov[match(obs.Year, fishery.year)], 1)
  all.ann.predicted = as.data.frame(cbind(obs.Year, obs.sets.DGN[match(obs.Year, fishery.year)], obs.cov[match(obs.Year, fishery.year)], obs.bycatch, annual.estimate, total.bycatch, CV.annual.TREE, ratio.est, CV.annual.RATIO, msi, CV.msi))
  names(all.ann.predicted)=c("Year","Obs.Sets","Obs.Cov","Obs.Bycatch","Tree.Est", "Bycatch.Tot", "CV.Bycatch.Tot", "Ratio.Est", "CV.Ratio.Est", "MSI", "CV.MSI")
  
  # Observed Set, Observer coverage, for 3 time periods
  
  p1.obs.sets = sum(obs.sets.DGN[which(obs.Year %in% per1)])
  p2.obs.sets = sum(obs.sets.DGN[which(obs.Year %in% per2)])
  p3.obs.sets = sum(obs.sets.DGN[which(obs.Year %in% per3)])
  
  p1.obs.cov = round(p1.obs.sets/(sum(effort.DGN[which(obs.Year %in% per1)])), 2)
  p2.obs.cov = round(p2.obs.sets/(sum(effort.DGN[which(obs.Year %in% per2)])), 2)
  p3.obs.cov = round(p3.obs.sets/(sum(effort.DGN[which(obs.Year %in% per3)])), 2)
  
  p1.obs = sum(obs.bycatch[which(obs.Year %in% per1)])
  p2.obs = sum(obs.bycatch[which(obs.Year %in% per2)])
  p3.obs = sum(obs.bycatch[which(obs.Year %in% per3)])
  
  ## sum.est.per1 = sum(annual.estimate[which(obs.Year %in% per1)])
  sum.est.per1 = mean(mean.predictions$predicted[which(mean.predictions$year %in% per1)]) * sum(unobs.sets.DGN[p1.index])
  total.bycatch.per1 = sum.est.per1 + sum(obs.bycatch[p1.index])
  sum.est.per2 = mean(mean.predictions$predicted[which(mean.predictions$year %in% per2)]) * sum(unobs.sets.DGN[p2.index])
  total.bycatch.per2 = sum.est.per2 + sum(obs.bycatch[p2.index])
  sum.est.per3 = mean(mean.predictions$predicted[which(mean.predictions$year %in% per3)]) * sum(unobs.sets.DGN[p3.index])
  total.bycatch.per3 = sum.est.per3 + sum(obs.bycatch[p3.index])
  
  ratio.per1 = round(p1.obs / p1.obs.cov, 1)
  ratio.per2 = round(p2.obs / p2.obs.cov, 1)
  ratio.per3 = round(p3.obs / p3.obs.cov, 1)
  
  # parse pre-2001 data
  data.per1 = mean.predictions[which(mean.predictions$year %in% per1),]
  ind.trees.per1 = apply(data.per1[,indiv.tree.cols], 2, sum)
  CV.per1 = sd(ind.trees.per1) / mean(ind.trees.per1)
  CV.per1 = (CV.per1 * sum.est.per1) / (p1.obs + sum.est.per1)
  
  if(prob.msi<1 & prob.msi>0) msi.per1 = apply(data.per1[,indiv.tree.cols], 2, mean) * sum(unobs.sets.DGN[p1.index]) * rbinom(trees, sum(obs.bycatch), prob=prob.msi)/sum(obs.bycatch) + sum(msi.per.yr[p1.index]) 
  if(prob.msi==1) msi.per1 = total.bycatch.per1
  if(prob.msi==0) msi.per1 = 0 
  CV.msi.per1 = sd(msi.per1)/mean(msi.per1)
  if(prob.msi==1) CV.msi.per1=CV.per1 
  CV.msi.per1=round(CV.msi.per1, 2)
  
  # parse post-2000 data
  data.per2 = mean.predictions[which(mean.predictions$year>2000),]
  ind.trees.per2 = apply(data.per2[,indiv.tree.cols], 2, sum)
  CV.per2 = sd(ind.trees.per2) / mean(ind.trees.per2)
  CV.per2 = (CV.per2 * sum.est.per2) / (p2.obs + sum.est.per2)
  if(prob.msi==1) msi.per2 = total.bycatch.per2
  if(prob.msi<1 & prob.msi>0) msi.per2 = apply(data.per2[,indiv.tree.cols], 2, mean) * sum(unobs.sets.DGN[p2.index]) * rbinom(trees, sum(obs.bycatch), prob=prob.msi)/sum(obs.bycatch) + sum(msi.per.yr[p2.index]) 
  if(prob.msi==0) msi.per2=0
  
  CV.msi.per2 = sd(msi.per2)/mean(msi.per2)
  if(prob.msi==1) CV.msi.per2=CV.per2
  CV.msi.per2=round(CV.msi.per2, 2)
  
  # parse most recent 5 yrs of data
  data.per3 = mean.predictions[which(mean.predictions$year%in%most.recent.5yrs),]
  ind.trees.per3 = apply(data.per3[,indiv.tree.cols], 2, sum)
  CV.per3 = sd(ind.trees.per3) / mean(ind.trees.per3)
  CV.per3 = (CV.per3 * sum.est.per3) / (p3.obs + sum.est.per3)
  
  all.ann.predicted$Tree.Est = round(all.ann.predicted$Tree.Est, 2)
  if(prob.msi<1 & prob.msi>0) msi.per3 = apply(data.per3[,indiv.tree.cols], 2, mean) * sum(unobs.sets.DGN[p3.index]) * rbinom(trees, sum(obs.bycatch), prob=prob.msi)/sum(obs.bycatch) + sum(msi.per.yr[p3.index]) 
  if(prob.msi==0) msi.per3 = 0 
  if(prob.msi==1) msi.per3 = total.bycatch.per3
  CV.msi.per3 = sd(msi.per3)/mean(msi.per3)
  if(prob.msi==1) CV.msi.per3=CV.per3
  CV.msi.per3=round(CV.msi.per3, 2)
  
  CV.per1 = round(CV.per1, 2)
  CV.per2 = round(CV.per2, 2)
  CV.per3 = round(CV.per3, 2)
  
  pdf(paste(spcode, "Bycatch.Est.pdf"), width=10, height=8)
  
  barplot(obs.estimated, obs.Year, col=c("red","green"), ylab="Estimated Bycatch", xlab="Year", main=paste(sp.name, "Regression Tree Bycatch Estimates"))
  legend.type = c("observed", "estimated (observed + estimated)")
  legend(40000, max(obs.estimated), legend.type, bty="n", col=c("red","green"), fill=c(col=c("red","green")))
  abline(h=1, lty=3)
  
  mtext(paste(p1.name, ": ", round(sum.est.per1 + p1.obs ,1), " ± ", round(CV.per1 * (sum.est.per1 + p1.obs), 1), sep="")
        , side=3, line=0)
  
  mtext(paste(p2.name, ": ", round(sum.est.per2 + p2.obs, 1),  " ± ", round(CV.per2 * (sum.est.per2 + p2.obs), 1), sep="")
        , side=3, line=-1)
  
  dev.off()
  
  bpue = (obs.bycatch/obs.sets.DGN[c(p1.index,p2.index)])
  tiff(paste(sp.name, "BPUE OBS PRED.TIF"), width=5, height=4, units="in", res=600)
  bp=barplot(bpue, obs.Year, ylab="Bycatch per set", xlab="Year", main=paste(sp.name, "n=", pos.events))
  legend.type = c("Observed", "Estimated")
  legend(20000, max(bpue), legend.type, pch=c(15,20), col=c("gray","black"), bty="n")
  lines(bp, bpue.annual.predicted)
  points(bp, bpue.annual.predicted, pch=20)
  
  dev.off()
  bpue = (obs.bycatch/obs.sets.DGN[c(p1.index,p2.index)])
  bp=barplot(bpue, obs.Year, ylab="Bycatch per set", xlab="Year", main=paste(sp.name, "n=", pos.events))
  legend.type = c("Observed", "Estimated")
  legend(20000, max(bpue), legend.type, pch=c(15,20), col=c("gray","black"), bty="n")
  lines(bp, bpue.annual.predicted)
  points(bp, bpue.annual.predicted, pch=20)
  dev.off()
  
  # add multi-year data to all.ann.predicted
  
  CV.ratio.per1 = round(bootstrp.CV(bycatch.per1), 2)
  CV.ratio.per2 = round(bootstrp.CV(bycatch.per2), 2)
  CV.ratio.per3 = round(bootstrp.CV(bycatch.per3), 2)
  
  p1.summary = cbind(p1.name, p1.obs.sets, p1.obs.cov, p1.obs, round(sum.est.per1, 1), round(sum(total.bycatch.per1), 1), signif(CV.per1,2), round(ratio.per1,1), CV.ratio.per1, round(mean(msi.per1),1), signif(CV.msi.per1,2))
  p2.summary = cbind(p2.name, p2.obs.sets, p2.obs.cov, p2.obs, round(sum.est.per2, 1), round(sum(total.bycatch.per2), 1), signif(CV.per2,2), round(ratio.per2,1), CV.ratio.per2, round(mean(msi.per2),1), signif(CV.msi.per2,2))
  p3.summary = cbind(p3.name, p3.obs.sets, p3.obs.cov, p3.obs, round(sum.est.per3, 1), round(sum(total.bycatch.per3), 1), signif(CV.per3,2), round(ratio.per3,1), CV.ratio.per3, round(mean(msi.per3),1), signif(CV.msi.per3,2))
  
  multi.yr.summary = as.data.frame(rbind(p1.summary, p2.summary, p3.summary))
  names(multi.yr.summary)=names(all.ann.predicted)
  
  # add multi-year data to summary table
  all.ann.predicted = rbind(all.ann.predicted, " ", multi.yr.summary)
  all.ann.predicted = as.matrix(all.ann.predicted)
  missing = which(all.ann.predicted=="NaN")
  all.ann.predicted[missing]="-"
  bycatch.zero=which(all.ann.predicted[,6]==0)
  all.ann.predicted[bycatch.zero,7]="-"
  MSI.zero = which(all.ann.predicted[,10]==0)
  all.ann.predicted[MSI.zero,10]=0
  CV.MSI.zero = which(all.ann.predicted[,11]==0)
  CV.MSI.na = which(is.na(all.ann.predicted[,11]=="TRUE"))
  all.ann.predicted[bycatch.zero,11]="-"
  all.ann.predicted[CV.MSI.na,11]="-"
  
  # multi-line text needs to be of similar lengths to left-justify format
  
  # "Year","Obs.Sets","Obs.Cov","Obs.Bycatch","Tree.Est", "Bycatch.Tot", "CV.Bycatch.Tot", "MSI", "Ratio.Est", "CV.Ratio.Est"
  
  text1 = paste("Table ", spp+table.offset, ". Observed sets (Obs.Sets), observer coverage (Obs.Cov), and observed bycatch (Obs.Bycatch) for ", sp.name, ". Unobserved bycatch", sep="")
  text2 = "estimated from regression trees (Tree.Est) and total bycatch (Bycatch.Tot) are shown with corresponding ratio estimates (Ratio.Est). Total mortality and serious injury (MSI) is"
  text3 = "based on prorating Bycatch.Tot by the fraction of observations resulting in death or serious injury. The probability that a bycatch event resulted in mortality or serious injury"
  text4 = paste("was", signif(prob.msi, 2), "for this species. Precision for all estimates are given as CVs.")
    
  pdf(paste("Table", spp+table.offset, sp.name, "Bycatch Estimates.pdf"), height=12, width=14)
  grid.table(all.ann.predicted, rows=NULL)
  grid.text(text1, x=0.05, just="left", vjust=-45)
  grid.text(text2, x=0.05, just="left", vjust=-43)
  grid.text(text3, x=0.05, just="left", vjust=-41)
  grid.text(text4, x=0.05, just="left", vjust=-39)
  dev.off()   
  
  save.image(paste(spcode, "Bycatch.Est.RData")) }
 
 end.time = date()
 start.time; end.time
 