# 25 October 2016

library(rfPermute)
library(randomForest)
library(gridExtra)
library(grid)

rm(list=ls())
start = date()
setwd("c:/carretta/noaa/net_mort/data")

# this is the full dataset, only some of which may be used  
data = read.csv("DGN.Current.Data.csv") 
real.vars <- c("days", "mei.index", "sst", "lat", "lon", "drift.km", "depth.p", "soak", "n.ping", "length.net", "height.net", "mesh", "extnd", "slope")

# which years to generate variable importance for?   
year.range = seq(1990,2015,1)
data = data[which(data$year%in%c(year.range)),]

first.sp = which(names(data)=="BA")
last.sp = which(names(data)=="all.delphinoids")
response.codes = names(data[first.sp:last.sp])

# eliminate response.codes with all zero observations
species = data[,first.sp:last.sp]
sum.species = apply(species, 2, sum)
empty = which(sum.species==0)
response.codes = response.codes[-empty]
response.is.factor=1

analysis.data = data

use.vars = which(names(analysis.data)%in%real.vars)

for (i in 1:length(response.codes))  {
  
spcode=response.codes[i]

# convert numeric response to factor if factor.response=1

bycatch=analysis.data[,which(names(analysis.data)==response.codes[i])]

if(response.is.factor==1) bycatch[which(bycatch==0)]="N"
if(response.is.factor==1) bycatch[which(bycatch!="N")]="Y"
if(response.is.factor==1) bycatch=as.factor(bycatch)

pos.events = length(which(bycatch=="Y"))

rfp = rfPermute(bycatch ~ ., analysis.data[,use.vars], ntree=10000, sampsize=c(pos.events, pos.events), nrep=200)

importance = as.data.frame(rp.importance(rfp, sort.by = NULL, decreasing = TRUE))
signif.vars = which(importance[,8]<=0.05)
signif.vars = row.names(importance)[signif.vars]

pdf(paste(spcode, "rfPermuteSignifVars.pdf"), width=10, height=8)
plot(rp.importance(rfp), scale=T) 

save.image(paste(spcode, "rfPermute_Signif_Vars.RData"))
dev.off()                                                }

### write output table containing species codes and variables selected
  add.cols = 3 # Num of columns to add to table beyond number of variables used
  ncols = length(use.vars)+add.cols
  mat1 = matrix(NA, length(response.codes), ncols)
  num.vars.id = matrix(NA, length(response.codes))
  num.entangled = matrix(NA, length(response.codes))
  

  for (j in 1:length(response.codes)) { load(paste(response.codes[j], "rfPermute_Signif_Vars.RData"))
    
    if (spcode=="aSP") sci.name = "Phalacrocorax sp., UNID. CORMORANT"
    if (spcode=="aVE") sci.name = "UNID. BIRD"
    if (spcode=="BA") sci.name = "Balaenoptera acutorostrata, MINKE WHALE"
    if (spcode=="BD") sci.name = "Berardius bairdii, BAIRD'S BEAKED WHALE"
    if (spcode=="BP") sci.name = "Balaenoptera physalus, FIN WHALE"
    if (spcode=="CC") sci.name = "Caretta caretta, LOGGERHEAD SEA TURTLE"
    if (spcode=="CM") sci.name = "Chelonia mydas, GREEN SEA TURTLE"
    if (spcode=="DC") sci.name = "Dermochelys coriacea, LEATHERBACK SEA TURTLE"
    if (spcode=="DL") sci.name = "Delphinus capensis, LONG-BEAKED COMMON DOLPHIN"
    if (spcode=="DS") sci.name = "Delphinus delphis, SHORT-BEAKED COMMON DOLPHIN"
    if (spcode=="ER") sci.name = "Eschrichtius robustus, GRAY WHALE"
    if (spcode=="EJ") sci.name = "Eumetopias jubatus, STELLER'S SEA LION"
    if (spcode=="fGL") sci.name = "Fulmarus glacialis, NORTHERN FULMAR"
    if (spcode=="GG") sci.name = "Grampus griseus, RISSO'S DOLPHIN"
    if (spcode=="GM") sci.name = "Globicephala macrorhynchus, SHORT-FINNED PILOT WHALE"
    if (spcode=="KB") sci.name = "Kogia breviceps, PYGMY SPERM WHALE"
    if (spcode=="LB") sci.name = "Lissodelphis borealis, NORTHERN RIGHT WHALE DOLPHIN"
    if (spcode=="LO") sci.name = "Lagenorhynchus obliquidens, PACIFIC WHITE-SIDED DOLPHIN"
    if (spcode=="LV") sci.name = "Lepidochelys olivacea, OLIVE RIDLEY SEA TURTLE"
    if (spcode=="MA") sci.name = "Mirounga angustirostris, NORTHERN ELEPHANT SEAL"
    if (spcode=="MC") sci.name = "Mesoplodon hubbsi, HUBB'S BEAKED WHALE"
    if (spcode=="MN") sci.name = "Megatptera novaeangliae, HUMPBACK WHALE"
    if (spcode=="MT") sci.name = "Mesoplodon stejnegeri, STEJNEGER'S BEAKED WHALE"
    if (spcode=="OO") sci.name = "Orcinus orca, KILLER WHALE"
    if (spcode=="PD") sci.name = "Phocoenoides dalli, DALL'S PORPOISE"
    if (spcode=="PM") sci.name = "Physeter macrocephalus, SPERM WHALE"
    if (spcode=="PU") sci.name = "UNID. PINNIPED"
    if (spcode=="SC") sci.name = "Stenella coeruleoalba, STRIPED DOLPHIN"
    if (spcode=="TT") sci.name = "Tursiops truncatus, BOTTLENOSE DOLPHIN"
    if (spcode=="UC") sci.name = "UNID. CETACEAN"
    if (spcode=="UM") sci.name = "Mesopldon sp., UNID. MESOPLODON"
    if (spcode=="UW") sci.name = "UNID. WHALE"
    if (spcode=="UT") sci.name = "UNID. TURTLE"
    if (spcode=="ZC") sci.name = "Zalophus californianus, CALIFORNIA SEA LION"
    if (spcode=="ZI") sci.name = "Ziphius cavirostris, CUVIER'S BEAKED WHALE"
    if (spcode=="ZU") sci.name = "Ziphiid sp., UNID. ZIPHIID"
    if (spcode=="beaked.whales") sci.name = "ALL BEAKED WHALES"
    if (spcode=="all.delphinoids") sci.name = "ALL DELPHINOIDS"
    
    lv = length(signif.vars)
    nsv = length(real.vars)-lv
    num.entangled[j]=sum(analysis.data[,which(names(analysis.data)==response.codes[j])])
    if (lv==0) sv=c(sci.name, pos.events, num.entangled[j], rep(NA, length(real.vars)))
    if (lv>0)  sv=c(sci.name, pos.events, num.entangled[j], signif.vars, rep(NA, nsv)) 
    
    mat1[j,] = c(sv)
    num.vars.id[j] = lv         }
  
  mat1[which(is.na(mat1==TRUE))] = "-"
  mat1 = data.frame(mat1)
  names(mat1)=c("Species", paste("Entanglement", "Events", sep=" "), paste("Number", "Animals", sep=" "),  paste("Variable ", seq(1:length(use.vars)), sep=""))
 
  mat1[,2] = as.numeric(as.character((mat1[,2])))
  mat1 = mat1[order(mat1[,2], decreasing=TRUE),]
  mat1 = mat1[,1:(max(num.vars.id)+add.cols)]
  
  # only write table with rows where sample size >=4
  mat1 = mat1[which(mat1[,2]>3),]
  write.csv(mat1, "Vars_Bycatch_Models.csv", row.names=FALSE)
  
  pdf("Vars_Bycatch_Models.pdf", width=14, height=9)
  grid.table(mat1, rows=NULL)
  dev.off()
  finish = date()
  start; finish
  
