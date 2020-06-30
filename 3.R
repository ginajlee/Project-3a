# A)
library(ggplot2)
library(readxl) 
library(dplyr)
library(maps)
library(RColorBrewer)

cnty = map_data("county")
gusa = map_data("state")

cnty2 = cnty %>% mutate(polyname=paste(region,subregion, sep=",")) %>%
  left_join(county.fips, by="polyname")

chci <- read.csv("chci.csv")
colnames(chci)[2]="fips"

cnty3 = cnty2 %>% left_join(chci, by="fips")

qt1=quantile(cnty3$chci09, probs=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), na.rm=T)
cnty3$chci09a =cut(cnty3$chci09, breaks=qt1, labels=paste(qt1[-1]))

state_layer=geom_polygon(aes(long,lat,group=group), fill=NA, data=gusa,color = "black") 

pdf("3A.pdf")

ggplot(cnty3, aes(long,lat,group=group)) + 
  geom_polygon(aes(fill = chci09a), colour = rgb(1,1,1,0.2)) + coord_quickmap() +
  scale_fill_brewer(palette = "RdBu") + state_layer

dev.off()

##

wdi <- data.frame(read_excel("W03b_wdi.xlsx"))

wdi.le = subset(wdi, Indicator.Code == "SP.DYN.LE00.IN")
wdi.po = subset(wdi, Indicator.Code == "SP.POP.TOTL")
wdi.fr = subset(wdi, Indicator.Code == "SP.DYN.TFRT.IN")

wdi3v.60a=merge(wdi.le[,c("Country.Name","Country.Code","X1960")], wdi.po[,c("Country.Code","X1960")], by="Country.Code")
wdi3v.60b=merge(wdi3v.60a, wdi.fr[,c("Country.Code","X1960")], by="Country.Code")

wdi3v.60=wdi3v.60b[!wdi3v.60b$Country.Code %in% c("ARB",	"CSS",	"CEB",	"EAR",	"EAS",	"EAP",	"TEA",	"EMU",	"ECS",
                                                  "ECA",	"TEC",	"EUU",	"FCS",	"HPC",	"HIC",	"IBD",	"IBT",	"IDB",	
                                                  "IDX",	"IDA",	"LTE",	"LCN",	"LAC",	"TLA",	"LDC",	"LMY",	"LIC",	
                                                  "LMC",	"MEA",	"MNA",	"TMN",	"MIC",	"NAC",	"INX",	"OED",	"OSS",	
                                                  "PSS",	"PST",	"PRE",	"SST",	"SAS",	"TSA",  "SSF",	"SSA",	"TSS",
                                                  "UMC",	"WLD"),]

colnames(wdi3v.60)=c("code", "country", "life","pop","fr")
p6=ggplot(wdi3v.60, aes(x=life, y=fr, size=pop)) + geom_point(alpha=0.4) + scale_size_continuous(range=c(0.5, 20)) 
p6
