load("./VaccineData.Rdata")

#Summary Table
meanTable <- as.data.frame(WA_KING_zctas) %>%
  summarize(popDen = mean(popDens,na.rm=TRUE),
            percPov = mean(hhPercPov,na.rm=TRUE),
            percAsian = mean(racePercAsian,na.rm=TRUE),
            percBlack = mean(racePercBlack,na.rm=TRUE),
            percHispanic = mean(racePercHisp,na.rm=TRUE),
            percAIAN = mean(racePercAIAN,na.rm=TRUE),
            percNHPI = mean(racePercNHPI,na.rm=TRUE),
            percCollege = mean(percCollegeGrad,na.rm=TRUE),
            percNoVehicles = mean(percNoVehicles,na.rm=TRUE),
            accTeen = mean(accTeen,na.rm=TRUE),
            accChild = mean(accChild,na.rm=TRUE),
            coverTeen = mean(coverTeen,na.rm=TRUE),
            coverChild = mean(coverChild,na.rm=TRUE)) %>%
  t()
sdTable <- as.data.frame(WA_KING_zctas) %>%
  summarize(popDen = sd(popDens,na.rm=TRUE),
            percPov = sd(hhPercPov,na.rm=TRUE),
            percAsian = sd(racePercAsian,na.rm=TRUE),
            percBlack = sd(racePercBlack,na.rm=TRUE),
            percHispanic = sd(racePercHisp,na.rm=TRUE),
            percAIAN = sd(racePercAIAN,na.rm=TRUE),
            percNHPI = sd(racePercNHPI,na.rm=TRUE),
            percCollege = sd(percCollegeGrad,na.rm=TRUE),
            percNoVehicles = sd(percNoVehicles,na.rm=TRUE),
            accTeen = sd(accTeen,na.rm=TRUE),
            accChild = sd(accChild,na.rm=TRUE),
            coverTeen = sd(coverTeen,na.rm=TRUE),
            coverChild = sd(coverChild,na.rm=TRUE)) %>%
  t()  
summTable <- cbind(meanTable,sdTable) 
colnames(summTable) <- c("Mean", "SD")
rownames(summTable) <- c("Population Density","Percent Households Below the Poverty Line",
                         "Percent Asian Residents","Percent Black Residents","Percent Hispanic Residents",
                         "Percent AI/AN Residents", "Percent NHPI Residents",
                         "Percent Adults with Bachelors or Higher Degree","Percent No Vehicles",
                         "Vaccine Accessibility - Adolescent","Vaccine Accessibility - School Age",
                         "Vaccine Coverage - Adolescent","Vaccine Coverage - School Age")

knitr::kable(summTable,format="html",digits=1,row.names = TRUE)

#Paired mean differences
mean(WA_KING_zctas$accTeen-WA_KING_zctas$accChild,na.rm=TRUE)
mean(WA_KING_zctas$coverTeen-WA_KING_zctas$coverChild,na.rm=TRUE)

#Paired t-test for accessibility and coverage
t.test(WA_KING_zctas$accChild,WA_KING_zctas$accTeen,paired = TRUE)
t.test(WA_KING_zctas$coverChild,WA_KING_zctas$coverTeen,paired = TRUE)

#Correlation analysis for accessibility and coverage
cor(WA_KING_zctas$accChild,WA_KING_zctas$accTeen,use="complete.obs")
cor(WA_KING_zctas$coverChild,WA_KING_zctas$coverTeen,use="complete.obs")

#Set up for spatial analysis
nb<-poly2nb(WA_KING_zctas)
lw <- nb2listw(nb,style="W",zero.policy = TRUE) #Neighbor weights, adjacency based, equal

#Moran testing to check if there is spatial autoregression for coverage
miTeen<-moran.mc(x = WA_KING_zctas$coverTeen,listw = lw,na.action = na.omit,zero.policy = TRUE,nsim=9999)
miChild<-moran.mc(x = WA_KING_zctas$coverChild,listw = lw,na.action = na.omit,zero.policy = TRUE,nsim=9999)

WA_KING_zctas$popDenScale <- WA_KING_zctas$popDens/1E3 #Scale pop density to be per 1000/sq mi for reporting


logit <- function(p){
  return(log(p/(1-p)))
}

#VIF Check
corrTesting <- lm(coverTeen ~ accTeen +
                    log10(popDens) +
                    hhPercPov +
                    racePercBlack +
                    racePercAsian +
                    racePercHisp +
                    racePercAIAN + 
                    racePercNHPI +
                    percCollegeGrad,
                  data = WA_KING_zctas)
car::vif(corrTesting)

corrTesting_Child <- lm(coverChild ~ accChild +
                          log10(popDens) +
                          hhPercPov +
                          racePercBlack +
                          racePercAsian +
                          racePercHisp +
                          racePercAIAN + 
                          racePercNHPI +
                          percCollegeGrad,
                        data = WA_KING_zctas)
car::vif(corrTesting_Child)


corrTesting_noPopDens <- lm(coverTeen ~ accTeen +
                              hhPercPov +
                              racePercBlack +
                              racePercAsian +
                              racePercHisp +
                              racePercAIAN + 
                              racePercNHPI +
                              percCollegeGrad,
                            data = WA_KING_zctas)
car::vif(corrTesting_noPopDens)

corrTesting_Child_noPD <- lm(coverChild ~ accChild +
                               hhPercPov +
                               racePercBlack +
                               racePercAsian +
                               racePercHisp +
                               racePercAIAN + 
                               racePercNHPI +
                               percCollegeGrad,
                             data = WA_KING_zctas)
car::vif(corrTesting_Child_noPD)

#Interaction Checking
intDensModel <- errorsarlm(coverTeen ~ accTeen * log10(popDens) +
                             log10(popDens) +
                             hhPercPov +
                             racePercBlack +
                             racePercAsian +
                             racePercHisp +
                             racePercAIAN + 
                             racePercNHPI +
                             percCollegeGrad,
                           data = WA_KING_zctas,listw = lw,zero.policy = TRUE)

intPovModel <- errorsarlm(coverTeen ~ accTeen * hhPercPov +
                            log10(popDens) +
                            hhPercPov +
                            racePercBlack +
                            racePercAsian +
                            racePercHisp +
                            racePercAIAN + 
                            racePercNHPI +
                            percCollegeGrad,
                          data = WA_KING_zctas,listw = lw,zero.policy = TRUE)

intCollegeModel <- errorsarlm(coverTeen ~ accTeen * percCollegeGrad +
                                log10(popDens) +
                                hhPercPov +
                                racePercBlack +
                                racePercAsian +
                                racePercHisp +
                                racePercAIAN + 
                                racePercNHPI +
                                percCollegeGrad,
                              data = WA_KING_zctas,listw = lw,zero.policy = TRUE)

summary(
  errorsarlm(coverTeen ~ accTeen * percCollegeGrad +
               hhPercPov +
               racePercBlack +
               racePercAsian +
               racePercHisp +
               racePercAIAN + 
               racePercNHPI +
               percCollegeGrad,
             data = WA_KING_zctas,listw = lw,zero.policy = TRUE)
)
#Make the range of values the heat map will test over
acc.seq <- seq(70,170,length.out=101)
pov.seq <- seq(0,15,length.out=101)
dens.seq <- 10^seq(0,5,length.out=101) #Log-space transform
col.seq <- seq(0,100,length.out=101)

#Houeshold Poverty Interaction
pov.grid <- data.frame(expand.grid(acc.seq,pov.seq))
colnames(pov.grid) <- c("acc","pov")
pov.predFcn <- function(acc,pov){ #Prediction function which uses mean values for variables not interacting
  predict(
    intPovModel,
    data.frame(
      accTeen = acc,
      hhPercPov = pov,
      popDens = mean(WA_KING_zctas$popDens),
      racePercBlack = mean(WA_KING_zctas$racePercBlack,na.rm=TRUE),
      racePercAsian = mean(WA_KING_zctas$racePercAsian,na.rm=TRUE),
      racePercHisp = mean(WA_KING_zctas$racePercHisp,na.rm=TRUE),
      racePercAIAN = mean(WA_KING_zctas$racePercAIAN,na.rm=TRUE),
      racePercNHPI = mean(WA_KING_zctas$racePercNHPI,na.rm=TRUE),
      percCollegeGrad = mean(WA_KING_zctas$percCollegeGrad,na.rm=TRUE)
    )
  )  
} 

pov.grid$cov <- pov.predFcn(pov.grid$acc,pov.grid$pov)[,1]
ggplot(data=pov.grid) +
  geom_tile(aes(
    acc,
    pov,
    fill=cov
  )) +
  scale_fill_gradient(low="red",high="yellow") +
  labs(x="Access",y="Household Poverty (%)",fill="Predicted Coverage (%)") +
  geom_point(data=filter(WA_KING_zctas,accTeen>70),
             aes(
               x=accTeen,
               y=hhPercPov
             )
  )


#Population Density
dens.grid <- data.frame(expand.grid(acc.seq,dens.seq))
colnames(dens.grid) <- c("acc","dens")
dens.predFcn <- function(acc,dens){
  predict(
    intDensModel,
    data.frame(
      accTeen = acc,
      popDens = dens,
      hhPercPov = mean(WA_KING_zctas$hhPercPov,na.rm=TRUE),
      racePercBlack = mean(WA_KING_zctas$racePercBlack,na.rm=TRUE),
      racePercAsian = mean(WA_KING_zctas$racePercAsian,na.rm=TRUE),
      racePercHisp = mean(WA_KING_zctas$racePercHisp,na.rm=TRUE),
      racePercAIAN = mean(WA_KING_zctas$racePercAIAN,na.rm=TRUE),
      racePercNHPI = mean(WA_KING_zctas$racePercNHPI,na.rm=TRUE),
      percCollegeGrad = mean(WA_KING_zctas$percCollegeGrad,na.rm=TRUE)
    )
  )  
} 

dens.grid$cov <- dens.predFcn(dens.grid$acc,dens.grid$dens)[,1]
ggplot(data=dens.grid) +
  geom_tile(aes(
    acc,
    dens,
    fill=cov
  )) +
  scale_fill_gradient(low="red",high="yellow") +
  scale_y_log10() +
  labs(x="Access",y="Population Density",fill="Predicted Coverage (%)") +
  geom_point(data=filter(WA_KING_zctas,accTeen>70),
             aes(
               x=accTeen,
               y=popDens
             )
  )

#College Density
col.grid <- data.frame(expand.grid(acc.seq,col.seq))
colnames(col.grid) <- c("acc","col")
col.predFcn <- function(acc,col){
  predict(
    intCollegeModel,
    data.frame(
      accTeen = acc,
      popDens = mean(WA_KING_zctas$popDens,na.rm=TRUE),
      hhPercPov = mean(WA_KING_zctas$hhPercPov,na.rm=TRUE),
      racePercBlack = mean(WA_KING_zctas$racePercBlack,na.rm=TRUE),
      racePercAsian = mean(WA_KING_zctas$racePercAsian,na.rm=TRUE),
      racePercHisp = mean(WA_KING_zctas$racePercHisp,na.rm=TRUE),
      racePercAIAN = mean(WA_KING_zctas$racePercAIAN,na.rm=TRUE),
      racePercNHPI = mean(WA_KING_zctas$racePercNHPI,na.rm=TRUE),
      percCollegeGrad = col
    )
  )  
} 

col.grid$cov <- col.predFcn(col.grid$acc,col.grid$col)[,1]
ggplot(data=col.grid) +
  geom_tile(aes(
    acc,
    col,
    fill=cov
  )) +
  scale_fill_gradient(low="red",high="yellow") +  
  labs(x="Access",y="College Percent",fill="Predicted Coverage (%)") +
  geom_point(data=filter(WA_KING_zctas,accTeen>70),
             aes(
               x=accTeen,
               y=percCollegeGrad
             )
  )



#Single variable with every variable
oneVarCovReg_Teen_Acc <- errorsarlm(coverTeen ~ accTeen,data = WA_KING_zctas,listw = lw,zero.policy = TRUE)
oneVarCovReg_Teen_Pov <- errorsarlm(coverTeen ~ hhPercPov,data = WA_KING_zctas,listw = lw,zero.policy = TRUE)
oneVarCovReg_Teen_Black <- errorsarlm(coverTeen ~ racePercBlack,data = WA_KING_zctas,listw = lw,zero.policy = TRUE)
oneVarCovReg_Teen_Asian <- errorsarlm(coverTeen ~ racePercAsian,data = WA_KING_zctas,listw = lw,zero.policy = TRUE)
oneVarCovReg_Teen_Hisp <- errorsarlm(coverTeen ~ racePercHisp,data = WA_KING_zctas,listw = lw,zero.policy = TRUE)
oneVarCovReg_Teen_AIAN <- errorsarlm(coverTeen ~ racePercAIAN,data = WA_KING_zctas,listw = lw,zero.policy = TRUE)
oneVarCovReg_Teen_NHPI <- errorsarlm(coverTeen ~ racePercNHPI,data = WA_KING_zctas,listw = lw,zero.policy = TRUE)
oneVarCovReg_Teen_College <- errorsarlm(coverTeen ~ percCollegeGrad,data = WA_KING_zctas,listw = lw,zero.policy = TRUE)

multiVarCovReg_Teen_noInt <- errorsarlm(coverTeen ~ accTeen + 
                                          hhPercPov +
                                          racePercBlack +
                                          racePercAsian +
                                          racePercHisp +
                                          racePercAIAN + 
                                          racePercNHPI +
                                          percCollegeGrad,
                                        data = WA_KING_zctas,listw = lw,zero.policy = TRUE)

multiVarCovReg_Teen <- errorsarlm(coverTeen ~ accTeen + 
                                    hhPercPov +
                                    racePercBlack +
                                    racePercAsian +
                                    racePercHisp +
                                    racePercAIAN + 
                                    racePercNHPI +
                                    percCollegeGrad * accTeen,
                                  data = WA_KING_zctas,listw = lw,zero.policy = TRUE)

multiVarAccReg_Teen <- errorsarlm(accTeen ~ hhPercPov +
                                    racePercBlack +
                                    racePercAsian +
                                    racePercHisp +
                                    racePercAIAN + 
                                    racePercNHPI + 
                                    percCollegeGrad,
                                  data = WA_KING_zctas,listw = lw,zero.policy = TRUE)

#Same for school-age
oneVarCovReg_Child_Acc <- errorsarlm(coverChild ~ accChild,data = WA_KING_zctas,listw = lw,zero.policy = TRUE)
oneVarCovReg_Child_Pov <- errorsarlm(coverChild ~ hhPercPov,data = WA_KING_zctas,listw = lw,zero.policy = TRUE)
oneVarCovReg_Child_Black <- errorsarlm(coverChild ~ racePercBlack,data = WA_KING_zctas,listw = lw,zero.policy = TRUE)
oneVarCovReg_Child_Asian <- errorsarlm(coverChild ~ racePercAsian,data = WA_KING_zctas,listw = lw,zero.policy = TRUE)
oneVarCovReg_Child_Hisp <- errorsarlm(coverChild ~ racePercHisp,data = WA_KING_zctas,listw = lw,zero.policy = TRUE)
oneVarCovReg_Child_AIAN <- errorsarlm(coverChild ~ racePercAIAN,data = WA_KING_zctas,listw = lw,zero.policy = TRUE)
oneVarCovReg_Child_NHPI <- errorsarlm(coverChild ~ racePercNHPI,data = WA_KING_zctas,listw = lw,zero.policy = TRUE)
oneVarCovReg_Child_College <- errorsarlm(coverChild ~ percCollegeGrad,data = WA_KING_zctas,listw = lw,zero.policy = TRUE)

multiVarCovReg_Child <- errorsarlm(coverChild ~ accChild +
                                     hhPercPov +
                                     racePercBlack +
                                     racePercAsian +
                                     racePercHisp +
                                     racePercAIAN + 
                                     racePercNHPI +
                                     percCollegeGrad * accChild,
                                   data = WA_KING_zctas,listw = lw,zero.policy = TRUE)
multiVarCovReg_Child_noInt <- errorsarlm(coverChild ~ accChild +
                                           hhPercPov +
                                           racePercBlack +
                                           racePercAsian +
                                           racePercHisp +
                                           racePercAIAN + 
                                           racePercNHPI +
                                           percCollegeGrad,
                                         data = WA_KING_zctas,listw = lw,zero.policy = TRUE)
multiVarAccReg_Child <- errorsarlm(accChild ~ hhPercPov +
                                     racePercBlack +
                                     racePercAsian +
                                     racePercHisp +
                                     racePercAIAN + 
                                     racePercNHPI +
                                     percCollegeGrad,
                                   data = WA_KING_zctas,listw = lw,zero.policy = TRUE)


theme_set(theme_bw() + theme(plot.margin=margin(-2,0,-2,0,"in")))

#Quick map plot for whatever checks you need
ggplot() +
  geom_sf(data=WA_KING_zctas$geometry,
          aes(fill=WA_KING_zctas$racePercAIAN)) +
  labs(title="%AI/AN",fill="Percent") +
  scale_fill_gradient(low="blue3",high="cyan",limits=c(0,4.1))

#Demo map to explain e2SFCA



ggplot() +
  geom_sf(data = WA_KING_zctas$geometry)


#Access Maps
mapAcc_Adol<- ggplot() +
  geom_sf(data=WA_KING_zctas$geometry,
          aes(fill=WA_KING_zctas$accTeen)) +
  labs(title="Vaccine Accessibility - Adolescent",fill="Vaccine Sites \nper 100,000 Children") +
  scale_fill_gradient(low="blue3",high="cyan",limits=c(0,200))
mapAcc_Adol %>% ggsave(filename="./Fig/AdolAccess_Map.tif", width = 8,height = 6,dpi = 300,device = "tiff")

mapAcc_Child <- ggplot() +
  geom_sf(data=WA_KING_zctas$geometry,
          aes(fill=WA_KING_zctas$accChild)) +
  labs(title="Vaccine Accessibility - School Age",fill="Vaccine Sites \nper 100,000 Children") +
  scale_fill_gradient(low="blue3",high="cyan",limits=c(0,60))
mapAcc_Child %>% ggsave(filename="./Fig/ChildAccess_Map.tif", width = 8,height = 6,dpi = 300,device = "tiff")

figAccess <- ggpubr::ggarrange(mapAcc_Adol, mapAcc_Child,
                               ncol = 1,
                               align =  "v")
figAccess %>% ggsave(filename = "./Fig/FigAccess.eps",width =6,height=8,units="in")

#Standard Deviation Maps for Debugging
mapAcc_Adol_SD <- ggplot() +
  geom_sf(data=WA_KING_zctas$geometry,
          aes(fill=WA_KING_zctas$accTeen_sd)) +
  labs(title="SD of Vaccine Accessibility - Adolescent",fill="SD") +
  scale_fill_gradient(low="blue3",high="cyan",limits=c(0,21))
mapAcc_Adol_SD %>% ggsave(filename="./Fig/AdolAccess_Map_SD.tif", width = 8,height = 6,dpi = 300,device = "tiff")


mapAcc_Child_SD <- ggplot() +
  geom_sf(data=WA_KING_zctas$geometry,
          aes(fill=WA_KING_zctas$accChild_sd)) +
  labs(title="SD of Vaccine Accessibility - School Age",fill="SD") +
  scale_fill_gradient(low="blue3",high="cyan",limits=c(0,5))
mapAcc_Child_SD %>% ggsave(filename="./Fig/ChildAccess_Map_SD.tif", width = 8,height = 6,dpi = 300,device = "tiff")



mapCov_Adol <- ggplot() +
  geom_sf(data=WA_KING_zctas$geometry,
          aes(fill=WA_KING_zctas$coverTeen)) +
  labs(title="Vaccine Coverage - Adolescent",fill="% Vaccinated \nChildren") +
  scale_fill_gradient(low="red",high="yellow",limits=c(20,100))
mapCov_Adol %>% ggsave (filename="./Fig/AdolCov_Map.tif",width = 8,height = 6, dpi = 300, device = "tiff")

mapCov_Child <- ggplot() +
  geom_sf(data=WA_KING_zctas$geometry,
          aes(fill=WA_KING_zctas$coverChild)) +
  labs(title="Vaccine Coverage - School Age",fill="% Vaccinated \nChildren") +
  scale_fill_gradient(low="red",high="yellow",limits=c(20,100))
mapCov_Child %>% ggsave (filename="./Fig/ChildCov_Map.tif",width = 8,height = 6, dpi = 300, device = "tiff")

figCov <- ggpubr::ggarrange(mapCov_Adol, mapCov_Child,
                               ncol = 1,
                               align =  "v")
figCov %>% ggsave(filename = "./Fig/FigCov.eps",width =6,height=8,units="in")

#Combined maps of both access and coverage
figAccCov <- ggpubr::ggarrange(mapAcc_Adol,mapCov_Adol,mapAcc_Child,mapCov_Child,
                            align =  "hv")
figAccCov %>% ggsave(filename = "./Fig/FigAccCov.eps",width =8,height=8,units="in")


#Vaccine Sites Maps
mapSite_Child <- ggplot() +
  geom_sf(data = KING_OUTLINE$geometry,
          fill="lightgray",
          color = "black",
          size=1.5) +
  geom_sf(data = VASHON_ZCTA$geometry,
          fill="lightgray",
          color="black",
          size=1.2) +
  geom_sf(data = WA_KING_zctas$geometry,
          color="black",
          fill="beige",
          size=1) +
  geom_sf(data = KING_WATER$geometry,
          fill="lightblue",
          color="lightblue",
          size=0.0)+
  geom_sf(data = vaxCoordChild[vaxCoordChild$inKingCty==TRUE,],
          color = "red",
          size=0.4) +
  ggtitle("Vaccination Sites - School Age")
mapSite_Child %>% ggsave(filename="./Fig/ChildSite_Map.tif", width = 8,height = 6,dpi = 300,device = "tiff")

mapSite_Adol <- ggplot() +
  geom_sf(data = KING_OUTLINE$geometry,
          fill="lightgray",
          color = "black",
          size=1.5) +
  geom_sf(data = VASHON_ZCTA$geometry,
          fill="lightgray",
          color="black",
          size=1.2) +
  geom_sf(data = WA_KING_zctas$geometry,
          color="black",
          fill="beige",
          size=1) +
  geom_sf(data = KING_WATER$geometry,
          fill="lightblue",
          color="lightblue",
          size=0.0)+
  geom_sf(data = vaxCoordTeen[vaxCoordTeen$inKingCty==TRUE,],
          color = "red",
          size=0.4) +
  ggtitle("Vaccination Sites - Adolescent")
mapSite_Adol %>% ggsave(filename="./Fig/AdolSite_Map.tif", width = 8,height = 6,dpi = 300,device = "tiff")

figSite <- ggpubr::ggarrange(mapSite_Adol, mapSite_Child,
                            ncol = 1,
                            align =  "v")
figSite %>% ggsave(filename = "./Fig/FigSite.eps",width =6,height=8,units="in")


#Was not included in final analysis but demographic maps, maybe add to supplement?
mapVehicle <- ggplot() +
  geom_sf(data=WA_KING_zctas$geometry,
          aes(fill=WA_KING_zctas$percNoVehicles)) +
  labs(title="",fill="% No Car") +
  scale_fill_gradient(low="yellow",high="red",limits=c(0,100))
#Scatter Plots For Diagnostics
scatterCorr <- function(x,y,xlab,ylab,xlim=NA,ylim=NA,m="pearson"){
  corObj <- cor.test(x,y,method=m)
  if(is.na(xlim)[1]){
    plot(x,y,
         main=paste("Correlation:",
                    round(corObj$estimate,digits=3),
                    "P-value:",
                    round(corObj$p.value,digits=3)
         ),
         xlab = xlab,
         ylab = ylab
    )
  }else{
    plot(x,y,
         main=paste("Correlation:",
                    round(corObj$estimate,digits=3),
                    "P-value:",
                    round(corObj$p.value,digits=3)
         ),
         xlab = xlab,
         ylab = ylab,
         xlim = xlim,
         ylim = ylim
    )
  }
}

par(mfrow=c(4,2))
par(mar=c(4,4,2,2))
scatterCorr(x=WA_KING_zctas$percCollegeGrad,
            y=WA_KING_zctas$coverTeen,
            xlab="% College",
            ylab="Coverage")    

scatterCorr(x=WA_KING_zctas$percCollegeGrad,
            y=WA_KING_zctas$accTeen,
            xlab="% College",
            ylab="Access")


scatterCorr(x=WA_KING_zctas$racePercAsian,
            y=WA_KING_zctas$coverTeen,
            xlab="% Asian",
            ylab="Coverage")    

scatterCorr(x=WA_KING_zctas$racePercAsian,
            y=WA_KING_zctas$accTeen,
            xlab="% Asian",
            ylab="Access")

scatterCorr(x=WA_KING_zctas$hhPercPov,
            y=WA_KING_zctas$coverTeen,
            xlab="% Poverty",
            ylab="Coverage")    

scatterCorr(x=WA_KING_zctas$hhPercPov,
            y=WA_KING_zctas$accTeen,
            xlab="% Poverty",
            ylab="Access")

scatterCorr(x=WA_KING_zctas$accTeen,
            y=WA_KING_zctas$coverTeen,
            xlab="Access",
            ylab="Coverage")    

WA_KING_zctas$highCollege <- WA_KING_zctas$percCollegeGrad >= 50



zctas_lowCollege <- filter(WA_KING_zctas,percCollegeGrad < 50)
zctas_highCollege <- filter(WA_KING_zctas,percCollegeGrad >= 50)

nb.hc<-poly2nb(zctas_highCollege)
lw.hc <- nb2listw(nb.hc,style="W",zero.policy = TRUE) #Neighbor weights, adjacency based, equal

nb.lc<-poly2nb(zctas_lowCollege)
lw.lc <- nb2listw(nb.lc,style="W",zero.policy = TRUE) #Neighbor weights, adjacency based, equal

CovReg_Teen_HighCollege <- errorsarlm(coverTeen ~ accTeen,
                                      data = zctas_highCollege,listw = lw.hc,zero.policy = TRUE)
CovReg_Teen_LowCollege <- errorsarlm(coverTeen ~ accTeen,
                                     data = zctas_lowCollege,listw = lw.lc,zero.policy = TRUE)

CovReg_Child_HighCollege <- errorsarlm(coverChild ~ accChild,
                                       data = zctas_highCollege,listw = lw.hc,zero.policy = TRUE)
CovReg_Child_LowCollege <- errorsarlm(coverChild ~ accChild,
                                      data = zctas_lowCollege,listw = lw.lc,zero.policy = TRUE)


par(mfrow=c(2,2))
scatterCorr(x=zctas_highCollege$accTeen,
            y=zctas_highCollege$coverTeen,
            xlab="Access (College >= 50%)",
            ylab="Coverage (Teen)",
            xlim=c(0,170),
            ylim=c(0,100))
scatterCorr(x=zctas_lowCollege$accTeen,
            y=zctas_lowCollege$coverTeen,
            xlab="Access (College < 50%)",
            ylab="Coverage (Teen)",
            xlim=c(0,170),
            ylim=c(0,100))
scatterCorr(x=zctas_highCollege$accChild,
            y=zctas_highCollege$coverChild,
            xlab="Access (College >= 50%)",
            ylab="Coverage (Child)",
            xlim=c(0,50),
            ylim=c(0,100))
scatterCorr(x=zctas_lowCollege$accChild,
            y=zctas_lowCollege$coverChild,
            xlab="Access (College < 50%)",
            ylab="Coverage (Child)",
            xlim=c(0,50),
            ylim=c(0,100))



t.test(zctas_highCollege$accChild,zctas_lowCollege$accChild)
t.test(zctas_highCollege$coverChild,zctas_lowCollege$coverChild)


t.test(zctas_highCollege$accTeen,zctas_lowCollege$accTeen)
t.test(zctas_highCollege$coverTeen,zctas_lowCollege$coverTeen)