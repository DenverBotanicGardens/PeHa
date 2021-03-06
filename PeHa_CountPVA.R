#' @title CountPVA specific for PeHa
#' @export

CountPVA_peha <- function(dataframe){
  DL <- dataframe[dataframe$Site == "Dry Lake",]
  AE <- dataframe[dataframe$Site == "Above Eagle",]
  
  DL.table <- lapply(4:nrow(DL), function(yrs){
    y.count <- DL$X.ofRosettes[1:yrs]
    x.years <- DL$Year[1:yrs]
    ySpecies <- log10(y.count[-1]/y.count[-length(y.count)])
    yr <- sqrt(x.years[-1]-x.years[-length(x.years)])
    
    PVA.lm <- lm(ySpecies ~ -1 + yr)
    mu_sp <- predict(PVA.lm, level=0.95, interval = "confidence",
                     se.fit = TRUE)$fit[1,]
    rsq <- summary(PVA.lm)$adj.r.squared
    out <- data.frame(rsq,RangeStart = min(DL$Year), RangeEnd = DL$Year[yrs],
                      t(mu_sp))
    out
  })
  DL.tab <- do.call(rbind, DL.table)
  DL.tab <- data.frame(Site = "Dry Lake", DL.tab)
  
  AE.table <- lapply(4:nrow(AE), function(yrs){
    y.count <- AE$X.ofRosettes[1:yrs]
    x.years <- AE$Year[1:yrs]
    ySpecies <- log10(y.count[-1]/y.count[-length(y.count)])
    yr <- sqrt(x.years[-1]-x.years[-length(x.years)])
    
    PVA.lm <- lm(ySpecies ~ -1 + yr)
    mu_sp <- predict(PVA.lm, level=0.95, interval = "confidence",
                     se.fit = TRUE)$fit[1,]
    rsq <- summary(PVA.lm)$adj.r.squared
    out <- data.frame(rsq,RangeStart = min(AE$Year), RangeEnd = AE$Year[yrs],
                      t(mu_sp))
    out
  })
  AE.tab <- do.call(rbind, AE.table)
  AE.tab <- data.frame(Site = "Above Eagle", AE.tab)
  list(AE.tab, DL.tab)
}