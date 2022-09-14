head(ppdata_test2New)

PurePrem.01 <- ppdata_test2New$PurePrem[1:2000]
PurePrem.02 <- ppdata_test2New$PurePrem[2001:4000]
PurePrem.03 <- ppdata_test2New$PurePrem[4001:6000]
PurePrem.04 <- ppdata_test2New$PurePrem[6001:8000]
PurePrem.05 <- ppdata_test2New$PurePrem[8001:10000]
PurePrem.06 <- ppdata_test2New$PurePrem[10001:12000]
PurePrem.07 <- ppdata_test2New$PurePrem[12001:14000]
PurePrem.08 <- ppdata_test2New$PurePrem[14001:16000]
PurePrem.09 <- ppdata_test2New$PurePrem[16001:18000]
PurePrem.10 <- ppdata_test2New$PurePrem[18001:20000]

Predicted.01 <- ppdata_test2New$Predicted[1:2000]
Predicted.02 <- ppdata_test2New$Predicted[2001:4000]
Predicted.03 <- ppdata_test2New$Predicted[4001:6000]
Predicted.04 <- ppdata_test2New$Predicted[6001:8000]
Predicted.05 <- ppdata_test2New$Predicted[8001:10000]
Predicted.06 <- ppdata_test2New$Predicted[10001:12000]
Predicted.07 <- ppdata_test2New$Predicted[12001:14000]
Predicted.08 <- ppdata_test2New$Predicted[14001:16000]
Predicted.09 <- ppdata_test2New$Predicted[16001:18000]
Predicted.10 <- ppdata_test2New$Predicted[18001:20000]

AvgPure <- c(mean(PurePrem.01),
             mean(PurePrem.02),
             mean(PurePrem.03),
             mean(PurePrem.04),
             mean(PurePrem.05),
             mean(PurePrem.06),
             mean(PurePrem.07),
             mean(PurePrem.08),
             mean(PurePrem.09),
             mean(PurePrem.10)
)

AvgPred <- c(mean(Predicted.01),
             mean(Predicted.02),
             mean(Predicted.03),
             mean(Predicted.04),
             mean(Predicted.05),
             mean(Predicted.06),
             mean(Predicted.07),
             mean(Predicted.08),
             mean(Predicted.09),
             mean(Predicted.10)
)

Quantiles <- data.frame(Quantile = c(1:10), AvgPred = AvgPred, AvgPure = AvgPure)
Quantiles2 <- data.frame(Quantile = c(1:10, 1:10), 
                         Averages = c(AvgPred, AvgPure), 
                         Type = c(rep("Pred", 10), rep("Pure", 10))
                         )


p = ggplot(Quantiles2, aes(x = Quantile, y = Averages, fill = Type)) + 
  geom_bar(position="stack",stat="identity") +
  geom_line(data = Quantiles2, aes(x = Quantile, y = Averages), color = "red") +
  xlab('Quantiles') +
  ylab('Averages')

print(p)

p2 = ggplot(Quantiles2, aes(x = Quantile, y = Averages)) + 
  geom_bar(stat="identity") +
  geom_line(data = Quantiles2, aes(x = Quantile, y = Averages), color = "red") +
  xlab('Quantiles') +
  ylab('Averages')

print(p2)

p3 = ggplot(Quantiles2, aes(x = Quantile, y = Averages, fill = Type)) + 
  geom_bar(stat="identity",position=position_dodge()) +
  geom_line(data = Quantiles2, aes(x = Quantile, y = Averages), color = "red") +
  xlab('Quantiles') +
  ylab('Averages')

print(p3)

#"Quant 1","Quant 2", "3"
for (t in 1:2000) {
  if(ppdata_test2New$Quant[t] == 1) {
    
  }
  if(ppdata_test2New$Quant[t] == 2) {
    Fric20 = Fric20 + 1
  }
  if(ppdata_test2New$Quant[t] == 3) {
    Fric30 = Fric30 + 1
  }
  if(ppdata_test2New$Quant[t] == 4) {
    Fric40 = Fric40 + 1
  }
  if(ppdata_test2New$Quant[t] == 5) {
    Fric50 = Fric50 + 1
  }
  if(ppdata_test2New$Quant[t] == 6) {
    Fric10 = Fric10 + 1
  }
  if(ppdata_test2New$Quant[t] == 7) {
    Fric20 = Fric20 + 1
  }
  if(ppdata_test2New$Quant[t] == 8) {
    Fric30 = Fric30 + 1
  }
  if(ppdata_test2New$Quant[t] == 9) {
    Fric40 = Fric40 + 1
  }
  if(ppdata_test2New$Quant[t] == 10) {
    Fric50 = Fric50 + 1
  }
}