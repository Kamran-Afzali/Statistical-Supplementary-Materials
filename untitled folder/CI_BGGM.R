
######This function plots a given node's edges and mark identify those with CI's not including 0#########################
require(BGGM)
require(ggplot2)
require(qgraph)
CI_BGGM=function(data=data, node=node, ci_width = 0.95){

fit <- BGGM::estimate(data, iter = 5000, analytic = FALSE)
Coefs=as.data.frame(BGGM:::beta_summary(fit, node = node, ci_width = ci_width, samples = 5000)[[1]][1])[,c(1,2,4,5)]
colnames(Coefs) = c("ID", "Mean","Lower","Upper")
CIs.long = melt(Coefs, id=c("ID"), variable.name = "CI")
CIs.long$ID=as.factor(CIs.long$ID-1)
CIs.long$value = as.numeric(as.character(CIs.long$value))
plot1 <-ggplot(CIs.long, aes(x = value ,y = ID, group = ID)) +
  labs(x = "Credible interval", y = "Edge with node") +
  geom_point( size = 1, shape = 15) + 
  geom_line() + 
  ggtitle(paste("node",node, sep= " ")) +
  theme(plot.title = element_text(hjust = 0.5)) 

Nodes_to_note=which(as.vector(apply(Coefs[,2:4],1,function(x){all(x>0)})|apply(Coefs[,2:4],1,function(x){all(x<0)})))

output=list(Coefs=Coefs,plot=plot1,Nodes_to_note=Nodes_to_note)
return(output)

}



CB=CI_BGGM(data=net1,node=1)
summary(CB)
plot(CB$plot)
