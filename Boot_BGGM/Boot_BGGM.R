
######Non parametric bootstrapping stability analysis of a BGGM model########################
require(BGGM)
require(ggplot2)
require(qgraph)
Boot_BGGM=function(data=data, niter=1000)
{
  data=data
  niter=niter
  centralities=matrix(NA,ncol(data),niter)
  edges=matrix(NA,(ncol(data)*(ncol(data)-1))/2,niter)
  pb <- txtProgressBar(min = 0, max = niter, style = 3)
  for (i in 1:niter)
  {
    p=(1-((i/niter)-(1)))/2
    index=sample(1:nrow(data), round (p*nrow(data),0), replace = F)
    data2=data[index,]
    fit <- BGGM::estimate(data2, iter = 5000, analytic = FALSE)
    edges[,i]=fit$parcors_mat[lower.tri(fit$parcors_mat, diag = FALSE)]
    centralities[,i]=centrality_auto(fit$parcors_mat)$node.centrality$Strength
    setTxtProgressBar(pb, i)
  }
  datagg=as.data.frame(cbind(proprtion=c((1-((2:niter/niter)-(1)))/2),
                             centrality_cors=apply(centralities[,2:niter],2,function(x){cor(x,centralities[,1])})))
  Strength_correlation=ggplot(datagg, aes(x = proprtion, y=centrality_cors)) + geom_point(col="tomato")+ 
    geom_smooth(method="loess",fullrange=TRUE, level=0.999999999999,span = 0.3)+ ggtitle("Strength_correlation") +
    theme(plot.title = element_text(hjust = 0.5))
  
  datagg=as.data.frame(cbind(proprtion=c((1-((2:niter/niter)-(1)))/2),
                             edges_cors=apply(edges[,2:niter],2,function(x){cor(x,edges[,1])})))
  Edge_correlation=ggplot(datagg, aes(x = proprtion, y=edges_cors)) + geom_point(col="gold")+ 
    geom_smooth(method="loess",fullrange=TRUE, level=0.999999999999,span = 0.3)+ ggtitle("Edge_correlation") +
    theme(plot.title = element_text(hjust = 0.5))
  
  output=list(proportions=(1-((1:niter/niter)-(1)))/2,centralities=centralities,edges=edges,Strength_correlation=Strength_correlation,Edge_correlation=Edge_correlation)
  return(output)
}


bb=Boot_BGGM(data=net1, niter=50)
summary(bb)
plot(bb$Edge_correlation)
