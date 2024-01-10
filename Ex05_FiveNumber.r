library(statnet)

###Location for plots
GG<-as.matrix(read.csv('https://raw.githubusercontent.com/SiuSunChun/Ex01_FiveNumber/main/NetDLoc.csv',he=T))

###Data in Sociomatrices (adjacency matrix) 
FD<-as.matrix(read.csv('https://raw.githubusercontent.com/SiuSunChun/Ex01_FiveNumber/main/NetD.csv', row.names=1,he=T,sep=','))
#FD

nFD<-network(FD)


plot(nFD, vertex.cex=apply(FD,2,sum)+1, usearrows=FALSE,
    vertex.sides=3+apply(FD,2,sum),label=network.vertex.names(nFD),
    vertex.col=2+(network.vertex.names(nFD)=="Ivory"),coord=GG)



####Size
network.size(nFD)

####Density 
gden(nFD)

####Components
components(nFD)

####Diameter 
lgc<-component.largest(nFD,result='graph')
gd<-geodist(lgc)
max(gd$gdist) ##Jane to Donny

###Clustering coefficient, Transitivity 
gtrans(nFD,mode='graph')

####Change
cbind(1:16,row.names(FD))

#####################
FD2<-FD
FD2[6,14]=1 #Fred to Nora
FD2[14,6]=1 #Nora to Fred

FD2[1,14]=1 #Ada to Jane
FD2[14,1]=1 #Jane to Ada

FD2[3,12]=1 #Cat to Lindo
FD2[12,3]=1 #Linda to Cat

#########
nFD2<-network(FD2)


##Set colour for the new link
cbind(1:46,as.edgelist(nFD2))

COLe<-rep(1,46)
COLe[c(2,8,16,32,36,37)]<-4 #blue colour



plot(nFD2, vertex.cex=apply(FD2,2,sum)+1, usearrows=FALSE,
    vertex.sides=3+apply(FD2,2,sum),label=network.vertex.names(nFD2),
    vertex.col=2+(network.vertex.names(nFD2)=="Ivory"),coord=GG,edge.col =COLe)



####Size
network.size(nFD)
network.size(nFD2)



####Density 
gden(nFD)
gden(nFD2)


####Components
components(nFD)
components(nFD2)


####Diameter 
lgc<-component.largest(nFD,result='graph')
gd<-geodist(lgc)
max(gd$gdist) ##Jane to Donny

lgc<-component.largest(nFD2,result='graph')
gd<-geodist(lgc)
max(gd$gdist) ##Jane to Donny



###Clustering coefficient, Transitivity
gtrans(nFD,mode='graph')
gtrans(nFD2,mode='graph')


