
#### Libraries ####
library(ndtv)
library(tsna)
data(short.stergm.sim)

#### Diff vis ####
render.animation(short.stergm.sim)
ani.replay()
render.d3movie(short.stergm.sim)
timeline(short.stergm.sim)

#### Basic temporal stats ####
# HOw many ties form at each time step?
tEdgeFormation(short.stergm.sim)
plot( tEdgeFormation(short.stergm.sim) )

# When is the first time node 13 can reach others? ---------------
path<-tPath(short.stergm.sim,v = 13, graph.step.time=1)

plotPaths(short.stergm.sim,
          path,
          label.cex=0.5)

#### Making our own ####

# Make nodes
wheel <- network.initialize(10)
class(wheel)
plot(wheel) # Views wime as compressed

# Add edges
add.edges.active(wheel,tail=1:9,head=c(2:9,1),onset=1:9, terminus=11)
add.edges.active(wheel,tail=10,head=c(1:9),onset=10, terminus=12)
plot(wheel)

as.data.frame(wheel)

# View at specific time point
plot(network.extract(wheel, at = 1))

# Or a range
plot(network.extract(wheel,onset=1,terminus=5))

# Get all edge activity for EDGES 1 and 2
get.edge.activity(wheel)[1:2]

#### When do edges form in one image ####

elabels<-lapply(get.edge.activity(wheel),
                function(spl){
                  paste("(",spl[,1],"-",spl[,2],")",sep='')
                })

plot(wheel,displaylabels=TRUE,edge.label=elabels,
     edge.label.col='blue') 

# Make a movie
render.animation(wheel)
ani.replay()

#### Example data cretion ####
t0<-as.network(matrix(c(0,1,0,
                        0,0,0,
                        1,0,0),ncol=3,byrow=TRUE))

t1<-as.network(matrix(c(0,1,0,
                        0,1,0,
                        0,0,0),ncol=3,byrow=TRUE))

t2<-as.network(matrix(c(0,0,0,
                        0,1,0,
                        0,1,0),ncol=3,byrow=TRUE))
# convert a list of networks into networkDynamic object
tnet<-networkDynamic(network.list=list(t0,t1,t2))

as.data.frame(tnet)[,1:4]

# Make network Dynamic
test = as.network.networkDynamic(tnet)

# How many edges exist at each time?
# plot 
timeline(short.stergm.sim,slice.par=list(start=0,end=25,interval=1,
                                         aggregate.dur=1,rule='latest'),
         plot.vertex.spells=FALSE)

# at one point
network.edgecount(network.extract(short.stergm.sim,onset=0,terminus=1))

# At all points
tErgmStats(short.stergm.sim,'edges',start = 0,end=25,time.interval=1)

# Getting metrics using colapsed time SNA and ERGMs

# Tpath to measure when goedesics are formed
path<-tPath(short.stergm.sim,v = 13,
            graph.step.time=1)
path
# When was each node reached?
plot(path,edge.lwd = 2)

transmissionTimeline(path,jitter=TRUE,
                     main='Earliest forward path from vertex 13')

# Dynamic network attributes

activate.edge.attribute(wheel,'width',1,onset=0,terminus=3) 
activate.edge.attribute(wheel,'width',5,onset=3,terminus=7)
activate.edge.attribute(wheel,'width',10,onset=7,terminus=Inf)

list.edge.attributes(wheel)
get.edge.attribute.active(wheel,'width', at=2)

activate.vertex.attribute(wheel,'mySize',1, onset=-Inf,terminus=Inf)
activate.vertex.attribute(wheel,'mySize',3, onset=5,terminus=10,v=4:8)

activate.vertex.attribute(wheel,'color','gray',onset=-Inf,terminus=Inf)
activate.vertex.attribute(wheel,'color','red',onset=5,terminus=6,v=4)
activate.vertex.attribute(wheel,'color','green',onset=6,terminus=7,v=5)
activate.vertex.attribute(wheel,'color','blue',onset=7,terminus=8,v=6)
activate.vertex.attribute(wheel,'color','pink',onset=8,terminus=9,v=7)

# CONDITIONALS - ---------
when.vertex.attrs.match(wheel,'color',value = 'green')

when.edge.attrs.match(wheel,'width', match.op = '>', value = 3)

render.d3movie(wheel,edge.lwd='width',vertex.cex='mySize',
                 vertex.col='color',verbose=FALSE)

# Special effects
render.d3movie(wheel,
                 edge.col=effectFun('edgeAgeColor',fade.dur=5,
                                    start.color='red',end.color='green'),
                 edge.lwd=4,
                 verbose=FALSE)






