
library(TrackReconstruction)
library(plotly)
library(smacof)
library(leaflet)
library(geojsonio)
library(gridExtra)
library(gclus)
library(adegraphics)

# coldiss()
# Color plots of a dissimilarity matrix, without and with ordering
#
# License: GPL-2 
# Author: Francois Gillet, August 2009
#

coldiss <- function(D, nc = 4, byrank = TRUE, diag = FALSE)
{
  require(gclus)
  
  if (max(D)>1) D <- D/max(D)
  
  if (byrank) {
    spe.color = dmat.color(1-D, cm.colors(nc))
  }
  else {
    spe.color = dmat.color(1-D, byrank=FALSE, cm.colors(nc))
  }
  
  spe.o = order.single(1-D)
  speo.color = spe.color[spe.o,spe.o]
  
  op = par(mfrow=c(1,2), pty="s")
  
  if (diag) {
    #plotcolors(spe.color, rlabels=attributes(D)$Labels, 
    #           main="Dissimilarity Matrix", 
    #           dlabels=attributes(D)$Labels)
    plotcolors(speo.color, rlabels=attributes(D)$Labels[spe.o], 
               main="Ordered Dissimilarity Matrix", 
               dlabels=attributes(D)$Labels[spe.o])
  }
  else {
    #plotcolors(spe.color, rlabels=attributes(D)$Labels, 
    #           main="Dissimilarity Matrix")
    plotcolors(speo.color, rlabels=attributes(D)$Labels[spe.o], 
               main="Ordered Dissimilarity Matrix",label.cex = 0.2)
  }
  
  par(op)
}

geoOffsetKm<-function(lat,lng,dy,dx){
  latitude<-lat+(dy/6378)*(180/pi)
  longitude<-lng+(dx/6378)*(180/pi)/cos(latitude*pi/180)
  return(c(latitude,longitude))
}

geoAngleBetweenPoints<-function(lat1,lng1,lat2,lng2){
  dy<-lat2-lat1
  dx<-cos(lat1*pi/180)*(lng2-lng1)
  return(atan2(dy,dx))
}

rotateMatrix<-function(M,angle){
  return(t(apply(M, 1, function(x) c(x[1]*cos(angle)-x[2]*sin(angle),x[1]*sin(angle)+x[2]*cos(angle)))))
}

set.seed(1)
wm<-geojsonio::geojson_read("Others/data/worldmap.json", what = "sp")
mapaChile<-wm[wm$id=='CHL',]
cities<-read.csv('Others/data/worldcities.csv')
cities<-cities[which(cities$country=='Chile'),]
n<-nrow(cities)


M=matrix(0,nrow=n,ncol=n,dimnames = list(cities$city,cities$city_ascii))

for( i in 0:n){
  for(j in 0:n){
    if(i<j){
      M[j,i]<-CalcDistance(cities[i,]$lat,cities[i,]$lng,cities[j,]$lat,cities[j,]$lng)
    }
  }
}

D<-as.dist(M)


set.seed(1)
citiesMDS<-cmdscale(D)
#citiesMDS<-smacofSym(D,ndim=2)$conf
citiesMDS<-as.data.frame(citiesMDS)
names(citiesMDS)<-c('x','y')
citiesMDS$x<-citiesMDS$x
citiesMDS$y<-citiesMDS$y*-1
#x<-geneticMds2(D,D)
#citiesMDS<-x[[2]][which(x[[2]]$i==1),c(3:4)]

citiesMDS<-as.data.frame(rotateMatrix(citiesMDS,1.026*pi))
names(citiesMDS)<-c('x','y')


dataAn<-citiesMDS[c('Santiago','Arica','Punta Arenas'),]
dataAn2<-citiesMDS[c('Valparaíso'),]

a <- list(
  autotick = FALSE,
  #ticks = "outside",
  tick0 = 0,
  dtick = 200
  #ticklen = ,
  #tickwidth = 2,
  #tickcolor = toRGB("blue")
)

p <- plot_ly(data = citiesMDS, x = ~y, y = ~x,type='scatter',mode='markers',text=rownames(citiesMDS),width = 200,height = 800)%>%
  add_annotations(x=dataAn$y,
                  y=dataAn$x,
                  text=rownames(dataAn),
                  xref='y',
                  yref='x',
                  showarrow=TRUE,
                  arrowhead=4,
                  arrowsize=.5,
                  ax=20,
                  ay=-40
                  )%>%
  add_annotations(x=dataAn2$y,
                  y=dataAn2$x,
                  text=rownames(dataAn2),
                  xref='y',
                  yref='x',
                  showarrow=TRUE,
                  arrowhead=4,
                  arrowsize=.5,
                  ax=-40,
                  ay=20
  )%>%
  layout(xaxis=a,yaxis=a)
p


citiesMDS$y<-citiesMDS$y-as.numeric(citiesMDS["Santiago",]$y)
citiesMDS$x<-citiesMDS$x-as.numeric(citiesMDS["Santiago",]$x)

citiesMDSgeo<-t(apply(citiesMDS,1,function(x) geoOffsetKm(-33.45,-70.667,x[1],x[2])))
citiesMDSgeo<-as.data.frame(citiesMDSgeo)
names(citiesMDSgeo)<-c('lat','lng')

citiesMDSgeo[1,]-cities[1,c('lat','lng')]


leaflet(data=cities)%>%
  addProviderTiles(providers$Esri.WorldGrayCanvas)%>%
  addCircleMarkers(lng=~lng,lat=~lat,label=~city,radius = 1,color='black',fillOpacity = 1, stroke = FALSE)%>%
  addCircleMarkers(lng=citiesMDSgeo$lng,lat=citiesMDSgeo$lat,color='red',radius = 2,fill = FALSE,weight=1)

#geo styling
g <- list(
  scope = 'chile',
  projection = list(type = 'santiago chile'),
  showland = TRUE,
  landcolor = toRGB("gray95"),
  subunitcolor = toRGB("gray85"),
  countrycolor = toRGB("gray85"),
  countrywidth = 0.5,
  subunitwidth = 0.5
)

p <- plot_geo(citiesMDSgeo, lat = ~lat, lon = ~lng) %>%
  add_markers(
    #text = ,
    #size = I(8), hoverinfo = "text"
  ) %>%
  layout(
     geo = g
  )

p

dataHeatmap<-as.matrix(D)[order.single(1-D),order.single(1-D)]
p<-plot_ly(z=dataHeatmap,x=rownames(dataHeatmap),y=colnames(dataHeatmap),type='heatmap')%>%
  layout(
      xaxis=list(type='category',
                 showticklabels=TRUE,

                 autotick=FALSE,
                 tickangle=-90
                 )
  )
p

write.table(dataHeatmap,"citiesDistTable.txt",sep='\t')
