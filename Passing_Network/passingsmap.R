#passingsnetwork by Jan Mullenberg and David Fombella

#install.packages("gridExtra")
#install.packages("knitr")
#install.packages("kableExtra")
#install.packages("ggnetwork")
#install.packages("raster")

#-----------------------------------------------------------------------------------------------------------------------------
getwd()

library(scales)
library (pheatmap)
library (igraph)
library(data.table)
library(network)
library(ndtv)
library(visNetwork)
library(RColorBrewer)
library(gplots)
library(png)
library(spdep)
library(sp)
library(grid)
library(ggnetwork)
library(raster)
library (superheat)
library(data.table)
library(dplyr)
library(formattable)
library(tidyr)



filename <- file.choose()
df <- read.csv(filename, header= TRUE, sep = ",", stringsAsFactors = F) 
name <- basename(filename)
head (df)
# change own path
# setwd("C:/Users/")

unique(df$teamName)
df$minute <- df$minute*60
seconds<- df$minute + df$second
df <- cbind(df,seconds)
df <- df[order(df$seconds),]
df <- subset(df, teamName=="Tottenham")
df <- cbind(df,ontvangst=rep(df$playerName))
df$ontvangst <- c(as.character(df$ontvangst)[-1], NA)
df <- subset(df, type=="Pass")
df <- subset(df, outcomeType=="Successful")
df <- subset(df, y != 0)
df <- subset(df, y != 100)
df <- subset(df, ontvangst != "NA")
unique(df$ontvangst)
unique(df$playerName)
 

library(plyr)
library(ggplot2)
# Passing counts pairs  ontvangst  is for receiver
counts <- ddply( df, c("playerName", "ontvangst"), nrow )
m <- data.frame(counts)

m <- xtabs(m$V1 ~ m$playerName + m$ontvangst , data = m)

rotate <- function (matrix) t(apply (matrix,2,rev))
rotmatrix <- rotate (matrix)
# Error in apply(matrix, 2, rev) : dim(X) must have a positive length 


# Matrix of passes
heatmap <-superheat(m, 
          # add text
          X.text = m,
          # scale columns
          scale = F, 
          # label aesthetics
          left.label.size = 0.3,
          bottom.label.size = 0.3,
          bottom.label.text.angle = 90,
          bottom.label.text.alignment = "right",
          bottom.label.col = "white",
          left.label.col = "white",
          legend = FALSE,
          column.title = "passes received",
          row.title = "passes given",
          left.label.text.size = 4,
          bottom.label.text.size = 4,
          
          # dendrogram
          #row.dendrogram = T,
          #col.dendrogram = T,
          pretty.order.rows = T,
          pretty.order.cols = T,
     
          yr.plot.type = "bar",
          yr.axis.name = 'total given',
          
          yt.plot.type = "bar",
          yt.axis.name = 'received',
          # grid lines
          grid.vline.col = "white",
          grid.hline.col = "white")




df <- df [c("playerName","ontvangst","type","seconds","teamName","x","y","endX","endY")]
teamname <- (df$teamName[1])	




#-----------------------------------------------------------------------------------------------
# igraph part

dataplot <- graph.data.frame(df, directed=TRUE)
# next line returns true is simple maybe consider removing is simple
# dataplot <- is_simple(simplify(dataplot, remove.loops= TRUE))
datamatrix <- get.adjacency(dataplot)
dataplot <- graph.adjacency(datamatrix,weighted=TRUE)

#---------------------------------------------------------------------------------------------
passes_given <- apply (datamatrix, 1, sum)
passes_received<- apply (datamatrix, 2, sum)
degree_total <- passes_given+passes_received
authority_score_ <- authority_score(dataplot, weights=NA)$vector
 

#---------------------------------------------------------------------------------------------

betweenness <-igraph::betweenness(dataplot)
betweenness_ <- rescale(betweenness, to = c(0, 1))
#---------------------------------------------------------------------------------------------
degree <- igraph::degree(dataplot)
degree_ <- rescale(degree, to = c(0, 1))
#---------------------------------------------------------------------------------------------
pagerank <- igraph::page.rank(dataplot)
pagerank <- pagerank$vector
pagerank_ <- rescale(pagerank, to = c(0, 1))

hub_score <- hub.score(dataplot)
hub_score<- hub_score$vector
authority_score <- authority_score(dataplot)
authority_score <- authority_score$vector
#---------------------------------------------------------------------------------------------

evcent_ <- igraph::evcent (dataplot, directed = FALSE)   
eigen_vector_<- evcent_$vector
d <- optimal.community(dataplot)
evcent_ <- igraph::evcent (dataplot, directed = FALSE)   
eigen_vector<- evcent_$vector

get_std_coords <- function(playerName, coord_dataframe){
  c(std.x = sd(coord_dataframe[coord_dataframe$playerName == playerName,"x"]),
    std.y = sd(coord_dataframe[coord_dataframe$playerName == playerName,"y"]))
}



playerName<-V(dataplot)$name
std_coord.df <- lapply(playerName, FUN = get_std_coords, df)
std_coord.df <- data.frame(do.call(rbind, std_coord.df))
std <- std_coord.df$std.x + std_coord.df$std.y/2 
variation_passing_location <- rescale(std, to = c(0, 1))
std_coord.df <- cbind(std_coord.df,std)
rownames(std_coord.df) <- playerName
sd <- data.frame(std_coord.df)


round_df <- function(descriptives, digits) {
  nums <- vapply(descriptives, is.numeric, FUN.VALUE = logical(1))
  descriptives[,nums] <- round(descriptives[,nums], digits = digits)
  (descriptives)
}


playerName<-V(dataplot)$name
descriptives <- data.frame (passes_given, passes_received, betweenness,pagerank_)
descriptives <- descriptives [order(descriptives$pagerank_, decreasing = T),]
descriptives <- round_df(descriptives, digits=2)
descriptives <- formattable(descriptives)

# PLOT cOLOR TABLE
formattable (descriptives, list(
  pagerank_= color_tile("white", "orange"),
  eigen_vector_= color_tile("white", "orange"),
  variation_passing_location = color_tile("white", "olivedrab2"),
  betweenness= color_tile("white", "yellow"),
  passes_given= color_tile("white", "light blue"),
  passes_received= color_tile("white", "light blue")
  
  ))



#------------------------------------------------------------------------------------------------

dataplot <- graph.data.frame(df, directed=TRUE)
datamatrix <- get.adjacency(dataplot)


suppressMessages(datamatrix[datamatrix < mean(datamatrix) ] <- 0)

# omzetten naar edge graph
# convertir a grafico de borde
dataplot <- graph.adjacency(datamatrix,weighted=TRUE)

#------------------------------------------------------------------------------------------------
get_mean_coords <- function(playerName, coord_dataframe){
  c(mean.x = mean(coord_dataframe[coord_dataframe$playerName == playerName,"x"]),
    mean.y = mean(coord_dataframe[coord_dataframe$playerName == playerName,"y"]))
}


mean_coord.df <- lapply(playerName, FUN = get_mean_coords, df)
mean_coord.df <- data.frame(do.call(rbind, mean_coord.df))
rownames(mean_coord.df) <- playerName

get_std_coords <- function(playerName, coord_dataframe){
  c(std.x = sd(coord_dataframe[coord_dataframe$playerName == playerName,"x"]),
    std.y = sd(coord_dataframe[coord_dataframe$playerName == playerName,"y"]))
}



colfunc <- colorRampPalette(c("gray80", "black"))


colorvec <- colfunc(8)


ind <- E(dataplot)$weight[order(E(dataplot)$weight)] - min(E(dataplot)$weight) + 1
ind <- as.numeric(cut(ind, 
                      breaks = c(-Inf, 2, 3, 4, 5, 7, 10, 15, 20, Inf), 
                      labels = c(1:9), 
                      right = FALSE))


E(dataplot)$color[order(E(dataplot)$weight)] <- colorvec[ind]


##Opties zetten voor igraph (Specifiek voor dataplot)
#Selecteren van de between kolom. op basis van volgorde van player names zoals in graph
bet<- descriptives[V(dataplot)$name,"betweenness"]
colfunc <- colorRampPalette(c("white", "darkcyan"))

#Deze kleuren toevoegen aan dataplot zodat ze ook worden geplot
bet<- as.numeric(cut(bet, 
                      breaks = c(-Inf, 1, 2, 3 ,5, 8, 10, 20, 30, Inf), 
                      labels = c(1:9), 
                      right = FALSE))
colorvec <- colfunc(8)[bet]
V(dataplot)$color = colorvec





#roteren van dit  dataframe
coords <- spdep::Rotation(as.matrix(mean_coord.df[,1:2]), 90*pi/180)

par (mfrow = c(1,1))
par(mar = c (-0.5,0,-0.5,0))

#extra opties voor het dataplot
igraph.options(edge.arrow.size= E(dataplot)$weight*0.7)
igraph.options(edge.arrow.size = E(dataplot)$weight*0.7)
igraph.options(edge.curved= 0.08)
igraph.options(vertex.label.dist = 1.4)
igraph.options(vertex.label.color = "gray20")
igraph.options(vertex.label.cex=1.4)
igraph.options(vetex.color = colorvec)

#cl_k <- cluster_optimal(dataplot)
#V(dataplot)$color <- membership(cl_k)
#dataplot$palette <- categorical_pal(length(cl_k))
#print(modularity(cl_k))



#Open png om mee te schrijven
png (paste(df$team_name, "IgraphNodes.png", sep = "-"), width = 1350, height = 850)

#plotten van dataplot om de parameters voor de coordinaten (lim) bekend te maken
plot <-plot.igraph(dataplot, 
            edge.width = E(dataplot)$weight^2*0.03,
            vertex.size= degree_total/8,
            layout = coords,
            vertex.color= colorvec,
            vertex.frame.color="black",
            rescale=T)



lim <- par() #plot informatie verkrijgen en dit gebruiken om een rasterimage te plotten
veld <- readPNG("squawkaveld.png")
rasterImage(veld, lim$usr[1]/1.3, lim$usr[3]*1.0, lim$usr[2]/1.3, lim$usr[4]*1.4)

plot <- plot.igraph(dataplot, 
                    edge.width = E(dataplot)$weight^2*0.03,
                    vertex.size= degree_total/8,
                    layout = coords,
                    rescale=T, 
                    vertex.color= colorvec,
                    vertex.frame.color="black",
                    add = T )

title(main="passing network", col.main="black", 
      sub= paste0 (name), col.sub="black", 
      xlab= paste0 (teamname), ylab="data by OPTA",
      col.lab="black", cex.lab= 1.6 )

mtext("node size is passing in & out", side=1, line = 0)
mtext("node color is betweenness ", side=1, line= 1)

fortify(plot)
dev.off()
