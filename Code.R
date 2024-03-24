# The data sets used: "alliance_v4.1_by_dyad.csv" from Formal Alliances (v4.1) and "NMC-60-abridged.csv" from National Material Capabilities (v6.0)
# on https://correlatesofwar.org/data-sets.


## Preparation
rm(list=ls())
library(tidyverse)
library(sna) 
library(igraph)
library(ggplot2)
library(kableExtra)
library(network)
## Load the datasets
alliance <- read.csv(file = "yourwd/alliance_v4.1_by_dyad.csv")
power <- read.csv(file=  "yourwd/NMC-60-abridged.csv")

# Filter all the alliances forged between 1816 to 1836(20 years)
alliance <- alliance[order(alliance$dyad_st_year),] |> 
  filter(1815 < dyad_st_year & dyad_st_year < 1837, 
         asymmetric == 0) # terms applies to both states
# Create a data frame containing information about edges based on the new alliance data frame
edges <- alliance |> 
  select(state_name1,state_name2) 
# Check how many and which countries we got
 table(edges$state_name1)
 table(edges$state_name2)

# Filter the power data frame
# limite age(1816-1836) and countries(19 based on table() in the last step) 
# We focus on "milper"(Military Personnel (thousands)) here
power <- power[order(power$year),] |> 
  filter(1815 < year & year < 1837,
         stateabb == "AUH"|stateabb == "BAD"|stateabb == "FRN"|stateabb == "GMY"|stateabb == "HSE"|stateabb == "HSG"|stateabb == "ITA"|stateabb == "POR"|stateabb == "RUS"|stateabb == "SAX"|stateabb == "SPN"|stateabb == "SWD"|stateabb == "TUR"|stateabb == "TUS"|stateabb == "SIC"|stateabb == "WRT"| stateabb == "BAV"|stateabb == "NTH"|stateabb == "UKG") |> 
  select(stateabb,year,milper,irst)
# Change the state names in edges. Use state abbreviations for simplification(as Power contains only state abbreviations)
edges <- edges |> 
  mutate(state_name1 = case_when(state_name1 == "Austria-Hungary" ~ "AUH",
                                 state_name1 == "Baden" ~ "BAD",
                                 state_name1 == "France" ~ "FRN",
                                 state_name1 == "Germany" ~ "GMY",
                                 state_name1 == "Hesse Electoral"~ "HSE" ,
                                 state_name1 == "Hesse Grand Ducal" ~ "HSG",
                                 state_name1 == "Italy"~ "ITA" ,
                                 state_name1 == "Portugal" ~ "POR",
                                 state_name1 == "Russia" ~ "RUS",
                                 state_name1 == "Saxony"~ "SAX", 
                                 state_name1 == "Spain" ~ "SPN",
                                 state_name1 == "Sweden" ~ "SWD",
                                 state_name1 == "Turkey" ~ "TUR",
                                 state_name1 == "Tuscany" ~ "TUS",
                                 state_name1 == "Two Sicilies" ~ "SIC",
                                 state_name1 == "Wuerttemburg" ~ "WRT",
                                 state_name1 == "Bavaria" ~ "BAV",
                                 state_name1 == "Netherlands" ~ "NTH",
                                 TRUE ~ "UKG"),
         state_name2 = case_when(state_name2 == "Austria-Hungary" ~ "AUH",
                                 state_name2 == "Baden" ~ "BAD",
                                 state_name2 == "France" ~ "FRN",
                                 state_name2 == "Germany" ~ "GMY",
                                 state_name2 == "Hesse Electoral"~ "HSE" ,
                                 state_name2 == "Hesse Grand Ducal" ~ "HSG",
                                 state_name2 == "Italy"~ "ITA" ,
                                 state_name2 == "Portugal" ~ "POR",
                                 state_name2 == "Russia" ~ "RUS",
                                 state_name2 == "Saxony"~ "SAX", 
                                 state_name2 == "Spain" ~ "SPN",
                                 state_name2 == "Sweden" ~ "SWD",
                                 state_name2 == "Turkey" ~ "TUR",
                                 state_name2 == "Tuscany" ~ "TUS",
                                 state_name2 == "Two Sicilies" ~ "SIC",
                                 state_name2 == "Wuerttemburg" ~ "WRT",
                                 state_name2 == "Bavaria" ~ "BAV",
                                 state_name2 == "Netherlands" ~ "NTH",
                                 TRUE ~ "UKG"))

# Replace all values in power that equals to -9 with NA as suggested by the codebook
power <- transform(power, milper = ifelse(milper == -9, NA, milper))
power <- transform(power, irst = ifelse(irst == -9, NA, irst))
# Check the change in military personnel and iron & steel production of each country in 1816-1836
ggplot(power,mapping = aes(x = year, y = milper))+
  geom_point(mapping = aes(color = stateabb)) +
 labs(
   title = "Military personnel of each country(1816-1836)",
   x = "year",
   y = "thousands",
   color = "country",
 )
ggplot(power,mapping = aes(x = year, y = irst))+
 geom_point(mapping = aes(color = stateabb)) +
   labs(
   title = "Iron and steel production (1816-1836)",
   x = "year",
   y = "thousands of tons",
   color = "country",
 )
# store the average military personnel in a column named "avg_milper"
# Store the average Iron and steel production(thousands of tons) in a column named "avg_irst"
# This new data frame will contain information on nodes(countries in our case)
nodes <- power |> 
  group_by(stateabb) |>  
  summarise(avg_milper = round(mean(milper,na.rm = TRUE,1)),
            avg_irst = round(mean(irst,na.rm = TRUE),1))

# Check that there are no duplicated subjects
length(unique(nodes$stateabb)) == nrow(nodes)
# Check there is no duplicated tie
any(duplicated(edges))
# Make sure the two sides of all the alliance are in our node set
all(edges$state_name1 %in% nodes$stateabb)
all(edges$state_name2 %in% nodes$stateabb)


## 2 Make a table
# Conversion to igraph format
grph <- graph.edgelist(as.matrix(edges),directed = FALSE)
# number of nodes and ties
n_n <- vcount(grph) 
n_t <- ecount(grph) 
# average degree
mean_deg <- mean(degree(grph,mode='in')) |> 
  round(4)# the network is undirected as it only shows two countries that
# forged an alliance together without a specific issuers
# Standard deviations
sd <- sd(degree(grph,mode='in')) |> 
  round(4)
# density
den <- edge_density(grph) |> 
  round(4)
# reciprocity
rec <-reciprocity(grph)
# transitivity
tran <- transitivity(grph) |> 
  round(4)
# average path length
pathleg <- average.path.length(grph) |> 
  round(4)
# isolates
n_iso <- sum(degree(grph) == 0) 
# components
n_com <- components(grph)$no 
# Assortativity(degree)
as <- assortativity.degree(grph) |> 
  round(4)
# which has the highest degree
max_deg <- V(grph)$name[degree(grph)==max(degree(grph))] 
# which has the highest betweenness centrality
max_betw <- V(grph)$name[betweenness(grph)==max(betweenness(grph))] 

df <- data.frame("Object" = c("Historical period covered","Major events","Nature of tie(s)","Number of nodes","Number of ties","Direction of ties","Average degree","Standard deviations","Density","Reciprocity","Transitivity","Average path length","Number of isolates","Number of components","Assortativity(degree)","Node with the highest degree","Node with the highest betweenness centrality"),
                 "Index" = c("1816-1836","The Congress of Vienna & The Industrial Revolution","Alliances between countries",n_n,n_t,"Undirected",mean_deg,sd,den,rec,tran,pathleg,n_iso,n_com,as,max_deg,max_betw))
kable(df,format = "latex", caption = "Descriptive statistics of alliance network(1816-1836)", align = "c") |> 
  add_footnote("Data source: Formal Alliances (v4.1) & National Material Capabilities (v6.0)") |> 
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                position = "center",
                latex_options = "hold_position")


## 3 Visualize the network
# Match the names in the graph with the names(stateabb) in nodes
# Add nodes' attributes(average military personal)
V(grph)$avg_milper <- nodes[match(V(grph)$name,nodes$stateabb),]$avg_milper
V(grph)$avg_irst <- nodes[match(V(grph)$name,nodes$stateabb),]$avg_irst
# Plot
# Let the size of vertex be based on avg_milper
# Use sqrt() and +1 here to make the graph clearer(use V(grph)$avg_milper directly would make the plot messy)
plot.igraph(grph,
            vertex.label= V(grph)$name,vertex.size= sqrt(V(grph)$avg_milper)+1,vertex.color = ifelse(V(grph)$avg_irst  == 0,"red",ifelse(V(grph)$avg_irst <=  50, "grey",ifelse(V(grph)$avg_irst <= 100,"green",ifelse(V(grph)$avg_irst <= 150,"darkgreen",ifelse(V(grph)$avg_irst <= 200,"lightblue",ifelse(V(grph)$avg_irst <= 250,"blue","purple")))))),
            edge.color='darkgrey',edge.arrow.size=.02,
            vertex.label.cex = 0.7,
            layout=layout_with_kk(grph),
            main='Graph 1: Alliances forged between 1816-1836
            (Vertex size based on average military personal)')
legend("left", legend = c('0','<=50','<=100','<=150','<=200','<=250','>250'), 
       pch=21, pt.bg=c("red","grey","green","darkgreen","lightblue","blue","purple"),title = "Iron and steel
       production
       (thousands of tons)",cex = 0.5)


## 4 Two different community detection algorithms
par(mfrow=c(1,2))
# GIRVAN-NEWMAN
lay <- layout_with_fr(grph)
plot(grph,
     vertex.label.cex = 0.5,
     main='Girvan-Newman',
     mark.groups = cluster_edge_betweenness(grph),
     layout = lay)

# Walktrap
plot(grph,
     vertex.label.cex = 0.5,
     main='Walktrap',
     mark.groups = cluster_walktrap(grph,step=5),
     layout = lay)


## 5 A blockmodel analysis using structural equivalence
# Convert grph to matrix format
mtx <- as.matrix(get.adjacency(grph))
# Convert matrix to network
net <- as.network(mtx, directed=FALSE)
gplot(net,displaylabels = TRUE, arrowhead.cex = FALSE)
# Structural dissimilarity
SE_dist <- sedist(net)
SE_dist
# Cluster Dendrogram
net_clustering <- equiv.clust(net,
                              equiv.fun="sedist",cluster.method='complete')
plot(net_clustering)

# Try partition with 4 positions
se.blockmodel.A <- blockmodel(net, net_clustering, k=4)
# Matching block membership information with the original data set
se.position.A <- 1+se.blockmodel.A$block.membership[match(1:nrow(mtx),se.blockmodel.A$order.vector)]
# Plot(color based on membership)
gplot(net, vertex.col=se.position.A,displaylabels = TRUE, arrowhead.cex = FALSE)
# Blockmodel
plot(se.blockmodel.A)
