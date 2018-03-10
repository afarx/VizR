#library(tidyverse)
library(stringr)
library(readr)
library(dplyr)
library(tibble)
library(reshape2)

# Load data
letters <- read_csv("E://networkViz//GEPHI//edge.csv",locale=locale(encoding = "GB18030"))

################################
## Create node and edge lists ##
################################

### Node list ###
sources <- letters %>%
  distinct(Source) %>%
  rename(label = Source)

destinations <- letters %>%
  distinct(Target) %>%
  rename(label = Target)

nodes <- full_join(sources, destinations, by = "label")

# Create id column and reorder columns
nodes <- nodes %>% rowid_to_column("id")

### Edge list ###
per_route <- letters %>%  
  select(Source, Target,weight)

# Join with node ids and reorder columns
edges <- per_route %>% 
  left_join(nodes, by = c("Source" = "label")) %>% 
  rename(from = id)

edges <- edges %>% 
  left_join(nodes, by = c("Target" = "label")) %>% 
  rename(to = id)

edges <- select(edges, from, to, weight)

########################
## Interactive network##
########################
library(visNetwork)
library(networkD3)
library(igraph)

routes_igraph <- graph_from_data_frame(d = edges, vertices = nodes, directed = FALSE)
nodes$bte <- betweenness(routes_igraph, directed = F)

quantile(nodes$bte,probs=0.95)
nodes$size <- car::recode(nodes$bte,"0=1;0:100=2;100:200=3;200:1000=4;else=10")

#extract group by first.name
nodes <- mutate(nodes, group = str_sub(label,0,1))

nodes$group <- car::recode(nodes$group,
                           "'顾'='顾';else='世家'")

edges <- mutate(edges, width=weight/5)

nodes_d3 <- mutate(nodes, id = id - 1)
edges_d3 <- mutate(edges, from = from - 1, to = to - 1)

#Cols <- car::recode(edges$weight,"1='#663399';2='#CC0000';3='#00CCFF';
#4='#33FF00';5='#663300F';6='#999999'")
nodes$description <- paste("This is a description of", nodes$label)
clickJS <- "
d3.selectAll('.xtooltip').remove(); 
d3.select('body').append('div')
.attr('class', 'xtooltip')
.style('position', 'fixed')
.style('border-radius', '0px')
.style('padding', '5px')
.style('opacity', '0.85')
.style('background-color', '#161823')
.style('box-shadow', '2px 2px 6px #161823')
.html('name: ' + d.description + '<br>' + 'group: ' + d.group)
.style('right', '50px')
.style('bottom', '50px')
.style('color', d3.select(this).style('fill'))
;
"


Cols <- car::recode(edges$weight,"1:6='#A78E44'")

fn <- forceNetwork(Links = edges_d3, Nodes = nodes_d3, Source = "from", Target = "to", 
                   NodeID = "label", Group = "group", Value = "width", fontFamily = "黑体",
                   opacity = 1, fontSize = 16, zoom = F,charge=-50,bounded=T,
                   legend=T,arrows = F, Nodesize = "size",linkColour = Cols,
                   opacityNoHover = 1,radiusCalculation = JS(" d.nodesize"), 
                   #colourScale = JS("d3.scaleOrdinal(d3.schemeCategory20c);"),
                   ColourScale <- 'd3.scaleOrdinal()
                   .domain(["顾", "世家"])
                   .range(["#FF6900", "#694489"]);',
                   width = 1120, height = 630, clickAction= clickJS)
fn$x$nodes$description <- nodes$description

library(htmltools)
library(crosstalk)


sd <- SharedData$new(nodes, key=~label, group="grp1" )

# no autocomplete so not the same
#  but will use this instead of writing something new
fs <- filter_select(
  id = "filter-node",
  label = "Search",
  sharedData = sd,
  group = ~label
)

fn <- htmlwidgets::onRender(
  fn,
  '
  function(el,x){
  // get the crosstalk group
  //  we used grp1 in the SharedData from R
  var ct_grp = crosstalk.group("grp1");
  debugger;
  ct_grp
  .var("filter")
  .on("change", function(val){searchNode(val.value)});
  
  function searchNode(filter_nodes) {
  debugger;
  //find the node
  var selectedVal = filter_nodes? filter_nodes : [];
  var svg = d3.select(el).select("svg");
  var node = d3.select(el).selectAll(".node");
  
  if (selectedVal.length===0) {
  node.style("opacity", "1");
  svg.selectAll(".link").style("opacity","1");
  } else {
  var selected = node.filter(function (d, i) {
  return selectedVal.indexOf(d.name) >= 0;
  });
  node.style("opacity","0.1");
  selected.style("opacity", "1");
  var link = svg.selectAll(".link").style("opacity", "0.1");
  /*
  svg.selectAll(".node, .link").transition()
  .duration(5000)
  .style("opacity", 1);
  */
  }
  }
  }  
  '
)



browsable(
  tagList(
    tags$head(
      tags$style('body{background-color: #161823 !important}',
                 '.legend text{fill: #FFFFFF}',
                 '.divclass {width: 300px;}')
    ),
    div(class='divclass',fs),
    fn
  )
)


