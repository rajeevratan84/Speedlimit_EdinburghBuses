### This file takes HyperStar outputs and builds a two-branch Uppaal Model
### output file is called output.xml
### when saving HyperStar outputfiles, name them as "1", "2",...

library(MASS)


source("split_fraction.R")

setwd("C:/data")
fn <- "output.xml"
if (file.exists(fn)) file.remove(fn)

file.create("output.xml")

current_directory <- getwd()  

##### SET DIRECTORY PATH OF THE HYPERSTAR OUTPUT FILES
setwd("C:/patch/31/Speedlimit On/Polton to East Craig (Day)/HyperStar")


s <- function() structure(list(patchID = numeric(), 
                               timeinpatch = numeric(), 
                               Direction = character()), 
                          class = "data.frame")
All_hyper <- s()
HyperStarOutputs <- s()
hyper_list <- list.files()
hyper_list <- as.numeric(hyper_list)

hyper_list <- sort(hyper_list, decreasing = TRUE)

for(i in 1:length(hyper_list)){
  hyper <- read.table(as.character(hyper_list[i]))
  hyper <- as.data.frame(t(hyper))
  hyper$V1 <- hyper_list[i]
  All_hyper <- rbind(All_hyper, hyper)
}
All_hyper <- All_hyper[c(1,6,7,2,3,4,5)]

names(All_hyper)[1] <- "Patch"
names(All_hyper)[2] <- "a1"
names(All_hyper)[3] <- "a2"
names(All_hyper)[4] <- "k1"
names(All_hyper)[5] <- "k2"
names(All_hyper)[6] <- "l1"
names(All_hyper)[7] <- "l2"
setwd(current_directory)
All_hyper < as.data.frame(All_hyper)
rownames(All_hyper) <- NULL
hyperparam <- All_hyper


names(hyperparam)[2] <- "Alpha_1"
names(hyperparam)[3] <- "Alpha_2"
names(hyperparam)[6] <- "Lamda_1"
names(hyperparam)[7] <- "Lamda_2"
hyperparam$Time_Contraint_K1 <- 0
hyperparam$Time_Contraint_K2 <- 0
hyperparam$Set_Limit <- "No"

x_end_dist <- (sum(hyperparam$k1)+sum(hyperparam$k2))*100

#set up writing
logFile = "output.xml"
cat("<?xml version=\"1.0\" encoding=\"utf-8\"?>
<!DOCTYPE nta PUBLIC '-//Uppaal Team//DTD Flat System 1.1//EN' 'http://www.it.uu.se/research/group/darts/uppaal/flat-1_2.dtd'>
  <nta>
    <declaration>// Place global declarations here.</declaration>
    <template>
    \t<name x=\"5\" y=\"5\">Template</name>
    \t<declaration>// Place global declarations here.
clock x;</declaration>
    \t<location id=\"id0\" x=\"",x_end_dist,"\" y=\"100\">
		\t<name x=\"",x_end_dist+50,"\" y=\"80\">end</name>
    \t</location>\n", file=logFile, append=TRUE, sep = "")

  j=1
  y <- 0
  id <- 1
  x1 <- 0
  x2 <- 0
#Add K Nodes  
 branch1_x <- 175
 branch2_x <- 175
 branch_node_x <- 0
 first_k1 <- 0
 first_k2 <- 0
 last_k1 <- 0
 last_k2 <- 0
 k_branch_id <- 0
 c <- 1
 
for(j in 1:nrow(hyperparam)){
  ##First Branch
  for(i in 1:hyperparam$k1[j]){
    if(i==1){first_k1[j] = id}
    
    if(hyperparam$Set_Limit[j] == "Yes"){    
      x1 <- (i*100)+ branch1_x
      cat("X1 - ",x1,"\n")
      cat("		<location id=\"id",(id),"\" x=\"",x1,"\" y=\"",(y-10),"\">
          \t<label kind=\"exponentialrate\" x=\"",x1,"\" y=\"",(y-10),"\">",hyperparam$Lamda_1[j],"</label>
          \t</location>\n", file=logFile, append=TRUE, sep = "")
      last_k1[j] <- id
      k_branch_id[c] <- id
      c <- c + 1
      id <- id + 1
    }
    else{
      x1 <- (i*100)+ branch1_x
      cat("X1 - ",x1,"\n")
      cat("		<location id=\"id",(id),"\" x=\"",x1,"\" y=\"",(y-10),"\">
  		\t<label kind=\"exponentialrate\" x=\"",x1,"\" y=\"",(y-10),"\">",hyperparam$Lamda_1[j],"</label>
      \t</location>\n", file=logFile, append=TRUE, sep = "")
      last_k1[j] <- id
      k_branch_id[c] <- id
      c <- c + 1
      id <- id + 1
    }
  }
  
  #last_k1 <- (id-1)
  x2 < x2 + 100
  ##Second Branch
  for(k in 1:hyperparam$k2[j]){
    x2 <- (k*100) + branch2_x
    if(k==1){first_k2[j] = id;x2 < x2 + 75;}
    
    if(hyperparam$Set_Limit[j] == "Yes"){    
      cat("X2 - ",x2,"\n")
      #cat("NOT SUPPOSED TO SEE ME!!!\n")
      cat("		<location id=\"id",id,"\" x=\"",x2,"\" y=\"",(y+200),"\">
          \t<label kind=\"exponentialrate\" x=\"",x2,"\" y=\"",(y+190),"\">",hyperparam$Lamda_2[j],"</label>
      \t</location>\n", file=logFile, append=TRUE, sep = "")
      last_k2[j] <- id
      k_branch_id[c] <- id
      c <- c + 1
      id <- id + 1
    }
    else{
      cat("X2 - ",x2,"\n")
      cat("		<location id=\"id",id,"\" x=\"",x2,"\" y=\"",(y+200),"\">
          \t<label kind=\"exponentialrate\" x=\"",x2,"\" y=\"",(y+190),"\">",hyperparam$Lamda_2[j],"</label>
          \t</location>\n", file=logFile, append=TRUE, sep = "")
      last_k2[j] <- id
      k_branch_id[c] <- id
      c <- c + 1
      id <- id + 1     
      
    }
    
     
  }  
  branch2_x <- max(x1,x2)+100
  branch1_x <- max(x1,x2)+100
  branch_node_x[j] <- max(branch1_x,branch2_x) + 20 
  #last_k2 <- (id-1)
} 


###Add Start Node 
start_id <- id
cat("  \t\t<location id=\"id",start_id,"\" x=\"70\" y=\"100\">
  \t\t\t<name x=\"50\" y=\"70\">start</name>
    \t\t<label kind=\"exponentialrate\" x=\"70\" y=\"110\">1</label>
  \t\t</location> 
    ", file=logFile, append=TRUE, sep = "")


###Add Branch Nodes  
x_start <- 135
branch_node <- 0
#bnode <- 0
for(j in 1:nrow(hyperparam)){ 
  id <- id + 1
  cat("\t<branchpoint id=\"id",id,"\" x=\"",x_start,"\" y=\"100\">
	\t</branchpoint>
    ", file=logFile, append=TRUE, sep = "")
  x_start <- branch_node_x[j]
  branch_node[j] = paste0("id",id)
  #bnode[j] <- x_start 
}

### Initialize Start Node
cat("\t<init ref=\"id",start_id,"\"/>\n", file = logFile, append = TRUE, sep = "")

##Start Connecting our nodes
##Connect Start for first probability branch
if(hyperparam$Set_Limit[1] == "Yes"){
  cat("	\t<transition>
      \t<source ref=\"id",start_id,"\"/>
      \t\t<target ref=\"",branch_node[1],"\"/>
			<label kind=\"assignment\" x=\"175\" y=\"32\">x=0</label>
      <label kind=\"assignment\" x=\"",x_start,"\" y=\"110\">x=0</label>
      \t\t</transition>",file = logFile, append = TRUE, sep = "") 
}
if(hyperparam$Set_Limit[1] == "No"){
  cat("	\t<transition>
      \t<source ref=\"id",start_id,"\"/>
      \t\t<target ref=\"",branch_node[1],"\"/>
      \t\t</transition>",file = logFile, append = TRUE, sep = "")
}


##Covert Probabilities to Fractions for Uppaal
library(MASS)
x_start <- 135


for(i in 1:nrow(hyperparam)){ 
  alpha <- hyperparam$Alpha_1[i]
  alpha <- as.data.frame(split_fraction(alpha))
  
  #Start of K Branchs
  if(hyperparam$Set_Limit[i] == "Yes"){
  cat("\n		<transition>
  			<source ref=\"",branch_node[i],"\"/>
      \t\t<target ref=\"id",first_k1[i],"\"/>
      <label kind=\"assignment\" x=\"",x_start,"\" y=\"90\">x=0</label>
      \t\t<label kind=\"probability\" x=\"",x_start,"\" y=\"50\">",alpha[1,],"</label>
      \t</transition>
      \t<transition>
      \t\t<source ref=\"",branch_node[i],"\"/>
      \t\t<target ref=\"id",first_k2[i],"\"/>
      <label kind=\"assignment\" x=\"",x_start,"\" y=\"110\">x=0</label>
      \t\t<label kind=\"probability\" x=\"",x_start,"\" y=\"150\">",alpha[2,],"</label>
      \t</transition>",file = logFile, append = TRUE, sep = "")
  x_start <- branch_node_x[i]
  }
  else{
    cat("\n		<transition>
        <source ref=\"",branch_node[i],"\"/>
        \t\t<target ref=\"id",first_k1[i],"\"/>
        \t\t<label kind=\"probability\" x=\"",x_start,"\" y=\"50\">",alpha[1,],"</label>
        \t</transition>
        \t<transition>
        \t\t<source ref=\"",branch_node[i],"\"/>
        \t\t<target ref=\"id",first_k2[i],"\"/>
        \t\t<label kind=\"probability\" x=\"",x_start,"\" y=\"150\">",alpha[2,],"</label>
        \t</transition>",file = logFile, append = TRUE, sep = "")
    x_start <- branch_node_x[i]
  }
}


for(i in 1:(nrow(hyperparam)-1)){ 
  j <- i+1
  if(hyperparam$Set_Limit[j-1] == "Yes"){
  #Ends of K Branchs
  cat("\n		<transition>
      \t\t<source ref=\"id",last_k1[i],"\"/>
      \t\t<target ref=\"",branch_node[j],"\"/>
			    <label kind=\"guard\" x=\"-135\" y=\"-34\">x&gt;=",hyperparam$Time_Contraint_K1[j-1],"</label>
    \t</transition>
    \t<transition>
      \t\t<source ref=\"id",last_k2[i],"\"/>
      \t\t<target ref=\"",branch_node[j],"\"/>
			    <label kind=\"guard\" x=\"-135\" y=\"-34\">x&gt;=",hyperparam$Time_Contraint_K2[j-1],"</label>
    \t</transition>",file = logFile, append = TRUE, sep = "")
  }
  else{
    #Ends of K Branchs
    cat("\n		<transition>
        \t\t<source ref=\"id",last_k1[i],"\"/>
        \t\t<target ref=\"",branch_node[j],"\"/>
        \t</transition>
        \t<transition>
        \t\t<source ref=\"id",last_k2[i],"\"/>
        \t\t<target ref=\"",branch_node[j],"\"/>
        \t</transition>",file = logFile, append = TRUE, sep = "")
  }
}
k_id <- as.data.frame(k_branch_id)


###Connect Phases (k) in each branch
nc <-1

for(j in 1:nrow(hyperparam)){
  ##First Branch
  for(i in 1:(hyperparam$k1[j]-1)){
    if(hyperparam$Set_Limit[j] == "Yes"){
      cat("X1 ",nc,"-",(nc+1),"\n")
      cat("\n		<transition>
  			<source ref=\"id",k_id[nc,],"\"/>
          \t<target ref=\"id",k_id[(nc+1),],"\"/>
			    <label kind=\"guard\" x=\"-135\" y=\"-34\">x&gt;=",hyperparam$Time_Contraint_K1[j],"</label>
			    <label kind=\"assignment\" x=\"175\" y=\"32\">x=0</label>
          </transition>",file = logFile, append = TRUE, sep = "")
      #cat("X1",k_id[nc,],"\n")
      nc <- nc + 1
    }
    else{
      cat("X1 ",nc,"-",(nc+1),"\n")
      cat("\n		<transition>
        <source ref=\"id",k_id[nc,],"\"/>
        \t<target ref=\"id",k_id[(nc+1),],"\"/>
        </transition>",file = logFile, append = TRUE, sep = "")
    #cat("X1",k_id[nc,],"\n")
    nc <- nc + 1
    }
  }

  
  ##Second Branch
  nc <- nc + 1
  for(k in 1:(hyperparam$k2[j]-1)){
    if(hyperparam$Set_Limit[j] == "Yes"){
      cat("X2 ",nc,"-",(nc+1),"\n")
      cat("\n		<transition>
  			<source ref=\"id",k_id[nc,],"\"/>
          \t<target ref=\"id",k_id[(nc+1),],"\"/>
			    <label kind=\"guard\" x=\"-135\" y=\"-34\">x&gt;=",hyperparam$Time_Contraint_K2[j],"</label>
			    <label kind=\"assignment\" x=\"175\" y=\"32\">x=0</label>
          </transition>",file = logFile, append = TRUE, sep = "")
      #cat("X2",k_id[nc,],"\n")
      nc <- nc + 1
    }
    else{
      cat("X2 ",nc,"-",(nc+1),"\n")
      cat("\n		<transition>
          <source ref=\"id",k_id[nc,],"\"/>
          \t<target ref=\"id",k_id[(nc+1),],"\"/>
          </transition>",file = logFile, append = TRUE, sep = "")
      #cat("X2",k_id[nc,],"\n")
      nc <- nc + 1
    }
  }  
  nc <- nc + 1
} 

num_k <- length(last_k1)

##Connect last nodes to End
cat("\n		<transition>
			<source ref=\"id",last_k1[num_k],"\"/>
    \t<target ref=\"id0\"/>
    </transition>",file = logFile, append = TRUE, sep = "")
cat("\n		<transition>
			<source ref=\"id",last_k2[num_k],"\"/>
    \t<target ref=\"id0\"/>
    </transition>",file = logFile, append = TRUE, sep = "")


##End File
cat("
	</template>
    <system>// Place template instantiations here.
  Process = Template();
  // List one or more processes to be composed into a system.
  system Process;
    </system>
    <queries>
    \t<query>
    \t\t<formula>Pr[&lt;=2160] (&lt;&gt; Process.end) 
    \t\t</formula>
    \t\t<comment>
    \t\t</comment>
    </query>
  </queries>
</nta>",file = logFile, append = TRUE, sep = "")