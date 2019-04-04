#extended example

library(streambugs)

name.run <- "ext_example"

#### State variables: #### 
Invertebrates     <- read.table("sourcepooltaxa_extended_example.dat",sep="\t",skip=1) 
Invertebrates     <- c(t(Invertebrates))
POM               <- c("FPOM","CPOM") # fine and coarse particulate organic matter
Algae <- c("crustyAlgae","filamentousAlgae")

Reaches <- c(108,174,109,172,173,442) 
Habitats <- "hab1"

y.names <- construct.statevariables(Reaches=Reaches,Habitats=Habitats,POM=POM,Algae=Algae,Invertebrates=Invertebrates)
y.names <- decode.statevarnames(y.names)

#### Parameters and inputs: #### 
# Define "parameters" (model parameters and environmental conditions, which are constant over time) 
# and "inputs" (can be model parameters and environmental conditions that vary over time)

pars <- read.table("pars_extended_example.dat",sep="\t")
par <- pars[,2]
names(par) <- pars[,1]

inp <- NA

#### output time points in years: ####
tout <- seq(0,10,by=0.05)

#### calculate results: ####
res <- run.streambugs(y.names        = y.names,
                        times          = tout,
                        par            = par,
                        inp            = inp,
                        C              = TRUE)

#### plot results: ####

# S3 class is not set for the output of run.streambugs

class(res$res)="streambugs"
plot(x=res$res,y=par, inp=inp) # 
# streambugs:::plot.streambugs(x=res$res,y=par,inp=inp) # always works for functions that are not exported

#### document model definition ####
sys.def <- streambugs.get.sys.def(y.names,par,inp)
streambugs.write.sys.def(sys.def,file="sysdef_extexample.dat")

####  
source("plot_foodweb.r") # check line 43: I had to made not exported function available ":::"

#pdf(paste("complete_foodwebs",name.run,".pdf",sep=""),width=9,height=5,onefile=T)
plot.foodweb(y.names,pars=par,cex=1.1,title="complete foodweb",ncrit=8,
             lcrit=7,lwd=2,bg=NA,lcol=colors()[555],font=2) #font=2,

plot.foodweb(y.names,pars=par,cex=1.1,title="complete foodweb",ncrit=8,
             lwd=2,lcol=grey(0.5),bg=NA,texts=F,pointcol=T) #font=2,
#dev.off()


### number of feeding links: ####
# only works if par is consistent with states)
stoich.Cons  <- streambugs:::get.par.stoich.web("Cons" ,par) 
nlinks <- 0
for(i in 1:length(stoich.Cons)){
  nlinks <- nlinks+length(stoich.Cons[[i]])
}
cat("number of feeding links: ", nlinks)

# safer alternative ####
stoich.Cons  <- streambugs:::get.par.stoich.web("Cons" ,par) #xxx
foods <-  list()
for ( i in 1:length(y.names$taxa) ){
  foods[[y.names$taxa[i]]] <- names(stoich.Cons[[y.names$taxa[i]]])
} 
nlinks <- length(unlist(foods))
cat("number of feeding links: ", nlinks)


