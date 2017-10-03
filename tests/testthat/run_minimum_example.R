# load streambugs library:
# ========================
library(streambugs)

# handler to suppress specific warnings from the streambugs library
.streambugs.suppress.warning.handler <- function(w) {
    if(any(grepl("(no (envcond|parameters) for .*|limitation by (\\w)+( \\w+)? not considered)", w))) {
        invokeRestart("muffleWarning")
    }
}

name.run <- "synthetic_example"

# set-up state variables, parameters, input, and output times:
# ============================================================

# this section will have to be replaced for a specific application


# set-up state variable names:
# ----------------------------

n.Reaches         <- 3
n.Habitats        <- 2
n.POM             <- 2
n.Algae           <- 2
n.Invertebrates   <- 5

Reaches           <- paste("Reach",1:n.Reaches,sep="")
Habitats          <- paste("Hab",1:n.Habitats,sep="")
POM               <- paste("POM",1:n.POM,sep="")
Algae             <- paste("Alga",1:n.Algae,sep="")
Invertebrates     <- paste("Invert",1:n.Invertebrates,sep="")

y.names <- character(0)
for ( i in 1:n.Reaches )
{
  for ( j in 1:n.Habitats )
  {
    y.names <-
      c(y.names,
        paste(paste(Reaches[i],Habitats[j],sep="_"),
              c(paste(POM,"POM",sep="_"),
                paste(Algae,"Algae",sep="_"),
                paste(Invertebrates,"Invertebrates",sep="_")),
              sep="_"))
  }
}
if ( length(y.names) != n.Reaches*n.Habitats*(n.POM+n.Algae+n.Invertebrates) ) stop("problem constructing state variables")

# FIXME: used before tests
y.names <- decode.statevarnames(y.names)


# set-up parameters:
# ------------------

par <- numeric(0)

# par["temp_vco"] = 3 + 273.15  # K
# par["temp_col"] = 8 + 273.15  # K
# par["temp_mod"] = 14 + 273.15  # K
# par["temp_war"] = 22 + 273.15  # K

# environmental parameters:

par["w"]                              <- 10            # m
par["Reach1_w"]                       <- 5             # m
par["L"]                              <- 1000          # m
par["T"]                              <- 273.15+20     # K
par["I0"]                             <- 125           # W/m2
par["fshade"]                         <- 0.2           # -
par["Hab1_fshade"]                    <- 0.2           # -
par["CP"]                             <- 0.02          # gP/m3
par["CN"]                             <- 1             # gN/m3
par["tau"]                            <- 0.5             # kg /(s2.m)
par["taucrit"]                        <- 1             # kg /(s2.m)

# initial conditions:

par["POM_Dini"]                       <- 100           # gDM/m2
par["Algae_Dini"]                     <- 100           # gDM/m2
par["Invertebrates_Dini"]             <- 10            # gDM/m2

# basal metabolism and energy content parameters:

par["POM_EC"]                         <- 18000         # J/gDM
par["Algae_M"]                        <- 1e-7          # g
par["Algae_Ea"]                       <- 0.231         # eV
par["Algae_b"]                        <- 0.93          # -
par["Algae_i0"]                       <- 4150455552    # J/a
par["Algae_EC"]                       <- 20000         # J/gDM
par["Invertebrates_M"]                <- 0.001         # g
par["Invertebrates_Ea"]               <- 0.68642       # eV
par["Invertebrates_b"]                <- 0.695071      # -
par["Invertebrates_i0"]               <- 1.10E+16      # J/a
par["Invertebrates_EC"]               <- 22000         # J/gDM
par["Algae_cdet"]                     <- 2             # s4m2/(kg2a)
par["POM_cdet"]                       <- 3             # s4m2/(kg2a)
par["Invertebrates_cdet"]             <- 1             # s4m2/(kg2a)

# generate stoichiometry of mineralization, respitation, death and production:

# POM:
taxa.POM <- unique(y.names$y.taxa[y.names$y.groups=="POM"])
# mineralization:
par[paste("Miner",taxa.POM,taxa.POM,sep="_")] <- -1
# drift:
par[paste("Drift",taxa.POM,taxa.POM,sep="_")] <- -1



# Algae:
taxa.Algae <- unique(y.names$y.taxa[y.names$y.groups=="Algae"])
# respiration:
par[paste("Resp",taxa.Algae,taxa.Algae,sep="_")]  <- -1
# death:
par[paste("Death",taxa.Algae,taxa.Algae,sep="_")] <- -1
par[paste("Death",taxa.Algae,"POM1",sep="_")]     <- 1
# production:
par[paste("Prod",taxa.Algae,taxa.Algae,sep="_")]  <- 1
# drift:
par[paste("Drift",taxa.Algae,taxa.Algae,sep="_")] <- -1


# Invertebrates:
taxa.Invertebrates <- unique(y.names$y.taxa[y.names$y.groups=="Invertebrates"])
# respiration:
par[paste("Resp",taxa.Invertebrates,taxa.Invertebrates,sep="_")]  <- -1
# death:
par[paste("Death",taxa.Invertebrates,taxa.Invertebrates,sep="_")] <- -1
par[paste("Death",taxa.Invertebrates,"POM1",sep="_")]             <- 1

# generate stoichiometry of consumption:

par[paste("Cons",taxa.Invertebrates,"POM1",taxa.Invertebrates,sep="_")] <- 1
par[paste("Cons",taxa.Invertebrates,"POM1","POM1",sep="_")]             <- -1
par[paste("Cons",taxa.Invertebrates,"Alga1",taxa.Invertebrates,sep="_")] <- 1
par[paste("Cons",taxa.Invertebrates,"Alga1","Alga1",sep="_")]             <- -1

# generate stoichiometry of fish predation:

par[paste("FishPred","Fish","Invert1","Invert1",sep="_")] <- -1
par[paste("FishPred","Fish","Invert5","Invert5",sep="_")] <- -1

# drift:
par[paste("Drift",taxa.Invertebrates,taxa.Invertebrates,sep="_")] <- -1


# process parameters:

par["POM_kminer"]                     <- 20            # 1/a

par["Algae_hdens"]                    <- 1000          # gDM/m2
par["Algae_KI"]                       <- 62.1          # W/m2
par["Algae_KP"]                       <- 0.01          # gP/m3
par["Algae_KN"]                       <- 0.1           # gN/m3
par["Algae_fresp"]                    <- 1             # -
par["Algae_fdeath"]                   <- 0.7           # -
par["Algae_fprod"]                    <- 5             # -

par["Invertebrates_hdens"]            <- 10            # gDM/m2
par["Invertebrates_fresp"]            <- 2.5           # -
par["Invertebrates_fdeath"]           <- 0.7           # -
par["Invertebrates_fcons"]            <- 5             # -
par["Invertebrates_Kfood"]            <- 1             # gDM/m2
par["Invertebrates_q"]                <- 2
par["Invertebrates_Pref"]             <- 1             # -

par["DFish"]                          <- 100           # kg/ha
par["Reach2_DFish"]                   <- 200           # kg/ha
par["Fish_cfish"]                     <- 10            # gDM/kg/d
par["Fish_Kfood"]                     <- 1             # gDM/m2
par["Fish_q"]                         <- 1             # -
par["Fish_Pref"]                      <- 1             # -

# set-up inputs:
# --------------

inp <- list(Reach3_w=matrix(c(0:1,10,2),ncol=2,byrow=F))
#inp <- NA

# set-up output times:
# --------------------

tout <- 0:200/100


# run R implementation:
# =====================

# then in tests run:

# sys.def <- streambugs.get.sys.def(y.names=y.names,par=par,inp=inp)
# streambugs.write.sys.def(sys.def,"sysdef_synthetic_example.dat")

# streambugs.debug=2

# calculate results

withCallingHandlers(
  res.R <- run.streambugs(y.names        = y.names,
                          times          = tout,
                          par            = par,
                          inp            = inp,
                          C              = FALSE),
  warning = .streambugs.suppress.warning.handler
)


# res.R <- run.streambugs(y.names        = y.names,
#                         times          = tout,
#                         par            = par,
#                         inp            = inp,
#                         C              = FALSE,
#                         return.res.add = T,
#                         file.def       = "output/streambugs_modelstruct_synth.dat",
#                         file.res       = "output/streambugs_results_synth.dat",
#                         file.add       = "output/streambugs_results_add_synth.dat")

res.R$args = list(
    y.names        = y.names,
    times          = tout,
    par            = par,
    inp            = inp,
    C              = FALSE
)

# then in tests run:

# pdf("output/streambugs_results_synth.pdf",width=8,height=6)
# plot.streambugs(res.R$res,res.R$args$par,res.R$args$inp)
# dev.off()


# run C implementation:
# =====================

# re-compile C routines of streambugs
# after having done modifications of C code:

#compile.streambugs()

# initialize global parameter and parameter name vectors for use in C
# and calculate results:

withCallingHandlers(
  res.C <- run.streambugs(y.names  = y.names,
                          times    = tout,
                          par      = par,
                          inp      = inp,
                          C        = TRUE),
  warning = .streambugs.suppress.warning.handler
)

# res.C <- run.streambugs(y.names  = y.names,
#                         times    = tout,
#                         par      = par,
#                         inp      = inp,
#                         C        = TRUE,
#                         file.def = "output/streambugs_modelstruct_synth.dat",
#                         file.res = "output/streambugs_results_synth_C.dat")

res.C$args = list(
    y.names  = y.names,
    times    = tout,
    par      = par,
    inp      = inp,
    C        = TRUE
)

# then in tests run:

# pdf("streambugs_results_synth_C.pdf",width=8,height=6)
# plot.streambugs(res.C$res,res.C$args$par,res.C$args$inp)
# dev.off()


########

# res.add <- res.R$res.add
#
# source("libraries/construct_matrix_res_add_t.r")
# source("libraries/get_res_add_t.r")
#
# res.add.t <- construct.matrix.res.add.t(res.add,t=tout[length(tout)],y.names,
#                                         file=paste("output/res_add_tend_synth",".dat",sep=""))
#
# factors.all.inverts <- get.res.add.t.invertebrates(res.add.t,
#                                                    y.names.invertebrates=
#                                                      y.names$y.names[which(y.names$y.groups=="Invertebrates")])
# factors.all.inverts
