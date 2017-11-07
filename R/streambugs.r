################################################################
#
# streambugs 1.0
# =================
#
# creation:      06.07.2012
# modifications: 06.09.2013
#
################################################################

C.NA.int <- -999
C.NA.double <- -999

# ==============================================================
# load required resources
# ==============================================================


# library for ordinary differential equation solvers:
# ---------------------------------------------------

library("deSolve")


# implementation of rhs of streambugs differential equations:
# -----------------------------------------------------------

# MR, Unnecessary in pkg
# source("library/core/streambugs_rhs.r")


# auxiliary functions:
# --------------------

# MR, Unnecessary in pkg
# source("library/core/streambugs_aux.r")

# # ==============================================================
# # function to (re-)compile the C routines for streambugs
# # ==============================================================


# MR, Unnecessary in pkg
# compile.streambugs <- function()
# {
#   if ( is.loaded("streambugs_rhs") )
#   {
#     dyn.unload(paste("library/core/streambugs_rhs",.Platform$dynlib.ext,sep=""))
#   }
#   errcode <- system("R CMD SHLIB library/core/streambugs_rhs.c")
#   cat("compilation error code:",errcode,"(zero indicates no error)\n")
#   if ( errcode == 0 )
#   {
#     dyn.load(paste("library/core/streambugs_rhs",.Platform$dynlib.ext,sep=""))
#     cat("C routines of streambugs successfully compiled\n")
#     cat("(or dates checked for no need of re-compilation)\n")
#   }
#   else
#   {
#     cat("*** problems during compilation of C routines of streambugs ***\n")
#   }
# }


# ==============================================================
# function to load C resources and allocate and initialize
# global variables
# ==============================================================


init.streambugs.C <- function(sys.def)
{
  # MR, Unnecessary in pkg
  # load dynamic library of compiled C functions

  # dyn.load(paste("library/core/streambugs_rhs",.Platform$dynlib.ext,sep=""))

  # initialize debug indicator:

  if ( !exists("streambugs.debug") ) streambugs.debug <- 0
  c_streambugs_init_debug(streambugs.debug)


  # initialize input structure:

  ninp <- length(sys.def$inp)
  if ( !is.list(sys.def$inp) ) ninp <- 0
  c_streambugs_create_input_structure(ninp)
  if ( ninp > 0 )
  {
    for ( i in 1:ninp )
    {
      n    <- nrow(sys.def$inp[[i]])
      x    <- sys.def$inp[[i]][,1]
      y    <- sys.def$inp[[i]][,2]
      c_streambugs_create_input(i, n, x, y)
    }
  }

  # initialize global parameters:

  nind <- nrow(sys.def$par.global$inpinds)
  inds <- as.integer(sys.def$par.global$inpinds)
  inds <- ifelse(is.na(inds),C.NA.int,inds)
  npar <- length(sys.def$par.global$parvals)
  nams <- names(sys.def$par.global$parvals)
  vals <- as.double(sys.def$par.global$parvals)
  vals <- ifelse(is.na(vals),C.NA.double,vals)
  c_streambugs_create_parglobal(nind, inds, npar, vals, nams)

  # initialize global parameters related to environmental conditions and traits:

  ntrait <- length(sys.def$par.global.envtraits)
  if ( !is.list(sys.def$par.global.envtraits) ) ntrait <- 0
  c_streambugs_create_parglobalenvtraits_structure(ntrait)
  if ( ntrait > 0 )
  {
    for ( i in 1:ntrait )
    {
      name <- names(sys.def$par.global.envtraits)[i]
      nind <- nrow(sys.def$par.global.envtraits[[i]]$inpinds)
      inds <- as.integer(sys.def$par.global.envtraits[[i]]$inpinds)
      inds <- ifelse(is.na(inds),C.NA.int,inds)
      npar <- length(sys.def$par.global.envtraits[[i]]$parvals)
      nams <- names(sys.def$par.global.envtraits[[i]]$parvals)
      vals <- as.double(sys.def$par.global.envtraits[[i]]$parvals)
      vals <- ifelse(is.na(vals),C.NA.double,vals)
      c_streambugs_create_parglobalenvtrait(i, name, nind, inds, npar, vals, nams)
    }
  }

  # initialize reach-dependent environmental conditions:

  nind <- nrow(sys.def$par.envcond.reach$inpinds)
  inds <- as.integer(sys.def$par.envcond.reach$inpinds)
  inds <- ifelse(is.na(inds),C.NA.int,inds)
  npar <- ncol(sys.def$par.envcond.reach$parvals)
  nrow <- nrow(sys.def$par.envcond.reach$parvals)
  nams <- colnames(sys.def$par.envcond.reach$parvals)
  vals <- as.double(sys.def$par.envcond.reach$parvals)
  vals <- ifelse(is.na(vals),C.NA.double,vals)
  c_streambugs_create_parenvcondreach(nind, inds, npar, nrow, vals, nams)

  # initialize habitat- (and reach-) dependent environmental conditions:

  nind <- nrow(sys.def$par.envcond.habitat$inpinds)
  inds <- as.integer(sys.def$par.envcond.habitat$inpinds)
  inds <- ifelse(is.na(inds),C.NA.int,inds)
  npar <- ncol(sys.def$par.envcond.habitat$parvals)
  nrow <- nrow(sys.def$par.envcond.habitat$parvals)
  nams <- colnames(sys.def$par.envcond.habitat$parvals)
  vals <- as.double(sys.def$par.envcond.habitat$parvals)
  vals <- ifelse(is.na(vals),C.NA.double,vals)
  c_streambugs_create_parenvcondhabitat(nind, inds, npar, nrow, vals, nams)

  # initialize habitat-dependent environmental conditions belonging to groups:

  ngroup <- length(sys.def$par.envcond.habitat.group)
  if ( !is.list(sys.def$par.envcond.habitat.group) ) ngroup <- 0
  c_streambugs_create_parenvcondhabitatgroups_structure(ngroup)
  if ( ngroup > 0 )
  {
    for ( i in 1:ngroup )
    {
      name <- names(sys.def$par.envcond.habitat.group)[i]
      nind <- nrow(sys.def$par.envcond.habitat.group[[i]]$inpinds)
      inds <- as.integer(sys.def$par.envcond.habitat.group[[i]]$inpinds)
      inds <- ifelse(is.na(inds),C.NA.int,inds)
      npar <- ncol(sys.def$par.envcond.habitat.group[[i]]$parvals)
      nrow <- nrow(sys.def$par.envcond.habitat.group[[i]]$parvals)
      nams <- colnames(sys.def$par.envcond.habitat.group[[i]]$parvals)
      vals <- as.double(sys.def$par.envcond.habitat.group[[i]]$parvals)
      vals <- ifelse(is.na(vals),C.NA.double,vals)
      c_streambugs_create_parenvcondhabitatgroup(i, name, nind, inds, npar, nrow, vals, nams)
    }
  }

  # initialize initial conditons:

  nind <- nrow(sys.def$par.initcond$inpinds)
  inds <- as.integer(sys.def$par.initcond$inpinds)
  inds <- ifelse(is.na(inds),C.NA.int,inds)
  npar <- ncol(sys.def$par.initcond$parvals)
  nrow <- nrow(sys.def$par.initcond$parvals)
  nams <- colnames(sys.def$par.initcond$parvals)
  vals <- as.double(sys.def$par.initcond$parvals)
  vals <- ifelse(is.na(vals),C.NA.double,vals)
  c_streambugs_create_parinitcond(nind, inds, npar, nrow, vals, nams)

  # initialize input:

  nind <- nrow(sys.def$par.input$inpinds)
  inds <- as.integer(sys.def$par.input$inpinds)
  inds <- ifelse(is.na(inds),C.NA.int,inds)
  npar <- ncol(sys.def$par.input$parvals)
  nrow <- nrow(sys.def$par.input$parvals)
  nams <- colnames(sys.def$par.input$parvals)
  vals <- as.double(sys.def$par.input$parvals)
  vals <- ifelse(is.na(vals),C.NA.double,vals)
  c_streambugs_create_parinput(nind, inds, npar, nrow, vals, nams)

  # initialize directly defined taxa properties:

  nind <- nrow(sys.def$par.taxaprop.direct$inpinds)
  inds <- as.integer(sys.def$par.taxaprop.direct$inpinds)
  inds <- ifelse(is.na(inds),C.NA.int,inds)
  npar <- ncol(sys.def$par.taxaprop.direct$parvals)
  nrow <- nrow(sys.def$par.taxaprop.direct$parvals)
  nams <- colnames(sys.def$par.taxaprop.direct$parvals)
  vals <- as.double(sys.def$par.taxaprop.direct$parvals)
  vals <- ifelse(is.na(vals),C.NA.double,vals)
  c_streambugs_create_partaxapropdirect(nind, inds, npar, nrow, vals, nams)

  # initialize trait-derived taxa properties:

  ntrait <- length(sys.def$par.taxaprop.traits)
  if ( !is.list(sys.def$par.taxaprop.traits) ) ntrait <- 0
  c_streambugs_create_partaxaproptraits_structure(ntrait)
  if ( ntrait > 0 )
  {
    for ( i in 1:ntrait )
    {
      name <- names(sys.def$par.taxaprop.traits)[i]
      nind <- nrow(sys.def$par.taxaprop.traits[[i]]$inpinds)
      inds <- as.integer(sys.def$par.taxaprop.traits[[i]]$inpinds)
      inds <- ifelse(is.na(inds),C.NA.int,inds)
      npar <- ncol(sys.def$par.taxaprop.traits[[i]]$parvals)
      nrow <- nrow(sys.def$par.taxaprop.traits[[i]]$parvals)
      nams <- colnames(sys.def$par.taxaprop.traits[[i]]$parvals)
      vals <- as.double(sys.def$par.taxaprop.traits[[i]]$parvals)
      vals <- ifelse(is.na(vals),C.NA.double,vals)
      c_streambugs_create_partaxaproptrait(i, name, nind, inds, npar, nrow, vals, nams)
    }
  }

  # initialize structure for processes:

  ny <- length(sys.def$y.names$y.names)
  c_streambugs_create_processes_structure(ny)

  # initialize taxon-specific processes:

  for ( i in 1:ny )
  {
    if ( length(sys.def$par.proc.taxon[[i]]) > 0 )
    {
      for ( j in 1:length(sys.def$par.proc.taxon[[i]]) )
      {
        procname    <- names(sys.def$par.proc.taxon[[i]])[j]
        ninp        <- nrow(sys.def$par.proc.taxon[[i]][[j]]$inpinds)
        inpinds     <- as.integer(sys.def$par.proc.taxon[[i]][[j]]$inpinds)
        inpinds     <- ifelse(is.na(inpinds),C.NA.int,inpinds)
        npar        <- length(sys.def$par.proc.taxon[[i]][[j]]$parvals)
        parnames    <- names(sys.def$par.proc.taxon[[i]][[j]]$parvals)
        parvals     <- as.double(sys.def$par.proc.taxon[[i]][[j]]$parvals)
        parvals     <- ifelse(is.na(parvals),C.NA.double,parvals)
        nstoich     <- ncol(sys.def$par.proc.taxon[[i]][[j]]$stoich)
        stoichnames <- colnames(sys.def$par.proc.taxon[[i]][[j]]$stoich)
        stoichinds  <- sys.def$par.proc.taxon[[i]][[j]]$stoich[1,]
        stoichvals  <- sys.def$par.proc.taxon[[i]][[j]]$stoich[2,]
        c_streambugs_create_proctaxon(i, j, procname, ninp, inpinds, npar,
          parnames, parvals, nstoich, stoichnames, stoichinds, stoichvals)
      }
    }
  }

  # initialize food web processes:

  for ( i in 1:ny )
  {
    if ( length(sys.def$par.proc.web[[i]]) > 0 )
    {
      for ( j in 1:length(sys.def$par.proc.web[[i]]) )
      {
        procname    <- names(sys.def$par.proc.web[[i]])[j]
        ninp        <- nrow(sys.def$par.proc.web[[i]][[j]]$inpinds)
        inpinds     <- as.integer(sys.def$par.proc.web[[i]][[j]]$inpinds)
        inpinds     <- ifelse(is.na(inpinds),C.NA.int,inpinds)
        npar        <- length(sys.def$par.proc.web[[i]][[j]]$parvals)
        parnames    <- names(sys.def$par.proc.web[[i]][[j]]$parvals)
        parvals     <- as.double(sys.def$par.proc.web[[i]][[j]]$parvals)
        parvals     <- ifelse(is.na(parvals),C.NA.double,parvals)
        c_streambugs_create_procweb(i, j, procname, ninp, inpinds, npar, parnames, parvals)
        for ( k in 1:length(sys.def$par.proc.web[[i]][[j]]$taxa2) )
        {
          procname    <- names(sys.def$par.proc.web[[i]][[j]]$taxa2)[k]
          ninp        <- nrow(sys.def$par.proc.web[[i]][[j]]$taxa2[[k]]$inpinds)
          inpinds     <- as.integer(sys.def$par.proc.web[[i]][[j]]$taxa2[[k]]$inpinds)
          inpinds     <- ifelse(is.na(inpinds),C.NA.int,inpinds)
          npar        <- length(sys.def$par.proc.web[[i]][[j]]$taxa2[[k]]$parvals)
          parnames    <- names(sys.def$par.proc.web[[i]][[j]]$taxa2[[k]]$parvals)
          parvals     <- as.double(sys.def$par.proc.web[[i]][[j]]$taxa2[[k]]$parvals)
          parvals     <- ifelse(is.na(parvals),C.NA.double,parvals)
          nstoich     <- ncol(sys.def$par.proc.web[[i]][[j]]$taxa2[[k]]$stoich)
          stoichnames <- colnames(sys.def$par.proc.web[[i]][[j]]$taxa2[[k]]$stoich)
          stoichinds  <- sys.def$par.proc.web[[i]][[j]]$taxa2[[k]]$stoich[1,]
          stoichvals  <- sys.def$par.proc.web[[i]][[j]]$taxa2[[k]]$stoich[2,]
          c_streambugs_create_procwebtaxon(i, j, k, procname, ninp, inpinds,
            npar, parnames, parvals, nstoich, stoichnames, stoichinds,
            stoichvals)
        }
      }
    }
  }

  # initialize indices for normalization of fA:

  nreach <- length(sys.def$y.names$ind.fA)
  c_streambugs_create_fA_structure(nreach)
  for ( i in 1:nreach )
  {
    nreachind  <- length(sys.def$y.names$indfA[[i]]$ind.reach)
    reachind   <- sys.def$y.names$indfA[[i]]$ind.reach
    nfsthabind <- length(sys.def$y.names$indfA[[i]]$ind.1sthab)
    fsthabind  <- sys.def$y.names$indfA[[i]]$ind.1sthab
    c_streambugs_create_fA(i, nreachind, reachind, nfsthabind, fsthabind)
  }

  return()
}


# ==============================================================
# function to load C resources and allocate and initialize
# global variables
# ==============================================================


free.streambugs.C <- function()
{
  # delete inputs:

  c_streambugs_delete_inputs()

  # delete global parameters:

  c_streambugs_delete_parglobal()

  # delete global trait-dependent parameters:

  c_streambugs_delete_parglobalenvtraits()

  # delete reach-dependent environmental conditons:

  c_streambugs_delete_parenvcondreach()

  # delete habitat- (and reach-) dependent environmental conditons:

  c_streambugs_delete_parenvcondhabitat()

  # delete habitat- (and reach-) dependent environmental conditons (group):

  c_streambugs_delete_parenvcondhabitatgroup()

  # delete initial conditons:

  c_streambugs_delete_parinitcond()

  # delete input:

  c_streambugs_delete_parinput()

  # initialize directly defined taxa properties:

  c_streambugs_delete_partaxapropdirect()

  # delete trait-dependent taxa properties:

  c_streambugs_delete_partaxaproptraits()

  # delete processes:

  c_streambugs_delete_processes()

  # delete indices for normalization of fA:

  c_streambugs_delete_fA()
}


# ==============================================================
# function to run streambugs (in either R or C version)
# ==============================================================

# inp=NA
# C=FALSE,
# file.def=NA
# file.res=NA
# file.add=NA
# return.res.add = FALSE
# tout.add=NA
# verbose=T
# method="lsoda"
# rtol=1e-4
# atol=1e-3

# TODO: add link to PDF syntax ref
# TODO: add example using streambugs.example.model.toy

#' Run the streambugs ODE model
#'
#' Numerically solve streambugs ODE model (in either R or C version) for given
#' parameters, inputs and time points, using the \code{\link[deSolve]{ode}}
#' routine.
#'
#' @param y.names state variables names, either as a vector encoded in the form
#'    \code{"Reach_Habitat_Taxon"} or \code{"Reach_Habitat_Taxon_Group"}, or
#'    a list as returned by \code{\link[streambugs]{decode.statevarnames}}
#'    function
#' @param times vector with time points for which output is wanted; the first
#     value of times must be the initial time
#' @param par vector with constant parameters and model inputs
#' @param inp list with time-dependent parameters or model inputs with one list
#'    element for each parameter or input that includes a matrix where first
#'    column is the time and second the corresponding parameter or input value
#' @param C identifier for C- or R-Version
#' @param file.def file name for writing system definition
#' @param file.res file name for results
#' @param file.add file name for additional output (e.g. process rates)
#' @param return.res.add returns \code{res.add} output additionally to res
#' @param tout.add optional identifier for specific output times for the
#'		additional output, if \code{NA} all \code{res.add} is calculated for all
#'    times
#' @param verbose prints some outputs to console
#' @param method method used by \code{\link[deSolve]{ode}}
#' @param rtol argument of \code{\link[deSolve]{ode}} function defining relative
#'    error tolerance, either a scalar or an array of the same size as \code{y.names}
#' @param atol argument of \code{\link[deSolve]{ode}} function defining absolute
#'    error, either a scalar or an array of the same size as \code{y.names}
#' @param ... further arguments passed to \code{\link[deSolve]{ode}}
#'
#' @return A list with:\describe{
#'    \item{\code{$res}}{matrix of class \code{\link{deSolve}} with up to as
#'      many rows aselements in times and as many columns as elements in
#'      \code{y.names}, plus an additional column for the time value.
#'      There will be a row for each element in times unless the
#'      FORTRAN routine \code{"lsoda"} returns with an unrecoverable error.}
#'    \item{\code{$res.add}}{optional additional output matrix with process
#'      rates and taxon specific factors, present only if \code{return.res.add}
#'      input parameter is set to \code{TRUE}.}
#'    }
#'
#' @export
run.streambugs <- function(y.names,times,par,inp=NA,C=FALSE,
                           file.def=NA,file.res=NA,file.add=NA,
                           return.res.add = FALSE,tout.add=NA,
                           verbose=T,
                           method="lsoda",rtol=1e-4,atol=1e-4,...)

  # file.add: File name for additional output
  # return.res.add: returns res.add additionally to res
  # tout.add: optional identifier for specific output times for the additional output,
  #           if NA all res.add is calculated for all times

{
  ptm <- proc.time()
  if ( !is.list(y.names) ) y.names <- decode.statevarnames(y.names)

  # extract structured system definition from parameter vector and input list:
  # --------------------------------------------------------------------------

  sys.def <- streambugs.get.sys.def(y.names=y.names,par=par,inp=inp)
  if ( !is.na(file.def) ) streambugs.write.sys.def(sys.def=sys.def,file=file.def)

  # update (time-dependent) parameters to initial time:
  # ---------------------------------------------------

  sys.def <- streambugs.updatepar(sys.def,times[1])

  # compile initial state:
  # ----------------------

  w     <- sys.def$par.envcond.reach$parvals[,"w"]
  fA    <- sys.def$par.envcond.habitat$parvals[,"fA"]
  D.ini <- sys.def$par.initcond$parvals[,"Dini"]
  y.ini <- D.ini * w * fA
  names(y.ini) <- y.names$y.names

  # write initialization message:
  # -----------------------------

  if ( verbose )
  {
    cat("streambugs\n")
    cat("----------\n")
    cat("number of state variables: ",length(y.ini),"\n",sep="")
    cat("number of inputs:          ",if(is.list(inp)) length(inp) else 0,"\n",sep="")
    cat("number of parameters:      ",length(par),"\n",sep="")
  }

  if ( !C )
  {
    # run R implementation of streambugs:
    # -----------------------------------

    if ( verbose ) cat("running R version of streambugs ...\n")

    res <-ode(y      = y.ini,
              times  = times,
              func   = rhs.streambugs,
              parms  = sys.def,
              method = method,
              rtol   = rtol,
              atol   = atol,
              ...)
  }
  else
  {
    # run C implementation of streambugs:
    # -----------------------------------

    if ( verbose ) cat("running C version of streambugs ...\n")

    init.streambugs.C(sys.def = sys.def)

    res <- ode(y        = y.ini,
               times    = times,
               func     = "streambugs_rhs",
               parms    = 0,
               #dllname  = "streambugs_rhs",
               dllname  = "streambugs",
               initfunc = "streambugs_rhs_init",
               nout     = 1,
               method   = method,
               rtol     = rtol,
               atol     = atol,
               ...)

    free.streambugs.C()
  }


  res <- res[,1:(1+length(y.ini))]
  if ( verbose )
  {
    cat("simulation completed\n")
    print(proc.time()-ptm)
  }

  if ( !is.na(file.res) ) write.table(res,file.res,col.names=T,row.names=F,sep="\t")

  if(length(times) > nrow(res)) warning("unrecoverable error occurred, res not complete")

  # write additional output (limiting factors and rates) if !is.na(file.add)
  # ----------------------------------------------------------------------------

  if ( !is.na(file.add) | return.res.add )
  {
    res.add <- calculate.additional.output(res=res,par=par,inp=inp,file.add=file.add,tout.add=tout.add)

    if(return.res.add) return(list(res.add=res.add,res=res))
  }

  return(list(res=res))
}
