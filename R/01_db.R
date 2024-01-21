#' @title Validates DB
#' @return Message
#' @param DB DB from load_DB or update_DB
validate_DB<-function(DB){
  #param check
  if( ! is.list(DB)) stop("DB must be a list")
  #function
  if( ! all(names(blank_DB)%in%names(DB))){
    stop("`DB` does not have the appropriate names. Did you use `load_DB()` to generate it?")
  }
  if((length(DB[["data"]])==0)>0){
    warning("Valid list but no data yet!",immediate. = T)
  }
  message("`DB` validated!")
}
#add year check

#' @title Reads DB from the directory
#' @param blank logical for blank load or last save
#' @return DB
#' @export
load_DB<-function(blank=F){
  DB_path<-file.path(get_dir(),"R_objects","DB.rdata")
  project_name_path<-file.path(get_dir(),"R_objects",".project_name.rdata")
  if(file.exists(DB_path)&!blank){
    load(file=DB_path)
    validate_DB(DB)
    if(file.exists(project_name_path)){
      load(file=project_name_path)
      if(DB$project_name!=project_name) stop("Do you have more than REDCap project? The current directory contains a Project ID that does not match!")
    }
  }else{
    DB <- blank_DB
    if(!blank) message("`DB` was empty and created. This should only happen with a new directory!")
  }
  # if(!blank){
  #   # x<-names(DB)
  #   # x<-x[which(!x%in%names(blank_DB))]
  #   # message("Studies: ",paste0(x,collapse = ", "))
  #   # message("Last Update: ",DB$last_update)
  # }
  DB
}

#' @title Saves DB in the directory
#' @param DB DB from load_DB or update_DB
#' @return Message
#' @export
save_DB<-function(DB){
  #param check
  if( ! is.list(DB)) stop("DB must be a list")
  #function
  validate_DB(DB)
  project_name<-DB$project_name
  save(project_name,file=file.path(get_dir(),"R_objects",".project_name.rdata"))
  save(DB,file=file.path(get_dir(),"R_objects","DB.rdata"))
  for(x in names(DB$data)){
    y<- DB$data[[x]]
    if(is.data.frame(y)){
      print(x)
      y %>% rio::export(paste0(get_dir(),"/output/",x,".xlsx"))
    }
  }
  for(x in names(DB$other)){
    plot<- DB$other[[x]]
    if("ggplot"%in%class(plot)){
      ggsave(paste0(get_dir(),"/output/",x,".png"),plot=plot,height = 4,width = 5,units = "in")
    }
    # if(is.data.frame(y)){
    #   y %>% rio::export(paste0(get_dir(),"/output/",x,".xlsx"))
    # }
  }
  # save_xls_wrapper(DB)
  message("Saved!")
}

#' @title Shows DB in the env
#' @param DB list for the package that contains applications and reference files
#' @return DB tables in your environment
#' @export
show_DB <- function(DB){
  DB <- validate_DB(DB)
  data_list <- list()
  for(NAME in names(DB$data)){
    L <- list(DB$data[[NAME]])
    names(L) <- NAME
    data_list <- data_list %>% append(L)
  }
  data_list %>% list2env(envir = .GlobalEnv)
}

