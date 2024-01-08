DB_run_all_kmeans <- function(DB){
  len <- (DB$data$coordinates$group=="Intervention") %>% which() %>% length()
  i <- 1

  OUT <- NULL
  OUT <- OUT %>% dplyr::bind_rows(
    data.frame(
      kmean = NA,
      type= "Minimum",
      distance_km=DB$data$eoi_to_int_min_min_distance_km
    )
  )
  OUT <- OUT %>% dplyr::bind_rows(
    data.frame(
      kmean = NA,
      type= "Median",
      distance_km=DB$data$eoi_to_int_median_min_distance_km
    )
  )
  OUT <- OUT %>% dplyr::bind_rows(
    data.frame(
      kmean = NA,
      type= "Mean",
      distance_km=DB$data$eoi_to_int_mean_min_distance_km
    )
  )
  OUT <- OUT %>% dplyr::bind_rows(
    data.frame(
      kmean = NA,
      type= "Maximum",
      distance_km=DB$data$eoi_to_int_max_min_distance_km
    )
  )

  for (i in 1:len){
    DB <- DB %>% run_kmeans(i)
    OUT <- OUT %>% dplyr::bind_rows(
      data.frame(
        kmean = i,
        type= "Minimum",
        distance_km=DB$data$eoi_to_pred_min_min_distance_km
      )
    )
    OUT <- OUT %>% dplyr::bind_rows(
      data.frame(
        kmean = i,
        type= "Median",
        distance_km=DB$data$eoi_to_pred_median_min_distance_km
      )
    )
    OUT <- OUT %>% dplyr::bind_rows(
      data.frame(
        kmean = i,
        type= "Mean",
        distance_km=DB$data$eoi_to_pred_mean_min_distance_km
      )
    )
    OUT <- OUT %>% dplyr::bind_rows(
      data.frame(
        kmean = i,
        type= "Maximum",
        distance_km=DB$data$eoi_to_pred_max_min_distance_km
      )
    )
  }
  library(ggplot2)
  z<-19
  siz <- 2.5
  above_line <- 0.5
  # OUT$type

  kmeans_long <- OUT

  kmeans_long$kmean[which(is.na(kmeans_long$kmean))] <- "Intervention"

  kmeans_wide <- kmeans_long %>% reshape(idvar = "kmean", timevar = "type", direction = "wide")
  DB$data$kmeans_long <- kmeans_long
  DB$data$kmeans_wide <- kmeans_wide
  return(DB)
}

DB_plot_all_kmeans <- function(DB,show_smooth = F, show_intervention = F){
  kmeans_long <- DB$data$kmeans_long
  kmeans_long$kmean <- as.integer( kmeans_long$kmean )

  all_kmeans_plot <- ggplot()+
    geom_line(
      aes(x=kmeans_long$kmean,y=kmeans_long$distance_km,color = kmeans_long$type)
    )
  if (show_smooth){
    all_kmeans_plot <- all_kmeans_plot +
      geom_smooth(
        aes(x=kmeans_long$kmean,y=kmeans_long$distance_km,color = kmeans_long$type),
        se=F
      )
  }
  if (show_smooth){
    all_kmeans_plot <- all_kmeans_plot +
      geom_hline(yintercept=kmeans_long$distance_km[which(is.na(kmeans_long$kmean)&kmeans_long$type=="Minimum")],linetype="dashed")+
      annotate("text", x=z, y=kmeans_long$distance_km[which(is.na(kmeans_long$kmean)&kmeans_long$type=="Minimum")]+above_line, label=paste0("Minimum Event Distance-to-Nearest-Intervention (n=",len,")"),size=siz)+
      geom_hline(yintercept=kmeans_long$distance_km[which(is.na(kmeans_long$kmean)&kmeans_long$type=="Mean")],linetype="dashed")+
      annotate("text", x=z, y=kmeans_long$distance_km[which(is.na(kmeans_long$kmean)&kmeans_long$type=="Mean")]+above_line, label=paste0("Mean Event Distance-to-Nearest-Intervention (n=",len,")"),size=siz)+
      geom_hline(yintercept=kmeans_long$distance_km[which(is.na(kmeans_long$kmean)&kmeans_long$type=="Maximum")],linetype="dashed")+
      annotate("text", x=z, y=kmeans_long$distance_km[which(is.na(kmeans_long$kmean)&kmeans_long$type=="Maximum")]+above_line, label=paste0("Maximum Event Distance-to-Nearest-Intervention (n=",len,")"),size=siz)
  }
  all_kmeans_plot <- all_kmeans_plot +
    ylab("Event Distance-to-Nearest-Centroid (kilometers)")+
    xlab("Chosen K for K-Means (k)")+
    theme_classic()+
    theme(
      legend.title=element_blank(),
      legend.position = c(0.9, 0.9)
    )
  DB$other$all_kmeans_plot <- all_kmeans_plot
  return(DB)
}
