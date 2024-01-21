run_kmeans <- function(DB,k){
  if(is.null(DB$data$coordinates))stop("is.null(DB$data$coordinates)")
  x <- DB$data$coordinates[which(DB$data$coordinates$group=="Event"),]
  rownames(x) <- NULL

  x$id <- rownames(x) %>% as.integer()
  DB$other$kmeans <- stats::kmeans(x=as.matrix(x %>% dplyr::select(latitude,longitude)), centers = k,iter.max = 30)
  z<-DB$other$kmeans$centers %>% as.data.frame()

  DB$data$cluster_df <- NULL
  DB$data$cluster_df <- data.frame(
    latitude = DB$other$kmeans$centers %>% as.data.frame() %>% dplyr::pull("latitude"),
    longitude = DB$other$kmeans$centers %>% as.data.frame() %>% dplyr::pull("longitude"),
    size = DB$other$kmeans$size %>% as.integer(),
    group =  "K-Mean Center",
    cluster = rownames(DB$other$kmeans$centers) %>% as.integer(),
    id = rownames(DB$other$kmeans$centers) %>% as.integer()
  )

  x$cluster <- DB$other$kmeans$cluster %>% as.integer()
  x$size <- 1
  lat_eoi <- x$latitude
  lon_eoi <- x$longitude
  pp_eoi <- spatstat.geom::ppp(lon_eoi,lat_eoi, window = spatstat.geom::owin(range(lon_eoi),range(lat_eoi)))  # Create point pattern object for the first set

  lat_pred <- z$latitude
  lon_pred <- z$longitude
  pp_pred <- spatstat.geom::ppp(lon_pred,lat_pred, window = spatstat.geom::owin(range(lon_pred),range(lat_pred)))  # Create point pattern object for the first set

  distances_eoi_pred <- spatstat.geom::crossdist(pp_eoi, pp_pred)
  nn_dist_eoi_pred <- spatstat.geom::nncross(pp_eoi, pp_pred)

  x$eoi_to_pred_min_distance_km <- geosphere::distGeo(
    data.frame(x = pp_eoi$x, y = pp_eoi$y),
    data.frame(x = pp_pred$x[nn_dist_eoi_pred$which], y = pp_pred$y[nn_dist_eoi_pred$which])
  ) %>% magrittr::divide_by(1000)

  DB$data$cluster_df$eoi_to_pred_min_min_distance_km <- 1:nrow(DB$data$cluster_df) %>% sapply(function(ROW){
    x$eoi_to_pred_min_distance_km[which(x$cluster==ROW)] %>% min()
  })
  DB$data$cluster_df$eoi_to_pred_median_min_distance_km <- 1:nrow(DB$data$cluster_df) %>% sapply(function(ROW){
    x$eoi_to_pred_min_distance_km[which(x$cluster==ROW)] %>% median()
  })
  DB$data$cluster_df$eoi_to_pred_mean_min_distance_km <- 1:nrow(DB$data$cluster_df) %>% sapply(function(ROW){
    x$eoi_to_pred_min_distance_km[which(x$cluster==ROW)] %>% mean()
  })
  DB$data$cluster_df$eoi_to_pred_max_min_distance_km <- 1:nrow(DB$data$cluster_df) %>% sapply(function(ROW){
    x$eoi_to_pred_min_distance_km[which(x$cluster==ROW)] %>% max()
  })
  DB$data$eoi_to_int_min_min_distance_km <- NULL
  DB$data$eoi_to_int_mean_min_distance_km <- NULL
  DB$data$eoi_to_int_median_min_distance_km <- NULL
  DB$data$eoi_to_int_max_min_distance_km <- NULL
  DB$data$eoi_to_pred_min_min_distance_km <- NULL
  DB$data$eoi_to_pred_median_min_distance_km <- NULL
  DB$data$eoi_to_pred_mean_min_distance_km <- NULL
  DB$data$eoi_to_pred_max_min_distance_km <- NULL

  DB$data$eoi_to_pred_min_min_distance_km <- x$eoi_to_pred_min_distance_km %>% min()
  DB$data$eoi_to_pred_median_min_distance_km <- x$eoi_to_pred_min_distance_km %>% median()
  DB$data$eoi_to_pred_mean_min_distance_km <- x$eoi_to_pred_min_distance_km %>% mean()
  DB$data$eoi_to_pred_max_min_distance_km <- x$eoi_to_pred_min_distance_km %>% max()
  # x$eoi_to_pred_min_distance_km %>% hist()

  if("Intervention" %in% DB$data$coordinates$group){
    y <- DB$data$coordinates[which(DB$data$coordinates$group=="Intervention"),]
    rownames(y) <- NULL
    y$id <- rownames(y) %>% as.integer()
    lat_int <- y$latitude
    lon_int <- y$longitude
    pp_int <- spatstat.geom::ppp(lon_int,lat_int, window = spatstat.geom::owin(range(lon_int),range(lat_int)))  # Create point pattern object for the first set
    distances_eoi_int <- spatstat.geom::crossdist(pp_eoi, pp_int)
    nn_dist_eoi_int <- spatstat.geom::nncross(pp_eoi, pp_int)

    x$eoi_to_int_min_distance_km <- geosphere::distGeo(
      data.frame(x = pp_eoi$x, y = pp_eoi$y),
      data.frame(x = pp_int$x[nn_dist_eoi_int$which], y = pp_int$y[nn_dist_eoi_int$which])
    ) %>% magrittr::divide_by(1000)

    x$eoi_to_int_vs_pred_min_distance_km <- x$eoi_to_int_min_distance_km - x$eoi_to_pred_min_distance_km

    DB$data$cluster_df$eoi_to_int_min_min_distance_km <- 1:nrow(DB$data$cluster_df) %>% sapply(function(ROW){
      x$eoi_to_int_min_distance_km[which(x$cluster==ROW)] %>% min()
    })
    DB$data$cluster_df$eoi_to_int_median_min_distance_km <- 1:nrow(DB$data$cluster_df) %>% sapply(function(ROW){
      x$eoi_to_int_min_distance_km[which(x$cluster==ROW)] %>% median()
    })
    DB$data$cluster_df$eoi_to_int_mean_min_distance_km <- 1:nrow(DB$data$cluster_df) %>% sapply(function(ROW){
      x$eoi_to_int_min_distance_km[which(x$cluster==ROW)] %>% mean()
    })
    DB$data$cluster_df$eoi_to_int_max_min_distance_km <- 1:nrow(DB$data$cluster_df) %>% sapply(function(ROW){
      x$eoi_to_int_min_distance_km[which(x$cluster==ROW)] %>% max()
    })
    # x$eoi_to_int_min_distance_km %>% hist()

    DB$data$eoi_to_int_min_min_distance_km <- x$eoi_to_int_min_distance_km %>% min()
    DB$data$eoi_to_int_median_min_distance_km <- x$eoi_to_int_min_distance_km %>% median()
    DB$data$eoi_to_int_mean_min_distance_km <- x$eoi_to_int_min_distance_km %>% mean()
    DB$data$eoi_to_int_max_min_distance_km <- x$eoi_to_int_min_distance_km %>% max()
    x <- x %>% dplyr::bind_rows(y)

  }

  DB$data$coordinates_plot <- x %>% dplyr::bind_rows(DB$data$cluster_df)
  DB$data$coordinates_plot$size[which(is.na(DB$data$coordinates_plot$size))] <- 1

  DB$data$coordinates_plot$label <- NA
  DB$data$coordinates_plot$label <- paste0(
    DB$data$coordinates_plot$group, ": ", DB$data$coordinates_plot$id,"<br>",
    "  Latitude: ", DB$data$coordinates_plot$latitude %>% round(3),"<br>",
    "  Longitude: ", DB$data$coordinates_plot$longitude %>% round(3),"<br>",
    "  Size: ", DB$data$coordinates_plot$size,"<br>",
    ifelse(
      DB$data$coordinates_plot$group=="Intervention",
      "",
      paste0("  Cluster: ", DB$data$coordinates_plot$cluster,"<br>"
      )
    ),
    ifelse(
      DB$data$coordinates_plot$group=="Event",
      paste0("  Minimum Distance to Cluster: ", DB$data$coordinates_plot$eoi_to_pred_min_distance_km %>% round(1), " km (",km_to_mi(DB$data$coordinates_plot$eoi_to_pred_min_distance_km) %>% round(1)," mi)<br>",
             ifelse("Intervention"%in%DB$data$coordinates_plot$group,
                    paste0("  Minimum Distance to Intervention: ", DB$data$coordinates_plot$eoi_to_int_min_distance_km %>% round(1), " km (",km_to_mi(DB$data$coordinates_plot$eoi_to_int_min_distance_km) %>% round(1)," mi)<br>")
                    ,"")
      ),
      ""
    ),
    ifelse(
      DB$data$coordinates_plot$group=="K-Mean Center",
      paste0(
        "  Number of Events in Cluster: ",DB$data$coordinates_plot$size,"<br>",
        "  <b>Distances Events to Nearest Cluster</b> ",DB$data$coordinates_plot$size,"<br>",
        "    Minimum: ",DB$data$coordinates_plot$eoi_to_pred_min_min_distance_km %>% round(1), " km (",DB$data$coordinates_plot$eoi_to_pred_min_min_distance_km %>% km_to_mi() %>% round(1)," mi)<br>",
        "    Median: ",DB$data$coordinates_plot$eoi_to_pred_median_min_distance_km %>% round(1), " km (",DB$data$coordinates_plot$eoi_to_pred_median_min_distance_km %>% km_to_mi() %>% round(1)," mi)<br>",
        "    Mean: ",DB$data$coordinates_plot$eoi_to_pred_mean_min_distance_km %>% round(1), " km (",DB$data$coordinates_plot$eoi_to_pred_mean_min_distance_km %>% km_to_mi() %>% round(1)," mi)<br>",
        "    Maximum: ",DB$data$coordinates_plot$eoi_to_pred_max_min_distance_km %>% round(1), " km (",DB$data$coordinates_plot$eoi_to_pred_max_min_distance_km %>% km_to_mi() %>% round(1)," mi)<br>"
      ),
      ""
    ),
    ifelse(
      DB$data$coordinates_plot$group=="K-Mean Center"&"Intervention"%in%DB$data$coordinates_plot$group,
      paste0(
        "  <b>Distances Events to Nearest Intervention</b> ",DB$data$coordinates_plot$size,"<br>",
        "    Minimum: ",DB$data$coordinates_plot$eoi_to_int_min_min_distance_km %>% round(1), " km (",DB$data$coordinates_plot$eoi_to_int_min_min_distance_km %>% km_to_mi() %>% round(1)," mi)<br>",
        "    Median: ",DB$data$coordinates_plot$eoi_to_int_median_min_distance_km %>% round(1), " km (",DB$data$coordinates_plot$eoi_to_int_median_min_distance_km %>% km_to_mi() %>% round(1)," mi)<br>",
        "    Mean: ",DB$data$coordinates_plot$eoi_to_int_mean_min_distance_km %>% round(1), " km (",DB$data$coordinates_plot$eoi_to_int_mean_min_distance_km %>% km_to_mi() %>% round(1)," mi)<br>",
        "    Maximum: ",DB$data$coordinates_plot$eoi_to_int_max_min_distance_km %>% round(1), " km (",DB$data$coordinates_plot$eoi_to_int_max_min_distance_km %>% km_to_mi() %>% round(1)," mi)<br>"
      ),
      ""
    )
  )

  DB <- DB %>% k_means_plot()
  # DB$other$main_plot
  # DB$other$main_plotly
  # if(!is.null(DB$data$eoi_to_int_min_min_distance_km)){
  #   range <- c(DB$data$eoi_to_pred_max_min_distance_km,DB$data$eoi_to_int_max_min_distance_km)%>%  km_to_mi()%>% max()
  #   DB$other$hist_of_eoi_to_pred_dist <- plotly_histogram(x = DB$data$coordinates_plot$eoi_to_pred_min_distance_km[which(DB$data$coordinates_plot$group=="Event")]%>% km_to_mi(), range=range,lab="Miles")
  #   DB$other$hist_of_eoi_to_int_dist <-  plotly_histogram(x = DB$data$coordinates_plot$eoi_to_int_min_distance_km[which(DB$data$coordinates_plot$group=="Event")]%>% km_to_mi(), range=range,lab="Miles")
  # }else{
  #   range <- DB$data$eoi_to_pred_max_min_distance_km %>% km_to_mi()
  #   DB$other$hist_of_eoi_to_pred_dist <- plotly_histogram(x = DB$data$coordinates_plot$eoi_to_pred_min_distance_km[which(DB$data$coordinates_plot$group=="Event")], range=range)
  #   DB$other$hist_of_eoi_to_int_dist <- NULL
  # }
  DB$other$hist<-plotly_histogram2(DB)
  DB
}

k_means_plot <- function(
    DB,
    maptype = mapstyles[2],
    zoom = 10,
    event_color = "black",
    intervention_color = "red",
    center_color = "green",
    color_events_scale = F,
    output_min=10, output_max = 28, opacity = 1
) {
  # event_shape = "circle"
  # intervention_shape = "circle"
  # center_shape = "circle"
  if(is.null(DB$data$coordinates_plot))stop("is.null(DB$data$coordinates_plot)")
  DB$data$coordinates_plot$color_groups <- 1:nrow(DB$data$coordinates_plot) %>% sapply(function(ROW){
    if(DB$data$coordinates_plot$group[ROW]=="Intervention"){
      return(DB$data$coordinates_plot$group[ROW])
    }
    if(DB$data$coordinates_plot$group[ROW]=="Event"){
      return(paste("Event Cluster",DB$data$coordinates_plot$cluster[ROW]))
    }
    if(DB$data$coordinates_plot$group[ROW]=="K-Mean Center"){
      return(paste("Center Cluster",DB$data$coordinates_plot$cluster[ROW]))
    }
  }) %>% unlist()
  color_groups <-unique(DB$data$coordinates_plot$color_groups)
  n_colors <- length(color_groups)
  cluster_numbers <- DB$data$coordinates_plot$cluster[grep("Center Cluster ",DB$data$coordinates_plot$color_groups)]
  cluster_groups <- unique(grep("Center Cluster ",DB$data$coordinates_plot$color_groups,value = T))
  n_clusters <- length(cluster_groups)

  # groups <- unique(DB$data$coordinates_plot$group)
  # groups <- data.frame(
  #   group = groups,
  #   symbol = get_plotly_shapes(size = length(groups),return = "name") %>% as.character()
  # )
  # DB$data$coordinates_plot$symbol <- DB$data$coordinates_plot$group %>% sapply(function(group){groups$symbol[groups$group==group]})
  if(color_events_scale){
    if((length(seq_pals)-1)>n_clusters){
      cluster_pals <- seq_pals %>% sample(n_clusters+1)
    }else{
      cluster_pals <- seq_pals %>% sample(n_clusters+1,replace = T)
    }
  }

  i <-1:nrow(DB$data$coordinates_plot) %>% sample(1)
  # RColorBrewer::brewer.pal(3,cluster_pals[DB$data$coordinates_plot$cluster[i]]) %>% scales::show_col()
  for (i in 1:nrow(DB$data$coordinates_plot)){
    if(DB$data$coordinates_plot$group[i]=="Event"){
      # DB$data$coordinates_plot$symbol[i] <- event_shape
      if(color_events_scale){
        DB$data$coordinates_plot$color[i]<-RColorBrewer::brewer.pal(3,cluster_pals[DB$data$coordinates_plot$cluster[i]])[2]
      }else{
        DB$data$coordinates_plot$color[i] <- event_color
      }
    }
    if(DB$data$coordinates_plot$group[i]=="Intervention"){
      # DB$data$coordinates_plot$symbol[i] <- intervention_shape
      if(color_events_scale){
        DB$data$coordinates_plot$color[i]<-RColorBrewer::brewer.pal(3,cluster_pals[length(cluster_pals)])[3]
      }else{
        DB$data$coordinates_plot$color[i] <- intervention_color
      }
    }
    if(DB$data$coordinates_plot$group[i]=="K-Mean Center"){
      # DB$data$coordinates_plot$symbol[i] <-center_shape
      if(color_events_scale){
        DB$data$coordinates_plot$color[i]<-RColorBrewer::brewer.pal(3,cluster_pals[DB$data$coordinates_plot$cluster[i]])[3]
      }else{
        DB$data$coordinates_plot$color[i] <- center_color
      }
    }
  }
  # if(!is.null(palette)){
  #   DB$other$main_plot <-  DB$other$main_plot +
  #     ggplot2::scale_color_brewer(palette = palette)
  # }
  # DB$data$coordinates_plot$symbol <- "circle"
  DB$other$main_plotly<-DB %>% plotly_map(maptype=maptype,zoom= zoom,output_min=output_min, output_max = output_max,opacity = opacity)
  DB$other$main_plotly
  DB
}

show_db_scan <- function(eps,minPts){
  dbscan_result <- dbscan(coords_matrix, eps = eps, MinPts = minPts)

  cluster_labels_dbscan <- dbscan_result$cluster

  cluster_df <- data.frame(traumas, dbscan = factor(cluster_labels_dbscan))

  # get a map of the area


  # set the size of the plotting device to 800x800 pixels

  # plot the map and the clusters
  ggmap(map) +
    # geom_point(data = cluster_df, aes(x = lon, y = lat, color = kmeans), size = 2, alpha = 1) +
    # scale_color_discrete(name = "K-means")
    geom_point(data = cluster_df, aes(x = lon, y = lat, color = dbscan), size = 2, alpha = 1) +
    scale_color_discrete(name = "DBSCAN")
}



