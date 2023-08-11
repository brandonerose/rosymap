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
run_kmeans <- function(DB,k){
  if(is.null(DB$data$coordinates))stop("is.null(DB$data$coordinates)")
  if(is.null(DB$map))stop("is.null(DB$map)")
  x <- DB$data$coordinates
  y <- DB$data$coordinates %>% dplyr::filter(group=="PT")
  DB$other$kmeans <- stats::kmeans(as.matrix(y %>% dplyr::select(latitude,longitude)), centers = k)
  y$group <- paste0("Cluster - ", DB$other$kmeans$cluster)
  y$size <- 1
  x <- x %>% dplyr::bind_rows(y)
  DB$data$cluster_df <- NULL
  DB$data$cluster_df <- data.frame(
    latitude = DB$other$kmeans$centers %>% as.data.frame() %>% dplyr::pull("latitude"),
    longitude = DB$other$kmeans$centers %>% as.data.frame() %>% dplyr::pull("longitude"),
    size = DB$other$kmeans$size
  )
  DB$data$cluster_df$group <- paste0("Center - ", rownames(DB$data$cluster_df)) %>% as.factor()
  DB$data$coordinates_plot <- x %>% dplyr::bind_rows(DB$data$cluster_df)
  DB <- DB %>% k_means_plot()
  DB
}

k_means_plot <- function(DB,palette= NULL){
  if(is.null(DB$data$coordinates_plot))stop("is.null(DB$data$coordinates_plot)")
  DB$other$main_plot <- ggmap::ggmap(DB$map) +
    ggplot2::geom_point(
      data = DB$data$coordinates_plot[which(!grepl("Center |Cluster ",DB$data$coordinates_plot$group)),],
      ggplot2::aes(
        x = longitude,
        y = latitude,
        color = group,
        size = size
      ),
      alpha = 1
    )+
    ggplot2::geom_point(
      data = DB$data$coordinates_plot[which(grepl("Cluster ",DB$data$coordinates_plot$group)),],
      ggplot2::aes(
        x = longitude,
        y = latitude,
        color = group,
        size = size
      ),
      alpha = 1
    )+
    ggplot2::geom_point(
      data = DB$data$coordinates_plot[which(grepl("Center ",DB$data$coordinates_plot$group)),],
      ggplot2::aes(
        x = longitude,
        y = latitude,
        color = group,
        size = size
      ),
      alpha = 1
    )
  if(!is.null(palette)){
    DB$other$main_plot <-  DB$other$main_plot +
      ggplot2::scale_color_brewer(palette = palette)
  }
  DB
}

make_map <- function(DB,f = 0.1,zoom = 11, maptype = "toner-hybrid"){
  # options(repr.plot.width = 12, repr.plot.height = 8)
  #add test
  DB$map <- ggmap::get_stamenmap(
    ggmap::make_bbox(
      lon = DB$data$coordinates$longitude,
      lat = DB$data$coordinates$latitude,
      f = f
    ),
    zoom = zoom,
    maptype = maptype
  )
  DB
}
