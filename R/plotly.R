
plotly_bar<-function(df,x_col,y_col,name){
  fig <- plotly::plot_ly(
    df,
    x = x_col,
    y = y_col,
    type = 'bar',
    name = name,
    text=x_col,
    textposition="inside",
    insidetextanchor="middle",
    insidetextfont=list(color="black")
  ) %>% plotly::layout(
    xaxis = list(
      title = list(
        # text=paste0(name," (",prettyNum(sum(b$n_applicants),","),")"),
        font=list(
          size=12,
          color="black"
        )
      ),
      tickfont=list(
        size=12,
        color="black"
      )
    ),
    yaxis = list(
      title = '',
      tickfont=list(
        size=10,
        color="black"
      )
    ),
    barmode = 'stack',
    annotations = list(
      x = x_col,
      y = y_col,
      xanchor="left",
      xref="x",
      yref="y",
      text = paste0((x_col/sum(x_col)*100) %>% round(1),"%"),
      showarrow = F,
      arrowhead = NULL,
      arrowsize = NULL,
      font=list(
        size=12,
        color="black"),
      textangle=0
    )
  ) %>%
    plotly::config(
      scrollZoom=F,
      displaylogo = F,
      modeBarButtonsToRemove = c(
        "zoom2d",
        "pan2d",
        "select2d",
        "lasso2d",
        # "zoomIn2d",
        # "zoomOut2d",
        "autoScale2d",
        # "resetScale2d",
        "hoverclosest",
        "hoverCompareCartesian",
        "toggleHover"
      )
    ) %>%
    plotly::layout(
      showlegend = F,
      xaxis = list(
        showgrid = FALSE,
        zeroline = FALSE,
        showticklabels = FALSE
      ),
      yaxis = list(
        showgrid = FALSE,
        zeroline = FALSE,
        showticklabels = T
      )
    ) %>% plotly::style(hoverinfo = 'none')
  fig
}

plotify<-function(GG){
  PLOTLY <-GG %>% plotly::ggplotly(
    tooltip = "text",
    hoverinfo="text"
  ) %>%
    plotly::config(
      scrollZoom=F,
      displaylogo = F,
      modeBarButtonsToRemove = c(
        "zoom2d",
        "pan2d",
        "select2d",
        "lasso2d",
        # "zoomIn2d",
        # "zoomOut2d",
        "autoScale2d",
        # "resetScale2d",
        "hoverClosestCartesian",
        "hoverCompareCartesian"
      )
    )
  PLOTLY <- PLOTLY %>%
    plotly::layout(
      hoverlabel = list(
        align = "left"
      ),
      xaxis = list(
        tickfont=list(
          size=10,
          color="black"
        )
      ),
      yaxis = list(
        tickfont=list(
          size=10,
          color="black"
        )
      ),
      legend = list(
        itemsizing='constant',
        orientation = "h",
        x = -0.1,
        y=1.1
      )
    )
  PLOTLY
}

plotly_histogram <- function(x,range,lab){
  PLOTLY <- plotly::plot_ly(
    x = x,
    type = "histogram",
    hoverinfo="none",
    texttemplate = "%{y}"
  )
  PLOTLY <-PLOTLY  %>%
    plotly::config(
      scrollZoom=F, displaylogo = F,
      modeBarButtonsToRemove = c(
        "zoom2d",
        "pan2d",
        "select2d",
        "lasso2d",
        # "zoomIn2d",
        # "zoomOut2d",
        "autoScale2d",
        # "resetScale2d",
        "hoverClosestCartesian",
        "hoverCompareCartesian"
      )
    )
  PLOTLY <- PLOTLY %>%
    plotly::layout(
      # hoverlabel = list(
      #   align = "left"
      # ),
      xaxis = list(
        tickfont=list(
          size=10,
          color="black"
        ),
        range=c(0,range),
        title = lab
      ),
      yaxis = list(
        tickfont=list(
          size=10,
          color="black"
        )
      )
      # legend = list(
      #   itemsizing='constant',
      #   orientation = "h",
      #   x = -0.1,
      #   y=1.1
      # )
    )
  PLOTLY
}

plotly_histogram2 <- function(DB){
  data<-DB$data$coordinates_plot[which(DB$data$coordinates_plot$group=="Event"),]
  opacity <- 0.6
  histnorm <- "probability"
  hoverinfo <-"all"
  texttemplate <- "%{y}"
  fig <- plotly::plot_ly()
  if(!all(is.na(data$eoi_to_pred_min_distance_km))){
    x<-data$eoi_to_pred_min_distance_km %>% km_to_mi()
    # fit <- density(x)
    fig <-fig %>%  plotly::add_histogram(
      x = x,
      name = "Event to Cluster",
      type = "histogram",
      hoverinfo=hoverinfo,
      texttemplate = texttemplate,
      histnorm = histnorm,
      opacity = opacity
    )
    # %>% plotly::add_trace(
    #   x = fit$x, y = fit$y, mode = "lines", fill = "tozeroy", yaxis = "y2", name = "Density"
    # )
  }
  if(!all(is.na(data$eoi_to_int_min_distance_km))){
    fig <- fig %>% plotly::add_histogram(
      x = data$eoi_to_int_min_distance_km %>% km_to_mi(),
      name = "Event to Intervention",
      type = "histogram",
      hoverinfo=hoverinfo,
      texttemplate = texttemplate,
      histnorm = histnorm,
      opacity = opacity
    )
  }
  fig <- fig  %>%
    plotly::config(
      scrollZoom=F,
      displaylogo = F,
      modeBarButtonsToRemove = c(
        "zoom2d",
        "pan2d",
        "select2d",
        "lasso2d",
        # "zoomIn2d",
        # "zoomOut2d",
        "autoScale2d",
        # "resetScale2d",
        "hoverClosestCartesian",
        "hoverCompareCartesian"
      )
    )
  fig <- fig %>%
    plotly::layout(
      # hoverlabel = list(
      #   align = "left"
      # ),
      legend = list(
        itemsizing='constant',
        orientation = "h",
        x = -0.1,
        y=1.1
      ),
      barmode = "overlay",
      xaxis = list(
        tickfont=list(
          size=10,
          color="black"
        ),
        visible = T,
        dtick = 1,
        title = list(
          text = "Miles"
        )
      ),
      yaxis = list(
        # tick0=10,
        tickfont=list(
          size=10,
          color="black"
        ),
        tickformat = ".0%"
      )
    )
  fig
}

get_plotly_shapes <- function(type="main",size = 1,return = "id"){
  if(type=="any"){
    set <- plotly_shapes_long[[return]]
  }
  if(type=="main"){
    set <- plotly_shapes_long[[return]][which(plotly_shapes_long$type=="main")]
  }
  if(type=="open"){
    set <- plotly_shapes_long[[return]][which(plotly_shapes_long$type=="open")]
  }
  if(type=="dot"){
    set <- plotly_shapes_long[[return]][which(plotly_shapes_long$type=="dot")]
  }
  if(type=="open-dot"){
    set <- plotly_shapes_long[[return]][which(plotly_shapes_long$type=="open-dot")]
  }
  replace <- F
  if(size>length(set)){
    replace <- T
  }
  set %>% sample(size = size,replace = replace) %>% return()
}

plotly_map <- function(DB,maptype="osm",zoom = 9){
  data<-DB$data$coordinates_plot
  fig <- plotly::plot_ly(
    data = data,
    name= ~group,
    lat = ~latitude,
    lon = ~longitude,
    mode= "markers",
    marker = list(
      color = ~color
    ),
    symbol = ~symbol,
    type = 'scattermapbox',
    text = ~label,
    hoverinfo = "text",
    hoverlabel=list(
      align = "left"
    )
  )
  fig <- fig %>% plotly::layout(
    mapbox = mapbox_base(
      type = maptype,
      center = get_DB_center_mapbox(DB),
      zoom=zoom
    ),
    legend = list(
      orientation = "h",
      x = -0.1,
      y=1.1
    )
  )
  fig <- fig %>% plotly::config(
    scrollZoom=F,
    displaylogo = F,
    modeBarButtonsToRemove = c(
      "zoom2d",
      "pan2d",
      "select2d",
      "lasso2d",
      # "zoomIn2d",
      # "zoomOut2d",
      "autoScale2d",
      # "resetScale2d",
      "hoverClosestCartesian",
      "hoverCompareCartesian"
    )
  )
  fig
}

get_DB_center_mapbox <- function(DB){
  latmin <- DB$data$coordinates_plot$latitude %>% min()
  latmax <- DB$data$coordinates_plot$latitude %>% max()
  latmid<-((latmax-latmin)/2)+latmin

  lonmin <- DB$data$coordinates_plot$longitude %>% min()
  lonmax <- DB$data$coordinates_plot$longitude %>% max()
  lonmid<-((lonmax-lonmin)/2)+lonmin
  return(list(lon = lonmid, lat = latmid))
}

mapbox_base <- function(type,center,zoom = 9){
  mapbox <- NULL
  if(type == "osm"){
    mapbox <- list(
      style = 'open-street-map',
      zoom = zoom,
      center = center
    )
  }
  if(type == "sat"){
    mapbox <- list(
      style = "white-bg",
      zoom = zoom,
      center = center,
      layers = list(
        list(
          below = 'traces',
          sourcetype = "raster",
          source = list("https://basemap.nationalmap.gov/arcgis/rest/services/USGSImageryOnly/MapServer/tile/{z}/{y}/{x}")
        )
      )
    )
  }
  return(mapbox)
}

