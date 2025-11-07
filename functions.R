

create_domain <- function(lat = 51.5138,
                          lon = -0.0983,
                          x = 532054,
                          y = 181145,
                          crs = 4326,
                          buffer_m = 500,
                          interactive = FALSE){
  
  # create a geo referenced point
  location <- st_point(c(x,y)) |> 
    st_sfc(crs = crs)
  
  # convert to lat lon for mapview
  location_ll <- st_transform(location,4326)
  
  # Enter lat lon
  latitude <- 51.5138
  longitude <- -0.0983
  
  if(crs == 4326){
    # create geo referenced point
    location_ll <- st_point(c(longitude, latitude)) |> 
      st_sfc(crs = 4326)
    location <- st_transform(location_ll, 27700)
  } else {
    location <- st_point(c(x_coord,y_coord)) |> 
      st_sfc(crs = crs)
    location_ll <- st_transform(location, 4326)
  }
  
  if(interactive == TRUE){
    
    modelled_area <- mapview(location_ll, map.types = c("OpenStreetMap", "Esri.WorldTopoMap", "Esri.WorldImagery", "Esri.WorldShadedRelief")) %>%
      editMap(title = "Use the rectangle tool to draw the area to be modelled and click Done")
    
    ##Once finished create variable to be plotted
    modelled_area <- modelled_area$finished
    
    # create the search box
    domain <- st_transform(modelled_area, 27700)
    
  } else {
    
    # alternatively create by buffering point (fully reproducible)
    domain <- location |>
      st_buffer(buffer_m)
    
  }
  
  return(domain)
  
}



split_links <- function(osm_links,
                        max_link_length ##MAX LINK LENGTH
                        ){

vs <- unique(osm_links$osm_id)
ac_links <- list()
for (v in vs) {
  sl <- filter(osm_links, osm_id == v)
  sl <- st_transform(sl, 27700)
  len <- as.numeric(st_length(sl))
  
  if(len < max_link_length){
    sl <- sl
    sl <- transmute(sl, L1 = 1, geometry)
    sl$L1 <- 1
  } 
  
  if(len >= max_link_length & len <= max_link_length*2){
    n_pts <- ceiling(len/5)
    splt <- ceiling(len/max_link_length)
    df1 <- st_line_sample(sl, sample = 0)
    df2 <- st_line_sample(sl, n = n_pts)
    df3 <- st_line_sample(sl, sample = 1)
    df1 <- data.frame(st_coordinates(df1))
    df2 <- data.frame(st_coordinates(df2))
    df3 <- data.frame(st_coordinates(df3))
    df_out <- rbind(df1, df2, df3)
    df_out_1 <- df_out[1:(NROW(df_out)/2),]
    df_out_1$L1 <- as.character(1)
    df_out_2 <- df_out[((NROW(df_out)/2)):NROW(df_out),]
    df_out_2 <- rbind(df_out_2, df3)
    df_out_2$L1 <- as.character(2)
    df_out <- rbind(df_out_1, df_out_2)
    sl <- st_as_sf(df_out, coords = c("X", "Y"), crs = 27700)
    sl <- sl %>% group_by(L1) %>% dplyr::summarise(do_union = FALSE) %>% st_cast("LINESTRING")
    sl <- st_as_sf(sl, crs = 27700)
  }
  
  if(len > max_link_length*2){
    n_pts <- ceiling(len/5)
    splt <- ceiling(len/max_link_length)
    df1 <- st_line_sample(sl, sample = 0)
    df2 <- st_line_sample(sl, n = n_pts)
    df3 <- st_line_sample(sl, sample = 1)
    df1 <- data.frame(st_coordinates(df1))
    df2 <- data.frame(st_coordinates(df2))
    df3 <- data.frame(st_coordinates(df3))
    df_out <- rbind(df1, df2, df3)
    
    num_groups =splt
    
    ds <- df_out %>% 
      group_by((row_number()-1) %/% (n()/num_groups)) %>%
      nest %>% pull(data)
    
    names(ds) <- seq(1:NROW(ds))
    
    df_out <- bind_rows(ds, .id = "variable")
    df_out$L1 <- df_out$variable
    u_list <- unique(df_out$variable)
    u_list <- u_list[2:NROW(u_list)]
    u <- 2
    dz <- list()
    for (u in u_list){
      u <- as.numeric(u)
      dt <- filter(df_out, L1 == u)
      dr <- filter(df_out, L1 == u-1)
      dr <- dr[NROW(dr),]
      dy <- rbind(dr, dt)
      dy$L1 <- u
      dz[[u]] <- dy 
    }
    df_out_2 <- do.call(rbind, dz)
    df_out_1 <- filter(df_out, L1 == 1)
    df_oot <- rbind(df_out_1, df_out_2)
    #sl$XY <- paste0(sl$L1,"_", sl$X, "_", sl$Y)
    sl <- st_as_sf(df_oot, coords = c("X", "Y"), crs = 27700)
    sl <- sl %>% group_by(L1) %>% dplyr::summarise(do_union = FALSE) %>% st_cast("LINESTRING")
    sl <- st_as_sf(sl, crs = 27700)
  }
  
  #sl <- distinct(sl, XY, .keep_all = TRUE)
  
  ##convert back to sf
  sl$Source.name <- v
  ##generate an Advanced Canyon Name
  sl$ACN <- paste0(sl$Source.name, "_", LETTERS[seq( from = 1, to = NROW(sl) )])
  ac_links[[v]] <- sl
  print(v)
}

AC_links <- do.call(rbind, ac_links)
AC_links <- select(AC_links, Source.name, ACN, geometry)
AC_links$lgth <- as.numeric(st_length(AC_links))
AC_links <- filter(AC_links, !lgth < 1)

return(AC_links)

}


create_vgt <- function(road_links){
  
  vgt_make <- road_links %>% 
    select(Source.name = ACN, geometry)
  
  vgt_outz <- list()
  vgtz <- unique(vgt_make$Source.name)
  for(v in vgtz){
    df <- filter(vgt_make, Source.name == v)
    df <- data.frame(st_coordinates(df))
    df <- transmute(df, v, X, Y)
    vgt_outz[[v]] <- df
  }
  
  vgt <- do.call(rbind, vgt_outz)
  
  #change names of headers to match requirements
  names(vgt) <- c("Source.name", "X..m.", "Y..m.")
  
  return(vgt)
  
}

#create function segment shift to be used below
segment.shift <- function(x, y, d){
  
  # calculate vector
  v <- c(x[2] - x[1],y[2] - y[1])
  
  # normalize vector
  v <- v/sqrt((v[1]**2 + v[2]**2))
  
  # perpendicular unit vector
  vnp <- c( -v[2], v[1] )
  
  return(list(x =  c( x[1] + d*vnp[1], x[2] + d*vnp[1]), 
              y =  c( y[1] + d*vnp[2], y[2] + d*vnp[2])))
  
}

get_road_dimensions <- function(vgt,
                                Width2Scope = 20, ##Enter the total width to scan in metres
                                StartPoint = 0.5, ##Enter the start point in metres from the road centre line
                                Interval = 0.5, #Enter the interval in metres to scan the road)
                                dsm
){
  
  xyz <- data.frame(rasterToPoints(raster(dsm)))
  names(xyz) <- c("x", "y", "z")

  #create variable of all unique Links
  Linkz <-  unique(vgt$Source.name)
  Lils <- "W17_01_D"
  #loop thorugh create the X Y and Z columns (mainly to measure the length of the links)
  mylillist <- list()
  for (Lils in Linkz) {
    df <- filter(vgt, Source.name == Lils)
    if(NROW(df)>1){
      X_1 <- df[1,2]
      X_2 <- df[NROW(df),2]
      Y_1 <- df[1,3]
      Y_2 <- df[NROW(df),3]
      X <- df[NROW(df),2]-X_1
      Y <- df[NROW(df),3]-Y_1
      Z <- (sqrt(X^2+Y^2)/1000)
      df$ID <- gsub("_.*","",df$Source.name)
      df <- transmute(df, Source.name, ID, X_1, Y_1, X_2, Y_2, Z)
      df <- df[1,]
      mylillist[[Lils]] <- df
    }
  }
  ##bind all the rows from the list
  Links_out_many <- do.call(rbind, mylillist)
  Links_out_many <- na.omit(Links_out_many)
  ##pick out the columns we want
  Links_out <- dplyr::select(Links_out_many, Source.name, ID, Z)
  ##rename the headers
  names(Links_out) <- c("Link", "ID", "Z")
  ##define all the distances the script will loop through with the segment shift function for Left and Right
  Distances_L <- seq(StartPoint, Width2Scope, Interval)
  Distances_R <- seq(-StartPoint, -Width2Scope, -Interval)

  
  ##create the list variables the script will loop through and populate
Els <- list()
ALL_the_elevations <- list()
Avg_Els <- list()
els_stat_L <- list()
els_stat_R <- list()
L_Canyons <- list()
Rd_stats_FULL <- list()

Linkz2 <-  unique(Links_out_many$Source.name)
#Advanced Street Canyon --------------------------------------------------

##the first loop is to split up the vgt file into it's seperate links
for(L in Linkz2){
  tryCatch({
    The_Link <- vgt %>% 
      filter(Source.name == L) %>% 
      mutate(XY = paste(X..m., Y..m.)) %>% 
      distinct(XY, .keep_all = TRUE) %>% 
      select(-XY)
    #X and Y min and max from .vgt file
    Links_ASCs_1m <- xyz %>% 
      filter(x > min(The_Link$X..m.)-30,
             x < max(The_Link$X..m.)+30,
             y > min(The_Link$Y..m.)-30,
             y < max(The_Link$Y..m.)+30)
    
    # link_sf <- st_as_sf(The_Link, coords = c("X..m.", "Y..m."), crs = 27700)
    # ALL_5 <- st_as_sf(ALL_ASCs_Objects_5, coords = c("x", "y"), crs = 27700)
    # Links_ASCs_1m_sf <- st_as_sf(Links_ASCs_1m, coords = c("x", "y"), crs = 27700)
    # Links_ASCs_2m_sf <- st_as_sf(Links_ASCs_2m, coords = c("x", "y"), crs = 27700)
    # mapview(link_sf)+Links_ASCs_1m_sf+Links_ASCs_2m_sf
    # mapview(link_sf)+ALL_5
    ##the second loop is to find the surface geometry at the distances defined above 
    for (D in Distances_L) {
      #distance away from road  
      x <-  The_Link$X..m.
      y <-  The_Link$Y..m.
      d <- D
      ##show a plot of the segment
      #plot(x,y, type="l", main = paste0(L, " distance =", D, "m"))
      
      # allocate memory for the path
      xn <- numeric( (length(x) - 1) * 2 )
      yn <- numeric( (length(y) - 1) * 2 )
      ##the third loop loops through each segment within the link
      for ( i in 1:(length(x) - 1) ) {
        xs <- c(x[i], x[i+1])
        ys <- c(y[i], y[i+1])
        new.s <- segment.shift( xs, ys, d )
        xn[(i-1)*2+1] <- new.s$x[1] ; xn[(i-1)*2+2] <- new.s$x[2]
        yn[(i-1)*2+1] <- new.s$y[1] ; yn[(i-1)*2+2] <- new.s$y[2]
      }
      
      # draw the shifted segment
      #lines(xn, yn, col="brown", lwd =2, lty=2)
      ##create a data frame of results
      Da_Link <- data.frame(x = xn, y = yn, main = paste0(L, " distance =", D, "m"))
      # da_link_sf <- st_as_sf(Da_Link, coords = c("x", "y"), crs = 27700)
      ##define how many points each segment is made up of
      sections <- seq(1:NROW(Da_Link))
      ##the fourth loop finds the closest point on the DSM LIDAR data map to find the height of each point
      for (S in sections){
        ID_SX <- which.closest(Links_ASCs_1m$x, Da_Link[S,1])
        ID_SY <- which.closest(Links_ASCs_1m$y, Da_Link[S,2])
        
        Elvtn <- filter(Links_ASCs_1m, x == Links_ASCs_1m$x[ID_SX] & y == Links_ASCs_1m$y[ID_SY])
        ##If 1m LIDAR data doesn't cover area then try the 2m
        if(NROW(Elvtn)>0){
          Els[[S]] <- Elvtn
        } else {
          ID_SX <- which.closest(Links_ASCs_2m$x, Da_Link[S,1])
          ID_SY <- which.closest(Links_ASCs_2m$y, Da_Link[S,2])
          
          Elvtn <- filter(Links_ASCs_2m, x == Links_ASCs_2m$x[ID_SX] & y == Links_ASCs_2m$y[ID_SY])
          Els[[S]] <- Elvtn
        }
        
      }
      
      ALL_Elvtns <- do.call(rbind, Els)
      
      els_stats <- data.frame(avg = mean(ALL_Elvtns$z))
      els_stats$max <- max(ALL_Elvtns$z)
      els_stats$min <- min(ALL_Elvtns$z)
      els_stats$D <- D
      els_stats$Side <- "Left"
      nam <- as.character(D)
      ALL_Elvtns$Distance <- D
      ALL_Elvtns$Link <- L
      ALL_the_elevations[[nam]] <- ALL_Elvtns
      els_stat_L[[nam]] <- els_stats
      
    }
    ##repeats the above for the right hand side
    for (D in Distances_R) {
      #distance away from road  
      x <-  The_Link$X..m.
      y <-  The_Link$Y..m.
      d <- D
      
      #plot(x,y, type="l", main = paste0(L, " distance =", D, "m"))
      
      # allocate memory for the path we are looking at
      xn <- numeric( (length(x) - 1) * 2 )
      yn <- numeric( (length(y) - 1) * 2 )
      
      for ( i in 1:(length(x) - 1) ) {
        xs <- c(x[i], x[i+1])
        ys <- c(y[i], y[i+1])
        new.s <- segment.shift( xs, ys, d )
        xn[(i-1)*2+1] <- new.s$x[1] ; xn[(i-1)*2+2] <- new.s$x[2]
        yn[(i-1)*2+1] <- new.s$y[1] ; yn[(i-1)*2+2] <- new.s$y[2]
      }
      
      # draw the path
      #lines(xn, yn, col="blue", lwd =2, lty=2)
      
      Da_Link <- data.frame(x = xn, y = yn, main = paste0(L, " distance =", D, "m"))
      
      sections <- seq(1:NROW(Da_Link))
      
      for (S in sections){
        ID_SX <- which.closest(Links_ASCs_1m$x, Da_Link[S,1])
        ID_SY <- which.closest(Links_ASCs_1m$y, Da_Link[S,2])
        
        Elvtn <- filter(Links_ASCs_1m, x == Links_ASCs_1m$x[ID_SX] & y == Links_ASCs_1m$y[ID_SY])
        ##If 1m LIDAR data doesn't cover area then try the 2m
        if(NROW(Elvtn)>0){
          Els[[S]] <- Elvtn
        } else {
          ID_SX <- which.closest(Links_ASCs_2m$x, Da_Link[S,1])
          ID_SY <- which.closest(Links_ASCs_2m$y, Da_Link[S,2])
          
          Elvtn <- filter(Links_ASCs_2m, x == Links_ASCs_2m$x[ID_SX] & y == Links_ASCs_2m$y[ID_SY])
          Els[[S]] <- Elvtn
        }
        
      }
      
      ALL_Elvtns <- do.call(rbind, Els)
      
      els_stats <- data.frame(avg = mean(ALL_Elvtns$z))
      els_stats$max <- max(ALL_Elvtns$z)
      els_stats$min <- min(ALL_Elvtns$z)
      els_stats$D <- D
      els_stats$Side <- "Right"
      nam <- as.character(D)
      
      els_stat_R[[nam]] <- els_stats
      
    }
    
    Rd_stats_L <- do.call(rbind, els_stat_L)
    Rd_stats_L$diff <- c("0", diff(Rd_stats_L$avg))
    Rd_stats_R <- do.call(rbind, els_stat_R)
    Rd_stats_R$diff <- c(diff(Rd_stats_R$avg), "0")
    ##calculate the maximum difference between half metre points to determine the Left canyon width
    Rd_L_MX <- Rd_stats_L[which.max(Rd_stats_L$diff), ]
    Rd_L_MX$D <- abs(Rd_L_MX$D)
    Rd_L_MX$min <- ifelse(Rd_L_MX$min < 0, 0, Rd_L_MX$min)
    names(Rd_L_MX) <- c("avgHeight_L", "maxHeight_L", "minHeight_L", "width_L", "Side_L", "diff_L") 
    ##calculate the maximum difference between half metre points to determine the Right canyon width
    Rd_R_MX <- Rd_stats_R[which.max(Rd_stats_R$diff), ]
    Rd_R_MX$D <- abs(Rd_R_MX$D)
    Rd_R_MX$min <- ifelse(Rd_R_MX$min < 0, 0, Rd_R_MX$min)
    names(Rd_R_MX) <- c("avgHeight_R", "maxHeight_R", "minHeight_R", "width_R", "Side_R", "diff_R") 
    ##change all numbers to positive (mainly for the D column)
    Rd_stats <- cbind(Rd_L_MX, Rd_R_MX)
    Rd_stats_FULL <- rbind(Rd_stats_L, Rd_stats_R)
    
    L_Canyons[[L]] <- Rd_stats
    Rd_stats_FULL[[L]] <- Rd_stats_FULL
    whereup2 <- paste0(L, " ", (which(Linkz2 == L)/NROW(Linkz2)*100), "%")
    print(whereup2)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

##bind all the results together in dataframes
#ALL_EVs <- do.call(rbind, ALL_the_elevations)
ALL_roads <- do.call(rbind, L_Canyons)
ALL_roads$ID <- row.names(ALL_roads)
#ALL_Rd_Stats <- do.call(rbind, Rd_stats_FULL)

return(ALL_roads)

}

create_canyons <- function(vgt,
                           road_dims){

vgt_can <- left_join(vgt, road_dims, by = c("Source.name" = "ID"))
Linkz <- unique(vgt_can$Source.name)
#Advanced Street Canyon --------------------------------------------------
Link_canyons <- list()
L_line <- list()
R_line <- list()
L <- Linkz[2]
##the first loop is to split up the vgt file into it's seperate links
for(L in Linkz){
  tryCatch({
    The_Link <- vgt_can %>% 
      filter(Source.name == L) %>% 
      mutate(XY = paste(X..m., Y..m.)) %>% 
      distinct(XY, .keep_all = TRUE) %>% 
      select(-XY)
    ##the second loop is to find the surface geometry at the distances defined above 
    #distance away from road  
    x <-  The_Link$X..m.
    y <-  The_Link$Y..m.
    d <- The_Link$width_L[1]
    ##show a plot of the segment
    #plot(x,y, type="l", main = paste0(L, " distance =", d, "m"))
    
    # allocate memory for the path
    xn <- numeric( (length(x) - 1) * 2 )
    yn <- numeric( (length(y) - 1) * 2 )
    ##the third loop loops through each segment within the link
    for ( i in 1:(length(x) - 1) ) {
      xs <- c(x[i], x[i+1])
      ys <- c(y[i], y[i+1])
      new.s <- segment.shift( xs, ys, d )
      xn[(i-1)*2+1] <- new.s$x[1] ; xn[(i-1)*2+2] <- new.s$x[2]
      yn[(i-1)*2+1] <- new.s$y[1] ; yn[(i-1)*2+2] <- new.s$y[2]
    }
    
    # draw the shifted segment
    #lines(xn, yn, col="brown", lwd =2, lty=2)
    ##create a data frame of results
    Da_Link_L <- data.frame(x = xn, y = yn, main = paste0(L, " distance =", d, "m"))
    ##add in the height
    Da_Link_L$height <- The_Link$avgHeight_L[1]
    L_sf <- st_as_sf(Da_Link_L, coords=c("x","y"), crs = 27700)
    L_sf <- L_sf %>% dplyr::summarise(do_union = FALSE) %>% st_cast("LINESTRING")
    L_sf$L <- L
    ##Repeat for right hand side
    d <- -The_Link$width_R[1]
    ##show a plot of the segment
    #plot(x,y, type="l", main = paste0(L, " distance =", d, "m"))
    
    # allocate memory for the path
    xn <- numeric( (length(x) - 1) * 2 )
    yn <- numeric( (length(y) - 1) * 2 )
    ##the third loop loops through each segment within the link
    for ( i in 1:(length(x) - 1) ) {
      xs <- c(x[i], x[i+1])
      ys <- c(y[i], y[i+1])
      new.s <- segment.shift( xs, ys, d )
      xn[(i-1)*2+1] <- new.s$x[1] ; xn[(i-1)*2+2] <- new.s$x[2]
      yn[(i-1)*2+1] <- new.s$y[1] ; yn[(i-1)*2+2] <- new.s$y[2]
    }
    
    # draw the shifted segment
    #lines(xn, yn, col="brown", lwd =2, lty=2)
    ##create a data frame of results
    Da_Link_R <- data.frame(x = xn, y = yn, main = paste0(L, " distance =", d, "m"))
    Da_Link_R$height <- The_Link$avgHeight_R[1]
    R_sf <- st_as_sf(Da_Link_R, coords=c("x","y"), crs = 27700)
    R_sf <- R_sf %>% dplyr::summarise(do_union = FALSE) %>% st_cast("LINESTRING")
    R_sf$L <- L
    Da_Link <- rbind(Da_Link_L, Da_Link_R)
    Da_Link$main <- L
    Da_Link <- st_as_sf(Da_Link, coords=c("x","y"), crs = 27700)
    
    Da_Link = Da_Link %>% 
      dplyr::group_by(main) %>% 
      dplyr::summarise() %>%
      st_cast("POLYGON") %>% 
      st_convex_hull()
    
    Link_canyons[[L]] <- Da_Link
    L_line[[L]] <- L_sf
    R_line[[L]] <- R_sf
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

canyonz <- do.call(rbind, Link_canyons)
canyonz <- st_transform(canyonz, 4326)
Left_lines <- do.call(rbind, L_line)
Right_lines <- do.call(rbind, R_line)

canyons_lines <- list(canyonz, Left_lines, Right_lines)

return(canyons_lines)

}
