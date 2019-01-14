# Script para realizar graficos meteorologicos inspirado
# en la sencillez de GrADS
# 
# Versión 0.7.2

# ---------------- LIBREIAS ---------------- #

library(maps)
library(shape)

# ---------------- COMANDOS---------------- #

# Calculo de diferencial finita centrada
cdiff <- function(expr, dim){

  drow <- nrow(expr)
  dcol <- ncol(expr)
  df <- matrix(NA, ncol = dcol, nrow = drow)
  
  if(dim == 'x'){
  
    dx = 1
    dy = 0
  
  }else if(dim == 'y'){
  
    dx = 0
    dy = 1
  
  }
  
  for(i in 2:(drow-1)){
  
    for(j in 2:(dcol-1)){
    
      df[i,j] <- expr[i+dx, j+dy] - expr[i-dx, j-dy]
    
    }
  
  }

  return(df)
  
}

# Calculo del angulo de un vector con respecto al eje X
angx <- function(iv, jv){

	# Calculo del angulo
	angulo <- atan(jv/iv)*180/pi
	
	# Determinando signo de los ejes
	ineg <- which(iv < 0)
	jneg <- which(jv < 0)
	bneg <- which(iv < 0 & jv < 0)
	
	# Ajustando angulo segun signo de los ejes
	corre <- matrix(0, ncol = ncol(iv), nrow = nrow(iv))
	corre[ineg] <- 180
	corre[jneg] <- 360
	corre[bneg] <- 180
	
	angulo <- angulo + corre
	angulo <- round(angulo,0)
	
	return(angulo)

}

# Calculo de la resultante de dos vectores
magx <- function(iv, jv){

	veloc <- sqrt(iv^2 + jv^2)

}

# Delimitar coordenadas para la data
# Este comando se debe ejecutar antes que el set.coor
set.dat <- function(dat, lon, lat, coord){

  #coord tiene las coordenadas de la forma lon_min, lon_max, lat_min, lat_max
  dat <- dat[lon >= coord[1] & lon <= coord[2], lat >= coord[3] & lat <= coord[4]]

  return(dat)
  
}

# Adecuar las dimensiones a lo que se desee
set.coor <- function(lonlat, coord, sel = NULL){

  #coord tiene las coordenadas de la forma lon_min, lon_max, lat_min, lat_max
  #o las coordenadas necesarias
  
  if(!is.null(sel) & length(coord) == 4){
    if(sel == 'lon'){
  
      coord <- coord[1:2]
  
    }else if(sel == 'lat'){
  
      coord <- coord[3:4]
  
    }
  }
  
  lonlat <- lonlat[lonlat >= coord[1] & lonlat <= coord[2]]
  
  return(lonlat)
  
}

# Parámetros de la gráfica
gxprint <- function(name, lon, lat, res = 100){
  
  para <- dimen(lon, lat)
  
  png(name, width = para[1]*res, height = para[2]*res, res = res)

}

# Dimensiones de la imagen de salida
dimen <- function(lon, lat){

  ran_lon <- max(lon) - min(lon)
  ran_lat <- max(lat) - min(lat)

  prop <- ran_lat/ran_lon
  
  base <- 5.6
  anch <- base + 1.2 + 0.6
  alto <- base*prop + 0.6 + 0.6

  return(c(anch, alto))

}

# Definición de colores
color.gs <- function(val, int = NULL, cant = NULL, colors, ...){

    # Intervalos
    if(!is.null(int)){
    
        brk <- seq(val[1], val[length(val)], by = int)
    
    }else if(!is.null(cant)){
    
        brk <- seq(val[1], val[length(val)], length.out = cant)
    
    }else{
    
        brk <- val
    
    }
    
    # Agregar otras escalas de colores predeterminadas
    pal <- colorRampPalette(colors, ...)(length(brk)-1)
    
    escala <- list(brk, pal)
    return(escala)

}


# Ploteo básico sombreado
disp.shaded <- function(dat, lon, lat, 
					    brk = NULL, pal = NULL,
					    map = TRUE, grid = TRUE, ...){
 
  # Adecuando latitudes
  if(lat[1] > lat[length(lat)]){
    
    lat <- lat[length(lat):1]
    dat <- dat[ ,length(lat):1]
    
  }
  
  # Creando paleta en caso no se defina (NULL)
  if(is.null(brk)){
  
    mini <- min(dat, na.rm = TRUE)
    maxi <- max(dat, na.rm = TRUE)
    brk <- round(seq(mini, maxi, length.out = 10), 2)
  
  }
  
  if(is.null(pal)){
  
    pal <- cm.colors(length(brk) - 1)
  
  }
  
  #Acomodando la escala de colores
  dat[dat < brk[1]] <- brk[1]
  dat[dat > brk[length(brk)]] <- brk[length(brk)]

  #Gráfica de colores  
  par(mai = c(0.6, 1.2, 0.6, 0.6))

  filled.contour2(
  	lon,
  	lat,
  	dat[ , ],
  	xlim = range(lon),
  	ylim = range(lat),
  	zlim = range(brk),
  	levels = brk,
  	axes = TRUE,
  	col = pal,
  	...
  )
  
  #Adicionando países
  if(map & min(lon) > 0){
		map('world2', add = T, lwd = 1)
  }else if(map & min(lon) <= 0){
		map("world", add = T, lwd = 1)
  }
  
  # Adicionando grillas
  if(grid){
    grid()
  }
  
  # Remarcando contornos de la grafica
  box()
  
}

# Ploteo de vectores
disp.vector <- function(u, v, lon, lat,
						leng = 0.6 , scale = 2.4,
						skip = 1){

  if(lat[1] > lat[length(lat)]){
    
    lat <- lat[length(lat):1]
    u <- u[ ,length(lat):1]
    v <- v[ ,length(lat):1]
    
  }

  par(mai = c(0.6, 1.2, 0.2, 0.6))
  par(new = TRUE)
  
  seq.lon <- seq(1, length(lon), skip)
  seq.lat <- seq(1, length(lat), skip)
  
  quiver(
		u[seq.lon, seq.lat],
		v[seq.lon, seq.lat],
		lon[seq.lon],
		lat[seq.lat],
		leng = leng,
		scale = scale
  )
  
}

# Ploteo de vectores
disp.vector2 <- function(u, v, lon, lat,
								skip = 1, scale = 0.3, ...){

	if(lat[1] > lat[length(lat)]){
    
		lat <- lat[length(lat):1]
		u <- u[ ,length(lat):1]
		v <- v[ ,length(lat):1]
    
	}
	
#   par(mai = c(0.6, 1.2, 0.2, 0.6))
#   par(new = TRUE)
	
	seq.lon <- seq(1, length(lon), skip)
	seq.lat <- seq(1, length(lat), skip)
	lon <- lon[seq.lon]
	lat <- lat[seq.lat]
	longrd <- matrix(lon, nrow = length(lon), ncol = length(lat))
	latgrd <- matrix(lat, nrow = length(lon), ncol = length(lat), byrow = T)
	
	u <- u[seq.lon, seq.lat]
	v <- v[seq.lon, seq.lat]
	
	direc <- angx(u, v)
	veloc <- magx(u, v)
	
	Arrowhead(longrd, latgrd, direc, arr.length = veloc/max(veloc)*scale, ...)

}

# Displayar puntos como grillas
disp.puntos <- function(dat, lon, lat, map = TRUE, ...){

  par(mai = c(0.6, 1.2, 0.2, 0.6))
  
  quilt.plot(lon, lat, dat, add.legend = FALSE, ...)
  
  if(map){
    cont <- readOGR(dsn = 'E:/Datos/Shapes/SA', layer = 'paises')
    map(cont, add = T, lwd = 1)
  }

}

# Displayar interpolación de puntos
disp.inter <- function(dat, lon, lat, map = TRUE, metod = 'Tps', ...){

  par(mai = c(0.6, 1.2, 0.2, 0.6))
  
  coor <- data.frame(lon, lat)
  
  fit <- Tps(coor, dat)
  fit <- predict.surface(fit)
  plot.surface(fit, add.legend = FALSE)
  
  if(map){
    cont <- readOGR(dsn = 'E:/Datos/Shapes/SA', layer = 'paises')
    map(cont, add = T, lwd = 1)
  }

}
 
# Leyenda de la gráfica
cbar <- function(dat = NULL, brk = NULL, pal = NULL, int = NULL, ylab = ''){

  # Crear paleta en caso no exista
  if(is.null(brk)){
  
    mini <- min(dat, na.rm = TRUE)
    maxi <- max(dat, na.rm = TRUE)
    brk <- round(seq(mini,maxi, length.out = 10), 2)
  
  }
  
  if(is.null(pal)){
  
    pal <- cm.colors(length(brk) - 1)
  
  }

  para <- dev.size('in')
  para <- para[1] - 1

  par(mai = c(0.6, 0.8, 0.6, para))
  par(new = TRUE)
  
  image.scale(
  	zlim = range(brk),
  	breaks = brk,
  	col = pal,
  	horiz = FALSE,
  	xlab = '',
  	ylab = ylab,
  	yaxt = 'n',
  	las = 2
  )
  
  #Parámetros de la leyenda
  if(is.null(int)){
  
    axis(2, at = brk[2:(length(brk)-1)], las = 2)
    
  }else{
  
    axis(2, at = int, las = 2)
  
  }
  
  #box()
  
}


##########################################################3
# aux_funcs


### image scale
image.scale <- function(z, zlim, col = heat.colors(12),
                        breaks, horiz=TRUE, ylim=NULL, xlim=NULL, ...){
  if(!missing(breaks)){
    if(length(breaks) != (length(col)+1)){stop("must have one more break than colour")}
  }
  if(missing(breaks) & !missing(zlim)){
    breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1)) 
  }
  if(missing(breaks) & missing(zlim)){
    zlim <- range(z, na.rm=TRUE)
    zlim[2] <- zlim[2]+c(zlim[2]-zlim[1])*(1E-3)#adds a bit to the range in both directions
    zlim[1] <- zlim[1]-c(zlim[2]-zlim[1])*(1E-3)
    breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1))
  }
  poly <- vector(mode="list", length(col))
  for(i in seq(poly)){
    poly[[i]] <- c(breaks[i], breaks[i+1], breaks[i+1], breaks[i])
  }
  xaxt <- ifelse(horiz, "s", "n")
  yaxt <- ifelse(horiz, "n", "s")
  if(horiz){YLIM<-c(0,1); XLIM<-range(breaks)}
  if(!horiz){YLIM<-range(breaks); XLIM<-c(0,1)}
  if(missing(xlim)) xlim=XLIM
  if(missing(ylim)) ylim=YLIM
  plot(1,1,t="n",ylim=ylim, xlim=xlim, xaxt=xaxt, yaxt=yaxt, xaxs="i", yaxs="i", ...)  
  for(i in seq(poly)){
    if(horiz){
      polygon(poly[[i]], c(0,0,1,1), col=col[i], border=NA)
    }
    if(!horiz){
      polygon(c(0,0,1,1), poly[[i]], col=col[i], border=NA)
    }
  }
}



### filled contour plot

filled.contour2 <-
  function (x = seq(0, 1, length.out = nrow(z)),
            y = seq(0, 1, length.out = ncol(z)), z, xlim = range(x, finite = TRUE), 
            ylim = range(y, finite = TRUE), zlim = range(z, finite = TRUE), 
            levels = pretty(zlim, nlevels), nlevels = 20, color.palette = cm.colors, 
            col = color.palette(length(levels) - 1), plot.title, plot.axes, 
            key.title, key.axes, asp = NA, xaxs = "i", yaxs = "i", las = 1, 
            axes = TRUE, frame.plot = axes,mar, ...) 
  {
    # modification by Ian Taylor of the filled.contour function
    # to remove the key and facilitate overplotting with contour()
    if (missing(z)) {
      if (!missing(x)) {
        if (is.list(x)) {
          z <- x$z
          y <- x$y
          x <- x$x
        }
        else {
          z <- x
          x <- seq.int(0, 1, length.out = nrow(z))
        }
      }
      else stop("no 'z' matrix specified")
    }
    else if (is.list(x)) {
      y <- x$y
      x <- x$x
    }
    if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
      stop("increasing 'x' and 'y' values expected")
    mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
    on.exit(par(par.orig))
    w <- (3 + mar.orig[2]) * par("csi") * 2.54
    par(las = las)
    mar <- mar.orig
    plot.new()
    par(mar=mar)
    plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
    if (!is.matrix(z) || nrow(z) <= 1 || ncol(z) <= 1) 
      stop("no proper 'z' matrix specified")
    if (!is.double(z)) 
      storage.mode(z) <- "double"
    .filled.contour(as.double(x), as.double(y), z, as.double(levels), 
                    col = col)
    if (missing(plot.axes)) {
      if (axes) {
        title(main = "", xlab = "", ylab = "")
        Axis(x, side = 1)
        Axis(y, side = 4)
      }
    }
    else plot.axes
    if (frame.plot) 
      box()
    if (missing(plot.title)) 
      title(...)
    else plot.title
    invisible()
  }


### wind arrow plot
quiver<- function(u,v,x.val,y.val,scale=1.5,length=leng)
{
  xx <- matrix(ncol=length(x.val),rep((x.val),length(y.val)),nrow=length(y.val),byrow=TRUE)
  yy <- matrix(ncol=length(x.val),rep((y.val),length(x.val)),nrow=length(y.val),byrow=FALSE)
  speed <- sqrt(u*u+v*v)
  maxspeed <- max(speed)
  u <- u*scale/maxspeed
  v <- v*scale/maxspeed
  arrows(xx,yy[,(length(yy[1,])):1],xx+t(u),yy[,(length(yy[1,])):1]+t(v),length=length*min(par.uin()),lwd=0.8)
}
par.uin <- function()# determine scale of inches/userunits in x and y
{
  u <- par("usr")
  p <- par("pin")
  c(p[1]/(u[2] - u[1]), p[2]/(u[4] - u[3]))
}
quiver.leg <- function(u,v,x.val,y.val,scale=1.5,length=leng)
{
  xx <- matrix(ncol=length(x.val),rep((x.val),length(y.val)),nrow=length(y.val),byrow=TRUE)
  yy <- matrix(ncol=length(x.val),rep((y.val),length(x.val)),nrow=length(y.val),byrow=FALSE)
  speed <- sqrt(u*u+v*v)
  maxspeed <- max(speed)
  u <- u*scale/maxspeed
  v <- v*scale/maxspeed
  arrows(xx,yy[,length(yy[,1]):1],xx+u,yy[,length(yy[,1]):1]+v,length=leng*min(par.uin()),lwd=0.8)
}

####
monthi <- function(i) {ifelse(i %% 12 == 0,12,i %% 12)}


