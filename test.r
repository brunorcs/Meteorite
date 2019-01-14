# Script de ejemplo para Meteorite

# -------------------------
# LIBRERIAS

library(ncdf4)

# -------------------------
# FUNCIONES

source('Meteorite.r')

# -------------------------
# PROCEDIMIENTO

# Lectura de datos
nc <- nc_open('gfs_0p25_20181113_00z.nc')
rhprs <- ncvar_get(nc, 'rhprs')
ugrdprs <- ncvar_get(nc, 'ugrdprs')
vgrdprs <- ncvar_get(nc, 'vgrdprs')
lat <- ncvar_get(nc, 'lat')
lon <- ncvar_get(nc, 'lon')
nc_close(nc)

# Creando escala de colores
paleta <- color.gs(c(0,90), int = 5, colors = c('white','darkblue'), space = 'Lab')

# Creando gráfica con paleta definida
gxprint('prueba.png', lon, lat, res = 100)
disp.shaded(rhprs[ , ,1], lon, lat, 
	brk = paleta[[1]], pal = paleta[[2]], 
	map = TRUE, grid = TRUE,
	plot.title = title(main = "2018-11-13 00 UTC"))
disp.vector2(ugrdprs[ , ,1], vgrdprs[ , ,2], lon, lat, skip = 4)
cbar(brk = paleta[[1]], pal = paleta[[2]], ylab = 'Hum. Rel. (%)')
dev.off()
