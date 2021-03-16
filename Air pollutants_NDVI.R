##LIBRERÍAS 
library(readxl)
library(dplyr)
library(gridExtra)
library(ggplot2)
library(raster)
library(MODISTools)
library(reshape2)
library(moments)
library(egg)

##Versión de Rstudio: Version 1.4.1106.

##LIMPIEZA DE DATOS

##Para trabajar con los datasets que son los de los índices de vegetación 
##de los satélites MODIS (Terra). Se trabaja con los datos de NDVI (a 250 m), 
##para la región sur de la Ciudad de México desde inicios de 2001 hasta 
##final de 2020. Se descargan y se cargan los datos para poder trabajar. De 
##igual manera, se cargan los datos de fiabilidad de los pixeles. La página 
##de donde se consiguen los datos es https://modis.ornl.gov/sites/
NDVI_data_Terra = 
    mt_subset(product = "MOD13Q1",
              site_id = "mx_distritofederal_mexico_city",
              band = "250m_16_days_NDVI",
              start = "2001-01-01",
              end = "2020-12-31",
              km_lr = 2,
              km_ab = 2,
              site_name = "CDMX",
              internal = TRUE,
              progress = FALSE)
PIXQ_data_Terra =
    mt_subset(product = "MOD13Q1",
              site_id = "mx_distritofederal_mexico_city",
              band = "250m_16_days_pixel_reliability",
              start = "2001-01-01",
              end = "2020-12-31",
              km_lr = 2,
              km_ab = 2,
              site_name = "testsite",
              internal = TRUE,
              progress = FALSE)
NDVI_r = mt_to_raster(df = NDVI_data_Terra)
PIXQ_r = mt_to_raster(df = PIXQ_data_Terra)

##Se limpian los datos usando los data sets de la fiabilidad de los pixeles.
##Para realizar esto, una máscara con base en estos datos con la que se 
##descartan valores inncesarios. 
masc_Terra = PIXQ_r
masc_Terra[(PIXQ_r < 0 | PIXQ_r > 1)] = NA
NDVI_m = mask(NDVI_r, masc_Terra, maskvalue = NA, updatevalue = NA)

##Los datos de RasterBrick son convertidos en data frames. También se cambian
##los datos nulos (representados por NA) por 0, y se crea un data frame
##con las medias de los índices de vegetación. 
NDVI_data = as.data.frame(NDVI_m, xy = TRUE)
NDVI_data[is.na(NDVI_data)] = 0
cleandf = NDVI_data[, -c(1:2)]
dates = as.Date(names(cleandf), "X%Y.%m.%d")
df_mean = as.numeric(colMeans(cleandf))
VI_df = data.frame(dates, df_mean)
names(VI_df) = c("Fechas", "NDVI")
NDVI_df = VI_df

##Para realizar los heatmaps de las medias anuales, se separan por año los datos
##y se combinan con las coordenadas.
coor = NDVI_data[,1:2]
VI2001 = rowMeans(NDVI_data[,3:25])
VI2002 = rowMeans(NDVI_data[,25:48])
VI2003 = rowMeans(NDVI_data[,49:71])
VI2004 = rowMeans(NDVI_data[,72:94])
VI2005 = rowMeans(NDVI_data[,95:117])
VI2006 = rowMeans(NDVI_data[,118:140])
VI2007 = rowMeans(NDVI_data[,141:163])
VI2008 = rowMeans(NDVI_data[,164:186])
VI2009 = rowMeans(NDVI_data[,187:209])
VI2010 = rowMeans(NDVI_data[,210:232])
VI2011 = rowMeans(NDVI_data[,233:255])
VI2012 = rowMeans(NDVI_data[,256:278])
VI2013 = rowMeans(NDVI_data[,279:301])
VI2014 = rowMeans(NDVI_data[,302:324])
VI2015 = rowMeans(NDVI_data[,325:347])
VI2016 = rowMeans(NDVI_data[,348:370])
VI2017 = rowMeans(NDVI_data[,371:393])
VI2018 = rowMeans(NDVI_data[,394:416])
VI2019 = rowMeans(NDVI_data[,417:439])
VI2020 = rowMeans(NDVI_data[,440:462])
VI2001 = data.frame(coor, z = VI2001)
VI2002 = data.frame(coor, z = VI2002)
VI2003 = data.frame(coor, z = VI2003)
VI2004 = data.frame(coor, z = VI2004)
VI2005 = data.frame(coor, z = VI2005)
VI2006 = data.frame(coor, z = VI2006)
VI2007 = data.frame(coor, z = VI2007)
VI2008 = data.frame(coor, z = VI2008)
VI2009 = data.frame(coor, z = VI2009)
VI2010 = data.frame(coor, z = VI2010)
VI2011 = data.frame(coor, z = VI2011)
VI2012 = data.frame(coor, z = VI2012)
VI2013 = data.frame(coor, z = VI2013)
VI2014 = data.frame(coor, z = VI2014)
VI2015 = data.frame(coor, z = VI2015)
VI2016 = data.frame(coor, z = VI2016)
VI2017 = data.frame(coor, z = VI2017)
VI2018 = data.frame(coor, z = VI2018)
VI2019 = data.frame(coor, z = VI2019)
VI2020 = data.frame(coor, z = VI2020)

##Para los datos de contaminación, descargar en el directorio el archivo 
##zip (bases de datos desde 2001 a 2020) y extraer todos los archivos 
##(formato .xls). La url de la base de datos es:
##http://www.aire.cdmx.gob.mx/default.php?opc=%27aKBh%27
unzip(zipfile = "01RAMA.zip")
unzip(zipfile = "02RAMA.zip")
unzip(zipfile = "03RAMA.zip")
unzip(zipfile = "04RAMA.zip")
unzip(zipfile = "05RAMA.zip")
unzip(zipfile = "06RAMA.zip")
unzip(zipfile = "07RAMA.zip")
unzip(zipfile = "08RAMA.zip")
unzip(zipfile = "09RAMA.zip")
unzip(zipfile = "10RAMA.zip")
unzip(zipfile = "11RAMA.zip")
unzip(zipfile = "12RAMA.zip")
unzip(zipfile = "13RAMA.zip")
unzip(zipfile = "14RAMA.zip")
unzip(zipfile = "15RAMA.zip")
unzip(zipfile = "16RAMA.zip")
unzip(zipfile = "17RAMA.zip")
unzip(zipfile = "18RAMA.zip")
unzip(zipfile = "19RAMA.zip")
unzip(zipfile = "20RAMA.zip")

##Cargar cada uno de los archivos por año para monóxidos de carbono (CO) y
##oxidos de nitrógeno (NOx) en variables para utilizar como data frames.
NOX_01_data = read_xls("2001NOX.xls")
O3_01_data = read_xls("2001O3.xls")
NOX_02_data = read_xls("2002NOX.xls")
O3_02_data = read_xls("2002O3.xls")
NOX_03_data = read_xls("2003NOX.xls")
O3_03_data = read_xls("2003O3.xls")
NOX_04_data = read_xls("2004NOX.xls")
O3_04_data = read_xls("2004O3.xls")
NOX_05_data = read_xls("2005NOX.xls")
O3_05_data = read_xls("2005O3.xls")
NOX_06_data = read_xls("2006NOX.xls")
O3_06_data = read_xls("2006O3.xls")
NOX_07_data = read_xls("2007NOX.xls")
O3_07_data = read_xls("2007O3.xls")
NOX_08_data = read_xls("2008NOX.xls")
O3_08_data = read_xls("2008O3.xls")
NOX_09_data = read_xls("2009NOX.xls")
O3_09_data = read_xls("2009O3.xls")
NOX_10_data = read_xls("2010NOX.xls")
O3_10_data = read_xls("2010O3.xls")
NOX_11_data = read_xls("2011NOX.xls")
O3_11_data = read_xls("2011O3.xls")
NOX_12_data = read_xls("2012NOX.xls")
O3_12_data = read_xls("2012O3.xls")
NOX_13_data = read_xls("2013NOX.xls")
O3_13_data = read_xls("2013O3.xls")
NOX_14_data = read_xls("2014NOX.xls")
O3_14_data = read_xls("2014O3.xls")
NOX_15_data = read_xls("2015NOX.xls")
O3_15_data = read_xls("2015O3.xls")
NOX_16_data = read_xls("2016NOX.xls")
O3_16_data = read_xls("2016O3.xls")
NOX_17_data = read_xls("2017NOX.xls")
O3_17_data = read_xls("2017O3.xls")
NOX_18_data = read_xls("2018NOX.xls")
O3_18_data = read_xls("2018O3.xls")
NOX_19_data = read_xls("2019NOX.xls")
O3_19_data = read_xls("2019O3.xls")
NOX_20_data = read_xls("2020NOX.xls")
O3_20_data = read_xls("2020O3.xls")

##Al ser esta base de datos en formato "tbl_df", convertir cada uno 
##en un formato adecuado (en este caso, a data frame).
NOX_01_data = as.data.frame(NOX_01_data)
O3_01_data = as.data.frame(O3_01_data)
NOX_02_data = as.data.frame(NOX_02_data)
O3_02_data = as.data.frame(O3_02_data)
NOX_03_data = as.data.frame(NOX_03_data)
O3_03_data = as.data.frame(O3_03_data)
NOX_04_data = as.data.frame(NOX_04_data)
O3_04_data = as.data.frame(O3_04_data)
NOX_05_data = as.data.frame(NOX_05_data)
O3_05_data = as.data.frame(O3_05_data)
NOX_06_data = as.data.frame(NOX_06_data)
O3_06_data = as.data.frame(O3_06_data)
NOX_07_data = as.data.frame(NOX_07_data)
O3_07_data = as.data.frame(O3_07_data)
NOX_08_data = as.data.frame(NOX_08_data)
O3_08_data = as.data.frame(O3_08_data)
NOX_09_data = as.data.frame(NOX_09_data)
O3_09_data = as.data.frame(O3_09_data)
NOX_10_data = as.data.frame(NOX_10_data)
O3_10_data = as.data.frame(O3_10_data)
NOX_11_data = as.data.frame(NOX_11_data)
O3_11_data = as.data.frame(O3_11_data)
NOX_12_data = as.data.frame(NOX_12_data)
O3_12_data = as.data.frame(O3_12_data)
NOX_13_data = as.data.frame(NOX_13_data)
O3_13_data = as.data.frame(O3_13_data)
NOX_14_data = as.data.frame(NOX_14_data)
O3_14_data = as.data.frame(O3_14_data)
NOX_15_data = as.data.frame(NOX_15_data)
O3_15_data = as.data.frame(O3_15_data)
NOX_16_data = as.data.frame(NOX_16_data)
O3_16_data = as.data.frame(O3_16_data)
NOX_17_data = as.data.frame(NOX_17_data)
O3_17_data = as.data.frame(O3_17_data)
NOX_18_data = as.data.frame(NOX_18_data)
O3_18_data = as.data.frame(O3_18_data)
NOX_19_data = as.data.frame(NOX_19_data)
O3_19_data = as.data.frame(O3_19_data)
NOX_20_data = as.data.frame(NOX_20_data)
O3_20_data = as.data.frame(O3_20_data)

##En cada data frame existen columnas que sólo tienen valores nulos
##(aquí representados como "-99"), por lo que se eliminan de cada 
##data frame -creando el primer subset, sin la fecha y sin las columnas
##con puros valores nulos-. De igual manera, se elimina la columna de
##hora de muestreo, debido a que no importará para este análisis. De
##igual manera, los valores nulos en las columnas con datos relevantes
##serán convertidos en 0. Sólo se usarán las columnas que cuentan con
##los valores de las estaciones del sur de la CDMX, eliminando las otras. 
null_del = function(x) {
    x[colMeans(x) == -99] = NULL
    x[x == -99] = 0
    return(x)
}
clean_NOX01 = NOX_01_data[, 19]
clean_O301 = O3_01_data[, c(18:19)]
clean_NOX02 = NOX_02_data[, c(19)]
clean_O302 = O3_02_data[, c(11, 18, 21)]
clean_NOX03 = NOX_03_data[, c(9, 19)]
clean_O303 = O3_03_data[, c(11, 15, 18, 21)]
clean_NOX04 = NOX_04_data[, c(9, 19)]
clean_O304 = O3_04_data[, c(11, 15, 18:19, 21)]
clean_NOX05 = NOX_05_data[, c(9, 18)]
clean_O305 = O3_05_data[, c(11, 15, 17:18, 20, 22)]
clean_NOX06 = NOX_06_data[, c(9, 18)]
clean_O306 = O3_06_data[, c(11, 15, 17:18, 20, 22)]
clean_NOX07 = NOX_07_data[, c(9, 17)]
clean_O307 = O3_07_data[, c(11, 14, 17, 19, 21, 24)]
clean_NOX08 = NOX_08_data[, c(9, 17)]
clean_O308 = O3_08_data[, c(11, 14, 17, 19, 21, 24)]
clean_NOX09 = NOX_09_data[, c(9, 17)]
clean_O309 = O3_09_data[, c(11, 14, 16, 17, 19, 21, 24)]
clean_NOX10 = NOX_10_data[, c(9, 16)]
clean_O310 = O3_10_data[, c(11, 14, 16:17, 19, 21, 24)]
clean_NOX11 = NOX_11_data[, c(6:8, 20, 23:24)]
clean_O311 = O3_11_data[, c(5:7, 15, 19, 22:23)]
clean_NOX12 = NOX_12_data[, c(6:8, 18, 20, 23, 26:28)]
clean_O312 = O3_12_data[, c(5:7, 16, 18, 21, 24:26)]
clean_NOX13 = NOX_13_data[, c(6:8, 18, 20, 23, 26:28)]
clean_O313 = O3_13_data[, c(6:8, 18, 20, 23, 26:28)]
clean_NOX14 = NOX_14_data[, c(6:7, 9, 19, 21, 24, 27:29)]
clean_O314 = O3_14_data[, c(6:7, 9, 19, 21, 24, 27:29)]
clean_NOX15 = NOX_15_data[, c(9:11, 23, 25, 28, 31:33)]
clean_O315 = O3_15_data[, c(5, 10:12, 25, 27, 30, 33:35)]
clean_NOX16 = NOX_16_data[, c(9:11, 24, 26, 28, 31:32)]
clean_O316 = O3_16_data[, c(5, 10:12, 26, 28, 30, 33:34)]
clean_NOX17 = NOX_17_data[, c(9:11, 24, 26, 28, 31:32)]
clean_O317 = O3_17_data[, c(5, 10:12, 26, 28, 30, 33:34)]
clean_NOX18 = NOX_18_data[, c(8:10, 23, 25, 27, 30:31)]
clean_O318 = O3_18_data[, c(5, 10:12, 26, 28, 30, 33:34)]
clean_NOX19 = NOX_19_data[, c(9:11, 25, 28, 30, 33:34)]
clean_O319 = O3_19_data[, c(5, 10:12, 28, 31, 33, 36:37)]
clean_NOX20 = NOX_20_data[, c(9:11, 25, 28, 30, 33:34)]
clean_O320 = O3_20_data[, c(5, 10:12, 27, 30, 32, 35:36)]
clean_NOX01[clean_NOX01 == -99] = 0
clean_O301 = null_del(clean_O301)
clean_NOX02[clean_NOX02 == -99] = 0
clean_O302 = null_del(clean_O302)
clean_NOX03 = null_del(clean_NOX03)
clean_O303 = null_del(clean_O303)
clean_NOX04 = null_del(clean_NOX04)
clean_O304 = null_del(clean_O304)
clean_NOX05 = null_del(clean_NOX05)
clean_O305 = null_del(clean_O305)
clean_NOX06 = null_del(clean_NOX06)
clean_O306 = null_del(clean_O306)
clean_NOX07 = null_del(clean_NOX07)
clean_O307 = null_del(clean_O307)
clean_NOX08 = null_del(clean_NOX08)
clean_O308 = null_del(clean_O308)
clean_NOX09 = null_del(clean_NOX09)
clean_O309 = null_del(clean_O309)
clean_NOX10 = null_del(clean_NOX10)
clean_O310 = null_del(clean_O310)
clean_NOX11 = null_del(clean_NOX11)
clean_O311 = null_del(clean_O311)
clean_NOX12 = null_del(clean_NOX12)
clean_O312 = null_del(clean_O312)
clean_NOX13 = null_del(clean_NOX13)
clean_O313 = null_del(clean_O313)
clean_NOX14 = null_del(clean_NOX14)
clean_O314 = null_del(clean_O314)
clean_NOX15 = null_del(clean_NOX15)
clean_O315 = null_del(clean_O315)
clean_NOX16 = null_del(clean_NOX16)
clean_O316 = null_del(clean_O316)
clean_NOX17 = null_del(clean_NOX17)
clean_O317 = null_del(clean_O317)
clean_NOX18 = null_del(clean_NOX18)
clean_O318 = null_del(clean_O318)
clean_NOX19 = null_del(clean_NOX19)
clean_O319 = null_del(clean_O319)
clean_NOX20 = null_del(clean_NOX20)
clean_O320 = null_del(clean_O320)

##Se genera una función con la que se calcula la media por día entre todos
##los puntos de muestreo, para cada contaminante por año. 
comb_col = function(data, year) {
    if (year == "2001") {
        fechas = seq(as.Date("2001-1-1"), as.Date("2001-12-31"), "days")
    }
    if (year == "2002") {
        fechas = seq(as.Date("2002-1-1"), as.Date("2002-12-31"), "days")
    }
    if (year == "2003") {
        fechas = seq(as.Date("2003-1-1"), as.Date("2003-12-31"), "days")
    }
    if (year == "2004") {
        fechas = seq(as.Date("2004-1-1"), as.Date("2004-12-31"), "days")
    }
    if (year == "2005") {
        fechas = seq(as.Date("2005-1-1"), as.Date("2005-12-31"), "days")
    }
    if (year == "2006") {
        fechas = seq(as.Date("2006-1-1"), as.Date("2006-12-31"), "days")
    }
    if (year == "2007") {
        fechas = seq(as.Date("2007-1-1"), as.Date("2007-12-31"), "days")
    }
    if (year == "2008") {
        fechas = seq(as.Date("2008-1-1"), as.Date("2008-12-31"), "days")
    }
    if (year == "2009") {
        fechas = seq(as.Date("2009-1-1"), as.Date("2009-12-31"), "days")
    }
    if (year == "2010") {
        fechas = seq(as.Date("2010-1-1"), as.Date("2010-12-31"), "days")
    }
    if (year == "2011") {
        fechas = seq(as.Date("2011-1-1"), as.Date("2011-12-31"), "days")
    }
    if (year == "2012") {
        fechas = seq(as.Date("2012-1-1"), as.Date("2012-12-31"), "days")
    }
    if (year == "2013") {
        fechas = seq(as.Date("2013-1-1"), as.Date("2013-12-31"), "days")
    }
    if (year == "2014") {
        fechas = seq(as.Date("2014-1-1"), as.Date("2014-12-31"), "days")
    }
    if (year == "2015") {
        fechas = seq(as.Date("2015-1-1"), as.Date("2015-12-31"), "days")
    }
    if (year == "2016") {
        fechas = seq(as.Date("2016-1-1"), as.Date("2016-12-31"), "days")
    }
    if (year == "2017") {
        fechas = seq(as.Date("2017-1-1"), as.Date("2017-12-31"), "days")
    }
    if (year == "2018") {
        fechas = seq(as.Date("2018-1-1"), as.Date("2018-12-31"), "days")
    }
    if (year == "2019") {
        fechas = seq(as.Date("2019-1-1"), as.Date("2019-12-31"), "days")
    }
    if (year == "2020") {
        fechas = seq(as.Date("2020-1-1"), as.Date("2020-12-31"), "days")
    }
    comb_data = rowMeans(data)
    comb_df = data.frame(fechas, comb_data)
    daymean = aggregate(. ~ fechas, comb_df, mean)
    return(daymean)
}
comb_NOX01 = data.frame(
    fechas = c(
        seq(as.Date("2001-1-1"), as.Date("2001-12-31"), "days")), comb_data = clean_NOX01)
comb_NOX01 = aggregate(. ~ fechas, comb_NOX01, mean)
comb_O301 = comb_col(clean_O301, 2001)
comb_NOX02 = data.frame(
    fechas = c(
        seq(as.Date("2002-1-1"), as.Date("2002-12-31"), "days")), comb_data = clean_NOX02)
comb_NOX02 = aggregate(. ~ fechas, comb_NOX02, mean)
comb_O302 = comb_col(clean_O302, 2002)
comb_NOX03 = comb_col(clean_NOX03, 2003)
comb_O303 = comb_col(clean_O303, 2003)
comb_NOX04 = comb_col(clean_NOX04, 2004)
comb_O304 = comb_col(clean_O304, 2004)
comb_NOX05 = comb_col(clean_NOX05, 2005)
comb_O305 = comb_col(clean_O305, 2005)
comb_NOX06 = comb_col(clean_NOX06, 2006)
comb_O306 = comb_col(clean_O306, 2006)
comb_NOX07 = comb_col(clean_NOX07, 2007)
comb_O307 = comb_col(clean_O307, 2007)
comb_NOX08 = comb_col(clean_NOX08, 2008)
comb_O308 = comb_col(clean_O308, 2008)
comb_NOX09 = comb_col(clean_NOX09, 2009)
comb_O309 = comb_col(clean_O309, 2009)
comb_NOX10 = comb_col(clean_NOX10, 2010)
comb_O310 = comb_col(clean_O310, 2010)
comb_NOX11 = comb_col(clean_NOX11, 2011)
comb_O311 = comb_col(clean_O311, 2011)
comb_NOX12 = comb_col(clean_NOX12, 2012)
comb_O312 = comb_col(clean_O312, 2012)
comb_NOX13 = comb_col(clean_NOX13, 2013)
comb_O313 = comb_col(clean_O313, 2013)
comb_NOX14 = comb_col(clean_NOX14, 2014)
comb_O314 = comb_col(clean_O314, 2014)
comb_NOX15 = comb_col(clean_NOX15, 2015)
comb_O315 = comb_col(clean_O315, 2015)
comb_NOX16 = comb_col(clean_NOX16, 2016)
comb_O316 = comb_col(clean_O316, 2016)
comb_NOX17 = comb_col(clean_NOX17, 2017)
comb_O317 = comb_col(clean_O317, 2017)
comb_NOX18 = comb_col(clean_NOX18, 2018)
comb_O318 = comb_col(clean_O318, 2018)
comb_NOX19 = comb_col(clean_NOX19, 2019)
comb_O319 = comb_col(clean_O319, 2019)
comb_NOX20 = comb_col(clean_NOX20, 2020)
comb_O320 = comb_col(clean_O320, 2020)

##La media anual por contaminante es calculada.
NOX_means = data.frame(Año = c(2001:2020), 
                       NOx = c(mean(comb_NOX01[,2]),
                               mean(comb_NOX02[,2]),
                               mean(comb_NOX03[,2]),
                               mean(comb_NOX04[,2]),
                               mean(comb_NOX05[,2]),
                               mean(comb_NOX06[,2]),
                               mean(comb_NOX07[,2]),
                               mean(comb_NOX08[,2]),
                               mean(comb_NOX09[,2]),
                               mean(comb_NOX10[,2]),
                               mean(comb_NOX11[,2]),
                               mean(comb_NOX12[,2]),
                               mean(comb_NOX13[,2]),
                               mean(comb_NOX14[,2]),
                               mean(comb_NOX15[,2]), 
                               mean(comb_NOX16[,2]), 
                               mean(comb_NOX17[,2]), 
                               mean(comb_NOX18[,2]), 
                               mean(comb_NOX19[,2]),
                               mean(comb_NOX20[,2])))
O3_means = data.frame(Año = c(2001:2020), 
                      O3 = c(mean(comb_O301[,2]), 
                             mean(comb_O302[,2]), 
                             mean(comb_O303[,2]), 
                             mean(comb_O304[,2]), 
                             mean(comb_O305[,2]), 
                             mean(comb_O306[,2]), 
                             mean(comb_O307[,2]), 
                             mean(comb_O308[,2]), 
                             mean(comb_O309[,2]), 
                             mean(comb_O310[,2]), 
                             mean(comb_O311[,2]), 
                             mean(comb_O312[,2]), 
                             mean(comb_O313[,2]), 
                             mean(comb_O314[,2]), 
                             mean(comb_O315[,2]), 
                             mean(comb_O316[,2]), 
                             mean(comb_O317[,2]), 
                             mean(comb_O318[,2]), 
                             mean(comb_O319[,2]),
                             mean(comb_O320[,2])))
contmeans = merge(NOX_means, O3_means, by = "Año")
media_cont = melt(contmeans, id.vars = "Año")

##Una vez limpiados los datos, se crea un subset con las medias de cada
##contaminante por cada fecha indicada en los registros para el índice de 
##vegetación de MODIS. Para realizar esto, se combinan los data frames
##de cada contaminante, para tener todas las fechas de este en un mismo
##data frame.
NOX_total = rbind(comb_NOX01, comb_NOX02, comb_NOX03, comb_NOX04, comb_NOX05,
                  comb_NOX06, comb_NOX07, comb_NOX08, comb_NOX09, comb_NOX10,
                  comb_NOX11, comb_NOX12, comb_NOX13, comb_NOX14,
                  comb_NOX15, comb_NOX16, comb_NOX17, comb_NOX18, comb_NOX19, comb_NOX20)
names(NOX_total) = c("Fechas", "NOx")
O3_total = rbind(comb_O301, comb_O302, comb_O303, comb_O304, comb_O305, 
                 comb_O306, comb_O307, comb_O308, comb_O309, comb_O310, 
                 comb_O311, comb_O312, comb_O313, comb_O314,
                 comb_O315, comb_O316, comb_O317, comb_O318, comb_O319, comb_O320)
names(O3_total) = c("Fechas", "O3")
NOX_VI = filter(NOX_total, Fechas %in% dates)
O3_VI = filter(O3_total, Fechas %in% dates)

##Con la función merge, un nuevo data frame con ambas medias de los contaminantes
##es creada.
names(NOX_VI) = c("Fechas", "Contaminantes")
names(O3_VI) = c("Fechas", "Contaminantes")
cont_VI = rbind(NOX_VI, O3_VI)
cont_VI$Contaminante = c(rep("NOx", 460), rep("O3", 460)) 

##ANÁLISIS ESTADÍSTICOS

##Primero se juntan todos los datos (contaminantes e NDVI) en un sólo df.
data_exp = data.frame(
    NOx = NOX_VI[,2], O3 = O3_VI[,2], NDVI = NDVI_df[,2])

##Posteriormente se realiza una prueba de D'Agostino para la normalidad de
##los datos.
agostino.test(data_exp$NOx) ##NOx
agostino.test(data_exp$O3) ##O3
agostino.test(data_exp$NDVI) ##NDVI

##Al presentar oblicuidad, contrastar con gráficas de densidad.
NOX_dist = ggplot(data_exp, aes(x = NOx)) + geom_density() + 
    geom_vline(aes(xintercept=mean(NOx), size = 2.5, color = "gray32"),
               color = "#E69F00", linetype = "dashed", size = 2) +
    ylab("Ocurrencia") + 
    xlab("Concentración NOx (ppb)") +
    ggtitle("Distribución de los datos de concentración de NOx") +
    theme(plot.title = element_text(hjust = 0.5)) + 
    theme(axis.text.x = element_text(
        vjust = 0.5, hjust=1, size = 10, color = "black"),
        axis.text.y = element_text(vjust = 0.5, hjust = 1, size = 10, 
                                   color = "black"),
        plot.margin = margin(2.2, 0.5, 2.2, 0.5, "cm"),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) 
NOX_dist
O3_dist = ggplot(data_exp, aes(x = O3)) + geom_density() + 
    geom_vline(aes(xintercept=mean(O3), size = 2.5, color = "gray32"),
               color = "#E69F00", linetype = "dashed", size = 2) +
    ylab("Ocurrencia") + 
    xlab("Concentración O3 (ppb)") +
    ggtitle("Distribución de los datos de concentración de O3") +
    theme(plot.title = element_text(hjust = 0.5)) + 
    theme(axis.text.x = element_text(
        vjust = 0.5, hjust=1, size = 10, color = "black"),
        axis.text.y = element_text(vjust = 0.5, hjust = 1, size = 10, 
                                   color = "black"),
        plot.margin = margin(2.2, 0.5, 2.2, 0.5, "cm"),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) 
O3_dist
NDVI_dist = ggplot(data_exp, aes(x = NDVI)) + geom_density() + 
    geom_vline(aes(xintercept = mean(NDVI), size = 2.5, color = "gray32"),
               color = "#E69F00", linetype = "dashed", size = 2) +
    ylab("Ocurrencia") + 
    xlab("NDVI") +
    ggtitle("Distribución de NDVI") +
    theme(plot.title = element_text(hjust = 0.5)) + 
    theme(axis.text.x = element_text(
        vjust = 0.5, hjust=1, size = 10, color = "black"),
        axis.text.y = element_text(vjust = 0.5, hjust = 1, size = 10, 
                                   color = "black"),
        plot.margin = margin(2.2, 0.5, 2.2, 0.5, "cm"),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) 
NDVI_dist

##Para realizar este análisis, se utiliza un análisis regresión lineal simple,
##con una prueba de significancia de la pendiente. Se tienen dos hipótesis:
##Ho = No existe relación significativa entre los contaminantes y el VI,
##por lo que las medias son iguales.
##Ha = Las medias de las variables son diferentes, por lo que sí hay 
##una relación.
##Se describe una función para obtener la tabla de ANOVA con los cálculos
##de significancia competentes.
rel_NOX = data.frame(x = data_exp[,1], y = data_exp[,3])
rel_O3 = data.frame(x = data_exp[,2], y = data_exp[,3])
estad_prueb = function(data) {
    ajuste = lm(data[,2] ~ data[,1], x = TRUE)
    y = data[,2]
    n = length(y)
    ss_t = sum(y^2) - sum(y)^2 / n
    ss_r = matrix(
        coef(ajuste), nrow = 1) %*% t(ajuste$x) %*% matrix(y, ncol=1) - sum(y)^2 / n
    ss_res = ss_t - ss_r
    ms_r = ss_r / (
        length(coef(ajuste))-1)
    ms_res = ss_res / (
        n-length(coef(ajuste)))
    F0 = ms_r / ms_res
    valorP = pf(
        F0, df1=length(coef(ajuste))-1, 
        df2=(n-length(coef(ajuste))), lower.tail=FALSE)
    tabla = matrix(NA, ncol = 5, nrow = 3)
    tabla[1, ] = c(
        ss_r, length(coef(ajuste))-1, ms_r, F0, valorP)
    tabla[2, 1:3] = c(
        ss_res, n-length(coef(ajuste)), ms_res)
    tabla[3, 1:2] = c(
        ss_t, n-1)
    colnames(tabla) = c(
        'Suma Cuadrados', 'gl', 'Cuadrado medio', 'F0', 'Valor-P')
    rownames(tabla) = c(
        'Reg', 'Resid', 'Total')
    return(tabla)
}
estad_prueb(rel_NOX)
estad_prueb(rel_O3)

##PLOTS

##Se realiza un plot de time series de los valores de índice de vegetación
##para el sur de la CDMX. Se elmimnan todos los valores menores a 0.1,
##ya que estos ya no representan áreas fotosintéticas. 
índices = c(0.1, 0.2, 0.3)
ts_NDVI = ggplot(
    NULL, aes(
        x = Fechas, y = NDVI)) +
    ylab("Índice de Vegetación de Diferencia Normalizada") + 
    xlab("") +
    ggtitle("Diferencia del NDVI del sur de CDMX en los últimos 20 años") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_date(date_breaks = "year", date_labels = "%Y") +
    theme(axis.text.x = element_text(
        angle = 45, vjust = 0.5, hjust=1, size = 10, color = "black"),
        axis.text.y = element_text(vjust = 0.5, hjust = 1, size = 10, 
                                   color = "black"),
        plot.margin = margin(2.2, 0.5, 2.2, 0.5, "cm"),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
    geom_hline(
        mapping = NULL, yintercept = índices, colour = "grey80") +
    geom_line(
        data = NDVI_df, color = "gray32", size = 1) 
ts_NDVI

##Un time series de los contaminantes es creado.
índices_cont = c(20, 30, 40, 50, 60)
ts_cont = ggplot(
    cont_VI, aes(
        x = Fechas, y = Contaminantes,
        group = Contaminante, col = Contaminante, fill = Contaminante)) +
    scale_color_manual(values=c('grey51','#E69F00')) +
    ylab("Concentración de contaminantes (ppb)") + 
    xlab("") +
    ggtitle("Evolución de la concentración de NOx y O3 en el sur de la CDMX desde el 2001 al 2020") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_date(
        date_breaks = "year", date_labels = "%Y") +
    theme(
        axis.text.x = element_text(
            angle = 45, vjust = 0.5, hjust=1, size = 10, color = "black"),
        axis.text.y = element_text(
            vjust = 0.5, hjust = 1, size = 10, color = "black"),
        plot.margin = margin(2.2, 0.1, 2.2, 0.1, "cm"),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
    geom_hline(
        mapping = NULL, yintercept = índices_cont, colour = "grey80") +
    geom_line(size = 1)
ts_cont   

##Para poder observar la relación entre los contaminantes y el índice de
##vegetación, se hacen gráficas de disperción con sus respectivas líneas
##de regresión, y ecuaciones de la pendiente.
lm_eq = function(df) {
    m = lm(y ~ x, df);
    eq = substitute((y) == a + b %.% (x)*","~~(r)^2~"="~r2, 
                    list(a = format(unname(coef(m)[1]), digits = 2),
                         b = format(unname(coef(m)[2]), digits = 2),
                         r2 = format(summary(m)$r.squared, digits = 3)))
    as.character(as.expression(eq));
}
índices = c(0.1, 0.2, 0.3)
relación_NOX = tibble(x = NOX_VI[,2], y = NDVI_df[,2])
rel_NOX = ggplot(
    data = relación_NOX, aes(x = x, y = y)) +
    ylab("") + 
    xlab("NOx (ppb)")+
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(
        vjust = 0.5, hjust=1, size = 10, color = "black"),
        axis.text.y = element_text(vjust = 0.5, hjust = 1, size = 10, 
                                   color = "black"),
        plot.margin = margin(1.5, 0, 1.5, 0, "cm"),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
    geom_hline(
        mapping = NULL, yintercept = índices, colour = "grey80") +
    geom_point(size = 1, color = "gray32") + 
    geom_smooth(
        method = "lm", color = "grey51", size = 1.5, formula = y ~ x)  +
    geom_label(
        x = 50, y = 0.35, label = lm_eq(relación_NOX), parse = TRUE)
relación_O3 = tibble(x = O3_VI[,2], y = NDVI_df[,2])
rel_O3 = ggplot(
    data = relación_O3, aes(x = x, y = y)) +
    ylab("") + 
    xlab("O3 (ppb)") +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(
        vjust = 0.5, hjust=1, size = 10, color = "black"),
        plot.margin = margin(0, 1.5, 0, 1.5, "cm"),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
    geom_hline(
        mapping = NULL, yintercept = índices, colour = "grey80") +
    geom_point(size = 1, color = "gray32") + 
    geom_smooth(method = "lm", color = "#E69F00", size = 1.5, 
                formula = y ~ x) +
    geom_label(
        x = 40, y = 0.35, label = lm_eq(relación_O3), parse = TRUE)
egg::ggarrange(
    rel_NOX, rel_O3,
    left = "Índice de Vegetación de Diferencia Normalizada",
    top = "Relaciones entre contaminantes e Índice de Vegetación")

##Se realiza una serie de heatmaps de las medias de NDVI anuales.
hm_01 = ggplot(VI2001, aes(x = x, y = y, fill = z)) + 
    geom_tile() +
    scale_fill_gradient(
        low="orange2", high="darkgreen", trans = "sqrt", name = "NDVI") +
    scale_alpha(range = c(0.5, 1.0)) +
    ylab("") + 
    xlab("") +
    ggtitle("2001") +
    theme(plot.title = element_text(hjust = 0.5, size = 10)) +
    theme(legend.title = element_blank(),
          legend.position = "none") +
    theme(
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.margin = margin(0, 0, 0, 0, "cm"),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))
hm_02 = ggplot(VI2002, aes(x = x, y = y, fill = z)) + 
    geom_tile() +
    scale_fill_gradient(
        low="orange2", high="darkgreen", trans = "sqrt", name = "NDVI") +
    scale_alpha(range = c(0.5, 1.0)) +
    ylab("") + 
    xlab("") +
    ggtitle("2002") +
    theme(plot.title = element_text(hjust = 0.5, size = 10)) +
    theme(legend.title = element_blank(),
          legend.position = "none") +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          plot.margin = margin(0, 0, 0, 0, "cm"),
          panel.background = element_rect(fill = "white"),
          axis.line.x = element_line(color="black", size = 0.5),
          axis.line.y = element_line(color="black", size = 0.5))
hm_03 = ggplot(VI2003, aes(x = x, y = y, fill = z)) + 
    geom_tile() +
    scale_fill_gradient(
        low="orange2", high="darkgreen", trans = "sqrt", name = "NDVI") +
    scale_alpha(range = c(0.5, 1.0)) +
    ylab("") + 
    xlab("") +
    ggtitle("2003") +
    theme(plot.title = element_text(hjust = 0.5, size = 10)) +
    theme(legend.title = element_blank(),
          legend.position = "none") +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          plot.margin = margin(0, 0, 0, 0, "cm"),
          panel.background = element_rect(fill = "white"),
          axis.line.x = element_line(color="black", size = 0.5),
          axis.line.y = element_line(color="black", size = 0.5))
hm_04 = ggplot(VI2004, aes(x = x, y = y, fill = z)) + 
    geom_tile() +
    scale_fill_gradient(
        low="orange2", high="darkgreen", trans = "sqrt", name = "NDVI") +
    scale_alpha(range = c(0.5, 1.0)) +
    ylab("") + 
    xlab("") +
    ggtitle("2004") +
    theme(plot.title = element_text(hjust = 0.5, size = 10)) +
    theme(legend.title = element_blank(),
          legend.position = "none") +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          plot.margin = margin(0, 0, 0, 0, "cm"),
          panel.background = element_rect(fill = "white"),
          axis.line.x = element_line(color="black", size = 0.5),
          axis.line.y = element_line(color="black", size = 0.5))
hm_05 = ggplot(VI2005, aes(x = x, y = y, fill = z)) + 
    geom_tile() +
    scale_fill_gradient(
        low="orange2", high="darkgreen", trans = "sqrt", name = "NDVI") +
    scale_alpha(range = c(0.5, 1.0)) +
    ylab("") + 
    xlab("") +
    ggtitle("2005") +
    theme(plot.title = element_text(hjust = 0.5, size = 10)) +
    theme(legend.title = element_blank(),
          legend.position = "none") +
    theme(
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.margin = margin(0, 0, 0, 0, "cm"),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))
hm_06 = ggplot(VI2006, aes(x = x, y = y, fill = z)) + 
    geom_tile() +
    scale_fill_gradient(
        low="orange2", high="darkgreen", trans = "sqrt", name = "NDVI") +
    scale_alpha(range = c(0.5, 1.0)) +
    ylab("") + 
    xlab("") +
    ggtitle("2006") +
    theme(plot.title = element_text(hjust = 0.5, size = 10)) +
    theme(legend.title = element_blank(),
          legend.position = "none") +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          plot.margin = margin(0, 0, 0, 0, "cm"),
          panel.background = element_rect(fill = "white"),
          axis.line.x = element_line(color="black", size = 0.5),
          axis.line.y = element_line(color="black", size = 0.5))
hm_07 = ggplot(VI2007, aes(x = x, y = y, fill = z)) + 
    geom_tile() +
    scale_fill_gradient(
        low="orange2", high="darkgreen", trans = "sqrt", name = "NDVI") +
    scale_alpha(range = c(0.5, 1.0)) +
    ylab("") + 
    xlab("") +
    ggtitle("2007") +
    theme(plot.title = element_text(hjust = 0.5, size = 10)) +
    theme(legend.title = element_blank(),
          legend.position = "none") +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          plot.margin = margin(0, 0, 0, 0, "cm"),
          panel.background = element_rect(fill = "white"),
          axis.line.x = element_line(color="black", size = 0.5),
          axis.line.y = element_line(color="black", size = 0.5))
hm_08 = ggplot(VI2008, aes(x = x, y = y, fill = z)) + 
    geom_tile() +
    scale_fill_gradient(
        low="orange2", high="darkgreen", trans = "sqrt", name = "NDVI") +
    scale_alpha(range = c(0.5, 1.0)) +
    ylab("") + 
    xlab("") +
    ggtitle("2008") +
    theme(plot.title = element_text(hjust = 0.5, size = 10)) +
    theme(legend.title = element_blank(),
          legend.position = "none") +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          plot.margin = margin(0, 0, 0, 0, "cm"),
          panel.background = element_rect(fill = "white"),
          axis.line.x = element_line(color="black", size = 0.5),
          axis.line.y = element_line(color="black", size = 0.5))
hm_09 = ggplot(VI2009, aes(x = x, y = y, fill = z)) + 
    geom_tile() +
    scale_fill_gradient(
        low="orange2", high="darkgreen", trans = "sqrt", name = "NDVI") +
    scale_alpha(range = c(0.5, 1.0)) +
    ylab("") + 
    xlab("") +
    ggtitle("2009") +
    theme(plot.title = element_text(hjust = 0.5, size = 10)) +
    theme(legend.title = element_blank(),
          legend.position = "none") +
    theme(
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.margin = margin(0, 0, 0, 0, "cm"),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))
hm_10 = ggplot(VI2010, aes(x = x, y = y, fill = z)) + 
    geom_tile() +
    scale_fill_gradient(
        low="orange2", high="darkgreen", trans = "sqrt", name = "NDVI") +
    scale_alpha(range = c(0.5, 1.0)) +
    ylab("") + 
    xlab("") +
    ggtitle("2010") +
    theme(plot.title = element_text(hjust = 0.5, size = 10)) +
    theme(legend.title = element_blank(),
          legend.position = "none") +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          plot.margin = margin(0, 0, 0, 0, "cm"),
          panel.background = element_rect(fill = "white"),
          axis.line.x = element_line(color="black", size = 0.5),
          axis.line.y = element_line(color="black", size = 0.5))
hm_11 = ggplot(VI2011, aes(x = x, y = y, fill = z)) + 
    geom_tile() +
    scale_fill_gradient(
        low="orange2", high="darkgreen", trans = "sqrt", name = "NDVI") +
    scale_alpha(range = c(0.5, 1.0)) +
    ylab("") + 
    xlab("") +
    ggtitle("2011") +
    theme(plot.title = element_text(hjust = 0.5, size = 10)) +
    theme(legend.title = element_blank(),
          legend.position = "none") +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          plot.margin = margin(0, 0, 0, 0, "cm"),
          panel.background = element_rect(fill = "white"),
          axis.line.x = element_line(color="black", size = 0.5),
          axis.line.y = element_line(color="black", size = 0.5))
hm_12 = ggplot(VI2012, aes(x = x, y = y, fill = z)) + 
    geom_tile() +
    scale_fill_gradient(
        low="orange2", high="darkgreen", trans = "sqrt", name = "NDVI") +
    scale_alpha(range = c(0.5, 1.0)) +
    ylab("") + 
    xlab("") +
    ggtitle("2012") +
    theme(plot.title = element_text(hjust = 0.5, size = 10)) +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          plot.margin = margin(0, 0, 0, 0, "cm"),
          panel.background = element_rect(fill = "white"),
          axis.line.x = element_line(color="black", size = 0.5),
          axis.line.y = element_line(color="black", size = 0.5))
hm_13 = ggplot(VI2013, aes(x = x, y = y, fill = z)) + 
    geom_tile() +
    scale_fill_gradient(
        low="orange2", high="darkgreen", trans = "sqrt", name = "NDVI") +
    scale_alpha(range = c(0.5, 1.0)) +
    ylab("") + 
    xlab("") +
    ggtitle("2013") +
    theme(plot.title = element_text(hjust = 0.5, size = 10)) +
    theme(legend.title = element_blank(),
          legend.position = "none") +
    theme(
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.margin = margin(0, 0, 0, 0, "cm"),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))
hm_14 = ggplot(VI2014, aes(x = x, y = y, fill = z)) + 
    geom_tile() +
    scale_fill_gradient(
        low="orange2", high="darkgreen", trans = "sqrt", name = "NDVI") +
    scale_alpha(range = c(0.5, 1.0)) +
    ylab("") + 
    xlab("") +
    ggtitle("2014") +
    theme(plot.title = element_text(hjust = 0.5, size = 10)) +
    theme(legend.title = element_blank(),
          legend.position = "none") +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          plot.margin = margin(0, 0, 0, 0, "cm"),
          panel.background = element_rect(fill = "white"),
          axis.line.x = element_line(color="black", size = 0.5),
          axis.line.y = element_line(color="black", size = 0.5))
hm_15 = ggplot(VI2015, aes(x = x, y = y, fill = z)) + 
    geom_tile() +
    scale_fill_gradient(
        low="orange2", high="darkgreen", trans = "sqrt", name = "NDVI") +
    scale_alpha(range = c(0.5, 1.0)) +
    ylab("") + 
    xlab("") +
    ggtitle("2015") +
    theme(plot.title = element_text(hjust = 0.5, size = 10)) +
    theme(legend.title = element_blank(),
          legend.position = "none") +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          plot.margin = margin(0, 0, 0, 0, "cm"),
          panel.background = element_rect(fill = "white"),
          axis.line.x = element_line(color="black", size = 0.5),
          axis.line.y = element_line(color="black", size = 0.5))
hm_16 = ggplot(VI2016, aes(x = x, y = y, fill = z)) + 
    geom_tile() +
    scale_fill_gradient(
        low="orange2", high="darkgreen", trans = "sqrt", name = "NDVI") +
    scale_alpha(range = c(0.5, 1.0)) +
    ylab("") + 
    xlab("") +
    ggtitle("2016") +
    theme(plot.title = element_text(hjust = 0.5, size = 10)) +
    theme(legend.title = element_blank(),
          legend.position = "none") +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          plot.margin = margin(0, 0, 0, 0, "cm"),
          panel.background = element_rect(fill = "white"),
          axis.line.x = element_line(color="black", size = 0.5),
          axis.line.y = element_line(color="black", size = 0.5))
hm_17 = ggplot(VI2017, aes(x = x, y = y, fill = z)) + 
    geom_tile() +
    scale_fill_gradient(
        low="orange2", high="darkgreen", trans = "sqrt", name = "NDVI") +
    scale_alpha(range = c(0.5, 1.0)) +
    ylab("") + 
    xlab("") +
    ggtitle("2017") +
    theme(plot.title = element_text(hjust = 0.5, size = 10)) +
    theme(legend.title = element_blank(),
          legend.position = "none") +
    theme(axis.text.x = element_text(angle = 45,
                                     vjust = 0.5, hjust=1, size = 10),
          axis.text.y = element_text(vjust = 0.5, hjust = 1, size = 10, 
                                     color = "black"),
          plot.margin = margin(0, 0, 0, 0, "cm"),
          panel.background = element_rect(fill = "white"),
          axis.line.x = element_line(color="black", size = 0.5),
          axis.line.y = element_line(color="black", size = 0.5))
hm_18 = ggplot(VI2018, aes(x = x, y = y, fill = z)) + 
    geom_tile() +
    scale_fill_gradient(
        low="orange2", high="darkgreen", trans = "sqrt", name = "NDVI") +
    scale_alpha(range = c(0.5, 1.0)) +
    ylab("") + 
    xlab("") +
    ggtitle("2018") +
    theme(plot.title = element_text(hjust = 0.5, size = 10)) +
    theme(legend.title = element_blank(),
          legend.position = "none") +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.x = element_text(angle = 45,
                                     vjust = 0.5, hjust=1, size = 10, color = "black"),
          plot.margin = margin(0, 0, 0, 0, "cm"),
          panel.background = element_rect(fill = "white"),
          axis.line.x = element_line(color="black", size = 0.5),
          axis.line.y = element_line(color="black", size = 0.5))
hm_19 = ggplot(VI2019, aes(x = x, y = y, fill = z)) + 
    geom_tile() +
    scale_fill_gradient(
        low="orange2", high="darkgreen", trans = "sqrt", name = "NDVI") +
    scale_alpha(range = c(0.5, 1.0)) +
    ylab("") + 
    xlab("") +
    ggtitle("2019") +
    theme(plot.title = element_text(hjust = 0.5, size = 10)) +
    theme(legend.title = element_blank(),
          legend.position = "none") +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.x = element_text(angle = 45,
                                     vjust = 0.5, hjust=1, size = 10, color = "black"),
          plot.margin = margin(0, 0, 0, 0, "cm"),
          panel.background = element_rect(fill = "white"),
          axis.line.x = element_line(color="black", size = 0.5),
          axis.line.y = element_line(color="black", size = 0.5))
hm_20 = ggplot(VI2020, aes(x = x, y = y, fill = z)) +
    geom_tile() +
    scale_fill_gradient(
        low="orange2", high="darkgreen", trans = "sqrt", name = "NDVI") +
    scale_alpha(range = c(0.5, 1.0)) +
    ylab("") + 
    xlab("") +
    ggtitle("2020") +
    theme(plot.title = element_text(hjust = 0.5, size = 10)) +
    theme(legend.title = element_blank(),
          legend.position = "none") +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.x = element_text(angle = 45,
                                     vjust = 0.5, hjust=1, size = 10, color = "black"),
          plot.margin = margin(0, 0, 0, 0, "cm"),
          panel.background = element_rect(fill = "white"),
          axis.line.x = element_line(color="black", size = 0.5),
          axis.line.y = element_line(color="black", size = 0.5))
egg::ggarrange(hm_01, hm_02, hm_03, hm_04, hm_05, hm_06, hm_07, hm_08, hm_09,
               hm_10, hm_11, hm_12, hm_13, hm_14, hm_15, hm_16, hm_17, hm_18,
               hm_19, hm_20, top = "Heatmaps de la media anual de NDVI en el sur de la CDMX")