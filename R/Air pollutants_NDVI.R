##LIBRERÍAS 
library(dplyr)
library(egg)
library(ggplot2)
library(gridExtra)
library(imputeTS)
library(np)
library(MODISTools)
library(moments)
library(naniar)
library(raster)
library(readxl)
library(reshape2)

##Versión de R: Version 4.1.0.

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
NDVI_r = mt_to_raster(df = NDVI_data_Terra, reproject = FALSE)
PIXQ_r = mt_to_raster(df = PIXQ_data_Terra, reproject = FALSE)

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

##Para los datos de contaminantes, radiación y temperatura, se descargan los
##datasets directo de las páginas de datos abiertos, yendo desde 2001 hasta
##2020.
url.poll = 
    "http://datosabiertos.aire.cdmx.gob.mx:8080/opendata/excel/RAMA/00RAMA.zip"
url.rad = 
    "http://datosabiertos.aire.cdmx.gob.mx:8080/opendata/excel/RADIACION/00RADIACION.zip"
url.temp = 
    "http://datosabiertos.aire.cdmx.gob.mx:8080/opendata/excel/REDMET/00REDMET.zip"
substrpoll.ini = substring(url.poll, first = 1, last = 63)
substrpoll.fin = substring(url.poll, first = 66)
substrrad.ini = substring(url.rad, first = 1, last = 68)
substrrad.fin = substring(url.rad, first = 71)
substrtemp.ini = substring(url.temp, first = 1, last = 65)
substrtemp.fin = substring(url.temp, first = 68)
years = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10",
          "11", "12", "13", "14", "15", "16", "17", "18", "19", "20")
dest = 
    "C:/Users/Tabby/Documents/R/Air pollutants NOx O3 of the south of Mexico City and their relation with the evolution of NDVI/Data/"
for(i in 1:length(years)) {
    URLPoll = paste0(
        substrpoll.ini, years[i], substrpoll.fin)
    dest.poll = paste0(dest, years[i], substrpoll.fin)
    download.file(URLPoll, destfile = dest.poll)
    URLRad = paste0(
        substrrad.ini, years[i], substrrad.fin)
    dest.rad = paste0(dest, years[i], substrrad.fin)
    download.file(URLRad, destfile = dest.rad)
    URLTemp = paste0(
        substrtemp.ini, years[i], substrtemp.fin)
    dest.temp = paste0(dest, years[i], substrtemp.fin)
    download.file(URLTemp, destfile = dest.temp)
}

##Se extraen todos los archivos de las carpetas zip (formato .xls).
original.wd = getwd()
setwd(dest)
zipfile = list.files(
    path = dest,
    pattern = "*.zip",
    full.names = F)
sapply(zipfile, unzip)

##Cargar cada uno de los archivos por año para todos los contaminantes, 
##radiación y temperatura, para usarlos como variables en data frames. De
##igual manera, se renombra cada uno de los data frames. 
poll_year = list.files(
    path = dest,
    pattern = 
        "CO.xls|NO2.xls|NOX.xls|O3.xls|PM10.xls|PM25.xls|PMCO.xls|SO2.xls|NO.xls",
    full.names = F)
rad_year = unlist(list.files(
    path = dest,
    pattern = "UVB.xls",
    full.names = F))
temp_year = list.files(
    path = dest,
    pattern = "TMP.xls",
    full.names = F)
poll_data = sapply(poll_year, read_xls)
rad_data = sapply(rad_year, read_xls)
temp_data = sapply(temp_year, read_xls)
df_poll = sapply(poll_data, as.data.frame)
df_rad = sapply(rad_data, as.data.frame)
df_temp = sapply(temp_data, as.data.frame)
list_namespoll = names(df_poll)
list_namesrad = names(df_rad)
list_namestemp = names(df_temp)
corr_namespoll = character()
corr_namesrad = character()
corr_namestemp = character()
for (i in 1:length(list_namespoll)) {
    xls_name = gsub("[.xls]", "", list_namespoll[i])
    corr_namespoll[i] = xls_name
}
for (v in 1:length(list_namesrad)) {
    xls_name = gsub("[radiacion_]|[.xls]", "", list_namesrad[v])
    corr_namesrad[v] = xls_name
}
for (w in 1:length(list_namestemp)) {
    xls_name = gsub("[.xls]", "", list_namestemp[w])
    corr_namestemp[w] = xls_name
}
names(df_poll) = corr_namespoll
names(df_rad) = corr_namesrad
names(df_temp) = corr_namestemp
setwd(original.wd)

##Las columnas que representan la hora y las estaciones de muestreo  que no
##pertenecen a los datos de NDVI obtenidos son eliminados, teniendo así subsets
##que enmbarcan las estaciones competentes (es decir, del sur). Para esto, 
##se realizó un loop en el cual se creó, para cada data frame, un subset con
##sólo las estaciones del sur.
stations = c("PLA", "CES", "TAX", ##Estos 3 fueron sustituidos en el 2011 por
                                    ##parte del rediseño del SIMAT.
       "SFE", "AJU", "UAX", ##Estos 3 fueron los sustitutos integrados en el
                            ##rediseño del SIMAT.
       "PED", "COY", "CUA", "TPN", "TAH", "UIZ", "CHO")
south_dat_poll = list()
for (i in 1:length(df_poll)) {
    df = df_poll[[i]]
    Fecha = df[, "FECHA"]
    new_df = data.frame(Fecha)
    stat_name = names(df)
    num = which(
        !is.na(
            match(stat_name, stations)))
    correct_stat = stat_name[num]
    for (x in 1:length(correct_stat)) {
        stat = correct_stat[x]
        stat_var = df[, stat]
        new_df[x + 1] = stat_var
        var = names(new_df[2:length(new_df)])[x]
        colnames(new_df)[colnames(new_df) %in% var] = correct_stat[x]
    }
    south_dat_poll[[i]] = new_df
}
south_dat_rad = list()
for (i in 1:length(df_rad)) {
    df = df_rad[[i]]
    colnames(df)[1] = "Fecha"
    df_rad[[i]] = df
}
for (i in 1:length(df_rad)) {
    df = df_rad[[i]]
    Fecha = df[, "Fecha"]
    new_df = data.frame(Fecha)
    stat_name = names(df)
    num = which(
        !is.na(
            match(stat_name, stations)))
    correct_stat = stat_name[num]
    for (x in 1:length(correct_stat)) {
        stat = correct_stat[x]
        stat_var = df[, stat]
        new_df[1 + 1] = stat_var
        var = names(new_df[2:length(new_df)])[x]
        colnames(new_df)[colnames(new_df) %in% var] = correct_stat[x]
    }
    south_dat_rad[[i]] = new_df
}
south_dat_rad[[8]] = df_rad[[8]] ##Las estaciones de muestreo sur para radiación
                                ##presentan valores nulos en su totalidad, así
                                ##que se usa el resto de datos de estaciones para
                                ##tener datos de radiación para ese año.
south_dat_rad[[10]] = df_rad[[10]]
south_dat_temp = list()
for (i in 1:length(df_temp)) {
    df = df_temp[[i]]
    Fecha = df[, "FECHA"]
    new_df = data.frame(Fecha)
    stat_name = names(df)
    num = which(
        !is.na(
            match(stat_name, stations)))
    correct_stat = stat_name[num]
    for (x in 1:length(correct_stat)) {
        stat = correct_stat[x]
        stat_var = df[, stat]
        new_df[x + 1] = stat_var
        var = names(new_df[2:length(new_df)])[x]
        colnames(new_df)[colnames(new_df) %in% var] = correct_stat[x]
    }
    south_dat_temp[[i]] = new_df
}
names(south_dat_poll) = corr_namespoll
names(south_dat_rad) = corr_namesrad
names(south_dat_temp) = corr_namestemp

##Debido a que existen valores nulos (representados como "-99"), se crea un
##loop en el cual se estima el porcentaje de estos en relación a todas las
##observaciones, esto con el fin de determinar el tratamiento de dichos valores.
total_poll = numeric()
null_ttl_poll = numeric()
for (i in 1:length(south_dat_poll)) {
    x = south_dat_poll[[i]]
    x = x[, -1]
    total_obs = ncol(x)*nrow(x)
    total_poll[i] = total_obs
    null_val = length(which(x == -99))
    null_ttl_poll[i] = null_val
    hun_per = sum(total_poll)
    null_per = sum(null_ttl_poll)
    tot_null_poll = (null_per*100)/hun_per
}
total_rad = numeric()
null_ttl_rad = numeric()
for (i in 1:length(south_dat_rad)) {
    x = south_dat_rad[[i]]
    x = x[, -1]
    if (class(x) == "numeric") {
        total_obs = length(x)
    } else {
        total_obs = ncol(x)*nrow(x)
    }
    total_rad[i] = total_obs
    null_val = length(which(x == -99))
    null_ttl_rad[i] = null_val
    hun_per = sum(total_rad)
    null_per = sum(null_ttl_rad)
    tot_null_rad = (null_per*100)/hun_per
}
total_temp = numeric()
null_ttl_temp = numeric()
for (i in 1:length(south_dat_temp)) {
    x = south_dat_temp[[i]]
    x = x[, -1]
    if (class(x) == "numeric") {
        total_obs = length(x)
    } else {
        total_obs = ncol(x)*nrow(x)
    }
    total_temp[i] = total_obs
    null_val = length(which(x == -99))
    null_ttl_temp[i] = null_val
    hun_per = sum(total_temp)
    null_per = sum(null_ttl_temp)
    tot_null_temp = (null_per*100)/hun_per
}
table.null = matrix(c(tot_null_poll, tot_null_rad, tot_null_temp), ncol = 1)
colnames(table.null) = "NA's (%)"
rownames(table.null) = c("Contaminantes", "Radiación", "Temperatura")
NAs.table = as.table(table.null)
NAs.table

##Se cambian todos los valores nulos -99 a NA para poder manejar los 
##datos de mejor manera.
NA_poll = list()
for (i in 1:length(south_dat_poll)) {
    z = south_dat_poll[[i]]
    u = replace_with_na_all(z, ~. == -99)
    NA_poll[[i]] = u
}
NA_rad = list()
for (i in 1:length(south_dat_rad)) {
    z = south_dat_rad[[i]]
    u = replace_with_na_all(z, ~. == -99)
    NA_rad[[i]] = u
}
NA_temp = list()
for (i in 1:length(south_dat_temp)) {
    z = south_dat_temp[[i]]
    u = replace_with_na_all(z, ~. == -99)
    NA_temp[[i]] = u
}

##Existen columnas que tienen un 100% de valores nulos, por lo que se 
##eliminan.
for (i in 1:length(NA_poll)) {
    df = NA_poll[[i]]
    df[, colSums(is.na(df)) != nrow(df)]
    NA_poll[[i]] = df
}
for (i in 1:length(NA_rad)) {
    df = NA_rad[[i]]
    df[, colSums(is.na(df)) != nrow(df)]
    NA_rad[[i]] = df
}
df = NA_rad[[20]]
df[c(8041:8784),] = rep(NA, 744)
Fecha = NA_poll$`2020SO2`[,1]
df[,1] = Fecha
NA_rad[[20]] = df
for (i in 1:length(NA_temp)) {
    df = NA_temp[[i]]
    df[, colSums(is.na(df)) != nrow(df)]
    NA_temp[[i]] = df
}
names(NA_poll) = corr_namespoll
names(NA_rad) = corr_namesrad
names(NA_temp) = corr_namestemp

##Se aplica una prueba de MCAR de Little para determinar la distribución 
##aleatoria de los NA's - H0 = Los datos son MCAR; Ha = Los datos no son
##MCAR.
MCARtest.poll = numeric()
for (i in 1:length(NA_poll)) {
    tryCatch({
    test = mcar_test(NA_poll[[i]])
    MCARtest.poll[i] = test$p.value
    }, error = function(e){
        cat("ERROR :", conditionMessage(e), "\n")})
}
MCARtest.rad = numeric()
for (i in 1:length(NA_rad)) {
    tryCatch({
        test = mcar_test(NA_rad[[i]])
        MCARtest.rad[i] = test$p.value
    }, error = function(e){
        cat("ERROR :", conditionMessage(e), "\n")})
}
MCARtest.temp = numeric()
for (i in 1:length(NA_temp)) {
    tryCatch({
        test = mcar_test(NA_temp[[i]])
        MCARtest.temp[i] = test$p.value
    }, error = function(e){
        cat("ERROR :", conditionMessage(e), "\n")})
}
MCARtest.poll 
MCARtest.rad 
MCARtest.temp ##Diversos df sueltan error: esto puede ser debido a la alta 
            ##correlación entre las variables.

##Los valores nulos entre los diferentes datos representan entre 20, 30 y 18%  
##del total de observaciones, para contaminantes, radiación y temperatura 
##respectivamente, además de que la mayoría de los df rechazan la H0 de la 
##prueba de Little, por lo que no pueden ser eliminados. Se realiza una 
##imputación de los valores nulos, utilizando el algoritmo de imputación por 
##media móvil.
impute_poll = sapply(NA_poll, na_ma)
impute_rad = sapply(NA_rad, na_ma)
impute_temp = sapply(NA_temp, na_ma)
names(impute_poll) = corr_namespoll
names(impute_rad) = corr_namesrad
names(impute_temp) = corr_namestemp

##Aún con todo el tratamiento, algunas columnas persisten con valores NA.
##Se crea una función que es aplicada a todos los df para eliminar estas
##columnas. 
na_rm = function(df) {
    df = df[, colSums(is.na(df)) == 0]
}
impu_poll = sapply(impute_poll, na_rm)
impu_rad = sapply(impute_rad, na_rm)
impu_temp = sapply(impute_temp, na_rm)
names(impu_poll) = corr_namespoll
names(impu_rad) = corr_namesrad
names(impu_temp) = corr_namestemp

##Se calcula la media por día entre todos los puntos de muestreo, para 
##cada variable por año.
day_poll = list()
for (i in 1:length(impu_poll)) {
    df = impu_poll[[i]]
    Fecha = df$Fecha
    DayMean = rowMeans(df[, -1])
    comb_data = data.frame(Fecha, DayMean)
    day_mean = aggregate(. ~ Fecha, comb_data, mean)
    poll_name = corr_namespoll[i]
    pollutant = substring(poll_name, 5)
    names(day_mean) = c("Fecha", pollutant)
    day_poll[[i]] = day_mean
}
day_rad = list()
for (i in 1:length(impu_rad)) {
    df = impu_rad[[i]]
    Fecha = df$Fecha
    DayMean = rowMeans(df[, -1])
    comb_data = data.frame(Fecha, DayMean)
    day_mean = aggregate(. ~ Fecha, comb_data, mean)
    names(day_mean) = c("Fecha", "UVB")
    day_rad[[i]] = day_mean
}
day_temp = list()
for (i in 1:length(impu_temp)) {
    df = impu_temp[[i]]
    Fecha = df$Fecha
    DayMean = rowMeans(df[, -1])
    comb_data = data.frame(Fecha, DayMean)
    day_mean = aggregate(. ~ Fecha, comb_data, mean)
    names(day_mean) = c("Fecha", "Temperatura")
    day_temp[[i]] = day_mean
}
names(day_poll) = corr_namespoll
names(day_rad) = corr_namesrad
names(day_temp) = corr_namestemp

##Se combinan todos los df de cada variable, por año.
sep_poll = sapply(corr_namespoll, substring, first = 5)
complete_dat = list()
for (i in 1:length(day_poll)) {
    name = corr_namespoll[i]
    poll_name = substring(name, 5)
    pos = which(sep_poll %in% poll_name)
    compl_list = list()
    for (v in pos) {
        df = day_poll[[v]]
        compl_list[[v]] = df
    }
    complete_poll = bind_rows(compl_list)
    complete_dat[[i]] = complete_poll
}
names(complete_dat)
complete_dat = complete_dat[c(1:6, 11)]
pollut_names = substring(corr_namespoll[c(1:6, 11)], 5)
UVB = bind_rows(day_rad)
TMP = bind_rows(day_temp)
complete_dat[[8]] = UVB
complete_dat[[9]] = TMP
names(complete_dat) = c(pollut_names, "UVB", "TMP")

##Se crea un subset con las medias de cada contaminante por cada fecha 
##indicada en los registros para el índice de vegetación de MODIS. Se 
##filtra cada df de contaminante usando las fechas - ya especificadas -  
##de los datos MODIS.
filt_list = list()
for (i in 1:length(complete_dat)) {
    df = complete_dat[[i]]
    Fecha = as.Date(df$Fecha)
    df$Fecha = Fecha
    filt_df = filter(df, Fecha %in% dates)
    filt_list[[i]] = filt_df
}
names(filt_list) = names(complete_dat)

##Se combinan todas las variables en un mismo df (sólo aquellos que sin 
##tengan registro desde el 2001 - NO, PM25 y PMCO cuentan con registros
##posteriores).
data_exp = data.frame(
    NDVI = NDVI_df[,2], CO = filt_list$CO[,2], 
    NO2 = filt_list$NO2[,2], NOx = filt_list$NOX[,2], 
    O3 = filt_list$O3[,2], SO2 = filt_list$SO2[,2],
    PM10 = filt_list$PM10[,2], UVB = filt_list$UVB[,2],
    TMP = filt_list$TMP[,2])

##ANÁLISIS ESTADÍSTICOS

##Se realiza una prueba de D'Agostino para la normalidad de
##los datos, primero para los contaminantes principales, donde:
##(con un alfa de 0.05)
##H0 = Los datos tienen una distribución normal.
##Ha = Los datos no tienen una distribución normal.
##Y, si la oblicuidad es 0 = distribución normal.
##Si es menor a 1 = distribución medio normal.
##Mayor a 1 = distribución exponencial.
t1 = agostino.test(data_exp$CO) ##CO
t2 = agostino.test(data_exp$NO2) ##NO2
t3 = agostino.test(data_exp$NOx) ##NOx
t4 = agostino.test(data_exp$O3) ##O3
t5 = agostino.test(data_exp$SO2) ##SO2
t6 = agostino.test(data_exp$PM10) ##PM10
t7 = agostino.test(data_exp$UVB) ##UVB
t8 = agostino.test(data_exp$TMP) ##TMP 
test_list = list(
    t1, t2, t3, t4, t5, t6, t7, t8)
test_names = c(
    "CO", "NO2", "NOx", "O3", "SO2", "PM10", "UVB", "TMP")
p_value = numeric()
skew_val = numeric()
for (i in 1:length(test_list)) {
    test = test_list[[i]]
    p.val = test$p.value
    p_value[i] = p.val
    skew = test$statistic
    skew_val[[i]] = skew[1]
}
names(p_value) = test_names
names(skew_val) = test_names
names(p_value[p_value < 0.05])  ##Aquí aparecen los contaminantes que tienen
                                ##un p-value menor al alfa, por lo que son 
                                ##considerados como datos no normales.
names(skew_val[skew_val == 0])  ##No hay ningún contaminante que tenga una
                                ##oblicuidad igual a 0.
names(skew_val[skew_val < 1])   ##Existen 5 contaminantes que tienen una
                                ##oblicuidad menor a 1:
                                ##NO2, NOx, O3, PM10 y TMP
                                ##por lo que son considerados como datos con
                                ##distribución medio normal. 

##Debido a que todos los datos presentan una distribución no normal, se realiza
##un análisis de regresión lineal multivariada no paramétrica, con prueba de
##significancia de la pendiente - esta prueba sólo se realiza con los 
##contaminantes que presentan la misma longitud y unidad; el resto será 
##probado con una regresión no paramétrica simple -.
##Se tienen dos hipótesis:
##Ho = Las medias de las variables son iguales, por lo que no hay relación 
##significativa entre los contaminantes y el VI.
##Ha = Las medias de las variables son diferentes, por lo que sí hay 
##una relación.
##La prueba se realiza para cada contaminante (ahora separados por unidad
##y por la longitud de los datos).
prueb.est = npregbw(
    formula = NDVI ~ CO + NO2 + NOx + O3 + SO2 + PM10 + UVB + TMP,
    data = data_exp,
    x = TRUE, y = TRUE)
model.np = npreg(bws = prueb.est)
summary(model.np) ##El R^2 es 0.4104033 (41.04%).
npsigtest(model.np) ##Las variables que presentan una correlación significativa
                    ##con NDVI son NO2, O3 y PM10.

##PLOTS

##Las relaciones lineales para cada variable (con relación significativa)
## con NDVI son representadas en gráficas de dispersión, con una línea de 
##regresión. 

indices = c(0.1, 0.2, 0.3)
q.NO2 = ggplot(
    data_exp, aes(x = NO2, y = NDVI)) + 
    ylab("") + 
    xlab("NO2 (ppb)") +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(
        vjust = 0.5, hjust=1, size = 10, color = "black"),
        axis.text.y = element_text(vjust = 0.5, hjust = 1, size = 10, 
                                   color = "black"),
        plot.margin = margin(0, 0, 0, 0, "cm"),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) + 
    geom_hline(
        mapping = NULL, yintercept = indices, colour = "grey80") +
    geom_point(size = 1, color = "gray32") +
    geom_smooth(
        method = "lm", color = "dodgerblue1", size = 1.5, formula = y ~ x) +
    geom_label(
        x = 59, y = 0.33,
        label = "P-Value = 0.0025")
q.O3 = ggplot(
    data_exp, aes(x = O3, y = NDVI)) + 
    ylab("") + 
    xlab("O3 (ppb)")+
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(
        vjust = 0.5, hjust=1, size = 10, color = "black"),
        axis.text.y = element_text(vjust = 0.5, hjust = 1, size = 10, 
                                   color = "black"),
        plot.margin = margin(0, 0, 0, 0, "cm"),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) + 
    geom_hline(
        mapping = NULL, yintercept = indices, colour = "grey80") +
    geom_point(size = 1, color = "gray32") +
    geom_smooth(
        method = "lm", color = "darkorange1", size = 1.5, formula = y ~ x) +
    geom_label(
        x = 65.9, y = 0.33,
        label = "P-Value = 0.0275")
q.PM10 = ggplot(
    data_exp, aes(x = NO2, y = NDVI)) + 
    ylab("") + 
    xlab("PM10 (µg/m^3)")+
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(
        vjust = 0.5, hjust=1, size = 10, color = "black"),
        axis.text.y = element_text(vjust = 0.5, hjust = 1, size = 10, 
                                   color = "black"),
        plot.margin = margin(0, 0, 0, 0, "cm"),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) + 
    geom_hline(
        mapping = NULL, yintercept = indices, colour = "grey80") +
    geom_point(size = 1, color = "gray32") +
    geom_smooth(
        method = "lm", color = "brown1", size = 1.5, formula = y ~ x) +
    geom_label(
        x = 59, y = 0.33,
        label = "P-Value = 0.0125")
egg::ggarrange(
    q.NO2, q.O3, q.PM10, left = "NDVI")
    
##Se realiza los time series de todos los contaminantes con correlación, junto
##al time series de NDVI.
dates = cont_VI[1:460, 1]
date.polls = data.frame(
    dates, NDVI = data_exp$NDVI,
    NO2 = data_exp$NO2, O3 = data_exp$O3,
    PM10 = data_exp$PM10)
indices.cont = c(0, 20, 40, 60)
ts.NO2 = ggplot(
    date.polls, aes(x = dates, y = NO2)) + 
    ylab("NO2 (ppb)") + 
    xlab("")+
    theme(
        axis.text.x = element_text(
            angle = 0, vjust = 1, hjust= 0.5, size = 10, color = "black"),
        axis.text.y = element_text(
            vjust = 0.5, hjust = 1, size = 10, color = "black"),
        plot.margin = margin(0, 0.1, 0, 0.1, "cm"),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))  + 
    scale_x_date(
        date_breaks = "2 years", date_labels = "%Y") +
    geom_hline(
        mapping = NULL, yintercept = indices.cont, colour = "grey80") +
    geom_line(color = "grey28", size = 0.5) + 
    stat_smooth(method = "loess", color = "dodgerblue3", fill = "dodgerblue1") 
ts.O3 = ggplot(
    date.polls, aes(x = dates, y = O3)) + 
    ylab("O3 (ppb)") + 
    xlab("")+
    theme(
        axis.text.x = element_text(
            angle = 0, vjust = 1, hjust= 0.5, size = 10, color = "black"),
        axis.text.y = element_text(
            vjust = 0.5, hjust = 1, size = 10, color = "black"),
        plot.margin = margin(0, 0.1, 0, 0.1, "cm"),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))  + 
    scale_x_date(
        date_breaks = "2 years", date_labels = "%Y") +
    geom_hline(
        mapping = NULL, yintercept = indices.cont, colour = "grey80") +
    geom_line(color = "grey28", size = 0.5) +
    stat_smooth(method = "loess", color = "darkorange3", fill = "darkorange1") 
indices..cont = c(0, 30, 60, 90)
ts.PM10 = ggplot(
    date.polls, aes(x = dates, y = PM10)) + 
    ylab("PM10 (µg/m^3)") + 
    xlab("")+
    theme(
        axis.text.x = element_text(
            angle = 0, vjust = 1, hjust= 0.5, size = 10, color = "black"),
        axis.text.y = element_text(
            vjust = 0.5, hjust = 1, size = 10, color = "black"),
        plot.margin = margin(0, 0.1, 0, 0.1, "cm"),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))  + 
    scale_x_date(
        date_breaks = "2 years", date_labels = "%Y") +
    geom_hline(
        mapping = NULL, yintercept = indices..cont, colour = "grey80") +
    geom_line(color = "grey28", size = 0.5) +
    stat_smooth(method = "loess", color = "brown3", fill = "brown1") 
ts.NDVI = ggplot(
    date.polls, aes(x = dates, y = NDVI)) + 
    ylab("NDVI") + 
    xlab("")+
    theme(
        axis.text.x = element_text(
            angle = 0, vjust = 1, hjust= 0.5, size = 10, color = "black"),
        axis.text.y = element_text(
            vjust = 0.5, hjust = 1, size = 10, color = "black"),
        plot.margin = margin(0, 0.1, 0, 0.1, "cm"),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))  + 
    scale_x_date(
        date_breaks = "2 years", date_labels = "%Y") +
    geom_hline(
        mapping = NULL, yintercept = indicescont, colour = "grey80") +
    geom_line(color = "grey28", size = 0.5) +
    stat_smooth(method = "loess", color = "purple4", fill = "purple1") 
egg::ggarrange(ts.NO2, ts.O3, ts.PM10, ts.NDVI, ncol = 1)
