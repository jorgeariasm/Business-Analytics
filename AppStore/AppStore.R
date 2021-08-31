## Formato RMarkdown

---
title: 'AppStore'
author: "Jorge Arias"
output:
  html_document:
    df_print: paged
editor_options:
  chunk_output_type: console
---

## 1 - SET UP
```{r setup, include=FALSE}

# Opciones generales
knitr::opts_chunk$set(echo = TRUE)
options(scipen=999)

#Librerías
paquetes <- c('tidyverse','lubridate','plotly')

# Crea un vector logico con si estan instalados o no
instalados <- paquetes %in% installed.packages()

# Si hay al menos uno no instalado los instala
if(sum(instalados == FALSE) > 0) {
  install.packages(paquetes[!instalados])
}

# Carga en memoria los que ya están instalados
lapply(paquetes,require,character.only = TRUE)

# Establece un tema general
theme_set(theme_classic())

```

## 2 - CONTEXTO

Los datos corresponden al histórico de descargas de apps de Google Play.

Trabajamos para una empresa de desarrollo de apps, que quiere analizar esos datos para ver de qué manera puede utilizar los insights obtenidos para incrementar las posibilidades de éxito de sus apps.

## 3 - CARGA Y CALIDAD DE DATOS

Los datos están en el fichero GooglePlayStoreApps.csv dentro de './Datasets/GooglePlayStoreApps/'

Recuperamos todo el proceso de carga de datos que ya habíamos avanzado en la práctica de Stringr.

Así como la parte de tranformación relativa a las variables de texto.

```{r}
df <- 
  #importacion
  read_csv('./GooglePlayStoreApps.csv',
               skip = 1,
               col_names = c('App',
                             'Category',
                             'Rating',
                             'Reviews',
                             'Size',
                             'Installs',
                             'Type',
                             'Price',
                             'Content_Rating',
                             'Genres',
                             'Last_Updated',
                             'Current_Ver',
                             'Android_Ver'),
                 col_types = cols(
                     Category = col_factor(),
                     Installs = col_factor(),
                     Type = col_factor(),
                     Content_Rating = col_factor(),
                     Genres = col_factor(),
                     Current_Ver = col_factor(),
                     Android_Ver = col_factor(),
                     Last_Updated = col_date(format = '%B %d, %Y')
                     )) %>% 
  #eliminamos el registro que daba problemas
  filter(App != 'Life Made WI-Fi Touchscreen Photo Frame') %>% 
  
  #transformacion de Size
  mutate(Unidad = str_sub(Size,start = -1,end = -1),#creamos otra variable con la unidad
         Size = str_sub(Size,start = 1,end = -2),#quitamos la unidad de Size
         Size = str_replace(Size,'Varies with devic', 'NA'),#Cambiamos los nulos
         Size = as.double(Size),#transformamos a numérico
         Size = ifelse(Unidad == 'k', Size / 1024, Size)) %>% #pasamos todo a mg
  select(-Unidad) %>%
  
  #tranformacion de Price
  filter(Price != 'Everyone') %>% 
  mutate(Price = ifelse(Price == 0, 
                        Price,
                        str_sub(Price,start = 2,end = str_length(Price))),
         Price = as.double(Price)) %>% 
  
  #tranformacion de App
  mutate(App = str_to_lower(App),
         Cocina = str_detect(App,'cook'),
         Fotografia = str_detect(App,'photo')) %>% 
  
  #tranformacion de Genres
  mutate(Genero_1 = as.factor(str_split(Genres,';',simplify = T)[,1]),
         Genero_2 = as.factor(str_split(Genres,';',simplify = T)[,2])
         ) %>%
  select(-Genres) %>% 
  
  #tranformacion de Android_Ver
  mutate(Android_version = as.factor(str_split(Android_Ver,'\\.',simplify = T)[,1]),
         Android_subversion = as.factor(str_split(Android_Ver,'\\.',simplify = T)[,2]),
         Android_subversion = as.factor(str_sub(Android_subversion, 1, 1))
         ) %>%
  select(-Android_Ver) 
```

Vamos a continuar desde este punto analizando la calidad de datos del resto de variables.

Retomamos la visión global

```{r}
glimpse(df)
```


Vamos a crear un pipe para analizar a la vez todas las variables que sean factores

```{r}
df %>% 
  select(-Current_Ver) %>% #saco esta variable del análisis porque tiene muchos valores diferentes
  select_if(is.factor) %>% 
  pivot_longer(everything()) %>% 
  group_by(name,value) %>% 
  summarise(Conteo = n()) %>% 
  arrange(name,desc(Conteo)) %>% 
  print(n = Inf)
```

Cosas a corregir:

Sobre nulos:
* Android_subversion tiene valor "": debería ser nulo
* Android_version tiene valor "Varies with device": debería ser nulo
* Android_version tiene valor "NaN": debería ser nulo
* Genero_2 tiene valor "": debería ser nulo
* Type tiene valor "NaN": debería ser nulo

Sobre valores poco frecuentes:
* Android_subversion: juntar valores 5 y 6 en "Mayor 4"
* Android_version: juntar valores 6,7 y 8 en "Mayor 5"
* Installs: revisión en detalle y agrupamiento personalizado

Ahora vamos a analizar a la vez todas las variables que sean numericas

Primero los nulos

```{r}
df %>% 
  select_if(is.numeric) %>% 
  summarise_all(.funs = ~sum(is.na(.)))
```

Rating y Size tienen nulos, vamos a revisar una muestra de los nulos de cada una

```{r}
filter(df, is.na(Rating))

filter(df, is.na(Size))
```

Parece que ambas tienen valores válidos en el resto de las variables.

Cosas a corregir:
* En Rating y Size imputar los nulos con la media del resto de apps

Ahora vamos a analizar los estadísticos básicos:

```{r}
df %>% 
  select_if(is.numeric) %>% 
  drop_na() %>% 
  summary
```

Todo ok excepto Reviews que tiene máximos irreales, vamos a ver el detalle de los valores más altos

```{r}
df %>% 
  select(Reviews) %>% 
  arrange(desc(Reviews)) %>% 
  print(n = 500)
```

No es que sea cosa de datos aislados, nos da poca fiabilidad.

Cosas a corregir:
* Discretizar la variable Reviews en categorías

Analisis de la variable tipo fecha

```{r}
summary(df$Last_Updated)

ggplot(df, aes(Last_Updated)) + geom_bar()
```

Tiene lógica que la mayoría de apps hayan tenido una actualización reciente, así que nada que destacar.

Resumen de las conclusiones sobre calidad de datos y cosas que tenemos que modificar:

Sobre nulos:
* Android_subversion tiene valor "": debería ser nulo
* Android_version tiene valor "Varies with device": debería ser nulo
* Android_version tiene valor "NaN": debería ser nulo
* Genero_2 tiene valor "": debería ser nulo
* Type tiene valor "NaN": debería ser nulo
* En Rating y Size imputar los nulos con la media del resto de apps

Sobre valores poco frecuentes:
* Android_subversion: juntar valores 5 y 6 en "Mayor 4"
* Android_version: juntar valores 6,7 y 8 en "Mayor 5"
* Installs: revisión en detalle y agrupamiento personalizado

Otros:
* En Reviews discretizar la variable en categorías

## 4 - TRANSFORMACION DE DATOS

### 4.1 - Gestión de Nulos

Vamos a empezar gestinando los nulos y los valores poco frecuentes salvo Installs

```{r}
df <- df %>% 
  mutate(Android_subversion = fct_recode(Android_subversion, NULL = ''),
         Android_version = fct_collapse(Android_version, NULL = c('Varies with device','NaN')),
         Genero_2 = fct_recode(Genero_2, NULL = ''),
         Type = fct_recode(Type, NULL = 'NaN'),
         Rating = if_else(is.na(Rating), mean(Rating, na.rm = T), Rating),
         Size = if_else(is.na(Size), mean(Size, na.rm = T), Size)
         ) %>% 
  #gestion de los valores poco frecuentes
  mutate(Android_subversion = fct_collapse(Android_subversion, 'Mayor 4' = c('5','6')),
         Android_version = fct_collapse(Android_version, 'Mayor 5' = c('6','7','8')),
         )
```

Ahora vamos con Installs. Empezamos viendo en detalle sus valores y conteos.

```{r}
count(df,Installs,sort = T) %>% print(n=Inf)
```

Tiene muchos valores, y algunos con pocos conteos, por lo que puede tener sentido agruparlos

### 4.2 - Discretizaciones

Discretizamos Installs.

Vamos a verla gráficamente para ver qué tramos tendría sentido agrupar.

```{r}
ggplot(df,aes(Installs)) + geom_bar()
```

Tenemos el problema de que los niveles no están ordenados, así que tendremos que ordenarlos.

Podríamos ordenarlos manualmente con fct_relevel(), pero son muchos valores así que sería un trabajo arduo.

Vamos a hacer un pequeño truco con fct_reorder() que nos permitía ordenar los niveles de un factor en base a el valor en otra variable, aprovechando que no nos cuestra mucho parsear Installs a número:

1) crearemos un Installs2 temporal que será Installs parseado a número
2) ordenaremos los niveles de Installs según el orden natural de Installs2
3) eliminaremos Installs2 que era temporal

```{r}
df <- df %>% 
  mutate(Installs2 = parse_number(str_replace_all(Installs,',','')),#Creamos un installs2 temporal como número
         Installs = fct_reorder(Installs,Installs2)) %>% #Reordenamos 
  select(-Installs2)
  
#Y volvemos a hacer el gráfico  
  ggplot(df,aes(Installs)) + 
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90))
```

Vamos a agrupar las 21 categorías en 6

```{r}
df <- df %>% 
  mutate(Installs = fct_collapse(Installs,
                                 '00_Menos_1.000' = c('0+','0','1+','5+','10+','50+','100+','500+'),
                                 '01_Entre_1.000_y_10.000' = c('1,000+','5,000+'),
                                 '02_Entre_10.000_y_100.000' = c('10,000+','50,000+'),
                                 '03_Entre_100.000_y_1.000.000' = c('100,000+','500,000+'),
                                 '04_Entre_1.000.000_y_10.000.000' = c('1,000,000+','5,000,000+'),
                                 '05_Mas_10.000.000' = c('10,000,000+','50,000,000+','100,000,000+','500,000,000+','1,000,000,000+')
                                 ))
```

Comprobamos cómo ha quedado

```{r}
ggplot(df,aes(Installs)) + 
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90))
```

Ahora vamos a discretizar también Reviews, que se nos había quedado pendiente.

Como es una variable contínua el proceso es diferente.

Primero vamos a ver su distribución

```{r}
ggplot(df,aes(Reviews)) +
  geom_histogram(bins = 30)

#no vemos nada por culpa de los atipicos y de los ceros, vamos a volver a hacerlo quitando los ceros y el 10% de los valores más altos

df %>% 
  filter(Reviews != 0) %>% 
  arrange(desc(Reviews)) %>% 
  slice(1000:nrow(df)) %>% 
  ggplot(aes(Reviews)) +
  geom_histogram(bins = 30)

#parece que para ver el grueso de la distrubución tendremos que hacer zoom sobre los que tienen 50.000 reseñas o menos

df %>% 
  filter(Reviews != 0) %>% 
  filter(Reviews <= 50000) %>% 
  ggplot(aes(Reviews)) +
  geom_histogram(bins = 30)

```

Vamos a discretizar tal cual aprendimos en el curso, con mutate + case_when

```{r}
df <- df %>% 
  mutate(Reviews = as.factor(case_when(Reviews == 0 ~ '00_Cero',
                             Reviews > 0 & Reviews <= 100 ~ '01_Menos_100',
                             Reviews > 100 & Reviews <= 10000 ~ '02_Entre_100_y_10.000',
                             Reviews > 10000 & Reviews <= 100000 ~ '03_Entre_10.000_y_100.000',
                             Reviews > 100000 & Reviews <= 1000000 ~ '04_Entre_100.000_y_1.000.000',
                             Reviews > 1000000 ~ '05_Mas_1.000.000',
                             TRUE ~ '99_Otros'
                             )))
```

Veamos como queda

```{r}
ggplot(df,aes(Reviews)) + geom_bar() +
    theme(axis.text.x = element_text(angle = 90))
```


## 5 - ANALISIS DE DISCOVERY

Dado que trabajamos para una empresa que quiere usar los datos para maximizar sus posibilidades de publicar una app de pago creo que desde negocio deberíamos responder a las siguientes preguntas:

1) ¿Cuales son las categorías de apps donde puede haber más mercado?
2) ¿Cuales son las categorías que pueden tener menor competencia?
3) ¿Cuales son las categorías en las que los usuarios están más dispuestos a pagar?
4) En base a lo anterior ¿Cuales son las mejores categorías para entrar con una nueva app?
5) ¿En qué rango de precios podríamos vender nuestra app en dichas categorías?

### 5.1 - ¿Cuales son las categorías de apps donde puede haber más mercado?

Para responder a esta pregunta vamos a analizar el número de descargas (Installs) y el número de reseñas (Reviews).

Comenzamos por las descargas. Vamos a analizar la frecuencia total de descargas

```{r}
ggplot(df,aes(Installs)) + geom_bar() + coord_flip()
```

Vamos a quedarnos con los dos valores de más descargas (a partir de 1.000.000) y analizar las categorías más frecuentes en ellos

```{r}
df %>% 
  filter(Installs %in% c('04_Entre_1.000.000_y_10.000.000','05_Mas_10.000.000')) %>% 
  count(Category, sort = T)
```

Con diferencia, las categorías más populares son juegos y familia. Vamos a guardarlas en una variable.

```{r}
mas_descargas <- df %>% 
  filter(Installs %in% c('04_Entre_1.000.000_y_10.000.000','05_Mas_10.000.000')) %>% 
  count(Category, sort = T)
```

Ahora vamos a analizar el número de reseñas

```{r}
ggplot(df,aes(Reviews)) + geom_bar() + coord_flip()
```

Vamos a quedarnos con las que tienen más de 100.000 reseñas y analizar las categorías más frecuentes en ellas

```{r}
df %>% 
  filter(Reviews %in% c('04_Entre_100.000_y_1.000.000','05_Mas_1.000.000')) %>% 
  count(Category, sort = T)
```

De nuevo las categorías más populares son juegos y familia. Vamos a guardarlas en una variable.

```{r}
mas_resenas <- df %>% 
  filter(Reviews %in% c('04_Entre_100.000_y_1.000.000','05_Mas_1.000.000')) %>% 
  count(Category, sort = T)
```

Dado que ya tenemos 2 indicadores vamos a juntarlos en el mismo dataset, crear un ranking para cada uno y después sumarlos para tener un ranking total de popularidad

```{r}
mercado <- inner_join(mas_descargas,mas_resenas, by = 'Category') %>% 
  rename(n_installs = n.x, n_reviews = n.y) %>% 
  mutate(rank_installs = row_number(desc(n_installs)),
         rank_reviews = row_number(desc(n_reviews)),
         rank_total = rank_installs + rank_reviews) %>% 
  arrange(rank_total)
```

### 5.2 - ¿Cuales son las categorías que pueden tener menor competencia?

Podemos medir la competencia de dos formas diferentes:

* Porque haya pocas apps: conteo de apps por categoría
* Porque tengan malas reseñas: media de Rating por categoría

Vamos a analizarlo. Empezamos por el número de apps, las contamos y las guardamos en una varible

```{r}
num_apps <- df %>%
  count(Category,sort = T)
```

Hacemos lo mismo con el Rating medio

```{r}
rating_medio <- df %>% 
  group_by(Category) %>% 
  summarise(rating_medio = mean(Rating,na.rm = T)) %>% 
  arrange(rating_medio) 
```

Y de nuevo juntamos los dos indicadores en el mismo dataset y crearmos los rankings individuales y el total

```{r}
competencia <- inner_join(num_apps,rating_medio, by = 'Category') %>% 
  rename(num_apps = n) %>% 
  mutate(rank_num_apps = row_number(num_apps),
         rank_reviews = row_number(rating_medio),
         rank_total = rank_num_apps + rank_reviews) %>% 
  arrange(rank_total)
```

### 5.3 - ¿Cuales son las categorías en las que los usuarios están más dispuestos a pagar?

Tenemos 2 variables relacionadas con el precio:

* Type: que nos dice si la app es gratis o de pago
* Price: que nos dice el precio

Así que de nuevo podríamos tener 2 indicadores complementarios para responder a esta pregunta, ya que el primero nos dice si en la categoría el usuario está dispuesto a pagar o no, y el segundo nos dice cuánto está dispuesto a pagar.

Vamos a construir primero las categorías en las que hay mayor porcentaje de apps de pago 

```{r}
dispuesto_pagar <- df %>% 
  group_by(Category,Type) %>% 
  summarise(Total_Tipo = n()) %>% 
  group_by(Category) %>% 
  mutate(Total_Categoria = sum(Total_Tipo),
         Porc_Tipo = Total_Tipo / Total_Categoria * 100) %>% 
  ungroup() %>% 
  select(Category,Type,Total_Tipo,Total_Categoria, Porc_Tipo) %>% 
  filter(Type == 'Paid' & Total_Categoria > 50) %>% #un minimo de muestra para calidad
  arrange(desc(Porc_Tipo)) %>% 
  select(Category,Porc_Tipo)
```

Y ahora las que tienen un precio medio medio mayor

```{r}
mayor_precio <- df %>% 
  filter(Type == 'Paid') %>% 
  group_by(Category) %>% 
  summarise(precio_medio = mean(Price, na.rm = T)) %>% 
  arrange(desc(precio_medio))
```

Y de nuevo juntamos los dos indicadores en el mismo dataset y crearmos los rankings individuales y el total

```{r}
disposicion_pago <- inner_join(dispuesto_pagar,mayor_precio, by = 'Category') %>% 
  mutate(rank_porc_tipo = row_number(desc(Porc_Tipo)),
         rank_mayor_precio = row_number(desc(precio_medio)),
         rank_total = rank_porc_tipo + rank_mayor_precio) %>% 
  arrange(rank_total)
```

Vamos a ver la matriz estratégica de dispocion al pago

```{r}
ggplotly(
  ggplot(disposicion_pago, aes(x= Porc_Tipo, y = precio_medio, text = Category)) +
  geom_point()
)
```

### 5.4 - En base a lo anterior ¿Cuales son las mejores categorías para entrar con una nueva app?

Primero nos quedamos únicamente con los rankings totales de cada dimensión y reseteamos los rankings totales
```{r}
mercado <- mercado %>% 
  mutate(rank_total_mercado = row_number(rank_total)) %>% 
    select(Category,rank_total_mercado)

competencia <- competencia %>% 
  mutate(rank_total_competencia = row_number(rank_total)) %>% 
    select(Category,rank_total_competencia)

disposicion_pago <- disposicion_pago %>% 
  mutate(rank_total_disposicion_pago = row_number(rank_total)) %>% 
    select(Category,rank_total_disposicion_pago)
```

Los unimos, creamos un ranking final y nos quedamos únicamente con las 5 mejores categorías

```{r}
final <- inner_join(inner_join(mercado, competencia),disposicion_pago) %>% 
  mutate(ranking_final = rank_total_mercado + rank_total_competencia + rank_total_disposicion_pago,
         ranking_final = row_number(ranking_final)) %>% 
  select(Category, ranking_final) %>% 
  arrange(ranking_final) %>% 
  slice(1:5)
```

### 5.4 - ¿En qué rango de precios podríamos vender nuestra app en dichas categorías?

Necesitamos volver a df para calcular el rango de precios por categoría.

Usaremos los cuartiles 1 (25%) y 3 (75%) para identificar los precios mínimos y máximos

```{r}
rango_precios <- df %>% 
  filter(!is.na(Price) & Type == 'Paid') %>% 
  group_by(Category) %>% 
  summarise(precio_mediano = median(Price),
            precio_inf = quantile(Price)[2],
            precio_sup = quantile(Price)[4])
```

Incorporamos la nueva info a nuestra selección final

```{r}
final <- left_join(final,rango_precios)
```

## 6 - CONCLUSION

Tras finalizar nuestro análisis ya somos capaces de darle a nuestro cliente no sólo las mejores categorías para desarrollar una nueva app de pago en base a los datos, si no también el rango de precios ideal al que debería comercializarla