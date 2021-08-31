

---
title: 'Proyecto discovery en hotel'
author: "Jorge Arias"
output: html_notebook
editor_options:
  chunk_output_type: console
---

## 1 - SET UP
```{r setup, include=FALSE}
# Opciones generales
knitr::opts_chunk$set(echo = TRUE)

#Librerías
paquetes <- c('tidyverse','lubridate','cowplot')

# Crea un vector logico con si estan instalados o no
instalados <- paquetes %in% installed.packages()

# Si hay al menos uno no instalado los instala
if(sum(instalados == FALSE) > 0) {
  install.packages(paquetes[!instalados])
}

# Carga en memoria los que ya están instalados
lapply(paquetes,require,character.only = TRUE)

# Establece un tema general
theme_set(theme_cowplot())
```

## 2 - CARGA DE DATOS

Los datos corresponden a dos hoteles, uno resort y otro de ciudad. En total son casi 120.000 reservas y más de 30 variables en cada una de ellas.

Están en el directorio Datasets/Hoteles.

```{r}
df <- read_csv('./Caso3/hoteles.csv')
```

## 3 - CONTEXTO

Trabajamos para una cadena hotelera que acaba de comprar 2 hoteles en Portugal, un resort en el Algarve y otro de ciudad en Lisboa.

La cadena es un grupo privado con varias propiedades en US dirigidas a un público empresarial. Pero no tiene experiencia ni en el mercado europeo ni en esta tipología de hoteles.

La dirección nos ha encargado estudiar el histórico de datos para generar insights que les permitan entender el comportamiento del consumidor y ver si hay diferencias o no entre ambos hoteles.

Por tanto, los objetivos del proyecto son 3:

1) Entender el comportamiento de los clientes de estos hoteles
2) Ver si hay diferencias entre los patrones de ambos tipos de hoteles
3) Localizar insights de interés que puedan ayudar a la dirección a hacer crecer el negocio

## 4 - CALIDAD DE DATOS y EDA

### 4.1 - Tipos de variables

```{r}
glimpse(df)

#Detectamos estas variables a cambiar de tipo

a_factor <- c('hotel','arrival_date_year','arrival_date_month','arrival_date_week_number',
              'arrival_date_day_of_month','meal','country','market_segment','distribution_channel',
              'reserved_room_type','assigned_room_type','deposit_type','agent','company',
              'customer_type','reservation_status')

a_logico <- c('is_canceled','is_repeated_guest')

a_discreto <- c('stays_in_weekend_nights','stays_in_week_nights','adults','children','babies',
                'previous_cancellations','previous_bookings_not_canceled','booking_changes','days_in_waiting_list',
                'required_car_parking_spaces','total_of_special_requests')

#Realizamos el cambio

df <- df %>% 
  mutate_at(a_factor,as.factor) %>% 
  mutate_at(a_logico,as.logical) %>% 
  mutate_at(a_discreto,as.integer)

```

### 4.2 - Nulos

```{r}
df %>% summarize_all(~sum(is.na(.))) %>% t()
```

No hay nulos, salvo 4 en children, pero en el glimpse habíamos visto la palabra 'NULL', así que vamos a buscarla

```{r}
contar_nulls <- function(variable) {
  sum(if_else(variable == 'NULL', 1, 0))
}

df %>% 
  mutate_all(as.character) %>% 
  lapply(.,contar_nulls)

```

Conclusiones:
- Eliminar company
- Imputar country, agent

### 4.3 - Valores y atípicos variables categóricas

```{r}
#Como son 16 variables vamos a dividirlas en 2 tramos de 8

#Tramo 1
df %>% 
  select_if(is.factor) %>%
  select(1:8) %>% 
  mutate(id = 1:nrow(.)) %>% 
  pivot_longer(-id, names_to = 'variable', values_to = 'valor') %>% 
  ggplot(aes(valor)) +
  geom_bar() +
  facet_wrap(~variable, scales = 'free', ncol = 2)

#Tramo 2
df %>% 
  select_if(is.factor) %>%
  select(9:16) %>% 
  mutate(id = 1:nrow(.)) %>% 
  pivot_longer(-id, names_to = 'variable', values_to = 'valor') %>% 
  ggplot(aes(valor)) +
  geom_bar() +
  facet_wrap(~variable, scales = 'free', ncol = 2)
```

Conclusiones:
- Revisar country, agent con una tabla
- En market_segment unir Undefined, Aviation, Complementary y Corporate
- En meal crear indicador entre BB y OTROS
- En assigned_room_type y reserved_room_type juntar todo excepto A y D
- En customer_type dejar en transient y OTROS
- En distribution_channel juntar Corporate, GDS y Undefined
- En reservation_status juntar No-Show con Canceled
- En desposit_type juntar Non Refund con Refundable y llamarlo Deposit

Vamos a revisar las variables del punto 1 con una tabla

```{r}
count(df,country,sort=T) %>% print(n=Inf)

count(df,agent,sort=T) %>% print(n=Inf)
```

Conclusiones:
- En country dejar los 15 primeros mercados y juntar el resto en OTROS
- Eliminar la variable agente por no ser muy útil en este contexto


### 4.4 - Valores y atípicos variables discretas

```{r}
df %>% 
  select_if(is.integer) %>% 
  mutate(id = 1:nrow(.)) %>% 
  pivot_longer(-id, names_to = 'variable', values_to = 'valor') %>% 
  ggplot(aes(valor)) +
  geom_bar() +
  facet_wrap(vars(variable), scales = 'free',ncol = 2)
```

Conclusiones:
- Pasar a indicadores: babies, booking_changes, children, previous_cancelations, required_car_parking_spaces, total_special_request, days_in_waiting_list, previous_bookings_not_canceled
- Convertir adultos a 1,2 mas de 2
- En stays_in_week_nights juntar las de más de 4
- En stays_in_weekend_nights juntar en 1 y mas de 1

### 4.5 - Valores y atípicos variables contínuas

```{r}
df %>% 
  select_if(is.double) %>% 
  summary
```

Vamos a revisar el maximo de adr

```{r}
arrange(df,desc(adr)) %>% select(adr) %>% slice(1:20)
```

Y lo mismo con lead_time

```{r}
arrange(df,desc(lead_time)) %>% select(lead_time) %>% slice(1:50) %>% as.data.frame()
```

Conclusiones:
- Eliminar el atipico de 5400 en adr


## 5 - TRANSFORMACIÓN DE DATOS

### 5.1 - Correccion de nulos

Vamos a imputar por el valor más frecuente, así que lo calculamos para cada variable implicada

```{r}
count(df,children, sort = T) %>% slice(1)

count(df,country, sort = T) %>% slice(1)
```


```{r}
df <- df %>% 
  select(-company) %>% #eliminamos la variable por tener muchos nulos
  mutate(children = ifelse(is.na(children), 0, children),
         country = fct_recode(country, 'PRT' = 'NULL')
  )
  
```

### 5.2 - Correccion de valores y atípicos

Recopilamos todo lo que habíamos detectado y las ordenamos por acciones:

Variables o registros a eliminar:
- Eliminar agent por no ser muy útil en este contexto
- Eliminar el atipico de 5400 en adr

Recodificar:
- En country dejar los 15 primeros mercados y juntar el resto en OTROS
- En market_segment unir Undefined, Aviation, Complementary en Corporate
- En assigned_room_type y reserved_room_type juntar todo excepto A y D
- En customer_type dejar en Transient y OTROS
- En distribution_channel juntar Corporate, GDS y Undefined en Corporate
- En reservation_status juntar No-Show con Canceled
- En meal recodificar entre BB y OTROS
- En desposit_type juntar Non Refund con Refundable y llamarlo Deposit

Discretizar:
- Convertir adults a 1,2 mas de 2
- En stays_in_week_nights juntar en 1,2,3,4 y mas de 4
- En stays_in_weekend_nights juntar en 1 y mas de 1

Crear indicadores:
- Pasar a indicadores: babies, booking_changes, children, previous_cancelations, required_car_parking_spaces, total_special_request, days_in_waiting_list, previous_bookings_not_canceled



```{r}
df <- df %>%
  
  #Eliminar
  select(-agent) %>% 
  filter(adr != 5400) %>% 
  
  #Recodificar
  mutate(
         country = fct_lump(country, n = 15, other_level = 'OTROS'),
         market_segment = fct_collapse(market_segment,
                                       'Corporate' = c('Corporate','Undefined','Aviation','Complementary')),
         distribution_channel = fct_collapse(distribution_channel,
                                       'Corporate' = c('Corporate','Undefined','GDS')),
         reservation_status = fct_collapse(reservation_status,
                                       'Canceled' = c('Canceled','No-Show')),
         deposit_type = fct_collapse(deposit_type,
                                       'Deposit' = c('Non Refund','Refundable')),
         assigned_room_type = fct_collapse(assigned_room_type,
                                           'A' = 'A',
                                           'D' = 'D',
                                           other_level = 'OTROS'),
         reserved_room_type = fct_collapse(reserved_room_type,
                                           'A' = 'A',
                                           'D' = 'D',
                                           other_level = 'OTROS'),
         customer_type = fct_collapse(customer_type,
                                           'Transient' = 'Transient',
                                           other_level = 'OTROS'),
         meal = fct_collapse(meal,
                                  'BB' = 'BB',
                                  other_level = 'OTROS'),
         
         #Discretizar
         adults = as.factor(case_when(adults <= 1 ~ '01_Uno',
                            adults == 2 ~ '02_Dos',
                            adults > 2 ~ '03_Mas_de_Dos',
                            TRUE ~ '99_ERROR')),
         stays_in_week_nights_disc = as.factor(case_when(stays_in_week_nights <= 1 ~ '01_Uno',
                            stays_in_week_nights == 2 ~ '02_Dos',
                            stays_in_week_nights == 3 ~ '03_Tres',
                            stays_in_week_nights == 4 ~ '04_Cuatro',
                            stays_in_week_nights > 4 ~ '05_Mas_de_Cuatro',
                            TRUE ~ '99_ERROR')),
         stays_in_weekend_nights_disc = as.factor(case_when(stays_in_weekend_nights <= 1 ~ '01_Uno',
                            stays_in_weekend_nights > 1 ~ '02_Mas_de_Uno',
                            TRUE ~ '99_ERROR')),
        
         #Indicadores
         babies = as.logical(ifelse(babies == 0, 0, 1)),
         booking_changes = as.logical(ifelse(booking_changes == 0, 0, 1)),
         children = as.logical(ifelse(children == 0, 0, 1)),
         previous_cancellations = as.logical(ifelse(previous_cancellations == 0, 0, 1)),
         required_car_parking_spaces = as.logical(ifelse(required_car_parking_spaces == 0, 0, 1)),
         total_of_special_requests = as.logical(ifelse(total_of_special_requests == 0, 0, 1)),
         days_in_waiting_list = as.logical(ifelse(days_in_waiting_list == 0, 0, 1)),
         previous_bookings_not_canceled = as.logical(ifelse(previous_bookings_not_canceled == 0, 0, 1))
         )
  
```

Volvemos a hacer los gráficos para comprobar

```{r}
#Factores
#Como son 17 variables vamos a dividirlas en 2 tramos de 8
#Tramo 1
df %>% 
  select_if(is.factor) %>%
  select(1:9) %>% 
  mutate(id = 1:nrow(.)) %>% 
  pivot_longer(-id, names_to = 'variable', values_to = 'valor') %>% 
  ggplot(aes(valor)) +
  geom_bar() +
  facet_wrap(vars(variable), scales = 'free', ncol = 3)

#Tramo 2
df %>% 
  select_if(is.factor) %>%
  select(10:17) %>% 
  mutate(id = 1:nrow(.)) %>% 
  pivot_longer(-id, names_to = 'variable', values_to = 'valor') %>% 
  ggplot(aes(valor)) +
  geom_bar() +
  facet_wrap(vars(variable), scales = 'free', ncol = 3)

#Logicas
df %>% 
  select_if(is.logical) %>% 
  mutate(id = 1:nrow(.)) %>% 
  pivot_longer(-id, names_to = 'variable', values_to = 'valor') %>% 
  ggplot(aes(valor)) +
  geom_bar() +
  facet_wrap(vars(variable), scales = 'free',ncol = 2)

#Contínuas
df %>% 
  select_if(is.double) %>% 
  summary
```

### 5.3 - Creación de variables sintéticas

A partir de la info original que tenemos podemos crear las siguientes variables con significado de negocio:

- fecha_llegada: a partir de año, mes y día
- familia: a partir de adults, children y babies
- extranjero: cero si es de Portugal y uno si es de fuera
- cambio_habit: si el valor de reserved_room_type es diferente al de assigned_room_type
- noches_totales: suma de stays_in_weekend_nights más stays_in_week_nights
- facturacion_reserva: lo que se ha facturado en esa reserva como resultado de multiplicar noches_totales por adr

```{r}
df <- df %>% 
  mutate(fecha_llegada = as_date(paste(arrival_date_year,arrival_date_month,arrival_date_day_of_month,
                               sep = '/')),
         familia = case_when(
           adults == '01_Uno' & children + babies == 0 ~ '01_Soltero',
           adults == '01_Uno' & children + babies > 0 ~ '02_Monoparental',
           adults == '02_Dos' & children + babies == 0 ~ '03_Pareja_Sin_Hijos',
           adults == '02_Dos' & children + babies > 0 ~ '04_Pareja_Con_Hijos',
           adults == '03_Mas_de_Dos' & children + babies == 0 ~ '05_Grupo_Amigos',
           adults == '03_Mas_de_Dos' & children + babies > 0 ~ '06_Grupo_Amigos_Con_Hijos',
           TRUE ~ '99_ERROR'),
         extranjero = ifelse(country == 'PRT',0,1),
         cambio_habit = ifelse(reserved_room_type == assigned_room_type,0,1),
         noches_totales = stays_in_weekend_nights + stays_in_week_nights,
         facturacion_reserva = noches_totales * adr
         )
```


## 6 - Discovery y Generación de Insights


### 6.1 - Análisis de estacionalidad

¿Cuales son las temporadas altas y bajas? ¿Hay diferencias entre los 2 hoteles?

Vamos a coger la última temporada para la que tenemos el año completo: 2016.

Analizamos la estacionalidad general
```{r}
df %>% 
  filter(arrival_date_year == '2016') %>% 
  ggplot(aes(fecha_llegada)) + 
  geom_density(size = 3, color = 'grey') + 
  theme(axis.text.x = element_text(angle = 90, size = 10))
```

Conclusiones:
- Mayo y Octubre son los meses de mayor demanda
- De Diciembre a Febrero son los meses de menos demanda
- Sorprende Julio y Agosto que son relativamente bajos

Vamos a diferenciar por hotel

```{r}
df %>% 
  filter(arrival_date_year == '2016') %>% 
  ggplot(aes(fecha_llegada, color = hotel)) + 
  geom_density(size = 3) + 
  theme(axis.text.x = element_text(angle = 90, size = 10))
```

Conclusiones:
- Mantienen curvas de demanda parecidas
- Pero el resort tiene ligeramente más demanda hasta primavera
- Mientras que el de ciudad destaca en Junio, Septiembre y Octubre

### 6.2 - Análisis de facturación

Para ver la facturación vamos a quedarnos sólo con las reservas que finalmente tuvieron check-out

```{r}
df %>% 
  filter(reservation_status == 'Check-Out') %>% 
  group_by(arrival_date_year,hotel) %>% 
  summarise(facturacion = sum(facturacion_reserva)) %>% 
  ggplot(aes(x = arrival_date_year, y = facturacion, fill = hotel)) + 
  geom_col(position = 'dodge')
```

Conclusiones:
- Lógicamente 2017 no es comparable por no tener el año entero
- Sin embargo se ve que el de ciudad se ha disparado en 2016 frente a 2015
- El resort también ha crecido, pero a un ritmo menor que el de ciudad

¿Por qué se explica ese crecimiento?

En este negocio la facturación puede crecer por tres motivos:
- Hay más reservas
- Crece el número medio de noches por reserva
- Crece el precio por noche

Vamos a analizar si son 3 factores o especialmente alguno de ellos

```{r}
df %>% 
  filter(reservation_status == 'Check-Out' & arrival_date_year %in% c('2015','2016')) %>% 
  group_by(hotel,arrival_date_year) %>% 
  summarise(
    num_reservas = n(),
    noches_por_reserva = mean(noches_totales),
    precio_noche = mean(adr))
```

Conclusiones:
- Claramente el motivo ha sido el crecimiento del número de reservas
- Aunque el de ciudad también ha realizado un incremento significativo del precio de 2015 a 2016

¿Todos los segmentos de clientes contribuyen igual a la facturación?

```{r}
df %>% 
  group_by(hotel,market_segment) %>% 
  summarise(facturacion_segmento = sum(facturacion_reserva)) %>% 
  group_by(hotel) %>% 
  mutate(facturacion_total = sum(facturacion_segmento),
         porc_facturacion_total = facturacion_segmento / facturacion_total * 100)
            
```

¿Todos los tipos de clientes contribuyen igual a la facturación?

```{r}
df %>% 
  group_by(hotel,familia) %>% 
  summarise(facturacion_familia = sum(facturacion_reserva)) %>% 
  group_by(hotel) %>% 
  mutate(facturacion_total = sum(facturacion_familia),
         porc_facturacion_total = facturacion_familia / facturacion_total * 100)
            
```

Conclusiones:
- La mayor parte de la facturación proviene del segmento online y de parejas sin hijos

### 6.3 - Análisis del mercado y del cliente

¿Los clientes son nacionales o extranjeros?

```{r}
count(df,hotel,extranjero) %>% 
  group_by(hotel) %>% 
  mutate(total_hotel = sum(n),
         porc_hotel = n / total_hotel * 100)
```

Conclusiones:
- La mayoría de los clientes son extranjeros
- No hay mucha diferencia entre los hoteles

¿Cuales son las nacionalidades más frecuentes?

```{r}
count(df,hotel,country) %>% 
  group_by(hotel) %>% 
  mutate(total_hotel = sum(n),
         porc_hotel = n / total_hotel * 100) %>% 
  arrange(hotel, desc(porc_hotel)) %>% 
  top_n(5)
```

Conclusiones:
- Aquí sí hay diferencias interesantes
- Los mayoritarios siempre son portugueses
- Pero en el de ciudad los siguientes son franceses y alemanes
- Mientras que en el resort son británicos y españoles


### 6.4 - Análisis de precios

¿Varía mucho el precio por noche a lo largo del año?

```{r}
df %>% 
  filter(arrival_date_year %in% c('2015','2016')) %>% 
  group_by(hotel,arrival_date_week_number) %>% 
  summarise(precio_medio = mean(adr)) %>%
  ggplot(aes(x = arrival_date_week_number, y = precio_medio, group = 1)) +
  geom_line(color = 'grey') +
  geom_smooth(color = 'red') +
  theme(axis.text.x = element_text(size = 8))
```

Conclusiones:
- Hay claramente una estacionalidad en los precios

¿Correlaciona los precios con la demanda?

```{r}
c <- df %>% 
  filter(arrival_date_year %in% c('2015','2016')) %>% 
  group_by(hotel,arrival_date_week_number) %>% 
  summarise(num_reservas = n(),
            precio_medio = mean(adr)) %>% 
  select(num_reservas,precio_medio)

cor(c$num_reservas,c$precio_medio)
```

Conclusiones:
- Tiene una correlación positiva pero no muy alta

¿Cual es el precio medio en cada hotel?
```{r}
ggplot(df,aes(x = hotel, y = adr, color = hotel)) +
  geom_boxplot()
```

Conclusiones:
- Es ligeramente superior en el de ciudad

¿Se ahorra reservando con antelación?

```{r}
df %>% 
  group_by(hotel, lead_time) %>% 
  summarise(precio_medio = mean(adr)) %>% 
  ggplot(aes(x= lead_time, y = precio_medio, color = hotel)) +
  geom_point(alpha = 0.3) +
  geom_smooth(se = F, size = 2)
```

Conclusiones:
- Reservando con más de 6 meses de antelación se puede ahorrar bastante dinero en ambos hoteles
