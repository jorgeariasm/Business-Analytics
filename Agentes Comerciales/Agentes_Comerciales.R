##Formato Rmarkdown

---
title: ' Agentes Comerciales'
author: "Jorge Arias"
proyecto: DS4B
output: html_notebook
editor_options:
  chunk_output_type: console
---



## 1 - SET UP
```{r setup, include=FALSE}

# Opciones generales
knitr::opts_chunk$set(echo = TRUE)
options(scipen=999)
theme_set(theme_classic())

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

```

## 2 - CONTEXTO

Los datos corresponden al histórico de ventas de negocio de una red agencial de una compañía de seguros.

Tras 2 años poco satisfactorios la compañía está valorando hacer cambios en su red de agentes.

Está diseñando una nueva red "premium", con unos objetivos mucho más altos pero también con una propuesta de valor mucho más atractiva.

La nueva red premium se formará con nuevos agentes contratados en mercado y con los mejores agentes de los actuales.

Nos piden analizar los datos para encontrar a los mejores agentes acuales a los que ofrecer formar parte de la nueva red.


## 3 - CARGA DE DATOS

Los datos están en el fichero AnalisisAgentes.csv dentro de './Datasets/AnalisisAgentes/'

Primero hacemos una exploración inicial de los datos en bruto.

```{r}
read_lines('./Datasets/AnalisisAgentes/AnalisisAgentes.csv',n_max = 3)
```

Hay cabecera con nombres de variables y el separador es la coma

```{r}
df <- read_csv('./Datasets/AnalisisAgentes/AnalisisAgentes.csv')
```

No ha dado warnings de importación. Vamos a hacer un vistazo general.

```{r}
glimpse(df)
```

* f_desde y f_hasta deberían ser tipo fecha
* zona y canal deberían ser factores

Volvemos a importar

```{r}
df <- read_csv('./Datasets/AnalisisAgentes/AnalisisAgentes.csv',
               col_types = cols(
                 f_desde = col_date(format = '%d/%m/%y'),
                 f_hasta = col_date(format = '%d/%m/%y'),
                 zona = col_factor(),
                 canal = col_factor()
               ))
```

Revisamos de nuevo

```{r}
glimpse(df)
```

Parece que ya hemos conseguido una buena importación. 
Si existiera algún otro error lo encontraríamos en la siguiente fase

## 4 - CALIDAD DE DATOS

Volumen del fichero

```{r}
dim(df)
```

Estadísticos básicos

```{r}
summary(df)
```

No detectamos aquí ningún problema de calidad de datos, aunque sí hay ciertas cosas que apuntamos para la fase de EDA:

* Agente parece el ID: comprobar que no haya duplicados
* El mínimo de f_desde es extraño: revisar
* f_hasta tiene siempre el mismo valor: usar sólo para construir la variable antigüedad
* zona y canal hay que explorarlas más a fondo
* producción y cartera parecen tener mucha variabilildad

Análisis de nulos

```{r}
df %>% 
  summarise_all(.funs = ~sum(is.na(.)))
```

## 5 - EDA

Empezamos analizando que no haya duplicados en el ID

```{r}
n_distinct(df$agente)
```

Analizamos la distribución de f_desde

```{r}
ggplot(df,aes(f_desde)) + geom_histogram(bins = 50)
```

Vemos que hay atípicos en los extremos, vamos a revisarlo

```{r}
#los más antiguos
count(df,f_desde) %>% 
  slice(1:20)

#los más recientes
count(df,f_desde) %>% 
  arrange(desc(f_desde)) %>% 
  slice(1:20)
```

Para entendero mejor vamos a analizarlo por año

```{r}
df %>% 
  mutate(ano = year(f_desde)) %>% 
  count(ano) %>% 
  ggplot(aes(ano,n))+ geom_col()
```

Ya lo vemos mucho mejor. Parece que hay unos "históricos" anteriores a 1990, y también que en 2009 y 2010 se hizo una contratación masiva por lo que habrá agentes muy nuevos.

Vamos a crear las variables antiguedad en meses y en años.

```{r}
df <- df %>% 
  mutate(antiguedad_meses = trunc(time_length(f_hasta -f_desde, unit = 'months')),
         antiguedad_anos = trunc(time_length(f_hasta -f_desde, unit = 'years'))
         )
```

Analizamos sus distribuciones

```{r}
ggplot(df,aes(antiguedad_meses)) + geom_bar()

ggplot(df,aes(antiguedad_anos)) + geom_bar()
```

Vamos a analizar ahora las variables principales de negocio: producción y cartera

```{r}
summary(df$produccion)
```

Parece que hay atipicos

```{r}
df %>% 
  arrange(desc(produccion)) %>% 
  slice(1:10)
```

Hay 4 agentes por encima del millón en producción.

Vamos a ver por el otro lado si hay muchos que tengan producción cero
```{r}
df %>% 
  filter(produccion == 0) %>% 
  summarise(conteo = n(),
            porcentaje = conteo / nrow(df) * 100)
```

Parece que empezamos a ver que habrá gran dispersión entre los diferentes agentes.

## 6 - ANALISIS DE NEGOCIO

Vamos a hacer un análisis de Pareto.

```{r}
df %>% 
  arrange(desc(produccion)) %>% 
  mutate(ranking_prod = row_number(desc(produccion)),
         porc_acum_prod = cumsum(produccion / sum(produccion))) %>% 
  ggplot(aes(x = ranking_prod, y = porc_acum_prod)) +
  geom_line()
```

El 100% de la producción se alcanza con 1000 agentes. Y sobre el 90% con menos de 500 agentes.

Pareto nos han demostrado que existe una gran desigualdad en la capacidad de producción de los agentes.

Vamos a ver si pasa lo mismo con la cartera.

```{r}
df %>% 
  arrange(desc(cartera)) %>% 
  mutate(ranking_car = row_number(desc(cartera)),
         porc_acum_car = cumsum(cartera / sum(cartera))) %>% 
  ggplot(aes(x = ranking_car, y = porc_acum_car)) +
  geom_line()
```

Efectivamente, por tanto tiene sentido que creemos unas variables de ranking de producción y de cartera

```{r}
df <- df %>% 
  mutate(ranking_prod = row_number(desc(produccion)),
         ranking_car = row_number(desc(cartera))
         )
```

¿Habrá relación entre la posición en el ranking y la antigüedad del agente?

```{r}
cor(df$antiguedad_anos,df$ranking_prod)
#a mayor edad, deberia tener mejor puesto en ranking? segun cor  NO

cor(df$antiguedad_anos,df$ranking_car)
#cuanto mas antiguo el gente menor posicion en cartera, eso nos indica el signo negtivo.
```

En producción no hay correlación y en cartera una ligera correlación, pero quizá menos de la que esperaríamos, por lo que no necesariamente los agentes más antiguos son los mejores.

Vamos a calcular las medias por año de antigüedad para verlo claro

```{r}
df %>% 
  group_by(antiguedad_anos) %>% 
  summarise(media_prod = mean(produccion),
            media_car = mean(cartera)) %>% 
  print(n = Inf)
```

Veámoslo gráficamente

```{r}
df %>% 
  group_by(antiguedad_anos) %>% 
  summarise(media_prod = mean(produccion)) %>% 
  ggplot(aes(x = antiguedad_anos, y = media_prod)) + geom_col()
```

```{r}
df %>% 
  group_by(antiguedad_anos) %>% 
  summarise(media_car = mean(cartera)) %>% 
  ggplot(aes(x = antiguedad_anos, y = media_car)) + geom_col()
```

¿Los agentes que son buenos lo son tanto para producción como para cartera?¿o hay "especializaciones"?

Podemos empezar a responder a esta pregunta simplemente haciendo una correlación

```{r}
cor(df$ranking_car,df$ranking_prod)
```

Vemos que en general sí son los mismos.

Pero vamos a profundizar más, vamos a crear una matriz estratética y posicionar a cada angente

```{r}
ggplotly(
ggplot(df,aes(cartera,produccion)) + geom_point()
)
```

Efectivamente, salvo excepciones los buenos agentes lo son en ambas cosas.


Vamos a analizar ahora el negocio por zona y canal.

```{r}
df_largo <- df %>% 
  select(agente,produccion,cartera,zona,canal) %>% 
  pivot_longer(cols = c('zona','canal'), names_to = 'dimension', values_to = 'dim_valor') %>% 
  pivot_longer(cols = c('produccion','cartera'), names_to = 'metrica', values_to = 'met_valor')

df_largo %>% 
  group_by(dimension,dim_valor,metrica) %>% 
  summarise(Suma = sum(met_valor)) %>% 
  ggplot(aes(x = dim_valor, y = Suma)) +
  scale_y_continuous(labels = scales::comma) +
  geom_col() +
  coord_flip() +
  facet_wrap(dimension ~ metrica, scales = 'free', ncol = 2)
```

Vamos a calcular la contribución de cada canal y area a la producción y a la cartera

```{r}
contribucion <- df_largo %>% 
  group_by(dimension,dim_valor,metrica) %>% 
  summarise(met_valor = sum(met_valor)) %>%
  ungroup() %>% 
  group_by(dimension,metrica) %>% 
  mutate(Total = sum(met_valor),
         Porc = met_valor / Total * 100) %>% 
  arrange(dimension, dim_valor,metrica,desc(Porc)) %>%
  select(-met_valor,-Total) %>% 
  pivot_wider(names_from = metrica, values_from = Porc) %>% 
  arrange(dimension,desc(cartera))

contribucion
```

Vamos a verlo gráficamente

```{r}
contribucion %>% 
  pivot_longer(cols = c('cartera','produccion'), names_to = 'metrica', values_to = 'valor') %>% 
  ggplot(aes(x = dim_valor, y = valor)) +
  geom_col() +
  facet_wrap(dimension ~ metrica, scales = 'free')
```

## 7 - SELECCION FINAL DE LOS AGENTES PARA LA NUEVA RED

Con esto hemos visto la contribución total al negocio, pero como lo que estamos buscando son los mejores agentes, lo que tendría sentido no es quedarnos con los mejores agentes en general, ya que los datos podrían estar sesgados por el potencial de la zona.

Es decir, a priori un mercado como la zona centro tiene mayor potencial que otro como Andalucía, por lo que comparativamente un agente que haga 100.000€ en Andalucía puede ser "mejor agente" que otro que haga 120.000€ en Madrid.

Además como la nueva red va a tener presencia nacional tiene sentido que identifiquemos los mejores en cada zona, para garantizar esa cobertura.

Por tanto podríamos crear un proceso que seleccionara los mejores agentes por zonas hasta cubrir su cuotas correspondientes. Seria algo así:

1) Identificar el número de agentes totales que vamos a coger de la red actual
2) Dividir ese número total por zonas en base al % de la contribución de negocio de cada zona. Por ejemplo si la zona centro representa el 50% del negocio haremos que el 50% de los agentes totales sean de la zona centro
3) Identificar a los mejores agentes por zona hasta cumplir todos los cupos

### 7.1 - Identificar el número de agentes totales

Vamos a ver cuantos agentes necesitamos para condensar el 70%, 80% y 90% de la produccion actual

```{r}
ranking_produccion <- df %>% 
  arrange(desc(produccion)) %>% 
  mutate(ranking_prod = row_number(desc(produccion)),
         porc_acum_prod = cumsum(produccion / sum(produccion))) %>% 
  select(agente,zona,porc_acum_prod,produccion,ranking_prod)

#para el 70%
filter(ranking_produccion, porc_acum_prod <= 0.7) %>% 
  summarise(conteo = n(),
            porc = conteo / nrow(ranking_produccion) * 100)

#para el 80%
filter(ranking_produccion, porc_acum_prod <= 0.8) %>% 
  summarise(conteo = n(),
            porc = conteo / nrow(ranking_produccion) * 100)

#para el 90%
filter(ranking_produccion, porc_acum_prod <= 0.9) %>% 
  summarise(conteo = n(),
            porc = conteo / nrow(ranking_produccion) * 100)
```

Vamos a quedarnos con el último escenario, ya que con el 20% de los agentes mantenemos el 90% de la producción.

Podríamos hacer lo mismo con la cartera, pero ya que habíamos visto que estaban muy relacionados lo dejaremos así por sencillez.

Por tanto sabemos que vamos a coger a 420 agentes

### 7.2 - Dividir ese número total por zonas

Si retomamos el objeto contribución filtrando sólo por zonas sabemos el % de agentes que tenemos que coger de cada zona. De nuevo por sencillez usaremos sólo el dato de producción

```{r}
cuotas_zonas <- filter(contribucion, dimension == 'zona') %>% select(-cartera)
```

Vamos a enriquecer cuotas_zonas con el número absoluto de agentes que necesitamos

```{r}
cuotas_zonas <- cuotas_zonas %>% 
  mutate(agentes = round(produccion / 100 * 420))
```

Con estos datos ya podríamos hacer la extracción de los agentes concretos que vamos a seleccionar para cada zona.

Por ejemplo vamos a obtener los de la zona Centro

```{r}
cuota <- filter(cuotas_zonas,dim_valor == 'Centro') %>% pull(agentes)

agentes_Centro <- ranking_produccion %>% 
  filter(zona == 'Centro') %>% 
  arrange(desc(produccion)) %>% 
  slice(1:cuota) %>% 
  select(-porc_acum_prod)
```

Estos son los agentes que hemos seleccionado finalmente para la zona Centro

```{r}
agentes_Centro
```

