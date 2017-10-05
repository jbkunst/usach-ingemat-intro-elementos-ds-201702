#' --- 
#' title: Tarea 01
#' output:
#'   html_document:
#'     theme: yeti
#'     toc: true
#'     toc_float: true
#' ---
#+ echo = FALSE, message = FALSE, warning = FALSE
knitr::opts_chunk$set(message = FALSE, warning = FALSE, cache = TRUE)
library(jbkmisc)
library(ggplot2)
library(webshot)
theme_set(theme_jbk())

# Default image paths
fig_path <- local({
  i <- 0
  path <- knitr::opts_chunk$get('fig.path')
  function() {
    i <<- i + 1
    paste0(path, 'webshot', i, '.png')
  }
})

#'
#' En http://www.quieroleer.com.ar/ existen muchos libros para leer. Cada uno
#' separado por páginas. Imáginen que quiero extraer como text la información
#'  de _JS gaviota_ (para los amigos). La página me muestra algo como 
#'  http://www.quieroleer.com.ar/libros/juansalvador/:
#'      
#+ echo=FALSE
webshot("http://www.quieroleer.com.ar/libros/juansalvador/", delay = 0.5)

#'
#' Y cada de las páginas tiene la siguiente forma.    
#'      
#+ echo=FALSE
webshot("http://www.quieroleer.com.ar/libros/juansalvador/2.html", delay = 0.5)

#'      
#' Supongamos que quremos realizar el ejercicio de 
#' https://github.com/jbkunst/usach-ingemat-intro-elementos-ds-201702/blob/master/01-AED/cadenas_markov.R
#' con este libro. Lo primero que debemos hacer es leer todas las páginas
#' y usar el __mismo__ código para realizar la generación de texto aleatorio.
#' 
#' ¿Cómo leemos las páginas? Si estamos usango google Chrome podemos
#' hacer `CTRL+U` en http://www.quieroleer.com.ar/libros/juansalvador/
#' y ver que cada página está linkeada de la forma `<A HREF="1.html">1</A>` por
#' lo que usando `rvest` es muy muy fácil.
#' 
library(tidyverse)
library(tidytext)
library(rvest)
library(stringr)

url <- "http://www.quieroleer.com.ar/libros/juansalvador/"

paginas <- read_html(url) %>% 
  html_nodes("a") %>% 
  html_text() 

#' 
#' Lo que hicimos fue:
#' 
#' 1. Leer la página
#' 2. Extraer los nodos que contengan un __tag__  `a`, vale decir todos los links
#' de la página.
#' 3. Y finalmente transformar a texto.
#' 
#' Podemos explorar el contenidos de paginas para ver si nos hace sentido
#' con las páginas que el libro tiene en el link. 
#
paginas

#' 
#' Todo de acuerdo al keikaku!!! (referencia: http://knowyourmeme.com/memes/just-according-to-keikaku)
#' 
#' Notar que finalmente hay un link que nos lleva a la página de lista de libros
#' por lo que lo removemos.
#' 
paginas <- paginas[-length(paginas)]

#' 
#' Ahora, conociendo las páginas que el libro tiene, puede acceder a cualquera y su
#' contenido!
#' 
txt <- read_html("http://www.quieroleer.com.ar/libros/lotr1/3.html") %>% 
  html_nodes("body") %>% 
  html_text() 

str_sub(txt, -50)

txt <- str_replace_all(txt, "ANTERIOR|INICIO|SIGUIENTE", "")

str_sub(txt, -50)

#' 
#' Ahora se puede recorrer cada página y unirlas para generar todo el libro:
#' un GRAN GRAN vector de strings!
#' 
#' ## Ejercicio 1
#' 
#' Dado que somos 

