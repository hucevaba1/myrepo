## Ejercicio de replica de clase con Yeve ##

#Para limpiar el ram de la computadora----
gc() 
getwd()


#Creando carpetas del proyecto ----
dir.create ("1_BASES")
dir.create ("2_SCRIPTS") 
dir.create ("3_GRAFICAS") 
dir.create ("4_COSAS_SECUNDARIAS")
dir.create("5_MAPAS")
# PARA BORRAR CARPETAS: unlink()


#Cargando paqueteria ----
pacman::p_load(
  tidyverse, 
  janitor, 
  sf, 
  stringr, 
  here, 
  scales, 
  showtext, 
  ggtext, 
  ggthemes, 
  purrr,
  beepr)


#Estableciento de region ----
Sys.setlocale(locale = "es_ES.UTF-8")


#Definiciones de ruta ----
path_bases <- "1_BASES"
path_grafs <- "3_GRAFICAS/" 
path_secundarias <- "4_COSAS_SECUNDARIAS/"
path_mapas <- "5_MAPAS/"


#Cargando las bases ----
# La para de graficas de barra
df_gobernaturas_mich_2018_raw <- read_csv(
  file = here::here(
    path_bases, "2021_SEE_GOB_MICH_SEC.csv"))
glimpse(
  df_gobernaturas_mich_2018_raw)

#La parte de mapas
read_sf(
  here::here(
    path_bases,
    "shp_mun", 
    "MUNICIPIO.shp")
) %>% 
  clean_names() %>% 
  st_transform(
    crs = "+proj=longlat +datum=WGS84 +no_defs") %>% 
  filter(
    entidad == "16"
  ) -> shp_mun



#Constuyendo la df limpia----
df_gobernaturas_mich_2018_raw %>%
  clean_names(.) %>%
  mutate(
    alfredo_bedolla = pt_morena + morena + pt,
    carlos_herrera = pan + pri + prd + pan_pri_prd + pan_pri + pan_prd + pri_prd,
    juan_magaña = pvem,
    mercedes_calderon = mc, 
    hipolito_mora = pes
  ) %>% 
  group_by(
    municipio) %>% 
  summarise(
    across(c(
      alfredo_bedolla, 
      carlos_herrera, 
      juan_magaña, 
      mercedes_calderon, 
      hipolito_mora,
      total_votos),
      ~ sum(
        .x, 
        na.rm = T)
    )
  ) %>% 
  pivot_longer(
    cols = alfredo_bedolla:hipolito_mora,
               names_to = "candidatos", 
               values_to = "votos") %>% 
  mutate(
    municipio = str_to_title(municipio),
    candidatos = str_to_title(gsub(x = candidatos, "_", " ")),
    porcentaje = votos/total_votos
  ) -> df_gobernaturas_mich_2018_clean


#Todo lo de la funcion para graficas ----
# Definimos vector pára automatización 
unique(df_gobernaturas_mich_2018_clean$municipio) -> vec_alcaldias

#Función
fun_barras <- function(var_alcaldia) {
  # Filtrar los datos para el municipio actual
  df_gobernaturas_mich_2018_clean %>% 
    filter(municipio == var_alcaldia) -> df_bar
  
  font_add_google("Nunito")
  showtext_auto()

  p <- ggplot(data = df_bar,
              mapping = aes(
                x = porcentaje,
                y = reorder(
                  candidatos, 
                  porcentaje)
              )) + 
    geom_bar(
      mapping = aes(
        fill = candidatos), 
      stat = "identity") +
    geom_text(
      aes(label = paste0(
        percent(
          porcentaje, 
          accuracy = 0.01),
        " (", 
        prettyNum(
          votos, 
          big.mark = ","),
        ")"),
        hjust = 0.5),
      family = "Nunito",
      fontface = "bold",
      size = 5,
      color = "black",
      position = position_stack(
        vjust = 0.5
      )
    ) + 
    labs(
      title = "Porcentaje de total de votos por candidato a Gobernatura: 2018",
      subtitle = var_alcaldia,
      caption = "Realización propia con base en datos del INE"
    ) +
    scale_x_continuous(
      name = "Porcentaje del total de votos",
      labels = percent,
      limits = c(0, .40)
    ) + 
    scale_y_discrete(
      name = "Candidatos a gobernatura"
    ) + 
    theme(
      axis.title.y = element_text(
        margin = margin(r = 15, unit = "pt")
      )
    ) + 
    scale_fill_manual(
      values = c(
        "Alfredo Bedolla" = "#761d01",
        "Carlos Herrera" = "#208e17",
        "Mercedes Calderon" = "#ff8300",
        "Hipolito Mora" = "#5a2a7c",
        "Juan Magaña" = "#026130"
      )
    ) + 
    ggthemes::theme_gdocs()
  
  p

  ggsave(
    paste0(
      path_grafs, 
      "resultados_candidato_", 
      make_clean_names(var_alcaldia), 
      ".png"), 
    p,
    width = 16.5, 
    height = 10.5, 
    dpi = 96
  )
}

# Llamar a la función para cada municipio
map(vec_alcaldias, fun_barras) 


#Ahora la parte del mapa----
#Primero hago que sean compatibles los vectores

df_gobernaturas_mich_2018_clean$municipio <- toupper(
  df_gobernaturas_mich_2018_clean$municipio)

#Luego la union
left_join(
  df_gobernaturas_mich_2018_clean,
  shp_mun,
  by = c(
    "municipio" = "nombre"
  )
)  %>% 
  filter(
    !is.na(
      entidad)) %>% 
  mutate(
    var_color = case_when(
      candidatos == "Alfredo Bedolla" ~ "#761d01",
      candidatos == "Carlos Herrera" ~ "#208e17", 
      candidatos == "Mercedes Calderon" ~ "#ff8300",
      candidatos == "Hipolito Mora" ~ "#5a2a7c",
      candidatos == "Juan Magaña" ~ "#026130"
    )
  ) %>% 
  st_as_sf -> df_mapa 

#Ahora si el código para el mapa
font_add_google("Nunito")
showtext_auto()

unique(df_mapa$candidatos) -> vec_candidatos

 
fun_mapas <- function(var_candidatos) {
  # var_candidatos = "Mercedes Calderon"
df_mapa %>% 
  filter(
    candidatos == var_candidatos
  ) -> df_temp 

df_temp %>% 
  slice_max(
    porcentaje,
    n = 5
  ) -> df_filtrval

ggplot() +
  labs(
    title = paste0(
      "% del total de votos de ", 
      var_candidatos),
    subtitle = "Porcentaje del total de votos por alcaldía que\nobtuvo la/el candidata/o",
    caption = "Realización propia con datos del INE."
  ) +
  geom_sf(
    data = df_temp, 
    mapping = aes(
      geometry = geometry, 
      fill = porcentaje
    )
  ) + 
  geom_sf_label(
    data = df_filtrval,
    mapping = aes(
      geometry = geometry, 
      label = paste0(municipio,
                     "\n", 
                     percent(porcentaje,
                             accuracy = 0.01))
    ),
    family = "Nunito",
    fontface = "bold",
    size = 4,
    fill = alpha(
      "white",
      0.5) 
  ) +
  scale_fill_gradient(
    "% del total de votos",
    low = "lightgray",
    high = unique(df_temp$var_color),
    labels = percent
  ) +
  theme_void() + 
  theme(
    plot.background = element_rect(
      fill = "white",
      color = "transparent"
    ),
    plot.title = element_text(
      family = "Nunito", 
      face = "bold", 
      size = 32,
      hjust = 0.5,
      margin = margin(
        t = unit(10, 
                 "cm"), 
        b = unit(10, 
                 "cm"))
    ),
    plot.subtitle = element_text(
      family = "Nunito", 
      face = "italic", 
      size = 26,
      hjust = 0.5,
      margin = margin(
        b = unit(5, 
                 "cm"))
    ),
    legend.position = "right",
    legend.title = element_text(
      family = "Nunito", 
      face = "bold"
    ),
    legend.text = element_text(
      family = "Nunito", 
      face = "bold"
    ),
    plot.caption = element_text(
      family = "Nunito",
      size = 10,
      hjust = 0.5,
      margin = margin(
        t = unit(
          15, 
          "cm"),
        b = unit(10,
                 "cm"))
    )
  ) -> map

map

ggsave(
  paste0(
    path_mapas, 
    "mapa_candidatos_",
    make_clean_names(var_candidatos), 
    ".png"),
  map,
  width = 10.5, 
  height = 10.5, 
  dpi = 96 
)
}
map(vec_candidatos, fun_mapas)
