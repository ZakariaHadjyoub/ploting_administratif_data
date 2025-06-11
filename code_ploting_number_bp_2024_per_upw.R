library(sf)
library(dplyr)
library(viridis)
library(classInt) # for calculating quantils
library(ggplot2)
library(patchwork)
library(scales)
library(RColorBrewer)
library(showtext) 

#font_add_google("Michroma", "michroma")  # Or replace with "michroma" or another preferred font
showtext_auto()  # Enable showtext for rendering


# read and prepare data
data_shp <- st_read("GCS_WGS_1984/Decoupage_Administratif_Wilaya_WGS.shp") %>% 
  st_make_valid() # for not having probleme when manipulating polygones

data_bp=read.csv2("upw_data_reseau_2024.csv") %>% 
  tibble() %>% 
  select(code=1,nb_bp=2)

data_shp_bp <- data_shp %>%
  select(code,Nom) %>% 
  left_join(data_bp,by = "code")


# Create custom breaks for the color scale
breaks <- classIntervals(data_shp_bp$nb_bp, 
                         n = 5, style = "pretty")

text_scale <- 300 / 96  # Adjust based on screen DPI (typically 96)

# Create the main map
main_map <- ggplot(data_shp_bp) +
  geom_sf(aes(fill = nb_bp), color = "white", size = 0.3) +
  scale_fill_distiller(palette = "YlOrBr", 
                       direction = 1,
                       name = "nb_bp",
                       breaks = breaks$brks,
                       labels = round(breaks$brks, 2)) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_blank()
  ) 

# Prepare enhanced table data
table_data <- data_shp_bp %>%
  st_drop_geometry() %>%
  select(Area = Nom, Value = nb_bp) %>%  
  arrange(desc(Value)) %>%
  mutate(
    Value_formatted = round(Value, 1),
    Value_display = comma(Value_formatted),
    rank = row_number(),
    display_name = Area,
    relative_importance = Value / max(Value, na.rm = TRUE),
    bar_color = colorRampPalette(RColorBrewer::brewer.pal(9, "YlOrBr"))(100)[
      as.numeric(cut(Value, breaks = 100, include.lowest = TRUE))
    ]
  )

# Create enhanced table with horizontal bars
table_plot <- ggplot(table_data, aes(y = reorder(display_name, Value))) +
  geom_col(aes(x = relative_importance), 
           fill = table_data$bar_color, width = 0.5) +
  geom_text(aes(x = -0.02, label = display_name), 
            hjust = 1, vjust = 0.5, size = 3 * text_scale, color = "black", family = "michroma") +
  geom_text(aes(x = relative_importance + 0.02, label = Value_display), 
            hjust = 0, vjust = 0.5, size = 3 * text_scale, color = "black", 
            fontface = "bold", family = "michroma") +
  scale_x_continuous(limits = c(-0.35, 1.15), expand = c(0, 0)) +
  scale_y_discrete(expand = c(0.05, 0.05)) +  # Increased vertical expansion for more space
  theme_void() +
  theme(
    plot.margin = margin(t = 0, r = 5, b = 0, l = 5),  # Reduced top margin to extend upward
    text = element_text(family = "michroma", size = 10 * text_scale)
  )


# Final combination with adjusted layout
final_plot <- main_map + table_plot + 
  plot_layout(ncol = 2, widths = c(3, 2))+
  plot_annotation(
    title = "Nombre de bureaux de poste existants par Wilaya",
    subtitle = "Données au 31/12/2024",
    theme = theme(
      plot.title = element_text(family = "michroma", size = 25 * text_scale, face = "bold", hjust = 0, 
                                margin = margin(t = 2, b = 2)),
      plot.subtitle = element_text(family = "michroma", size = 14 * text_scale, hjust = 0, color = "grey30",
                                   margin = margin(b = 3)),
      plot.background = element_rect(fill = "white", color = NA)  # Set transparent background
    )
  )

ggsave("bureau_de_poste_par_wilaya.png", final_plot, 
       width = 15, height = 10, dpi = 300, device = ragg::agg_png, bg = "transparent")