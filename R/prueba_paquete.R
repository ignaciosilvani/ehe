# aca voy probando el paquete

#cargo paquete
# library()

# 1. consulto bases disponibles
bases_disponibles <- available_ehe()

# 2. cargo base usuaria
base <- get_ehe(
  localidad = "Lanús", 
  anio = 2023,
  tipo = "individual"
)

# 3. etiqueta con funcion label
# primero obtengo el diseño de registro:
dic <- ehe_dictionary(
  encuesta = "EHE-M",
  anio = 2023,
  tipo = "individual"
)

# etiqueto base:
base_lab <- ehe_labels(
  base,
  dictionary = dic
)
