rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
setwd( "~/buckets/b1/" )

#cargo el dataset
dataset  <- fread( "./datasets/competencia1_historia_2022.csv.gz" )

#IPC de 201901 a 202103 (27 meses)
IPC = c(189.6,196.8,206.0,213.1,219.6,225.5,230.5,239.6,253.7,262.1,273.2,283.4,289.8,295.7,305.6,310.1,314.9,322.0,328.2,337.1,346.6,359.7,371.0,385.9,401.5,415.9,435.9)

#Tipo de cambio oficial de 201901 a 202103 (27 meses)
TipoCambio = c(37.04,39.00,43.35,44.01,44.87,42.45,43.87,59.08,57.56,59.73,59.86,59.90,60.33,62.21,64.47,66.84,68.54,70.46,72.32,74.18,76.18,78.33,81.30,84.15,87.30,89.83,91.99)

#variables a ajustar por IPC
varPesos = c("mrentabilidad","mrentabilidad_annual","mcomisiones","mactivos_margen","mpasivos_margen","mcuenta_corriente_adicional","mcuenta_corriente","mcaja_ahorro","mcaja_ahorro_adicional","mcuentas_saldo","mautoservicio","mtarjeta_visa_consumo","mtarjeta_master_consumo","mprestamos_personales","mprestamos_prendarios","mprestamos_hipotecarios","mplazo_fijo_pesos","minversion1_pesos","minversion2","mpayroll","mpayroll2","mcuenta_debitos_automaticos","mttarjeta_master_debitos_automaticos","mpagodeservicios","mpagomiscuentas","mcajeros_propios_descuentos","mtarjeta_master_descuentos","mcomisiones_mantenimiento","mcomisiones_otras","mtransferencias_recibidas","mtransferencias_emitidas","mextraccion_autoservicio","mcheques_depositados","mcheques_emitidos","mcheques_depositados_rechazados","mcheques_emitidos_rechazados","matm","matm_other","Master_mfinanciacion_limite","Master_msaldototal","Master_msaldopesos","Master_mconsumospesos","Master_mlimitecompra","Master_madelantopesos","Master_mpagado","Master_mpagospesos","Master_mconsumototal","Master_mpagominimo","Visa_mfinanciacion_limite","Visa_msaldototal","Visa_msaldopesos","Visa_mconsumospesos","Visa_mlimitecompra","Visa_madelantopesos","Visa_mpagado","Visa_mpagospesos","Visa_mconsumototal","Visa_mpagominimo")
varDolares = c("mcaja_ahorro_dolares","mplazo_fijo_dolares","minversion1_dolares","Master_msaldodolares","Master_mconsumosdolares","Master_madelantodolares","Master_mpagosdolares","Visa_msaldodolares","Visa_mconsumosdolares","Visa_madelantodolares","Visa_mpagosdolares","mforex_buy","mforex_sell")
varMonetarias = c(varPesos,varDolares)

#------------------------------------------------------------------------------- 
#Reconversión a dólar por tipo de cambio oficial
for (var in varDolares) { 
  
  dataset[foto_mes == 201901, (paste0(var,"_USD")) := lapply(.SD, '/', TipoCambio[1]), .SDcols = var]
  dataset[foto_mes == 201902, (paste0(var,"_USD")) := lapply(.SD, '/', TipoCambio[2]), .SDcols = var]
  dataset[foto_mes == 201903, (paste0(var,"_USD")) := lapply(.SD, '/', TipoCambio[3]), .SDcols = var]
  dataset[foto_mes == 201904, (paste0(var,"_USD")) := lapply(.SD, '/', TipoCambio[4]), .SDcols = var]
  dataset[foto_mes == 201905, (paste0(var,"_USD")) := lapply(.SD, '/', TipoCambio[5]), .SDcols = var]
  dataset[foto_mes == 201906, (paste0(var,"_USD")) := lapply(.SD, '/', TipoCambio[6]), .SDcols = var]
  dataset[foto_mes == 201907, (paste0(var,"_USD")) := lapply(.SD, '/', TipoCambio[7]), .SDcols = var]
  dataset[foto_mes == 201908, (paste0(var,"_USD")) := lapply(.SD, '/', TipoCambio[8]), .SDcols = var]
  dataset[foto_mes == 201909, (paste0(var,"_USD")) := lapply(.SD, '/', TipoCambio[9]), .SDcols = var]
  dataset[foto_mes == 201910, (paste0(var,"_USD")) := lapply(.SD, '/', TipoCambio[10]), .SDcols = var]
  dataset[foto_mes == 201911, (paste0(var,"_USD")) := lapply(.SD, '/', TipoCambio[11]), .SDcols = var]
  dataset[foto_mes == 201912, (paste0(var,"_USD")) := lapply(.SD, '/', TipoCambio[12]), .SDcols = var]
  dataset[foto_mes == 202001, (paste0(var,"_USD")) := lapply(.SD, '/', TipoCambio[13]), .SDcols = var]
  dataset[foto_mes == 202002, (paste0(var,"_USD")) := lapply(.SD, '/', TipoCambio[14]), .SDcols = var]
  dataset[foto_mes == 202003, (paste0(var,"_USD")) := lapply(.SD, '/', TipoCambio[15]), .SDcols = var]
  dataset[foto_mes == 202004, (paste0(var,"_USD")) := lapply(.SD, '/', TipoCambio[16]), .SDcols = var]
  dataset[foto_mes == 202005, (paste0(var,"_USD")) := lapply(.SD, '/', TipoCambio[17]), .SDcols = var]
  dataset[foto_mes == 202006, (paste0(var,"_USD")) := lapply(.SD, '/', TipoCambio[18]), .SDcols = var]
  dataset[foto_mes == 202007, (paste0(var,"_USD")) := lapply(.SD, '/', TipoCambio[19]), .SDcols = var]
  dataset[foto_mes == 202008, (paste0(var,"_USD")) := lapply(.SD, '/', TipoCambio[20]), .SDcols = var]
  dataset[foto_mes == 202009, (paste0(var,"_USD")) := lapply(.SD, '/', TipoCambio[21]), .SDcols = var]
  dataset[foto_mes == 202010, (paste0(var,"_USD")) := lapply(.SD, '/', TipoCambio[22]), .SDcols = var]
  dataset[foto_mes == 202011, (paste0(var,"_USD")) := lapply(.SD, '/', TipoCambio[23]), .SDcols = var]
  dataset[foto_mes == 202012, (paste0(var,"_USD")) := lapply(.SD, '/', TipoCambio[24]), .SDcols = var]
  dataset[foto_mes == 202101, (paste0(var,"_USD")) := lapply(.SD, '/', TipoCambio[25]), .SDcols = var]
  dataset[foto_mes == 202102, (paste0(var,"_USD")) := lapply(.SD, '/', TipoCambio[26]), .SDcols = var]
  dataset[foto_mes == 202103, (paste0(var,"_USD")) := lapply(.SD, '/', TipoCambio[length(TipoCambio)]), .SDcols = var]
  
}


#------------------------------------------------------------------------------- 
#AJUSTE POR IPC (de todas las variables en pesos o valores pesificados)
for (var in varMonetarias) { 
  
  dataset[foto_mes == 201901, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[1]), .SDcols = var]
  dataset[foto_mes == 201902, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[2]), .SDcols = var]
  dataset[foto_mes == 201903, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[3]), .SDcols = var]
  dataset[foto_mes == 201904, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[4]), .SDcols = var]
  dataset[foto_mes == 201905, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[5]), .SDcols = var]
  dataset[foto_mes == 201906, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[6]), .SDcols = var]
  dataset[foto_mes == 201907, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[7]), .SDcols = var]
  dataset[foto_mes == 201908, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[8]), .SDcols = var]
  dataset[foto_mes == 201909, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[9]), .SDcols = var]
  dataset[foto_mes == 201910, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[10]), .SDcols = var]
  dataset[foto_mes == 201911, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[11]), .SDcols = var]
  dataset[foto_mes == 201912, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[12]), .SDcols = var]
  dataset[foto_mes == 202001, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[13]), .SDcols = var]
  dataset[foto_mes == 202002, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[14]), .SDcols = var]
  dataset[foto_mes == 202003, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[15]), .SDcols = var]
  dataset[foto_mes == 202004, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[16]), .SDcols = var]
  dataset[foto_mes == 202005, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[17]), .SDcols = var]
  dataset[foto_mes == 202006, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[18]), .SDcols = var]
  dataset[foto_mes == 202007, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[19]), .SDcols = var]
  dataset[foto_mes == 202008, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[20]), .SDcols = var]
  dataset[foto_mes == 202009, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[21]), .SDcols = var]
  dataset[foto_mes == 202010, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[22]), .SDcols = var]
  dataset[foto_mes == 202011, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[23]), .SDcols = var]
  dataset[foto_mes == 202012, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[24]), .SDcols = var]
  dataset[foto_mes == 202101, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[25]), .SDcols = var]
  dataset[foto_mes == 202102, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[26]), .SDcols = var]
  #dataset[foto_mes == 202103, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[27]), .SDcols = var]
  
}


#grabo el dataset
setwd( "./datasets" )
fwrite( dataset,
        "competencia1_historia_2022_ajustadoIPC.csv.gz",
        logical01= TRUE,
        sep= "," )