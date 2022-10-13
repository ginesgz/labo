# source( "~/labo/src/lightgbm/z633_lightgbm_binaria_BO.r" )
# Este script esta pensado para correr en Google Cloud
#   8 vCPU
#  64 GB memoria RAM
# 256 GB espacio en disco

# se entrena con POS =  { BAJA+1, BAJA+2 }
# Optimizacion Bayesiana de hiperparametros de  lightgbm, con el metodo TRADICIONAL de los hiperparametros originales de lightgbm
# 5-fold cross validation
# la probabilidad de corte es un hiperparametro

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rlist")

require("lightgbm")

#paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")

options(error = function() { 
  traceback(20); 
  options(error = NULL); 
  stop("exiting after script error") 
})


kBO_iter  <- 100   #cantidad de iteraciones de la Optimizacion Bayesiana

# ATENCION  si NO se quiere utilizar  undersampling  se debe  usar  kundersampling <- 1.0
kundersampling  <- 0.15   # un undersampling de 0.1  toma solo el 10% de los CONTINUA

prob_min  <- 0.5/( 1 + kundersampling*39)
prob_max  <- pmin( 1.0, 4/( 1 + kundersampling*39) )

#Aqui se cargan los hiperparametros
hs <- makeParamSet( 
  makeNumericParam("learning_rate",     lower=  0.001   , upper=    0.25),
  makeNumericParam("feature_fraction",  lower=  0.15    , upper=    1.0),
  makeNumericParam("perc_data_in_leaf", lower= 0.001     , upper= 0.15),            # % mín. de observaciones representadas en cada nodo, con esto voy a calcular min_data_in_leaf
  #makeIntegerParam("min_data_in_leaf", lower=  0      , upper= 8000),          # ya queda calculada en param.básicos con perc_data_in_leaf
  makeNumericParam("perc_coverage",     lower= 0.25     , upper = 1.0),              # % de datos que voy a considerar en las hojas
  #makeIntegerParam("num_leaves",       lower= 16L     , upper= 1024L),         # ya queda calculada en param.básicos con perc_coverage
  makeNumericParam("prob_corte",        lower= prob_min, upper= prob_max  )  #esto sera visto en clase en gran detalle
)

kdataset       <- "./exp/FE8150_FEadic_lag123_IPC/dataset_815_FEadic_lag123_IPC.csv.gz"    #dataset con FE adic, lags3 y ajustado por IPC

ksemilla_azar  <- c(791321,910643,263537,693167,484607)  #Aqui poner la propia semilla
kexperimento   <- "HT6330_lag3_us015_5r5fcv_5mes"
ktraining      <- c( 202101,202012,202011,202010,202009 )   #periodos en donde entreno

kPOS_ganancia  <- 78000
kNEG_ganancia  <- -2000

#------------------------------------------------------------------------------
#graba a un archivo los componentes de lista
#para el primer registro, escribe antes los titulos

loguear  <- function( reg, arch=NA, folder="./exp/", ext=".txt", verbose=TRUE )
{
  archivo  <- arch
  if( is.na(arch) )  archivo  <- paste0(  folder, substitute( reg), ext )
  
  if( !file.exists( archivo ) )  #Escribo los titulos
  {
    linea  <- paste0( "fecha\t", 
                      paste( list.names(reg), collapse="\t" ), "\n" )
    
    cat( linea, file=archivo )
  }
  
  linea  <- paste0( format(Sys.time(), "%Y%m%d %H%M%S"),  "\t",     #la fecha y hora
                    gsub( ", ", "\t", toString( reg ) ),  "\n" )
  
  cat( linea, file=archivo, append=TRUE )  #grabo al archivo
  
  if( verbose )  cat( linea )   #imprimo por pantalla
}
#------------------------------------------------------------------------------
#esta funcion calcula internamente la ganancia de la prediccion probs

fganancia_logistic_lightgbm   <- function( probs, datos) 
{
  vlabels  <- get_field(datos, "label")
  vpesos   <- get_field(datos, "weight")
  
  gan  <- sum( (probs > PROB_CORTE  ) *
                 ifelse( vpesos == 1.0000002, kPOS_ganancia, 
                         ifelse( vpesos == 1.0000001, kNEG_ganancia, kNEG_ganancia / kundersampling ) ) )
  
  
  return( list( "name"= "ganancia", 
                "value"=  gan,
                "higher_better"= TRUE ) )
}
#------------------------------------------------------------------------------
#esta funcion solo puede recibir los parametros que se estan optimizando
#el resto de los parametros se pasan como variables globales, la semilla del mal ...

EstimarGanancia_lightgbm  <- function( x )
{
  gc()  #libero memoria
  
  #llevo el registro de la iteracion por la que voy
  GLOBAL_iteracion  <<- GLOBAL_iteracion + 1
  
  PROB_CORTE <<- x$prob_corte   #asigno la variable global
  
  kfolds  <- 5   # cantidad de folds para cross validation
  
  #agregado para 5repated-5fold CV
  ganancia_total <- 0
  cantidad_semillas_usadas <- 0
  
  for (semilla in ksemilla_azar){
    
    param_basicos  <- list( objective= "binary",
                            metric= "custom",
                            first_metric_only= TRUE,
                            boost_from_average= TRUE,
                            feature_pre_filter= FALSE,
                            verbosity= -100,
                            max_depth=  -1,         # -1 significa no limitar,  por ahora lo dejo fijo
                            min_gain_to_split= 0.0, #por ahora, lo dejo fijo
                            lambda_l1= 0.0,         #por ahora, lo dejo fijo
                            lambda_l2= 0.0,         #por ahora, lo dejo fijo
                            max_bin= 31,            #por ahora, lo dejo fijo
                            num_iterations= 9999,   #un numero muy grande, lo limita early_stopping_rounds
                            force_row_wise= TRUE,   #para que los alumnos no se atemoricen con tantos warning
                            seed= semilla, 
                            min_data_in_leaf = max(round(DS_LEN * x$perc_data_in_leaf, 0),2), #redondeo porque este hp tiene que ser entero
                            #num_leaves = DS_LEN / min_data_in_leaf,              # el nro de hojas es el total de datos div en el mínimo de datos por hoja
                            #num_leaves = num_leaves * x$perc_coverage,           # no quiero cubrir el 100% de los datos en hojas, cubro un % para evitar overfitting
                            num_leaves = max(round(x$perc_coverage / x$perc_data_in_leaf, 0),2) #redondeo porque este hp tiene que ser entero
    )
    
    #el parametro discolo, que depende de otro
    param_variable  <- list(  early_stopping_rounds= as.integer(50 + 5/x$learning_rate) )
    
    param_completo  <- c( param_basicos, param_variable, x )
    
    # set.seed( ksemilla_azar )
    set.seed( semilla )
    modelocv  <- lgb.cv( data= dtrain,
                         eval= fganancia_logistic_lightgbm,
                         stratified= TRUE, #sobre el cross validation
                         nfold= kfolds,    #folds del cross validation
                         param= param_completo,
                         verbose= -100
    )
    
    ganancia_semilla <-  unlist(modelocv$record_evals$valid$ganancia$eval)[ modelocv$best_iter ]
    ganancia_total  <- ganancia_total + ganancia_semilla
    
    cantidad_semillas_usadas <- cantidad_semillas_usadas + 1
    
    print(paste0("semilla = ", semilla ))
    print(paste0("cantidad_semillas_usadas = ", cantidad_semillas_usadas ))
    print(paste0("ganancia_semilla = ", ganancia_semilla ))
    print(paste0("ganancia_total = ", ganancia_total ))
    
    
    if (ganancia_semilla < 17500000) {
      print("Sale de iteración por poca ganancia de semilla")
      break
    }
    
    if (cantidad_semillas_usadas == 4 & ganancia_total < 72000000) {
      print("Sale de iteración por poca ganancia total")
      break
    }
    
  }
  
  
  ganancia_normalizada  <-  ganancia_total / cantidad_semillas_usadas * kfolds     #normailizo la ganancia
  
  #el lenguaje R permite asignarle ATRIBUTOS a cualquier variable
  attr(ganancia_normalizada ,"extras" )  <- list("num_iterations"= modelocv$best_iter)  #esta es la forma de devolver un parametro extra
  
  param_completo$num_iterations <- modelocv$best_iter  #asigno el mejor num_iterations
  param_completo["early_stopping_rounds"]  <- NULL     #elimino de la lista el componente  "early_stopping_rounds"
  
  #logueo 
  xx  <- param_completo
  xx$ganancia  <- ganancia_normalizada   #le agrego la ganancia
  xx$iteracion <- GLOBAL_iteracion
  xx$semillas_usadas <- cantidad_semillas_usadas
  loguear( xx, arch= klog )
  
  return( ganancia_normalizada )
}
#------------------------------------------------------------------------------
#Aqui empieza el programa

#Aqui se debe poner la carpeta de la computadora local
setwd("~/buckets/b1/")   #Establezco el Working Directory

#cargo el dataset donde voy a entrenar el modelo
dataset  <- fread( kdataset)

#creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( paste0( "./exp/", kexperimento, "/"), showWarnings = FALSE )
setwd( paste0( "./exp/", kexperimento, "/") )   #Establezco el Working Directory DEL EXPERIMENTO

#en estos archivos quedan los resultados
kbayesiana  <- paste0( kexperimento, ".RDATA" )
klog        <- paste0( kexperimento, ".txt" )


GLOBAL_iteracion  <- 0   #inicializo la variable global

#si ya existe el archivo log, traigo hasta donde llegue
if( file.exists(klog) )
{
  tabla_log  <- fread( klog )
  GLOBAL_iteracion  <- nrow( tabla_log )
}



#paso la clase a binaria que tome valores {0,1}  enteros
dataset[ foto_mes %in% ktraining, clase01 := ifelse( clase_ternaria=="CONTINUA", 0L, 1L) ]


#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01", "azar", "training" ) )

#set.seed( ksemilla_azar )
dataset[  , azar := runif( nrow( dataset ) ) ]
dataset[  , training := 0L ]
dataset[ foto_mes %in% ktraining & ( azar <= kundersampling | clase_ternaria %in% c( "BAJA+1", "BAJA+2" ) ), training := 1L ]

#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix(  dataset[ training == 1L, campos_buenos, with=FALSE]),
                        label= dataset[ training == 1L, clase01 ],
                        weight=  dataset[ training == 1L, ifelse( clase_ternaria=="BAJA+2", 1.0000002, ifelse( clase_ternaria=="BAJA+1",  1.0000001, 1.0) )],
                        free_raw_data= FALSE  )


#mi agregado
DS_LEN <- dataset[training == 1L,.N]
print(paste0("columns = ", length(campos_buenos)))
print(paste0("len = ", DS_LEN))

#Aqui comienza la configuracion de la Bayesian Optimization
funcion_optimizar  <- EstimarGanancia_lightgbm   #la funcion que voy a maximizar

configureMlr( show.learner.output= FALSE)

#configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
#por favor, no desesperarse por lo complejo
obj.fun  <- makeSingleObjectiveFunction(
  fn=       funcion_optimizar, #la funcion que voy a maximizar
  minimize= FALSE,   #estoy Maximizando la ganancia
  noisy=    TRUE,
  par.set=  hs,     #definido al comienzo del programa
  has.simple.signature = FALSE   #paso los parametros en una lista
)

ctrl  <- makeMBOControl( save.on.disk.at.time= 600,  save.file.path= kbayesiana)  #se graba cada 600 segundos
ctrl  <- setMBOControlTermination(ctrl, iters= kBO_iter )   #cantidad de iteraciones
ctrl  <- setMBOControlInfill(ctrl, crit= makeMBOInfillCritEI() )

#establezco la funcion que busca el maximo
surr.km  <- makeLearner("regr.km", predict.type= "se", covtype= "matern3_2", control= list(trace= TRUE))

#inicio la optimizacion bayesiana
if( !file.exists( kbayesiana ) ) {
  run  <- mbo(obj.fun, learner= surr.km, control= ctrl)
} else {
  run  <- mboContinue( kbayesiana )   #retomo en caso que ya exista
}


quit( save="no" )

