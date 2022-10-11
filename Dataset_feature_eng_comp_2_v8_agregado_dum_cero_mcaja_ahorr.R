#Se utiliza el algoritmo Random Forest, creado por Leo Breiman en el año 2001
#Una libreria que implementa Rando Forest se llama  ranger
#La libreria esta implementada en lenguaje C y corre en paralelo, utiliza TODOS los nucleos del procesador
#Leo Breiman provenia de la estadistica y tenia "horror a los nulos", con lo cual el algoritmo necesita imputar nulos antes


#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

require("data.table")
require("ranger")
require("randomForest")  #solo se usa para imputar nulos

#Aqui se debe poner la carpeta de la computadora local
setwd("/Users/martinacazzolli/UBA2022/DMFyE")  #Establezco el Working Directory

#cargo el dataset
dataset  <- fread("./datasets/competencia2_2022.csv.gz", stringsAsFactors= TRUE)
typeof(dataset)
#imputo los nulos, ya que ranger no acepta nulos
#Leo Breiman, ¿por que le temias a los nulos?

#data.frame(table(dataset$clase_ternaria,colum=c(dataset$active_quarter)))
data.frame(table(dataset$clase_ternaria,colum=c(dataset$ctrx_quarter)))


#dataset[ , campo1 := as.integer( ctrx_quarter <14 & mcuentas_saldo < -1256.1 & cprestamos_personales <2 ) ]
#dataset[ , campo2 := as.integer( ctrx_quarter <14 & mcuentas_saldo < -1256.1 & cprestamos_personales>=2 ) ]

#dataset[ , campo3 := as.integer( ctrx_quarter <14 & mcuentas_saldo>= -1256.1 & mcaja_ahorro <2601.1 ) ]
#dataset[ , campo4 := as.integer( ctrx_quarter <14 & mcuentas_saldo>= -1256.1 & mcaja_ahorro>=2601.1 ) ]

#dataset[ , campo5 := as.integer( ctrx_quarter>=14 & ( Visa_status>=8 | is.na(Visa_status) ) & ( Master_status>=8 | is.na(Master_status) ) ) ]
#dataset[ , campo6 := as.integer( ctrx_quarter>=14 & ( Visa_status>=8 | is.na(Visa_status) ) & ( Master_status <8 & !is.na(Master_status) ) ) ]

#dataset[ , campo7 := as.integer( ctrx_quarter>=14 & Visa_status <8 & !is.na(Visa_status) & ctrx_quarter <38 ) ]
#dataset[ , campo8 := as.integer( ctrx_quarter>=14 & Visa_status <8 & !is.na(Visa_status) & ctrx_quarter>=38 ) ]

#dataset[ , campo9 := as.integer( ctrx_quarter>=14 & Visa_status <8 & !is.na(Visa_status) & ctrx_quarter <38 ) ]
#dataset[ , campo10 := as.integer( ctrx_quarter>=14 & Visa_status <8 & !is.na(Visa_status) & ctrx_quarter>=38 ) ]

#Mese a años

#dataset$cliente_antiguedad <- dataset$cliente_antiguedad/12
#Dummies
dataset[ , tcuentas_ninguna := as.integer( tcuentas == 0 ) ]

dataset[ , tcuentas_una := as.integer( tcuentas == 1 ) ]
dataset[ , tcuentas_varias := as.integer( tcuentas == 2 ) ]

#Variables anteriores


dataset$mpasivos_margen_ctrx_quarter <- dataset$mpasivos_margen/dataset$ctrx_quarter

dataset$mtarjeta_visa_consumo_ctrx_quarter <- dataset$mtarjeta_visa_consumo/dataset$ctrx_quarter



dataset$mcuentas_saldo_ctrx_quarter <- dataset$mcuentas_saldo/dataset$ctrx_quarter


#Nuevo arbol
dataset$ctrx_quarter[dataset$ctrx_quarter==0] <- 1

dataset$mcaja_ahorro_ctrx_quarter <- dataset$mcaja_ahorro/dataset$ctrx_quarter

dataset$mcuenta_corriente_ctrx_quarter <- dataset$mcuenta_corriente/dataset$ctrx_quarter

dataset$ctarjeta_master_ctrx_quarter <- dataset$ctarjeta_master/dataset$ctrx_quarter


dataset$mprestamos_personales_ctrx_quarter <- dataset$mprestamos_personales/dataset$ctrx_quarter

dataset$cprestamos_personales_ctrx_quarter <- dataset$cprestamos_personales/dataset$ctrx_quarter

dataset$ctarjeta_master_ctrx_quarter <- dataset$ctarjeta_master/dataset$ctrx_quarter

dataset$mactivos_margen_ctrx_quarter <- dataset$mactivos_margen/dataset$ctrx_quarter

dataset$mcomisiones_mantenimiento_ctrx_quarter <- dataset$mcomisiones_mantenimiento/dataset$ctrx_quarter


#Elimino la variable + imp y genero nuevos feature con la importancia

dataset$mcaja_ahorro[dataset$mcaja_ahorro==0] <- 1


dataset$mtarjeta_visa_consumo_mcaja_ahorro <- dataset$mtarjeta_visa_consumo/dataset$mcaja_ahorro

dataset$mtarjeta_master_consumo_mcaja_ahorro <- dataset$mtarjeta_master_consumo/dataset$mcaja_ahorro

dataset$mprestamos_personales_mcaja_ahorro <- dataset$mprestamos_personales/dataset$mcaja_ahorro

dataset$ctarjeta_visa_transacciones_mcaja_ahorro <- dataset$ctarjeta_visa_transacciones/dataset$mcaja_ahorro

dataset$ctarjeta_master_transacciones_mcaja_ahorro <- dataset$ctarjeta_master_transacciones/dataset$mcaja_ahorro

dataset$cprestamos_personales_mcaja_ahorro <- dataset$cprestamos_personales/dataset$mcaja_ahorro

dataset$Visa_msaldopesos_mcaja_ahorro <- dataset$Visa_msaldopesos/dataset$mcaja_ahorro

dataset$master_msaldopesos_mcaja_ahorro <- dataset$master_msaldopesos/dataset$mcaja_ahorro

dataset$master_msaldototal_mcaja_ahorro <- dataset$Master_msaldototal/dataset$mcaja_ahorro

dataset$mpasivos_margen_mcaja_ahorro <- dataset$mpasivos_margen/dataset$mcaja_ahorro

dataset$mcomisiones_mantenimiento_mcaja_ahorro <- dataset$mcomisiones_mantenimiento/dataset$mcaja_ahorro

dataset$ctarjeta_master_debitos_automaticos_mcaja_ahorro <- dataset$ctarjeta_master_debitos_automaticos/dataset$mcaja_ahorro

dataset$ctarjeta_visa_debitos_automaticos_mcaja_ahorro <- dataset$ctarjeta_visa_debitos_automaticos/dataset$mcaja_ahorro



#Rankeo de variables

dataset[ , mrentabilidad_ninguna := as.integer( mrentabilidad == 0 ) ]

dataset[,rank_rentabilidad:=rank(-mrentabilidad,ties.method="first"),by=foto_mes]
dataset[,  mrentabilidad := NULL]

dataset[ , mrentabilidad_annual_ninguna := as.integer( mrentabilidad_annual == 0 ) ]

dataset[,rank_mrentabilidad_annual:=rank(-mrentabilidad_annual,ties.method="first"),by=foto_mes]
dataset[,  mrentabilidad_annual := NULL]

dataset[ , mpasivos_margen_ninguna := as.integer( mpasivos_margen == 0 ) ]


dataset[,rank_mpasivos_margen:=rank(-mpasivos_margen,ties.method="first"),by=foto_mes]
dataset[,  mpasivos_margen := NULL]

dataset[ , mtarjeta_visa_consumo_ninguna := as.integer( mtarjeta_visa_consumo == 0 ) ]


dataset[,rank_mtarjeta_visa_consumo:=rank(-mtarjeta_visa_consumo,ties.method="first"),by=foto_mes]
dataset[,  mtarjeta_visa_consumo := NULL]

dataset[ , mtarjeta_master_consumo_ninguna := as.integer( mtarjeta_master_consumo == 0 ) ]



dataset[,rank_mtarjeta_master_consumo:=rank(-mtarjeta_master_consumo,ties.method="first"),by=foto_mes]
dataset[,  mtarjeta_master_consumo := NULL]

dataset[ , mcuentas_saldo_ninguna := as.integer( mcuentas_saldo == 0 ) ]


dataset[,rank_mcuentas_saldo:=rank(-mcuentas_saldo,ties.method="first"),by=foto_mes]
dataset[,  mcuentas_saldo := NULL]

dataset[ , mcaja_ahorro_ninguna := as.integer( mcaja_ahorro == 0 ) ]


mcaja_ahorro <- setdiff( colnames(dataset), "clase_ternaria" )

for( campo in mcaja_ahorro )
{
  if(  dataset[ get(campo) < 0, .N ]  > 0 ) {
    dataset[   , paste0( campo, "_neg" ) := ifelse( get(campo)< 0, get(campo), 0 ) ]
    dataset[   , paste0( campo, "_pos" ) := ifelse( get(campo)> 0, get(campo), 0 ) ]
  }
}


dataset[,  mcaja_ahorro := NULL]


mcuenta_corriente <- setdiff( colnames(dataset), "clase_ternaria" )

for( campo in mcuenta_corriente )
{
  if(  dataset[ get(campo) < 0, .N ]  > 0 ) {
    dataset[   , paste0( campo, "_neg" ) := ifelse( get(campo)< 0, get(campo), 0 ) ]
    dataset[   , paste0( campo, "_pos" ) := ifelse( get(campo)> 0, get(campo), 0 ) ]
  }
}

dataset[,  mcuenta_corriente := NULL]



dataset[,rank_mprestamos_personales:=rank(-mprestamos_personales,ties.method="first"),by=foto_mes]
dataset[,  mprestamos_personales := NULL]


dataset[,rank_mactivos_margen:=rank(-mactivos_margen,ties.method="first"),by=foto_mes]
dataset[,  mactivos_margen := NULL]


dataset[,rank_mcomisiones_mantenimiento:=rank(-mcomisiones_mantenimiento,ties.method="first"),by=foto_mes]
dataset[,  mcomisiones_mantenimiento := NULL]

dataset[,rank_Visa_msaldopesos:=rank(-Visa_msaldopesos,ties.method="first"),by=foto_mes]
dataset[,  Visa_msaldopesos := NULL]

dataset[,rank_Visa_msaldototal:=rank(-Visa_msaldototal,ties.method="first"),by=foto_mes]
dataset[,  Visa_msaldototal := NULL]

dataset[,rank_Master_msaldopesos:=rank(-Master_msaldopesos,ties.method="first"),by=foto_mes]
dataset[,  Master_msaldopesos := NULL]

dataset[,rank_Master_msaldototal:=rank(-Master_msaldototal,ties.method="first"),by=foto_mes]
dataset[,  Master_msaldototal := NULL]

dataset[,  master_msaldopesos_mcaja_ahorro := NULL]

dataset[-c(clase_ternaria)] <- lapply(dataset,as.numeric)
dataset
write.csv(dataset,"/Users/martinacazzolli/UBA2022/DMFyE/datasets/competencia2_2022_v9.csv", row.names = FALSE)
