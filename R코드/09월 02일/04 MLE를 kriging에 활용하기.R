#∎Question∎. 오케이(송정환 교수님 톤으로) 그러면 얘네를 kriging 에 어떻게 활용하지?
#good_parameters <- optim(parameters,NegLogL_optim,method='L-BFGS-B')$par

#MLE_sill <- good_parameters[1]
#MLE_range <- good_parameters[2]
#MLE_nu <- good_parameters[3]

#Matern_variogram <- vgm(psill = MLE_sill, range = MLE_range, kappa = MLE_nu, model = "Mat")
#Kriging_model <- krige(rem_dev~1, remV.sp, grd, model=Matern_variogram)
#Kriging_model$var1.pred=Kriging_model$var1.pred+mean(rem_tem)