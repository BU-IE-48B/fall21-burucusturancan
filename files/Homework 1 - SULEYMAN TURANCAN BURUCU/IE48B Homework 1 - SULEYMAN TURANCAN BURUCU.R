library(scatterplot3d)
setwd("D:/Lectures/(2021-2022) Lectures Senior/IE48B/GitHub IE48B Repository/fall21-burucusturancan/files/Homework 1 - SULEYMAN TURANCAN BURUCU")

x_acceleration = read.table("uWaveGestureLibrary_X_TRAIN")
y_acceleration = read.table("uWaveGestureLibrary_Y_TRAIN")
z_acceleration = read.table("uWaveGestureLibrary_Z_TRAIN")

velocity = function(datatable)
{
  for (i in 3:ncol(datatable)) 
  {
    datatable[,i] = datatable[,i] + datatable[,i-1]
  }
  return(datatable)
}

x_velocity = velocity(x_acceleration)
y_velocity = velocity(y_acceleration)
z_velocity = velocity(z_acceleration)

location = function(datatable)
{
  datatable2 = datatable
  datatable2[,2] = datatable[,2]/2
  for (i in 3:ncol(datatable)) 
  {
    datatable2[,i] = datatable2[,i-1] + ((datatable[,i] + datatable[,i-1]) / 2)
  }
  return(datatable2)
}

x_location = location(x_velocity)
y_location = location(y_velocity)
z_location = location(z_velocity)

classgrouping = c(rep(1, 40), rep(2, 40), rep(3, 40), 
                  rep(4, 40), rep(5, 40), rep(6, 40),
                  rep(7, 40), rep(8, 36))

class1_example = cbind(classgrouping, 
                       t(x_location[657,]), 
                       t(y_location[657,]), 
                       t(z_location[657,]))
class1_example = class1_example[-1,]
scatterplot3d(class1_example[,2:4], pch = 8, color = class1_example[,1],
              main = "Class 1",
              xlim = c(min(class1_example), max(class1_example)),
              ylim = c(min(class1_example), max(class1_example)),
              zlim = c(min(class1_example), max(class1_example)),
              angle = 350, xlab = "X-Axis", ylab = "Y-Axis", zlab = "Z-Axis")

class2_example = cbind(classgrouping, 
                       t(x_location[575,]), 
                       t(y_location[575,]), 
                       t(z_location[575,]))
class2_example = class2_example[-1,]
scatterplot3d(class2_example[,2:4], pch = 8, color = class2_example[,1],
              main = "Class 2",
              xlim = c(min(class2_example), max(class2_example)),
              ylim = c(min(class2_example), max(class2_example)),
              zlim = c(min(class2_example), max(class2_example)),
              angle = 330, xlab = "X-Axis", ylab = "Y-Axis", zlab = "Z-Axis")

class3_example = cbind(classgrouping, 
                       t(x_location[413,]), 
                       t(y_location[413,]), 
                       t(z_location[413,]))
class3_example = class3_example[-1,]
scatterplot3d(class3_example[,2:4], pch = 8, color = class3_example[,1],
              main = "Class 3",
              xlim = c(min(class3_example), max(class3_example)),
              ylim = c(min(class3_example), max(class3_example)),
              zlim = c(min(class3_example), max(class3_example)),
              angle = 290, xlab = "X-Axis", ylab = "Y-Axis", zlab = "Z-Axis")

class4_example = cbind(classgrouping, 
                       t(x_location[328,]), 
                       t(y_location[328,]), 
                       t(z_location[328,]))
class4_example = class4_example[-1,]
scatterplot3d(class4_example[,2:4], pch = 8, color = class4_example[,1],
              main = "Class 4",
              xlim = c(min(class4_example), max(class4_example)),
              ylim = c(min(class4_example), max(class4_example)),
              zlim = c(min(class4_example), max(class4_example)),
              angle = 320, xlab = "X-Axis", ylab = "Y-Axis", zlab = "Z-Axis")

class5_example = cbind(classgrouping, 
                       t(x_location[411,]), 
                       t(y_location[411,]), 
                       t(z_location[411,]))
class5_example = class5_example[-1,]
scatterplot3d(class5_example[,2:4], pch = 8, color = class5_example[,1],
              main = "Class 5",
              xlim = c(min(class5_example), max(class5_example)),
              ylim = c(min(class5_example), max(class5_example)),
              zlim = c(min(class5_example), max(class5_example)),
              angle = 30, xlab = "X-Axis", ylab = "Y-Axis", zlab = "Z-Axis")

class6_example = cbind(classgrouping,
                       t(x_location[863,]), 
                       t(y_location[863,]), 
                       t(z_location[863,]))
class6_example = class6_example[-1,]
scatterplot3d(class6_example[,2:4], pch = 8, color = class6_example[,1],
              main = "Class 6",
              xlim = c(min(class6_example), max(class6_example)),
              ylim = c(min(class6_example), max(class6_example)),
              zlim = c(min(class6_example), max(class6_example)),
              angle = 20, xlab = "X-Axis", ylab = "Y-Axis", zlab = "Z-Axis")

class7_example = cbind(classgrouping, 
                       t(x_location[659,]), 
                       t(y_location[659,]), 
                       t(z_location[659,]))
class7_example = class7_example[-1,]
scatterplot3d(class7_example[,2:4], pch = 8, color = class7_example[,1],
              main = "Class 7",
              xlim = c(min(class7_example), max(class7_example)),
              ylim = c(min(class7_example), max(class7_example)),
              zlim = c(min(class7_example), max(class7_example)),
              angle = 330, xlab = "X-Axis", ylab = "Y-Axis", zlab = "Z-Axis")

class8_example = cbind(classgrouping, 
                       t(x_location[371,]), 
                       t(y_location[371,]), 
                       t(z_location[371,]))
class8_example = class8_example[-1,]
scatterplot3d(class8_example[,2:4], pch = 8, color = class8_example[,1],
              main = "Class 8",
              xlim = c(min(class8_example), max(class8_example)),
              ylim = c(min(class8_example), max(class8_example)),
              zlim = c(min(class8_example), max(class8_example)),
              angle = 345, xlab = "X-Axis", ylab = "Y-Axis", zlab = "Z-Axis")

piecewise_aggregate_approximation = function(datatable)
{
  datatable2 = datatable[, 1:10]
  for (i in 1:896) 
  {
    for (j in 2:10) 
    {
      datatable2[i, j] = mean(as.numeric(datatable[i, (35*j-68):(35*j-34)]))
    }
  }
  return(datatable2)
}

x_location_piecewise_aggregate_approximation = piecewise_aggregate_approximation(x_location)
y_location_piecewise_aggregate_approximation = piecewise_aggregate_approximation(y_location)
z_location_piecewise_aggregate_approximation = piecewise_aggregate_approximation(z_location)

class1_piecewise_aggregate_approximation = cbind(t(x_location_piecewise_aggregate_approximation[657,]), 
                                                 t(y_location_piecewise_aggregate_approximation[657,]), 
                                                 t(z_location_piecewise_aggregate_approximation[657,]))
class1_piecewise_aggregate_approximation = class1_piecewise_aggregate_approximation[-1,]
scatterplot3d(class1_piecewise_aggregate_approximation, pch = 8, color = 1:9,
              main = "Class 1 PAA",
              xlim = c(min(class1_piecewise_aggregate_approximation), max(class1_piecewise_aggregate_approximation)),
              ylim = c(min(class1_piecewise_aggregate_approximation), max(class1_piecewise_aggregate_approximation)),
              zlim = c(min(class1_piecewise_aggregate_approximation), max(class1_piecewise_aggregate_approximation)),
              angle = 60, xlab = "X-Axis", ylab = "Y-Axis", zlab = "Z-Axis")

class2_piecewise_aggregate_approximation = cbind(t(x_location_piecewise_aggregate_approximation[575,]), 
                                                 t(y_location_piecewise_aggregate_approximation[575,]), 
                                                 t(z_location_piecewise_aggregate_approximation[575,]))
class2_piecewise_aggregate_approximation = class2_piecewise_aggregate_approximation[-1,]
scatterplot3d(class2_piecewise_aggregate_approximation, pch = 8, color = 1:9,
              main = "Class 2 PAA",
              xlim = c(min(class2_piecewise_aggregate_approximation), max(class2_piecewise_aggregate_approximation)),
              ylim = c(min(class2_piecewise_aggregate_approximation), max(class2_piecewise_aggregate_approximation)),
              zlim = c(min(class2_piecewise_aggregate_approximation), max(class2_piecewise_aggregate_approximation)),
              angle = 340, xlab = "X-Axis", ylab = "Y-Axis", zlab = "Z-Axis")

class3_piecewise_aggregate_approximation = cbind(t(x_location_piecewise_aggregate_approximation[413,]), 
                                                 t(y_location_piecewise_aggregate_approximation[413,]), 
                                                 t(z_location_piecewise_aggregate_approximation[413,]))
class3_piecewise_aggregate_approximation = class3_piecewise_aggregate_approximation[-1,]
scatterplot3d(class3_piecewise_aggregate_approximation, pch = 8, color = 1:9,
              main = "Class 3 PAA",
              xlim = c(min(class3_piecewise_aggregate_approximation), max(class3_piecewise_aggregate_approximation)),
              ylim = c(min(class3_piecewise_aggregate_approximation), max(class3_piecewise_aggregate_approximation)),
              zlim = c(min(class3_piecewise_aggregate_approximation), max(class3_piecewise_aggregate_approximation)),
              angle = 290, xlab = "X-Axis", ylab = "Y-Axis", zlab = "Z-Axis")

class4_piecewise_aggregate_approximation = cbind(t(x_location_piecewise_aggregate_approximation[328,]), 
                                                 t(y_location_piecewise_aggregate_approximation[328,]), 
                                                 t(z_location_piecewise_aggregate_approximation[328,]))
class4_piecewise_aggregate_approximation = class4_piecewise_aggregate_approximation[-1,]
scatterplot3d(class4_piecewise_aggregate_approximation, pch = 8, color = 1:9,
              main = "Class 4 PAA",
              xlim = c(min(class4_piecewise_aggregate_approximation), max(class4_piecewise_aggregate_approximation)),
              ylim = c(min(class4_piecewise_aggregate_approximation), max(class4_piecewise_aggregate_approximation)),
              zlim = c(min(class4_piecewise_aggregate_approximation), max(class4_piecewise_aggregate_approximation)),
              angle = 290, xlab = "X-Axis", ylab = "Y-Axis", zlab = "Z-Axis")

class5_piecewise_aggregate_approximation = cbind(t(x_location_piecewise_aggregate_approximation[411,]), 
                                                 t(y_location_piecewise_aggregate_approximation[411,]), 
                                                 t(z_location_piecewise_aggregate_approximation[411,]))
class5_piecewise_aggregate_approximation = class5_piecewise_aggregate_approximation[-1,]
scatterplot3d(class5_piecewise_aggregate_approximation, pch = 8, color = 1:9,
              main = "Class 5 PAA",
              xlim = c(min(class5_piecewise_aggregate_approximation), max(class5_piecewise_aggregate_approximation)),
              ylim = c(min(class5_piecewise_aggregate_approximation), max(class5_piecewise_aggregate_approximation)),
              zlim = c(min(class5_piecewise_aggregate_approximation), max(class5_piecewise_aggregate_approximation)),
              angle = 30, xlab = "X-Axis", ylab = "Y-Axis", zlab = "Z-Axis")

class6_piecewise_aggregate_approximation = cbind(t(x_location_piecewise_aggregate_approximation[863,]), 
                                                 t(y_location_piecewise_aggregate_approximation[863,]), 
                                                 t(z_location_piecewise_aggregate_approximation[863,]))
class6_piecewise_aggregate_approximation = class6_piecewise_aggregate_approximation[-1,]
scatterplot3d(class6_piecewise_aggregate_approximation, pch = 8, color = 1:9,
              main = "Class 6 PAA",
              xlim = c(min(class6_piecewise_aggregate_approximation), max(class6_piecewise_aggregate_approximation)),
              ylim = c(min(class6_piecewise_aggregate_approximation), max(class6_piecewise_aggregate_approximation)),
              zlim = c(min(class6_piecewise_aggregate_approximation), max(class6_piecewise_aggregate_approximation)),
              angle = 30, xlab = "X-Axis", ylab = "Y-Axis", zlab = "Z-Axis")

class7_piecewise_aggregate_approximation = cbind(t(x_location_piecewise_aggregate_approximation[659,]), 
                                                 t(y_location_piecewise_aggregate_approximation[659,]), 
                                                 t(z_location_piecewise_aggregate_approximation[659,]))
class7_piecewise_aggregate_approximation = class7_piecewise_aggregate_approximation[-1,]
scatterplot3d(class7_piecewise_aggregate_approximation, pch = 8, color = 1:9,
              main = "Class 7 PAA",
              xlim = c(min(class7_piecewise_aggregate_approximation), max(class7_piecewise_aggregate_approximation)),
              ylim = c(min(class7_piecewise_aggregate_approximation), max(class7_piecewise_aggregate_approximation)),
              zlim = c(min(class7_piecewise_aggregate_approximation), max(class7_piecewise_aggregate_approximation)),
              angle = 345, xlab = "X-Axis", ylab = "Y-Axis", zlab = "Z-Axis")

class8_piecewise_aggregate_approximation = cbind(t(x_location_piecewise_aggregate_approximation[371,]), 
                                                 t(y_location_piecewise_aggregate_approximation[371,]), 
                                                 t(z_location_piecewise_aggregate_approximation[371,]))
class8_piecewise_aggregate_approximation = class8_piecewise_aggregate_approximation[-1,]
scatterplot3d(class8_piecewise_aggregate_approximation, pch = 8, color = 1:9,
              main = "Class 8 PAA",
              xlim = c(min(class8_piecewise_aggregate_approximation), max(class8_piecewise_aggregate_approximation)),
              ylim = c(min(class8_piecewise_aggregate_approximation), max(class8_piecewise_aggregate_approximation)),
              zlim = c(min(class8_piecewise_aggregate_approximation), max(class8_piecewise_aggregate_approximation)),
              angle = 345, xlab = "X-Axis", ylab = "Y-Axis", zlab = "Z-Axis")

min(x_location_piecewise_aggregate_approximation)
max(x_location_piecewise_aggregate_approximation)
min(y_location_piecewise_aggregate_approximation)
max(y_location_piecewise_aggregate_approximation)
min(z_location_piecewise_aggregate_approximation)
max(z_location_piecewise_aggregate_approximation)

symbolic_aggregate_approximation = function(datatable)
{
  datatable2 = datatable
  for (i in 1:896) 
  {
    for (j in 2:10) 
    {
      if ((-30000)<=datatable[i,j] & datatable[i,j]<(-24000))
      {
        datatable2[i,j] = "a"
      }
      else if ((-24000)<=datatable[i,j] & datatable[i,j]<(-18000))
      {
        datatable2[i,j] = "b"
      }
      else if ((-18000)<=datatable[i,j] & datatable[i,j]<(-12000))
      {
        datatable2[i,j] = "c"
      }
      else if ((-12000)<=datatable[i,j] & datatable[i,j]<(-6000))
      {
        datatable2[i,j] = "d"
      }
      else if ((-6000)<=datatable[i,j] & datatable[i,j]<(0))
      {
        datatable2[i,j] = "e"
      }
      else if ((0)<=datatable[i,j] & datatable[i,j]<(6000))
      {
        datatable2[i,j] = "f"
      }
      else if ((6000)<=datatable[i,j] & datatable[i,j]<(12000))
      {
        datatable2[i,j] = "g"
      }
      else if ((12000)<=datatable[i,j] & datatable[i,j]<(18000))
      {
        datatable2[i,j] = "h"
      }
      else if ((18000)<=datatable[i,j] & datatable[i,j]<(24000))
      {
        datatable2[i,j] = "i"
      }
      else if ((24000)<=datatable[i,j] & datatable[i,j]<(30000))
      {
        datatable2[i,j] = "j"
      }
    }
  }
  return(datatable2)
}

x_location_symbolic_aggregate_approximation = symbolic_aggregate_approximation(x_location_piecewise_aggregate_approximation)
x_location_symbolic_aggregate_approximation

y_location_symbolic_aggregate_approximation = symbolic_aggregate_approximation(y_location_piecewise_aggregate_approximation)
y_location_symbolic_aggregate_approximation

z_location_symbolic_aggregate_approximation = symbolic_aggregate_approximation(z_location_piecewise_aggregate_approximation)
z_location_symbolic_aggregate_approximation










