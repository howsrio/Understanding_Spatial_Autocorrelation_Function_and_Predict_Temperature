#01. Matching Temperature and Colors
#영하 섭씨 20도 ~ 영상 섭씨 30도의 색을 미리 정해두고,
#각 계절의 최저기온과 최고기온을 입력받아 그 계절에 사용할 색의 팔레트를 반환해주는 함수
data_to_col=function(tem_min,tem_max){
  #colorRampPalette(RColorBrewer)함수를 이용
  #아래 여섯 개의 색 변화에 맞춰서 200개의 색 그라데이션을 만듦
  my_colors <-colorRampPalette(c('darkblue','blue','white','yellow','red','black'))(200)
  
  #영하 섭씨 20도가 숫자 0, 영상 섭씨 30도가 숫자 200에 대응되는 일차함수를 만들고,
  #각 계절의 최저기온, 최고기온을 그 일차함수에 대입하여 함숫값을 반환
  #이때 최저(최고)기온이 영하 섭씨 20도 보다 낮으면 함숫값은 0으로 고정
  #최고(최저)기온이 영상 섭씨 30도 보다 높으면 함숫값은 200으로 고정
  num=tem_min
  if (num < -20){
    min=0
  }
  else if (num > 30){
    min=200
  }
  else{
    min=((num+20)*4)
  }
  min=round(min, digits = 0)
  
  num=tem_max
  if (num < -20){
    max=0
  } 
  else if (num > 30){
    max=200
  }
  else{
    max=((num+20)*4)
  }
  max=round(max, digits = 0)
  
  #온도 map을 그릴 때 사용하기 위한 색 팔레트 반환
  return (my_colors[min:max])
}

#02. Covariance function(We are using Matern Covariance function)
#거리 Matrix와, Matern 함수의 세가지 parameter(Lecture 03 참고) alpha, beta, nu를 입력 받아,
#covariance 값을 반환해주는 함수(Matern Covariance function 기반)
covariance_func= function(dist_matrix,a,b,nu){
  #거리 Matrix를 입력 받아, Covariance 값을 계산
  x = dist_matrix
  c = besselK(x/b,nu)*a*((x/b)**nu)
  
  #이때, besselK 함수에서 x = 0 이면 NaN 결과 값을 배출
  #x = 0은 거리가 0인 것을 의미하므로, 0에 아주 가까운 값을 대입하여 대신 계산
  x = 1.0e-10
  c_0 = besselK(x/b,nu)*a*((x/b)**nu)
  #거리 Matrix의 대각원소의 값이 전부 0 이었으므로, 대각원소만 수정
  diag(c) = c_0
  
  #만들어진 Covariance Matrix를 반환
  return(c)
}

#03. Calculate log-likelihood
#거리 matrix, 편차 matrix, Matern function의 parameter를 입력 받고
#(편차 matrix는 온도 matrix에서 그것의 평균을 뺀 matrix)
#이를 통해 계산한 log-likelihood 값을 반환하는 함수
LogL=function(dist_matrix,dev_matrix,a,b,nu){

  covariance_matrix = covariance_func(dist_matrix,a,b,nu)  
  
  #아래 다섯 줄의 코드를 위에서 만든 covariance_func함수로 대체
  #x=dist_matrix
  #covariance_matrix=besselK(x/b,nu)*a*((x/b)**nu)
  
  #x=1.0e-100
  #c_0=besselK(x/b,nu)*a*((x/b)**nu)
  #diag(covariance_matrix) = c_0
  
  #Calculate determinant and inverse_matrix
  det=as.double(determinant(covariance_matrix)$modulus)
  inverse_matrix=solve(covariance_matrix)
  
  #Calculate log-likelihood
  LogL=-(det+t(dev_matrix)%*%inverse_matrix%*%(dev_matrix))/2-length(dev_matrix)*log(2*pi)/2
  LogL=as.numeric(LogL)
  return(LogL)
}
chordal_dsit=function(LongLat_matrix){
  #01. Calculate geodesic distance
  geodesic_matrix <- geodist(LongLat_matrix, measure = "geodesic")/1000 #거리 km단위로 변환
  
  #02. Convert geodesic distance into chordal distance(Lecture01 p16, p17)
  radius <- 6371 #지구 반지름의 평균값(단위: km)
  theta <- geodesic_matrix/radius
  chordal_matrix <- 2 * radius * sin(theta/2)
  return(chordal_matrix)
}

