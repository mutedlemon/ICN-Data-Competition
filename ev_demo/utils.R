pmedian=function(num, ev.num, pop, building, ev.location, dtframe){
  #num:선정개수 입력
  #ev.num:전기차 등록대수
  #pop:인구밀도
  #building:건물정보
  #ev.location:충전소위치
  #dtframe: 위경도와 변수 네 가지가 모두 수치형으로 나와있는 데이터프레임
  require(dplyr)
  require(geosphere)
  require(tbart)
  require(sp)
  
  dtframe=dtframe %>% rename(lat=위도, lng=경도)
  dtframe_idx=dtframe
  dtframe=dtframe %>% select(-name)
  dtframe$weight=ev.location*dtframe$X500m충전소 + pop*dtframe$주민등록세대 + ev.num*dtframe$전기차등록수 + building*dtframe$X500m생활근린시설
  
  coordinates(dtframe)<- ~lng+lat
  # 주차장간의 거리 계산
  eucdist=euc.dists(dtframe, dtframe)
  len=dim(dtframe)[1]
  dist=matrix(0, len, len)
  for(i in 1:len){
    dist[i,]=dtframe$weight[i]*eucdist[i,]
  }
  allocations(dtframe, metric = dist, p=num, verbose = T)->result_t
  print('<P-median 입지선정 결과>')
  print(dtframe_idx[unique(result_t$allocation), 1:3])
  return(dtframe_idx[unique(result_t$allocation), 1:3])
}

require(ggmap)
require(geosphere)
require(dplyr)
require(tbart)
require(sp)


simpleMCLP2 <- function(candidate, car_reg, pop, building, charger_location, dtframe){
  
  max_index = 0
  select_index <- rep(NA, candidate)  # 선정된 입지의 인덱스 모음
  
  dtframe$wei = charger_location * dtframe$X500m충전소 + 
    pop * dtframe$주민등록세대 + 
    car_reg * dtframe$전기차등록수 + 
    building * dtframe$X500m생활근린시설
  
  for(i in 1:candidate){
    max_index = which.max(dtframe[ , 'wei'])
    select_index[i] = max_index
    
    for(j in 1:nrow(dtframe)){
      if(distGeo(c(dtframe[j, '경도'], dtframe[j, '위도']), 
                 c(dtframe[max_index, '경도'], 
                   dtframe[max_index, '위도'])) 
         <  500){
        
        dtframe[j, 'wei'] = 0  # 거리 이내 후보지의 점수를 0으로
      }
    }
  }
  
  result_MCLP <- dtframe[select_index, ]
  
  result_MCLP <- result_MCLP[ , c("name", 
                                  "위도", "경도")]
  
  names(result_MCLP) <- c("name", "lat", "lng")
  
  options(pillar.sigfig = 7)
  print(result_MCLP)
  return(result_MCLP)
}


