for(iin1:nrow(NBA1415)){
  
  if(NBA1415[i,"GamesPlayed"]>70&NBA1415[i,"TotalPoints"]>1500){
    
    print(NBA1415[i,c("Name","Team","Position")])
    
  }
  
}

#剛剛的問題:取出打超過70場,總得分超過1500分的球員姓名、所屬隊伍、守備位置。