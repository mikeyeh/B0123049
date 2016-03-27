unique(NBA1415$Team)

FinalOutput<-NULL

for(team in unique(NBA1415$Team)){
  selectTeam<-subset(NBA1415,Team==team)
  FinalOutput<-rbind(FinalOutput,selectTeam
                     [order(selectTeam$TotalMinutesPlayed,decreasing = T)[1],
                                            c("Name","Team","TotalMinutesPlayed")])
}
FinalOutput