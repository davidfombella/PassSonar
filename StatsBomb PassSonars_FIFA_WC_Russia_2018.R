#PassSonar tutorial using StatsBomb free data

library(tidyverse)
library(viridis)
library(StatsBombR)

#load in pitch plot function
source('createPitchETM.R')

#load in Statsbomb data 
StatsBombData = StatsBombFreeEvents()

#filter to one competion and include only open play passes, and round pass angle
#to nearest x degrees
round.angle=15

# 43 Fifa WC 
FIFA.passes <- StatsBombData%>%filter(competition_id==43,
                                   type.name=="Pass",
                                   ! play_pattern.name %in% c("From Corner", "From Free Kick", "From Throw In"))%>%
                            mutate(angle.round=round(pass.angle*180/pi/round.angle)*round.angle)

#create data frame for sonars per player and team for a season
#this normalizes the most frequent pass angle to 1 and all other angles relative to that angle
#it also associates pass distance with each angle, but this can be changed to anything
#such as pass success or any other metric
sonar=FIFA.passes%>%
  group_by(player.name, team.name)%>%
  mutate(N=n())%>%
  ungroup()%>%
  group_by(player.name, team.name, angle.round)%>%
  mutate(n.angle=n()/N)%>%
  ungroup()%>%
  group_by(player.name, team.name)%>%
  mutate(maxN=max(n.angle),
         angle.norm=n.angle/maxN)%>%
  ungroup()%>%
  group_by(angle.round, player.name, team.name,N)%>%
  summarize(angle.norm=mean(angle.norm),
            distance=mean(pass.length),
            distance=ifelse(distance>30, 30,distance))


#plot sonar for a single player
#depending on the data source, you may have to change the start and direction
#for the polar coordinates
#hint: choose an outside back or midfielder to verify these parameters
players=unique(sonar$player.name)


# 71 pique

ggplot(sonar%>%filter(player.name == players[71]))+geom_bar(aes(x=angle.round, y=angle.norm, fill=distance), stat="identity")+
  scale_y_continuous(limits=c(0,1))+
  scale_x_continuous(breaks=seq(-180,180, by=90), limits=c(-180,180))+
  coord_polar(start=pi, direction=1)+
 # scale_fill_viridis("Distance (yards)", limits=c(0,30),option = "inferno", na.value="#FDE725FF")+
  scale_fill_viridis("Distance (yards)", limits=c(0,30), na.value="#FDE725FF")+
  labs(x='', y='',title= players[71])+
  theme_void()+
  theme(plot.title = element_text(hjust=0.5),
        #legend.position = "none", #uncomment to remove colorbar
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.background = element_rect(fill = "transparent",colour = NA))

#Plotting on top of a field by a team's formation
#The trick is to save each players PassSonar as a grob into a list. Then using
#annotation_custom() each PassSonar is placed in the correct position on the pitch
#It takes some trial and error to get the PassSonars into the correct position

text_color="black"
background_color="white"
radar.size=27
ymax=80
xmax=120

team.select="Spain"
match.select=7576

game.lineup=StatsBombData%>%filter(team.name==team.select, type.name=='Starting XI', match_id==match.select )
game.players=game.lineup$tactics.lineup[[1]][["player.name"]]
# team.formation= parse_number( game.lineup$tactics.formation)
team.formation=  game.lineup$tactics.formation
#game.lineup$tactics.lineup[[1]][["position.name"]]  #uncomment to view positions to help place into correct locations of field

player.plots=list()
for (i in 1:length(game.players)){
  
  plot.data=sonar%>%filter(team.name==team.select & player.name==game.players[i])
  
  player.plots[[i]]=ggplot(plot.data)+geom_bar(aes(x=angle.round, y=angle.norm, fill=distance), stat="identity")+
    scale_y_continuous(limits=c(0,1))+
    scale_x_continuous(breaks=seq(-180,180, by=90), limits=c(-180,180))+
    coord_polar(start=pi, direction=1)+
    scale_fill_viridis("Distance (yards)", limits=c(0,30), na.value="#FDE725FF")+
    labs(x='', y='',title=plot.data$player.name[1])+
    theme_void()+
    theme(plot.title = element_text(hjust=0.5, color=text_color),
          plot.background = element_rect(fill = "transparent",colour = NA),
          panel.background = element_rect(fill = "transparent",colour = NA),
          legend.position = "none")
  player.plots[[i]]=ggplotGrob(player.plots[[i]])
  
  if (i==length(game.players)){
    colorbar=
      ggplot(plot.data)+geom_bar(aes(x=angle.round, y=angle.norm, fill=distance), stat="identity")+
      scale_y_continuous(limits=c(0,0))+
      scale_fill_viridis("", limits=c(0,30), na.value="#FDE725FF")+
      labs(x='', y='')+
      theme_void()+
      theme( legend.position = "bottom",
             plot.background = element_rect(fill = "transparent",colour = NA),
             panel.background = element_rect(fill = "transparent",colour = NA))
    colorbar=ggplotGrob(colorbar)
  }
}


# new line added
text.color="black"
text_colour="black"


#this is a 4-4-2 example. Use similar methods for other formations

if (team.formation==433){
  team.formation='4-3-3'
  
  back.line=20
  mid.line=48
  forward.line=77
  p<-createPitch(grass_colour = background_color,goal_colour=text_colour, line_colour = text.color)+coord_flip(ylim=c(0,80))+
    theme(aspect.ratio = 120/80, plot.title = element_text(size=16, hjust=0.5, vjust=-2, color=text.color),
          plot.background = element_rect(fill = background_color,colour = NA),
          panel.background = element_rect(fill = background_color,colour = NA))+
    annotation_custom(grob=player.plots[[1]], xmin=-9, xmax=-9+radar.size, ymax=ymax/2+radar.size/2-1.5, y=ymax/2-radar.size/2-1.5)+ #GK
    annotation_custom(grob=player.plots[[2]], xmin=back.line+3, xmax=back.line+3+radar.size, ymax=ymax+1, y=ymax-radar.size+1)+ #RBACK NACHO
    annotation_custom(grob=player.plots[[5]], xmin=back.line+3, xmax=back.line+3+radar.size, ymax=-3+radar.size, y=-3)+ #LBACK ALBA
    annotation_custom(grob=player.plots[[4]], xmin=back.line, xmax=back.line+radar.size, ymax=ymax/2-23.5+radar.size, y=ymax/2-23.5)+ #LCENTRALBACK
    annotation_custom(grob=player.plots[[3]], xmin=back.line, xmax=back.line+radar.size, ymax=ymax/2-6+radar.size, y=ymax/2-6)+ #RCENTRALBACK PIQUE
    
    annotation_custom(grob=player.plots[[7]], xmin=mid.line, xmax=mid.line+radar.size, ymax=ymax-1, y=ymax-radar.size-1)+ #RCM KOKE
    annotation_custom(grob=player.plots[[6]], xmin=mid.line, xmax=mid.line+radar.size,ymax=ymax/2+radar.size/2-1.5, y=ymax/2-radar.size/2-1.5)+ #MCM BUSQUETS
    annotation_custom(grob=player.plots[[8]], xmin=mid.line, xmax=mid.line+radar.size, ymax=-2+radar.size, y=-2)+ #RM INIESTA
    
    annotation_custom(grob=player.plots[[10]], xmin=forward.line, xmax=forward.line+radar.size, ymax=-2+radar.size, y=-2)+ #LEFT FORWARD ISCO
    annotation_custom(grob=player.plots[[11]], xmin=forward.line, xmax=forward.line+radar.size,ymax=ymax/2+radar.size/2-1.5, y=ymax/2-radar.size/2-1.5)+ #CENTER FORWARD COSTA
    annotation_custom(grob=player.plots[[9]], xmin=forward.line, xmax=forward.line+radar.size,ymax=ymax-1,y=ymax-radar.size-1)+ #RIGHT FORWARD SILVA
    
    annotation_custom(grob=colorbar, xmin=3, xmax=7, ymin=1, ymax=18)+
    annotate("text", label="data:@StatsBomb", x=6, y=79, hjust=1,vjust=1 ,size=3.75, color=text.color)+
    annotate("text", label="Mean Pass Distance (Yards)", x=9, y=3, hjust=0, size=3, color=text.color)+
    annotate("text", label='Bar length = normalized pass angle frequency; Bar color = mean pass distance', color=text.color, x=-2, y=79, hjust=1, size=3)+
    annotate("text", label=paste0('Starting Formation: ', team.formation), color=text.color, x=-2, y=0, hjust=0, size=5, fontface="bold")+
    annotate("text", label=paste0('PassSonar: ', team.select), color=text.color, x=121.5, y=0, hjust=0, size=9, fontface="bold")+
    guides(fill = guide_colourbar())
  
}

ggsave(p, file=paste0('./', team.select,' PassSonar.png'), width=9.5, height=11.5, dpi=150, bg=background_color )
