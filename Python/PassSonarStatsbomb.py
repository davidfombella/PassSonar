import json
import pandas as pd
from pandas.io.json import json_normalize
import matplotlib.pyplot as plt
import numpy as np
from mpl_toolkits.axes_grid1.inset_locator import inset_axes
from matplotlib.projections import get_projection_class
from matplotlib.patches import Arc


pd.set_option("display.max_columns", 110)


#########
with open (r"22921.json", "r") as f:
	obj = json.load(f)


df = json_normalize(obj)

############
player_dict = {}
klist = []
for player in df.iloc[0,103]:
	p = player["player"]
	name = p["name"]
	klist.append(name)
xlist = [0,42,42,42,42,62,62,85,85,97,97]
ylist = [45,15,30,55,75,30,60,30,55,22,68]

for x,y,z in zip(xlist, ylist, klist):
    entry = {z:[x,y]}
    player_dict.update(entry)
    
########

def Passer(player):
    local_df = df.copy(deep=True)
    local_df = local_df[local_df["type.name"]=="Pass"]
    local_df = local_df[local_df["player.name"]==player]
    local_df = local_df.dropna(axis=1, how="all")

    df1 = local_df[['pass.angle','pass.length']].copy()

    bins = np.linspace(-np.pi,np.pi,20)

    df1['binned'] = pd.cut(local_df['pass.angle'], bins, include_lowest=True, right = True)
    df1["Bin_Mids"] = df1["binned"].apply(lambda x: x.mid)
    df1 = df1[:-1]

    A= df1.groupby("Bin_Mids", as_index=False)["pass.length"].mean()
    A = A.dropna(0)

    return A



fig, ax = plt.subplots()


def plot_inset(width, axis_main, data, x,y):
    ax_sub= inset_axes(axis_main, width=width, height=width, loc=10, 
                       bbox_to_anchor=(x,y),
                       bbox_transform=axis_main.transData, 
                       borderpad=0.0, axes_class=get_projection_class("polar"))

    theta = data["Bin_Mids"]
    radii = data["pass.length"]
    bars = ax_sub.bar(theta, radii, width=0.3, bottom=0.0)
    ax_sub.set_xticklabels([])
    ax_sub.set_yticks([])
    ax_sub.yaxis.grid(False)
    ax_sub.xaxis.grid(False)
    ax_sub.spines['polar'].set_visible(False)
    for r, bar in zip(theta, bars):
        bar.set_facecolor(plt.cm.viridis(r))
        bar.set_alpha(0.5)


for player, loc in player_dict.items():
    plot_inset(1.1,ax, data = Passer(player), x = loc[0], y = loc[1])
    ax.text(loc[0]+10, loc[1], player, size = 5, rotation = -90)

#plot invisible scatter plot for the axes to autoscale
ax.scatter(xlist, ylist, s=1, alpha=0.0)




##############

ax.plot([0,0],[0,90], color="black")
ax.plot([0,130],[90,90], color="black")
ax.plot([130,130],[90,0], color="black")
ax.plot([130,0],[0,0], color="black")
ax.plot([65,65],[0,90], color="black")
    
#Left Penalty Area
ax.plot([16.5,16.5],[65,25],color="black")
ax.plot([0,16.5],[65,65],color="black")
ax.plot([16.5,0],[25,25],color="black")
    
    #Right Penalty Area
ax.plot([130,113.5],[65,65],color="black")
ax.plot([113.5,113.5],[65,25],color="black")
ax.plot([113.5,130],[25,25],color="black")
    
    #Left 6-yard Box
ax.plot([0,5.5],[54,54],color="black")
ax.plot([5.5,5.5],[54,36],color="black")
ax.plot([5.5,0.5],[36,36],color="black")
    
    #Right 6-yard Box
ax.plot([130,124.5],[54,54],color="black")
ax.plot([124.5,124.5],[54,36],color="black")
ax.plot([124.5,130],[36,36],color="black")
    
    #Prepare Circles
centreCircle = plt.Circle((65,45),9.15,color="black",fill=False)
centreSpot = plt.Circle((65,45),0.8,color="black")
leftPenSpot = plt.Circle((11,45),0.8,color="black")
rightPenSpot = plt.Circle((119,45),0.8,color="black")
    
    #Draw Circles
ax.add_patch(centreCircle)
ax.add_patch(centreSpot)
ax.add_patch(leftPenSpot)
ax.add_patch(rightPenSpot)
    
    #Prepare Arcs
leftArc = Arc((11,45),height=18.3,width=18.3,angle=0,theta1=310,theta2=50,color="black")
rightArc = Arc((119,45),height=18.3,width=18.3,angle=0,theta1=130,theta2=230,color="black")

#Goals

ax.plot([-3,0],[41.35,41.35],color="black")
ax.plot([-3,-3],[41.35,48.65],color="black")
ax.plot([-3,0],[48.65,48.65],color="black")

ax.plot([133,130],[41.35,41.35],color="black")
ax.plot([133,133],[41.35,48.65],color="black")
ax.plot([133,130],[48.65,48.65],color="black")

    #Draw Arcs
ax.add_patch(leftArc)
ax.add_patch(rightArc)



    #Tidy Axes
ax.axis('off')
ax.text(135, 42, "PASS SONAR: FRANCE", rotation = -90, fontweight = "bold", fontsize = 12)
ax.text(132, 59, "vs South Korea", rotation = -90, fontweight = "bold", fontsize = 7)
plt.show()
    

