library(plyr)
library(ggplot2)

# Custom Rounding
mround <- function(x,base){
  base*round(x/base)
}

# Load Data 

bbm_iv_rd1 <- read.csv("~/Xavier_Backup/NFL/Best Ball/BBM_Stacking_Analysis/bbm_iv_rd1.csv", stringsAsFactors=TRUE)
bbm_iv_rd2 <- read.csv("~/Xavier_Backup/NFL/Best Ball/BBM_Stacking_Analysis/bbm_iv_rd2.csv", stringsAsFactors=TRUE)
bbm_iv_rd3 <- read.csv("~/Xavier_Backup/NFL/Best Ball/BBM_Stacking_Analysis/bbm_iv_rd3.csv", stringsAsFactors=TRUE)
bbm_iv_rd4 <- read.csv("~/Xavier_Backup/NFL/Best Ball/BBM_Stacking_Analysis/bbm_iv_rd4.csv", stringsAsFactors=TRUE)

bbm_iv_rd2_results <- bbm_iv_rd2[,c("tournament_entry_id", "player_id", "made_playoffs")]
bbm_iv_rd3_results <- bbm_iv_rd3[,c("tournament_entry_id", "player_id", "made_playoffs")]
bbm_iv_rd4_results <- bbm_iv_rd4[,c("tournament_entry_id", "player_id", "made_playoffs", "roster_points")]

bbm_iv_full <- merge(bbm_iv_rd1, bbm_iv_rd2_results, by = c("tournament_entry_id", "player_id"), all.x = TRUE)
bbm_iv_full <- merge(bbm_iv_full, bbm_iv_rd3_results, by = c("tournament_entry_id", "player_id"), all.x = TRUE)
bbm_iv_full <- merge(bbm_iv_full, bbm_iv_rd4_results, by = c("tournament_entry_id", "player_id"), all.x = TRUE)

colnames(bbm_iv_full) <- c(colnames(bbm_iv_full)[1:22], "regular.season.points", "reg.zeroes", "made.round.2", "made.round.3", "made.round.4", "final.points")
bbm_iv_full$made.round.2[is.na(bbm_iv_full$made.round.2)] <- 0
bbm_iv_full$made.round.3[is.na(bbm_iv_full$made.round.3)] <- 0
bbm_iv_full$made.round.4[is.na(bbm_iv_full$made.round.4)] <- 0
bbm_iv_full$projection_adp[is.na(bbm_iv_full$projection_adp)] <- 216

full.adp <- ddply(bbm_iv_full, .(player_name), summarize, adp.mean=mean(projection_adp))
full.adp <- arrange(full.adp, adp.mean)

bbm_iv_full <- bbm_iv_full[order(bbm_iv_full$player_name),]

# Players and ADP display Range

player.one <- "Josh Allen"
player.two <- "Khalil Shakir"
adp.diff.range <- -30:30
round.granularity <- 5

mean.one <- full.adp[full.adp$player_name == player.one,]$adp.mean[1]
mean.two <- full.adp[full.adp$player_name == player.two,]$adp.mean[1]

player.one.data <- bbm_iv_full[bbm_iv_full$player_name == player.one,]
player.two.data <- bbm_iv_full[bbm_iv_full$player_name == player.two,]

# Calculate ADP difference for each time players are selected

player.one.data$adp.diff <- player.one.data$overall_pick_number-player.one.data$projection_adp
player.one.data$full.adp.diff <- player.one.data$overall_pick_number - mean.one

player.two.data$adp.diff <- player.two.data$overall_pick_number-player.two.data$projection_adp
player.two.data$full.adp.diff <- player.two.data$overall_pick_number - mean.two

player.two.join <- player.two.data[,c("tournament_entry_id", "player_name", "overall_pick_number", "pick_points", "made.round.2", "made.round.3", "made.round.4", "final.points", "adp.diff", "full.adp.diff")]
colnames(player.two.join) <- c("tournament_entry_id", "P2.player.name", "P2.Pick.Number", "P2.Pick.Points", "P2.R2", "P2.R3", "P2.R4", "P2.Final.Points", "P2.adp.diff", "P2.full.adp.diff")

merged.players <- merge(player.one.data, player.two.join, by = "tournament_entry_id", all.x = TRUE, all.y = TRUE)

## Stacked Teams, Player One Only Teams, and Player Two Only Teams
stacked.teams <- merged.players[!is.na(merged.players$player_name) & !is.na(merged.players$P2.player.name),]
player.one.only <- merged.players[!is.na(merged.players$player_name) & is.na(merged.players$P2.player.name),]
player.two.only <- merged.players[is.na(merged.players$player_name) & !is.na(merged.players$P2.player.name),]

## Calculate Stack Team Buckets
stacked.teams$total.adp.diff <- mround(stacked.teams$adp.diff + stacked.teams$P2.adp.diff, round.granularity)
stacked.teams$total.full.adp.diff <- mround(stacked.teams$full.adp.diff + stacked.teams$P2.full.adp.diff, round.granularity)

base.table <- table(stacked.teams$total.adp.diff, stacked.teams$made.round.2)
stacked.sample.size <- base.table[,1]+base.table[,2]
r2.table.stacked <- prop.table(base.table, margin = 1)
x.axis <- as.numeric(row.names(r2.table.stacked))
y.axis <- r2.table.stacked[,2]

stacked.plot.data <- data.frame(x.axis, y.axis, stacked.sample.size, rep("Both", length(x.axis)))
colnames(stacked.plot.data) <- c("ADP.Diff", "Perc.R2", "Sample.Size", "Stack.Players")
stacked.plot.data <- stacked.plot.data[stacked.plot.data$ADP.Diff %in% adp.diff.range, ]

ggplot(stacked.plot.data, aes(x=ADP.Diff, y=Perc.R2, label = Sample.Size)) + geom_point() + geom_text(hjust=0, vjust=-1)

## Calculate Player One Only Buckets

base.one <- table(mround(player.one.only$adp.diff, round.granularity), player.one.only$made.round.2) 
one.sample.size <- base.one[,1]+base.one[,2]
r2.table.one <- prop.table(base.one, margin = 1)
x.axis <- as.numeric(row.names(r2.table.one))
y.axis <- r2.table.one[,2]

one.plot.data <- data.frame(x.axis, y.axis, one.sample.size, rep(player.one, length(x.axis)))
colnames(one.plot.data) <- c("ADP.Diff", "Perc.R2", "Sample.Size", "Stack.Players")
one.plot.data <- one.plot.data[one.plot.data$ADP.Diff %in% adp.diff.range, ]

ggplot(one.plot.data, aes(x=ADP.Diff, y=Perc.R2, label = Sample.Size)) + geom_point() + geom_text(hjust=0, vjust=-1)

## Calculate Player Two Only Buckets

base.two <- table(mround(player.two.only$P2.adp.diff, round.granularity), player.two.only$P2.R2) 
two.sample.size <- base.two[,1]+base.two[,2]
r2.table.two <- prop.table(base.two, margin = 1)
x.axis <- as.numeric(row.names(r2.table.two))
y.axis <- r2.table.two[,2]

two.plot.data <- data.frame(x.axis, y.axis, two.sample.size, rep(player.two, length(x.axis)))
colnames(two.plot.data) <- c("ADP.Diff", "Perc.R2", "Sample.Size", "Stack.Players")
two.plot.data <- two.plot.data[two.plot.data$ADP.Diff %in% adp.diff.range, ]

ggplot(two.plot.data, aes(x=ADP.Diff, y=Perc.R2, label = Sample.Size)) + geom_point() + geom_text(hjust=0, vjust=-1)

## Combine All Buckets for Graph

all.r2.data <- rbind(stacked.plot.data, one.plot.data, two.plot.data)

ggplot(all.r2.data, aes(x=ADP.Diff, y=Perc.R2, label = Sample.Size)) + geom_line(aes(color = Stack.Players)) + geom_point(aes(color = Stack.Players)) + geom_text(hjust=0, vjust=-1)

#### Round 3 ####

## Stacked

base.table <- table(stacked.teams$total.adp.diff, stacked.teams$made.round.3)
stacked.sample.size <- base.table[,1]+base.table[,2]
r3.table.stacked <- prop.table(base.table, margin = 1)
x.axis <- as.numeric(row.names(r3.table.stacked))
y.axis <- r3.table.stacked[,2]

stacked.plot.data <- data.frame(x.axis, y.axis, stacked.sample.size, rep("Both", length(x.axis)))
colnames(stacked.plot.data) <- c("ADP.Diff", "Perc.R3", "Sample.Size", "Stack.Players")
stacked.plot.data <- stacked.plot.data[stacked.plot.data$ADP.Diff %in% adp.diff.range, ]

## Calculate Player One Only Buckets

base.one <- table(mround(player.one.only$adp.diff, round.granularity), player.one.only$made.round.3) 
one.sample.size <- base.one[,1]+base.one[,2]
r3.table.one <- prop.table(base.one, margin = 1)
x.axis <- as.numeric(row.names(r3.table.one))
y.axis <- r3.table.one[,2]

one.plot.data <- data.frame(x.axis, y.axis, one.sample.size, rep(player.one, length(x.axis)))
colnames(one.plot.data) <- c("ADP.Diff", "Perc.R3", "Sample.Size", "Stack.Players")
one.plot.data <- one.plot.data[one.plot.data$ADP.Diff %in% adp.diff.range, ]

## Calculate Player Two Only Buckets

base.two <- table(mround(player.two.only$P2.adp.diff, round.granularity), player.two.only$P2.R3) 
two.sample.size <- base.two[,1]+base.two[,2]
r3.table.two <- prop.table(base.two, margin = 1)
x.axis <- as.numeric(row.names(r3.table.two))
y.axis <- r3.table.two[,2]

two.plot.data <- data.frame(x.axis, y.axis, two.sample.size, rep(player.two, length(x.axis)))
colnames(two.plot.data) <- c("ADP.Diff", "Perc.R3", "Sample.Size", "Stack.Players")
two.plot.data <- two.plot.data[two.plot.data$ADP.Diff %in% adp.diff.range, ]

## Combine All Buckets for Graph

all.r3.data <- rbind(stacked.plot.data, one.plot.data, two.plot.data)

ggplot(all.r3.data, aes(x=ADP.Diff, y=Perc.R3, label = Sample.Size)) + geom_line(aes(color = Stack.Players)) + geom_point(aes(color = Stack.Players)) + geom_text(hjust=0, vjust=-1)

#### Round 4 ####

## Stacked

base.table <- table(stacked.teams$total.adp.diff, stacked.teams$made.round.4)
stacked.sample.size <- base.table[,1]+base.table[,2]
r4.table.stacked <- prop.table(base.table, margin = 1)
x.axis <- as.numeric(row.names(r4.table.stacked))
y.axis <- r4.table.stacked[,2]

stacked.plot.data <- data.frame(x.axis, y.axis, stacked.sample.size, rep("Both", length(x.axis)))
colnames(stacked.plot.data) <- c("ADP.Diff", "Perc.R4", "Sample.Size", "Stack.Players")
stacked.plot.data <- stacked.plot.data[stacked.plot.data$ADP.Diff %in% adp.diff.range, ]

## Calculate Player One Only Buckets

base.one <- table(mround(player.one.only$adp.diff, round.granularity), player.one.only$made.round.4) 
one.sample.size <- base.one[,1]+base.one[,2]
r4.table.one <- prop.table(base.one, margin = 1)
x.axis <- as.numeric(row.names(r4.table.one))
y.axis <- r4.table.one[,2]

one.plot.data <- data.frame(x.axis, y.axis, one.sample.size, rep(player.one, length(x.axis)))
colnames(one.plot.data) <- c("ADP.Diff", "Perc.R4", "Sample.Size", "Stack.Players")
one.plot.data <- one.plot.data[one.plot.data$ADP.Diff %in% adp.diff.range, ]

## Calculate Player Two Only Buckets

base.two <- table(mround(player.two.only$P2.adp.diff, round.granularity), player.two.only$P2.R4) 
two.sample.size <- base.two[,1]+base.two[,2]
r4.table.two <- prop.table(base.two, margin = 1)
x.axis <- as.numeric(row.names(r4.table.two))
y.axis <- r4.table.two[,2]

two.plot.data <- data.frame(x.axis, y.axis, two.sample.size, rep(player.two, length(x.axis)))
colnames(two.plot.data) <- c("ADP.Diff", "Perc.R4", "Sample.Size", "Stack.Players")
two.plot.data <- two.plot.data[two.plot.data$ADP.Diff %in% adp.diff.range, ]

## Combine All Buckets for Graph

all.r4.data <- rbind(stacked.plot.data, one.plot.data, two.plot.data)

ggplot(all.r4.data, aes(x=ADP.Diff, y=Perc.R4, label = Sample.Size)) + geom_line(aes(color = Stack.Players)) + geom_point(aes(color = Stack.Players)) + geom_text(hjust=0, vjust=-1)

#### Round 4 Points ####

stacked.teams.r4 <- stacked.teams[stacked.teams$made.round.4 == 1, ] 
player.one.only.r4 <- player.one.only[player.one.only$made.round.4 == 1, ]
player.two.only.r4 <- player.two.only[player.two.only$P2.R4 == 1, ]

stacked.teams.r4$total.adp.diff <- mround(stacked.teams.r4$adp.diff + stacked.teams.r4$P2.adp.diff, round.granularity)
player.one.only.r4$total.adp.diff <- mround(player.one.only.r4$adp.diff, round.granularity)
player.two.only.r4$total.adp.diff <- mround(player.two.only.r4$P2.adp.diff, round.granularity)

stack.sample <- as.vector(table(stacked.teams.r4$total.adp.diff))
p1.sample <- as.vector(table(player.one.only.r4$total.adp.diff))
p2.sample <- as.vector(table(player.two.only.r4$total.adp.diff))

stack.points <- ddply(stacked.teams.r4, .(total.adp.diff), summarize, Week.17.Points=mean(final.points))
stack.points.df <- data.frame(stack.points$total.adp.diff, stack.points$Week.17.Points, rep("Both", nrow(stack.points)), stack.sample)
colnames(stack.points.df) <- c("ADP.Diff", "Avg.Points", "Stack.Players", "Sample.Size")
                      
p1.points <- ddply(player.one.only.r4, .(total.adp.diff), summarize, Week.17.Points=mean(final.points))
p1.points.df <- data.frame(p1.points$total.adp.diff, p1.points$Week.17.Points, rep(player.one, nrow(p1.points)), p1.sample)
colnames(p1.points.df) <- c("ADP.Diff", "Avg.Points", "Stack.Players", "Sample.Size")

p2.points <- ddply(player.two.only.r4, .(total.adp.diff), summarize, Week.17.Points=mean(P2.Final.Points))
p2.points.df <- data.frame(p2.points$total.adp.diff, p2.points$Week.17.Points, rep(player.two, nrow(p2.points)), p2.sample)
colnames(p2.points.df) <- c("ADP.Diff", "Avg.Points", "Stack.Players", "Sample.Size")

all.r4.point.data <- rbind(stack.points.df, p1.points.df, p2.points.df)

ggplot(all.r4.point.data, aes(x=ADP.Diff, y=Avg.Points, label = Sample.Size)) + geom_line(aes(color = Stack.Players)) + geom_point(aes(color = Stack.Players)) + geom_text(hjust=0, vjust=-1)

