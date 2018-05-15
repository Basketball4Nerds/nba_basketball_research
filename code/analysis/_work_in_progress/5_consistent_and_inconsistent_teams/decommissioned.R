



####
knicks2016 <- subset(games2016, team=='Knicks')
cavaliers2016 <- subset(games2016, team=='Cavaliers')

ggplot(knicks2016, aes(x=date, y=rqP)) + 
  geom_point(aes(color=site)) + 
  geom_line(group=1) + 
  theme(axis.text.x = element_text(angle=45, hjust=1)) + 
  scale_y_continuous(limits=c(70, 145)) + 
  ylab('regular-quarter points') + 
  ggtitle("New York Knicks 2016 Regular Season: Regular-Quarter Points")

ggplot(cavaliers2016, aes(x=date, y=p)) + 
  geom_point(aes(color=site)) + 
  geom_line(group=1) + 
  ylab('points') + 
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  scale_y_continuous(limits=c(70, 145)) + 
  ylab('regular-quarter points') + 
  ggtitle("Cleveland Cavaliers 2016 Regular Season: Regular-Quarter Points")




ggplot(celtics2016, aes(x=date, y=rqP)) + 
  geom_point(aes(color=outcome)) + 
  geom_line(group=1) + 
  ylab('points') + 
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  scale_y_continuous(limits=c(70, 145)) + 
  facet_grid(site ~ .)

ggplot(cavaliers2016, aes(x=date, y=rqP)) + 
  geom_point(aes(color=outcome)) + 
  geom_line(group=1) + 
  ylab('points') + 
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  scale_y_continuous(limits=c(70, 145)) + 
  facet_grid(site ~ .)



## find teams that do great at home but poorly at away
## find teams 


x <- mean(celtics2016$p[celtics2016$site=='home'])
y <- sd(celtics2016$p[celtics2016$site=='home'])

a <- mean(celtics2016$p[celtics2016$site=='away'])
b <- sd(celtics2016$p[celtics2016$site=='away'])