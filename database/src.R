
#load data
data=read.table('C:/Users/shaun/workspace/datamining/database/horse-colic.data',
    col.names=c('surgery?','Age','Hospital Number','rectal temperature','pulse',
                'respiratory rate','temperature of extremities','peripheral pulse',
                'mucous membranes','capillary refill time','pain','peristalsis',
                'abdominal distension','nasogastric tube','nasogastric reflux',
                'nasogastric reflux PH','rectal examination - feces','abdomen',
                'packed cell volume','total protein','abdominocentesis appearance',
                'abdomcentesis total protein','outcome','surgical lesion?',
                'type of lesion1','type of lesion2','type of lesion3','cp_data'
                ),
    na.string=c('?'),
    colClasses=c('factor','factor','factor','double','double','double','factor',
                 'factor','factor','factor','factor','factor','factor','factor',
                 'factor','double','factor','factor','double','double','factor',
                 'double','factor','factor','factor','factor','factor','factor'
                 )
    )
#����ժҪ
summary(data)

#����ֱ��ͼ
hist(data$'rectal.temperature')

#����qqͼ
library(car)
qqPlot(data$'rectal.temperature')

#���ƺ�ͼ
#ylabΪ����y����⣻
#rug�������Ʊ�����ʵ��ֵ��side=4��ʾ������ͼ���Ҳࣨ1���·���2����࣬3���Ϸ�����
#abline��������ˮƽ�ߣ�mean��ʾ��ֵ��na.rm=Tָ����ʱ������NAֵ��lty=2��������Ϊ���ߡ�
boxplot(data$'rectal.temperature',ylab='rectal temperature')
rug(data$'rectal.temperature',side=4)
abline(h=mean(data$'rectal.temperature',na.rm=T),lty=2)

#��ȱʧ�����޳�
omitdata = na.omit(data)
write.table(omitdata,'C:/Users/shaun/workspace/datamining/database/omitedData.data',
            col.names = F,row.names = F, quote = F)

#ʹ�����Ƶ��ֵ���ȱʧֵ
library(DMwR)
preprocess = data[-manyNAs(data),]
preprocess = centralImputation(data)
preprocess[is.na(data$'surgery.'),'surgery.']<-as.factor(names(which.max(table(data$'surgery.'))))
preprocess[is.na(data$'Age'),'Age']<-as.factor(names(which.max(table(data$'Age'))))
preprocess[is.na(data$'Hospital.Number'),'Hospital.Number']<-as.factor(names(which.max(table(data$'Hospital.Number'))))
preprocess[is.na(data$'rectal.temperature'),'rectal.temperature']<-as.double(names(which.max(table(data$'rectal.temperature'))))
preprocess[is.na(data$'pulse'),'pulse']<-as.double(names(which.max(table(data$'pulse'))))
preprocess[is.na(data$'respiratory.rate'),'respiratory.rate']<-as.double(names(which.max(table(data$'respiratory.rate'))))
preprocess[is.na(data$'temperature.of.extremities'),'temperature.of.extremities']<-as.factor(names(which.max(table(data$'temperature.of.extremities'))))
preprocess[is.na(data$'peripheral.pulse'),'peripheral.pulse']<-as.factor(names(which.max(table(data$'peripheral.pulse'))))
preprocess[is.na(data$'mucous.membranes'),'mucous.membranes']<-as.factor(names(which.max(table(data$'mucous.membranes'))))
preprocess[is.na(data$'capillary.refill.time'),'capillary.refill.time']<-as.factor(names(which.max(table(data$'capillary.refill.time'))))
preprocess[is.na(data$'pain'),'pain']<-as.factor(names(which.max(table(data$'pain'))))
preprocess[is.na(data$'peristalsis'),'peristalsis']<-as.factor(names(which.max(table(data$'peristalsis'))))
preprocess[is.na(data$'abdominal.distension'),'abdominal.distension']<-as.factor(names(which.max(table(data$'abdominal.distension'))))
preprocess[is.na(data$'nasogastric.tube'),'nasogastric.tube']<-as.factor(names(which.max(table(data$'nasogastric.tube'))))
preprocess[is.na(data$'nasogastric.reflux'),'nasogastric.reflux']<-as.factor(names(which.max(table(data$'nasogastric.reflux'))))

preprocess[is.na(data$'nasogastric.reflux.PH'),'nasogastric.reflux.PH']<-as.double(names(which.max(table(data$'nasogastric.reflux.PH'))))

preprocess[is.na(data$'rectal.examination...feces'),'rectal.examination...feces']<-as.factor(names(which.max(table(data$'rectal.examination...feces'))))
preprocess[is.na(data$'abdomen'),'abdomen']<-as.factor(names(which.max(table(data$'abdomen'))))

preprocess[is.na(data$'packed.cell.volume'),'packed.cell.volume']<-as.double(names(which.max(table(data$'packed.cell.volume'))))
preprocess[is.na(data$'total.protein'),'total.protein']<-as.double(names(which.max(table(data$'total.protein'))))

preprocess[is.na(data$'abdominocentesis.appearance'),'abdominocentesis.appearance']<-as.factor(names(which.max(table(data$'abdominocentesis.appearance'))))

preprocess[is.na(data$'abdomcentesis.total.protein'),'abdomcentesis.total.protein']<-as.double(names(which.max(table(data$'abdomcentesis.total.protein'))))

preprocess[is.na(data$'outcome'),'outcome']<-as.factor(names(which.max(table(data$'outcome'))))
preprocess[is.na(data$'surgical.lesion.'),'surgical.lesion.']<-as.factor(names(which.max(table(data$'surgical.lesion.'))))
preprocess[is.na(data$'type.of.lesion1'),'type.of.lesion1']<-as.factor(names(which.max(table(data$'type.of.lesion1'))))
preprocess[is.na(data$'type.of.lesion2'),'type.of.lesion2']<-as.factor(names(which.max(table(data$'type.of.lesion2'))))
preprocess[is.na(data$'type.of.lesion3'),'type.of.lesion3']<-as.factor(names(which.max(table(data$'type.of.lesion3'))))
preprocess[is.na(data$'cp_data'),'cp_data']<-as.factor(names(which.max(table(data$'cp_data'))))
write.table(preprocess,'C:/Users/shaun/workspace/datamining/database/maxFrequency.data',
            col.names = F,row.names = F, quote = F)

#ͨ�����Ե���ع�ϵ���ȱʧֵ
data=read.table('C:/Users/shaun/workspace/datamining/database/horse-colic.data',
                col.names=c('surgery?','Age','Hospital Number','rectal temperature','pulse',
                            'respiratory rate','temperature of extremities','peripheral pulse',
                            'mucous membranes','capillary refill time','pain','peristalsis',
                            'abdominal distension','nasogastric tube','nasogastric reflux',
                            'nasogastric reflux PH','rectal examination - feces','abdomen',
                            'packed cell volume','total protein','abdominocentesis appearance',
                            'abdomcentesis total protein','outcome','surgical lesion?',
                            'type of lesion1','type of lesion2','type of lesion3','cp_data'
                ),
                na.string=c('?'),
                colClasses=c('double','double','double','double','double','double','double',
                             'double','double','double','double','double','double','double',
                             'double','double','double','double','double','double','double',
                             'double','double','double','double','double','double','double'
                )
                
)
preprocess2 = data[-manyNAs(data),]
symnum(cor(preprocess2[,4:28],use='complete.obs'))
lm(formula=pulse~rectal.temperature, data=preprocess2)
fillpulse <- function(rectal.temperature){
  if(is.na(rectal.temperature))
    return(NA)
  else return (-112.790 + 4.795 * rectal.temperature)
}
preprocess2[is.na(preprocess2$pulse),'pulse'] <- sapply(preprocess2[is.na(preprocess2$rectal.pulse),'rectal.temperature'],fillpulse)
write.table(preprocess2,'C:/Users/shaun/workspace/datamining/database/linearDefaultData.txt',
            col.names = F,row.names = F, quote = F)

#ͨ�����ݶ���֮������������ȱʧֵ
preprocess3 = data[-manyNAs(data),]
preprocess3 = knnImputation(preprocess3,k =5, scale = T, meth = "weighAvg", distData = NULL)
write.table(preprocess3,'C:/Users/shaun/workspace/datamining/database/knnImputationData.data',
            col.names = F,row.names = F, quote = F)