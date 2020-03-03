
clear
load('DadesCeciliaMarc.mat');
C = who('-file','DadesCeciliaMarc.mat');
%%

for i=1:length(C)
  
    name = strcat('/Users/jloboprat/Google Drive/ARM TRACKER/Testing/METRICS/Data/',C{i},'.csv')
    csvwrite(name, eval(C{i}));

end