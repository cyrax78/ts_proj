%%  read data
LAST_PRICE = csvread('/Users/dianas/Documents/GWU Study/Advanced Time Series/Project/Data/tcehy.csv',1,4,[1,4,51328,4]);
%TIME = csvread('/Users/dianas/Documents/GWU Study/Advanced Time Series/Project/Data/tcehy.csv',1,0,[1,0,51328,0]);
% for i=4:1:60
% %%  matrixprofile
%     SubLen=i
%     [matrixProfile, profileIndex, motifIndex, discordIndex] = interactiveMatrixProfileVer2(LAST_PRICE,SubLen);
%     filename = sprintf('MP_output%d.csv',i);
%     csvwrite(filename,matrixProfile)
% end
%[matrixProfile, profileIndex, motifIndex, discordIndex] = interactiveMatrixProfileVer2(smooth(LAST_PRICE,10),SubLen);
%%  segmentation
%Date = csvread('/Users/dianas/Documents/GWU Study/Advanced Time Series/Project/Data/tcehy.csv',1,0,[1,0,51328,0]);
CAC_5 = runSegmentation(LAST_PRICE, 5);
argmin_5 = find(CAC_5==min(CAC_5));
CAC_30 = runSegmentation(LAST_PRICE, 30);
argmin_30 = find(CAC_30==min(CAC_30));
CAC_60 = runSegmentation(LAST_PRICE, 60);
argmin_60 = find(CAC_60==min(CAC_60));
figure
hold on
title('Corrected Arc-Curve of Last Price of Tencent');
plot(CAC_5,'DisplayName','CAC_5');
plot(CAC_30,'DisplayName','CAC_30');
plot(CAC_60,'DisplayName','CAC_60');
plot(LAST_PRICE/max(LAST_PRICE),'DisplayName','Scaled Last Price');
hold off
%%MP
    SubLen=13
    [matrixProfile, profileIndex, motifIndex, discordIndex] = interactiveMatrixProfileVer2(LAST_PRICE,SubLen);