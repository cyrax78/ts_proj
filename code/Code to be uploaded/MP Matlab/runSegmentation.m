% Script for running time series segmentation. Computes the matrix profile 
% and the matrix profile index and segments the time series. Returns all
% computed values for reference
% 
% function [crosscount, splitLoc, MatrixProfile, MPindex] = 
%                                      RunSegmentation(ts, slWindow)
% Input parameters:
% ts - time series to segment
% slWindow - sliding window size
%
% Output parameters:
% crosscount - number of crossings at each point
% splitLoc - split locations
% MatrixProfile - matrix profile
% MPindex - matrix profile index
function [crosscount] = ...
        runSegmentation(ts, slWindow )    
    [MatrixProfile, MPindex] = timeseriesSelfJoinFast(ts, slWindow);
    [crosscount] = segmentTimeSeries(MPindex);
    [crosscount] = normCrossCountAll(crosscount, slWindow);
   
