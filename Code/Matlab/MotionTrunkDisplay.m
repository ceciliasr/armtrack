%% Motion display with chest
% This script shows the upper limb joint positions toghether with the
% tilting and orientation of the trunk segment.
addpath(genpath('slamToolbox'),'Models',genpath('Data'))
load VictorModel.mat
load VictorLandmarks.mat
uiopen('*.mat')
