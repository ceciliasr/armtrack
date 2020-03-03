function [RActivity,LActivity] = UnilateralActivity(Acc1,Acc2,AccRef,count)
% Unilateral Activity calculates the unilateral activity for both arms from
% the acceleration of the IMUs placed at the wrists. Acceleration values
% are used in sets of 50 samples (1 sec) and counts are defined by a
% threshold. At the final activity vector a moving average filter is
% applied.

% INPUTS:
% Acc1 - Acceleration of the rigth IMU
% Acc2 - Acceleration of the left IMU
% AccRef - Acceleration of the chest IMU
% Count - Count threshold

% OUTPUTS: 
% RActivity - Activity counts per second of the right arm
% LActivity - Activity counts per second of the left arm

    RActivity = floor(length(Acc1)/50);
    LActivity = floor(length(Acc1)/50);
    for i = 1:floor(length(Acc1)/50)
        if i == 1
            RActivity(i) = mean((Acc1(1:50)-AccRef(1:50)))/count;
            LActivity(i) = mean(Acc2(1:50)-AccRef(1:50))/count;
        else
            RActivity(i) = mean(Acc1((i-1)*50:i*50)-AccRef((i-1)*50:i*50))/count;
            LActivity(i) = mean(Acc2((i-1)*50:i*50)-AccRef((i-1)*50:i*50))/count;
        end
    end
   
    threshold = 0.1;
    RActivity(RActivity<threshold)= 0;
    RActivity = movmean(RActivity,[2 2]);
    LActivity(LActivity<threshold) = 0;
    LActivity = movmean(LActivity,[2 2]);
end
