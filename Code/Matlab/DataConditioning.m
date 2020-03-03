function [Time,AccRFA,AccLFA,AccC] = DataConditioning(TimeStamp,AccRFA,AccLFA,AccC)
%This function generates constant frequency signals (time and accelerations) 
%from the data inputs after computing the modulus of the accelerations
%if needed

% INPUTS:
% TimeStamp - TimeStamp vector from the teensy (microcontroller)
% AccRFA - Acceleration data from the right forearm IMU
% AccLFA - Acceleration data from the left forearm IMU
% AccC   - Acceleration data from the chest IMU

% OUTPUTS:
% Time - Time vector from the timestamp resampled
% AccRFA - Acceleration data from the right forearm IMU resampled
% AccLFA - Acceleration data from the left forearm IMU resampled
% AccC   - Acceleration data from the chest IMU resampled

% CODE:
    LEN = isequal(length(TimeStamp),length(AccC));
    switch LEN
        case 0 
            AccRFA = sqrt(AccRFA(:,1).^2 + AccRFA(:,2).^2 + AccRFA(:,3).^2);
            AccLFA = sqrt(AccLFA(:,1).^2 + AccLFA(:,2).^2 + AccLFA(:,3).^2);
            AccC = sqrt(AccC(:,1).^2 + AccC(:,2).^2 + AccC(:,3).^2);
            Time = (TimeStamp-TimeStamp(1))/1000;       %Generating time from TimeStamp
            [AccRFA, Time] = resample(AccRFA,Time,50);  %Resampling Right Forearm Acceleration
            [AccLFA, ~] = resample(AccLFA,Time,50);     %Resampling Left Forearm Acceleration
            [AccC, ~] = resample(AccC,Time,50);         %Resampling Chest Acceleration
            AccRFA = [AccRFA ;AccRFA(end)];
            AccLFA = [AccLGA ;AccLFA(end)];
            AccC = [AccC ;AccC(end)];
            Time = [Time ;Time(end)+1/50];
        case 1
            Time = TimeStamp;
            AccRFA = AccRFA;
            AccLFA = AccLFA;
            AccC = AccC;
    end
end