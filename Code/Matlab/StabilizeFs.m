function [RShPos,RElbPos,RHandPos] = StabilizeFs(Time,ShPos,ElbPos,HandPos,Fs)
% StabilizeFs resamples signals that do not show contant sampling
% frequency, using the time vector from the timestamp and the desired
% sampling frequency.

% INPUTS: Time - Time vector of the signal
%         ShPos - Position of the Shoulder Joint
%         Elbow - Position of the Elbow Joint
%         Hand - Position of the Hand Joint

% OUTPUTS:
RShPos = resample(ShPos,Time,Fs);
RElbPos = resample(ElbPos,Time,Fs);
RHandPos = resample(HandPos,Time,Fs);

end