function [RightBilateralAct,LeftBilateralAct,RightUnilateralAct,LeftUnilateralAct] = BilateralArmActivity(RightArmActivity,LeftArmActivity)
% BilateralArmActivity generates 4 vectors depending on the activity of 
% each arm at every sample.

% INPUTS: 
% RightArmActivity - ACtivity vector for the right arm
% LeftArmActivity - Activity vector for the left arm

% OUTPUTS: 
% RightBilateralAct - Activity of the right arm when both arms are active
% LeftBilateralAct - Activity of the left arm when both arms are active
% RightUnilateralAct - Activity of the right arm when only right is active
% LeftUnilateralAct - Activity of the left arm whan only left is active

% CODE:
    RightBilateralAct = [];
    LeftBilateralAct = [];
    RightUnilateralAct =[];
    LeftUnilateralAct = [];
    for i = 1:length(RightArmActivity)
        if RightArmActivity(i)~= 0 && LeftArmActivity(i)~=0
            RightBilateralAct = [RightBilateralAct;RightArmActivity(i)];
            LeftBilateralAct = [LeftBilateralAct;LeftArmActivity(i)];
        elseif RightArmActivity(i)==0 && LeftArmActivity(i)~=0
            LeftUnilateralAct = [LeftUnilateralAct;LeftArmActivity(i)];
        elseif RightArmActivity(i)~=0 && LeftArmActivity(i)==0
            RightUnilateralAct = [RightUnilateralAct;RightArmActivity(i)];
        end  
    end
    
end