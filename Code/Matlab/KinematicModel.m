function [ElbPos,HandPos] = KinematicModel(s1qs2,s2qs3,s3qs4,T1,T2,T3)
% Outputs the position estimates for the wrist and the elbow joints from
% the quaternion and joint starting position information

% INPUTS: 
% s1qs2 - Quaternion relating 1st & 2nd segments (chest upperarm)
% s2qs3 - Quaternion relating 2nd & 3rd segments (upperarm forearm)
% s3qs4 - Quaternion relating 3rd & 4th segments (forearm hand)
% T1    - Translation vector for 1st coordinate system (shoulder)
% T2    - Translation vector for 2nd coordinate system (elbow)
% T4    - Translation vector for 3rd coordinate system (wrist)

% OUTPUTS:
% ElbPos  - Positions of the elbow joint
% HandPos - positions of the wrist joint

% CODE:
    % Generate the Homogenous Transform matrices
    s1Hs2 = HomogenousTransform(s1qs2,T1);
    s2Hs3 = HomogenousTransform(s2qs3,T2);
    s3Hs4 = HomogenousTransform(s3qs4,T3); %I 
    
    % Operate the Homogenous Matrices
    s1Hs3 = s1Hs2*s2Hs3;          % Torso to Forearm
    s1Hs4 = s1Hs2*s2Hs3*s3Hs4;    % Torso to Hand
    
    % Extract the positions of the elbow and the wrist
    ElbPos = s1Hs3(1:3,4);
    HandPos = s1Hs4(1:3,4);
end