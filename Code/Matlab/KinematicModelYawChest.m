function [ShPos,ElbPos,HandPos] = KinematicModelYawChest(w2qw,wqs1,s1qs2,s2qs3,s3qs4,T0,T1,T2,T3,T4)
% Outputs the position estimates for the wrist and the elbow joints from
% the quaternion and joint starting position information

% INPUTS: 
% w2qw  - Quaternion relating offset corrected world & world
% wqs1  - Quaternion relating world & 1st segment
% s1qs2 - Quaternion relating 1st & 2nd segments (chest upperarm)
% s2qs3 - Quaternion relating 2nd & 3rd segments (upperarm forearm)
% s3qs4 - Quaternion relating 3rd & 4th segments (forearm hand)
% T1    - Translation vector for 1st coordinate system (shoulder)
% T2    - Translation vector for 2nd coordinate system (elbow)
% T4    - Translation vector for 3rd coordinate system (wrist)

% OUTPUTS:
% ShPos   - Positions of the shoulder joint
% ElbPos  - Positions of the elbow joint
% HandPos - positions of the wrist joint

% CODE:
    % Generate the Homogenous Transform matrices
    w2Hw = HomogenousTransform(w2qw,T0);
    wHs1 = HomogenousTransform(wqs1,T1);
    s1Hs2 = HomogenousTransform(s1qs2,T2);
    s2Hs3 = HomogenousTransform(s2qs3,T3);
    s3Hs4 = HomogenousTransform(s3qs4,T4);
    
    % Operate the Homogenous Matrices
    w2Hs1 = w2Hw*wHs1;                      % OffWorld to World
    w2Hs2 = w2Hw*wHs1*s1Hs2;                % World to Torso
    w2Hs3 = w2Hw*wHs1*s1Hs2*s2Hs3;          % Torso to Forearm
    w2Hs4 = w2Hw*wHs1*s1Hs2*s2Hs3*s3Hs4;    % Torso to Hand
    
    % Extract the positions of the elbow and the wrist
    ShPos = w2Hs2(1:3,4);
    ElbPos = w2Hs3(1:3,4);
    HandPos = w2Hs4(1:3,4);
end