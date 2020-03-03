function H = HomogenousTransform(quaternion,translation)
% Homogenous transformation matrix generation from quaternions and
% translation arrays.

% INPUTS:
% Quaternion - Relative rotations between segments
% Translation - The translation part of the Homogenous transform matrix.
% The position of the previous joint

% OUTPUTS:
% H - Homogenous matrix

%CODE:
   R = q2R(quaternion);
   H = [R,translation;zeros(1,3),1];
end
