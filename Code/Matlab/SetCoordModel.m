function [Landmarks,Points] = SetCoordModel(Landmarks,Points,alpha)
% Rotates arround the Z axis the points of the inputs (conditioning) for the
% models to be facing [1 0 0] direction

% INPUTS:
% Landmarks - Points of the kinematical model, anatomicaly important to
% represent a simple model of the human body
% Points    - Points of the mesh of a human 3D model
% alpha     - Angle of rotation of the points
 
% OUTPUTS:
% Landmarks - Points of the kinematical model, anatomicaly important to
% represent a simple model of the human body facing [1 0 0] direction
% Points    - Points of the mesh of a human 3D model facing [1 0 0] direction

% CODE:
    rotmat = [cosd(alpha) -sind(alpha) 0; sind(alpha) cosd(alpha) 0; 0 0 1];
    for i = 1:size(Points)
        Points(i,:) = (rotmat*Points(i,:)')';
    end
    for i = 1:size(Landmarks,2)
        Landmarks(:,i) = rotmat*Landmarks(:,i);
    end
end