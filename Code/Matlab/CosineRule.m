function Angle = CosineRule(u,v)
% Cosine Rule applies the cosine rule to 2 vectors calculating the angle
% between both vectors

% INPUTS: 2 vectors

% OUTPUTS: Angle between the vectors

%CODE: 
    Angle = acosd((u*v')/(norm(u).*norm(v)));
end