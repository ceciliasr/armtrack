function euler = eulXZY(q)
% Euler angle computation in XYZ configuration

% INPUTS:
% q - quaternion 

% OUTPUTS: 
% euler - array of euler angles [Roll, Pitch, Yaw]

% CODE:
    R = q2R(q);
    if R(1,2) < 1
        if R(1,2)>-1
            Yaw = asin(-R(1,2));
            Roll = atan2(R(3,2),R(2,2));
            Pitch = atan2(R(1,3),R(1,1));         
        else
            Yaw = pi/2;
            Roll = -atan2(-R(3,1),R(3,3));
            Pitch = 0;
        end
    else
        Yaw = -pi/2;
        Roll = atan2(-R(3,1),R(3,3));
        Pitch = 0;
    end
    Roll = rad2deg(Roll);
    Yaw = rad2deg(Yaw);
    Pitch = rad2deg(Pitch);
    
    euler = [Roll, Pitch, Yaw];
end

