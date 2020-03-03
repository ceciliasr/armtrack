function euler = eulZXY(q)
% Euler angle computation in ZXY configuration

% INPUTS:
% q - quaternion 

% OUTPUTS: 
% euler - array of euler angles [Roll, Pitch, Yaw]

% CODE:
    R = q2R(q);
    if R(3,2) < 1
        if R(3,2)>-1
            Roll = asin(R(3,2));
            Yaw = atan2(-R(1,2),R(2,2));
            Pitch = atan2(-R(3,1),R(3,3));         
        else
            Roll = -pi/2;
            Yaw = atan2(-R(1,3),R(1,1));
            Pitch = 0;
        end
    else
        Roll = pi/2;
        Yaw = atan2(-R(1,3),R(1,1));
        Pitch = 0;
    end
    Roll = rad2deg(Roll);
    Yaw = rad2deg(Yaw);
    Pitch = rad2deg(Pitch);
    
    euler = [Roll, Pitch, Yaw];
end

