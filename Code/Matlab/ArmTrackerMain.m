%% ArmTracker Movement Metrics computation
% Author: Víctor Antonio Carmona Ortiz
% SLAM toolbox is used to perform all the quaternion computations
% Bachelor's Thesis on Biomedical Engineering
%% INDEX
% 1. DATA IMORT AND CONDITIONING 
% imports the data selected from the Data no matter whether it has already 
% been processed or not and prepares the data for the kinematic models 
% and the 3D model used for visualisation (humanhsape.org)

% 2. CALIBRATION 
% From the acceleration data of the wrist IMUs peaks of acceleration are
% found and the first peak is used to choose the calibration instant by the
% user. Quaternion data for the calibration instant are used to extract the
% constant relation between the IMU and the segment to which it is attached

% 3. KINEMATIC MODELING
% Quaternion data is used to calculate relative rotations between segments
% using the Slam Toolbox. After this, using forward kinematics and
% homogenous transforms the kinematics of the upper limbs are extracted
% (positions and orientations)

% 4. METRICS: Range of Motion -> Euler Angles and Cosine Rule
% Range of motion is estimated by the euler angles in specific order for
% each joint and using the cosine rule to extract arm elevation and elbow
% flexion

% 5. METRICS: Workspace Volume
% Calculates the volume covered by the user of the arm tracker during a
% capture session approximately and divides the hole space visited into 8
% different regions defining anatomical related thresholds. The percentage
% of time spent in those regions is extracted

% 6. METRICS: Accelerometry
% Extracts the activity of each arm of the ArmTracker user from the
% acceleration data of the IMUs located at the wrists. Single arm activity
% and bilateral activity is computed and compared to the dominancy of each
% arm. IMPORTANT: Change dominant value to the dominant arm of the user
%% 1. DATA IMORT AND CONDITIONING: Load quaternion data logged into the SD card and other files
addpath('Models',genpath('Data'))
load VictorModel.mat
load VictorLandmarks.mat
answer = questdlg('Is the data saved?', ...
    'Data Loading', ...
    'Yes','No','Loaded','Yes');
switch answer
    case 'Yes' % Data is saved in a .mat file
        cd Data\Imported
        cd ..\..
        uiopen('*.mat')
        [pks1,locs1] = findpeaks(Acc2,'MinPeakHeight',30);
        Time = (TimeStamp-TimeStamp(1))/1000;
        Time(end) = Time(end)+0.02;
    case 'No' % Data is not imported and is in a .txt file
        cd Data\Raw
        [FileName,PathName]= uigetfile('*.txt','browse');
        cd ..\..
        [Time,wQic,wQi1,wQi2,wQi3,wQi4,AccC,Acc2,Acc4] = ImportATData(FileName);
        Acc2 = sqrt(Acc2(:,1).^2+Acc2(:,2).^2+Acc2(:,3).^2);
        Acc4 = sqrt(Acc4(:,1).^2+Acc4(:,2).^2+Acc4(:,3).^2);
        AccC = sqrt(AccC(:,1).^2+AccC(:,2).^2+AccC(:,3).^2);        
        Time(end) = Time(end)+0.02;
        %[Time,AccRFA,AccLFA,AccC] = DataConditioning(TimeStamp,Acc2,Acc4,AccC); % Uncomment if resampling is nedded
    case 'Loaded' %Data is already in the workspace
end
addpath(genpath('slamtb'))
% Set the proper orientation for the human model
[Landmarks,Points] = SetCoordModel(HumanLandmarks.Variables,hooman.v,180);
hooman.v = Points;
HumanLandmarks.Variables = Landmarks;

% Showing the model used (comment if not desired)
A1 = [HumanLandmarks.HeadNeckJoint HumanLandmarks.C7T1Jnt HumanLandmarks.T12L1Jnt HumanLandmarks.L5S1Jnt HumanLandmarks.HiptRt HumanLandmarks.KneeJntRt HumanLandmarks.AnkleJntRt];
A2 = [HumanLandmarks.L5S1Jnt HumanLandmarks.HipLt HumanLandmarks.KneeJntLt HumanLandmarks.AnkleJntLt];
%3 = [HumanLandmarks.WristJntRt HumanLandmarks.ElbowJntRt HumanLandmarks.ShoulderRtJnt HumanLandmarks.C7T1Jnt HumanLandmarks.ShoulderLtJnt HumanLandmarks.ElbowJntLt HumanLandmarks.WristJntLt];
A32 = [HumanLandmarks.ShoulderRtJnt HumanLandmarks.C7T1Jnt HumanLandmarks.ShoulderLtJnt];
% A34 = [HumanLandmarks.WristJntRt HumanLandmarks.ElbowJntRt HumanLandmarks.ShoulderRtJnt];
% A35 = [HumanLandmarks.ShoulderLtJnt HumanLandmarks.ElbowJntLt HumanLandmarks.WristJntLt];
% A36 = [HumanLandmarks.C7T1Jnt HumanLandmarks.T12L1Jnt];
% display_obj(hooman,0,0.6)
% hold on
% plot3(A1(1,:),A1(2,:),A1(3,:),'-mo','Color','b','LineWidth',1,'MarkerEdgeColor','b','MarkerSize',13,'linewidth',2)
% plot3(A2(1,:),A2(2,:),A2(3,:),'-mo','Color','b','LineWidth',1,'MarkerEdgeColor','b','MarkerSize',13,'linewidth',2)
% plot3(A3(1,:),A3(2,:),A3(3,:),'-mo','Color','b','LineWidth',1,'MarkerEdgeColor','b','MarkerSize',13,'linewidth',2)
% plot3(A34(1,:),A34(2,:),A34(3,:),'-mo','Color','r','LineWidth',1,'MarkerEdgeColor','r','MarkerSize',13,'linewidth',2)
% plot3(A35(1,:),A35(2,:),A35(3,:),'-mo','Color','r','LineWidth',1,'MarkerEdgeColor','r','MarkerSize',13,'linewidth',2)
% plot3(A36(1,:),A36(2,:),A36(3,:),'-mo','Color','r','LineWidth',1,'MarkerEdgeColor','r','MarkerSize',13,'linewidth',2)
% xlabel('x')
% ylabel('y')
% zlabel('z')
%Kinematic model dimensions of the limbs
RUarmLength = [0 0 -norm(HumanLandmarks.ShoulderRtJnt-HumanLandmarks.ElbowJntRt)];
LUarmLength = [0 0 -norm(HumanLandmarks.ShoulderLtJnt-HumanLandmarks.ElbowJntLt)];
RFarmLength = [0 0 -norm(HumanLandmarks.ElbowJntRt-HumanLandmarks.WristJntRt)];
LFarmLength = [0 0 -norm(HumanLandmarks.ElbowJntLt-HumanLandmarks.WristJntLt)];

%% 2. CALIBRATION: Finding Calibration Instant & Sensor to Segment Calibration
% Claps are used to perform a simple calibration of the system, after a
% clap, an N-pose is performed 
figure('Units','normalized','Position',[0 0 0.99 0.9]);
plot(Time(1:5000),Acc2(1:5000))
disp('Choose tcal')
tcal = ginput(1);
tcal = uint16(tcal);
Time2 = Time(Time<tcal(1));
[row,col] = find(Time == Time2(end));
tcal = row;

% Since every segment must be refered to the chest, we have to subtract the
% Yaw angle of the chest to correct for the Yaw deviation of all the imus
% first
wec = q2e(wQic(tcal,:));
wyc = wec(3);
% At the moment of the calibration cq(k) (being k the kth segment 
% calibrated ) is identity
wQc = e2q([0,0,wyc]); 
w2qw = q2qc(wQc);
wQ1 = e2q([0,0,wyc]);
wQ2 = e2q([0,0,wyc]);
wQ3 = e2q([0,0,wyc]);
wQ4 = e2q([0,0,wyc]);

% The relative rotation between the  IMU and the segment is expressed as:
% w_q_i(k)*·w_q_(k), where * stands for the complex conjugate of a
% quaternion
icQc = qProd(q2qc(wQic(tcal,:)),wQc);
i1Q1 = qProd(q2qc(wQi1(tcal,:)),wQ1);
i2Q2 = qProd(q2qc(wQi2(tcal,:)),wQ2);
i3Q3 = qProd(q2qc(wQi3(tcal,:)),wQ3);
i4Q4 = qProd(q2qc(wQi4(tcal,:)),wQ4);

cqs1 = zeros(length(wQic),4);
cqs2 = zeros(length(wQic),4);
s1qs2 = zeros(length(wQic),4);
cqs3 = zeros(length(wQic),4);
cqs4 = zeros(length(wQic),4);
s3qs4 = zeros(length(wQic),4);

wqc = zeros(length(wQic),4);
wqs1 = zeros(length(wQic),4);
wqs2 = zeros(length(wQic),4);
wqs3 = zeros(length(wQic),4);
wqs4 = zeros(length(wQic),4);
e = zeros(length(wQic),3);
w_yqc = zeros(length(wQic),4);

RShPos    = zeros(length(wQic),3);
RElbPos    = zeros(length(wQic),3);
RHandPos   = zeros(length(wQic),3);
LShPos    = zeros(length(wQic),3);
LElbPos    = zeros(length(wQic),3);
LHandPos   = zeros(length(wQic),3);
A1_2 = zeros(size(A1,1),size(A1,2),length(wQic));
A2_2 = zeros(size(A2,1),size(A2,2),length(wQic));
A32_2 = zeros(size(A32,1),size(A32,2),length(wQic));

Eul1 = zeros(length(wQic),3);
Eul2 = zeros(length(wQic),3);
Eul3 = zeros(length(wQic),3);
Eul4 = zeros(length(wQic),3);
RElbAngle = zeros(length(wQic),1);
LElbAngle = zeros(length(wQic),1);

Vec = zeros(length(wQic),4);
ang = zeros(length(wQic),1);
close all

%% 3. KINEMATIC MODELING
% Used to follow XSens euler angle computations
Q_SameRefSys = [cosd(-45) sind(-45)*1 0 0]; 

% Computation of the relative rotation quaternions for each segment
for i = 1:length(wQic)
    cqs1(i,:) = qProd(q2qc(icQc),qProd(q2qc(wQic(i,:)),qProd(wQi1(i,:),i1Q1)))';
    s1qs2(i,:) = qProd(q2qc(i1Q1),qProd(q2qc(wQi1(i,:)),qProd(wQi2(i,:),i2Q2)))';
    cqs3(i,:) = qProd(q2qc(icQc),qProd(q2qc(wQic(i,:)),qProd(wQi3(i,:),i3Q3)))';
    s3qs4(i,:) = qProd(q2qc(i3Q3),qProd(q2qc(wQi3(i,:)),qProd(wQi4(i,:),i4Q4)))';
    wqc(i,:) = qProd(wQic(i,:),icQc)';
    e(i,:) = q2e(wqc(i,:));
    w_yqc(i,:) = e2q([0,0,e(i,3)]);
end

for i = 1:length(wQic)
%     [RShPos(i,:),RElbPos(i,:),RHandPos(i,:)] = KinematicModelYawChest(w2qw,w_yqc(i,:),cqs1(i,:),s1qs2(i,:),[1 0 0 0],[0;0;0],[0;0;0],HumanLandmarks.ShoulderRtJnt,[0;0;HumanLandmarks.ElbowJntRt(3)-HumanLandmarks.ShoulderRtJnt(3)],[0;0;HumanLandmarks.WristJntRt(3)-HumanLandmarks.ElbowJntRt(3)]);
%     [LShPos(i,:),LElbPos(i,:),LHandPos(i,:)] = KinematicModelYawChest(w2qw,w_yqc(i,:),cqs3(i,:),s3qs4(i,:),[1 0 0 0],[0;0;0],[0;0;0],HumanLandmarks.ShoulderLtJnt,[0;0;HumanLandmarks.ElbowJntLt(3)-HumanLandmarks.ShoulderLtJnt(3)],[0;0;HumanLandmarks.WristJntLt(3)-HumanLandmarks.ElbowJntLt(3)]);
%     R = q2R(w2qw)*q2R(w_yqc(i,:));
    [RShPos(i,:),RElbPos(i,:),RHandPos(i,:)] = KinematicModelYawChest([1 0 0 0],[1 0 0 0],cqs1(i,:),s1qs2(i,:),[1 0 0 0],[0;0;0],[0;0;0],HumanLandmarks.ShoulderRtJnt,[0;0;HumanLandmarks.ElbowJntRt(3)-HumanLandmarks.ShoulderRtJnt(3)],[0;0;HumanLandmarks.WristJntRt(3)-HumanLandmarks.ElbowJntRt(3)]);
    [LShPos(i,:),LElbPos(i,:),LHandPos(i,:)] = KinematicModelYawChest([1 0 0 0],[1 0 0 0],cqs3(i,:),s3qs4(i,:),[1 0 0 0],[0;0;0],[0;0;0],HumanLandmarks.ShoulderLtJnt,[0;0;HumanLandmarks.ElbowJntLt(3)-HumanLandmarks.ShoulderLtJnt(3)],[0;0;HumanLandmarks.WristJntLt(3)-HumanLandmarks.ElbowJntLt(3)]);
    R = q2R([1 0 0 0]);

    for j = 1:size(A1,2)
        A1_2(:,j,i) = R*A1(:,j);
    end
    for j = 1:size(A2,2)
        A2_2(:,j,i) = R*A2(:,j);
    end
    for j = 1:size(A32,2)
        A32_2(:,j,i) = R*A32(:,j);
    end
end

%{
A1_2Res = zeros(size(A1_2,1),size(A1_2,2),uint16(Time(end)/20*1000+1));
A2_2Res = zeros(size(A2_2,1),size(A2_2,2),uint16(Time(end)/20*1000+1));
A32_2Res = zeros(size(A32_2,1),size(A32_2,2),uint16(Time(end)/20*1000+1));

for i = 1:size(A1_2,2)
        hlp1 = A1_2(:,i,:);
        hlp1 = permute(hlp1,[1 3 2]);
        hlp1 = resample(hlp1',Time,50);
        for j = 1:length(A1_2Res)
            A1_2Res(:,i,j) = hlp1(j,:);
        end
end
for i = 1:size(A2_2,2)
        hlp2 = A2_2(:,i,:);
        hlp2 = permute(hlp2,[1 3 2]);
        hlp2 = resample(hlp2',Time,50);
        for j = 1:length(A2_2Res)
            A2_2Res(:,i,j) = hlp2(j,:);
        end
end
for i = 1:size(A32_2,2)
        hlp3 = A32_2(:,i,:);
        hlp3 = permute(hlp3,[1 3 2]);
        hlp3 = resample(hlp3',Time,50);
        for j = 1:length(A32_2Res)
            A32_2Res(:,i,j) = hlp3(j,:);
        end
end

[RShPos,RElbPos,RHandPos] = StabilizeFs(Time,RShPos,RElbPos,RHandPos,50);
[LShPos,LElbPos,LHandPos] = StabilizeFs(Time,LShPos,LElbPos,LHandPos,50);
%}
%% 4. METRICS: Range of Motion -> Euler Angles and Cosine Rule
for i = 1:length(wQic)
    Eul1(i,:) = eulXZY(cqs1(i,:));
    Eul2(i,:) = eulZXY(qProd(Q_SameRefSys,qProd(s1qs2(i,:),q2qc(Q_SameRefSys)))');
    Eul3(i,:) = eulXZY(cqs3(i,:));
    Eul4(i,:) = eulZXY(qProd(q2qc(Q_SameRefSys),qProd(s3qs4(i,:),Q_SameRefSys))');
end

% Euler Angles present irregularities, elevation (shoulder) and Flexion
% (elbow) angles are computed using cosine rule

RElbAngle = zeros(length(RElbPos),1);
LElbAngle = zeros(length(RElbPos),1);
RSAngle = zeros(length(RElbPos),1);
LSAngle = zeros(length(RElbPos),1);
TorsoAngle = zeros(length(RElbPos),1);

for i = 1:length(RElbPos)
    temp = qRot([0 0 1]',wqc(i,:));
    TorsoAngle(i) = CosineRule([0 0 1],temp');    
end

for i = 1:length(RElbPos)
    RElbAngle(i) = CosineRule(RElbPos(i,:)-HumanLandmarks.ShoulderRtJnt',RHandPos(i,:)-RElbPos(i,:));
    LElbAngle(i) = CosineRule(LElbPos(i,:)-HumanLandmarks.ShoulderLtJnt',LHandPos(i,:)-LElbPos(i,:));
    RSAngle(i) = CosineRule(RElbPos(i,:)-HumanLandmarks.ShoulderRtJnt',[0 0 -norm(RUarmLength)]);
    LSAngle(i) = CosineRule(LElbPos(i,:)-HumanLandmarks.ShoulderLtJnt',[0 0 -norm(LUarmLength)]);
end

%% 5. METRICS: Workspace Volume
RElbFilt = PositionDistributionFilter(RElbPos(:,1),RElbPos(:,2),RElbPos(:,3)) ;
RHandFilt = PositionDistributionFilter(RHandPos(:,1),RHandPos(:,2),RHandPos(:,3));
LElbFilt = PositionDistributionFilter(LElbPos(:,1),LElbPos(:,2),LElbPos(:,3));
LHandFilt = PositionDistributionFilter(LHandPos(:,1),LHandPos(:,2),LHandPos(:,3));

[EnvRE,VolRE] = boundary(RElbFilt(:,2),RElbFilt(:,3),RElbFilt(:,4),0.1);
[EnvLE,VolLE] = boundary(LElbFilt(:,2),LElbFilt(:,3),LElbFilt(:,4),0.1);
[EnvRH,VolRH] = boundary(RHandFilt(:,2),RHandFilt(:,3),RHandFilt(:,4),0.1);
[EnvLH,VolLH] = boundary(LHandFilt(:,2),LHandFilt(:,3),LHandFilt(:,4),0.1);

% Distribution in quadrants
% Right Hand
RHandRegA = RHandFilt(RHandFilt(:,3)>HumanLandmarks.ShoulderRtJnt(2),:);
RHandReg1A = RHandRegA(RHandRegA(:,4)>HumanLandmarks.ShoulderRtJnt(3),:);
RHandReg1 = RHandReg1A(RHandReg1A(:,2)>norm(RUarmLength)+HumanLandmarks.ShoulderRtJnt(1),:);
RHandReg5 = RHandReg1A(RHandReg1A(:,2)<norm(RUarmLength)+HumanLandmarks.ShoulderRtJnt(1),:);
RHandReg2A = RHandRegA(RHandRegA(:,4)<HumanLandmarks.ShoulderRtJnt(3),:);
RHandReg2 = RHandReg2A(RHandReg2A(:,2)>norm(RUarmLength)+HumanLandmarks.ShoulderRtJnt(1),:);
RHandReg6 = RHandReg2A(RHandReg2A(:,2)<norm(RUarmLength)+HumanLandmarks.ShoulderRtJnt(1),:);

RHandRegB = RHandFilt(RHandFilt(:,3)<HumanLandmarks.ShoulderRtJnt(2),:);
RHandReg3B = RHandRegB(RHandRegB(:,4)>HumanLandmarks.ShoulderRtJnt(3),:);
RHandReg3 = RHandReg3B(RHandReg3B(:,2)>norm(RUarmLength)+HumanLandmarks.ShoulderRtJnt(1),:);
RHandReg7 = RHandReg3B(RHandReg3B(:,2)<norm(RUarmLength)+HumanLandmarks.ShoulderRtJnt(1),:);
RHandReg4B = RHandRegB(RHandRegB(:,4)<HumanLandmarks.ShoulderRtJnt(3),:);
RHandReg4 = RHandReg4B(RHandReg4B(:,2)>norm(RUarmLength)+HumanLandmarks.ShoulderRtJnt(1),:);
RHandReg8 = RHandReg4B(RHandReg4B(:,2)<norm(RUarmLength)+HumanLandmarks.ShoulderRtJnt(1),:);

DistRReg1 = (length(RHandReg1)/length(RHandFilt))*100;
DistRReg2 = (length(RHandReg2)/length(RHandFilt))*100;
DistRReg3 = (length(RHandReg3)/length(RHandFilt))*100;
DistRReg4 = (length(RHandReg4)/length(RHandFilt))*100;
DistRReg5 = (length(RHandReg5)/length(RHandFilt))*100;
DistRReg6 = (length(RHandReg6)/length(RHandFilt))*100;
DistRReg7 = (length(RHandReg7)/length(RHandFilt))*100;
DistRReg8 = (length(RHandReg8)/length(RHandFilt))*100;

% Left Hand
LHandRegA = LHandFilt(LHandFilt(:,3)<HumanLandmarks.ShoulderLtJnt(2),:);
LHandReg1A = LHandRegA(LHandRegA(:,4)>HumanLandmarks.ShoulderLtJnt(3),:);
LHandReg1 = LHandReg1A(LHandReg1A(:,2)>norm(LUarmLength)+HumanLandmarks.ShoulderLtJnt(1),:);
LHandReg5 = LHandReg1A(LHandReg1A(:,2)<norm(LUarmLength)+HumanLandmarks.ShoulderLtJnt(1),:);
LHandReg2A = LHandRegA(LHandRegA(:,4)<HumanLandmarks.ShoulderLtJnt(3),:);
LHandReg2 = LHandReg2A(LHandReg2A(:,2)>norm(LUarmLength)+HumanLandmarks.ShoulderLtJnt(1),:);
LHandReg6 = LHandReg2A(LHandReg2A(:,2)<norm(LUarmLength)+HumanLandmarks.ShoulderLtJnt(1),:);

LHandRegB  = LHandFilt(LHandFilt(:,3) > HumanLandmarks.ShoulderLtJnt(2),:);
LHandReg3B = LHandRegB(LHandRegB(:,4) > HumanLandmarks.ShoulderLtJnt(3),:);
LHandReg3  = LHandReg3B(LHandReg3B(:,2) > norm(LUarmLength)+HumanLandmarks.ShoulderLtJnt(1),:);
LHandReg7  = LHandReg3B(LHandReg3B(:,2) < norm(LUarmLength)+HumanLandmarks.ShoulderLtJnt(1),:);
LHandReg4B = LHandRegB(LHandRegB(:,4) < HumanLandmarks.ShoulderLtJnt(3),:);
LHandReg4  = LHandReg4B(LHandReg4B(:,2) > norm(LUarmLength)+HumanLandmarks.ShoulderLtJnt(1),:);
LHandReg8  = LHandReg4B(LHandReg4B(:,2) < norm(LUarmLength)+HumanLandmarks.ShoulderLtJnt(1),:);

DistLReg1 = length(LHandReg1)/length(LHandFilt)*100;
DistLReg2 = length(LHandReg2)/length(LHandFilt)*100;
DistLReg3 = length(LHandReg3)/length(LHandFilt)*100;
DistLReg4 = length(LHandReg4)/length(LHandFilt)*100;
DistLReg5 = length(LHandReg5)/length(LHandFilt)*100;
DistLReg6 = length(LHandReg6)/length(LHandFilt)*100;
DistLReg7 = length(LHandReg7)/length(LHandFilt)*100;
DistLReg8 = length(LHandReg8)/length(LHandFilt)*100;


[EnvRReg1,VolRReg1] = boundary(RHandReg1(:,2),RHandReg1(:,3),RHandReg1(:,4),0.9);
[EnvRReg2,VolRReg2] = boundary(RHandReg2(:,2),RHandReg2(:,3),RHandReg2(:,4),0.9);
[EnvRReg3,VolRReg3] = boundary(RHandReg3(:,2),RHandReg3(:,3),RHandReg3(:,4),0.9);
[EnvRReg4,VolRReg4] = boundary(RHandReg4(:,2),RHandReg4(:,3),RHandReg4(:,4),0.9);
[EnvRReg5,VolRReg5] = boundary(RHandReg5(:,2),RHandReg5(:,3),RHandReg5(:,4),0.9);
[EnvRReg6,VolRReg6] = boundary(RHandReg6(:,2),RHandReg6(:,3),RHandReg6(:,4),0.9);
[EnvRReg7,VolRReg7] = boundary(RHandReg7(:,2),RHandReg7(:,3),RHandReg7(:,4),0.9);
[EnvRReg8,VolRReg8] = boundary(RHandReg8(:,2),RHandReg8(:,3),RHandReg8(:,4),0.9);

[EnvLReg1,VolLReg1] = boundary(LHandReg1(:,2),LHandReg1(:,3),LHandReg1(:,4),0.9);
[EnvLReg2,VolLReg2] = boundary(LHandReg2(:,2),LHandReg2(:,3),LHandReg2(:,4),0.9);
[EnvLReg3,VolLReg3] = boundary(LHandReg3(:,2),LHandReg3(:,3),LHandReg3(:,4),0.9);
[EnvLReg4,VolLReg4] = boundary(LHandReg4(:,2),LHandReg4(:,3),LHandReg4(:,4),0.9);
[EnvLReg5,VolLReg5] = boundary(LHandReg5(:,2),LHandReg5(:,3),LHandReg5(:,4),0.9);
[EnvLReg6,VolLReg6] = boundary(LHandReg6(:,2),LHandReg6(:,3),LHandReg6(:,4),0.9);
[EnvLReg7,VolLReg7] = boundary(LHandReg7(:,2),LHandReg7(:,3),LHandReg7(:,4),0.9);
[EnvLReg8,VolLReg8] = boundary(LHandReg8(:,2),LHandReg8(:,3),LHandReg8(:,4),0.9);

%% 6. METRICS: Accelerometry
%Mono Arm Use / Bilateral Arm Use
count = 0.0163;
[RightArmActivity,LeftArmActivity] = UnilateralActivity(Acc2,Acc4,AccC,count);
[RightBilateralAct,LeftBilateralAct,RightUnilateralAct,LeftUnilateralAct] = BilateralArmActivity(RightArmActivity,LeftArmActivity);

dominant = 'Right';
switch dominant
    case 'Right'
        % Bilateral Magnitude is just the sum of both arm activity
        BilateralMagnitude = RightArmActivity+LeftArmActivity;
        % Magnitude ratio is calculated by:
        % 1) Adding one count at each UE magnitude vector
        % 2) Dividing magnitude ND/D
        % 3) Log transforming the values
        %1)
        RightArmActivity2 = RightArmActivity +1;
        LeftArmActivity2 = LeftArmActivity +1;
        % 2)
        Div = LeftArmActivity2./RightArmActivity2;
        % 3) 
        MAG_RATIO = real(log(Div));
    case 'Left'
        BilateralMagnitude = RightArmActivity + LeftArmActivity;
        %1)
        RightArmActivity2 = RightArmActivity +1;
        LeftArmActivity2 = LeftArmActivity +1;
        % 2)
        Div = RightArmActivity2./LeftArmActivity2;
        % 3) 
        MAG_RATIO = real(log(Div));
        subplot(2,2,4)  
end
