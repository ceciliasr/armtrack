figure();
for i = 1:5:length(wQic)
    plot3(A1(1,:),A1(2,:),A1(3,:),'-mo','Color','k','LineWidth',2,'MarkerSize',13)
    hold on
    plot3(A2(1,:),A2(2,:),A2(3,:),'-mo','Color','k','LineWidth',2,'MarkerSize',13)
    plot3(A32(1,:),A32(2,:),A32(3,:),'-mo','Color','k','LineWidth',2,'MarkerSize',13)
    plot3([HumanLandmarks.ShoulderRtJnt(1) RElbPos(i,1) RHandPos(i,1)],[HumanLandmarks.ShoulderRtJnt(2) RElbPos(i,2) RHandPos(i,2)],[HumanLandmarks.ShoulderRtJnt(3) RElbPos(i,3) RHandPos(i,3)],'-mo','Color','b','LineWidth',2,'MarkerSize',13,'MarkerFaceColor','b');
    plot3([HumanLandmarks.ShoulderLtJnt(1) LElbPos(i,1) LHandPos(i,1)],[HumanLandmarks.ShoulderLtJnt(2) LElbPos(i,2) LHandPos(i,2)],[HumanLandmarks.ShoulderLtJnt(3) LElbPos(i,3) LHandPos(i,3)],'-mo','Color','r','LineWidth',2,'MarkerSize',13,'MarkerFaceColor','r');
    hold off
    axis square
    axis off;
    axis equal;
    xlim([-1000 1000])
    ylim([-1000 1000])
    zlim([0 2000])
    title(['iteration: ',num2str(i)])
    view([1 -1 0.6])
    drawnow();
end