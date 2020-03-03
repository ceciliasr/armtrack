figure();
% v = VideoWriter('SJMotionRES.avi');
% v.FrameRate = 50;
% open(v);
for i = 1:length(A1_2)
    plot3([LShPos(i,1) A1_2(1,2,i) RShPos(i,1)],[LShPos(i,2) A1_2(2,2,i) RShPos(i,2)],[LShPos(i,3) A1_2(3,2,i) RShPos(i,3)],'-mo','Color','k','LineWidth',2,'MarkerSize',13,'MarkerFaceColor','k')
    hold on
    plot3([RShPos(i,1) RElbPos(i,1) RHandPos(i,1)],[RShPos(i,2) RElbPos(i,2) RHandPos(i,2)],[RShPos(i,3) RElbPos(i,3) RHandPos(i,3)],'-mo','Color','b','LineWidth',2,'MarkerSize',13,'MarkerFaceColor','b');
    plot3([LShPos(i,1) LElbPos(i,1) LHandPos(i,1)],[LShPos(i,2) LElbPos(i,2) LHandPos(i,2)],[LShPos(i,3) LElbPos(i,3) LHandPos(i,3)],'-mo','Color','r','LineWidth',2,'MarkerSize',13,'MarkerFaceColor','r');
    plot3(A1_2(1,:,i),A1_2(2,:,i),A1_2(3,:,i),'-mo','Color','k','LineWidth',2,'MarkerSize',13,'MarkerFaceColor','k');
    plot3(A2_2(1,:,i),A2_2(2,:,i),A2_2(3,:,i),'-mo','Color','k','LineWidth',2,'MarkerSize',13,'MarkerFaceColor','k');
    plot3(A32_2(1,:,i),A32_2(2,:,i),A32_2(3,:,i),'-mo','Color','k','LineWidth',2,'MarkerSize',13,'MarkerFaceColor','k');   
    hold off
    axis square
    axis off;
    axis equal;
    xlim([-1000 1000])
    ylim([-1000 1000])
    zlim([0 2000])
    title(['iteration: ',num2str(i)])
    view([1 0 0])
    drawnow();
    set(gca,'color','none')
%     frame = getframe(gcf);
%     writeVideo(v,frame);
end
% close(v);
