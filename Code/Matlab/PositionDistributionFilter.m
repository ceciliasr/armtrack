function [Posfilt] = PositionDistributionFilter(X,Y,Z)
% Calculates the centroid of a scattered distribution of positions and
% subtracts the 10% most distant points (it's supposed to subtract errors
% due to magnetic interference)

% INPUTS:
% X - X coordinates 
% Y - Y coordinates
% Z - Z coordinates

% OUTPUTS:
% Posfilt - Filtered [x, Y ,Z] coodinates

% CODE: 
    centroid = [mean(X) mean(Y) mean(Z)];
    distances = zeros(length(X),1);
    extraction = floor(length(distances)*0.1);

    for i = 1:length(distances)
        pos = [X(i) Y(i) Z(i)];
        distances(i) = norm(pos-centroid);
    end
    ord = sort(distances);
    ext = ord(end-extraction:end);
    [p,q] = find(distances(distances>min(ext)));

    k = ismember(distances, ext);
    indexes = find(k);
    REP = zeros(length(X)-extraction,4);
    cons = 0;
    for i = 1:length(X)
        if ~sum(ismember(indexes,i))
            cons = cons+1;
            Posfilt(cons,:) = [0,X(i),Y(i),Z(i)];
        end
    end
end


