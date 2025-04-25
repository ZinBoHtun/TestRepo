function [filter,xc,yc] = MakeCircleMatchingFilter(diameter,W)
    % Calculate center coordinates (1-based indexing)
    xc = (W + 1)/2;  % Column center
    yc = (W + 1)/2;  % Row center
    
    % Create coordinate grid using MATLAB's meshgrid
    [j, i] = meshgrid(1:W, 1:W);  % j=columns, i=rows
    
    % Calculate radius
    radius = diameter/2;
    
    % Create double-precision filter using circle equation
    filter = double((i - yc).^2 + (j - xc).^2 <= radius^2);
end

