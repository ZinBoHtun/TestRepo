function [coinvalue,x_plot,y_plot,col] = AddCoinToPlotAndCount(x,y,cls)
% initialize radians for defining x_plot and y_plot using cos and sin functions
rads = 0:2*pi/32:2*pi;
% initialize parameters for radius and color of circle for each type of coin
% Initialize coin parameters based on classification
    if cls == 1       % Dime
        radius = 22;
        col = 'r';    % Red
        coinvalue = 10;
    elseif cls == 2   % Nickel
        radius = 30;
        col = 'g';    % Green
        coinvalue = 5;
    elseif cls == 3   % Quarter
        radius = 40;
        col = 'm';    % Magenta
        coinvalue = 25;
    else
        error('Invalid coin classification');
    end
    
    % Calculate circle coordinates
    x_plot = x + radius * cos(rads);
    y_plot = y + radius * sin(rads);
    

plot(x_plot,y_plot,col);
end
