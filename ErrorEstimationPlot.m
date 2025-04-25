function [mse, R, p, rg] = ErrorEstimationPlot(prediction, target)
    % ErrorEstimationPlot - Compare predicted vs actual values
    % Inputs:
    %   prediction: predicted values [Mx1]
    %   target: true values [Mx1]
    % Outputs:
    %   mse: mean squared error
    %   R: Pearson correlation coefficient
    %   p: p-value of correlation
    %   rg: range of the data (for diagonal line plotting)

    % Ensure column vectors
    prediction = prediction(:);
    target = target(:);

    % Compute error metrics
    mse = mean((prediction - target).^2);
    [R, p] = corr(prediction, target);

    % Plot
    scatter(target, prediction, 'filled');
    xlabel('Actual Values');
    ylabel('Predicted Values');
    title(sprintf('Prediction vs Actual (MSE = %.2f, R = %.2f)', mse, R));
    grid on;
    axis equal;
    hold on;

    % Plot diagonal reference line
    rg = [min([target; prediction]), max([target; prediction])];
    plot(rg, rg, 'r--', 'LineWidth', 1.5);
    hold off;
end
