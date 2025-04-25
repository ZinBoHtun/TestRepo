function [class, score] = my_predictpca(mdl, data)
    M = size(data, 1);
    numClasses = length(mdl.class);

    class = zeros(M, 1);
    score = zeros(M, 1);

    for i = 1:M
        min_dist = inf;
        best_class = 0;

        for j = 1:numClasses
            [md, ~, ~] = MahalanobisDistance(mdl.class(j), data(i, :));
            if md < min_dist
                min_dist = md;
                best_class = j;
            end
        end

        class(i) = best_class;
        score(i) = min_dist;
    end
end

% -------------------------------------------------
% Helper function embedded inside my_predictpca
% -------------------------------------------------
function [md, b, std_per_mode] = MahalanobisDistance(pcamdl, v)
    v_centered = v - pcamdl.mu;
    b = pcamdl.eigvects * v_centered';           % Column vector
    std_per_mode = abs(b ./ sqrt(pcamdl.eigvals)); % Must be positive
    md = sqrt(sum(std_per_mode.^2));
end
