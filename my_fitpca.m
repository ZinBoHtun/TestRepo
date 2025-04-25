function mdl = my_fitpca(D, class)
    class_labels = unique(class);
    numclasses = length(class_labels);
    mdl.class = struct([]);
    for i = 1:numclasses
        idx = class == class_labels(i);
        D_class = D(idx, :);
        [coeff, ~, latent, ~, ~, mu] = pca(D_class);
        mdl.class(i).eigvects = coeff';
        mdl.class(i).eigvals = latent;
        mdl.class(i).mu = mu;
    end
end

