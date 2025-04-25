% Define the filter size we will use in step 2:
filtsize = 85;

% Creating test image 'im' by splicing together two built in images.
% Also zero-padding (adding zeros around the border) with half the 
% filter size (filtsize) we will use so that the filter could be 
% centered on any actual image pixel, including those near the border.
% 'coins.png' contains bright nickels and dimes on a dark background
% 'eight.tif' contains dark quarters on a bright background, so we invert it
% to match 'coins.png'
im1 = imread('coins.png');
[r,c] = size(im1);
im2 = imread('eight.tif');
[r2,c2] = size(im2);
filtsizeh = floor(filtsize/2);
im = zeros(r+r2+filtsize,c+filtsize);
im(filtsizeh+1:filtsizeh+r+r2,filtsizeh+1:filtsizeh+c) = [im1;255-im2(:,1:c)];
[r,c] = size(im);
imagesc(im);colormap(gray);title('test image');axis equal;

% Initializing assessed/displayed variables as empty so that code is executable 
msk=[]; msk_dil=[]; msk_dil_erd=[]; centroid=[]; component_size=[]; 

%%%%% 1. Localize the centroid of each coin
% Otsu threshold
msk = OtsuThreshold(im);
figure; imagesc(msk); colormap(gray); title('Otsu'); axis equal;

% Dilate 9x9
msk_dil = imdilate(msk,ones(9,9));
figure; imagesc(msk_dil); colormap(gray); title('Dilated'); axis equal;

% Erode 23x23
msk_dil_erd = imerode(msk_dil,ones(23,23));
figure; imagesc(msk_dil_erd); colormap(gray); title('Eroded'); axis equal;


% Connected components to get centroids of coins:
cc = bwconncomp(msk_dil_erd);
props_struct = regionprops(cc);
centroid = zeros(length(props_struct),2);
component_size = zeros(length(props_struct),1);
for i=1:length(props_struct)
    centroid(i,:) = round(props_struct(i).Centroid);
    component_size(i) = props_struct(i).Area;
end


%%%%% 2. Measure features for each coin using a bank of matching filters
% make matching filters to create features
% Define diameters to use for filters
dimediameter = 31;
quarterdiameter = 51;
nickeldiameter = 41;

% Initialize assessed variable D
D=[]; nickelfilter = []; dimefilter = []; quarterfilter = [];

% Use the MakeCircleMatchingFilter function to create matching filters for dimes, nickels, and quarters
% (This is in a separate Matlab grader problem. Save your work, 
%       complete the corresponding grader problem and embed the solution 
%       in the helper function list below.)
nickelfilter = MakeCircleMatchingFilter(nickeldiameter,filtsize);
dimefilter = MakeCircleMatchingFilter(dimediameter,filtsize);
quarterfilter = MakeCircleMatchingFilter(quarterdiameter,filtsize);

figure;
subplot(1,3,1); imagesc(dimefilter); colormap(gray); title('dime filter'); axis tight equal;
subplot(1,3,2); imagesc(nickelfilter); colormap(gray); title('nickel filter'); axis tight equal;
subplot(1,3,3); imagesc(quarterfilter); colormap(gray); title('quarter filter'); axis tight equal;

% Evaluate each of the 3 matching filters on each coin to serve as 3 feature measurements 
D = zeros(length(centroid),3);
for i=1:length(centroid)
    D(i,1) = corr(dimefilter(:),reshape(msk_dil_erd(centroid(i,2)-filtsizeh:...
        centroid(i,2)+filtsizeh,centroid(i,1)-filtsizeh:centroid(i,1)+filtsizeh),[filtsize*filtsize,1]));
    D(i,2) = corr(nickelfilter(:),reshape(msk_dil_erd(centroid(i,2)-filtsizeh:...
        centroid(i,2)+filtsizeh,centroid(i,1)-filtsizeh:centroid(i,1)+filtsizeh),[filtsize*filtsize,1]));
    D(i,3) = corr(quarterfilter(:),reshape(msk_dil_erd(centroid(i,2)-filtsizeh:...
        centroid(i,2)+filtsizeh,centroid(i,1)-filtsizeh:centroid(i,1)+filtsizeh),[filtsize*filtsize,1]));    
end

figure;
subplot(1,3,1); imagesc(dimefilter); colormap(gray); title('dime filter'); axis tight equal;
subplot(1,3,2); imagesc(nickelfilter); colormap(gray); title('nickel filter'); axis tight equal;
subplot(1,3,3); imagesc(quarterfilter); colormap(gray); title('quarter filter'); axis tight equal;

%%%%% 3. Perform k-means clustering of features for unsupervised learning classifier
rng(0);
cls_init=[]; cls=[]; totcount=[];
%%%%% 3. Perform k-means clustering of features for unsupervised learning classifier
rng(0);  % Set random seed for reproducibility
cls_init = kmeans(D, 3);  % Apply k-means with 3 clusters to the feature matrix D

% Compute the average area (size) of the components in each class
class_ave_object_size = zeros(3, 1);
for j = 1:3
    class_ave_object_size(j) = mean(component_size(cls_init == j));
end

cls_init
% relabel centroid classes based on average size of the objects in each class. smallest will be dime, next nickel, and largest quarter
% Sort class_ave_object_size to map the labels based on size
[class_ave_object_size, classmap] = sort(class_ave_object_size);

% Map the labels from k-means to the class labels [1, 2, 3] for dimes, nickels, and quarters
cls = zeros(length(cls_init), 1);
for i = 1:length(cls_init)
    cls(i) = find(cls_init(i) == classmap);
end


cls
% Visualize the result
figure; imagesc(im);colormap(gray);title('test image');hold on;axis equal;

% plot circles around each coin with different color/diameter unique to each type and count the change
% Initialize total count for the sum of coin values
totcount = 0;

% Plot each classified coin as a circle with a unique color
for i = 1:length(centroid)
    % Get the coin classification and size information
    coin_type = cls(i);  % 1 for dime, 2 for nickel, 3 for quarter
    
    % Set color and diameter based on coin type
    if coin_type == 1
        coin_color = 'r';  % Red for dimes
        coin_diameter = dimediameter;  % Diameter for dimes
    elseif coin_type == 2
        coin_color = 'g';  % Green for nickels
        coin_diameter = nickeldiameter;  % Diameter for nickels
    elseif coin_type == 3
        coin_color = 'b';  % Blue for quarters
        coin_diameter = quarterdiameter;  % Diameter for quarters
    end
    
    % Add coin to plot using the AddCoinToPlotAndCount function
    [totcount, coin_value] = AddCoinToPlotAndCount(centroid(i, 1), centroid(i, 2), coin_diameter, coin_color, totcount);
end


title([num2str(totcount),' cents'])


%%%%%%%%%%%%%%%%%%%% Helper Functions %%%%%%%%%%%%%%%%%%%%%
function [totcount, coin_value] = AddCoinToPlotAndCount(x, y, diameter, color, totcount)
    % Draw a circle representing the coin
    theta = linspace(0, 2*pi, 100);
    radius = diameter / 2;
    xp = radius * cos(theta) + x;
    yp = radius * sin(theta) + y;
    plot(xp, yp, color, 'LineWidth', 2);

    % Assign a coin value based on the type (you may modify this as per your classification)
    if color == 'r'  % Red for dimes
        coin_value = 10;  % Dime = 10 cents
    elseif color == 'g'  % Green for nickels
        coin_value = 5;   % Nickel = 5 cents
    elseif color == 'b'  % Blue for quarters
        coin_value = 25;  % Quarter = 25 cents
    end
    
    % Update total count
    totcount = totcount + coin_value;
end


function filter = MakeCircleMatchingFilter(diameter,filtsize)
filter = zeros(filtsize,filtsize);
radius = diameter/2;
c = (filtsize+1)/2;
for i=1:filtsize
    for j=1:filtsize
        if (i-c)*(i-c) + (j-c)*(j-c) <= radius*radius
            filter(i,j) = 1;
        end
    end
end
end

function [msk,thrsh] = OtsuThreshold(im)
hst = imhist(im);
res = otsuthresh(hst);
thrsh = res*255;
msk = im>thrsh;
end
