
R2vec = [ 0 0.2 0.4 0.6 0.8 ];

NumValC1 = max(size(R2vec));
NumValC2 = max(size(R2vec));

C1 = R2vec(1:NumValC1);
C2 = R2vec(1:NumValC2);

matrix_1 = importdata('1.txt');
matrix_2 = importdata('2.txt');
matrix_3 = importdata('3.txt');
matrix_4 = importdata('4.txt');
matrix_5 = importdata('5.txt');
matrix_6 = importdata('6.txt');
matrix_7 = importdata('7.txt');
matrix_8 = importdata('8.txt');
matrix_9 = importdata('9.txt');
matrix_10 = importdata('10.txt');
matrix_11 = importdata('11.txt');
matrix_12 = importdata('12.txt');
matrix_13 = importdata('13.txt');
matrix_14 = importdata('14.txt');
matrix_15 = importdata('15.txt');
matrix_16 = importdata('16.txt');
matrix_17 = importdata('17.txt');
matrix_18 = importdata('18.txt');
matrix_19 = importdata('19.txt');
matrix_20 = importdata('20.txt');



figure;

AxesH = axes;
drawnow;
InSet = get(AxesH, 'TightInset');
set(AxesH, 'Position', [InSet(1:2), 1-InSet(1)-InSet(3), 1-InSet(2)-InSet(4)])

% Define titles for each subplot
titles = {
    'Post-Double Mean Bias', 'Discrete Mean Bias', 'Half-Discrete Mean Bias', 'Adaptive Mean Bias', 'Score Mean Bias', ...
    'Post-Double rp(0.05)', 'Discrete rp(0.05)', 'Half-Discrete rp(0.05)', 'Adaptive rp(0.05)', 'Score rp(0.05)', ...
    'Post-Double Relevant', 'Discrete Relevant', 'Half-Discrete Relevant', 'Adaptive Relevant', 'Score Relevant', ...
    'Post-Double Total', 'Discrete Total', 'Half-Discrete Total', 'Adaptive Total', 'Score Total'


};

% Loop through each column of subplots (4 columns total)
for col = 1:4
    % Define matrices and common z-axis limits for the current column
    switch col
        case 1
            matrices = {matrix_1, matrix_2, matrix_3, matrix_4, matrix_5}; % Column 1: matrix_1, matrix_4, matrix_7
        case 2
            matrices = {matrix_6, matrix_7, matrix_8, matrix_9, matrix_10}; % Column 2: matrix_2, matrix_5, matrix_8
        case 3
            matrices = {matrix_11, matrix_12, matrix_13, matrix_14, matrix_15}; % Column 3: matrix_3, matrix_6, matrix_9
        case 4
            matrices = {matrix_16, matrix_17, matrix_18, matrix_19, matrix_20}; % Column 4 (mirroring column 3): matrix_7, matrix_8, matrix_9
    end
    
    % Calculate common z-axis limits for the current column
    z_min = min(cellfun(@(x) min(x(:)), matrices));
    z_max = max(cellfun(@(x) max(x(:)), matrices));

    % Loop through each subplot in the current column
    for row = 1:5
        subplot(5, 4, (row-1)*4 + col);
        surf(C1, C2, matrices{row});
        
        % Set title for each subplot
        title_text = titles{(col-1)*5 + row}; % Get title from titles array
        title(title_text, 'Interpreter', 'Latex');
        xlabel('First Stage $R^2$', 'Interpreter', 'Latex');
        ylabel('Second Stage $R^2$', 'Interpreter', 'Latex');
        zlabel('', 'Interpreter', 'Latex'); % Replace with your actual z-axis label
        
        % Set the z-axis limits to be the same for the current column
        zlim([z_min, z_max]);
    end
end

% Adjust figure size (example: 1000x800 pixels)
 set(gcf, 'Position', [100, 100, 1300, 1000]);

 print('test.eps', '-depsc', '-r300');  % '-r300' sets resolution to 300 DPI
