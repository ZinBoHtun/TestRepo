function MagnitudeSpectrumPlot(y, Fs, col)
    % MagnitudeSpectrumPlot - Plots the magnitude spectrum of the signal's FFT.
    % y: Input signal vector (Nx1)
    % Fs: Sampling frequency of the signal
    % col: Color or style for the plot ('*' reveals the hidden message)
    
    % Step 1: Compute the FFT using the myfft function
    [yfft, f] = myfft(y, Fs);
    
    % Step 2: Compute the magnitude of the FFT using abs
    magnitude = abs(yfft);
    
    % Step 3: Plot the magnitude spectrum using '*' as the plot symbol
    figure;
    plot(f, magnitude, col);  % Use '*' to plot the points as stars
    
    % Step 4: Add labels and title
    xlabel('Frequency (Hz)');
    ylabel('Magnitude');
    title('Magnitude Spectrum');
end
