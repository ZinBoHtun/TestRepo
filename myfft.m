function [yfft, f] = myfft(y, Fs)
    N = length(y);  % Length of the input signal
    yfft = fft(y);  % Compute the FFT of the signal
    yfft = yfft(1:floor(N/2));  % Keep only the first half of the FFT (due to symmetry)
    f = (0:floor(N/2)-1)' * (Fs / N);  % Frequency vector corresponding to the FFT bins
end
