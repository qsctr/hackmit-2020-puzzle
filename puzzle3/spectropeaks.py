import matplotlib.pyplot as plt
from scipy import signal
from scipy.io import wavfile
import numpy as np

wav_filename = input(".wav file to analyze: ")
sample_rate, samples = wavfile.read(wav_filename)
frequencies, times, spectrogram = signal.spectrogram(samples, sample_rate)

fourier = np.fft.fft(samples)
w = np.linspace(0, sample_rate, len(fourier))

# First half is the real component, second half is imaginary
fourier_to_plot = fourier[0:len(fourier)//2]
w = w[0:len(fourier)//2]

plt.figure(1)
plt.plot(w, fourier_to_plot)
plt.xlabel('frequency')
plt.ylabel('amplitude')
plt.show()

peak_locs = signal.find_peaks(fourier_to_plot, height=2e7)
peak_freqs = w[peak_locs[0]]//10*10
print(peak_freqs)