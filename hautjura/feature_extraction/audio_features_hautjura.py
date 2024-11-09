#Gets audio features from a list of directories. Requires addition of audiopath for proper execution!

import os
import numpy as np
import librosa
import pandas as pd

def get_feats(audio_path):
    y, sr = librosa.load(audio_path, sr=None)
    
    # Calculate call length in seconds
    t_call = librosa.get_duration(y=y, sr=sr)
    
    # Calculate spectrogram
    y_stft = librosa.stft(y)
    y_db = librosa.amplitude_to_db(np.abs(y_stft), ref=np.max)
    
    # Calculate frequencies
    freqs = librosa.fft_frequencies(sr=sr)
    times = librosa.times_like(y_db)
    
    # Find principal frequency
    f_principal_khz = freqs[np.argmax(np.mean(y_db, axis=1))] / 1000
    
    # Calculate frequency range
    low_freq_khz = np.min(freqs[np.any(y_db > -60, axis=1)]) / 1000
    high_freq_khz = np.max(freqs[np.any(y_db > -60, axis=1)]) / 1000
    delta_freq_khz = high_freq_khz - low_freq_khz
    
    # Calculate frequency standard deviation
    freq_std_khz = np.std(freqs[np.any(y_db > -60, axis=1)]) / 1000
    
    # Calculate slope
    time_diff = np.diff(times)
    freq_diff = np.diff(np.argmax(y_db, axis=0))
    slope_khz_s = np.mean(freq_diff / time_diff) * (sr / 2000) / len(freqs)
    
    # Calculate sinuosity
    path_length = np.sum(np.sqrt(time_diff**2 + (freq_diff * sr / len(freqs) / 1000)**2))
    sinuosity = path_length / (times[-1] - times[0])
    
    # Calculate mean power
    mean_power_db_hz = np.mean(y_db)
    
    # Calculate tonality
    tonality = np.max(y_db) - np.mean(y_db)
    
    # Calculate peak frequency
    peak_freq_khz = freqs[np.argmax(np.max(y_db, axis=1))] / 1000
    
    return [
        t_call,
        f_principal_khz,
        low_freq_khz,
        high_freq_khz,
        delta_freq_khz,
        freq_std_khz,
        slope_khz_s,
        sinuosity,
        mean_power_db_hz,
        tonality,
        peak_freq_khz
    ]


def make_featframe(directory):
    features_list = []
    for filename in os.listdir(directory):
        if filename.endswith(".wav") or filename.endswith(".WAV"):
            file_path = os.path.join(directory, filename)
            features = get_feats(file_path)
            features_list.append([filename] + features)
        else: 
            print('extension mismatch')
    
    feature_names = ['filename', 't_call', 'f_principal_khz', 'low_freq_khz', 'high_freq_khz', 'delta_freq_khz', 'frequency_standard_deviation_khz', 'slope_khz_s', 'sinuosity', 'mean_power_db_hz', 'tonality', 'peak_freq_khz']
    
    df = pd.DataFrame(features_list, columns=feature_names)
    return df


#Audiofile directory. Must be fullpath. 
audiodir = r'fullpath_to_audio_directory'

#Generates a csv from dataframe of audio features for a directory of wav files
def audio_features_hautjura(audiodir):
    
    results_df = make_featframe(audiodir)
    
    results_df.to_csv("audio_features_hautjura.csv", index=False)
    
    print('fin.')
