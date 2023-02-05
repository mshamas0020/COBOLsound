COBOLsound
----------------

Input a MIDI track and get playback with simple oscillators.
Use from the console with:

...> COBOLsound.exe MIDI-file waveform

Where 'MIDI-file' is any valid .midi or .mid file.

Optionally, choose one of the following waveforms:
	sin1, sin2, sin3, sin4, tri, saw, square, noise

e.g. ...> COBOLsound.exe test/tbp.midi sin4

NOTE: Simultaneous multi-track playback is not supported.
If you enter a multi-track file, the tracks will be played sequentially.  
If parts are missing (or nothing is playing at all), this is most likely why.

----------------

gnuCOBOL (3.1 or later) compiler must be installed for make.bat.
Additionally, SDL2 for MinGW must be installed in the include and lib folders.
A Windows x32 install is included with this distribution.