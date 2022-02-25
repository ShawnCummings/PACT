# 01-Concatenated

Stims taken from Tzeng et al. 2021, re-annotated in 4 tiers: Token (entire word), Onset, Offset, and Fricative. See Annotation Criteria.docx in Notes/

# 02-Voice_Changed

f1 Stims altered using Praat Vocal Toolkit (to create sh) and Praat Change Gender (to create pe). Specifications as follows:

f1 -> sh:
Vocal Toolkit Process-
Change vocal tract size, pitch, and duration...
0.8 180 90 0.0

f1 -> pe:
Convert-
Change Gender...
75.0 600.0 0.8 100.0 1.0 1.0

# 03-Chopped

Stim onsets, offsets, and fricatives chopped to individual .wav's, using extract.praat (margin set to 0.000). Arranged by talker (f1, pe, sh) and condition (Exposure Clear, Exposure Ambiguous, Test) and batch renamed by segment to match talker. 

# 04-Collated

All stims from 03-Chopped put together in one mass directory (for ease of access by Praat-Concatenate-Script.praat).

# 05-Script

Tab separated .txt files prepared for use by Praat-Concatenate-Script.praat. 5 combinations created:
	ONSET	FRICATIVE	OFFSET
	f1	f1		f1
	pe	pe		pe
	pe	f1		pe
	sh	sh		sh
	sh	f1		sh

# 06-Reconcatenated

Praat-Concatenate-Script.praat used to recombine segments and create 5 lists based on combinations from 05-Script. 

NOTE: f1f1f1, shshsh, and pepepe are theoretically redundant, as they are reconcatenations of the same segments as chopped in 03-Chopped.

# 07-Converted_to_.mp3

Converted all stims from .wav to .mp3 (except shshsh, which is never needed)

using ffmpeg -i *stim*.wav *stim*.mp3

# 08-Fillers_.mp3

Selected filler words converted to .mp3


