# DO NOT PUT .WAV in the INPUT FILE TABLE! IT WILL DRIVE YOU BANANAS IF YOU DO!
# Concatenate groups of 3 sound files with pauses according to file names in tab-separated text file
# 
# For each group of sound files, file names must be listed in order 
# in 1st 3 columns (minus '.wav'), with 4th column as name for final concatenated file
# **Table must have header row, although names are irrelevant**
#
# ***Make sure you don't have any spaces in your file paths***
# (i.e. don't use folders with spaces in the name, and make sure there are no extra spaces at the end)
#
# Useful for oddity or ABX experiments
#
# Author: Danielle Daidone 4/27/17
####################################################

form Concatenate sound triads from table 
      comment Give the directory of the sound files:
      sentence soundDir 
      comment Give the directory where concatenated sound files will be saved:
      sentence saveDir 
      comment Give the name of the tab-separated input text file:
      word inputTable 
endform  

# remove any objects open in object window
select all
numberOfSelectedObjects = numberOfSelected ()
if numberOfSelectedObjects > 0
     Remove
endif

# read in table; first row will be treated as headers
Read Table from tab-separated file... 'inputTable$'
Rename... table

select Table table

# get number of rows in table
numRows = Get number of rows

for i to numRows
	
	# get names of sound files and name of final concatenated file
	filename1$ = Table_table$ [i, 1]
	filename2$ = Table_table$ [i, 2]
	filename3$ = Table_table$ [i, 3]
	concatname$ = Table_table$ [i, 4] 

		# open 1st sound file
		Read from file... 'soundDir$''filename1$'.wav
	
			#get sampling frequency of sound and create silence based on that
   			sampling_freq = Get sampling frequency

		# open 2nd sound file
		Read from file... 'soundDir$''filename2$'.wav
	
			# create copy of silence

		# open 3rd sound file
		Read from file... 'soundDir$''filename3$'.wav
	
			# now concatenate all three sound files with pauses in between	
			select all
			minus Table table
			Concatenate

			# save concatenated sounds to wav file 
			Write to WAV file... 'saveDir$''concatname$'.wav

			select all
			minus Table table
			Remove
		
	
endfor

select all
Remove
writeInfoLine: "Files successfully created!"