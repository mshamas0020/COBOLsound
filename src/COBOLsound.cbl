      ******************************************************************
      * COBOLsound.cbl
      * 
      * Input a MIDI track and get playback with simple oscillators.
      * Execute from the command line with:
      *
      * C:...> COBOLsound.exe MIDI-file waveform
      *
      *    Where 'MIDI-file' is any valid .midi or .mid file.
      *    
      *    Optionally, choose one of the following waveforms:
      *        sin1, sin2, sin3, sin4, tri, saw, square, noise
      *
      *    NOTE: Simultaneous multi-track playback is not supported.
      *    If you enter a multi-track file, the tracks will be
      *    played sequentially.   
      *
      * Resource for MIDI format specifications:
      * http://www.music.mcgill.ca/~ich/classes/mumt306/StandardMIDIfileformat.html
      ******************************************************************
       >> SOURCE FORMAT FREE

       >> define constant BUFFER-SIZE      as 4096
       >> define constant SAMPLE-RATE      as 44100
       *>  audio format is signed 16-bit, 1 channel, little-endian

       >> define constant ENV-ATTACK-MS    as 5
       >> define constant ENV-RELEASE-MS   as 20

       >> define constant MAX-OSCILLATORS  as 32
       >> define constant WAVETABLE-AMP    as 0.125
       >> define constant WAVETABLE-SIZE   as 1024
      
       
       identification division.
       program-id. COBOLSound.

       environment division.
       input-output section.
       file-control.

       *>  input file is cmd-line argument 1
       select in-file assign to dynamic in-name
           file status is in-stat.

       *>  output file is raw, using format 16-bit, 1-channel, little-endian
       *>  some audio tools such as Audacity can import these files
       select out-file assign to 'output.pcm'
           organization sequential.
               

       data division.
       file section.

       *>  input file (MIDI)
       fd  in-file.
       01  in-byte            binary-char unsigned.

       *>  output file (PCM)
       fd  out-file.
       01  buff.                
           02  buff-item       binary-short 
                               occurs BUFFER-SIZE times 
                               indexed by buff-i.
                             

       working-storage section.

       01  SAMPLE-SIZE         constant as 2. *> in bytes
                               
      
       01  TWO-PI                  constant as 6.28319.
       
       01  cmd-line            pic x(099).

       *>  INPUT FILE

       01  in-name             pic x(100).
       01  in-stat             pic x(002).
       01  in-pos              binary-long unsigned value 0.
       01  in-eof              pic 9(001) value 0.
                     
       01  read-size           binary-short unsigned.
       01  READ-MAX            constant as 4.
       01  read-table.
           02  read-item       binary-char unsigned
                               occurs READ-MAX times.
       01  read-vlq            binary-long unsigned.
       01  read-long           binary-long unsigned.

       *>  MIDI

       01  midi-tick-rate      binary-long unsigned value 1.  *> in ticks per frame

       *>  when tempo is set, 1 frame = 1 quarter note
       01  midi-frame-size     binary-long unsigned value 500000. *> in micro-seconds
      
       01  midi-to-next-event  binary-long unsigned value 0. *> in samples

       *>  when file read is inside track
       01  midi-inside-track   binary-char unsigned value 0.

       01  midi-msg-code       binary-char unsigned value 0.
       01  midi-msg-len-str    pic x(015) value '000000022221120'.
       01  midi-msg-len        redefines midi-msg-len-str.
           02  midi-msg-len-item   binary-char unsigned
                                   occurs 15 times.

       *>  OSCILLATORS

       *>  gnuCOBOL's implementation of float arithmetic has very poor performance
       *>  instead, using a long where 0x8000 is equivalent to 1 is much less expensive
       01  OUT-RANGE           CONSTANT AS 32768.
       01  osc.
           02  osc-item        occurs MAX-OSCILLATORS times
                               indexed by osc-i.
               *> from midi
               03  osc-note        binary-char unsigned value 0.
               03  osc-velocity    binary-long value 0. 

               03  osc-frequency   usage comp-1 value 1.
               03  osc-envelope    binary-long value 0.
               03  osc-phase       binary-long unsigned value 0.
               03  osc-phase-inc   binary-long unsigned value 0.
               03  osc-held        binary-char unsigned value 0.
               03  osc-output      binary-short value 0.
       01  osc-phase-range         binary-long unsigned.
       01  osc-active          pic 9(001) value 0.

       *>  ENVELOPE

       01  env-attack-inc      binary-long unsigned.
       01  env-release-dec     binary-long unsigned.

       *>  WAVETABLE

       01  wavetable.
           02  wt-item         binary-short value 0
                               occurs WAVETABLE-SIZE times.

       *>  waveform type: sin1, sin2, sin3, sin4, tri, saw, square, noise
       *>      sin1 - system implementation (default)
       *>      sin2 - Bhaskara I's approximation
       *>      sin3 - Taylor's series - 9th order
       *>      sin4 - Chebyshev polynomial - 7th order
       *>  from cmd-line argument 2
       01  wt-type             pic x(010).
       01  wt-x                usage comp-1 value 0.
       01  wt-amplitude        binary-long value 1.
       01  wt-phase            binary-long value 0.
       01  wt-output           binary-short value 0.

       01  audio-playing       pic 9(001) value 0.

       01  main-exit           pic 9(001) value 0.

       01  error-message       pic x(100).


       *>  shared local vars
       01  i                   binary-long value 0.
       01  local-1             binary-long value 0.
       01  local-2             binary-long value 0.



       *>  PROCEDURE
       procedure division.

       *>  entry point
       perform COBOL-SOUND.

       COBOL-SOUND.
           perform CS-INIT
           perform CS-MAIN until main-exit = 1
           perform CS-CLOSE
           stop run.





       *>  INIT
       CS-INIT.
           *> get command line 
           accept cmd-line from command-line

           *> compute values
           compute wt-amplitude = (OUT-RANGE - 1) * WAVETABLE-AMP
           compute env-attack-inc = 1000 / ENV-ATTACK-MS / SAMPLE-RATE * (OUT-RANGE - 1)
           compute env-release-dec = 1000 / ENV-RELEASE-MS / SAMPLE-RATE * (OUT-RANGE - 1)
           compute osc-phase-range = WAVETABLE-SIZE *  OUT-RANGE
           

           set buff-i to 1

           *> remove ascii offset from midi-msg-len
           *> because string was reformatted into binary-char table
           perform varying i from 1 by 1 until i > 15
               subtract 48 from midi-msg-len-item(i)
           end-perform

           *> open input file
           perform INIT-OPEN-INPUT

           *> make wavetable
           perform INIT-MAKE-WAVETABLE

           *> from audio_out.c
           call "audio_init" using
               by reference BUFFER-SIZE
               by reference SAMPLE-SIZE
               by reference SAMPLE-RATE

           *> read data from midi header
           perform INIT-READ-MIDI-HEADER

           open output out-file
           exit paragraph.



       *>  insist on user input until a valid filename is entered
       INIT-OPEN-INPUT.
           perform until cmd-line > spaces
               display "Enter MIDI file: " with no advancing
               accept cmd-line
           end-perform

           *> distribute arguments to in-file (filename) and cs-wf-type (waveform name)
           unstring cmd-line delimited by space
               into in-name, wt-type
           
           open input in-file

           *> '35' status on open means file does not exist
           if in-stat = '35' then
               display '"' function TRIM(in-name, TRAILING) '" does not exist.'
               move spaces to cmd-line
               move '0' to in-stat
               perform INIT-OPEN-INPUT
           end-if
           exit paragraph.



       *>  generate wavetable used as lookup for oscillators
       INIT-MAKE-WAVETABLE.

           *> sin2
           *> Bhaskara I's approximation
           if wt-type = 'sin2' then 
               perform varying i from 1 by 1 until i > WAVETABLE-SIZE
                   compute wt-x = 360 * i / WAVETABLE-SIZE
                   if wt-x < 180 then
                       compute wt-item(i) = 4 * wt-x * (180 - wt-x) / (40500 - wt-x * (180 - wt-x)) * wt-amplitude
                   else
                       compute wt-x = wt-x - 180
                       compute wt-item(i) = -4 * wt-x * (180 - wt-x) / (40500 - wt-x * (180 - wt-x)) * wt-amplitude
                   end-if
               end-perform
               exit paragraph
           end-if


           *> sin3
           *> Taylor's series - 9th order
           if wt-type = 'sin3' then 
               perform varying i from 1 by 1 until i > WAVETABLE-SIZE
                   compute wt-x = TWO-PI * (0.5 - i / WAVETABLE-SIZE)
                   compute wt-item(i) = (wt-x - wt-x ** 3 / 6 + wt-x ** 5 / 120 - wt-x ** 7 / 5040 + wt-x ** 9 / 362880) * wt-amplitude
               end-perform
               exit paragraph
           end-if


           *> sin4
           *> Chebyshev polynomial - 7th order
           if wt-type = 'sin4' then 
               perform varying i from 1 by 1 until i > WAVETABLE-SIZE
                   compute wt-x = TWO-PI * (0.5 - i / WAVETABLE-SIZE)
                   compute wt-item(i) = (0.99999660 * wt-x - 0.16664824 * wt-x ** 3 + 0.00830629 * wt-x ** 5 - 0.00018363 * wt-x ** 7) * wt-amplitude
               end-perform
               exit paragraph
           end-if

           *> tri
           if wt-type = 'tri' then 
               perform varying i from 1 by 1 until i > WAVETABLE-SIZE
                   compute wt-x = 4 * i / WAVETABLE-SIZE
                   if wt-x < 1 then 
                       compute wt-item(i) = wt-x * wt-amplitude
                   end-if
                   if (wt-x >= 1 and wt-x < 3) then 
                       compute wt-item(i) = (2 - wt-x) * wt-amplitude
                   end-if
                   if wt-x >= 3 then
                       compute wt-item(i) = (wt-x - 4) * wt-amplitude
                   end-if
               end-perform
               exit paragraph
           end-if

           *> saw
           if wt-type = 'saw' then 
               perform varying i from 1 by 1 until i > WAVETABLE-SIZE
                   compute wt-item(i) = (-1 + 2 * (i / WAVETABLE-SIZE)) * wt-amplitude
               end-perform
               exit paragraph
           end-if

           *> square
           if wt-type = 'square' then 
               perform varying i from 1 by 1 until i > WAVETABLE-SIZE
                   if i < (WAVETABLE-SIZE / 2) then
                       compute wt-item(i) = -wt-amplitude 
                   else 
                       compute wt-item(i) = wt-amplitude 
                   end-if
               end-perform
               exit paragraph
           end-if
      
           *> noise
           if wt-type = 'noise' then 
               perform varying i from 1 by 1 until i > WAVETABLE-SIZE
                   compute wt-item(i) = (-1 + 2 * function RANDOM()) * wt-amplitude
               end-perform
               exit paragraph
           end-if

           *> sin1 (default)
           *> system implementation
           perform varying i from 1 by 1 until i > WAVETABLE-SIZE
               compute wt-x = TWO-PI * i / WAVETABLE-SIZE
               compute wt-item(i) = function SIN(wt-x) * wt-amplitude
           end-perform

           exit paragraph.



       *>  read MIDI header
       *>  header format:
       *>      <'MThd'>        4 bytes
       *>      <length>        4 bytes
       *>      <track format>  2 bytes
       *>      <# of tracks>   2 bytes
       *>      <time format>   2 bytes

       INIT-READ-MIDI-HEADER.

           *> <'MThd'>
           perform READ-WORD
           if not (read-item(1) = FUNCTION ORD('M') - 1
               and read-item(2) = FUNCTION ORD('T') - 1
               and read-item(3) = FUNCTION ORD('h') - 1
               and read-item(4) = FUNCTION ORD('d') - 1) then
               move 'MIDI file does not begin with a valid header.' to error-message
               perform ERROR-ESCAPE
           end-if

           *> <length> - should always be 6 bytes
           perform READ-WORD
           if not read-item(4) = 6
               move 'MIDI file has abnormal header length.' to error-message
               perform ERROR-ESCAPE
           end-if
      
           *> <track format> and <# of tracks>
           perform READ-WORD
           *> if (read-item(2) > 0 or read-item(4) > 0)
           *>  display 'WARNING: Multi-track rendering is not supported.'
           *>  display 'Tracks will be rendered sequentially.'
           *> end-if
           *> ^ many MIDI files begin with a separate track containing only meta-data and no ticks, so this warning is overused

           *> <time format>
           perform READ-HALFWORD
           if read-item(1) >= 128 then
               *> frames are not variable, defined by microseconds
               compute midi-frame-size = 1000000 / (read-item(1) - 128)
               set midi-tick-rate to read-item(2)
           else
               *> frames are variable, depending on tempo (1 frame = 1 quarter note)
               *> tempo is expected to be set later
               compute midi-tick-rate = read-item(1) * 256 + read-item(2)
           end-if
           exit paragraph.





       *>  MAIN
       *>  executes as loop until end of file
       CS-MAIN.
      
           *> read midi file
           perform MAIN-READ-MIDI-TRACK
      
           *> update oscillators
           perform MAIN-UPDATE-OSC

           *> mix oscillators to next buffer sample
           perform MAIN-MIX-TO-BUFF

           *> at end of buffer, write and play
           if buff-i = BUFFER-SIZE then 
               perform MAIN-OUTPUT-BUFF
           end-if

           *> increment buffer index
           add 1 to buff-i
           if buff-i > BUFFER-SIZE then
               set buff-i to 1
           end-if
           exit paragraph.



       *>  read midi file in sequence
       *>  track format:
       *>      <'MTrk'>    4 bytes
       *>      <length>    4 bytes
       *>      <events>    ...

       MAIN-READ-MIDI-TRACK.

           *> if outside track, enter
           if midi-inside-track not = 1 then
               perform READ-WORD

               if in-eof = 1 then
                   exit paragraph
               end-if

               *> <'MTrk'>
               if read-item(1)       = FUNCTION ORD('M') - 1
                   and read-item(2)  = FUNCTION ORD('T') - 1
                   and read-item(3)  = FUNCTION ORD('r') - 1
                   and read-item(4)  = FUNCTION ORD('k') - 1 then
                   set midi-inside-track to 1
               else
                   move 'Expected start of track.' to error-message
                   perform ERROR-ESCAPE
               end-if

               *> <length> - unused
               perform READ-WORD

               perform MIDI-GET-TIME-TO-NEXT-EVENT
           end-if

           *> check if wait is over, and handle events while it is
           perform until midi-to-next-event > 0
               perform MIDI-HANDLE-EVENT

               if midi-inside-track = 0 then
                   exit paragraph
               end-if

               perform MIDI-GET-TIME-TO-NEXT-EVENT
           end-perform

           *> decrement wait
           subtract 1 from midi-to-next-event
           exit paragraph.



       *>  <delta-time>
       *>  get waiting period in samples
       *>  this is in a VLQ format. see MIDI standard for details
       MIDI-GET-TIME-TO-NEXT-EVENT.
           perform READ-TO-VLQ
           compute midi-to-next-event = read-vlq * midi-frame-size * SAMPLE-RATE / midi-tick-rate / 1000000.
           exit paragraph.



       *>  handle MIDI event/message

       *>  event format:
       *>      <delta-time>    VLQ
       *>      <event code>    1 byte
       *>      for meta events (code 0xFF):
       *>          <type>      1 byte
       *>          <length>    1 byte
       *>          <data>      ...
       *>      for messages:   
       *>          <data>      ...

       MIDI-HANDLE-EVENT.

           *> <delta-time> is handled by READ-MIDI

           *> <event code>
           perform READ-BYTE

           *> meta events (code 0xFF)
           if read-item(1) = 255 then

               *> <type>
               perform READ-BYTE

               *> end of track (type 0x2F)
               if read-item(1) = 47 then
                   set midi-inside-track to 0
               *> <length> is 0
                   perform READ-BYTE
                   exit paragraph
               end-if

               *> change tempo (type 0x51)
               if read-item(1) = 81 then
                   *> <length> is 3
                   perform READ-BYTE
                   *> <data>
                   set read-size to 3
                   perform READ-TO-LONG
                   set midi-frame-size to read-long
                   exit paragraph
               end-if

               *> skip other meta events
               *> <length>
               perform READ-BYTE
               *> <data>
               set read-size to read-item(1)
               perform READ-TO-NONE
               exit paragraph
           end-if

           *> MIDI messages
           *> <event code>
           *> get first nibble
           compute midi-msg-code = read-item(1) / 16

           *> first bit of code should be set
           if midi-msg-code < 8 then 
               display read-item(1)
               move 'Expected MIDI code.' to error-message
               perform ERROR-ESCAPE
           end-if
           

           *> note off (code 0x8?)
           if midi-msg-code = 8 then
               *> <data> - 2 bytes
               perform MIDI-NOTE-OFF
               exit paragraph
           end-if

           *> note on (code 0x9?)
           if midi-msg-code = 9 then
               *> <data> - 2 bytes
               perform MIDI-NOTE-ON
               exit paragraph
           end-if

           *> other message codes are ignored

           *> system exclusive message (0xF0) - unlikely to encounter
           if read-item(1) = 240 then 
               *> contains unknown length of <data> ended by 0xF7
               set read-size to 1
               perform READ-BYTE until (read-item(1) = 247 or in-eof = 1)
               exit paragraph
           end-if
      
           *> otherwise, <data> has a set length for each first nibble
           set read-size to midi-msg-len-item(midi-msg-code)
      
           *> ... with the exception of 0xF2 and 0xF3
           if read-item(1) = 242 then 
               set read-size to 2
           end-if

           if read-item(1) = 243 then 
               set read-size to 1
           end-if
      
           *> skip <data> of ignored messages
           perform READ-TO-NONE

           exit paragraph.



       *>  note on
       *>  format:
       *>      <note>      1 byte
       *>      <velocity>  1 byte
       MIDI-NOTE-ON.
           perform READ-HALFWORD

           *> find inactive osc
           set osc-i to 0
           perform varying i from 1 by 1 until (i > MAX-OSCILLATORS or osc-i > 0)
               if (osc-held(i) = 0 and osc-envelope(i) <= 0) then
                   set osc-i to i
               end-if
           end-perform

           if osc-i = 0
               display 'WARNING: All oscillators in use. Note on is ignored.'
               exit paragraph
           end-if

           *> set osc attributes
           set osc-note(osc-i) to read-item(1)
           compute osc-frequency(osc-i) = 2 ** ((osc-note(osc-i) - 69) / 12) * 440
           compute osc-velocity(osc-i) = read-item(2) / 127 * (OUT-RANGE - 1)
           set osc-phase(osc-i) to 0
           compute osc-phase-inc(osc-i) = osc-frequency(osc-i) / SAMPLE-RATE * WAVETABLE-SIZE * OUT-RANGE
           set osc-held(osc-i) to 1

           exit paragraph.



       *>  note off
       *>  format:
       *>      <note>      1 byte
       *>      <velocity>  1 byte
       MIDI-NOTE-OFF.
           perform READ-HALFWORD

           *> find osc with matching note
           set osc-i to 0
           perform varying i from 1 by 1 until (i > MAX-OSCILLATORS or osc-i > 0)
               if (osc-held(i) = 1 and osc-note(i) = read-item(1)) then 
                   set osc-i to i
               end-if
           end-perform

           *> release osc
           if osc-i > 0 then 
               set osc-held(osc-i) to 0
           end-if
           exit paragraph.
      


       *>  update envelopes and outputs for each oscillator
       MAIN-UPDATE-OSC.
           set osc-active to 0

           perform varying i from 1 by 1 until i > MAX-OSCILLATORS
      
               *> envelopes
               if osc-held(i) = 1 then
                   *> if held, attack env
                   if osc-envelope(i) < OUT-RANGE then 
                       add env-attack-inc to osc-envelope(i)
                   else 
                       set osc-envelope(i) to OUT-RANGE
                   end-if
               else 
                   *> if not held, release env
                   if osc-envelope(i) > 0 then 
                       subtract env-release-dec from osc-envelope(i)
                   end-if
               end-if

               *> calculate output if envelope is open
               if osc-envelope(i) > 0 then
                   set osc-active to 1

                   *> calculate phase
                   add osc-phase-inc(i) to osc-phase(i)
                   if osc-phase(i) > osc-phase-range then
                       subtract osc-phase-range from osc-phase(i)
                   end-if
      
                   *> wavetable lookup
                   set wt-phase to osc-phase(i)
                   perform OSC-WAVETABLE-LOOKUP

                   *> output
                   compute osc-output(i) = wt-output * osc-envelope(i) / OUT-RANGE * osc-velocity(i) / OUT-RANGE
               end-if
           end-perform
           exit paragraph.



       *>  get value from wavetable, using linear interpolation
       *>  arguments: wt-phase
       *>  output: wt-output
       OSC-WAVETABLE-LOOKUP.
      
           *> find sample before and after phase
           compute local-1 = wt-phase / OUT-RANGE
           compute local-2 = local-1 + 1
           if local-2 > WAVETABLE-SIZE then 
               subtract WAVETABLE-SIZE from local-2
           end-if
      
           *> linear interpolation
           compute wt-output = wt-item(local-1) + (wt-item(local-2) - wt-item(local-1)) * (wt-phase / OUT-RANGE - local-1)
           exit paragraph.



       *>  sum every oscillator to get buffer sample
       MAIN-MIX-TO-BUFF.
           set buff-item(buff-i) to 0
           perform varying i from 1 by 1 until i > MAX-OSCILLATORS
               if osc-envelope(i) > 0 then
                   compute buff-item(buff-i) = buff-item(buff-i) + osc-output(i)
               end-if
           end-perform
           exit paragraph.



       *>  called when buffer is filled
       MAIN-OUTPUT-BUFF.

           *> write to pcm file
           write buff.

           *> start audio playback with SDL
           *> see audio_out.c
           if audio-playing = 0 then
               call "audio_start_playback"
               set audio-playing to 1
           end-if

           *> add buffer to SDL audio queue
           call "audio_queue" using
               by reference buff

           *> if reading and rendering is done, request to exit main loop
           if (in-eof = 1 and osc-active = 0) then 
               set main-exit to 1
           end-if

           exit paragraph.





       *>  CLOSE
       CS-CLOSE.

           *> display 'Finished rendering.'
           *> close files
           close in-file
           close out-file

           *> exit when playback has ended
           call "audio_wait_for_queue_end"
           call "audio_close"
           stop run
           exit paragraph.





       *>  read next byte of input
       *>  output:     read-item(1)
       READ-BYTE.
           if in-eof = 0 then 
               perform READ-FILE

               set read-item(1) to in-byte
           end-if
           exit paragraph.

       *>  read next 2 bytes of input
       *>  output:     read-item(1 - 2)
       READ-HALFWORD.
           set read-size to 2
           perform READ-TO-TABLE
           exit paragraph.

       *>  read next 4 bytes of input
       *>  output:     read-item(1 - 4)
       READ-WORD.
           set read-size to 4
           perform READ-TO-TABLE
           exit paragraph.

       *>  read n bytes of input
       *>  arguments:  read-size
       *>  output:     read-item(1 - n)
       READ-TO-TABLE.
           perform varying i from 1 by 1 until (in-eof = 1 or i > read-size)
               perform READ-FILE
               set read-item(i) to in-byte
           end-perform
           exit paragraph.

       *>  read next n bytes into an unsigned long
       *>  arguments:  read-size
       *>  output:     read-long
       READ-TO-LONG.
           set read-long to 0
           perform varying i from 1 by 1 until (in-eof = 1 or i > read-size)
               perform READ-FILE
               compute read-long = read-long * 256 + in-byte
           end-perform
           exit paragraph.

       *>  get variable length quantity
       *>    - see MIDI standard for explanation
       *>  output:     read-vlq
       READ-TO-VLQ.
           set read-vlq to 0
           set i to 1.
           set local-1 to 1.
           perform until (in-eof = 1 or i > 4 or local-1 = 0)
               perform READ-FILE

               set read-item(1) to in-byte
               if read-item(1) >= 128 then
                   subtract 128 from read-item(1)
               else 
                   set local-1 to 0
               end-if
               compute read-vlq = read-vlq * 128 + read-item(1)
               add 1 to i
           end-perform
           exit paragraph.

       *>  advance input file n bytes, no output
       *>  arguments:  read-size
       READ-TO-NONE.
           perform varying i from 1 by 1 until (in-eof = 1 or i > read-size)
               perform READ-FILE
           end-perform
           exit paragraph.
       
       *>  read single byte
       *>  output:     in-byte
       READ-FILE.
           read in-file
               at end
                   move 1 to in-eof
           end-read
           add 1 to in-pos
           exit paragraph.



       *>  print error message and stop execution
       *>  arguments: error-message
       ERROR-ESCAPE.
           display 'ERROR: ' function TRIM(error-message, TRAILING)
           display 'Input file position: ' in-pos
           close in-file
           close out-file
           call 'audio_close'
           stop run.



       stop run.
       end program COBOLSound.
