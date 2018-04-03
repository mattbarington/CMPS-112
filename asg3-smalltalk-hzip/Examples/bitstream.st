#!/usr/local/bin/gst -f
"#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f
$Id: bitstream.st,v 1.6 2016-10-13 13:08:26-07 - - $"
execname := '$RCSfile: bitstream.st,v $' =~ '.*: (.*),v.*' at: 1.

Character extend [
   isGraph [
      ^ (Character space < self) & (self <= $~)
   ]
   visible [
      self isGraph ifTrue: [^ '$', self asString]
                   ifFalse: [^ self asInteger printStringRadix: 16]
   ]
]

Object subclass: BitStream [
   |bitIndex byte myStream|
   BitStream class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   init: fileStream [
      myStream := fileStream.
      bitIndex := 1.
   ]
   nextBit [
      bitIndex = 1 ifTrue: [
         byte := myStream next.
         bitIndex := 9.
      ].
      bitIndex := bitIndex - 1.
      ^byte value bitAt: bitIndex
   ]
   atEnd [
      ^bitIndex = 1 and: [myStream atEnd]
   ]
   currByte [
      ^byte
   ]
]

nl := Character nl.
stdinBits := BitStream new: stdin.
count := 0.
char := 0.
[stdinBits atEnd] whileFalse: [
    bit := stdinBits nextBit.
   stdout << bit.
   count := count + 1.
   char := char bitAt: (9 - count) put: bit.
   count = 4 ifTrue: [stdout << ' '].
   count = 8 ifTrue: [
      stdout << ' ' << stdinBits currByte visible << ' ' << char << nl.
      count := 0
   ].
]

"TEST: echo hello world | ./bitstream.st 2>&1"
