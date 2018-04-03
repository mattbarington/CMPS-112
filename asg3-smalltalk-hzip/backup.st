#!/usr/local/bin/gst -f

nl := Character nl.

Object subclass: ZeroArray [
  |theArray|

  ZeroArray class >> new [self shouldNotImplement]

  ZeroArray class >> new: size [
    ^ (super new) init: size
  ]
  init: size [
    theArray := Array new: size.
  ]
  size [
    ^ theArray size.
  ]
  at: index [
    ^ theArray at: index + 1
  ]
  at: index put: value [
    ^ theArray at: index + 1 put: value.
  ]
  incr: index [
    (self at: index) isNil
      ifTrue: [ self at: index put: 0].
    self at: index put: (self at: index) + 1
  ]
  keysAndValuesDo: aBlock [
    (0 to: (self size) - 1) do: [:index |
      aBlock value: index value: (self at: index).
      ]
    ]
]

Character extend [
   isPrint [
      ^ (Character space <= self) & (self <= $~)
   ]
   asHexadecimal [
   | hexKey q r h|
   h := #('0' '1' '2' '3' '4' '5' '6' '7' '8' '9'
   'A' 'B' 'C' 'D' 'E' 'F').
   q := (self asInteger) // 16.
   r := (self asInteger) - (16 * q).
   ^ 'x', (h at: (q+1)), (h at: (r+1)).
   ]
   visible [
      self isPrint ifTrue: [^ self asString]
            ifFalse: [(self asInteger = 256)
                ifTrue: [^ 'EOF']
                ifFalse: [^ self asHexadecimal]
            ]
   ]
]

Object subclass: OutBits [
   |bitIndex currentByte myStream|
   OutBits class >> new [
      self shouldNotImplement.
   ]
   OutBits class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   clearByte [
      bitIndex := 8.
      currentByte := 0.
   ]
   init: fileStream [
      myStream := fileStream.
      self clearByte.
   ]
   flushByte [
      bitIndex = 8 ifFalse: [
         myStream nextPutByte: currentByte.
         self clearByte.
      ]
   ]
   writeBit: bit [
      currentByte := currentByte bitAt: bitIndex put: bit.
      bitIndex := bitIndex - 1.
      bitIndex = 0 ifTrue: [self flushByte].
   ]
   close [
      myStream close.
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

Object subclass: Leaf [
   |char count|
   char [ ^ char ]
   count [ ^ count ]

   Leaf class >> new [
      self shouldNotImplement
   ]

   Leaf class >> new: aChar count: aCount [
      |result|
      result := super new.
      result setChar: aChar andCount: aCount.
      ^result
   ]

   setChar: aChar andCount: aCount [
      char := aChar.
      count := aCount.
   ]

   <= other [
      ^ (count < other count)
      | ((count = other count) & (char <= other char))
   ]

   printBase: aStream [
      ^ aStream << self class << '(' << char << ',' << count
   ]

   printOn: aStream [
      (self printBase: aStream) << ')'.
   ]

   inorder: visitor prefix: string [
      visitor value: char value: string.
   ]

   postorder: visitor [
      visitor value: char value: count
   ]
]

Leaf subclass: Tree [
   |left right|

   Tree class >> new: aChar count: aCount [
      self shouldNotImplement
   ]

   Tree class >> new: aChar count: aCount left: aLeft right: aRight [
      |result|
      result := super new: aChar count: aCount.
      result setLeft: aLeft andRight: aRight.
      ^ result
   ]

   setLeft: aLeft andRight: aRight [
      left := aLeft.
      right := aRight.
   ]

   getLeft [
      ^ left.
   ]
   getRight [
      ^ right.
   ]

   printOn: aStream [
      (self printBase: aStream) << ',' << left << ',' << right << ')'.
   ]

   inorder: visitor prefix: string [
      left inorder: visitor prefix: string, '0'.
      right inorder: visitor prefix: string, '1'.
   ]

   postorder: visitor [
      left postorder: visitor.
      right postorder: visitor.
      visitor value: char value: count.
   ]
]

"------------------------------------------------------------------"

(Smalltalk arguments size) < 2 ifTrue: [
  stderr << 'Error. Too few arguments: No file to read in' << nl.
  ObjectMemory quit: 1
].

optd := false.
optc := false.
optt := false.
optu := false.

Smalltalk arguments: '-d -c -u -t'
do: [:opt :arg |
   opt = $d ifTrue: [optd := true].
   opt = $c ifTrue: [optc := true].
   opt = $t ifTrue: [optt := true].
   opt = $u ifTrue: [optu := true].
] ifError: [
  'Invalid option YACUNT'. displayNl.
].

optd ifTrue: [
  stdout << 'd:' << optd << nl.
  stdout << 'c:' << optc << nl.
  stdout << 't:' << optt << nl.
  stdout << 'u:' << optu << nl.
].


optc & optt ifTrue: [
  stderr << '-t and -c options are mutually exclusive.' << nl.
  ObjectMemory quit: 1.
].
optc & optu ifTrue: [
  stderr << '-u and -c options are mutually exclusive.' << nl.
  ObjectMemory quit: 1.
].

optc & optu ifTrue: [
  stderr << '-u and -c options are mutually exclusive.' << nl.
  ObjectMemory quit: 1.
].


infile := FileStream open: (Smalltalk arguments at: 2)
                  mode: FileStream read.
inBits := BitStream new: infile.
stack := OrderedCollection new.


[[inBits atEnd] whileFalse: [
    bit := inBits nextBit.
    (bit = 0) ifTrue: [
    byte := 0.
    1 to: 8 do: [:i| byte := byte bitAt: (9 - i) put: (inBits nextBit)].
     "stdout << byte << nl."
     (byte = 0) ifTrue: [
      oneMore := inBits nextBit.
      (oneMore = 1) ifTrue: [
        "stdout << 'pushing 256 to the stack' << nl."
        stack add: (Leaf new: 256 count: 0).
      ].
     ] ifFalse: [
       "stdout << 'pushing ' << byte asCharacter visible << ' to the stack' << nl."
       stack add: (Leaf new: byte count: 0).
     ].
    ] ifFalse: [
      oneLeaf := stack removeLast.
      zeroLeaf := stack removeLast.
      "oneTree := Leaf new: oneLeaf count: 0.
      zeroTree := Leaf new: zeroLeaf count: 0."
      stack add: (Tree new: 0 count: 1 left: zeroLeaf right: oneLeaf).
    ]
 ]] on: SystemExceptions.EmptyCollection do: [:skip |
    root := oneLeaf.
    "stdout << oneLeaf << nl."
  ].
  pointyboi := root.

  (Smalltalk arguments size < 3)
    ifTrue: [outy := stdout]
    ifFalse: [outy := FileStream
                      open: (Smalltalk arguments at: 3)
                      mode: FileStream write].

  [inBits atEnd] whileFalse: [
  "Leaf nodes will have a count of 1.
   Interior nodes will have count of 0."
   "stdout << (pointyboi count) << nl."
    pointyboi count = 1 ifTrue: [
      (inBits nextBit = 0) ifTrue: [
        "stdout << 'going left lmao' << nl."
        pointyboi := pointyboi getLeft.
      ] ifFalse: [
        "stdout << 'goingRight lmao' << nl."
        pointyboi := pointyboi getRight.
      ]
    ] ifFalse: [
    pointyboi char = 256 ifFalse: [
    outy << (pointyboi char) asCharacter.
  ].
    "stdout << 'pointyBoi: ' << pointyboi << nl."
    pointyboi := root.
    ]
  ].


ObjectMemory quit: 0.



optu ifFalse:[

freq := ZeroArray new: 257.
infile := FileStream open: (Smalltalk arguments at: 2)
                    mode: FileStream read.
[infile atEnd] whileFalse:[
  nexty := infile next.
  freq incr: nexty asInteger.
].
infile close.
freq incr: 256.

].
"[stdin atEnd] whileFalse: [
  freq incr: stdin next asInteger.
]."

"freq keysAndValuesDo: [:key :val |
  (val notNil and: [val > 0]) ifTrue: [
    stdout << 'freq[' << key asCharacter << ']=' << val << nl.
  ]
]."
optu ifFalse:[

sorty := SortedCollection new.
freq keysAndValuesDo: [:key :val |
  (val notNil and: [val > 0]) ifTrue: [
    sorty add: (Leaf new: key count: val).
  ]
].

].

optu ifFalse:[

  [sorty size > 1] whileTrue: [
    |first second|
    first := sorty removeFirst.
    second := sorty removeFirst.
    nt := Tree new: (first char) count: (first count + second count)
        left: first right: second.
    sorty add: nt.
  ].

].



codingTree := sorty removeFirst.
stringTable := Array new: 257.
codingTree inorder: [:char :string |
  stringTable at: char asInteger put: string] prefix: ''.



optt ifTrue: [
  stringTable keysAndValuesDo: [:key :val |
    (val notNil and: [val > 0]) ifTrue: [
      c := key asCharacter.
      n := freq at: key asInteger.
      (c isPrint) ifTrue: [ stdout << ' ' << c << '   ']
      ifFalse: [ stdout << c visible << '  ' ].
      n asString size to: 5 do: [:skip| stdout << ' '].
      stdout << n << '  ' << val << nl.
    ]
  ].
  ObjectMemory quit: 0.
].


optc ifTrue: [
  optd ifTrue: [stdout << 'Outputting encoding tree' << nl].
  (Smalltalk arguments size < 3)
    ifTrue: [outy := OutBits new: stdout]
    ifFalse: [outy := OutBits new: (FileStream
                                open: (Smalltalk arguments at: 3)
                                mode: FileStream write)].
    "outy := OutBits new: (FileStream
                      open: (Smalltalk arguments at: 3)
                      mode: FileStream write)]."

  codingTree postorder: [:char :count|
    (count = (freq at: char asInteger)) ifTrue: [
      (char < 256) & (char > 0) ifFalse: [
      "  optd ifTrue: [stdout << 0 << nl.]."
        outy writeBit: 0.
        bit := 1 to: 8 do: [:bit |
          "optd ifTrue: [stdout << 0.]."
          outy writeBit: 0.
        ].
          (char = 256)
          ifTrue: ["stdout << 1." outy writeBit: 1]
          ifFalse: ["stdout << 0." outy writeBit: 0].
          "optd ifTrue: [stdout << ': ' << char asCharacter visible << nl.]."
      ] ifTrue: [
        "optd ifTrue: [stdout << '0' << nl.]."
        outy writeBit: 0.
        bit := 8 to: 1 by: -1 do: [:bit |
          "optd ifTrue: [stdout << (char bitAt: bit).]."
          outy writeBit: (char bitAt: bit).
        ].
      "  optd ifTrue: [stdout << ': ' << char asCharacter visible << nl.]."
      ].
    ]
    ifFalse: [
      outy writeBit: 1.
      "optd ifTrue: [stdout << '1 interior' << nl.]."
    ]
  ].
  optd ifTrue: [stdout << 'Writing out dummy 1.' << nl].
  "Dummy 1 to cause stack underflow"
  outy writeBit: 1.
  "optd ifTrue: [stdout << '1 yee' << nl.]."
  optd ifTrue: [stdout << 'Encoding tree complete.' << nl].
  infile := FileStream open: (Smalltalk arguments at: 2)
                      mode: FileStream read.
  [infile atEnd] whileFalse: [

    nexty := infile next asInteger.
    "optd ifTrue: [stdout << (stringTable at: nexty) << nl.]."
    (stringTable at: nexty) do: [:c|
        "stdout << c."
        outy writeBit: c asInteger.
      ].
    "stdout << nl << nl."
  ].
  (stringTable at: 256) do: [:c|
      "stdout << c."
      outy writeBit: c asInteger.
    ].
  outy flushByte.
  infile close.
  outy close.
  optd ifTrue: [stdout << 'Compression complete. Exiting program' << nl].
  ObjectMemory quit: 0.
].








"[stack size > 1] whileTrue: [
    |first second|
    first := stack removeLast.
    second := stack removeLast.
    nt := Tree new: 0 count: 0
      left: second right: first.
    stack add: nt.
]."

"tree := stack removeLast.
tree inspect."



"
stdout << nl << 'This is the encoding tree' << nl.

"




















"endything delete me pls"
