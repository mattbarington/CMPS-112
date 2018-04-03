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
"---------------------------------------------------------------------"
"(Smalltalk arguments size) < 2 ifTrue: [
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
]."
"---------------------------------------------------------------------"


infile := FileStream open: (Smalltalk arguments at: 2)
                  mode: FileStream read.
inBits := BitStream new: infile.
stack := OrderedCollection new.


[[inBits atEnd] whileFalse: [
    bit := inBits nextBit.
    (bit = 0) ifTrue: [
    byte := 0.
    1 to: 8 do: [:i| byte := byte bitAt: (9 - i) put: (inBits nextBit)].
     (byte = 0) ifTrue: [
      oneMore := inBits nextBit.
      (oneMore = 1) ifTrue: [
        stack add: (Leaf new: 256 count: 0).
      ].
     ] ifFalse: [
       stack add: (Leaf new: byte count: 0).
     ].
    ] ifFalse: [
      oneLeaf := stack removeLast.
      zeroLeaf := stack removeLast.
      stack add: (Tree new: 0 count: 1 left: zeroLeaf right: oneLeaf).
    ]
 ]] on: SystemExceptions.EmptyCollection do: [:skip |
    root := oneLeaf.
  ].
  pointyboi := root.

  (Smalltalk arguments size < 3)
    ifTrue: [outy := stdout]
    ifFalse: [outy := FileStream
                      open: (Smalltalk arguments at: 3)
                      mode: FileStream write].

  [inBits atEnd] whileFalse: [
    pointyboi count = 1 ifTrue: [
      (inBits nextBit = 0) ifTrue: [
        pointyboi := pointyboi getLeft.
      ] ifFalse: [
        pointyboi := pointyboi getRight.
      ]
    ] ifFalse: [
    pointyboi char = 256 ifFalse: [
    outy << (pointyboi char) asCharacter.
  ].
    pointyboi := root.
    ]
  ].
