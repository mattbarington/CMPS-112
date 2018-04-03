#!/usr/local/bin/gst -f
"#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f"

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
      ^ (Character space < self) & (self <= $~)
   ]
   asHexadecimal [
   |q r h|
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
   position [
      ^myStream position
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
   isALeaf [
      ^ true
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
   isALeaf [
      ^ false.
   ]
]

(Smalltalk arguments size) < 2 ifTrue: [
  stderr << 'Error. Too few arguments: No file to read in' << nl.
  ObjectMemory quit: 1
].

|opt_c opt_u opt_t| "opt_d|"
opt_c := false.
opt_t := false.
opt_u := false.

Smalltalk arguments: '-d -c -u -t'
do: [:opt :arg |
   "opt = $d ifTrue: [opt_d := true]."
   opt = $c ifTrue: [opt_c := true].
   opt = $t ifTrue: [opt_t := true].
   opt = $u ifTrue: [opt_u := true].
] ifError: [
  'Invalid option'. displayNl.
].

opt_c & opt_t ifTrue: [
  stderr << '-t and -c options are mutually exclusive.' << nl.
  ObjectMemory quit: 1.
].
opt_c & opt_u ifTrue: [
  stderr << '-u and -c options are mutually exclusive.' << nl.
  ObjectMemory quit: 1.
].

opt_c & opt_u ifTrue: [
  stderr << '-u and -c options are mutually exclusive.' << nl.
  ObjectMemory quit: 1.
].

"-------------------------------------------------------------"
"Decompression"
opt_u ifTrue: [
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
          stack addFirst: (Leaf new: 256 count: 0).
        ] ifFalse: [
          stack addFirst: (Leaf new: 0 count: 0).
        ].
     ] ifFalse: [
       stack addFirst: (Leaf new: byte count: 0).
     ].
    ] ifFalse: [
      oneLeaf := stack removeFirst.
      zeroLeaf := stack removeFirst.
      stack addFirst: (Tree new: 0 count: 1
        left: zeroLeaf right: oneLeaf).
    ]
 ]] on: SystemExceptions.EmptyCollection do: [:skip |
    root := oneLeaf.
  ].
  pointyboi := root.
  (Smalltalk arguments size < 3)
    ifTrue: [outy := OutBits new: stdout]
    ifFalse: [outy := OutBits new: (FileStream
                      open: (Smalltalk arguments at: 3)
                      mode: FileStream write)].
  [inBits atEnd] whileFalse: [
    pointyboi isALeaf ifFalse: [
      (inBits nextBit = 0) ifTrue: [
        pointyboi := pointyboi getLeft.
      ] ifFalse: [
        pointyboi := pointyboi getRight.
      ]
    ] ifTrue: [
      pointyboi char = 256 ifFalse: [
        8 to: 1 by: -1 do: [:bit |
          outy writeBit: (pointyboi char bitAt: bit).
        ].
        pointyboi := root.
      ] ifTrue: [ "If eof is decoded, read no more."
        ObjectMemory quit: 0.
    ].
    ]
  ].
  ObjectMemory quit: 0.
].
"-------------------------------------------------------------"
"Part (a)"

freq := ZeroArray new: 257.
infile := FileStream open: (Smalltalk arguments at: 2)
                    mode: FileStream read.
[infile atEnd] whileFalse:[
  nexty := infile next.
  freq incr: nexty asInteger.
].
infile close.
freq incr: 256.

"Part (b)"
sorty := SortedCollection new.
freq keysAndValuesDo: [:key :val |
  (val notNil and: [val > 0]) ifTrue: [
    sorty add: (Leaf new: key count: val).
  ]
].

"Part (c)"
[sorty size > 1] whileTrue: [
  |first second|
  first := sorty removeFirst.
  second := sorty removeFirst.
  nt := Tree new: (first char) count: (first count + second count)
        left: first right: second.
  sorty add: nt.
].

codingTree := sorty removeFirst.
stringTable := ZeroArray new: 257.
codingTree inorder: [:char :string |
  stringTable at: char asInteger put: string] prefix: ''.

"-------------------------------------------------------------"
"Part (e)"
opt_t ifTrue: [
  stringTable keysAndValuesDo: [:key :val |
    |c n|
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

hecka := $c.

"-------------------------------------------------------------"

opt_c ifTrue: [
"  opt_d := (Smalltalk arguments at: 1) = '-dc'."
  (Smalltalk arguments size < 3)
    ifTrue: [outy := OutBits new: stdout]
    ifFalse: [outy := OutBits new: (FileStream
                                open: (Smalltalk arguments at: 3)
                                mode: FileStream write)].
  codingTree postorder: [:char :count|
    (count = (freq at: char asInteger)) ifTrue: [
      "The case for all Leaves"
      outy writeBit: 0.
      8 to: 1 by: -1 do: [:bit |
        outy writeBit: (char bitAt: bit).
      ].
      "The special case char = 0 or eof"
      (char = 256) ifTrue: [
        outy writeBit: 1].
      (char = 0) ifTrue: [
        outy writeBit: 0].
    ] ifFalse: [
      outy writeBit: 1.
    ]
  ].
  outy writeBit: 1.
  infile := FileStream open: (Smalltalk arguments at: 2)
                      mode: FileStream read.
  [infile atEnd] whileFalse: [
    nexty := infile next asInteger.
    (stringTable at: nexty) do: [:c|
        outy writeBit: c asInteger.
      ].
  ].
  (stringTable at: 256) do: [:c|
      outy writeBit: c asInteger.
    ].
  outy flushByte.
  infile close.
  outy close.
  ObjectMemory quit: 0.
].






















"endything delete me pls"
