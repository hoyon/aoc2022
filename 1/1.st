Object subclass: Calories [
    | elves |
    Calories class >> new [
        | r |
        r := super new.
        r init.
        ^r
    ]

    init [
        elves := OrderedCollection new.
    ]

    addFoodItem: calories [
        | newVal |
        newVal := (elves at: (elves size)) + calories.
        elves at: (elves size) put: newVal.
    ]

    nextElf [
        elves addLast: 0.
    ]

    fromTop: positionFromTop [
        | sorted |
        sorted := SortedCollection from: elves.

        ^sorted at: (sorted size - positionFromTop)
    ]

    highest [
        ^self fromTop: 0
    ]

    topThreeSum [
        ^(self fromTop: 0) + (self fromTop: 1) + (self fromTop: 2)
    ]
]

Eval [
    f := FileStream open: './input' mode: FileStream read.
    c := Calories new.

    c nextElf.

    f linesDo: [ :line |
                 line = ''
                   ifTrue: [c nextElf]
                   ifFalse: [c addFoodItem: (line asInteger)]
               ].
    f close.

    c highest displayNl.
    c topThreeSum displayNl.
]
