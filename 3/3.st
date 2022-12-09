Object subclass: Rucksack [
    | compartment1 compartment2 |

    Rucksack class >> new: items [
        | r |
        r := super new.
        r init: items.
        ^r
    ]

    init: items [
        | len |
        len := items size.
        compartment1 := items copyFrom: 1 to: len / 2.
        compartment2 := items copyFrom: len / 2 + 1.
    ]

    itemInBothCompartments [
        compartment1 do: [ :item |
                           (compartment2 includes: item)
                           ifTrue: [^item]
                         ]
    ]
].

Object subclass: ItemPriority [
    ItemPriority class >> get: item [
        | ascii |
        ascii := item asciiValue - 65.
        ascii < 26
              ifTrue: [ ^ascii + 27 ]
              ifFalse: [ ^ascii - 31 ].
    ]
]

Eval [
    f := FileStream open: './input' mode: FileStream read.

    part1 := 0.

    f linesDo: [ :line |
                 r := Rucksack new: line.
                 priority := ItemPriority get: (r itemInBothCompartments).
                 part1 := part1 + priority
               ].

    part1 displayNl.

    f close.
].
