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

    items [
        ^Set from: compartment1,compartment2.
    ]
].

OrderedCollection subclass: Group [
    <shape: #inherit>

    findBadge [
        | sets badge |
        sets := self collect: [ :rucksack | rucksack items ].
        badge := sets fold: [ :a :b | a & b ].
        ^(OrderedCollection from: badge) at: 1
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

    f position: 0.

    groups := OrderedCollection new.
    index := 0.
    f linesDo: [ :line |
                 r := Rucksack new: line.

                 (index rem: 3) = 0
                                ifTrue: [groups add: (Group new).].

                 groups last add: r.

                 index := index + 1.
               ].

    part2 := 0.
    groups do: [ :g |
                 badge := g findBadge.
                 priority := ItemPriority get: badge.
                 part2 := part2 + priority.
               ].

    part2 displayNl.
    f close.
].
