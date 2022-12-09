Object subclass: Round [
    | them me desiredOutcome |

    Round class >> new: theirMove me: myMove [
        | r |
        r := super new.
        r init: theirMove me: myMove.
        ^r
    ]

    Round class >> letterToMove: letter [
        | mappings |
        mappings := Dictionary from: {
                            'A' -> #rock.
                            'B' -> #paper.
                            'C' -> #scissors.
                            'X' -> #rock.
                            'Y' -> #paper.
                            'Z' -> #scissors.
                     }.
        ^mappings at: letter
    ]

    Round class >> letterToOutcome: letter [
        | mappings |
        mappings := Dictionary from: {
                            'X' -> #lose.
                            'Y' -> #draw.
                            'Z' -> #win.
                     }.
        ^mappings at: letter
    ]

    init: theirMove me: myMove [
        them := Round letterToMove: theirMove.
        me := Round letterToMove: myMove.
        desiredOutcome := Round letterToOutcome: myMove
    ]

    Round class >> moveScore: move [
        | r |
        move = #rock ifTrue: [ r := 1 ].
        move = #paper ifTrue: [ r := 2 ].
        move = #scissors ifTrue: [ r := 3 ].
        ^r
    ]

    planAScore [
        | r |
        them = me
             ifTrue: [ r := 3 ]
             ifFalse: [
                 ((them = #rock and: [ me = #scissors ]) or:
                      [(them = #paper and: [ me = #rock ]) or:
                           [(them = #scissors and: [ me = #paper ])]])
                     ifTrue: [ r := 0 ]
                     ifFalse: [ r := 6 ]
             ].
        ^r
    ]

    planBScore [
        | mappings outcomeScore myMoveScore |
        mappings := Dictionary from: {
                            #lose -> 0.
                            #draw -> 3.
                            #win -> 6.
                     }.
        outcomeScore := mappings at: desiredOutcome.

        mappings := Dictionary from: {
                            { #lose. #rock } -> #scissors.
                            { #lose. #paper } -> #rock.
                            { #lose. #scissors } -> #paper.
                            { #draw. #rock } -> #rock.
                            { #draw. #paper } -> #paper.
                            { #draw. #scissors } -> #scissors.
                            { #win. #rock } -> #paper.
                            { #win. #paper } -> #scissors.
                            { #win. #scissors } -> #rock.
                        }.

        myMoveScore := Round moveScore: (mappings at: {desiredOutcome. them}).
        ^outcomeScore + myMoveScore
    ]

    scorePlanA [
        ^(Round moveScore: me) + self planAScore
    ]

    scorePlanB [
        ^self planBScore
    ]
]

Object subclass: RpsStrategy [
    | rounds |
    RpsStrategy class >> new [
        | r |
        r := super new.
        r init.
        ^r
    ]

    init [
        rounds := Bag new.
    ]

    addMove: theirMove myResponse: myMove [
        rounds add: (Round new: theirMove me: myMove).
    ]

    planA [
        | score |
        score := 0.
        rounds do: [ :round | score := score + round scorePlanA].
        ^score
    ]

    planB [
        | score |
        score := 0.
        rounds do: [ :round | score := score + round scorePlanB].
        ^score
    ]
]

Eval [
    f := FileStream open: './input' mode: FileStream read.
    rps := RpsStrategy new.

    f linesDo: [ :line |
                 moves := line substrings: ' '.
                 rps addMove: (moves at: 1) myResponse: (moves at: 2)
               ].

    rps planA displayNl.
    rps planB displayNl.

    f close.
].
