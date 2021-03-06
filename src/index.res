open Belt.Array

module Board = {
  let printQueens = board => {
    let strings = board->map(row => {
      row->map(col => {
        if col === 1 {
          "Q"
        } else {
          "x"
        }
      })
    })

    strings->Js.Array2.forEach(row => Js.log(Js.Array2.joinWith(row, " ") ++ " "))
  }

  let isDimensionOcccupied = d => {
    d->reduce(0, (acc, v) => acc + v) > 0
  }

  let isSolution = (a): bool => {
    let d =
      a
      ->map(row => isDimensionOcccupied(row))
      ->reduce(0, (acc, v) => {
        switch v {
        | true => acc + 1
        | false => acc
        }
      })

    d === a->length
  }

  let isRowOccupied = isDimensionOcccupied

  let isColumnOccupied = (rows, columnIndex) => {
    rows
    ->map(row => {
      row[columnIndex]
    })
    ->isDimensionOcccupied
  }

  let isDiagonalOccupied = (rows, rowIndex, columnIndex, _n) => {
    let isCheckTrue = ref(false)

    let i = ref(_n)
    let cursorIndex1 = ref(rowIndex)
    let cursorIndex2 = ref(columnIndex)

    // check left and up diagonal
    while (
      !isCheckTrue.contents &&
      i.contents >= 0 &&
      cursorIndex1.contents > 0 &&
      cursorIndex2.contents > 0
    ) {
      cursorIndex1 := cursorIndex1.contents - 1
      cursorIndex2 := cursorIndex2.contents - 1

      if rows[cursorIndex1.contents][cursorIndex2.contents] === 1 {
        isCheckTrue := true
      } else {
        i := i.contents - 1
      }
    }

    let cursorIndex1 = ref(rowIndex)
    let cursorIndex2 = ref(columnIndex)
    let j = ref(_n)

    // check right and up diagonal
    while (
      !isCheckTrue.contents &&
      j.contents >= 0 &&
      cursorIndex1.contents > 0 &&
      cursorIndex2.contents < _n - 1
    ) {
      cursorIndex1 := cursorIndex1.contents - 1
      cursorIndex2 := cursorIndex2.contents + 1

      if rows[cursorIndex1.contents][cursorIndex2.contents] === 1 {
        isCheckTrue := true
      } else {
        j := j.contents - 1
      }
    }

    isCheckTrue.contents
  }

  let placeQueen = (_rows, n, firstIndex) => {
    let rec _placeQueen = (rowIndex, colIndex) => {
      let ci = ref(colIndex)
      let nextRow = _rows[rowIndex]

      while n > ci.contents {
        if (
          isColumnOccupied(_rows, ci.contents) ||
          isDiagonalOccupied(_rows, rowIndex, ci.contents, n)
        ) {
          ci := ci.contents + 1
        } else {
          nextRow[ci.contents] = 1
          ci := n + 1
          if rowIndex + 1 < n {
            _placeQueen(rowIndex + 1, 0)
          }
        }
      }
    }

    _placeQueen(0, firstIndex)
    _rows
  }

  let make = n => {
    let range = Belt.Array.range(0, n - 1)

    range->forEachWithIndex((i, _) => {
      let rows = range->map(_ => {
        range->map(_ => {
          0
        })
      })

      let r = placeQueen(rows, n, i)

      if isSolution(r) {
        Js.log("Solution .......")
        printQueens(r)
      }
    })
  }
}

let r = Board.make(5)
