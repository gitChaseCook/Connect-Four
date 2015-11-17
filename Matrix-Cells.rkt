;Chase Cook
;Program  11
;Phase 1
;R.Simms
;11/9/15

;This program contains a small library of functions to manipulate
;a matrix. The two main functions are getCell and setCell.
;getCell retrieves a value from a matrix and setCell modifies
;a value in a matrix.

(define m1 '((2 4 6 8)(1 3 5 7)(2 9 0 1)))


;To retrieve a whole row from a matrix.
(define (getRow M rowNum)
   (if (= rowNum 1)
       (car M)
       (getRow (cdr M) (- rowNum 1))
   )
)


;To retrieve a number/column from a given row.
(define (getCol row colNum) 
   (if (= colNum 1)
       (car row)
       (getCol (cdr row) (- colNum 1))
    )
)


;Retrieves a cell value from a matrix, given a matrix, row number, and column number.
(define (getCell M Row Column)
  (getCol (getRow M Row) Column)
 )

(getCell m1 3 3)



;Helper function for setCell to modify a cell in a given row.
(define (modCell row col Item)
  (if (null? row)
      0
      (if (= col 1)
          (cons Item (cdr row))
          (cons (car row) (modCell (cdr row) (- col 1) Item) )
      )
  )
)


;Changes a cell in a matrix given a matrix, row, column, and item.
(define (setCell Matrix row col Item)
  (if (null? Matrix)
      0
     (if (= row 1)
          (cons (modCell (car Matrix) col Item) (cdr Matrix))
          (cons (car Matrix) (setCell (cdr Matrix) (- row 1) col Item) )
      )
  )

)

(setCell m1 1 3 100)


