(load "utils.scm")
(load "ingredient.scm")
(load "data/ingredients.scm")
     
;; Ref: https://stackoverflow.com/questions/55694462/how-do-i-read-a-text-file-in-mit-gnu-scheme
(define (read-lines-to-list filename)
  (with-input-from-file filename
    (lambda ()
      (let loop ((lines '())
                 (next-line (read-line)))
       (if (eof-object? next-line) ; when we hit the end of file
           (reverse lines)         ; return the lines
           (loop (cons next-line lines) ; else loop, keeping this line
            (read-line)))))))       ; and move to next one

;; load ingredients
(define
  raw-ingredients-names
  (read-lines-to-list "data/ingredients_raw.csv"))

(define
  ingredients-names
  (unique-sorted (sort raw-ingredients-names string<?)))

(define
  ingredients-list
  (map make-simple-ingredient ingredients-names))

(define
  ingredients-list-v2
  (map (lambda (item)
        (let ((name (car item))
              (tags (cdr item)))
           (make-ingredient name tags))) tagged-ingredients))


;; Gets the ingredient object for the ingredient with the given name in
;; ingredients-list
(define (ingredient-by-name name)
  (assert (string? name))
  (find (lambda (ingredient)
         (string=? (ingredient-name ingredient) name))
   ingredients-list-v2))

;; TODO probably should start adding relations
