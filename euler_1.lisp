;; If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
;; Find the sum of all the multiples of 3 or 5 below 1000.

(defun divisible-nump (n sub-multi-num)
  (integerp (/ n sub-multi-num)))

(defun sum-numbers (max)
  (let ((sum 0))
    (mapc (lambda (x)
            (when (or (divisible-nump x 3) (divisible-nump x 5))
              (incf sum x)))
          (loop for i from 1 to (1- max) collect i))
    sum))

;; faster version
(defun sum-divisivle-numbers (n max)
  (let ((upper 0))
    (setf upper (truncate (/ (1- max) n)))
    (/ (* n upper (+ upper 1)) 2)))

(defun sum-numbers-faster (max)
  (- (+ (sum-divisivle-numbers 3 max)
        (sum-divisivle-numbers 5 max))
     (sum-divisivle-numbers 15 max)))
