;; clean part
(defun primep (condidate-num)
  (labels ((reduction (t-num)
             (if (zerop (mod condidate-num t-num))
                 (eql t-num 1)
                 (reduction (1- t-num)))))
    (if (integerp condidate-num)
        (reduction (ash condidate-num -1))
        nil)))

(defun next-prime-number (p-num)
  (labels ((next (condidate-num)
             (if (evenp condidate-num) (next (1+ condidate-num))
                 (if (primep condidate-num)
                     condidate-num
                     (next (1+ condidate-num))))))
    (next (1+ p-num))))

;; darty part
(defparameter *terget-num* 600851475143)

(defun max-prime-factor (product)
  (let ((factor-list nil)
        (quatient nil))
    (labels ((search-prime (target-num condidate-prime-num)
               (if (> target-num condidate-prime-num)
                   (progn
                     (setq quatient (/ target-num condidate-prime-num))
                     (if (integerp quatient)
                         (progn
                           (push condidate-prime-num factor-list)
                           (search-prime quatient condidate-prime-num))
                         (search-prime target-num (next-prime-number condidate-prime-num))))
                   (progn
                     (if (primep target-num)
                         (push target-num factor-list))))))
      (search-prime product 2))
    (apply #'max factor-list)))

;; (max-prime-factor *terget-num*)
