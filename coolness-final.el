;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                      C  O  O  L  N  E  S  S
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'cl)

(defun for-all-for-all (pred x y)
  (block 'outer-for-all-for-all
         (loop for itemX across x
               do (loop for itemY across y
                        do (if (null (funcall pred itemX itemY))
                               (return-from 'outer-for-all-for-all nil))))
         t))


(defun print-args ()
  (let* ((args (getenv "ARGS"))
          (arg-list (split-string args " " t)))
    (dolist (arg arg-list)
      (princ arg)
      (princ "\n"))))

(for-all-for-all (> 2 1) ; should return t
(for-all-for-all (< 1 2) ; should return t
(for-all-for-all (< 2 1) ; should return nil




(defun for-all-for-some (pred x y)
  (block 'outer-for-all
    (loop for itemX across x
          do (if (null (block 'inner-for-some
                         (loop for itemY across y
                               do (if (funcall pred itemX itemY)
                                      (return-from 'inner-for-some t)))
                         nil))
                 (return-from 'outer-for-all nil)))
    t))

(for-all-for-some (> 2 1) ; should return t
(for-all-for-some (< 1 2) ; should return t
(for-all-for-some (< 2 1) ; should return nil




(defun for-some-for-all (pred x y)
  (block 'outer-for-some
    (loop for itemX across x
          do (if (block 'inner-for-all
                         (loop for itemY across y
                               do (if (null (funcall pred itemX itemY))
                                      (return-from 'inner-for-all nil)))
                         t)
                 (return-from 'outer-for-some t)))
    nil))




(for-some-for-all (> 2 1) ; should return t
(for-some-for-all (> 1 2) ; should return nil
(for-some-for-all (> 2 1) ; should return t




(defun for-some-for-some (pred x y)
  (block 'outer-for-some-for-some
    (loop for itemX across x
          do (loop for itemY across y
                   do (if (funcall pred itemX itemY)
                   (return-from 'outer-for-some-for-some t))))
    nil))

(for-some-for-some (> 2 1) ; should return t
(for-some-for-some (< 2 1) ; should return nil
(for-some-for-some (< 1 2) ; should return t



(defun catch-all ()
  (cond ((null argv)
         (message "Which test(s) would you like to try?"))
        ((equal (elt argv 0) "TAA")
         (message "[4 5 6] should be greater than [1 2 3]")
         (for-all-for-all '> [4 5 6] [1 2 3])))
)
