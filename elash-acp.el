;;; elash-acp.el --- ACP adapter for elash  -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; Thin ACP client construction helpers and data normalization.
;;
;;; Code:

(require 'cl-lib)
(require 'dash)
(require 's)
(require 'acp)

(defun elash-acp--keywordize (key)
  "Convert camelCase symbol KEY to kebab-case keyword."
  (->> (symbol-name key)
       s-split-words
       (--map (downcase it))
       (s-join "-")
       (concat ":")
       intern))

(defun elash-acp--normalize (data)
  "Convert ACP DATA into pure plists with kebab-case keyword keys.
Vectors become lists, alists become plists."
  (cond
   ((vectorp data) (mapcar #'elash-acp--normalize (append data nil)))
   ((and (listp data) (--all? (consp it) data))
    (--mapcat (list (elash-acp--keywordize (car it))
                    (elash-acp--normalize (cdr it)))
              data))
   ((consp data)
    (cons (elash-acp--normalize (car data))
          (elash-acp--normalize (cdr data))))
   (t data)))

(provide 'elash-acp)
;;; elash-acp.el ends here
