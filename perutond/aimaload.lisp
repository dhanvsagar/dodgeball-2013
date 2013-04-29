;;; This is the IDE's built-in-editor, where you create and edit
;;; lisp source code.  You could use some other editor instead,
;;; though the IDE's menu-bar commands would not be applicable there.
;;; 
;;; This editor has a tab for each file that it's editing.  You can
;;; create a new editor buffer at any time with the File | New command.
;;; Other commands such as Search | Find Definitions will create
;;; editor buffers automatically for existing code.
;;; 
;;; You can use the File | Compile and Load command to compile and
;;; load an entire file, or compile an individual definition by
;;; placing the text cursor inside it and using Tools | Incremental
;;; Compile.  You can similarly evaluate test expressions in the
;;; editor by using Tools | Incremental Evaluation; the returned
;;; values and any printed output will appear in a lisp listener
;;; in the Debug Window.
;;; 
;;; For a brief introduction to other IDE tools, try the
;;; Help | Interactive IDE Intro command.  And be sure to explore
;;; the other facilities on the Help menu.

(load "/home/daemonx/FIT/magistr/2. semestr/dodgeball/AIMA/aima.lisp")
(aima-load 'all)
(aima-compile)
;(test 'all) ;calling once is enough -- run first time that you use AIMA


(setq prefix "/home/daemonx/FIT/magistr/2. semestr/dodgeball/AIMA/")
(load (concatenate 'string prefix "utilities/utilities.lisp"))
(load (concatenate 'string prefix "utilities/binary-tree.lisp"))
(load (concatenate 'string prefix "utilities/queue.lisp"))
(load (concatenate 'string prefix "utilities/cltl2.lisp"))
(load (concatenate 'string prefix "agents/environments/basic-env.lisp"))
(load (concatenate 'string prefix "agents/environments/grid-env.lisp"))
(load (concatenate 'string prefix "agents/agents/agent.lisp"))
(load (concatenate 'string prefix "agents/algorithms/grid.lisp"))
(load (concatenate 'string prefix "agents/environments/hide-seek.lisp"))

(load (concatenate 'string prefix "agents/environments/dodgeball.lisp"))

;(defun startup-function () (progn (test-agent-mode-1 'perutond) (quit)))
;(EXT:SAVEINITMEM "vybijena" :executable t :init-function 'startup-function)
;(test-agent-mode-1 'perutond-1)
;(dotimes (n 100) 
;  (if (test-agent-mode-1 'perutond-1)
;    (print (format nil "perutond test ~a successfull" n))
;    (print (format nil "perutond test ~a FAILED" n))))
