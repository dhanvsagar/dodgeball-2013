#!/usr/bin/clisp

(load "/home/m/Projects/teaching/flp/dodgeball_2013_submissions/ENVIRONMENT/AIMA/aima.lisp")
(aima-load 'all)
(aima-compile)

(setq prefix "/home/m/Projects/teaching/flp/dodgeball_2013_submissions/ENVIRONMENT/AIMA/")
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

(load "<AGENT_PATH>")

(setf *random-state* (make-random-state t))
(test-agent-mode-1 '<AGENT_NAME>)
