## run in subdir because test files there there
EMACS=${EMACS-emacs}
$EMACS -batch -l ./dev/linked-buffer-dot-emacs.el \
    -l ./linked-buffer.el -l ./linked-buffer-block.el -l ./linked-buffer-latex-code.el \
    -l ./dev/linked-buffer-test.el \
    -f ert-run-tests-batch-and-exit
