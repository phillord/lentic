Linked-buffer allows two buffers to share the same or similar content but
otherwise operate independently. This allows multi-modal editing.

For more details see linked-buffer.el


## ChangeLog

### 0.5

#### Features
- org to emacs-lisp support
- Asciidoc support for lisp
- More Tests added
- Full testing framework implemented

#### Bug Fix

- pabbrev handling was broken

### 0.4

Some bug fixes and asciidoc->clojure support added.

### 0.3

This release is mainly a bug fix of 0.2, but also includes support for delayed
cloning.

#### Features

First addition of linked-buffer-delayed.el enabling buffer cloning in the idle
thread.

#### Bug Fixes

A typo in linked-buffer-block.el prevented it from working. This has now been fixed.


### 0.2

The configuration system has been totally written using EIEIO objects
which should scale better into the future.
