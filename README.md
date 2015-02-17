[![Build Status](https://travis-ci.org/phillord/lentic.svg?branch=master)](https://travis-ci.org/phillord/lentic)

Lentic allows two buffers to share the same or similar content but
otherwise operate independently. This allows multi-modal editing, both with
identical text, or with related but different text. It's somewhat similar to
Emacs indirect buffers but more powerful.

While lentic could be used for many purposes, the original motivation is to
enable a literate programming environment. The lentic documentation is
self-hosting -- the source code contains all of the documentation. It can be
generated and viewed using the menu items (Edit->Lentic->Read Doc), or it can
be viewed at
http://homepages.cs.ncl.ac.uk/phillip.lord/lentic/lenticular.html.

Previously, lentic was known as linked-buffer.

## Contributions

I welcome contributions to lentic. I would like to contribute lentic to ELPA
or the Emacs core in the future, so would prefer that contributors has
completed the relevant paperwork with the FSF.


## ChangeLog

### 0.8

This buffer introduces multiple lentic buffers. This is important because
lentic has more than one use -- both for maintaining two syntactic views (for
example for literate programming) and for maintaining two identical persistent
views (for example, for editing a file in two places at once). It's now
possible to do both of these at the same time. Specific support has been added
for views in lentic-mode.

This necessitates a change to the return value of lentic-clone to enable the
percolation of changes between several buffers. In this default usage of two
lentic buffers this will have no effect.

#### New Features

- Multiple lentic buffers are now possible.
- Features to create new views.

#### Breaking Changes

- The return value of lentic-clone has now changed.

### 0.7

#### New Features

- haskell->latex support added
- Full Documentation System added
- Lentic buffers can be auto-killed now
- Rot13 added after many requests
- Mode lighter now shows when a lentic buffer exists.
- lentic-block shows invalid states in the mode-line

#### Breaking Changes

lentic-start-mode has now gone, and been combined with lentic-mode. A new
global-lentic-mode has been introduced which you should use instead.

#### Bug Fix

- "swap" menu item now functional
- lentic-mode actually does something now.

### 0.6.1

#### Bug Fix

- documentation updates for autoload error
- lentic-mode.el now requires lentic.el

### 0.6

The main feature of this release is that changes percolation now happens
incrementally, so only those parts of the buffer are updated. As a result,
lentic now cope with long files with little noticable delay.

The files in the package have been re-organised. All the user interaction code
is now in lentic-mode.el, meaning that lentic can be used as a library if
required. Development code is now in lentic-dev.el.

This package has now been renamed from linked-buffer to lentic.

Finally, the documentation in this package is now self-hosting, using orgel
comments. See lentic-org.el for further details.

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
