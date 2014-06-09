# linked-buffer.el --- One buffer as a view of another 

## Preamble

- One buffer as a view of another
- lexical-binding: t
- This file is not part of Emacs

Author: Phillip Lord <phillip.lord@newcastle.ac.uk>
Maintainer: Phillip Lord <phillip.lord@newcastle.ac.uk>
Version: 0.1


## License

The contents of this file are subject to the GPL License, Version 3.0.

Copyright (C) 2014, Phillip Lord, Newcastle University

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.


## Commentary

Sometimes, it would be nice to edit a file in two ways at once. For
instance, you might have a source file in a computational language with
richly marked documentation. As Emacs is a modal editor, you can edit one
in a mode for the computational language or for the marked up
documentation.

One solution to this is to use one of the multiple-mode tools which are
available. The problem with this is that generally need some support from
the modes in question. In addition, they also require tools that work with
the mixed content; for example, Haskells literate mode.

Linked buffers provide an alternative solution. Two linked buffers, by
default, the two share identical content but are otherwise independent.
Therefore, you can have two buffers open, each showing the content in
different modes; to switch modes, you simply switch buffers. The content,
location of point, and view are shared.

However, linked-buffers also a bi-directional transformation between the
two. If this is done, then the two can have different but related text. It
is possible to configure the transformation for any two pairs of modes.

Main entry points are `linked-buffer-split-window-right` and
`linked-buffer-split-window-below` both of which create a linked buffer in
the new window. Programmatically `linked-buffer-create`, well, creates a
linked-buffer.

## Code

This is going to be the thing that we put in the file local var
although we need a default

    (defvar linked-buffer-init 'linked-buffer-default-init
      "Function that initializes a linked-buffer. This should set up
    `linked-buffer-config' appropriately and do")
    
In future versions, this may get turned into a list so that we can have
multiple linked buffers, but it is not clear how the user interface
functions such as `linked-buffer-swap-window` would work now.

    (defvar linked-buffer-config nil
      "Configuration for linked-buffer.

    This is a `linked-buffer-configuration' object, which defines the
    way in which the text in the different buffers is kept
    synchronized. This configuration is resiliant to changes of mode
    in the current buffer.")
    
    (make-variable-buffer-local 'linked-buffer-config)
    (put 'linked-buffer-config 'permanent-local t)

And here is some more documentation.

    (defun linked-buffer-config-name (buffer)
      (format "linked: %s" buffer))

