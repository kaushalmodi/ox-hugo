+++
title = "Use Org Code markup for kbd tag"
date = 2017-07-31
tags = ["formatting"]
draft = false
+++

Here the Org code markup is explicitly specified to be used for
`<kbd>` tag generation by setting `EXPORT_HUGO_USE_CODE_FOR_KBD`
property to `t`. So `~C-h f~` will show up as `<kbd>C-h f</kbd>`.

Example:

-   Few of Emacs help keybindings: <kbd>C-h f</kbd>, <kbd>C-h v</kbd>
