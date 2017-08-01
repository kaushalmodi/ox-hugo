+++
title = "Don't Use Org Code markup for kbd tag"
date = 2017-07-31
tags = ["formatting"]
draft = false
+++

Note that to disable the "use code for kbd" option, the value portion
of the property needs to be left **empty** instead of setting to `nil`!

```text
:PROPERTIES:
:EXPORT_HUGO_USE_CODE_FOR_KBD:
:END:
```

Here `~C-h f~` will show up as `` `C-h f` `` in Markdown and then
`<code>C-h f</code>` in the final Hugo generated HTML.

Example:

-   Few of Emacs help keybindings: `C-h f`, `C-h v`
