+++
title = "Debug"
draft = false
[menu.meta]
  weight = 3004
  identifier = "debug"
+++

If the `ox-hugo` exports do not work as expected, or if you get an
error backtrace,

1.  Open an [Issue](https://github.com/kaushalmodi/ox-hugo/issues).
2.  Describe the problem you are seeing.
3.  Provide the debug info:
    -   Do `M-x org-hugo-debug-info`, **That will copy the debug info in
        Markdown format to the kill ring.** So do **not** manually do any
        other kill (copy/cut) operation after that!
    -   Paste the Markdown contents in the GitHub issue.
        -   You can still hit the _Preview_ tab of the Issue before
            submitting it.
