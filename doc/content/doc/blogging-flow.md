+++
title = "Blogging Flow"
draft = false
[menu."getting_started"]
  weight = 3004
  identifier = "blogging-flow"
+++

There are 2 major blogging flows that can be used with this package:

1.  One post per Org subtree (preferred)
    -   Export only the **current** post Org subtree, or
    -   Export all valid Hugo post subtrees in a loop.
2.  One post per Org file
    -   This works but you won't be able to leverage Org-specific
        benefits like tag and property inheritance, use of TODO states to
        translate to post `draft` state, auto weight calculation for
        posts and menu items, etc.

See the [Org Capture Setup](/doc/org-capture-setup) page to see how to quickly create new posts.

See the [Auto-export on Saving](/doc/auto-export-on-saving) page to learn how to setup up seeing
live-preview of the Hugo-rendered HTML each time you do `C-x C-s` in
the Org file. That section explains how to set that up for either of
the above two blogging flows.
