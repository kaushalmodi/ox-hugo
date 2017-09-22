+++
title = "Why ox-hugo?"
draft = false
[menu."early_questions"]
  weight = 3001
  identifier = "why-ox-hugo"
+++

Using Org just as a markup like Markdown is a miniscule part of its
complete feature-set. Org also allows stuff like:

-   Easy ordering/manipulation/commenting of subtrees
-   Creating tables (with even formulas like in Excel)
-   Directly including source code snippets from external files (instead
    of having to copy/paste them in)
-   Running code snippets within the Org file and embedding the results
    (Org Babel)
-   ..

Using Org for content writing allows using in-built Org features to
translate to Hugo front-matter:

-   Org uses an outline structure and can inherit meta data (tags and
    properties) from one subtree to children subtrees.
-   Using that feature, one can tag one tree as _emacs_, and everything
    under that tree (all posts under that) will get that tag
    automatically.
-   The same concept applies to inheriting any Org _property_ meta data
    like menu entry, category, section name, etc.
-   A subtree can be quickly marked to be in TODO state (default binding
    `C-c C-t`). A **TODO** post is marked as a _draft_ Hugo post.
-   The _menu-item weights_ and/or _post weights_ can be set to be
    auto-calculated so that the menu items or post order in the final
    HTML appear in the same order as the respective subtrees in Org.

    If the subtrees are re-ordered in Org, the weights get changed too.
-   One can have a subtree with section property set to "posts" and all
    post subtrees under that will go to that section. Similarly another
    parent subtree can have that property set to "articles", and so on.
-   Images can be displayed inline in the Org buffer.
-   After save hooks can be set up in Emacs so that each time I save the
    file, only the current subtree in Org gets exported to
    Markdown. With the Hugo server running with the new switch that auto
    changes the preview to the last changed post (`--navigateToChanged`
    introduced in Hugo 0.25), the flow is seamless -- Save the Org file
    and see the exact changed post in browser.
-   **All** posts can simply be subtrees in a single Org file. That way
    one can take advantage of Org subtree filtering and searching
    functions (`org-sparse-tree` bound to `C-c /` by default).
-   (and much more..)
