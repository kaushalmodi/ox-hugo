+++
title = "Contributing Guide"
draft = false
[menu.meta]
  weight = 3003
  identifier = "contributing-guide"
+++

NOTE TO FUTURE CONTRIBUTORS: I plan to merge this package into GNU
Elpa or Org source at some point.

So you will need to assign your copyright to FSF in order to get your
patches accepted.

-   [Why assign copyright to FSF?](https://www.gnu.org/licenses/why-assign.html)
-   [How to start this process](https://www.gnu.org/prep/maintain/html_node/Copyright-Papers.html#Copyright-Papers)

As a bonus, once you have assigned your copyright to FSF, doors open up
for your future contributions to Emacs too!


## Contribute to documentation {#contribute-to-documentation}

1.  Clone this repo.
2.  Add/edit documentation to `doc/ox-hugo-manual.org`.
3.  Run `make doc`.
4.  Review the changes in the generated .org and .md files.
5.  Commit, push branch and create PR.


## Contribute to code {#contribute-to-code}

1.  Clone this repo.
2.  Add/edit the .el files, **tests** in `test/site/content-org/`, and
    **documentation** to `doc/ox-hugo-manual.org`.
3.  Run `make md doc`.
4.  Review the changes in the generated .org and .md files.
5.  Commit (**don't push your branch yet!**).
6.  Run test: `make test` (you **need** to `git commit` i.e. do the above
    step before this step).
7.  Fix commit if test fails.. repeat till test passes.
8.  Push branch and create PR.
