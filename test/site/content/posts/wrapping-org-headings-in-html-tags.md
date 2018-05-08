+++
title = "Wrapping Org headings in HTML tags"
description = """
  Test the workaround for wrapping Org headings in HTML tags like
  `section` and `div`.
  """
tags = ["body", "wrapping", "headings", "section", "tags", "HTML"]
draft = false
+++

`ox-hugo` Issue #[160](https://github.com/kaushalmodi/ox-hugo/issues/160)


## Org Source {#org-source}

```org
#+html: <style>.left { float: left; width: 45%;} .right { float: right; width: 45%;}</style>

#+html: <section class="left">
# Hack to get around Blackfriday limitation: https://github.com/kaushalmodi/ox-hugo/issues/93
#+html: <section></section>
**** This section will show up on the left side
Content in the left side section
#+html: </section>

#+html: <section class="right">
#+html: <section></section>
**** This section will show up on the right side
Content in the right side section
#+html: </section>

# Clear the floats
#+html: <div style="clear: both;"></div>
```


## Output {#output}

<style>.left { float: left; width: 45%;} .right { float: right; width: 45%;}</style>

<section class="left">

<section></section>


### This section will show up on the left side {#this-section-will-show-up-on-the-left-side}

Content in the left side section

</section>

<section class="right">

<section></section>


### This section will show up on the right side {#this-section-will-show-up-on-the-right-side}

Content in the right side section

</section>

<div style="clear: both;"></div>
