+++
title = "Issue 470 – Fill region on inlined SVG files"
description = "Test to check inlining + fill region of SVG from Issue 470"
tags = ["svg"]
categories = ["issues"]
draft = false
+++

`ox-hugo` Issue #[470](https://github.com/kaushalmodi/ox-hugo/issues/470)

This test shows that it's OK to do fill-region on inlined SVG. In the
referenced issue, the issue author was inlining an SVG file containing
the `DOCTYPE` tag and that was causing the problem:

{{< figure src="https://user-images.githubusercontent.com/3578197/143625834-4548e998-0d45-4c9f-85bb-3d56456d3200.png" link="https://github.com/kaushalmodi/ox-hugo/issues/470#issuecomment-980385275" >}}

With the `DOCTYPE` tag removed, the browser doesn't throw any error
(below is the SVG from that issue.. seems like it's just blank. But
you will be able to see when you view this page's source).

<svg width="100%"
height="100%" viewBox="0 0 1485 440" version="1.1"
xmlns="http://www.w3.org/2000/svg"
xmlns:xlink="http://www.w3.org/1999/xlink" xml:space="preserve"
xmlns:serif="http://www.serif.com/"
style="fill-rule:evenodd;clip-rule:evenodd;stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:1.5;">
</svg>


<div class="figure-caption">

  <span class="figure-number">Figure 1: </span>SVG from Issue 470
</div>
