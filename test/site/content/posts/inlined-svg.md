+++
title = "Inlined SVG"
description = "Test inlined SVGs with hyperlinks."
tags = ["image", "svg", "inline"]
draft = false
+++

`ox-hugo` Issue #[253](https://github.com/kaushalmodi/ox-hugo/issues/253)

-   [Example of generating SVG with hyperlinks using PlantUML](http://plantuml.com/svg)
-   [SVGs with hyperlinks need to be inlined](https://alligator.io/svg/hyperlinks-svg/)

Introduce a new `:inlined t` HTML attribute.

<svg
xmlns="http://www.w3.org/2000/svg"
xmlns:xlink="http://www.w3.org/1999/xlink"
contentScriptType="application/ecmascript" contentStyleType="text/css"
height="134px" preserveAspectRatio="none"
style="width:162px;height:134px;" version="1.1" viewBox="0 0 162 134"
width="162px" zoomAndPan="magnify"><defs><filter height="300%"
id="fuuvga0hq4qb7" width="300%" x="-1" y="-1"><feGaussianBlur
result="blurOut" stdDeviation="2.0"/><feColorMatrix in="blurOut"
result="blurOut2" type="matrix" values="0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 .4 0"/><feOffset dx="4.0" dy="4.0" in="blurOut2"
result="blurOut3"/><feBlend in="SourceGraphic" in2="blurOut3"
mode="normal"/></filter></defs><g><ellipse cx="80.5" cy="20"
fill="#000000" filter="url(#fuuvga0hq4qb7)" rx="10" ry="10"
style="stroke: none; stroke-width: 1.0;"/><rect fill="#FEFECE"
filter="url(#fuuvga0hq4qb7)" height="33.9688" rx="12.5" ry="12.5"
style="stroke: #A80036; stroke-width: 1.5;" width="141" x="10"
y="50"/><a target="_parent" xlink:actuate="onRequest"
xlink:href="https://ox-hugo.scripter.co/" xlink:show="new"
xlink:title="https://ox-hugo.scripter.co/" xlink:type="simple"><text
fill="#0000FF" font-family="sans-serif" font-size="12"
lengthAdjust="spacingAndGlyphs" text-decoration="underline"
textLength="121" x="20" y="71.1387">ox-hugo homepage</text><line
style="stroke: #0000FF; stroke-width: 1.0;" x1="20" x2="141"
y1="73.1387" y2="73.1387"/></a><ellipse cx="80.5" cy="113.9688"
fill="none" filter="url(#fuuvga0hq4qb7)" rx="10" ry="10"
style="stroke: #000000; stroke-width: 1.0;"/><ellipse cx="81"
cy="114.4688" fill="#000000" filter="url(#fuuvga0hq4qb7)" rx="6"
ry="6" style="stroke: none; stroke-width: 1.0;"/><line style="stroke:
#A80036; stroke-width: 1.5;" x1="80.5" x2="80.5" y1="30"
y2="50"/><polygon fill="#A80036"
points="76.5,40,80.5,50,84.5,40,80.5,44" style="stroke: #A80036;
stroke-width: 1.0;"/><line style="stroke: #A80036; stroke-width: 1.5;"
x1="80.5" x2="80.5" y1="83.9688" y2="103.9688"/><polygon
fill="#A80036"
points="76.5,93.9688,80.5,103.9688,84.5,93.9688,80.5,97.9688"
style="stroke: #A80036; stroke-width: 1.0;"/></g></svg>

<div class="figure-caption">
  Figure 1: An SVG with **hyperlinks** &#x2013; generated using PlantUML
</div>
