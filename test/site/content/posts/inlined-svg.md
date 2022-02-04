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

  <span class="figure-number">Figure 1: </span>An SVG with **hyperlinks** -- generated using PlantUML
</div>


## Captioned source block followed by an inlined SVG {#captioned-source-block-followed-by-an-inlined-svg}

`ox-hugo` Issue #[552](https://github.com/kaushalmodi/ox-hugo/issues/552)

```plantuml
class A
url of A is [[https://example.org/]]
```

<div class="src-block-caption">
  <span class="src-block-number">Code Snippet 1</span>:
  My class diagram
</div>

<svg
xmlns="http://www.w3.org/2000/svg"
xmlns:xlink="http://www.w3.org/1999/xlink"
contentScriptType="application/ecmascript" contentStyleType="text/css"
height="67px" preserveAspectRatio="none"
style="width:57px;height:67px;" version="1.1" viewBox="0 0 57 67"
width="57px" zoomAndPan="magnify"><defs><filter height="300%"
id="f492as0tkgyr4" width="300%" x="-1" y="-1"><feGaussianBlur
result="blurOut" stdDeviation="2.0"/><feColorMatrix in="blurOut"
result="blurOut2" type="matrix" values="0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 .4 0"/><feOffset dx="4.0" dy="4.0" in="blurOut2"
result="blurOut3"/><feBlend in="SourceGraphic" in2="blurOut3"
mode="normal"/></filter></defs><g><a target="_top"
xlink:actuate="onRequest" xlink:href="https://example.org/"
xlink:show="new" xlink:title="https://example.org/"
xlink:type="simple"><rect fill="#FEFECE" filter="url(#f492as0tkgyr4)"
height="48" id="A" style="stroke: #A80036; stroke-width: 1.5;"
width="40" x="6" y="8"/><ellipse cx="21" cy="24" fill="#ADD1B2"
rx="11" ry="11" style="stroke: #A80036; stroke-width: 1.0;"/><path
d="M23.9688,29.6406 Q23.3906,29.9375 22.75,30.0781 Q22.1094,30.2344
21.4063,30.2344 Q18.9063,30.2344 17.5781,28.5938 Q16.2656,26.9375
16.2656,23.8125 Q16.2656,20.6875 17.5781,19.0313 Q18.9063,17.375
21.4063,17.375 Q22.1094,17.375 22.75,17.5313 Q23.4063,17.6875
23.9688,17.9844 L23.9688,20.7031 Q23.3438,20.125 22.75,19.8594
Q22.1563,19.5781 21.5313,19.5781 Q20.1875,19.5781 19.5,20.6563
Q18.8125,21.7188 18.8125,23.8125 Q18.8125,25.9063 19.5,26.9844
Q20.1875,28.0469 21.5313,28.0469 Q22.1563,28.0469 22.75,27.7813
Q23.3438,27.5 23.9688,26.9219 L23.9688,29.6406 Z "/><text
fill="#000000" font-family="sans-serif" font-size="12"
lengthAdjust="spacingAndGlyphs" textLength="8" x="35"
y="28.1543">A</text><line style="stroke: #A80036; stroke-width: 1.5;"
x1="7" x2="45" y1="40" y2="40"/><line style="stroke: #A80036;
stroke-width: 1.5;" x1="7" x2="45" y1="48" y2="48"/></a></g></svg>
