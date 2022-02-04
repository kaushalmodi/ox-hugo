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
height="142px" preserveAspectRatio="none"
style="width:169px;height:142px;background:#FFFFFF;" version="1.1"
viewBox="0 0 169 142" width="169px" zoomAndPan="magnify"><defs><filter
height="300%" id="fuuvga0hq4qb7" width="300%" x="-1"
y="-1"><feGaussianBlur result="blurOut"
stdDeviation="2.0"/><feColorMatrix in="blurOut" result="blurOut2"
type="matrix" values="0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 .4
0"/><feOffset dx="4.0" dy="4.0" in="blurOut2"
result="blurOut3"/><feBlend in="SourceGraphic" in2="blurOut3"
mode="normal"/></filter></defs><g><ellipse cx="81.5" cy="20"
fill="#000000" filter="url(#fuuvga0hq4qb7)" rx="10" ry="10"
style="stroke:none;stroke-width:1.0;"/><rect fill="#FEFECE"
filter="url(#fuuvga0hq4qb7)" height="33.9688" rx="12.5" ry="12.5"
style="stroke:#A80036;stroke-width:1.5;" width="141" x="11" y="50"/><a
href="https://ox-hugo.scripter.co/" target="_parent"
title="https://ox-hugo.scripter.co/" xlink:actuate="onRequest"
xlink:href="https://ox-hugo.scripter.co/" xlink:show="new"
xlink:title="https://ox-hugo.scripter.co/" xlink:type="simple"><text
fill="#0000FF" font-family="sans-serif" font-size="12"
lengthAdjust="spacing" text-decoration="underline" textLength="121"
x="21" y="71.1387">ox-hugo homepage</text></a><ellipse cx="81.5"
cy="114.9688" fill="#FFFFFF" filter="url(#fuuvga0hq4qb7)" rx="11"
ry="11" style="stroke:#000000;stroke-width:1.0;"/><ellipse cx="81.5"
cy="114.9688" fill="#000000" rx="6" ry="6"
style="stroke:#7F7F7F;stroke-width:1.0;"/><line
style="stroke:#A80036;stroke-width:1.5;" x1="81.5" x2="81.5" y1="30"
y2="50"/><polygon fill="#A80036"
points="77.5,40,81.5,50,85.5,40,81.5,44"
style="stroke:#A80036;stroke-width:1.0;"/><line
style="stroke:#A80036;stroke-width:1.5;" x1="81.5" x2="81.5"
y1="83.9688" y2="103.9688"/><polygon fill="#A80036"
points="77.5,93.9688,81.5,103.9688,85.5,93.9688,81.5,97.9688"
style="stroke:#A80036;stroke-width:1.0;"/></g></svg>

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
height="69px" preserveAspectRatio="none"
style="width:61px;height:69px;background:#FFFFFF;" version="1.1"
viewBox="0 0 61 69" width="61px" zoomAndPan="magnify"><defs><filter
height="300%" id="f492as0tkgyr4" width="300%" x="-1"
y="-1"><feGaussianBlur result="blurOut"
stdDeviation="2.0"/><feColorMatrix in="blurOut" result="blurOut2"
type="matrix" values="0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 .4
0"/><feOffset dx="4.0" dy="4.0" in="blurOut2"
result="blurOut3"/><feBlend in="SourceGraphic" in2="blurOut3"
mode="normal"/></filter></defs><g><a href="https://example.org/" target="_top"
title="https://example.org/" xlink:actuate="onRequest"
xlink:href="https://example.org/" xlink:show="new"
xlink:title="https://example.org/" xlink:type="simple"><rect
codeLine="1" fill="#FEFECE" filter="url(#f492as0tkgyr4)" height="48"
id="A" style="stroke:#A80036;stroke-width:1.5;" width="40" x="7"
y="7"/><ellipse cx="22" cy="23" fill="#ADD1B2" rx="11" ry="11"
style="stroke:#A80036;stroke-width:1.0;"/><path d="M24.9688,28.6406
Q24.3906,28.9375 23.75,29.0781 Q23.1094,29.2344 22.4063,29.2344
Q19.9063,29.2344 18.5781,27.5938 Q17.2656,25.9375 17.2656,22.8125
Q17.2656,19.6875 18.5781,18.0313 Q19.9063,16.375 22.4063,16.375
Q23.1094,16.375 23.75,16.5313 Q24.4063,16.6875 24.9688,16.9844
L24.9688,19.7031 Q24.3438,19.125 23.75,18.8594 Q23.1563,18.5781
22.5313,18.5781 Q21.1875,18.5781 20.5,19.6563 Q19.8125,20.7188
19.8125,22.8125 Q19.8125,24.9063 20.5,25.9844 Q21.1875,27.0469
22.5313,27.0469 Q23.1563,27.0469 23.75,26.7813 Q24.3438,26.5
24.9688,25.9219 L24.9688,28.6406 Z " fill="#000000"/><text
fill="#000000" font-family="sans-serif" font-size="12"
lengthAdjust="spacing" textLength="8" x="36" y="27.1543">A</text><line
style="stroke:#A80036;stroke-width:1.5;" x1="8" x2="46" y1="39"
y2="39"/><line style="stroke:#A80036;stroke-width:1.5;" x1="8" x2="46"
y1="47" y2="47"/></a></g></svg>
