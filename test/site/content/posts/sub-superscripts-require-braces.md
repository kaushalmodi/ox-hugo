+++
title = "Sub/superscripts require braces"
tags = ["export-option", "superscripts", "subscripts"]
draft = false
+++

By default, `ox-hugo` implements the `^:{}` export option. See `C-h v
org-export-with-sub-superscripts` for details. With this option, the
text that needs to be subscripted or superscripted has to be
surrounded by braces `{..}` following the `_` or `^`.


## Following text will export `_` and `^` verbatim {#following-text-will-export-and-verbatim}

a_b a_bc a^b a^bc


## Following text will export `_{..}` as subscript and `^{..}` as superscript {#following-text-will-export-dot-dot-as-subscript-and-dot-dot-as-superscript}

a<sub>b</sub> a<sub>bc</sub> a<sup>b</sup> a<sup>bc</sup>
