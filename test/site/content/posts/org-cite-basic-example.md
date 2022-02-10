+++
title = "Org Cite Basic Example"
description = "Basic example of using [cite:@xyz] citation."
tags = ["citations", "org-cite"]
draft = false
+++

**Org 9.5** is needed for this as that version introduced
<mark>the new **oc.el** Org Cite library</mark> .


## Specify Bibligraphy files {#specify-bibligraphy-files}

Org Cite (`oc.el`) requires the bibliography files to be specified
using one or more `#+bibliography: <file.bib>` keywords or using the
`org-cite-global-bibliography` variable.

Example: `#+bibliography: orgcite.bib`

<div class="note">

`:EXPORT_BIBLIOGRAPHY:` in subtree property drawers will **not** work
with Org Cite!

</div>


## Citation Syntax {#citation-syntax}

Below Org snippet:

```org
[cite:@OrgCitations]
```

exports to:

(org, mode and Syntax, Citation and List, Mailing and Effort, Time, 2021)

See [the _TMIO_ July 2021 blog post](https://blog.tecosaur.com/tmio/2021-07-31-citations.html) for more information on the `cite:`
syntax.


## Printing Bibliography {#printing-bibliography}

Below Org snippet:

```org
#+print_bibliography:
```

exports to:

org, mode and Syntax, Citation and List, Mailing and Effort, Time (2021). _Elegant Citations with Org-Mode_, Journal of Plain Text Formats.
