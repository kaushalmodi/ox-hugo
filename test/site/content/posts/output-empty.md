+++
title = "Setting empty outputs is fine"
tags = ["outputs", "empty"]
draft = false
+++

If the `EXPORT_HUGO_OUTPUTS` property is left empty/unset, `ox-hugo`
will not set the `outputs` variable in the front-matter at all. So
only the HTML output will be created (default).
