+++
title = "Special block description overrides property-set description"
description = "Description set in special block"
tags = ["front-matter", "description", "special-block", "toml"]
draft = false
+++

If the description is set via the subtree property
`:EXPORT_DESCRIPTION` (or the `#+description` keyword), and via the
"description" special block too, the latter will take precedence.
