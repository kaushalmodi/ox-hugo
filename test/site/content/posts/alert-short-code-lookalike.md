+++
title = "Alert Shortcode Lookalike"
tags = ["shortcode", "alert", "special-block", "attr_html"]
draft = false
+++

`ox-hugo` Issue #[119](https://github.com/kaushalmodi/ox-hugo/issues/119)

Below doesn't export to an `alert` shortcode, but the exported
Markdown contains HTML that resembles the [shortcode code](https://github.com/gcushen/hugo-academic/blob/master/layouts/shortcodes/alert.html) (Courtesy:
[`hugo-academic` theme](https://github.com/gcushen/hugo-academic)):

```html
<div class="alert alert-{{ .Get 0 }}">
  {{ .Inner }}
</div>
```

There are few ways to mimic that.


## CSS {#css}

[Source for **alert** CSS](https://github.com/gcushen/hugo-academic/blob/66b71fa2f6a41f26d6c3b202fef212fab151112e/layouts/partials/css/academic.css#L1370-L1426)

Note 1
: The `ox-hugo` test site is not using **FontAwesome**, so
    using the unicode symbols 🛈 and ⚠ instead.

Note 2
: Also, due to the limitation that Markdown text cannot be
    simply wrapped in `div`, a hack is used, that requires
    using an empty `div` pair. So that requires overriding a
    bit of the default CSS from the theme.


### CSS Override for Academic theme {#css-override-for-academic-theme}

In summary, these overrides over the theme CSS would suffice:

```css
/* Because of the empty div hack, the first paragraph will be the
   second child in the div. So use "p:nth-child(2)" instead of the
   original "p:first-child". */
div.alert p:nth-child(2)::before {
    position: absolute;
    top: -0.5rem;
    left: -2rem;
    font-family: 'FontAwesome';
    font-size: 1.5rem;
    color: #fff;
    content: '\f05a';
    /* Use below if not using FontAwesome */
    /* content: '🛈'; */
    width: 1.5rem;
    text-align: center;
}

div.alert-warning p:nth-child(2):before {
    content: '\f071';
    /* Use below if not using FontAwesome */
    /* content: '⚠'; */
}
```

<style>
 .alert {
     padding: 15px;
     margin-bottom: 20px;
     border: 1px solid transparent;
     border-radius: 4px;
 }
 div.alert {
     border-radius: 10px;
     margin-bottom: 1rem;
 }

 div.alert p {
     position: relative;
     display: block;
     font-size: 1rem;
     margin-left: 2rem;
     margin-top: 0;
     margin-bottom: 0;
 }

 div.alert a {
     color: rgba(255,255,255,0.9);
     text-decoration: none;
     border-bottom: solid 1px #e4e4e4;
     transition: color 0.2s ease-in-out, border-color 0.2s ease-in-out;
 }

 div.alert a:hover {
     border-bottom-color: transparent;
     color: rgba(255,255,255,0.5) !important;
 }

 .alert-note {
     color: #fff;
     background-color: #03A9F4; /* Material LightBlue500 */
     border-color: #bce8f1;
 }

 .alert-warning {
     color: #fff;
     background-color: #f44336; /* Material Red500 */
     border-color: #ebccd1;
 }
</style>

<style>
 /* Because of the empty div hack, the first paragraph will be the
    second child in the div. So use "p:nth-child(2)" instead of the
    original "p:first-child". */
 div.alert p:nth-child(2)::before {
     position: absolute;
     top: -0.5rem;
     left: -2rem;
     font-family: 'FontAwesome';
     font-size: 1.5rem;
     color: #fff;
     /* content: '\f05a'; */
     content: '🛈';
     width: 1.5rem;
     text-align: center;
 }

 /* Because of the empty div hack, the first paragraph will be the
    second child in the div. So use "p:nth-child(2)" instead of the
    original "p:first-child". */
 div.alert-warning p:nth-child(2):before {
     /* content: '\f071'; */
     content: '⚠';
 }
</style>


## Alert using Special Block {#alert-using-special-block}

<div class="alert-note alert">

Here's a tip or note.

This can be multi-paragraph too.

</div>

<div class="alert-warning alert">

Here's a warning!

This can be multi-paragraph too.

</div>


## Alert using only `#+attr_html` {#alert-using-only-plus-attr-html}

This will work only if the message is a single paragraph.

<div class="alert alert-note">

Here's a tip or note.

</div>

<div class="alert alert-warning">

Here's a warning!
</div>
