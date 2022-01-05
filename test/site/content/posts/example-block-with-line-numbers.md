+++
title = "Example blocks with line number annotation"
tags = ["example"]
draft = false
+++

-   [Org reference](https://orgmode.org/manual/Literal-examples.html)


## Default new line number start {#default-new-line-number-start}

```text { linenos=table, linenostart=1 }
line 1
 line 2
```

An example without `-n`:

```text
foo
bar
```


## Specify new line number start {#specify-new-line-number-start}

```text { linenos=table, linenostart=20 }
line 20
line 21
```


## Default continued line numbers {#default-continued-line-numbers}

```text { linenos=table, linenostart=22 }
 line 22
line 23
```


## Specify continued line numbers jump {#specify-continued-line-numbers-jump}

```text { linenos=table, linenostart=33 }
line 33
line 34
```


## Specifying `linenos` parameter {#specifying-linenos-parameter}

In the below block, `:linenos false` switch was added to the example
block header.

```text { linenos=false }
This is line 1
This is line 2
This is line 3
```
