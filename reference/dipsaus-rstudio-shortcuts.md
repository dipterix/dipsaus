# Register customized R code to 'RStudio' shortcuts

'RStudio' keyboard shortcuts is handy, however, it is non-trivial to set
shortcuts that run customized code. The proposing functions allow 10
customized R expressions to be registered. The first five (1 to 5) are
interactive shortcuts, the rest five (6 to 10) are non-interactive.

## Usage

``` r
rs_add_insertion_shortcut(which, txt, force = FALSE)

rs_add_shortcut(which, expr, force = FALSE, quoted = FALSE)

rs_remove_shortcut(which)

rs_show_shortcut(which)

rs_quick_debug(env = globalenv())
```

## Arguments

- which:

  integer from 1 to 10, which keyboard shortcut to edit

- txt:

  an insertion/replacement shortcut to add

- force:

  whether to remove existing shortcut if the hot-key has been registered

- expr:

  expression to run if shortcut is pressed

- quoted:

  whether `expr` is quoted, default is false

- env:

  environment to debug code; default is global environment

## Details

There are two steps to register an 'RStudio' keyboard shortcut.

1\. Please enable the shortcuts by opening
`'Tools' > 'Modify Keyboard Shortcuts'` in 'RStudio' menu bar; search
and locate add-in items starting with 'Dipsaus'; register hot-keys of
your choices, and then save. It is recommended that these keys are
`'Alt' + 1` to `'Alt' + 0`. On Apple, 'Alt' is equivalent to 'option'
key.

2\. run `rs_add_insertion_shortcut` or `rs_add_shortcut` to customize
the behaviors of each shortcuts; see Examples.

Function `rs_quick_debug` provides quick way to debug a script or
function without messing up the code. The script only works in
'RStudio'. When executing the quick-debug function, the cursor context
will be automatically resolved and nearest debugging code blocks will be
searched and executed. To enable this feature, add a line with
`"# DIPSAUS: DEBUG START"` in your code, followed by debugging code
blocks in comments. The script will figure it out. Since the 'RStudio'
context will be obtained when executing the function, it is recommended
to add this function to your shortcuts. By default, if the shortcut-1 is
unset, this function will be executed.

## Examples

``` r
if (FALSE) { # \dontrun{

# Need to run in RStudio
# Please read the Section 'Details' carefully

# --------------------------------------------

# I assume the shortcuts are Alt+1,2,...,9,0,
# corresponding to shortcuts 1 - 10

# Adds an insertion to Alt+9
rs_add_insertion_shortcut(9, " %?<-% ", force = TRUE)
# restart RStudio and try `Alt+9`


# Adds an expression to Alt+2
rs_add_shortcut(2, {
  expr <- sprintf("system.time({\n%s\n})\n",
                  rstudioapi::selectionGet()$value)
  cat(expr)
  eval(parse(text = expr))
}, force = TRUE)

# Select any valid R code and press Alt+1

# --------------------------------------------

# run this to set your shortcut (one-time setup)
rs_add_shortcut(1, { dipsaus::rs_quick_debug() })

# Add debug feature: insert the following comment anywhere in your code
# You may open a new script in the RStudio

# DIPSAUS: DEBUG START
# message("Debugging...")
# a <- 1
# print(a)
# message("Finished")


# Place your cursor here, press the shortcut key

} # }
```
