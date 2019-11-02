# Resubmission - 2019-11-01

#### Changes:

1. Removed commented example code lines in `queue.Rd`
2. Replaced `utils-filesystem.R` with my previous implementations and abandon 
the licensed functions from package `txtq`. These functions are non-critical 
and are just wrappers of the base functions for convenience.
3. Added `LICENSE` file.

**Original feedback from `Martina Schmirl`:**

```
Thanks,

found: Author: Eli Lilly and Company

Please always add all authors and copyright holders in the Authors@R
field with the appropriate roles.
 From CRAN policies you agreed to:
"The ownership of copyright and intellectual property rights of all
components of the package must be clear and unambiguous (including from
the authors specification in the DESCRIPTION file). Where code is copied
(or derived) from the work of others (including from R itself), care
must be taken that any copyright/license statements are preserved and
authorship is not misrepresented.
Preferably, an ‘Authors@R’ would be used with ‘ctb’ roles for the
authors of such code. Alternatively, the ‘Author’ field should list
these authors as contributors.
Where copyrights are held by an entity other than the package authors,
this should preferably be indicated via ‘cph’ roles in the ‘Authors@R’
field, or using a ‘Copyright’ field (if necessary referring to an
inst/COPYRIGHTS file)."
Please explain in the submission comments what you did about this issue.

Some code lines in examples are commented out.
Please never do that.
e.g. in queue.Rd

Please fix and resubmit.
```


# Previous Records:

## Last Submission - 2019-10-29

#### Changes:

1. Totally removed function `async_lapply`. I need to re-consider the implementation. Examples in `async_expr` are also removed because they must use at lease 2 cores.

2. `\value` is added to `%?<-%`, `make_forked_clusters`, `updateActionButtonStyled`,
`updateCompoundInput2`, `registerInputBinding`, `sync_shiny_inputs`, `cat2`.

3. `\value` is added in `progress2.Rd`, `queue.Rd`, two docs newly added since last submission

4. The newly added examples have the one `dontrun` case (in `queue.rd`). I expect that line to raise error as a destroyed queue is no longer valid


**Original feedback from `Martina Schmirl`:**

```
...
> Functions that have no
>    `\value` fields and reasons
> 1. `%?<-%`,
>    `make_forked_clusters`, `updateActionButtonStyled`,
>    `updateCompoundInput2`, `registerInputBinding`,
>    `sync_shiny_inputs`, `cat2`. These functions return
>    nothing

Still, it would be preferable to have that information in the \value field.

...

> **\donttest{}** cases:
> *
>    `async_expr` uses more than 1 cores
> * `async_lapply`
>    uses more than 1 cores

Ok thats no reason to put in donttest. As long no more than 2 cores are
used it's fine.
(And more than 2 cores are not allowed in examples any way)

```

## Last Submission - 2019-10-25

Changes:

### 1. Add the explanation of "dipping sauce" to DESCRIPTION file

Added *Just like dipping sauce adding flavors to potato chips or pita bread, 'dipsaus' for data analysis and visualizations addes handy functions and enhancements to popular packages. The goal is to provide simple solutions that are frequently asked for online, such as ...*

Thanks. I think adding explanation of the phrase "dipping sauce" really helps me defining the goal of this package.

### 2. Changed all directed quotation marks in the DESCRIPTION file to `'...'`

### 3. Added `\value` to functions

These functions have newly added `\value`:
* `eval_dirty`, `async_expr`, `async_lapply`

Functions that have no `\value` fields and reasons
1. `%?<-%`, `make_forked_clusters`, `updateActionButtonStyled`, `updateCompoundInput2`, `registerInputBinding`, `sync_shiny_inputs`, `cat2`. These functions return nothing
2. `async_flapply`, a wrapper of `future.apply::future_lapply`, added `seealso` field


### 4. In examples, there is no `\dontshow{}` cases, the rest are:

**Normal** cases: (shown to user, run tests)
* `parse_svec`, `deparse_svec`, `collapse` (were `\dontrun`)

**interactive()** cases:
* `compoundInput2` has shiny code parts (was `\dontrun`)
* `actionButtonStyled` requires launching shiny app (was `\dontrun` and `\donttest`)
* `sync_shiny_inputs` requires shiny app (was `\dontrun` and `\donttest`)

**\donttest{}** cases:
* `async_expr` uses more than 1 cores
* `async_lapply` uses more than 1 cores

**\dontrun{}** cases:
* `getInputBinding`, some examples are examples of bad usage
* `updateCompoundInput2`, this function must be called within shiny reactive context

### 5. Some commented code in examples
* `getInputBinding`, uncommented and used `dontrun` to wrap up bad usage which raises errors in some situations


Original feedback from `Martina Schmirl`:

```
Thanks,

Please add the explanation of "dipping sauce", as you wrote to Uwe, to
the description of the DESCRIPTION file.

Please do not use directed quotation marks in the DESCRIPTION file.
e.g. --> ... add-ons to packages ...
but use undirected quotation marks for package names:
e.g. --> 'rlang'

Please add \value to .Rd files regarding methods and explain the
functions results in the documentation.
e.g. eval_dirty.Rd, ...

Please ensure that you do not use more than 2 cores in your examples.

There is no need to wrap \dontrun in \donttest.
Also, \dontrun{} should be only used if the example really cannot be
executed (e.g. because of missing additional software, missing API keys,
...) by the user. That's why wrapping examples in \dontrun{} adds the
comment ("# Not run:") as a warning for the user.
Does not seem necessary.
Please replace \dontrun with \donttest.

Some code lines in examples are commented out.
Please never do that.
e.g. getInputBinding.Rd, ...

When creating the examples please keep in mind that the structure
would be desirable:
\examples{
    examples for users and checks:
    executable in < 5 sec
    \dontshow{
        examples for checks:
        executable in < 5 sec together with the examples above
        not shown to users
    }
    \donttest{
        further examples for users; not used for checks
        (e.g. data loading examples, or time consuming functions )
    }
    \dontrun{
        not used by checks, not used by example()
        adds the comment ("# Not run:") as a warning for the user.
        cannot run (e.g. because of missing additional software,
        missing API keys, ...)
    }
    if(interactive()){
        functions not intended for use in scripts,
        or are supposed to only run interactively
        (e.g. shiny)
    }
}

Please fix and resubmit, and document what was changed in the submission
comments.
```

