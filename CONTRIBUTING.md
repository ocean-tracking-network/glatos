# Contributing to the GLATOS R Package

You may have a feature or function that you'd like to contribute to the package. In order to get your feature into a form that the community can use, it will need to be integrated and tested before being accepted by our package maintainers. Here's a step-by-step guide to contributing the code for your feature directly via Git. This model of contributing to a Git project is rationalized [here](http://nvie.com/posts/a-successful-git-branching-model/). To get an idea of *why* we suggest doing things this way, that link will hopefully provide good insight. The two core principles that we'll borrow are that:

> We consider `origin/master` to be the main branch where the source code of HEAD always reflects a production-ready state. Stable releases will flow to here from `origin/dev`.

> We consider `origin/dev` to be the main branch where the source code of HEAD always reflects a state with the latest delivered development changes for the next release. Tested and approved features and functions will be integrated into this by the package maintainers.

## Method 1: The R Package Developer Method:
### Clone this repository and branch off of the current development branch of `glatos`

Clone the `glatos` repository onto your local machine. On the command line, you would CD to a working directory, then do:

```
git clone git@gitlab.oceantrack.org:GreatLakes/glatos.git
cd glatos
git fetch
git checkout -b dev origin/dev
```

This gets you the latest development branch, a version of the `glatos` codebase being prepared for release.

Next we build our new 'feature' branch, based on the `dev` branch, with:

```
git checkout -b myfeature
```

This creates a fresh branch for you to work in locally, based on the main `dev` branch for ease of re-integration with the glatos codebase. You should name it something sufficiently descriptive of what you're trying to add. Here in your *feature branch* you can add and rewrite anything that needs rewriting to support your feature's seamless inclusion in the package. If possible, your features should be self-contained. The less of glatos' existing codebase you change, the easier it'll be to merge back in and release to the group.

Once you've committed your `myfeature` branch and have it in a state you want to share, push it back up to the GitLab server with:

```
git push -u origin myfeature
```

You can push to your feature branch as many times as you like, this way you and another developer can collaborate on the feature, or just when you've reached a point that you feel you're ready to come back into the main package.

Finally, alert the package maintainers (@chrisholbrook , @trbinder , @thayden , @frank.smith , @jdpye) that your feature's ready for integration with `dev` by issuing a [Merge Request](https://gitlab.oceantrack.org/GreatLakes/glatos/merge_requests) for `myfeature` into `dev`. GitLab will run the reports on what you've changed and how it affects the current state of the `dev` branch, even if the `dev` branch has changed since you last checked!

 Responsibility for making sure your branch won't break everything now falls to the package maintainers. They'll merge in your feature if it's in good working order and it will be further tested and then eventually merged into the `master` branch, from which all the end-users of the `glatos` package run code.

 In the near future, testing will be an important part of contributing code to the package. Any tests that you can write to verify the integrity of your feature will be included as part of the build procedure for releasing official versions of `glatos`. Add them in a subfolder called `tests\myfeature` where myfeature is the name of your feature. For testing, the R package [testthat](https://cran.r-project.org/web/packages/testthat/testthat.pdf) is a good starting point.

 Documentation is done using the [roxygen](https://cran.r-project.org/web/packages/roxygen2/vignettes/rd.html) package and associated notation, so annotating your feature's functions in that way will be most helpful for integration purposes.

## Method 2: The 'Wishing Well' Method

If all that sounded quite complicated, and you'd like to see a feature added to the package, but can only describe it in plain English and not in a sweet, sweet .R file full of declarative statements and function calls, then create an [Issue](https://gitlab.oceantrack.org/GreatLakes/glatos/issues), describe what you'd like to see in the new feature, and assign it the Feature Request label. From there, any member of the community could pounce on your idea, assign it to themselves, write and commit it and claim ~~all~~ an appropriate amount of the credit. 


In either contribution paradigm, providing supporting literature for the analysis methods you propose should be prioritized. Everyone likes to see their name in a references list, and collaborative analysis packages can be a great way to get some sunshine on that analysis method you really like.


## Code Review and Style

The maintainers might point out a few changes that should be made to your feature branch before they can accept it in `dev`. Some of this stuff might be style, some might be substance. Make and push (or refute) the recommended changes and issue a new merge request. Polishing up features for the `dev` and `master` branches will be mostly left to the contributors/programmers. 

A line-length of 100 characters is going to be loosely enforced to make things optimally readable on older displays/terminals. Other style tips will hopefully find their way into a guide that we link from here.

## Bug Reports

Bug reports are hugely important! Before you start a new one, check/search the Issues page to make sure you're the first one to find the bug. Describe what you did, how you got to the buggy state, and provide everything you can to help a maintainer reproduce the issue. *Contributors:* If the bug is in a feature that you wrote, we might ping you for help sorting out the bug!
