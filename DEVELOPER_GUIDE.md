## Building

Clone the repository (recursively to include submodules) and then build using
`sbt`. Use the `develop` branch if you intend to create pull requests: 

```
git clone --recurse-submodules https://github.com/kframework/kale --branch develop
sbt compile
```

These git configuration settings smoothen out working with submodules:

```
git config --global push.recurseSubmodules check
git config --global fetch.recurseSubmodules true
```

If you intend to push to the `kore` repository too, using you SSH adding:

```
[url "git@github.com.:"]
    pushInsteadOf = https://github.com/
```

to your `~/.gitconfig` will let you do that.


## Guide on adding a new feature to terms/labels

In an `if-then-elseif-...` style:

#### Is it very specific to your use/project?

Leave it in your project / out of the Scala backend.

#### It it only invoked occasionally, performance is not critical?

Put it in Rich* (`org.kframework.kale` package object)

#### Performance is important?

Create an `Environment` `Mixin` and use `Unary` or `Binary`.  

#### Do you think the feature is extremely important and should be in the main trait hierarchy, not a mixin?

**Talk with a few other developers before putting it there!!!**

#### Do you an idea of how we could move functionality out of the main trait hierarchy into a Mixin?

**Let's talk about it!**
