# webdriver-wrapper

This package was created so end-users don't need to manually manage Selenium or their webdriver when dealing with the [webdriver](https://hackage.haskell.org/package/webdriver) package. For now, only `geckodriver` (aka Firefox) is supported - but `chromedriver` should be easy to implement. If you can't use Firefox for whichever reason, let me know and I'll look into implementing `chromedriver` support. 

This package is strongly inspired by [sandwich-webdriver](https://hackage.haskell.org/package/sandwich-webdriver). My original use-case didn't require the sandwich test suite, though, so I made this package which intends on being simpler and more minimalistic. 

## Extra dependencies
You must have Java installed, since Selenium is distributed as a `.jar` file. I'm running OpenJDK 17 on my machine, and it works perfectly with the selected Selenium version. 

## How to use
There are two functions which are "batteries-included": `wrappedRunSession` and `wrappedRunWD`. Just use them as you would `runSession` and `runWD`. The wrapped functions will handle downloading Selenium and geckodriver (if they were not already downloaded), plus starting and stopping Selenium. If you already have some code with `runSession` or `runWD`, their wrapped counterparts are drop-in replacements. 