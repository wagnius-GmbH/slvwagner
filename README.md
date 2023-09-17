# slvwagner
My R library, including all functions I use very often.

Some function need yacas a symbolic solver: \
http://www.yacas.org/

## Usage
If you just want to use the package you can download the latest release and install a binary or a source package. 
 <br>
If you use the source package (.tar.gz) you also need to install rtools to compile.

Easy way to install: <br>
```
devtools::install_git("https://github.com/wagnius-GmbH/slvwagner/",   build_manual = TRUE,
                      build_vignettes = TRUE)
```

<br>
Create binary package including vignettes:

``` 
devtools::build(devtools::build(), binary = TRUE)

```
