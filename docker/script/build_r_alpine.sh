apk add --no-cache $R_SYS_DEPS \
    && apk add --no-cache --virtual _build_deps $R_SYS_BUILD_DEPS \
    && mkdir -p ~/.R \
    && echo "CFLAGS=-D__USE_MISC" > ~/.R/Makevars \
    && echo "\
    rep <- getOption('repos'); \
    rep['CRAN'] <- 'https://mran.microsoft.com/snapshot/"$R_PACKAGES_DATE"'; \
    options(repos = rep); \
    install.packages("$R_PACKAGES")" > install.R \
    && Rscript install.R \
    && apk del _build_deps



