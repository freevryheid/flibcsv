# flibcsv
fortran bindings for the c library [libcsv](https://github.com/rgamble/libcsv)

## test
``` sh
fpm test --runner='valgrind --leak-check=full' -- csv/test_01.csv
```

## docs
see [flibcsv.f90](https://github.com/freevryheid/flibcsv/blob/main/src/flibcsv.f90) and [pdf](https://github.com/rgamble/libcsv/blob/master/csv.pdf)

## fpm.toml
``` sh
[dependencies]
flibcsv.git = "https://github.com/freevryheid/flibcsv"
```

