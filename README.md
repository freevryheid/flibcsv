# flibcsv
fortran bindings for the c library [libcsv](https://github.com/rgamble/libcsv)

## test
``` sh
fpm test --runner='valgrind --leak-check=full' -- csv/test_01.csv
```

