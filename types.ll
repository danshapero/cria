
%value_t = type {
     i32,                                ; type of data
     i64,                                ; integer
     i1,                                 ; bool
     i8*,                                ; string
     %value_t**,                         ; array/fenv
     i64,                                ; array/string length
     double,                             ; double
     %value_t* (i32, %value_t**, ...)*,  ; function
     i8                                  ; char
}