# additional examples brms 2

    Code
      print(output, width = Inf)
    Output
      # A tibble: 14 x 14
      # Model:    brmsfit
      # Settings: resolution 500 | 1000 posterior draws
         metric direction  group       estimate conf.low conf.high sig_val type    
         <chr>  <chr>      <fct>          <dbl>    <dbl>     <dbl>   <dbl> <chr>   
       1 nsec   decreasing ametryn       -1.90    -2.21     -1.69     0.01 absolute
       2 nsec   increasing ametryn       NA       NA        NA        0.01 absolute
       3 nsec   decreasing atrazine       0.274   -1.00      0.768    0.01 absolute
       4 nsec   increasing atrazine      NA       NA        NA        0.01 absolute
       5 nsec   decreasing diuron        -1.78    -2.15     -1.55     0.01 absolute
       6 nsec   increasing diuron        NA       NA        NA        0.01 absolute
       7 nsec   decreasing hexazinone    -0.984   -1.89     -0.616    0.01 absolute
       8 nsec   increasing hexazinone    NA       NA        NA        0.01 absolute
       9 nsec   decreasing irgarol       -2.07    -2.26     -1.93     0.01 absolute
      10 nsec   increasing irgarol       NA       NA        NA        0.01 absolute
      11 nsec   decreasing simazine       1.21    -0.111     1.77     0.01 absolute
      12 nsec   increasing simazine      NA       NA        NA        0.01 absolute
      13 nsec   decreasing tebuthiuron    1.22    -0.291     1.69     0.01 absolute
      14 nsec   increasing tebuthiuron   NA       NA        NA        0.01 absolute
         anchor control reference ecnsec ecnsec.low ecnsec.high
         <chr>    <dbl>     <dbl>  <dbl>      <dbl>       <dbl>
       1 model    0.685     0.665 0.0300    0.00502      0.0540
       2 model    0.685     0.705 0.0616    0.00693      0.110 
       3 model    0.623     0.604 0.0300    0.00427      0.0562
       4 model    0.623     0.642 0.0527    0.00716      0.0914
       5 model    0.698     0.676 0.0322    0.00665      0.0587
       6 model    0.698     0.722 0.0765    0.0121       0.129 
       7 model    0.678     0.661 0.0256    0.00379      0.0473
       8 model    0.678     0.696 0.0551    0.00740      0.0967
       9 model    0.670     0.647 0.0344    0.00530      0.0635
      10 model    0.670     0.697 0.0809    0.0192       0.132 
      11 model    0.683     0.662 0.0313    0.00429      0.0535
      12 model    0.683     0.701 0.0553    0.00483      0.107 
      13 model    0.708     0.696 0.0171    0.00223      0.0316
      14 model    0.708     0.720 0.0431    0.00694      0.0763

# nsec works for drc using continuous data

    Code
      print(output, width = Inf)
    Output
      # A tibble: 2 x 13
      # Model:    drc
      # Settings: resolution 500 | 1000 bootstrap realisations
        metric direction  estimate conf.low conf.high sig_val type     anchor control
        <chr>  <chr>         <dbl>    <dbl>     <dbl>   <dbl> <chr>    <chr>    <dbl>
      1 nsec   decreasing     1.18    0.994      1.28    0.01 absolute model    0.897
      2 nsec   increasing    NA      NA         NA       0.01 absolute model    0.897
        reference ecnsec ecnsec.low ecnsec.high
            <dbl>  <dbl>      <dbl>       <dbl>
      1     0.882 0.0169    0.00358      0.0304
      2     0.912 0.0165    0.00256      0.0303

# nsec for drc works for curveid and binomial data

    Code
      print(output, width = Inf)
    Output
      # A tibble: 4 x 14
      # Model:    drc
      # Settings: resolution 500 | 1000 bootstrap realisations
        metric direction  group estimate conf.low conf.high sig_val type     anchor
        <chr>  <chr>      <fct>    <dbl>    <dbl>     <dbl>   <dbl> <chr>    <chr> 
      1 nsec   decreasing 24h       517.     143.      946.    0.01 absolute model 
      2 nsec   increasing 24h        NA       NA        NA     0.01 absolute model 
      3 nsec   decreasing 48h       287.     130.      462.    0.01 absolute model 
      4 nsec   increasing 48h        NA       NA        NA     0.01 absolute model 
        control reference ecnsec ecnsec.low ecnsec.high
          <dbl>     <dbl>  <dbl>      <dbl>       <dbl>
      1   0.989     0.933 0.0564     0.0134      0.0644
      2   0.989     0.998 0.837      0.252       0.967 
      3   0.993     0.955 0.0379     0.0106      0.0436
      4   0.993     0.999 0.855      0.246       0.969 

# nsec for drc throws an error for an increasing function

    Code
      print(output, width = Inf)
    Output
      # A tibble: 4 x 14
      # Model:    drc
      # Settings: resolution 500 | 1000 bootstrap realisations
        metric direction  group estimate conf.low conf.high sig_val type     anchor
        <chr>  <chr>      <fct>    <dbl>    <dbl>     <dbl>   <dbl> <chr>    <chr> 
      1 nsec   decreasing 24h        NA       NA        NA     0.01 absolute model 
      2 nsec   increasing 24h       513.     147.      970.    0.01 absolute model 
      3 nsec   decreasing 48h        NA       NA        NA     0.01 absolute model 
      4 nsec   increasing 48h       275.     130.      447.    0.01 absolute model 
        control reference ecnsec ecnsec.low ecnsec.high
          <dbl>     <dbl>  <dbl>      <dbl>       <dbl>
      1 0.0106   0.00159  0.850      0.210       0.969 
      2 0.0106   0.0653   0.0554     0.0145      0.0634
      3 0.00735  0.000901 0.877      0.343       0.972 
      4 0.00735  0.0421   0.0350     0.0107      0.0408

# nsec drc additional example 1

    Code
      print(output, width = Inf)
    Output
      # A tibble: 2 x 13
      # Model:    drc
      # Settings: resolution 500 | 1000 bootstrap realisations
        metric direction  estimate conf.low conf.high sig_val type     anchor control
        <chr>  <chr>         <dbl>    <dbl>     <dbl>   <dbl> <chr>    <chr>    <dbl>
      1 nsec   decreasing    0.390    0.337     0.428    0.01 absolute model    0.679
      2 nsec   increasing   NA       NA        NA        0.01 absolute model    0.679
        reference ecnsec ecnsec.low ecnsec.high
            <dbl>  <dbl>      <dbl>       <dbl>
      1     0.664 0.0220    0.00487      0.0381
      2     0.693 0.0203    0.00340      0.0381

# nsec drc additional example 2

    Code
      print(output, width = Inf)
    Output
      # A tibble: 2 x 13
      # Model:    drc
      # Settings: resolution 500 | 1000 bootstrap realisations
        metric direction  estimate conf.low conf.high sig_val type     anchor control
        <chr>  <chr>         <dbl>    <dbl>     <dbl>   <dbl> <chr>    <chr>    <dbl>
      1 nsec   decreasing    0.402    0.338     0.441    0.01 absolute model    0.677
      2 nsec   increasing   NA       NA        NA        0.01 absolute model    0.677
        reference ecnsec ecnsec.low ecnsec.high
            <dbl>  <dbl>      <dbl>       <dbl>
      1     0.662 0.0215    0.00355      0.0380
      2     0.690 0.0190    0.00179      0.0377

# nsec accepts a toxval_pred

    Code
      print(nsec(output), width = Inf)
    Output
      # A tibble: 2 x 13
      # Model:    brmsfit
      # Settings: resolution 500 | 1000 posterior draws
        metric direction  estimate conf.low conf.high sig_val type     anchor control
        <chr>  <chr>         <dbl>    <dbl>     <dbl>   <dbl> <chr>    <chr>    <dbl>
      1 nsec   decreasing    0.258    0.116      1.28    0.01 absolute model    0.914
      2 nsec   increasing   NA       NA         NA       0.01 absolute model    0.914
        reference ecnsec ecnsec.low ecnsec.high
            <dbl>  <dbl>      <dbl>       <dbl>
      1     0.880 0.0369    0.00606      0.0617
      2     0.943 0.339     0.0790       0.503 

# brms grouped nsec with draws has one row per group and direction

    Code
      print(output, width = Inf)
    Output
      # A tibble: 4 x 15
      # Model:    brmsfit
      # Settings: resolution 500 | 1000 posterior draws
        metric direction  group estimate conf.low conf.high sig_val type     anchor
        <chr>  <chr>      <fct>    <dbl>    <dbl>     <dbl>   <dbl> <chr>    <chr> 
      1 nsec   decreasing 1        NA      NA         NA       0.01 absolute model 
      2 nsec   increasing 1         1.05    1.05       1.05    0.01 absolute model 
      3 nsec   decreasing 2         1.05    0.979      1.05    0.01 absolute model 
      4 nsec   increasing 2         1.05    0.921      1.05    0.01 absolute model 
        control reference ecnsec ecnsec.low ecnsec.high draws               
          <dbl>     <dbl>  <dbl>      <dbl>       <dbl> <list>              
      1   0.900     -5.12   6.17      2.45        31.2  <tibble [1,000 x 3]>
      2   0.900      4.33   3.37      0.225       24.5  <tibble [1,000 x 3]>
      3   0.921     -1.57   2.69      1.78         7.43 <tibble [1,000 x 3]>
      4   0.921      2.28   1.44      0.128        8.32 <tibble [1,000 x 3]>
    Code
      output$draws[[1]]
    Output
      # A tibble: 1,000 x 3
         .draw value.draw ecnsec.draw
         <int>      <dbl>       <dbl>
       1     1      Inf         12.8 
       2     2      Inf         14.6 
       3     3      Inf          3.20
       4     4      Inf         NA   
       5     5      Inf         NA   
       6     6      Inf          4.59
       7     7      Inf         NA   
       8     8        0.8       NA   
       9     9        0.8       NA   
      10    10        0.8       NA   
      # i 990 more rows

# nsec drc model has expected structure and shape

    Code
      print(output, width = Inf)
    Output
      # A tibble: 2 x 13
      # Model:    drc
      # Settings: resolution 600 | 1000 bootstrap realisations
        metric direction  estimate conf.low conf.high sig_val type     anchor control
        <chr>  <chr>         <dbl>    <dbl>     <dbl>   <dbl> <chr>    <chr>    <dbl>
      1 nsec   decreasing     1.12   0.0323      1.25    0.05 absolute model    0.897
      2 nsec   increasing    NA     NA          NA       0.05 absolute model    0.897
        reference ecnsec ecnsec.low ecnsec.high
            <dbl>  <dbl>      <dbl>       <dbl>
      1     0.887 0.0109   -0.00243      0.0245
      2     0.908 0.0115   -0.00241      0.0252

