list(
  V = list(
    parents=c(),
    simFn="simLinear",
    simOptions = list(
      coefs=list(
        U=0.1
      )
    )
  ),
  M = list(
    parents=c("V"),
    simFn="simLinear",
    simOptions = list(
      coefs=list(
        U=0.4,
        V=0.5
      )
    )
  ),
  Y = list(
    parents=c("M"),
    simFn="simLinear",
    simOptions = list(
      coefs=list(
        U=0.1,
        M=0.6
      )
    )
  )
)