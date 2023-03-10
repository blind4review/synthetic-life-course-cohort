list(
  truth=list(
    n=100000
  ),
  measurement=list(
    "measM"=list(
      measFn = "measurement1",
      options=list(
        cohorts=list(
          "younger"=list(
            obs=c("V","M"),
            n=2500
          ),
          "older"=list(
            obs=c("M","Y"),
            n=2500
          )
        )
      )
    )
  ),
  distances=list(
    "euclidian_spec"=list(
      distanceFn="euclidian",
      options=list(
        vars=c("M"),
        distanceMetric="unweighted"
      )
    ),
    "euclidian_weighted_spec"=list(
      distanceFn="euclidian",
      options=list(
        vars=c("M"),
        distanceMetric="rsq_weighted"
      )
    )
  ),
  combining=list(
    "detMatch"=list(
      combFn="deterministicMatch",
      options=list(
        nMatches=1,
        nBootstraps=1,
        distance="euclidian_spec"
      )
    ),
    "probMatch"=list(
      combFn="probMatch_manyYoungerPerOlder",
      options=list(
        nMatches=1,
        nBootstraps=1,
        distance="euclidian_spec"
      )
    ),
    "detMatch_10Matches"=list(
      combFn="deterministicMatch",
      options=list(
        nMatches=10,
        nBootstraps=1,
        distance="euclidian_spec"
      )
    ),
    "detMatch_10Matches_rsq"=list(
      combFn="deterministicMatch",
      options=list(
        nMatches=10,
        nBootstraps=1,
        distance="euclidian_weighted_spec"
      )
    ),
    "probMatch_10Matches"=list(
      combFn="probMatch_manyYoungerPerOlder",
      options=list(
        nMatches=10,
        nBootstraps=1,
        distance="euclidian_spec"
      )
    ),
    "detMatch_30Matches"=list(
      combFn="deterministicMatch",
      options=list(
        nMatches=30,
        nBootstraps=1,
        distance="euclidian_spec"
      )
    ),
    "probMatch_30Matches"=list(
      combFn="probMatch_manyYoungerPerOlder",
      options=list(
        nMatches=30,
        nBootstraps=1,
        distance="euclidian_spec"
      )
    )
  ),
  analysis=list(
    "lm_V"=list(
      analysisFn="lm",
      options=list(
        formula="Y~V"
      )
    ),
    "lm_V_weighted"=list(
      analysisFn="lm",
      options=list(
        trimWeights=TRUE,
        weighted=TRUE,
        formula="Y~V"
      )
    ),
    "lmcluster_V"=list(
      analysisFn="lmcluster",
      options=list(
        formula="Y~V",
        weighted=FALSE,
        clusterVar="older"
      )
    ),
    "lmcluster_V_weighted"=list(
      analysisFn="lmcluster",
      options=list(
        formula="Y~V",
        trimWeights=TRUE,
        weighted=TRUE,
        clusterVar="older"
      )
    )
  ),
  dags = list(
    "dag_earthworm"=list(
      measurement=list(
        "measM" = c(#"lm_V",
                    "lm_V_weighted",
                    #"lmcluster_V"#,
                    "lmcluster_V_weighted"
                    )
      ),
      combining = c(#"detMatch",
                     "detMatch_10Matches",
                     "detMatch_10Matches_rsq"#,
                    # "detMatch_30Matches",
                    #"probMatch",
                    # "probMatch_10Matches",
                    # "probMatch_30Matches"
                    )
    )
  ),
  effect = list(outcomes="Y",
                do="V")
)
