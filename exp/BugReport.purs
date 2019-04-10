module BugReport where


import Preface



-- Data ------------------------------------------------------------------------


type Bug =
  { application :: String
  , version :: Int
  , description :: String
  , severity :: Severity
  }


data Severity
  = Critical
  | Normal



-- Tasks -----------------------------------------------------------------------


report_bug :: Task {}
report_bug = do
  { bug } <- enter "Bug information"
  case bug.severity of
    Critical -> do
      { assessor } <- select_assessor { application: bug.application, version: bug.version }
      { confirmed } <- assessor -@- confirm_critical_bug { bug }
      { developer } <- select_developer { application: bug.application, version: bug.version }
      if confirmed
        then developer -@- resolve_critical_bug { bug }
        else developer -@- resolve_normal_bug { bug }
    _ -> do
      { developer } <- select_developer { application: bug.application, version: bug.version }
      developer -@- resolve_normal_bug { bug }


select_assessor :: { application :: String, version :: Int } -> Task { assessor :: User }
select_assessor = undefined


select_developer :: { application :: String, version :: Int } -> Task { developer :: User }
select_developer = undefined


confirm_critical_bug :: { bug :: Bug } -> Task { confirmed :: Boolean }
confirm_critical_bug = undefined


resolve_critical_bug :: { bug :: Bug } -> Task {}
resolve_critical_bug = undefined


resolve_normal_bug :: { bug :: Bug } -> Task {}
resolve_normal_bug = undefined
