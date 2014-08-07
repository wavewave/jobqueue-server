module HEP.Automation.JobQueue.Server.JobAssign where

import HEP.Automation.JobQueue.Config
import HEP.Automation.JobQueue.JobQueue

import Data.Maybe

checkJobCompatibility :: ClientConfiguration -> JobInfo -> Bool 
checkJobCompatibility (ClientConfiguration _ math _pbs montecarlo _dsdir) 
                      jobinfo =
  case jobinfo_detail jobinfo of 
    EventGen _ _ -> montecarlo
    MathAnal _ _ _ -> math  

findFirstJob :: ClientConfiguration -> ([JobInfo],[JobInfo]) -> Maybe JobInfo
findFirstJob cc (unassigned,finished) = do 
  let compatible = filter (checkJobCompatibility cc) unassigned
      finished_assoc = map (\x->(jobinfo_id x,x)) finished
      checkDependency jinfo = do 
        let deps = jobinfo_dependency jinfo
        mapM_ (\x->lookup x finished_assoc) deps
        return jinfo
      workable = mapMaybe checkDependency compatible
  if null workable
    then Nothing 
    else Just (head workable)
