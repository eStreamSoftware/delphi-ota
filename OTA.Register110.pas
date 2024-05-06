unit OTA.Register110;

interface

implementation

uses OTA.IDE, OTA.IDE110,
     OTA.BuildAllFromHere, OTA.SearchProject;

initialization
  TOTAFactory
    .Register(TNotifierOTA_ProjectManager_110.Create(TBuildAllFromHere))
    .Register(TNotifierOTA_ProjectManager_110.Create(TSearchProject))
  ;
end.
