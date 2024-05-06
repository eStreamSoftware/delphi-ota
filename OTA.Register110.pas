unit OTA.Register110;

interface

implementation

uses OTA.IDE, OTA.IDE110,
     OTA.BuildAllFromHere;

initialization
  TOTAFactory
    .Register(TNotifierOTA_ProjectManager_110.Create(TBuildAllFromHere))
  ;
end.
