unit OTA.Register110;

interface

implementation

uses OTA.IDE, OTA.IDE110,
     OTA.BuildAllFromHere, OTA.SearchMissingFile, OTA.SearchProject,
     OTA.RemoveDefaultResource
     {$if CompilerVersion = 18.5}, OTA.SetOEMDir {$ifend}
     ;

initialization
  TOTAFactory
    .Register(TNotifierOTA_ProjectManager_110.Create(TBuildAllFromHere))
    .Register(TNotifierOTA_ProjectManager_110.Create(TSearchMissingFile))
    .Register(TNotifierOTA_ProjectManager_110.Create(TSearchProject))
    {$ifdef DEBUG}.Register(TNotifierOTA_ProjectManager.Create(TRemoveDefaultResource)){$endif}
    {$if CompilerVersion = 18.5}
      .Register(TNotifierOTA_Services.Create(TSetOEMDir))
    {$ifend}
    ;
end.
