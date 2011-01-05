unit OTA.Register;

interface

implementation

uses OTA.IDE, OTA.BuildAllFromHere, OTA.SearchMissingFile, OTA.SearchProject,
     OTA.SetActiveProjectModule, OTA.SetOEMDir
     {$if CompilerVersion = 18.5}, OTA.CGRC {$ifend}
     ;

initialization
  TOTAFactory
    .Register(TNotifierOTA_ProjectManager.Create(TBuildAllFromHere))
    .Register(TNotifierOTA_ProjectManager.Create(TSearchMissingFile))
    .Register(TNotifierOTA_ProjectManager.Create(TSearchProject))
    .Register(TNotifierOTA_Services.Create(TSetActiveProjectModule))
    .Register(TNotifierOTA_Services.Create(TSetOEMDir))
    {$if CompilerVersion = 18.5}.Register(TNotifierOTA_Services.Create(TResourceCompiler_CGRC)){$ifend}
    ;
end.
