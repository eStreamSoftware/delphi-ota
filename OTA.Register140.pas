unit OTA.Register140;

interface

implementation

uses
  OTA.FormatUses, OTA.IDE, OTA.IDE140;

initialization
  TOTAFactory
    .Register(TNotifierOTA_ProjectManager_140.Create(TProject_FormatUses))
    ;
end.
