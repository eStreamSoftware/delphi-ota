unit OTA.Register140;

interface

implementation

uses OTA.IDE, OTA.IDE140, OTA.DataSnap.ProxyClient;

initialization
  TOTAFactory
    .Register(TNotifierOTA_ProjectManager_140.Create(TAddInterface_DataSnap_ProxyClient))
    ;
end.
