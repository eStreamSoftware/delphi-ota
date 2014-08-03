unit OTA.Register140;

interface

implementation

uses OTA.IDE, OTA.IDE140, OTA.QC92507, OTA.DataSnap.ProxyClient;

initialization
  TOTAFactory
    .Register(TNotifierOTA_ProjectManager_140.Create(T_QC92507_BaseConfigurationContainer))
    .Register(TNotifierOTA_ProjectManager_140.Create(T_QC92507_BuildConfigContainer))
    .Register(TNotifierOTA_ProjectManager_140.Create(TAddInterface_DataSnap_ProxyClient))
    ;
end.
