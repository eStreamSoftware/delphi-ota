unit OTA.IDE110;

interface

uses ToolsAPI, OTA.IDE;
type
  TNotifierOTA_ProjectManager_110 = class(TNotifierOTA)
  public
    procedure Setup; override;
    procedure BeforeDestruction; override;
  end;

implementation

procedure TNotifierOTA_ProjectManager_110.Setup;
begin
  FNotifier := FClass.Create;
  FNotifierIndex := (BorlandIDEServices as IOTAProjectManager).AddMenuCreatorNotifier(FNotifier as INTAProjectMenuCreatorNotifier);
end;

procedure TNotifierOTA_ProjectManager_110.BeforeDestruction;
begin
  inherited;
  if FNotifierIndex <> -1 then
    (BorlandIDEServices as IOTAProjectManager).RemoveMenuCreatorNotifier(FNotifierIndex);
end;

end.
