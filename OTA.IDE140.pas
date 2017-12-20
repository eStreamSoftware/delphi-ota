unit OTA.IDE140;

interface

uses
  System.Classes, System.SysUtils, ToolsAPI,
  OTA.IDE;

type
  TProjectMenuContextExecute = TProc<IOTAProjectMenuContext>;

  IOTAProjectManagerMenuExecute = interface
  ['{C5980256-159A-4B94-8FEC-85E2FEEE0131}']
    procedure SetExecute(aExec: TProjectMenuContextExecute);
    property Execute: TProjectMenuContextExecute write SetExecute;
  end;

  TNotifierOTA_ProjectManager_140 = class(TNotifierOTA)
  public
    procedure Setup; override;
    procedure BeforeDestruction; override;
  end;

  TNotifierOTA_ProjectManagerMenu = class(TNotifierObject, IOTALocalMenu,
      IOTAProjectManagerMenu, IOTAProjectManagerMenuExecute)
  private
    FCaption: string;
    FChecked: boolean;
    FEnabled: boolean;
    FExecute: TProjectMenuContextExecute;
    FHelpContext: integer;
    FIsMultiSelectable: boolean;
    FName: string;
    FParent: string;
    FPosition: integer;
    FVerb: string;
  protected  // IOTALocalMenu
    function GetCaption: string;
    function GetChecked: Boolean;
    function GetEnabled: Boolean;
    function GetHelpContext: Integer;
    function GetName: string;
    function GetParent: string;
    function GetPosition: Integer;
    function GetVerb: string;
    procedure SetCaption(const Value: string);
    procedure SetChecked(Value: Boolean);
    procedure SetEnabled(Value: Boolean);
    procedure SetHelpContext(Value: Integer);
    procedure SetName(const Value: string);
    procedure SetParent(const Value: string);
    procedure SetPosition(Value: Integer);
    procedure SetVerb(const Value: string);
  protected  // IOTAProjectManagerMenu
    function GetIsMultiSelectable: Boolean;
    procedure SetIsMultiSelectable(Value: Boolean);
    procedure Execute(const MenuContextList: IInterfaceList); overload;
    function PreExecute(const MenuContextList: IInterfaceList): Boolean;
    function PostExecute(const MenuContextList: IInterfaceList): Boolean;
  protected  // IOTAProjectManagerMenuExecute
    procedure SetExecute(aExec: TProjectMenuContextExecute);
  public
    procedure AfterConstruction; override;
  end;

implementation

procedure TNotifierOTA_ProjectManager_140.BeforeDestruction;
begin
  inherited;
  if FNotifierIndex <> -1 then
    (BorlandIDEServices as IOTAProjectManager).RemoveMenuItemCreatorNotifier(FNotifierIndex);
end;

procedure TNotifierOTA_ProjectManager_140.Setup;
begin
  FNotifier := FClass.Create;
  FNotifierIndex := (BorlandIDEServices as IOTAProjectManager).AddMenuItemCreatorNotifier(FNotifier as IOTAProjectMenuItemCreatorNotifier);
end;

procedure TNotifierOTA_ProjectManagerMenu.AfterConstruction;
begin
  inherited;
  SetChecked(False);
  SetEnabled(True);
  SetPosition(0);
  SetIsMultiSelectable(False);
end;

procedure TNotifierOTA_ProjectManagerMenu.Execute(const MenuContextList: IInterfaceList);
var i: integer;
begin
  for i := 0 to MenuContextList.Count - 1 do
    FExecute(MenuContextList[i] as IOTAProjectMenuContext);
end;

function TNotifierOTA_ProjectManagerMenu.GetCaption: string;
begin
  Result := FCaption;
end;

function TNotifierOTA_ProjectManagerMenu.GetChecked: Boolean;
begin
 Result := FChecked;
end;

function TNotifierOTA_ProjectManagerMenu.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

function TNotifierOTA_ProjectManagerMenu.GetHelpContext: Integer;
begin
  Result := FHelpContext;
end;

function TNotifierOTA_ProjectManagerMenu.GetIsMultiSelectable: Boolean;
begin
  Result := FIsMultiSelectable;
end;

function TNotifierOTA_ProjectManagerMenu.GetName: string;
begin
  Result := FName;
end;

function TNotifierOTA_ProjectManagerMenu.GetParent: string;
begin
  Result := FParent;
end;

function TNotifierOTA_ProjectManagerMenu.GetPosition: Integer;
begin
  Result := FPosition;
end;

function TNotifierOTA_ProjectManagerMenu.GetVerb: string;
begin
  Result := FVerb;
end;

function TNotifierOTA_ProjectManagerMenu.PostExecute(const MenuContextList: IInterfaceList): Boolean;
begin

end;

function TNotifierOTA_ProjectManagerMenu.PreExecute(const MenuContextList: IInterfaceList): Boolean;
begin

end;

procedure TNotifierOTA_ProjectManagerMenu.SetCaption(const Value: string);
begin
  FCaption := Value;
end;

procedure TNotifierOTA_ProjectManagerMenu.SetChecked(Value: Boolean);
begin
  FChecked := Value;
end;

procedure TNotifierOTA_ProjectManagerMenu.SetEnabled(Value: Boolean);
begin
  FEnabled := Value;
end;

procedure TNotifierOTA_ProjectManagerMenu.SetExecute(aExec: TProjectMenuContextExecute);
begin
  FExecute := aExec;
end;

procedure TNotifierOTA_ProjectManagerMenu.SetHelpContext(Value: Integer);
begin
  FHelpContext := Value;
end;

procedure TNotifierOTA_ProjectManagerMenu.SetIsMultiSelectable(Value: Boolean);
begin
  FIsMultiSelectable := Value;
end;

procedure TNotifierOTA_ProjectManagerMenu.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TNotifierOTA_ProjectManagerMenu.SetParent(const Value: string);
begin
  FParent := Value;
end;

procedure TNotifierOTA_ProjectManagerMenu.SetPosition(Value: Integer);
begin
  FPosition := Value;
end;

procedure TNotifierOTA_ProjectManagerMenu.SetVerb(const Value: string);
begin
  FVerb := Value;
end;

end.
