unit OTA.ProjectManagerMenu;

interface

uses
  System.Classes, System.SysUtils, ToolsAPI;

type
  TNotifierObject_ProjectManagerMenu = class(TNotifierObject, IOTALocalMenu,
      IOTAProjectManagerMenu)
  type
    TExecuteProc = reference to procedure(const aMenuContextList: IInterfaceList);
  strict private
    FCaption: string;
    FChecked: Boolean;
    FEnabled: Boolean;
    FExecuteProc: TExecuteProc;
    FHelpContext: Integer;
    FIsMultiSelectable: Boolean;
    FName: string;
    FParent: string;
    FPosition: Integer;
    FVerb: string;
  strict protected
    { IOTALocalMenu }
    function GetCaption: string;
    function GetChecked: Boolean;
    function GetEnabled: Boolean;
    function GetHelpContext: Integer;
    function GetName: string;
    function GetParent: string;
    function GetPosition: Integer;
    function GetVerb: string;
    procedure SetCaption(const AValue: string);
    procedure SetChecked(AValue: Boolean);
    procedure SetEnabled(AValue: Boolean);
    procedure SetHelpContext(AValue: Integer);
    procedure SetName(const AValue: string);
    procedure SetParent(const AValue: string);
    procedure SetPosition(AValue: Integer);
    procedure SetVerb(const AValue: string);
    { IOTAProjectManagerMenu }
    procedure Execute(const AMenuContextList: IInterfaceList); overload;
    function GetIsMultiSelectable: Boolean;
    function PostExecute(const AMenuContextList: IInterfaceList): Boolean;
    function PreExecute(const AMenuContextList: IInterfaceList): Boolean;
    procedure SetIsMultiSelectable(AValue: Boolean);
  public
    constructor Create(const ACaption, AVerb: string; const APosition: Integer;
        const AExecuteProc: TExecuteProc = nil; const AName: string = ''; aChecked:
        Boolean = False; aEnabled: Boolean = True; const AParent: string = '';
        aIsMultiSelectable: Boolean = False);
  end;

implementation

constructor TNotifierObject_ProjectManagerMenu.Create(const ACaption, AVerb:
    string; const APosition: Integer; const AExecuteProc: TExecuteProc = nil;
    const AName: string = ''; aChecked: Boolean = False; aEnabled: Boolean =
    True; const AParent: string = ''; aIsMultiSelectable: Boolean = False);
begin
  inherited Create;
  SetCaption(ACaption);
  SetIsMultiSelectable(aIsMultiSelectable);
  SetName(AName);
  SetChecked(aChecked);
  SetEnabled(aEnabled);
  SetParent(AParent);
  SetPosition(APosition);
  SetVerb(AVerb);
  FExecuteProc := AExecuteProc;
end;

procedure TNotifierObject_ProjectManagerMenu.Execute(const AMenuContextList: IInterfaceList);
begin
  if Assigned(FExecuteProc) then
    FExecuteProc(AMenuContextList);
end;

function TNotifierObject_ProjectManagerMenu.GetCaption: string;
begin
  Result := FCaption;
end;

function TNotifierObject_ProjectManagerMenu.GetChecked: Boolean;
begin
  Result := FChecked;
end;

function TNotifierObject_ProjectManagerMenu.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

function TNotifierObject_ProjectManagerMenu.GetHelpContext: Integer;
begin
  Result := 0;
end;

function TNotifierObject_ProjectManagerMenu.GetIsMultiSelectable: Boolean;
begin
  Result := False;
end;

function TNotifierObject_ProjectManagerMenu.GetName: string;
begin
  Result := FName;
end;

function TNotifierObject_ProjectManagerMenu.GetParent: string;
begin
  Result := FParent;
end;

function TNotifierObject_ProjectManagerMenu.GetPosition: Integer;
begin
  Result := FPosition;
end;

function TNotifierObject_ProjectManagerMenu.GetVerb: string;
begin
  Result := FVerb;
end;

function TNotifierObject_ProjectManagerMenu.PostExecute(const AMenuContextList: IInterfaceList): Boolean;
begin
  Result := False;
end;

function TNotifierObject_ProjectManagerMenu.PreExecute(const AMenuContextList: IInterfaceList): Boolean;
begin
  Result := False;
end;

procedure TNotifierObject_ProjectManagerMenu.SetCaption(const AValue: string);
begin
  FCaption := aValue;
end;

procedure TNotifierObject_ProjectManagerMenu.SetChecked(AValue: Boolean);
begin
  FChecked := AValue;
end;

procedure TNotifierObject_ProjectManagerMenu.SetEnabled(AValue: Boolean);
begin
  FEnabled := AValue;
end;

procedure TNotifierObject_ProjectManagerMenu.SetHelpContext(AValue: Integer);
begin
  FHelpContext := AValue;
end;

procedure TNotifierObject_ProjectManagerMenu.SetIsMultiSelectable(AValue: Boolean);
begin
  FIsMultiSelectable := AValue;
end;

procedure TNotifierObject_ProjectManagerMenu.SetName(const AValue: string);
begin
  FName := AValue;
end;

procedure TNotifierObject_ProjectManagerMenu.SetParent(const AValue: string);
begin
  FParent := AValue;
end;

procedure TNotifierObject_ProjectManagerMenu.SetPosition(AValue: Integer);
begin
  FPosition := AValue;
end;

procedure TNotifierObject_ProjectManagerMenu.SetVerb(const AValue: string);
begin
  FVerb := AValue;
end;

end.
