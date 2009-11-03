unit OTA.SearchProject;

interface

uses
  OTA.IDE, ToolsAPI, Menus;

type
  TSearchProject = class(TNotifierObject, INTAProjectMenuCreatorNotifier)
  private
    FText: string;
    function FindActiveProjectIndex: Integer;
    procedure OnMenuClick(Sender: TObject);
  protected
  class var
    function AddMenu(const Ident: string): TMenuItem;
    function CanHandle(const Ident: string): Boolean;
  public
  end;

implementation

uses SysUtils, Dialogs;

function TSearchProject.AddMenu(const Ident: string): TMenuItem;
begin
  Result := TMenuItem.Create(nil);
  Result.Caption := 'Search Project';
  Result.ShortCut := TextToShortCut('F3');
  Result.OnClick := OnMenuClick;
end;

function TSearchProject.CanHandle(const Ident: string): Boolean;
begin
  Result := SameText(Ident, sProjectContainer);
end;

function TSearchProject.FindActiveProjectIndex: Integer;
var G: IOTAProjectGroup;
    i: integer;
begin
  Result := -1;
  G := (BorlandIDEServices as IOTAModuleServices).MainProjectGroup;
  for i := 0 to G.ProjectCount - 1 do begin
    if G.Projects[i] = G.ActiveProject then begin
      Result := i;
      Break;
    end;
  end;
end;

procedure TSearchProject.OnMenuClick(Sender: TObject);
var i, lActiveIndex: integer;
    G: IOTAProjectGroup;
begin
  if InputQuery('Find Project', 'Project to find', FText) then begin
    G := (BorlandIDEServices as IOTAModuleServices).MainProjectGroup;
    lActiveIndex := FindActiveProjectIndex + 1;
    if lActiveIndex >= G.ProjectCount then
      lActiveIndex := 0; //start from first
    for i := lActiveIndex to G.ProjectCount - 1 do begin
      if Pos(FText, G.Projects[i].ProjectOptions.TargetName) > 0 then begin
        G.ActiveProject := G.Projects[i];
        Break;
      end;
    end;
  end;
end;

initialization
  TOTAFactory.Register(TNotifierOTA_ProjectManager.Create(TSearchProject));

end.
