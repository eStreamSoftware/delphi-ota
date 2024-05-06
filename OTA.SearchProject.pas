unit OTA.SearchProject;

interface

uses
  System.Classes, System.SysUtils, Vcl.Menus, ToolsAPI;

type
  TSearchProject = class(TNotifierObject, IOTAProjectMenuItemCreatorNotifier,
      TFunc<IOTAProjectMenuItemCreatorNotifier>, TFunc<IOTAKeyboardBinding>)
  private
    FText: string;
    procedure DoExecute(const aMenuContextList: IInterfaceList);
    function FindActiveProjectIndex: Integer;
  protected
    procedure AddMenu(const Project: IOTAProject; const IdentList: TStrings; const
        ProjectManagerMenuList: IInterfaceList; IsMultiSelect: Boolean);
    function TFunc<IOTAKeyboardBinding>.Invoke = Invoke_IOTAKeyboardBinding;
    function TFunc<IOTAProjectMenuItemCreatorNotifier>.Invoke = Invoke_IOTAProjectMenuItemCreatorNotifier;
    function Invoke_IOTAKeyboardBinding: IOTAKeyboardBinding;
    function Invoke_IOTAProjectMenuItemCreatorNotifier: IOTAProjectMenuItemCreatorNotifier;
  end;

implementation

uses
  Winapi.Windows, Vcl.Dialogs,
  OTA.KeyboardBinding, OTA.ProjectManagerMenu;

procedure TSearchProject.AddMenu(const Project: IOTAProject;
    const IdentList: TStrings; const ProjectManagerMenuList: IInterfaceList;
    IsMultiSelect: Boolean);
begin
  if Assigned(Project) and (IdentList.Contains(sProjectContainer) or IdentList.Contains(sProjectGroupContainer)) then
    ProjectManagerMenuList.Add(TNotifierObject_ProjectManagerMenu.Create('Search Project', '', pmmpUserReorder, DoExecute, ClassName) as IOTAProjectManagerMenu);
end;

procedure TSearchProject.DoExecute(const aMenuContextList: IInterfaceList);
begin
  if InputQuery('Find Project', 'Name:', FText) then begin
    var G := (BorlandIDEServices as IOTAModuleServices).MainProjectGroup;
    var iActive := FindActiveProjectIndex + 1;
    if iActive >= G.ProjectCount then
      iActive := 0; //start from first
    for var i := iActive to G.ProjectCount - 1 do begin
      if G.Projects[i].ProjectOptions.TargetName.ToLower.Contains(FText.ToLower) then begin
        G.ActiveProject := G.Projects[i];
        Break;
      end;
    end;
  end;
end;

function TSearchProject.FindActiveProjectIndex: Integer;
begin
  Result := -1;
  var G := (BorlandIDEServices as IOTAModuleServices).MainProjectGroup;
  for var i := 0 to G.ProjectCount - 1 do begin
    if G.Projects[i] = G.ActiveProject then begin
      Result := i;
      Break;
    end;
  end;
end;

function TSearchProject.Invoke_IOTAKeyboardBinding: IOTAKeyboardBinding;
begin
  Result := TOTA_KeyboardBinding.Create(ClassName, '', [ShortCut(VK_F3, [])]) as IOTAKeyboardBinding;
end;

function TSearchProject.Invoke_IOTAProjectMenuItemCreatorNotifier: IOTAProjectMenuItemCreatorNotifier;
begin
  Result := Self as IOTAProjectMenuItemCreatorNotifier;
end;

end.
