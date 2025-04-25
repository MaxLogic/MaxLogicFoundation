unit Maxlogic.GdiUtils;

interface

uses
  Winapi.Windows;

/// <summary>
/// Retrieves the count of GUI resources (GDI or User objects)
/// currently used by the calling process.
/// </summary>
/// <param name="aResourceType">Specify GR_GDIOBJECTS or GR_USEROBJECTS.</param>
/// <returns>The number of handles, or 0 on failure.</returns>
function GetGuiResourceCount(aResourceType: DWORD): DWORD;

/// <summary>
/// Gets the current count of GDI objects (bitmaps, pens, brushes, fonts, DCs, regions)
/// used by this process.
/// </summary>
/// <returns>The number of GDI handles.</returns>
function GetGDIObjectCount: DWORD;

/// <summary>
/// Gets the current count of User objects (windows, menus, cursors, icons, etc.)
/// used by this process.
/// </summary>
/// <returns>The number of User handles.</returns>
function GetUserObjectCount: DWORD;

implementation

function GetGuiResourceCount(aResourceType: DWORD): DWORD;
var
  lProcessHandle: THandle;
begin
  lProcessHandle := GetCurrentProcess(); // Gets a pseudo-handle for the current process
  Result := Winapi.Windows.GetGuiResources(lProcessHandle, aResourceType);
  // No need to close the pseudo-handle returned by GetCurrentProcess
end;

function GetGDIObjectCount: DWORD;
begin
  Result := GetGuiResourceCount(GR_GDIOBJECTS);
end;

function GetUserObjectCount: DWORD;
begin
  Result := GetGuiResourceCount(GR_USEROBJECTS);
end;

end.

