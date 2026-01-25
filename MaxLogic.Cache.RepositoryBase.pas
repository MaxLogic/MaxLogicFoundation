unit MaxLogic.Cache.RepositoryBase;

{$I fpc_delphimode.inc}
{$SCOPEDENUMS ON}

interface

uses
  System.SysUtils,
  MaxLogic.Cache;

type
  TMaxCacheRepositoryBase = class abstract
  private
    fCache: IMaxCache;
  protected
    class function TenantNamespace(const aDbName: string): string; static;
    class function FilesNamespace: string; static;
    class function SiteNamespace(const aSiteId: string): string; static;
    property Cache: IMaxCache read fCache;
  public
    constructor Create(const aCache: IMaxCache);
  end;

implementation

constructor TMaxCacheRepositoryBase.Create(const aCache: IMaxCache);
begin
  inherited Create;
  if aCache = nil then
    raise EArgumentNilException.Create('Cache is required');
  fCache := aCache;
end;

class function TMaxCacheRepositoryBase.TenantNamespace(const aDbName: string): string;
begin
  Result := 'tenant:' + aDbName;
end;

class function TMaxCacheRepositoryBase.FilesNamespace: string;
begin
  Result := 'files';
end;

class function TMaxCacheRepositoryBase.SiteNamespace(const aSiteId: string): string;
begin
  Result := 'site:' + aSiteId;
end;

end.

