unit helpers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, graphics;

function FormatByteSize(const bytes: longint): string;
function TimeModificationFichier(fichier: string): TDateTime;
procedure FileSearch(const PathName: string; const Extensions: string; const Recursive: boolean; var lstFiles: TStringList);
function GetFileSize(const APath: string): int64;
function GetNextFileName(AFolder, AFile: string): string;

implementation

//Format file byte size
function FormatByteSize(const bytes: longint): string;
const
  B = 1; //byte
  KB = 1024 * B; //kilobyte
  MB = 1024 * KB; //megabyte
  GB = 1024 * MB; //gigabyte
begin
  if bytes > GB then
    Result := FormatFloat('#.## GB', bytes / GB)
  else if bytes > MB then
    Result := FormatFloat('#.## MB', bytes / MB)
  else if bytes > KB then
    Result := FormatFloat('#.## KB', bytes / KB)
  else
    Result := FormatFloat('#.## bytes', bytes);
end;

{===========================================================================}
{ fonction de split classique                                               }
{===========================================================================}
function split(chaine: string; delimiteur: string): TStringList;
begin
  Result := TStringList.Create;
  Result.Text := StringReplace(chaine, delimiteur, #13#10, [rfReplaceAll]);
end;

{===========================================================================}
{ fonction renvoyant la date et heure de la dernière modification du fichier}
{===========================================================================}
function TimeModificationFichier(fichier: string): TDateTime;
var
  SearchRec: TSearchRec;
  Resultat: longint;
begin
  Result := 0;
  Resultat := FindFirst(fichier, FaAnyFile, SearchRec);
  if Resultat = 0 then
    Result := FileDateToDateTime(SearchRec.Time);
  // FileDateToDateTime transforme une date de type dos en format TDateTime
  FindClose(SearchRec);
end;

{===========================================================================}
{ Scanne un dossier en recherchant les fichiers avec les extensions définies}
{===========================================================================}
procedure FileSearch(const PathName: string; const Extensions: string; const Recursive: boolean; var lstFiles: TStringList);
const
  FileMask = '*.*';
var
  Rec: TSearchRec;
  Path: string;
begin
  Path := IncludeTrailingBackslash(PathName);
  if FindFirst(Path + FileMask, faAnyFile - faDirectory, Rec) = 0 then
    try
      repeat

        if AnsiPos( ExtractFileExt(LowerCase (Rec.Name) ), Extensions) > 0 then lstFiles.Add( ConcatPaths([PathName, Rec.Name]) );

      until FindNext(Rec) <> 0;
    finally
      SysUtils.FindClose(Rec);
    end;

  if FindFirst(Path + '*.*', faDirectory, Rec) = 0 then
    try
      repeat

        if Recursive and ((Rec.Attr and faDirectory) <> 0) and (Rec.Name <> '.') and (Rec.Name <> '..') then
          FileSearch(Path + Rec.Name, Extensions, Recursive, lstFiles);

      until FindNext(Rec) <> 0;
    finally
      FindClose(Rec);
    end;
end;

{===========================================================================}
{ Renvoi la taille d'un fichier                                             }
{===========================================================================}
function GetFileSize(const APath: string): int64;
var
  Sr: TSearchRec;
begin
  if FindFirst(APath, faAnyFile, Sr) = 0 then
    try
      //Result := int64(Sr.FindData.nFileSizeHigh) shl 32 + Sr.FindData.nFileSizeLow;
      Result := Sr.Size;
    finally
      FindClose(Sr);
    end
  else
    Result := 0;
end;

{===========================================================================}
{ Renvoi le prochain nom de fichier disponible                              }
{===========================================================================}
function GetNextFileName(AFolder, AFile: string): string;
var
  v, v1: Integer;
  Body, Ext: string;
  sr: TSearchRec;

  function FileBody(FileName: string): string;
  begin
    Result := ChangeFileExt(FileName, '');
  end;

  function GetPostFix(FileName: string): Integer;
  begin
    Result := StrToIntDef(Copy(FileBody(FileName), Length(Body) + 1, 255), 0);
  end;

begin
  Result := AFile;
  v := 0;
  Body := FileBody(AFile);
  Ext := ExtractFileExt(AFile);
  if FindFirst(AFolder + Body + '*' + Ext, faAnyFile xor faDirectory, sr) = 0 then
  begin
    repeat
      v1 := GetPostFix(sr.Name);
      if v1 < v then
        v := v1;
    until
      FindNext(sr) <> 0;
    FindClose(sr);
    Result := Body + IntToStr(v - 1) + Ext;
  end;
end;


end.
