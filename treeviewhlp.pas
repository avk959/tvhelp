{****************************************************************************
*                                                                           *
*   Helper utilities to save/load TreeView in JSON format.                  *
*                                                                           *
*   Copyright(c) 2025 A.Koverdyaev(avk)                                     *
*                                                                           *
*   This code is free software; you can redistribute it and/or modify it    *
*   under the terms of the MIT License.                                     *
*                                                                           *
*  Unless required by applicable law or agreed to in writing, software      *
*  distributed under the License is distributed on an "AS IS" BASIS,        *
*  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. *
*  See the License for the specific language governing permissions and      *
*  limitations under the License.                                           *
*                                                                           *
*****************************************************************************}
unit TreeViewHlp;

{$MODE OBJFPC}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, ZStream;

type
  TTvStoreProp = (
    tspExpanded, tspSelected, tspFocused, tspVisible, tspEnabled, tspTag,
    tspImageIndex, tspSelectedIndex, tspStateIndex, tspOverlayIndex
  );
  TTvStoreProps = set of TTvStoreProp;

const
  TV_STORE_ALL = [
    tspExpanded, tspSelected, tspFocused, tspVisible, tspEnabled, tspTag,
    tspImageIndex, tspSelectedIndex, tspStateIndex, tspOverlayIndex
  ];
  TV_STORE_MOST = [
    tspExpanded, tspSelected, tspFocused, tspVisible, tspEnabled,
    tspImageIndex, tspSelectedIndex, tspStateIndex, tspOverlayIndex
  ];
  TV_STORE_INDEX = [tspImageIndex, tspSelectedIndex, tspStateIndex, tspOverlayIndex];

type
  TTreeNodeHelper = class helper for TTreeNode
  private
    function  GetTag: SizeInt; inline;
    procedure SetTag(aValue: SizeInt); inline;
  public
  { mapped to property TTreeNode.Data }
    property Tag: SizeInt read GetTag write SetTag;
  end;

  TCompressLevel = ZStream.TCompressionLevel;

  { TTreeViewJsonHelper }
  TTreeViewJsonHelper = class helper for TCustomTreeView
  private
  type
    TZMagic = array[0..5] of AnsiChar;
  const
  {$PUSH}{$J-}
    Z_MAGIC: TZMagic = 'z_Json';
  {$POP}
  private
    procedure DoSaveZJson(aStream: TStream; aProps: TTvStoreProps; aLevel: TCompressLevel);
    procedure DoLoadZJson(aStream: TStream; aProps: TTvStoreProps);
  public
    procedure SaveToJson(aStream: TStream; aProps: TTvStoreProps = TV_STORE_MOST);
    function  SaveToJson(aProps: TTvStoreProps = TV_STORE_MOST): string;
    procedure SaveToJson(const aFileName: string; aProps: TTvStoreProps = TV_STORE_MOST);
  { saves properties as zlib-compressed JSON }
    function  SaveToZJson(aProps: TTvStoreProps = TV_STORE_MOST; aLevel: TCompressLevel = clDefault): TBytes;
    procedure SaveToZJson(const aFileName: string; aProps: TTvStoreProps = TV_STORE_MOST;
                          aLevel: TCompressLevel = clDefault);
  { loading procedures may raise an exception if something goes wrong }
    procedure LoadJson(aStream: TStream; aProps: TTvStoreProps = TV_STORE_MOST);
    procedure LoadJson(const aJson: string; aProps: TTvStoreProps = TV_STORE_MOST);
    procedure LoadJsonFile(const aFileName: string; aProps: TTvStoreProps = TV_STORE_MOST);
    procedure LoadZJson(const aBytes: TBytes; aProps: TTvStoreProps = TV_STORE_MOST);
    procedure LoadZJson(const aFileName: string; aProps: TTvStoreProps = TV_STORE_MOST);
  end;

  ETreeViewJsonLoad = class(Exception);

resourcestring
  SETvUnexpectFmt = 'Unexpected stream format';

implementation
{$B-}
{$WARN 6058 OFF : Call to subroutine "$1" marked as inline is not inlined }

uses
  LgHelpers, LgVector, LgJson;

{ TTreeNodeHelper }

{$PUSH}{$WARN 4055 OFF : Conversion between ordinals and pointers is not portable}
function TTreeNodeHelper.GetTag: SizeInt;
begin
  Result := SizeInt(Self.Data);
end;

procedure TTreeNodeHelper.SetTag(aValue: SizeInt);
begin
  Self.Data := Pointer(aValue);
end;
{$POP}

const
  csText          = 'text';
  csEnabled       = 'enabled';
  csVisible       = 'visible';
  csFocused       = 'focused';
  csSelected      = 'selected';
  csExpanded      = 'expanded';
  csTag           = 'tag';
  csImageIndex    = 'imgIndex';
  csSelectedIndex = 'selIndex';
  csStateIndex    = 'staIndex';
  csOverlayIndex  = 'ovlIndex';
  csItems         = 'items';
  csNodes         = 'nodes';
  csRootNode      = 'treeView';

procedure TvSaveToJson(aTv: TCustomTreeView; aStream: TStream; aProps: TTvStoreProps);
var
  Writer: TJsonWriter = nil;

  procedure WriteNode(aNode: TTreeNode); forward;

  procedure WriteItems(aNode: TTreeNode);
  begin
    while aNode <> nil do begin
      Writer.BeginObject;
        WriteNode(aNode);
      Writer.EndObject;
      aNode := aNode.GetNextSibling;
    end;
  end;

  procedure WriteNode(aNode: TTreeNode);
  begin
    if aNode = nil then exit;
    Writer.Add(csText, aNode.Text);
    if (tspExpanded in aProps) and aNode.Expanded then
      Writer.AddTrue(csExpanded);
    if (tspVisible in aProps) and not aNode.Visible then
      Writer.AddFalse(csVisible);
    if (tspEnabled in aProps) and not aNode.Enabled then
      Writer.AddFalse(csEnabled);
    if (tspFocused in aProps) and aNode.Focused then
      Writer.AddTrue(csFocused);
    if (tspSelected in aProps) and aNode.Selected then
      Writer.AddTrue(csSelected);

    if (tspTag in aProps) and (aNode.Tag <> 0) then
      Writer.Add(csTag, aNode.Tag.ToString);
    if (tspImageIndex in aProps) and (aNode.ImageIndex > -1) then
      Writer.Add(csImageIndex, Integer(aNode.ImageIndex));
    if (tspSelectedIndex in aProps) and (aNode.SelectedIndex > -1) then
      Writer.Add(csSelectedIndex, aNode.SelectedIndex);
    if (tspStateIndex in aProps) and (aNode.StateIndex > -1) then
      Writer.Add(csStateIndex, aNode.StateIndex);
    if (tspOverlayIndex in aProps) and (aNode.OverlayIndex > -1) then
      Writer.Add(csOverlayIndex, aNode.OverlayIndex);

    Writer.AddName(csItems).BeginArray;
      WriteItems(aNode.GetFirstChild);
    Writer.EndArray;
  end;
begin
  Writer := TJsonWriter.Create(aStream);
  try
    Writer.BeginObject
      .AddName(csRootNode).BeginObject
        .AddName(csNodes).BeginArray;
          if aTv.Items.Count > 0 then
            WriteItems(aTv.Items[0]);
    Writer.EndArray.EndObject.EndObject;
  finally
    Writer.Free;
  end;
end;

function TryStr2Int(const s: string; out si: SizeInt): Boolean; inline;
begin
{$IF DEFINED(CPU64)}
  Result := TryStrToInt64(s, si);
{$ELSEIF DEFINED(CPU32)}
  Result := TryStrToInt(s, si);
{$ELSE}
  {$ERROR Not implemented}
{$ENDIF}
end;

function TvLoadJson(aTv: TCustomTreeView; aStream: TStream; aProps: TTvStoreProps): Boolean;
var
  Reader: TJsonReader = nil;
  ToExpand: specialize TGLiteVector<TTreeNode>;

  function IsNonNegativeInt(out aValue: Integer): Boolean;
  var
    I: Int64;
  begin
    if not((Reader.TokenKind = tkNumber) and Double.IsExactInt(Reader.AsNumber, I))then
      exit(False);
    Result := (I >= 0) and (I <= MaxInt);
    if Result then
      aValue := Integer(I);
  end;

  function ReadNode(aNode: TTreeNode): Boolean; forward;

  function ReadItems(aNode: TTreeNode): Boolean;
  var
    FirstItem: Boolean;
  begin
    if Reader.TokenKind <> tkArrayBegin then exit(False);
    FirstItem := True;
    while Reader.Read do begin
      if Reader.TokenKind = tkArrayEnd then break;
      if Reader.TokenKind = tkObjectEnd then continue;
      if FirstItem then begin
        aNode := aTv.Items.AddChild(aNode, '');
        FirstItem := False;
      end else
        aNode := aTv.Items.Add(aNode, '');
      if not ReadNode(aNode) then exit(False);
    end;
    Result := Reader.ReadState = rsGo;
  end;

  function ReadNode(aNode: TTreeNode): Boolean;
  var
    I: Integer;
    sI: SizeInt;
  begin
    if Reader.TokenKind <> tkObjectBegin then exit(False);
    while Reader.Read do begin
      if Reader.TokenKind = tkObjectEnd then break;
      case Reader.Name of
        csText:
          begin
            if Reader.TokenKind <> tkString then exit(False);
            aNode.Text := Reader.AsString;
          end;
        csEnabled:
          begin
            if Reader.TokenKind <> tkFalse then exit(False);
            aNode.Enabled := not(tspEnabled in aProps);
          end;
        csVisible:
          begin
            if Reader.TokenKind <> tkFalse then exit(False);
            aNode.Visible := not(tspVisible in aProps);
          end;
        csFocused:
          begin
            if Reader.TokenKind <> tkTrue then exit(False);
            aNode.Focused := tspFocused in aProps;
          end;
        csSelected:
          begin
            if Reader.TokenKind <> tkTrue then exit(False);
            aNode.Selected := tspSelected in aProps;
          end;
        csExpanded:
          begin
            if Reader.TokenKind <> tkTrue then exit(False);
            if tspExpanded in aProps then ToExpand.Add(aNode);
          end;
        csTag:
          begin
            if Reader.TokenKind <> tkString then exit(False);
            if not TryStr2Int(Reader.AsString, si) or (si = 0) then exit(False);
            if tspTag in aProps then aNode.Tag := si;
          end;
        csImageIndex:
          begin
            if not IsNonNegativeInt(I) then exit(False);
            if tspImageIndex in aProps then aNode.ImageIndex := I;
          end;
        csSelectedIndex:
          begin
            if not IsNonNegativeInt(I) then exit(False);
            if tspSelectedIndex in aProps then aNode.SelectedIndex := I;
          end;
        csStateIndex:
          begin
            if not IsNonNegativeInt(I) then exit(False);
            if tspStateIndex in aProps then aNode.StateIndex := I;
          end;
        csOverlayIndex:
          begin
            if not IsNonNegativeInt(I) then exit(False);
            if tspOverlayIndex in aProps then aNode.OverlayIndex := I;
          end;
        csItems:
          if not ReadItems(aNode) then exit(False);
      else
        exit(False);
      end;
    end;
    Result := Reader.ReadState = rsGo;
  end;
var
  Node: TTreeNode;
begin
  Reader := TJsonReader.Create(aStream);
  try
    aTv.BeginUpdate;
    try
      aTv.Items.Clear;
      if not(Reader.Read and (Reader.TokenKind = tkObjectBegin)) then exit(False);
      if not(Reader.Read and (Reader.Name = csRootNode)) then exit(False);
      if not(Reader.Read and (Reader.Name = csNodes)) then exit(False);
      if not ReadItems(nil) then exit(False);
      if not(Reader.Read and (Reader.TokenKind = tkObjectEnd)) then exit(False);
      if not(Reader.Read and (Reader.TokenKind = tkObjectEnd)) then exit(False);
      if Reader.Read or (Reader.ReadState <> rsEOF) then exit(False);
      for Node in ToExpand do
        Node.Expanded := True;
      Result := True;
    finally
      aTv.EndUpdate;
    end;
  finally
    Reader.Free;
  end;
end;

{ TTreeViewJsonHelper }

procedure TTreeViewJsonHelper.DoSaveZJson(aStream: TStream; aProps: TTvStoreProps; aLevel: TCompressLevel);
var
  cs: TCompressionStream;
begin
  cs := TCompressionStream.Create(aLevel, aStream, True);
  try
    cs.WriteBuffer(Z_MAGIC, SizeOf(Z_MAGIC));
    SaveToJson(cs, aProps);
  finally
    cs.Free;
  end;
end;

{$PUSH}{$WARN 5057 OFF : Local variable "$1" does not seem to be initialized}
procedure TTreeViewJsonHelper.DoLoadZJson(aStream: TStream; aProps: TTvStoreProps);
var
  dcs: TDecompressionStream;
  m: TZMagic;
begin
  dcs := TDecompressionStream.Create(aStream, True);
  try
    dcs.ReadBuffer(m, SizeOf(m));
    if m <> Z_MAGIC then
      raise Exception.Create(SETvUnexpectFmt);
    LoadJson(dcs, aProps);
  finally
    dcs.Free;
  end;
end;
{$POP}

procedure TTreeViewJsonHelper.SaveToJson(aStream: TStream; aProps: TTvStoreProps);
begin
  TvSaveToJson(Self, aStream, aProps);
end;

function TTreeViewJsonHelper.SaveToJson(aProps: TTvStoreProps): string;
var
  ss: TStringStream;
begin
  ss := TStringStream.Create('', CP_UTF8);
  try
    TvSaveToJson(Self, ss, aProps);
    Result := ss.DataString;
  finally
    ss.Free;
  end;
end;

procedure TTreeViewJsonHelper.SaveToJson(const aFileName: string; aProps: TTvStoreProps);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(aFileName, fmCreate);
  try
    TvSaveToJson(Self, fs, aProps);
  finally
    fs.Free;
  end;
end;

function TTreeViewJsonHelper.SaveToZJson(aProps: TTvStoreProps; aLevel: TCompressLevel): TBytes;
var
  bs: TBytesStream;
  sz: SizeInt;
begin
  bs := TBytesStream.Create;
  try
    DoSaveZJson(bs, aProps, aLevel);
    Result := bs.Bytes;
    sz := bs.Size;
  finally
    bs.Free;
  end;
  SetLength(Result, sz);
end;

procedure TTreeViewJsonHelper.SaveToZJson(const aFileName: string; aProps: TTvStoreProps; aLevel: TCompressLevel);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(aFileName, fmCreate);
  try
    DoSaveZJson(fs, aProps, aLevel);
  finally
    fs.Free;
  end;
end;

procedure TTreeViewJsonHelper.LoadJson(aStream: TStream; aProps: TTvStoreProps);
begin
  if not TvLoadJson(Self, aStream, aProps) then
    raise ETreeViewJsonLoad.Create(SETvUnexpectFmt);
end;

procedure TTreeViewJsonHelper.LoadJson(const aJson: string; aProps: TTvStoreProps);
var
  ss: TStringStream;
begin
  ss := TStringStream.Create(aJson, CP_UTF8);
  try
    if not TvLoadJson(Self, ss, aProps) then
      raise ETreeViewJsonLoad.Create(SETvUnexpectFmt);
  finally
    ss.Free;
  end;
end;

procedure TTreeViewJsonHelper.LoadJsonFile(const aFileName: string; aProps: TTvStoreProps);
var
  fs: TFileStream;
begin
  try
    fs := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyWrite);
    try
      LoadJson(fs, aProps);
    finally
      fs.Free;
    end;
  except
    on e: Exception do
      raise ETreeViewJsonLoad.Create(e.Message);
  end;
end;

procedure TTreeViewJsonHelper.LoadZJson(const aBytes: TBytes; aProps: TTvStoreProps);
var
  bs: TBytesStream;
begin
  try
    bs := TBytesStream.Create(aBytes);
    try
      DoLoadZJson(bs, aProps);
    finally
      bs.Free;
    end;
  except
    on e: Exception do
      raise ETreeViewJsonLoad.Create(e.Message);
  end;
end;

procedure TTreeViewJsonHelper.LoadZJson(const aFileName: string; aProps: TTvStoreProps);
var
  fs: TFileStream;
begin
  try
    fs := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyWrite);
    try
      DoLoadZJson(fs, aProps);
    finally
      fs.Free;
    end;
  except
    on e: Exception do
      raise ETreeViewJsonLoad.Create(e.Message);
  end;
end;

end.
