unit SearchResultItemUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CollectionUnit;

type

  { TSearchItem }

  TSearchItem= class (TObject)
    FLink: String;
    FMask: Integer;
    FRate: Double;
    FSearchEngineName: PChar;
    FSummary: String;
    FTitle: String;
    
  private
  public
    property Link: String read FLink;
    property Title: String read FTitle;
    property Summary: String read FSummary;
    property Rate: Double read FRate;
    property SearchEngineName: PChar read FSearchEngineName;
    property Mask: Integer read FMask;
    

    constructor Create (_Link, _Title, _Summary: String; SEName: PChar);
    destructor Destroy; override;
    
    function ToString: String;
    
  end;
  
  { TSearchItemCollection }

  TSearchItemCollection= class (TBaseCollection)
  private
    FTotalNumber: Int64;
    Mutex: TRTLCriticalSection;
    
    function GetSearchItem (Index: Integer): TSearchItem;

  public
    property SearchItem [Index: Integer]: TSearchItem read GetSearchItem;
    property TotalNumber: Int64 read FTotalNumber write FTotalNumber;
    
    constructor Create;
    destructor Destroy; override;
    
    procedure Add (Item: TSearchItem);
    
    function ToString: String;
    function ToString (StartIndex, EndIndex: Integer): String;
    
    procedure MergeWith (AnotherCollection: TSearchItemCollection);
    
  end;
  
implementation
uses
  MyTypes;
  
{ TSearchItem }

constructor TSearchItem.Create (_Link, _Title, _Summary: String; SEName: PChar);
begin
  inherited Create;
  
  FLink:= _Link;
  FTitle:= _Title;
  FSummary:= _Summary;
  FSearchEngineName:= SEName;
  
end;

destructor TSearchItem.Destroy;
begin
  inherited Destroy;
  
end;

function TSearchItem.ToString: String;
var
  SEIndex: Integer;
  m: Integer;
  
begin
  Result:= Result+ '<TR><TD> <P dir="rtl" align="right"><font face="Tahoma" size="2">';

  SEIndex:= 1;
  m:= Mask;
  while M<> 0 do
  begin
    if (M and 1)<> 0 then
      Result:= '<IMG src="/Images/SearchEngine'+ IntToStr (SEIndex)+
      '.gif" align="absbottom" border="0" height="16" width="16"/>';
      
    m:= m shr 1;
    Inc (SEIndex);
    
  end;

  Result+= '<A style="text-decoration: none" href=""'+ FLink+ '\>'+ '&nbsp;&nbsp;'+
      FTitle+ '<B></B></A>';

  if FSummary<> '' then
  begin
{    RemoveSubstrFromString (FSummary, '<br>");
    RemoveSubstrFromString (FSummary, '<BR>");}
    Result+= '<P dir= "rtl" align= "right"><FONT face= "Tahoma" size= "2">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;'+
      FSummary+'</FONT></P>';
    
  end;
  

  Result+= '</BR></BR>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;'+ 'آدرس اصلي:' +'<FONT color=\"#008000\"><SPAN lang="en-us">';
  if 60< Length (FLink) then
    Result+= Copy (FLink, 1, 58)+ '...'
  else
    Result+= FLink;
    
  Result+= '</SPAN></FONT></BR></BR>&nbsp;';
  Result+= '</TR></TD>';

end;

{ TSearchItemCollection }

function TSearchItemCollection.GetSearchItem (Index: Integer): TSearchItem;
begin
  Result:= Member [Index] as TSearchItem;
  
end;

constructor TSearchItemCollection.Create;
begin
  InitCriticalSection (Mutex);
  
end;

destructor TSearchItemCollection.Destroy;
begin
  DoneCriticalsection (Mutex);
  
  inherited Destroy;
  
end;

procedure TSearchItemCollection.Add (Item: TSearchItem);
begin
  EnterCriticalsection (Mutex);

  inherited Add (Item);
  
  LeaveCriticalsection (Mutex);
  
end;

function TSearchItemCollection.ToString: String;
const
  EndLine: char= #10;
var
  Ptr: PObject;
  i: Integer;
  
begin
  Ptr:= GetPointerToFirst;
  
  Result:= '';
  
  for i:= 1 to Size do
  begin
    Result+= (Ptr^ as TSearchItem).ToString+ EndLine;
    Inc (Ptr);
    
  end;

end;

function TSearchItemCollection.ToString (StartIndex, EndIndex: Integer): String;
const
  EndLine: char= #10;
  
var
  i, High: Integer;
  Ptr: PObject;
  
begin
  Result:= '';
  i:= EndIndex- StartIndex+ 1;
  
  if (Size<= EndIndex) or (StartIndex< 0) then
  begin
    EndIndex:= Size- 1;
    StartIndex:= EndIndex- i+ 1;
    
  end;
  
  if EndIndex< 0 then
    Exit;
  if StartIndex< 0 then
    StartIndex:= 0;
    
  Ptr:= GetPointerToFirst+ StartIndex;
  for i:= StartIndex to EndIndex do
  begin
    Result+= '<TABLE border= "0" width= "100%">';
    Result+= (Ptr^ as TSearchItem).ToString+ EndLine+ EndLine;
    Result+= '</TABLE>';
    
    Inc (Ptr);
    
  end;

end;

procedure TSearchItemCollection.MergeWith (AnotherCollection: TSearchItemCollection);
begin

end;

end.

