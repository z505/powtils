{$ifdef fpc} {$MODE OBJFPC} {$H+} {$endif}
{

  This is an HTML wrapper unit will help program HTML via Pascal instead of
  HTML, by using HTML wrappers. It will be useful when a programmer wants to
  output HTML in a clean way without all the < > < > messy characters in his
  program source code (the wrapper takes care of this).

  The idea is to use simple structures to hold html widgets, such as the DIV
  widget. The DIV widget is very flexible in that you can create panels, boxes,
  quotation areas, paragraph areas, footer areas, header areas, etc. all without
  the restrictions of a table.
  
  Code in this unit will be created and output any time the programmer wants,
  since DIV allows us to do this via Absolute Positioning  (tables do not allow
  us to do this).
  
  wHTM stands for "wrapper for HTML". Unit name is kept short and
  sweet so that when the user calls wHTM.function it is easy for him to type
  wHTM.myfunction or wHTM.myproc

  Since the functions in this unit are very genericly named, you may need to
  prefix calls to this unit with wHTM to avoid conflicts with other units.
  Other units other than wHTM may have functions with similar names. i.e. bold,
  italic, are very common words that might be used in some other unit as global
  variables or other conflicts may arise. In Pascal due to its unit/modular
  abilities, we can prefix the procedure call with the exact unit the procedure
  is in.

  
  --
  L505

--------------------------------------------------------------------------------
  Naming scheme
--------------------------------------------------------------------------------

 html widget structure types are prefixed with THtm. Css related structure types
 are prefixed with TCss.
 
 The OUT functions output strings to standard out (web browser), whereas the
 functions with no OUT just format the text and do not immediately output it.

  
--------------------------------------------------------------------------------
  TO DO
--------------------------------------------------------------------------------
  - make more widget structures


--------------------------------------------------------------------------------
  Authors
--------------------------------------------------------------------------------
  - Lars L505
  - Anthony Henry



--------------------------------------------------------------------------------

}

unit pwhtmw;


{$H+}

interface

uses
 {$IFDEF FPC}CompactSysUtils{$ELSE}Sysutils{$ENDIF},
  pwmain;

const
  //  For Form Tag Output
  GET = 0;
  POST = 1;
  MULTIPART = 3;

  // For Chkbox output
  CHECKED = TRUE;
  NOTCHECKED = FALSE;


type

{-- enumerations --------------------------------------------------------------}

  // relative or absolute positioning
  TCssPosition = (
    cpAbsolute,
    cpRelative
  );


type
  // horizontal content alignment
  TCssHAlign = (
    chaNone,
    chaLeft,
    chaRight,
    chaCenter,
    chaJustify
  );
  
  // vertical content alignment
  TCssVAlign = (
    cvaNone,
    cvaBaseline,
    cvaSub,
    cvaSuper,
    cvaTop,
    cvaTextTop,
    cvaNiddle,
    cvaBottom,
    cvaTextBottom
  );

const
  CSS_HALIGN: array [chaNone..chaJustify] of string =
    ('', 'left', 'right', 'center', 'justify');
  CSS_VALIGN: array [cvaNone..cvaTextBottom] of string =
    ('', 'baseline','sub','super','top', 'text-top',
     'middle', 'bottom', 'text-bottom');
type
  // CSS borders
  TCssBorder = (
    cbNone,
    cbDotted,
    cbDashed,
    cbSolid,
    cbDouble,
    cbGroove,
    cbRidge,
    cbInset,
    cbOutset
  );

  // CSS display attributes
  TCssDisplay = (
    cdNone,
    cdBlock,
    cdInline,
    cdListItem,
    cdRunIn,
    cdCompact,
    cdMarker,
    cdTable,
    cdInlineTable,
    cdTableRowGroup,
    cdTableHeaderGroup,
    cdTableFooterGroup,
    cdTableRow,
    cdTableColumnGroup,
    cdTableColumn,
    cdTableCell,
    cdTableCaption
  ); // CSS display

 // deprecated, use Styles where possible!
  THtmAlign = (
    haNone,   // default
    haLeft,
    haRight,
    haCenter
  ); // html align=

  THtmVertAlign = (
    hvaNone,
    hvaTop,
    hvaBottom,
    hvaMiddle
  ); // html valign=

  THtmBorder = (
    bNone,
    bSingle
  ); // html borders (rarely want more than single borders)
  
  THtmFormMethod = (
    hfmGET,
    hfmPOST
  ); // html for POST or GET method

  THtmFormEncoding = (
    hfeUrlEncoded, // default
    hfeMultiPartFormData,
    hfeTextPlain
  );
{------------------------------------------------------------------------------}

  //  ** All of these definitions are public but only StyleSheet should
  //  ** be used by developer.
  PStyleProperty = ^StyleProperty;
  StyleProperty = record
    Prop_Name : string[40];
    Prop_Val : string[100];
    NextProperty   : PStyleproperty;
  end;

  PStyleElement = ^StyleElement;

  StyleElement = object
    ElementName: string[50];
    Properties: PStyleProperty;
    NextElement: PStyleElement;
    constructor Create(Name : ShortString);
    destructor Destroy;
    procedure Write_Out;
    procedure InsertProperty(PropName, PropValue : ShortString);
  end;
        
  //  public use.
  PStyleSheet = ^CS_Sheet;
  CS_Sheet = object
    FirstTag: PStyleElement;
    IsExternal: Boolean;
    URL: AnsiString;
    constructor Create;
    constructor CSExternal(href: string);
    destructor Destroy;
    procedure Write_Out;
    procedure InsertProperty(ElementName, PropName, PropValue : ShortString);
  end;


{-- INPUT AND FORM ELEMENTS ---------------------------------------------------}
const
  HTM_FORM_METHOD: array [hfmGet..hfmPost] of string = ('GET', 'POST');
  HTM_FORM_ENCODING: array [hfeUrlEncoded.. hfeTextPlain] of string = ('application/x-www-form-urlencoded', 'multipart/form-data', 'text/plain');

type
  { FORM }
  THtmForm = record
    Action: string;
    Method: THtmFormMethod;
    EncodeType: THtmFormEncoding;
  end;

  { SUBMIT BUTTON INPUT (button) }
  THtmSubmitButton = record
    name: string;
    caption: string;
  end;

  { SUBMIT BUTTON INPUT (button) }
  THtmSubmitButtonCustom = record
    name: string;
    textcolor: string;
    bgcolor: string;
    border: TCssBorder;
    bordercolor: string;
    caption: string;
  end;
  
  { SUBMIT BUTTON INPUT (button) absolute positioning }
  THtmSubmitButtonCustom2 = record
    name: string;
    left, top,
    zindex: integer;
    textcolor: string;
    bgcolor: string;
    border: TCssBorder;
    bordercolor: string;
    caption: string;
  end;

  { BUTTON INPUT (button) }
  THtmButton = record
    name: string;
    caption: string;
  end;

  { BUTTON INPUT (button) }
  THtmButtonCustom = record
    name: string;
    textcolor: string;
    bgcolor: string;
    border: TCssBorder;
    bordercolor: string;
    caption: string;
  end;

  { BUTTON INPUT (button) absolute positioning}
  THtmButtonCustom2 = record
    name: string;
    left, top,
    zindex: integer;
    textcolor: string;
    bgcolor: string;
    border: TCssBorder;
    bordercolor: string;
    caption: string;
  end;

  { TEXT INPUT (edit) }
  THtmEdit = record
    name: string;
    readonly: boolean;
    maxlength: integer; // maximum characters
    size: integer; // character positions shown
    text: string;
  end;
  
  { TEXT INPUT (edit) }
  THtmEditCustom = record
    name: string;
    textcolor: string;
    bgcolor: string;
    border: TCssBorder;
    bordercolor: string;
    readonly: boolean;
    maxlength: integer;
    size: integer;
    text: string;
  end;

  { TEXT INPUT (edit) absolute positioning }
  THtmEditCustom2 = record
    name: string;
    left, top,
    zindex: integer;
    textcolor: string;
    bgcolor: string;
    border: TCssBorder;
    bordercolor: string;
    readonly: boolean;
    maxlength: integer;
    size: integer;
    text: string;
  end;

  { TEXTAREA (memo) }
  THtmMemoCustom = record
    name: string;
    rows, cols: integer;
    textcolor: string;
    bgcolor: string;
    border: TCssBorder;
    bordercolor: string;
    text: string;
    readonly: boolean;
  end;
  
  { TEXTAREA (memo) }
  THtmMemoCustom2 = record
    name: string;
    left, top, rows, cols,
    zindex: integer;
    textcolor: string;
    bgcolor: string;
    border: TCssBorder;
    bordercolor: string;
    text: string;
    readonly: boolean;
  end;

  { TEXTAREA (standard memo) }
  THtmMemo = record
    name: string;
    rows, cols: integer;
    text: string;
    readonly: boolean;
  end;
  { Note: textarea does not support the MAXLENGTH property }

  { PASSWORD INPUT (edit) }
  THtmPasswdEdit = record
    name: string;
    left, top: integer;
    zindex: integer;
    textcolor: string;
    bgcolor: string;
    border: TCssBorder;
    bordercolor: string;
    readonly: boolean;
    maxlength: integer;
    size: integer;
    text: string;
  end;

// END OF INPUT AND FORM ELEMENTS
{------------------------------------------------------------------------------}
type
  TCssUnits = (
    cuEms, // default
    cuPercent,
    cuPixels,
    cuPoints
  );

  TFontRec = record
    family: string;
    size: string;
    color: string;
  end;

const
  CSS_UNITS: array [cuEms..cuPoints] of string = ('em', '%', 'px', 'pt');

type
  { <DIV> box with no positioning }
  THtmBox = record
    HAlign: TCssHAlign;
    VAlign: TCssVAlign;
    Pad: integer;
    PadUnit: TCssUnits;
    Font: TFontRec;
    FontUnit: TCssUnits;
    BgColor:string;
    Text: string;
    Margin: integer;
    MarginUnit: TCssUnits;
  end;

  { <DIV> box with pixel positioning and measurement }
  THtmPixelBox = record
    HAlign: TCssHAlign;
    VAlign: TCssVAlign;
    left, top, height, width,
    pad: integer;
    font: TFontRec;
    FontUnit: TCssUnits;
    BgColor:string;
    zindex: integer;
    Margin: integer;
    text: string;
  end;
  
  { <DIV> box with percentage positioning and measurement }
  THtmPercentBox = record
    HAlign: TCssHAlign;
    VAlign: TCssVAlign;
    left, top, height, width,
    pad: integer;
    Font: TFontRec;
    BgColor:string;
    zindex: integer;
    Margin: integer;
    Text: string;
  end;

  { <DIV> custom box structure (panel) }
  THtmCustombox = record
    HAlign: TCssHAlign;
    VAlign: TCssVAlign;
    display: TCssDisplay;
    position: TCssPosition;
    left,
    top,
    height,
    width,
    pad: integer;
    HeightUnit,
    WidthUnit: TCssUnits;
    Font: TFontRec;
    bgcolor: string;
    PageAlign: TCssHAlign;
    Border: TCssBorder;
    zindex: integer;
    margin: integer;
    text: string;
  end;

 { <BODY> standard body structure }
  THtmBody = record
    bgcolor: string;
    Font: TFontRec;
    margin: string;
  end;

 { <BODY> custom body structure }
  THtmBodyCustom = record
    bgcolor: string;
    font: TFontRec;
    margin: record
      left: string;
      right: string;
      top: string;
      bottom: string;
    end;
  end;
  
 { <BODY> body structure (obsolete) }
  THtmBodyOld = record
    bgcolor,
    text,
    link,
    alink,
    vlink: string;
  end;

  THtmLinkStyle = record
    link: record
      color: string;
      underline: boolean;
      bold: boolean;
      size: string;
    end;
    hoverlink: record
      color: string;
      underline: boolean;
      bold: boolean;
      size: string;
    end;
    activelink: record
      color: string;
      underline: boolean;
      bold: boolean;
      size: string;
    end;
    visitedlink: record
      color: string;
      underline: boolean;
      bold: boolean;
      size: string;
    end;
  end;


{------------------------------------------------------------------------------}
{  PUBLIC PROCEDURES/FUNCTION DECLARATIONS                                     }
{------------------------------------------------------------------------------}

procedure HtmBegin;
procedure HtmBegin(title: string); overload;
procedure HtmBegin(title: string; stylesheet: string); overload;
procedure HtmBegin(title: string; stylesheet: PStyleSheet); overload;
procedure HtmBegin(title: string; body: THtmBodyOld); overload;
procedure HtmBegin(title: string; body: THtmBody); overload;
procedure HtmBegin(title: string; body: THtmBody; stylesheet: string); overload;
procedure HtmBegin(title: string; body: THtmBodyOld; stylesheet: string); overload;
procedure HtmEnd;

procedure BoxOut(input: THtmBox); overload;
procedure BoxOut(input: THtmPixelBox); overload;
procedure BoxOut(input: THtmPercentBox); overload;
procedure BoxOut(input: THtmCustomBox); overload;

procedure BoxBegin(input: THtmBox); overload;
procedure BoxBegin(input: THtmPixelBox); overload;
procedure BoxBegin(input: THtmPercentBox); overload;
procedure BoxBegin(input: THtmCustomBox); overload;

procedure BoxEnd;


procedure FormBegin(input: THtmForm); overload;
procedure FormEnd; overload;

procedure ButtonOut(input: THtmButton); overload;
procedure ButtonOut(input: THtmButtonCustom); overload;
procedure ButtonOut(input: THtmButtonCustom2); overload;
procedure ButtonOut(input: THtmSubmitButton); overload;
procedure ButtonOut(input: THtmSubmitButtonCustom); overload;
procedure ButtonOut(input: THtmSubmitButtonCustom2); overload;

procedure EditOut(input: THtmEditCustom); overload;
procedure EditOut(input: THtmEditCustom2); overload;
procedure EditOut(input: THtmEdit); overload;

procedure MemoOut(input: THtmMemo); overload;
procedure MemoOut(input: THtmMemoCustom); overload;
procedure MemoOut(input: THtmMemoCustom2); overload;

function Bold(input: string): string;
function Italic(input: string): string;
function Strong(input: string): string;
function BlockQt(input: string): string;
function Highlight(input: string; HTMLColor: string): string;
function Preformat(input: string): string;

procedure ItalicOut(input: string);
procedure BoldOut(input: string);
procedure StrongOut(input: string);
procedure BlockQtOut(input: string);
procedure HighlightOut(input: string; HTMLColor: string);
procedure PreformatOut(input: string);

procedure RuleOut;
procedure RuleOut(Extra : String);

procedure NOBR;
procedure NOBR_Start;
procedure RBON;
procedure NOBR_End;
procedure BR;
procedure HLineBreak;
procedure HParaBreak;
procedure Pre;
procedure Pre_Start;
procedure erp;
procedure Pre_End;
procedure nbsp;

procedure Img(Src,Width,Height,Extra : String);
procedure ImgOut(Src, Width, Height, Extra : String);
procedure ImgOut(Src,Width,Height : String);
procedure ImgOut(Src : String);


procedure TableBegin(Extra : string);
procedure TableBegin;
    procedure RowBegin(Extra : String);
    procedure RowBegin;

       procedure CellHOut(Contents, Extra : String);
       procedure CellHOut(Contents : String);
       procedure CellOut(Contents, Extra : String);
       procedure CellOut(Contents : String);

      procedure CellHBegin(Extra : String);
      procedure CellHBegin;
      procedure CellHEnd;

      procedure CellBegin(Extra : String);
      procedure CellBegin;
      procedure CellEnd;

   procedure RowEnd;
procedure TableEnd;


procedure Span_Begin(Extra : String);
procedure Span_Begin;
procedure Span_End;

procedure Div_Begin(Extra : String);
procedure Div_Begin;
procedure Div_End;


procedure CSS_ExternalOut(URL, Name : AnsiString);
procedure CSS_ExternalOut(URL : AnsiString);

procedure FormOut(FormName, Action : String;  Method : Integer); overload;
procedure Form_inputOut(IType, Name, Value, Extra : String);

procedure Form_RequestFileOut(Name, Extra : String);
procedure Form_RequestFileOut(Name : String);

procedure Form_ButtonOut(Name,Value,Extra : String);
procedure Form_ButtonOut(Name, Value : String);

procedure Form_radioOut(Name, Value, Extra : String);
procedure Form_radioOut(Name, Value : String);

procedure Form_SubmitOut(Name, Value, Extra : String);
procedure Form_SubmitOut(Name,Value : String);

procedure Form_TextOut(Name,Value,Extra : String);
procedure Form_TextOut(Name, Value : String);

procedure Form_TextAreaOut(Name, Value,Extra : String);
procedure Form_TextAreaOut(Name,Value : String);

procedure Form_CheckBoxOut(Name : String; IsChecked : boolean; Extra : String);
procedure Form_CheckBoxOut(Name : String; IsChecked : boolean);

procedure Form_HiddenOut(Name,Value : String);

procedure RollUpCGIValues;

procedure Form_SelectOut(Name,Extra : String);
procedure Form_SelectOut(Name : String);

// Both the same
procedure EndSelectOut;
procedure Select_End;

// Both the same 
procedure EndFormOut;


// END OF PUBLIC procedure/FUNCTION DECLARATIONS
{------------------------------------------------------------------------------}

implementation


{------------------------------------------------------------------------------}
{  PRIVATE PROCEDURES/FUNCTIONS                                                }
{------------------------------------------------------------------------------}

{ Output body structure (obsolete) }
procedure OldBodyOut(input: THtmBodyOld);
begin
  webwrite(
    '<body' +
       'bgcolor="'+ input.bgcolor+'";' +  // background color
       'text="'+ input.text+'";' +        // text color
       'link="'+ input.link+'";' +        // link color
       'alink="'+ input.alink+'";' +      // active link color
       'vlink="'+ input.vlink+'";' +      // visited link color
    '>');
end;


{ Output body structure via CSS}
procedure BodyOut(input: THtmBody);
begin
  webwrite(
    '<body style="'+
       'background:'+input.bgcolor+';' +  // background color
       'color:'+input.font.color+';' +    // font color
       'font-size:'+input.font.size+';' +
       'font-family:'+input.font.family+';' +
       'margin:'+input.margin+';' +
    '">');
end;

procedure BodyOut(input: THtmBodyCustom);
begin

end;


// END OF PRIVATE PROCEDURES
{------------------------------------------------------------------------------}


{------------------------------------------------------------------------------}
{ PUBLIC PROCEDURES/FUNCTIONS                                                  }
{------------------------------------------------------------------------------}

{ start form }
procedure FormBegin(input: THtmForm); overload;
begin
  webwrite(
  '<FORM ' +
     'ACTION="'+ input.action + '" ' +
     'METHOD="'+ HTM_FORM_METHOD[input.method] + '" ' +
     'ENCTYPE="'+ HTM_FORM_ENCODING[input.EncodeType] + '" ');
  webwrite('>');
end;


{-- FORM INPUT WIDGETS --------------------------------------------------------}

{ Output button }
procedure ButtonOut(input: THtmButtonCustom); overload;
begin
  webwrite(
  '<INPUT STYLE="' +
     'background-color:'+ input.bgcolor + ';' +
     '" ' +
     'NAME="'+ input.name + '" ' +
     'TYPE="button" ' +
     'VALUE="'+input.caption+'" ' );
  webwrite('>');
end;

{ Output button }
procedure ButtonOut(input: THtmButtonCustom2); overload;
begin
  webwrite(
  '<INPUT STYLE="position: absolute;' +
     'left:'+ inttostr(input.Left)+'%;' +
     'top:'+ inttostr(input.top) +'%;' +
     'background-color:'+ input.bgcolor + ';' +
     'z-index:'+ inttostr(input.zindex) + ';' +
     '" ' +
     'NAME="'+ input.name + '" ' +
     'TYPE="button" ' +
     'VALUE="'+input.caption+'" ' );
  webwrite('>');
end;


{ Output standard button }
procedure ButtonOut(input: THtmButton); overload;
begin
  webwrite(
  '<INPUT ' +
     'NAME="'+ input.name + '" ' +
     'TYPE="button" ' +
     'VALUE="'+input.caption+'" ' );
  webwrite(
   '>');
end;

{ Output standard submit button }
procedure ButtonOut(input: THtmSubmitButton); overload;
begin
  webwrite(
  '<INPUT ' +
     'NAME="'+ input.name + '" ' +
     'TYPE="submit" ' +
     'VALUE="'+input.caption+'" ' );
  webwrite(
   '>');
end;

{ Output custom submit button }
procedure ButtonOut(input: THtmSubmitButtonCustom); overload;
begin
  webwrite(
  '<INPUT STYLE="' +
     'background-color:'+ input.bgcolor + ';' +
     'color:'+ input.textcolor + ';' +
     '" ' +
     'NAME="'+ input.name + '" ' +
     'TYPE="submit" ' +
     'VALUE="'+input.caption+'" ' );
  webwrite(
   '>');
end;

{ Output custom submit button }
procedure ButtonOut(input: THtmSubmitButtonCustom2); overload;
begin
  webwrite(
  '<INPUT STYLE="position: absolute;' +
     'left:'+ inttostr(input.Left)+'%;' +
     'top:'+ inttostr(input.top) +'%;' +
     'background-color:'+ input.bgcolor + ';' +
     'color:'+ input.textcolor + ';' +
     'z-index:'+ inttostr(input.zindex) + ';' +
     '" ' +
     'NAME="'+ input.name + '" ' +
     'TYPE="submit" ' +
     'VALUE="'+input.caption+'" ' );
  webwrite(
   '>');
end;


{ Output edit box }
procedure EditOut(input: THtmEditCustom); overload;
begin
  webwrite(
  '<INPUT STYLE="' +
     'background-color:'+ input.bgcolor + ';' +
     '" ' +
     'NAME="'+ input.name + '" ' +
     'TYPE="text" ' +
     'SIZE="'+inttostr(input.size)+'" ' +
     'VALUE="'+input.text+'" ' +
     'MAXLENGTH="'+inttostr(input.maxlength)+'" ');
  if input.readonly then webwrite('READONLY ');
  webwrite(
   '>');
end;

{ Output edit box }
procedure EditOut(input: THtmEditCustom2); overload;
begin
  webwrite(
  '<INPUT STYLE="position: absolute;' +
     'left:'+ inttostr(input.Left)+'%;' +
     'top:'+ inttostr(input.top) +'%;' +
     'background-color:'+ input.bgcolor + ';' +
     'z-index:'+ inttostr(input.zindex) + ';' +
     '" ' +
     'NAME="'+ input.name + '" ' +
     'TYPE="text" ' +
     'SIZE="'+inttostr(input.size)+'" ' +
     'VALUE="'+input.text+'" ' +
     'MAXLENGTH="'+inttostr(input.maxlength)+'" ');
  if input.readonly then webwrite('READONLY ');
  webwrite(
   '>');
end;


{ Output edit box }
procedure EditOut(input: THtmEdit); overload;
begin
  webwrite(
  '<INPUT ' +
     'NAME="'+ input.name + '" ' +
     'TYPE="text" ' +
     'SIZE="'+inttostr(input.size)+'" ' +
     'VALUE="'+input.text+'" ' +
     'MAXLENGTH="'+inttostr(input.maxlength)+'" ');
  if input.readonly then webwrite('READONLY ');
  webwrite(
   '>');
end;

{ Output a box/panel via varying percentage widths }
procedure MemoOut(input: THtmMemo); overload;
begin
  webwrite(
  '<TEXTAREA ' +
     'NAME="'+ input.name + '" ' +
     'ROWS="'+ inttostr(Input.rows) + '" ' +
     'COLS="'+ inttostr(Input.cols) + '" ');
  if input.readonly then webwrite('READONLY ');
  webwrite(
   '>' + input.text +
  '</TEXTAREA>');
end;

{ Output a box/panel }
procedure MemoOut(input: THtmMemoCustom); overload;
begin
  webwrite(
  '<TEXTAREA STYLE="' +
     'background-color:'+ input.bgcolor + ';' +
     'color:'+ input.textcolor + ';' +
     '" ' +
     'NAME="'+ input.name + '" ' +
     'ROWS="'+ inttostr(Input.rows) + '" ' +
     'COLS="'+ inttostr(Input.cols) + '" ' );
  if input.readonly then webwrite('READONLY ');
  webwrite(
   '>' + input.text +
  '</TEXTAREA>');
end;

{ Output a box/panel via varying percentage widths }
procedure MemoOut(input: THtmMemoCustom2); overload;
begin
  webwrite(
  '<TEXTAREA STYLE="position:absolute;' +
     'left:'+ inttostr(input.Left)+'%;' +
     'top:'+ inttostr(input.top) +'%;' +
     'background-color:'+ input.bgcolor + ';' +
     'z-index:'+ inttostr(input.zindex) + ';' +
     '" ' +
     'NAME="'+ input.name + '" ' +
     'ROWS="'+ inttostr(Input.rows) + '" ' +
     'COLS="'+ inttostr(Input.cols) + '" ' );
  if input.readonly then webwrite('READONLY ');
  webwrite(
   '>' + input.text +
  '</TEXTAREA>');
end;


// END OF FORM INPUT WIDGETS
{------------------------------------------------------------------------------}

constructor StyleElement.Create(Name : ShortString);
 begin
   With Self DO
     begin
       ElementName := Name;
       Properties := NIL;
       NextElement := NIL;
     end;
 end;
       
destructor StyleElement.Destroy;
var
  CurProperty: PStyleProperty;
  NextProp: PStyleProperty;
begin
  If Self.Properties <> NIL THEN
     BEGIN
      CurProperty := Self.Properties;
      repeat
        NextProp := CurProperty^.NextProperty;
        Dispose(CurProperty);
        CurProperty := NextProp;
      until CurProperty = NIL;
     END;
  If Self.NextElement <> NIL THEN Dispose(Self.NextElement, Destroy);  // Should traverse the entire linked list. 
                                                                    // Freeing all memory.
end;

procedure StyleElement.Write_Out;
var
 CurProperty : PStyleProperty;
begin
  If Self.Properties = NIL then Exit;
  WebWrite(#13#10);
  WebWrite(Self.ElementName+ ' {');
  CurProperty := Self.Properties;
  while CurProperty <> NIL do
  begin
      WebWrite(CurProperty^.Prop_Name + ': ' + CurProperty^.Prop_Val + ';');
      CurProperty := CurProperty^.NextProperty;
  end;
  Write('}');
end;

procedure StyleElement.InsertProperty(PropName, PropValue : ShortString);
var
 CurProperty,Marker : PStyleProperty;
    procedure SetValue(Prop : PStyleProperty);
      begin
       If Prop = NIL THEN
         { DO Nothing }
       else
        begin
         Prop^.Prop_Name := PropName;
         Prop^.Prop_Val := PropValue;
         Prop^.NextProperty := NIL;
       end;
      end;
begin
 With Self DO
  begin
   If Properties = NIL THEN
     begin
      Properties := New(PStyleProperty);
      SetValue(Properties);
     end
   else
    begin
     CurProperty := Properties;
     While (CurProperty <> NIL) and (CurProperty^.Prop_Name <> PropName) do 
         begin
           Marker := CurProperty;
           CurProperty := CurProperty^.NextProperty;
         end; 
     If CurProperty = NIL then 
        begin
         CurProperty := New(PStyleProperty);
         Marker^.NextProperty := CurProperty;
        end;
     SetValue(CurProperty);
    end;
  end;
end;
      
constructor CS_Sheet.Create;
begin
  with self do
  begin
    firstTag:= NIL;
    IsExternal:= FALSE;
    System.Initialize(URL);
   end;
end;

constructor CS_Sheet.CSExternal(href : AnsiString);
begin
  Self.Create;
  Self.URL := href;
  Self.IsExternal := TRUE;
end;

destructor CS_Sheet.Destroy;
begin
  If Self.FirstTag <> NIL then
     Dispose(Self.FirstTag,Destroy);
  Finalize(Self.URL);
end; 

procedure CS_Sheet.InsertProperty(ElementName, PropName,PropValue : ShortString);
var
 CurElement : PStyleElement;
 Marker     : PStyleElement;
begin
  With Self DO
   BEGIN
     If FirstTag = NIL THEN
        begin
          FirstTag := New(PStyleElement, Create(ElementName));
          If FirstTag <> NIL then FirstTag^.InsertProperty(PropName, PropValue);
        end
     else
        begin
          CurElement := FirstTag;
          while (CurElement <> NIL) and (CurElement^.ElementName <> ElementName) DO
            begin
              Marker := CurElement;
              CurElement := CurElement^.NextElement;
            end;
          If CurElement = NIL then
             begin
              CurElement := New(PStyleElement, Create(ElementName));
              Marker^.NextElement := CurElement;
             end;
          CurElement^.InsertProperty(PropName, PropValue);
        end;
   END;
end;

procedure CSS_ExternalOut(URL, Name : AnsiString);
begin
  WebWrite('<LINK rel="stylesheet" type="text/css" ');
  If Name <> '' then WebWrite('name="' + Name + '" ');
  WebWrite('href="' + URL + '">');
end;

procedure CSS_ExternalOut(URL : AnsiString);
begin
  CSS_ExternalOut(URL, '');
end;

procedure CS_Sheet.Write_Out;
var
 CurElement : PStyleElement;
 begin
   If Self.IsExternal THEN
      CSS_ExternalOut(Self.URL,'');
   If Self.FirstTag = NIL then Exit;
   WebWrite('<STYLE TYPE="text/css">' +#13 + #10);
   CurElement := Self.FirstTag;
   While CurElement <> NIL do
    begin
      CurElement^.Write_Out;
      CurElement := CurElement^.NextElement;
    end;
    WebWrite(#13 + #10);
    WebWrite('</STYLE>');
 end;

procedure Img(Src,Width,Height,Extra : string);
var 
 tmp : string;
begin
   tmp := '<IMG SRC="' + Src +'"';
   If Width <> '' THEN
      tmp := tmp + ' WIDTH="' + Width + '"';
   If Height <> '' THEN 
      tmp := tmp + ' HEIGHT="' + Height + '"';
   If Extra <> '' THEN
      tmp := tmp + #32 + Extra;
   tmp := tmp + '>';
   WebWrite(tmp);
end;

procedure ImgOut(Src, Width, Height, Extra: string); overload;
begin
  Img(Src,Width,Height,Extra);
end;

procedure ImgOut(Src,Width,Height : string); overload;
begin
  Img(Src, Width, Height,'');
end;

procedure ImgOut(Src : string); overload;
begin
  Img(Src,'','','');
end;


procedure TableBegin(Extra : string); overload;
begin
  If Extra <> '' then Extra := #32 + Extra;
  WebWrite('<TABLE' + Extra + '>');
end;

procedure TableBegin; overload;
begin
  WebWrite('<TABLE>');
end;

procedure RowBegin(Extra : string);
begin
  If Extra <> '' then Extra := #32 + Extra;
  WebWrite('<TR' + Extra + '>');
end;

procedure RowBegin;
begin
 WebWrite('<TR>');
end;

procedure RowEnd;
begin
  WebWrite('</TR>');
end;

procedure CellBegin(Extra : string);
begin
  If Extra <> '' then Extra := #32 + Extra;
  WebWrite('<TD' + Extra + '>');
end;

procedure CellBegin;
begin
  WebWrite('<TD>');
end;

procedure CellEnd;
begin
 WebWrite('</TD>');
end;

procedure CellHBegin(Extra : string);
begin
 If Extra <> '' then Extra := #32 + Extra;
 WebWrite('<TH'+ Extra + '>');
end;

procedure CellHBegin;
begin
  WebWrite('<TH>');
end;

procedure CellHEnd;
begin
 WebWrite('</TH>');
end;

procedure CellOut(Contents, Extra: string);
begin
 If Extra <> '' then Extra := #32 + Extra;
 WebWrite('<TD' + Extra + '>' + Contents + '</TD>');
end;

procedure CellOut(Contents: string);
begin
 CellOut(Contents, '');
end;

procedure CellHOut(Contents, Extra: string);
begin
 If Extra <> '' then Extra := #32 + Extra;
 WebWrite('<TH' + Extra + '>' + Contents + '</TH>');
end;

procedure CellHOut(Contents: string);
begin
  CellHOut(Contents,'');
end;

procedure TableEnd;
begin
 WebWrite('</TABLE>');
end;

procedure Span_Begin(Extra: string);
begin
   If Extra <> '' THEN Extra := #32 + Extra;
   WebWrite('<SPAN' + Extra + '>');
end;

procedure Span_Begin;
begin
  WebWrite('<SPAN>');
end;

procedure Span_End;
begin
   WebWrite('</SPAN>');
end;

procedure DIV_Begin(Extra: string);
begin
   If Extra <> '' THEN Extra := #32 + Extra;
   WebWrite('<DIV' + Extra + '>');
end;

procedure DIV_Begin;
begin
  WebWrite('<DIV>');
end;

procedure DIV_End;
begin
   WebWrite('</DIV>');
end;

procedure FormOut(FormName, Action: string;  Method: Integer);
var
 THtml,tmp, encoding : AnsiString;
begin
   System.Initialize(THtml);
   System.Initialize(tmp);
   System.Initialize(encoding);
   thtml := ('<FORM ACTION="' + Action +'" ');
   If FormName <> '' THEN thtml := thtml + 'NAME="'+FormName+'" ';
   CASE Method OF
          POST : begin
                  tmp := 'POST';
                  encoding := '"application/x-url-encoded"';
                 end;

     MULTIPART : begin
                  tmp := 'POST';
                  encoding := '"multipart/form-data"';
                 end;

           GET : begin
                  tmp := 'GET';
                  encoding := '"application/x-url-encoded"';
                 end;
   end; { case }

   thtml := thtml + 'METHOD="' + tmp + '"';
   If encoding <> '' THEN 
      thtml := thtml + ' enctype=' + encoding;

   WebWrite(thtml + '>');

   Finalize(THtml);
   Finalize(tmp);
   Finalize(encoding);
end;

procedure Form_InputOut(IType: string;  Name, Value, Extra: string);
var
 THtml : AnsiString;
begin
   System.Initialize(THtml);
   THtml := '<input type="' +  Itype  + '"'
          + ' name="' +   Name + '"';
   If Value <> '' then 
      THtml := THtml + ' value="' + Value + '"';
  If Extra = '' then
    { null }
  else
     THtml := THtml + ' ' + Extra;
  WebWrite(THtml + '>');
  Finalize(THtml);
end;


//**  YOU NEVER SUPPLY A VALUE FOR FILE (IE 4.0 USED TO ALLOW THIS A BIG
//**  BAD SECURITY VIOLATION (You could steal registrys *.pwl etc) 
procedure Form_RequestFileOut(Name, Extra: string);
begin
  Form_inputOut('FILE', Name, '', Extra);
end;

procedure Form_RequestFileOut(Name: string);
begin
  Form_RequestFileOut(Name,'');
end;

procedure Form_ButtonOut(Name,Value, Extra: string);
begin
  Form_inputOut('BUTTON',Name, Value, Extra);
end;

procedure Form_ButtonOut(Name,Value: string);
begin
  Form_ButtonOut(Name, Value, '');
end;

procedure Form_radioOut(Name, Value, Extra: string);
begin
  Form_inputOut('RADIO', Name, Value, Extra);
end;

procedure Form_radioOut(Name, Value: string);
begin
  Form_radioOut(Name, Value,'');
end;

procedure Form_SubmitOut(Name, Value, Extra: string);
begin
  Form_inputOut('SUBMIT', Name, Value, Extra);
end;

procedure Form_SubmitOut(Name, Value: string);
begin
  Form_SubmitOut(Name, Value, '');
end;

procedure Form_TextOut(Name,Value,Extra: string);
begin
  Form_inputOut('TEXT',Name, Value,Extra);
end;

procedure Form_TextOut(Name,Value: string);
begin
  Form_TextOut(Name, Value, '');
end;

procedure Form_TextAreaOut(Name, Value, Extra: string);
begin
  Form_InputOut('TEXTAREA', Name, Value, Extra);
end;
 
procedure Form_TextAreaOut(Name, Value: string);
begin
  Form_TextAreaOut(Name, Value, '');
end;

procedure Form_CheckBoxOut(Name: string; IsChecked : boolean; Extra: string);
begin
 If IsChecked THEN
    Extra := Extra + ' CHECKED';
 Form_InputOut('CHECKBOX', Name, '', Extra);
end;

procedure Form_CheckBoxOut(Name: string; IsChecked : boolean);
begin
 Form_CheckBoxOut(Name, IsChecked, '');
end;

procedure Form_HiddenOut(Name, Value: string);
begin
  Form_InputOut('HIDDEN', Name, Value, '');
end;

{ Write All Current CGI Values as Hidden Values. Useful for Error condition
  retries without getting all the values from the user again.
 A <FORM> tag should already have been written }
procedure RollUpCGIValues;
var 
 I,Cnt : LongInt;
begin
 Cnt := CountCGIVars();
 If Cnt >= 1 then
   For I := 1 to Cnt  DO form_HiddenOut(FetchCGIVarName(I),FetchCGIVarValue(I));
end;


procedure Form_SelectOut(Name, Extra: string);
begin
  If Extra <> '' then Extra := #32 + Extra;
    Webwrite('<SELECT NAME="' + Name + '"' +  Extra + '>');
end;

procedure Form_SelectOut(Name: string);
begin
  Form_SelectOut(Name,'');
end;

procedure EndSelectOut;
begin
  WebWrite('</SELECT>');
end;

procedure EndSelect;
begin
  EndSelectOut;
end;
 
procedure Select_End;
begin
  EndSelectOut;
end;

procedure EndFormOut;
begin
  WebWrite('</FORM>');
end;

procedure FormEnd;
begin
  EndFormOut;
end;
 
procedure RuleOut;
begin
  webwrite('<HR>');
end;

procedure RuleOut(Extra: string);
begin
  if Extra <> '' then Extra := #32 + Extra;
    WebWrite('<HR' + Extra + '>');
end;

procedure nbsp;
begin
  WebWrite('&nbsp;');
end;

procedure br;
begin
  WebWrite('<BR>');
end;

// html line break
procedure HLineBreak;
begin
  WebWrite('<BR />');
end;
// html paragraph break
procedure HParaBreak;
begin
  WebWrite('<P />');
end;


procedure NOBR;
begin
  WebWrite('<NOBR>');
end;

procedure NOBR_Start;
begin
  NOBR;
end;

procedure RBON;
begin
  WebWrite('</NOBR>');
end;

procedure NOBR_End;
begin
  RBON;
end;

procedure Pre;
begin
  WebWrite('<PRE>');
end;

procedure Pre_Start;
begin
  Pre;
end;

procedure erp;
begin
  WebWrite('</pre>');
end;

procedure Pre_End;
begin
  erp;
end;

procedure BoxEnd;
begin
  webwrite('</div>');
end;

{ Output a box/panel with no specific positioning }
procedure BoxBegin(input: THtmBox); overload;
begin
  webwrite(
  '<div style="' +
     'text-align:'+CSS_HALIGN[input.halign]+';' +    // horizontal content align
     'vertical-align:'+CSS_VALIGN[input.valign]+';' +    // vertical content align
     'color:'+input.font.color+';' +    // font color
     'font-size:'+input.font.size+';' +
     'font-family:'+input.font.family+';' +
     'padding:'+ inttostr(input.pad) + CSS_UNITS[input.padunit]+';' +
     'background-color:'+ input.bgcolor + ';' +
     'margin:'+ inttostr(input.margin) + CSS_UNITS[input.marginunit]+';' +
     '">' +
     input.text);
end;

{ Output a box/panel with no positioning }
procedure BoxOut(input: THtmBox); overload;
begin
  BoxBegin(input);
  BoxEnd;
end;

{ Output a box/panel via fixed pixel widths }
procedure BoxBegin(input: THtmPixelBox); overload;
begin
  webwrite(
  '<div style="position:absolute;' +
     'color:'+input.font.color+';' +    // font color
     'font-size:'+input.font.size+';' +
     'font-family:'+input.font.family+';' +
     'left:'+ inttostr(input.Left) + 'px;' +
     'top:'+ inttostr(input.top) +'px;' +
     'height:'+ inttostr(input.height) + 'px;' +
     'width:'+ inttostr(input.width) + 'px;' +
     'padding:'+ inttostr(input.pad) + 'px;' +
     'background-color:'+ input.bgcolor + ';' +
     'z-index:'+ inttostr(input.zindex) + ';' +
     '">' +
    input.text);
end;

procedure BoxOut(input: THtmPixelBox); overload;
begin
  BoxBegin(input);
  BoxEnd;
end;

{ Output a box/panel via varying percentage widths }
procedure BoxBegin(input: THtmPercentBox); overload;
begin
  webwrite(
  '<div style="position: absolute;' +
    'color:'+input.font.color+';' +    // font color
    'font-size:'+input.font.size+';' +
    'font-family:'+input.font.family+';' +
    'left:'+ inttostr(input.Left)+'%;' +
    'top:'+ inttostr(input.top) +'%;' +
    'height:'+ inttostr(input.height) + '%;' +
    'width:'+ inttostr(input.width) + '%;' +
    'padding:'+ inttostr(input.pad) + 'px;' +
    'background-color:'+ input.bgcolor + ';' +
    'z-index:'+ inttostr(input.zindex) + ';' +
    '">' +
    input.text);
end;

procedure BoxOut(input: THtmPercentBox); overload;
begin
  BoxBegin(input);
  BoxEnd;
end;


{ Output a box/panel via custom widths, ability to mix percentage and pixels
  since the user must specify a string as the input and specify his own width
  measurement, px or % }
procedure BoxBegin(input: THtmCustomBox);
var
  MarginCalc: integer;
  CSSPos: array [cpAbsolute..cpRelative] of string[8] = ('absolute', 'relative');
begin
  // can't get MarginCalc if number is odd, must be divisible by two
  if ( odd(input.width) )
    and (input.PageAlign = chaCenter)
    and (input.widthunit = cuPercent) then
  begin
    ThrowWebError('<script>alert("Warning: CustomBox.width should be EVEN number if centered")</script>');
  end;

  webwrite('<div style="position:' + CSSPos[input.position] + ';' +
              'color:'+input.font.color+';' +    // font color
              'font-size:'+input.font.size+';' +
              'font-family:'+input.font.family+';' +
              'left:' + inttostr(input.Left) + ';' +
              'top:' + inttostr(input.top) +';');
             
  if input.heightunit = cuPixels then         // measured in pixels
    webwrite( 'height:' + inttostr(input.height) + 'px;')
  else if input.heightunit = cuPercent then  // measured in percent
    webwrite( 'height:' + inttostr(input.height) + '%;');

  if input.widthunit = cuPixels then          // measured in pixels
    webwrite( 'width:' + inttostr(input.width) + 'px;')
  else if input.widthunit = cuPercent then   // measured in percent
    webwrite( 'width:' + inttostr(input.width) + '%;');

  if input.PageAlign = chaCenter then
  begin
    if input.widthunit = cuPixels then
      webwrite('margin-left:auto; margin-right: auto;')
    else if input.widthunit = cuPercent then
    begin
      MarginCalc:= round( (100{%} - input.width{%}) / 2); // get leftover percentage
      webwrite('margin-left:' + inttostr(MarginCalc) + '%;margin-right:' + inttostr(MarginCalc) +'%;')
    end;
  end;
  
  if input.PageAlign = chaLeft then
  begin
    if input.widthunit = cuPixels then
      webwrite('margin-left:0px;margin-right:auto;')
    else if input.widthunit = cuPercent then
    begin
      MarginCalc:= round( (100{%} - input.width{%}) / 2); // get leftover percentage
      webwrite('margin-left:' + inttostr(MarginCalc) + '%;margin-right:' + inttostr(MarginCalc) +'%;')
    end;
  end;

  webwrite('padding:' + inttostr(input.pad) + 'px;' +
           'background-color:' + input.bgcolor + ';' +
           'z-index:' + inttostr(input.zindex) + ';' +
           '">' +
           input.text);
end;

procedure BoxOut(input: THtmCustomBox);
begin
  BoxBegin(input);
  BoxEnd;
end;


{ (HTML BEGIN) will be the standard name the user uses in his program
  for the beginning chunk of html. Since this procedure varies from document
  to document the programmer himself can make his own custom HTMb procedure
  and just not use this one. This is a simple one }
procedure HtmBegin;
begin
  webwrite('<html>');
  webwrite('<body>');
end;

{ Additional HTML BEGIN with ability to specify title }
procedure HtmBegin(title: string); overload;
begin
  webwrite('<html>');
  webwrite('<head>');
  webwrite('<title>' + title + '</title>');
  webwrite('</head>');
  webwrite('<body>');
end;

{ Additional HTML BEGIN with ability to specify title and stylesheet }
procedure HtmBegin(title: string; stylesheet: string); overload;
begin
  webwrite('<html>');
  webwrite('<head>');
  webwrite('<title>' + title + '</title>');
  webwrite('<link href="' + stylesheet + '" type="text/css" />');
  webwrite('</head>');
  webwrite('<body>');
end;

{ Additional HTML BEGIN with ability to specify title and body }
procedure HtmBegin(title: string; body: THtmBodyOld); overload;
begin
  webwrite('<html>');
  webwrite('<head>');
  webwrite('<title>' + title + '</title>');
  webwrite('</head>');
  OldBodyOut(body); //output body
end;

{ Same as above but with CSS body }
procedure HtmBegin(title: string; body: THtmBody); overload;
begin
  webwrite('<html>');
  webwrite('<head>');
  webwrite('<title>' + title + '</title>');
  webwrite('</head>');
  BodyOut(body); //output body
end;

{ Additional HTML BEGIN with ability to specify title, body, and stylesheet }
procedure HtmBegin(title: string; body: THtmBodyOld; stylesheet: string); overload;
begin
  webwrite('<html>');
  webwrite('<head>');
  webwrite('<title>' + title + '</title>');
  webwrite('<LINK rel="stylesheet" href="' + stylesheet + '" type="text/css" />');
  webwrite('</head>');
  OldBodyOut(body); //output body
end;

{ Same as above but with CSS body }
procedure HtmBegin(title: string; body: THtmBody; stylesheet: string); overload;
begin
  webwrite('<html>');
  webwrite('<head>');
  webwrite('<title>' + title + '</title>');
  webwrite('<LINK rel="stylesheet" href="' + stylesheet + '" type="text/css" />');
  webwrite('</head>');
  BodyOut(body); //output body
end;

{ Additional HTML BEGIN with title and stylesheet passing a pointer
  to Stylesheet Object.}
procedure HtmBegin(title: string; stylesheet: PStyleSheet); overload;
begin
  webwrite('<html>');
  webwrite('<head>');
  webwrite('<title>' + title +  '</title>');
  StyleSheet^.Write_Out;
  webwrite('</head>');
  webwrite('<body>');
end;

{ htmEND (HTML END) will be the standard name the user uses in his program
  for the END chunk of html. Since this procedure varies from document
  to document the programmer himself can make his own custom HTMb procedure
  and just not use this one. This is a simple one. }
procedure HtmEnd;
begin
  webwriteln('</body>');
  webwriteln('</html>');
end;


{ make bold text }
function Bold(input: string): string;
begin
  result:='<b>' + input + '</b>';
end;

{ output bold text }
procedure BoldOut(input: string);
begin
  webwrite('<b>' + input + '</b>');
end;

{ make italic text }
function Italic(input: string): string;
begin
  result:= '<i>' + input + '</i>';
end;

{ output italic text }
procedure ItalicOut(input: string);
begin
  webwrite('<i>' + input + '</i>');
end;

{ make strong text }
function Strong(input: string): string;
begin
  result:= ('<strong>' + input + '</strong>');
end;

{ output strong text }
procedure StrongOut(input: string);
begin
  webwriteln('<strong>' + input + '</strong>');
end;

{ make block quotation }
function BlockQt(input: string): string;
begin
  result:= ('<blockquote>' + input + '</blockquote>');
end;

{ output block quotation }
procedure BlockQtOut(input: string);
begin
  webwriteln('<blockquote>' + input + '</blockquote>');
end;

{ make text highlighted with any color}
function Highlight(input: string; HTMLColor: string): string;
begin
  result:= '<span style="background-color:' + HTMLColor +  '">' + input + '</span>';
end;

{ highlighted text output }
procedure HighlightOut(input: string; HTMLColor: string);
begin
  webwriteln('<span style="background-color:' + HTMLColor +  '">' + input + '</span>');
end;

{ make preformatted }
function Preformat(input: string): string;
begin
  result:= '<pre>' +  input + '</pre>';
end;

procedure PreformatOut(input: string);
begin
  webwriteln('<pre>' +  input + '</pre>');
end;


// END OF PUBLIC PROCEDURES/FUNCTIONS
{------------------------------------------------------------------------------}



end.



