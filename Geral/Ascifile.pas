unit Ascifile;

(*
I wrote this component because it took me too long to get the ODBC Ascii
file driver working. All I wanted was to create an ascii file from a
database. I then sat down and wrote this. It is not fancy or tested in any
way, but it works for me.

This is freeware. You may do as you please with this component and the
source code. You use this component at your own risk. I will not be liable
for any damage this component causes. So if it causes your PC to blow up,
don't come crying to me.

*)


interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, DB, DBTables;

type
  TFileType = (CSV, Fixed);
  TAsciiFile = class(TComponent)
  private
    { Private declarations }
    FFileType   :  TFileType;
    FHeaders    :  boolean;
    FFileName   :  string;
    FDataSource :  TDataSource;
    FQuoteAll   :  boolean;
    FAscii      :  TextFile;
  protected
    { Protected declarations }
    Procedure WriteCSVHeader;
    Procedure WriteCSVField(Field : TField;Var FirstField : boolean);
    Procedure CreateCSVFile;
    Procedure WriteFixedHeader;
    Procedure WriteFixedField(Field : TField);
    Procedure CreateFixedFile;
  public
    { Public declarations }
    Procedure Execute;
    constructor Create(AOwner: TComponent); virtual;
  published
    { Published declarations }
    property FileType : TFileType read FFileType write FFileType;
    property Headers : boolean read FHeaders write FHeaders;
    property FileName : string read FFileName write FFileName;
    property DataSource : TDataSource read FDataSource write FDataSource;
    property QuoteAll : boolean read FQuoteAll write FQuoteAll;
  end;

procedure Register;

implementation

(*
The constructor sets up the defaults for the properties. I
decided to use a constructor, in order for anybody to set their
own defaults for the properties
*)
constructor TAsciiFile.Create(AOwner: TComponent);
  Begin
   inherited Create(AOwner);
   FFileType := CSV;
   FHeaders := True;
   FQuoteAll := False;
   FFileName := '';
  end;

(*
This procedure creates the header-line containing the names of the columns.
*)
Procedure TAsciiFile.WriteCSVHeader;
 Var S              : string;
     X              : integer;
     FirstLabel     : boolean;
  Begin
   FirstLabel := True;
   for X := 0 to FDataSource.DataSet.FieldCount-1 do
    Begin
     if FDataSource.DataSet.Fields[X].Visible = True then
      Begin
       if not FirstLabel then Write(FAscii, ';');
       Write (FAscii, FDataSource.DataSet.Fields[X].DisplayLabel);
       FirstLabel := False;
      end;
    end;
   Writeln(FAscii);
  end;

(*
This procedure creates the entries for the fields. String fields
are enclosed in double quotes. Numeric fields are not enclosed in
double quotes, unless the QuoteAll propery is True. The following
field types are not supported - TBytesField,TVarBytesField,TBlobField,
TMemoField,TGraphicField. These fields will cause an "Invallid Field"
entry to appear in the file.
*)
Procedure TAsciiFile.WriteCSVField(Field : TField;Var FirstField : boolean);
 Var S : string;
     X : byte;
  Begin
   if not Field.Visible then Exit;

   Case Field.DataType of
    ftUnknown, ftString:
      S := '"'+Field.DisplayText+'"';

    ftSmallInt, ftInteger, ftWord, ftBoolean, ftFloat,
    ftCurrency, ftBCD, ftDate, ftTime, ftDateTime:
      if FQuoteAll then
         S := '"'+Field.DisplayText+'"'
      else
         S := Field.DisplayText;

    ftBytes, ftVarBytes, ftBlob, ftMemo, ftGraphic :
      S := '"Invallid Field"'
   end;

   if FirstField = False then Write (FAscii, ';');
   Write (FAscii,S);
   FirstField := False;
  end;

(*
This procedure loops through the file and calls WriteCSVField
for each field.
*)
Procedure TAsciiFile.CreateCSVFile;
 Var FirstField : boolean;
     X          : integer;
  Begin
   if FHeaders = True then WriteCSVHeader;
   FDataSource.DataSet.DisableControls;
   try
     FDataSource.DataSet.First;
     While Not FDataSource.DataSet.Eof do
       Begin
       FirstField := True;
       for X := 0 to FDataSource.DataSet.FieldCount-1 do
          WriteCSVField(FDataSource.DataSet.Fields[X],FirstField);
       Writeln (FAscii);
       FDataSource.DataSet.Next;
       end;
   finally
     FDataSource.DataSet.EnableControls;
   end;
  end;

(*
I use PadString to create string that are padded and justified for the
fixed ascii file.
*)

Function PadString(S : string;Align : TAlignment;Size : byte) : string;
 Var X,Y       : byte;
     BlankStr  : string[255];
  Begin
   FillChar(BlankStr,SizeOf(BlankStr),#32);
   BlankStr[0] := #255;
   X := Length(S);
   if X>=Size then
    Begin
     PadString := Copy(S,1,Size);
     Exit;
    end;
   Case Align of
    taLeftJustify    : Begin
                        Y := Size-X;
                        PadString := S+Copy(BlankStr,1,Y);
                       end;
    taCenter         : Begin
                        Y := Size-X;
                        X := Y div 2;
                        Y := Y-X;
                        PadString := Copy(BlankStr,1,X)+S+Copy(BlankStr,1,Y);
                       end;
    taRightJustify   : Begin
                        Y := Size-X;
                        PadString := Copy(BlankStr,1,Y)+S;
                       end;
   end;
  end;

(*
WriteFixedHeader writes the names of the fields in the first line of the
output file.
*)
Procedure TAsciiFile.WriteFixedHeader;
 Var S              : string;
     X,Y            : integer;
     JstFy          : TAlignment;
  Begin
   for X := 0 to FDataSource.DataSet.FieldCount-1 do
     if FDataSource.DataSet.Fields[X].Visible then
       Begin
       S := FDataSource.DataSet.Fields[X].DisplayLabel;
       Y := FDataSource.DataSet.Fields[X].DisplayWidth;
       JstFy := FDataSource.DataSet.Fields[X].Alignment;
       Write (FAscii, PadString(S, JstFy, Y));
       end;
   Writeln(FAscii);
  end;

(*
WriteFixedField uses PadString to create padded & justified strings to
write to the file.
*)
Procedure TAsciiFile.WriteFixedField(Field : TField);
 Var S      : string;
     X      : byte;
     JstFy  : TAlignment;
  Begin
   if Field.Visible = False then Exit;
   Case Field.DataType of
    ftBytes,ftVarBytes,ftBlob,ftMemo,ftGraphic : S := '"Invallid Field"'
    else S := Field.DisplayText;
   end;
   JstFy := Field.Alignment;
   X := Field.DisplayWidth;
   Write (FAscii,PadString(S,JstFy,X));
  end;

(*
CreateFixedFile just loops through the database and creats a line for each
record.
*)
Procedure TAsciiFile.CreateFixedFile;
 Var X    : integer;
  Begin
   if FHeaders = True then WriteFixedHeader;
   FDataSource.DataSet.DisableControls;
   try
     FDataSource.DataSet.First;
     While Not FDataSource.DataSet.Eof do
       Begin
       for X := 0 to FDataSource.DataSet.FieldCount-1 do
          WriteFixedField(FDataSource.DataSet.Fields[X]);
       Writeln (FAscii);
       FDataSource.DataSet.Next;
       end;
   finally
     FDataSource.DataSet.EnableControls;
   end;
  end;

(*
This is the only method of TAsciiFile. It creates the ascii file
and then calls either CreateCSVFile or CreateFixedFile. It then
closes the ascii file.
*)
Procedure TAsciiFile.Execute;
Begin
  FDataSource.DataSet.Active := True;
  AssignFile(FAscii,FFileName);
  System.Rewrite(FAscii);
  Case FFileType of
    CSV   : CreateCSVFile;
    Fixed : CreateFixedFile;
  end;
  CloseFile(FAscii);
end;

procedure Register;
begin
  RegisterComponents('Data Access', [TAsciiFile]);
end;

end.
