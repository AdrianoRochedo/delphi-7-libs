unit DialogUtils;

interface
uses Graphics,
     Classes;

  function SelectFile(  out FileName: String;
                      const InitialDir: String;
                      const Filter: String = 'Todos (*.*)|*.*';
                      const FileMustExist: Boolean = true;
                      const Caption: String = ' Selecione um arquivo'): Boolean;

  function ChooseFilename(  out FileName: String;
                          const InitialDir: String;
                          const DefaultExt: string;
                          const Filter: String = 'Todos (*.*)|*.*'): boolean;

  function SelectColor(DefaultColor: TColor): TColor;

  procedure SelectFont(Font: TFont);

  procedure EditText(Text: TStrings);

  function SelectInteger(const Caption: string; defaultValue: integer): integer;

  function SelectItem(var Selected: integer; const Items: array of string): string;

implementation
uses Dialogs,
     SysUtils,
     StringsDialog,
     ComboBoxDialog;

  function SelectFile(  out FileName: String;
                      const InitialDir: String;
                      const Filter: String = 'Todos (*.*)|*.*';
                      const FileMustExist: Boolean = true;
                      const Caption: String = ' Selecione um arquivo'): Boolean;

  var OpenDialog: TOpenDialog;
  begin
    OpenDialog := TOpenDialog.Create(nil);
    OpenDialog.Title := Caption;
    OpenDialog.Filter := Filter;

    if FileMustExist then
       OpenDialog.Options := OpenDialog.Options + [ofFileMustExist];

    OpenDialog.InitialDir := InitialDir;

    Result := OpenDialog.Execute();
    if Result then
       FileName := OpenDialog.FileName
    else
       Filename := '';

    OpenDialog.Free();
  end;

  function ChooseFilename(  out FileName: string;
                          const InitialDir: string;
                          const DefaultExt: string;
                          const Filter: string = 'Todos (*.*)|*.*'): boolean;
  var saveDialog: TSaveDialog;
  begin
    saveDialog := TSaveDialog.Create(nil);
    saveDialog.Title := ' Salvar arquivo';
    saveDialog.Filter := Filter;
    saveDialog.DefaultExt := DefaultExt;

    result := saveDialog.Execute();
    if result then
       Filename := saveDialog.FileName
    else
       Filename := '';

    saveDialog.Free();
  end;

  function SelectColor(DefaultColor: TColor): TColor;
  var d: TColorDialog;
  begin
    d := TColorDialog.Create(nil);
    d.Color := DefaultColor;

    if d.Execute() then
       result := d.Color
    else
       result := DefaultColor;

    d.Free();
  end;

  procedure SelectFont(Font: TFont);
  var d: TFontDialog;
  begin
    d := TFontDialog.Create(nil);
    d.Font.Assign(Font);

    if d.Execute() then
       Font.Assign(d.Font);

    d.Free();
  end;

  procedure EditText(Text: TStrings);
  begin
    TStringsDialog.getStrings(Text);
  end;

  function SelectInteger(const Caption: string; defaultValue: integer): integer;
  var s: string;
  begin
    s := Dialogs.InputBox(Caption, 'Entre com um valor inteiro', intToStr(defaultValue));
    try
      result := StrToInt(s);
    except
      raise Exception.Create('Valor inválido');
    end;
  end;

  function SelectItem(var Selected: integer; const Items: array of string): string;
  begin
    result := TComboBoxDialog.getSelected(Selected, Items);
  end;

end.
