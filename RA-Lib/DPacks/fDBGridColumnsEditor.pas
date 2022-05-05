{***********************************************************
                R&A Library
              R&A Form Designer
       Copyright (C) 1998-2001 Andrei Prygounkov

       description : R&A implemetation of
                     Delphi design-time packages

       author      : Andrei Prygounkov
       e-mail      : black@infa.ru
       www         : http://www.infa.ru/black/ralib.htm/ralib
************************************************************}

{$INCLUDE RA.INC}

unit fDBGridColumnsEditor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ComCtrls, StdCtrls, ExtCtrls, DBGrids,
  DsgnIntf, fCollectionEditor, RARegAuto;

type
  TDBGridColumnsEditor = class(TCollectionEditor)
    AddAllFields: TButton;
    RestoreDefaults: TButton;
  private
  end;

implementation

{$R *.dfm}

              
end.
