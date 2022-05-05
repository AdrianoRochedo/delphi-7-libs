// this sample don't work

unit Unit1;

procedure ButtonClick(Sender: TObject);
begin
  MessageBox(0, TButton(Sender).Caption + ' clicked!', 'Information', MB_ICONINFORMATION);
end;

end.
