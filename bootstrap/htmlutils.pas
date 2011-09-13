  {$MODE OBJFPC}
  {$M+}
  unit htmlutils;
  
  interface
  
    uses sysutils, strutils;
  
      function text2html(str:String):String;
  
  implementation
  
      function text2html(str:String):String;
      var
        buffer:String;
        i:Integer;
      begin
        buffer := '';
        for i:=1 to Length(str) do
        begin
            if str[i]='<' then
            begin
              buffer := buffer + '&lt;';
            end
            else if str[i]='>' then
            begin
              buffer := buffer + '&gt;';
            end
            else if str[i]='&' then
            begin
              buffer := buffer + '&amp;';
            end
            else
            begin
              buffer := buffer + str[i];
            end;
        end;
    
        Result := buffer;
      end;
  end.
