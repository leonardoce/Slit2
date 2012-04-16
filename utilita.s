# -*- mode:lout -*-
@Chapter
@Title { HTML generation utilities }
@Begin @LP

HTML generation is a little pecurial because text documentation need a
specific syntax: some character must be escaped as HTML entities. @PP

So I've made a function to output a string of text in HTML format and
this function is used to print scraps. @PP

The implementation need to read every character in the string:

@d text2html
@{
  function text2html(str:String):String;
  var
    buffer:String;
    i:Integer;
  begin
    buffer := '';
    for i:=1 to Length(str) do
    begin
      @<text2html, checking every character@>
    end;

    Result := buffer;
  end;
@}

Every character, if needed, is escaped with the relative HTML entity:

@d text2html, checking every character
@{
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
@}

Another useful function is @F cleantext, which remove all the
characters that can't be in LOut indexing keys:

@d cleantext
@{
function cleantext(str:String):String;
var
  buffer : String;
  i : Integer;
begin
  buffer := '';

  for i:=1 to Length(str) do
  begin
    if ((str[i]>='A') and (str[i]<='Z')) or ((str[i]>='a') and (str[i]<='z')) or (str[i]='.') then
    begin
      buffer := buffer + LowerCase(str[i]);
    end;
  end;

  Result := buffer;      
end;
@}

This is the declaration of the @F htmlutils unit:

@d unit htmlutils
@{
{$MODE OBJFPC}
{$M+}
unit htmlutils;

interface

  uses sysutils, strutils;

  function text2html(str:String):String;
  function cleantext(str:String):String;

implementation

  @<text2html@>
  @<cleantext@>
end.
@}

@o htmlutils.pas
@{
  @<unit htmlutils@>
@}

@End @Chapter
