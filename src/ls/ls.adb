with Ada.Command_Line,
     Ada.Text_IO,
     Ada.Strings.Unbounded,
     Posix,
     Posix.Files,
     Posix.Permissions,
     Posix.File_Status,
     ACoreTools.Switches;

procedure Ls is
  package CLI   renames Ada.Command_Line;
  package TIO   renames Ada.Text_IO;
  package SU    renames Ada.Strings.Unbounded;
  package Files renames Posix.Files;
  package Perms renames Posix.Permissions;
  package Stat  renames Posix.File_Status;
  package SW    is new ACoreTools.Switches(
  	Argument_Count => CLI.Argument_Count,
	Argument       => CLI.Argument
  );
  List_All          : SW.Switch_Record := ('a', False);
  Long_Listing      : SW.Switch_Record := ('l', False);
  procedure Display_File(
	  D_Entry : Files.Directory_Entry;
	  Quit    : in out Boolean
  ) is
    Entry_Name : String := Posix.To_String(Files.Filename_Of(D_Entry));
  begin
    SW.Look_For_Switch(List_All);
    SW.Look_For_Switch(Long_Listing);
    if List_All.Value or
       (Entry_Name'Length > 0 and
	Entry_Name(Entry_Name'First) /= '.')
    then
      TIO.Put_Line(Entry_Name);
    end if;
  end Display_File;

  procedure Display_Directory is new Posix.Files.For_Every_Directory_Entry(
	Action => Display_File
  );

  function Unbounded_To_Posix_String(
	Str : SU.Unbounded_String
  ) return Posix.Posix_String is
  begin
    return Posix.To_Posix_String(SU.To_String(Str));
  end Unbounded_To_Posix_String;

  UFN               : SU.Unbounded_String;
  Current_Directory : String := ".";
begin
  if CLI.Argument_Count < SW.First_Nonswitch_Index then
    UFN := SU.To_Unbounded_String(Current_Directory);
  else
    UFN := SU.To_Unbounded_String(CLI.Argument(SW.First_Nonswitch_Index));
  end if;
  declare
    FN : Posix.Posix_String := Unbounded_To_Posix_String(UFN);
  begin
    if not Posix.Files.Is_File_Present(FN) then
      TIO.Put_Line(
        TIO.Standard_Error,
        Posix.To_String(FN) & ": not found"
      );
      CLI.Set_Exit_Status(1);
    else
      if Posix.Files.Is_Directory(FN) then
      	Display_Directory(FN);
      else
        TIO.Put_Line(Posix.To_String(FN));
      end if;
    end if;
  end;
end Ls;
