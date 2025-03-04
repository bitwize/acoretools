with Ada.Command_Line,
     Ada.Containers.Vectors,
     Ada.Text_IO,
     Ada.Strings.Unbounded,
     Posix,
     Posix.Files,
     Posix.Permissions,
     Posix.File_Status,
     ACoreTools.Switches;

procedure Ls is
  package CLI      renames Ada.Command_Line;
  package TIO      renames Ada.Text_IO;
  package SU       renames Ada.Strings.Unbounded;
  package Files    renames Posix.Files;
  package Perms    renames Posix.Permissions;
  package Stat     renames Posix.File_Status;
  package SW       is new ACoreTools.Switches(
  	Argument_Count => CLI.Argument_Count,
	Argument       => CLI.Argument
  );
  package SVec     is new Ada.Containers.Vectors(
  	Index_Type   => Positive,
	Element_Type => SU.Unbounded_String,
	"="          => SU."="
  );
  package SVSorter is new SVec.Generic_Sorting("<" => SU."<");
  List_All          : Boolean := False;
  Long_Listing      : Boolean := False;
  Invalid_Switch    : Boolean := False;
  Invalid_Switch_Sel: SU.Unbounded_String;
  Filenames_Vec     : SVec.Vector;

  procedure Add_File(
	  D_Entry : Files.Directory_Entry;
	  Quit    : in out Boolean
  ) is
    Entry_Name : String := Posix.To_String(Files.Filename_Of(D_Entry));
  begin
    if List_All or
       (Entry_Name'Length > 0 and
	Entry_Name(Entry_Name'First) /= '.')
    then
    	Filenames_Vec.Append(SU.To_Unbounded_String(Entry_Name));
    end if;
  end Add_File;

  procedure Display_Directory(FN: Posix.Posix_String) is
  	procedure DD_Helper is new Posix.Files.For_Every_Directory_Entry(
		Action => Add_File
  	);
	I : Positive;
  begin
  	Filenames_Vec.Clear;
	DD_Helper(FN);
	SVSorter.Sort(Filenames_Vec);
	for I in Filenames_Vec.First_Index .. Filenames_Vec.Last_Index loop
		TIO.Put_Line(SU.To_String(Filenames_Vec(I)));
	end loop;
  end Display_Directory;

  procedure Set_Switch(Selector : String) is
  begin
	if Selector = "l" then
  		Long_Listing := True;
	elsif Selector = "a" then
  		List_All := True;
	else
		Invalid_Switch := True;
		Invalid_Switch_Sel := SU.To_Unbounded_String(Selector);
	end if;
  end Set_Switch;

  procedure Scan_Switches is new SW.For_Every_Switch(
  	Action => Set_Switch
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
  Scan_Switches;
  if Invalid_Switch then
	TIO.Put_Line("unknown option -" & SU.To_String(Invalid_Switch_Sel));
	CLI.Set_Exit_Status(1);
	return;
  end if;
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
