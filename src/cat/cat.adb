with Ada.Command_Line,
     Ada.Containers.Vectors,
     Ada.IO_Exceptions,
     Ada.Text_IO,
     Ada.Strings.Unbounded,
     Posix,
     Posix.Files,
     Posix.Permissions,
     Posix.File_Status,
     Posix.IO,
     ACoreTools.Switches;

procedure Cat is
	package PIO renames Posix.IO;
	package TIO renames Ada.Text_IO;
	package CLI renames Ada.Command_Line;
	package SW       is new ACoreTools.Switches(
    		Argument_Count => CLI.Argument_Count,
		Argument       => CLI.Argument
	);
	subtype Byte is Posix.Octet;
	subtype IO_Buffer is PIO.IO_Buffer;
	subtype IO_Count is Posix.IO_Count;
	use type Posix.IO_Count;
	package SU renames Ada.Strings.Unbounded;
	package SVec is new Ada.Containers.Vectors(
		Index_Type =>	Positive,
		Element_Type =>	SU.Unbounded_String,
		"=" =>	     	SU."="
	);
	Filenames_Vec : SVec.Vector;
	Invalid_Switch    : Boolean := False;
	Invalid_Switch_Sel: SU.Unbounded_String;
	procedure Cat_File (F: PIO.File_Descriptor) is
		B : IO_Buffer(1..1);
		C : IO_Count;
	begin
		PIO.Read(F, B, C);
		begin
			while true loop
				PIO.Write(PIO.Standard_Output, B, C);
				PIO.Read(F, B, C);
			end loop;
		exception
		when Ada.IO_Exceptions.End_Error =>
			return;
		end;
	end Cat_File;
	procedure Set_Switch(Selector : String) is
  	begin
		Invalid_Switch := True;
		Invalid_Switch_Sel := SU.To_Unbounded_String(Selector);
	end Set_Switch;
	procedure Scan_Switches is new SW.For_Every_Switch(
		Action => Set_Switch
	);
begin
	Scan_Switches;
	if Invalid_Switch then
		TIO.Put_Line("unknown option -" & SU.To_String(Invalid_Switch_Sel));
		CLI.Set_Exit_Status(1);
		return;
	end if;
	if CLI.Argument_Count < SW.First_Nonswitch_Index then
		Cat_File(PIO.Standard_Input);
	else
		for I in SW.First_Nonswitch_Index .. CLI.Argument_Count loop
			declare
				FN : Posix.Posix_String := Posix.To_Posix_String(CLI.Argument(I));
				FD : PIO.File_Descriptor;
			begin
				if Posix.Files.Is_Directory(FN) then
					TIO.Put_Line(
						TIO.Standard_Error,
					        Posix.To_String(FN) & ": is a directory"
      					);
					CLI.Set_Exit_Status(1);
					return;
				elsif not Posix.Files.Is_File(FN) then
					TIO.Put_Line(
						TIO.Standard_Error,
				        	Posix.To_String(FN) & ": not found"
      					);
					CLI.Set_Exit_Status(1);
					return;
				else
					FD := PIO.Open(FN, PIO.Read_Only);
					Cat_File(FD);
					PIO.Close(FD);
				end if;
			end;
		end loop;
	end if;	
end Cat;	  
