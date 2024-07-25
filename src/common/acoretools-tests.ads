generic
	Suite_Description : in String;
package ACoreTools.Tests is
	generic
		Description : in String;
		with function Test_Body return Boolean;
	procedure Test;

	procedure Show_Test_Header;
	procedure Show_Test_Results;
private
	Total_Tests  : Natural := 0;
	Tests_Passed : Natural := 0;
	Tests_Failed : Natural := 0;
end ACoreTools.Tests;
