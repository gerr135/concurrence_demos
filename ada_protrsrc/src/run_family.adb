--
-- An excercise in parallel programming, shared resource control
-- 
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--

with Ada.Command_Line, GNAT.Command_Line;
with Ada.Text_IO, Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with family;


procedure run_family is
    procedure printUsage is
        use Ada.Text_IO;
    begin
        Put_Line ("concurrent programming exc,""family""");
        New_Line;
        Put_Line ("usage:");
        Put_Line ("   " & Ada.Command_Line.Command_Name & " [-h -n: -v]  positional");
        New_Line;
        Put_Line ("options:");
        --  only short options for now
        
        Put_Line ("-n      n children");
        Put_Line ("-h      print this help");
        Put_Line ("-v(v)   verbocity level");
    end printUsage;

    Finish : exception;

    package fm is new Family(Resource => Natural);
    
    type ParamRec is record
		--  mostly the commandline params. DepGraph and USE flags will go as separate vars
        name    : Unbounded_String := Null_Unbounded_String;
        NChildren  : fm.ChildIdx := 5;
        Debug   : Boolean := False;
    end record;

    procedure processCommandLine (params : in out ParamRec) is
        use Ada.Command_Line, GNAT.Command_Line, Ada.Text_IO, Ada.Integer_Text_IO;
        Options : constant String := "h n: v";
        Last:Positive;
    begin
        if Argument_Count < 1 then
            printUsage;
            raise Finish;
        end if;
        begin -- need to process local exceptions
            loop
                case Getopt (Options) is
                    when ASCII.NUL =>
                        exit;
                    --  end of option list

                    when 'g' | 'v' => params.Debug := True;
                    when 'h' =>
                        printUsage;
                        raise Finish;

                    when 'n' => Get(Parameter,Positive(params.NChildren),Last);

                    when others =>
                        raise Program_Error;
                        --  serves to catch "damn, forgot to include that option here"
                end case;
            end loop;
        exception
            when Invalid_Switch =>
                Put_Line ("Invalid Switch " & Full_Switch);
                raise Finish;
            when Invalid_Parameter =>
                Put_Line ("No parameter for " & Full_Switch);
                raise Finish;
            when Data_Error =>
                Put_Line ("Invalid numeric format for switch" & Full_Switch);
                raise Finish;
        end;
        -- get positional params
        loop
            declare
                S : constant String := Get_Argument (Do_Expansion => True);
            begin
                exit when S'Length = 0;
                if params.Debug then Put_Line ("alternative file was passed: '" & S &"'"); end if;
                params.name := To_Unbounded_String(S);
            end;
        end loop;
    end processCommandLine;


    use Ada.Text_IO;

    params : ParamRec;
    family : fm.FamilyAccess;

begin  -- main
    processCommandLine (params);
    Put_Line("starting family run");
    family := new fm.FamilyType(Nch => params.NChildren);
    fm.Stop_and_Destroy(family);
exception
	when Finish => null;
end run_family;
