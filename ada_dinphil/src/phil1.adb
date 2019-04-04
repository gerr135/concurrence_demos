--
-- Testing concurrency. Dining philosophers.
-- Using protected object to hold sticks
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--

with Ada.Text_IO, Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Numerics.Float_Random;
with Ada.Calendar;
with Ada.Command_Line, GNAT.Command_Line;

with GNAT.Bounded_buffers;

with Phil_Common; use Phil_Common;

use Ada.Text_IO;

procedure phil1 is
    procedure printUsage is
        use Ada.Text_IO;
    begin
        Put_Line ("concurrency tests: dining philosophers, using protected object for sticks");
        New_Line;
        Put_Line ("usage:");
        Put_Line ("   " & Ada.Command_Line.Command_Name & " [-h -g] [alg]");
        New_Line;
        Put_Line ("options:");
        --  only short options for now
        Put_Line ("-h      print this help");
        Put_Line ("-g      turn on debug output");
        Put_Line ("alg     (positional) algorythm to use: b - basic, a - alternating, 1 - 1 with diff order");
    end printUsage;



    Sticks : Stick_Array;
    CR : CombinedInfo_Rec := Reset_CR;

    -----------------------------------------------
    --  now the processes
    type Phil_Type is (Basic,Alter,OneOff);
    PhilType_param : Phil_Type := Basic;

    type Phil_Iface  is task interface;
    type Phil_Access is access Phil_Iface'Class;

    task type Phil_Type_base (N : PIndex) is new Phil_Iface with end;
    task type Phil_Type_altr (N : PIndex) is new Phil_Iface with end;
    task type Phil_Type_1off (N : PIndex) is new Phil_Iface with end;

    task body Phil_Type_base is
    begin
        Put_Line("start of base P"&N'Img);
        loop
            pickupStick(N, Left,  Sticks, CR);
            pickupStick(N, Right, Sticks, CR);
            --
            Eat(N,CR);
            --
            releaseStick(N, Left,  Sticks, CR);
            releaseStick(N, Right, Sticks, CR);
            --
            Think(N, CR);
        end loop;
    end;

    task body Phil_Type_altr is
    begin
        Put_Line("start of alternating P"&N'Img);
        loop
            -- try alternating hands
            if N mod 2 = 0 then
                pickupStick(N, Left,  Sticks, CR);
                pickupStick(N, Right, Sticks, CR);
            else
                pickupStick(N, Right, Sticks, CR);
                pickupStick(N, Left,  Sticks, CR);
            end if;
            --
            Eat(N,CR);
            --
            releaseStick(N, Left,  Sticks, CR);
            releaseStick(N, Right, Sticks, CR);
            --
            Think(N, CR);
        end loop;
    end;

    task body Phil_Type_1off is
    begin
        Put_Line("start of P"&N'Img);
        loop
            -- try alternating hands
            if N = 4 then
                pickupStick(N, Left,  Sticks, CR);
                pickupStick(N, Right, Sticks, CR);
            else
                pickupStick(N, Right, Sticks, CR);
                pickupStick(N, Left,  Sticks, CR);
            end if;
            --
            Eat(N,CR);
            --
            releaseStick(N, Left,  Sticks, CR);
            releaseStick(N, Right, Sticks, CR);
            --
            Think(N, CR);
        end loop;
    end;



    ----------------------------------------------------
    Finish : exception;
    type ParamRec is record
        Verbose : Boolean := False;
        StickTask : Boolean := False;
        doArbitr  : Boolean := False;
    end record;

    procedure processCommandLine(params : in out ParamRec) is
        use Ada.Command_Line, GNAT.Command_Line;
        Options : constant String := "a h t v";
        --Last:Positive;
    begin
        begin -- need to process local exceptions
            loop
                case Getopt (Options) is
                    when ASCII.NUL => exit;

                    when 'a' => params.doArbitr := True;
                    when 'h' => printUsage; raise Finish;
                    when 't' => params.StickTask := True;
                    when 'v' => params.Verbose := True;

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
                if S = "b" then
                    PhilType_param := Basic;
                elsif S = "a" then
                    PhilType_param := Alter;
                elsif S="1" then
                    PhilType_param := OneOff;
                else
                    Put_Line("invalid dining algorythm selected!");
                    New_Line;
                    printUsage;
                    raise Finish;
                end if;
            end;
        end loop;
    end processCommandLine;


    params : ParamRec;

begin  -- main
    processCommandLine(params);
    New_Line;
    Put_Line("main program start");
    declare
        P : array (PIndex) of Phil_Access;
        Logger : Logger_Task(CR.LB'Unchecked_Access, doPrinting => params.Verbose);
    begin
        -- we first need to setup proper Sticks - protected ot task
        if params.StickTask then
            for i in PIndex loop
                Sticks(i) := new Task_Stick_Type;
            end loop;
        else
            for i in PIndex loop
                Sticks(i) := new Prot_Stick_Type;
            end loop;
        end if;
        -- Sticks are ready, init philosopphers now
        case PhilType_param is
            when Basic =>
                for i in PIndex loop
                    P(i) := new Phil_Type_base(i);
                end loop;
            when Alter =>
                for i in PIndex loop
                    P(i) := new Phil_Type_altr(i);
                end loop;
            when OneOff =>
                for i in PIndex loop
                    P(i) := new Phil_Type_1off(i);
                end loop;
        end case;
        New_Line;
        Put_Line("_main: initialized philosophers");
        loop
            updateState(CR.State);
            updateStats(CR.Stats, CR.State);
            if params.Verbose then
                printFullState(CR.State, Sticks);
            end if;
            printStats(CR.Stats);
            checkAlive(CR.State);
            delay maxDuration;
        end loop;
    exception
        when Phil_Death =>
            for i in PIndex loop
                abort P(i).all;
            end loop;
            abort Logger;
    end;
exception
    when FInish => Null;
end phil1;
