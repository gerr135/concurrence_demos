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

use Ada.Text_IO;

procedure phil_arbitr is
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



    Finish : exception;

    MaxDuration : constant Duration := 0.01;
    waitTick    : constant Duration := 0.001;
    --type Durations is new Positive range 1 .. MaxDuration;
    --package Rnd is new Ada.Numerics.Discrete_Random(Durations);
    package Rnd renames Ada.Numerics.Float_Random;
    --
    procedure rndWait(G : Rnd.Generator) is
        dur : Duration := MaxDuration*Duration(Rnd.Random(G));
    begin
        --Put_Line("P"&N'Img&" waiting "&dur'Img);
        delay dur;
    end;


    type Index is mod 5;
    package IO is new Ada.Text_IO.Modular_IO(Index);

    ---- stick handling -----------------
    type Stick_State is (Free,Taken);

    task type Stick_Type is
        entry pickup;
        entry release;
        entry GetState(S : out Stick_State);
    end;

    function State(S : Stick_Type) return Stick_State;
    function State(S : Stick_Type) return Character;

    Sticks : array (Index) of Stick_Type;



    ---- hunger monitoring ------------------
    type Hunger_Rec  is record
        curHunger : Natural;
        lastEaten : Ada.Calendar.Time;
    end record;

    type Hunger_State is (Fed, Hungry, Starved, Dead);
    -- we go here with 1 unit of hunger per MaxDuration;

    function getHunger(H : Hunger_Rec) return Hunger_State is
    begin
        if H.curHunger < 6 then
            return Fed;
        elsif H.curHunger < 11 then
            return Hungry;
        elsif H.curHunger < 21 then
            return Starved;
        else
            return Dead;
        end if;
    end;

    procedure updateHunger(H : in out Hunger_Rec);
    procedure satiate(H : in out Hunger_Rec);

    type Hunger_Array is array (Index) of Hunger_Rec;


    --------------------------------------------------------
    --  Implementation area

    task body Stick_Type is
        State : Stick_State := Free;
    begin
        loop
            select
                when State = Free =>
                accept pickup do
                    State := Taken;
                end pickup;
            or
                accept release do
                    State := Free;
                end release;
            or
                accept GetState(S : out Stick_State) do
                    S := State;
                end GetState;
            end select;
        end loop;
    end;

    function State(S : Stick_Type) return Stick_State is
        X : Stick_State;
    begin
        S.GetState(X);
        return X;
    end;
    function State(S : Stick_Type) return Character is
        X : Stick_State;
    begin
        S.GetState(X);
        return (if X = Taken then '|' else '_');
    end;


    procedure updateHunger(H : in out Hunger_Rec) is
        use Ada.Calendar;
        durSinceEaten : Duration := Clock - H.lastEaten;
    begin
        H.curHunger := Natural(durSinceEaten/MaxDuration);
    end;

    procedure satiate(H : in out Hunger_Rec) is
        use Ada.Calendar;
    begin
        H.curHunger := 0;
        H.lastEaten := Clock;
    end;


    -----------------------------------------------
    --  now the processes

    --  need access to hunger data to signal eating
    Hunger : Hunger_Array := (others => (0,Ada.Calendar.Clock));

    --------------------------------------------------------
    -- encapsulate all monitoring in a separate task, which will also provide some arbitrage..
    task Arbitr is
        entry Start_Eating (N : Index);
        entry Finish_Eating(N : Index);
    end;

    task body Arbitr is
        NEating : Index := 0;
    begin
        loop
            select
                when NEating < Index'Last => -- 0-based, so it is indeded all-1
                accept Start_Eating (N : Index) do
                    NEating := NEating + 1;
                end Start_Eating;
            or
                accept Finish_Eating(N : Index) do
                    NEating := NEating - 1;
                end Finish_Eating;
            end select;
        end loop;
    end;

    -----------------------------------------------------
    type Phil_Type is (Basic,Alter,OneOff);
    PhilType_param : Phil_Type := Basic;

    type Phil_Iface is task interface;
    type Phil_Access is access Phil_Iface'Class;

    task type Phil_Type_base (N : Index) is new Phil_Iface with end;
    task type Phil_Type_altr (N : Index) is new Phil_Iface with end;
    task type Phil_Type_1off (N : Index) is new Phil_Iface with end;

    task body Phil_Type_base is
        G : Rnd.Generator;
        use IO;
    begin
        Rnd.Reset(G);
        Put_Line("start of P"&N'Img);
        loop
            -- thinking process
            rndWait(G);
            updateHunger(Hunger(N));
            -- now attempt to eat
            Arbitr.Start_Eating(N);
            -- good to go, take sticks
            Put("P");Put(N,1);Put(" pickup L, ");
            Sticks(N).pickUp;
            Put("P");Put(N,1);Put(" pickup R, ");
            Sticks(N+1).pickUp;
            Put("P");Put(N,1);Put_Line(" eating.");
            -- acquired sticks, lets eat
            rndWait(G);
            satiate(Hunger(N));
            Arbitr.Finish_Eating(N);
            --
            Put("P");Put(N,1);Put(" released, ");
            Sticks(N).release;Sticks(N+1).release;
            Put("P");Put(N,1);Put_Line(" done,  now sleeping..  ");
        end loop;
    end;

    task body Phil_Type_altr is
        G : Rnd.Generator;
        use IO;
    begin
        Put_Line("start of P"&N'Img);
        Rnd.Reset(G);
        loop
            rndWait(G);
            updateHunger(Hunger(N));
            -- try alternating hands
            if N mod 2 = 0 then
                Put("P");Put(N,1);Put(" pickup L, ");
                Sticks(N).pickUp;
                Put("P");Put(N,1);Put(" pickup R, ");
                Sticks(N+1).pickUp;
            else
                Put("P");Put(N,1);Put(" pickup R, ");
                Sticks(N+1).pickup;
                Put("P");Put(N,1);Put(" pickup L, ");
                Sticks(N).pickUp;
            end if;
            Put("P");Put(N,1);Put_Line(" eating.");
            --
            rndWait(G);
            satiate(Hunger(N));  --eating
            --
            Put("P");Put(N,1);Put(" released, ");
            Sticks(N).release;Sticks(N+1).release;
            Put("P");Put(N,1);Put_Line(" done,  now sleeping..  ");
        end loop;
    end;

    task body Phil_Type_1off is
        G : Rnd.Generator;
        use IO;
    begin
        Put_Line("start of P"&N'Img);
        Rnd.Reset(G);
        loop
            rndWait(G);
            updateHunger(Hunger(N));
            -- try alternating hands
            if N = 4 then
                Put("P");Put(N,1);Put(" pickup L, ");
                Sticks(N+1).pickUp;
                Put("P");Put(N,1);Put(" pickup R, ");
                Sticks(N).pickUp;
            else
                Put("P");Put(N,1);Put(" pickup R, ");
                Sticks(N).pickup;
                Put("P");Put(N,1);Put(" pickup L, ");
                Sticks(N+1).pickUp;
            end if;
            Put("P");Put(N,1);Put_Line(" eating.");
            --
            rndWait(G);
            satiate(Hunger(N));  --eating
            --
            Put("P");Put(N,1);Put(" released, ");
            Sticks(N).release;Sticks(N+1).release;
            Put("P");Put(N,1);Put_Line(" done,  now sleeping..  ");
        end loop;
    end;



    -------------------------------------------------------------
    --  main routine - monitoring services
    procedure updateState(H : Hunger_Array) is
    begin
        -- just the hunger for now
        for i in Index loop
            updateHunger(Hunger(i));
        end loop;
    end;

    procedure printState(H : Hunger_Array) is
        use Ada.Integer_Text_IO;
    begin
        New_Line;
        Put("Sticks: [");
        for i in Index loop
            Put(State(Sticks(i)));
        end loop;
        Put("];    hunger: [");
        for i in Index loop
            Put(H(i).curHunger, 2);Put(",");
        end loop;
        Put_Line("]");
    end;

    procedure checkAlive(H : Hunger_Array) is
        use IO;
        starving : Boolean := False;
    begin
        for i in Index loop
            if getHunger(H(i)) = Hungry then
                starving := True;
                Put("P");Put(i,1);Put(" is hungry, ");
            elsif getHunger(H(i)) = Starved then
                starving := True;
                Put("P");Put(i,1);Put(" is starved,  ");
            elsif getHunger(H(i)) = Dead then
                New_Line;
                Put("P");Put(i,1);Put_Line(" is Dead!!  Aborting.. ");
                raise Finish;
            end if;
        end loop;
        if starving then New_Line; end if;
    end;


    ----------------------------------------------------

    procedure processCommandLine is
        use Ada.Command_Line, GNAT.Command_Line;
        Options : constant String := "h";
        --Last:Positive;
    begin
        begin -- need to process local exceptions
            loop
                case Getopt (Options) is
                    when ASCII.NUL =>
                        exit;

                    when 'h' => printUsage; raise Finish;

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

    use Ada.Command_Line;

    P : array (Index) of Phil_Access;

begin  -- main
    processCommandLine;
    New_Line;
    Put_Line("main program start");
    case PhilType_param is
        when Basic =>
            for i in Index loop
                P(i) := new Phil_Type_base(i);
            end loop;
        when Alter =>
            for i in Index loop
                P(i) := new Phil_Type_altr(i);
            end loop;
        when OneOff =>
            for i in Index loop
                P(i) := new Phil_Type_1off(i);
            end loop;
    end case;
    loop
        updateState(Hunger);
        printState(Hunger);
        checkAlive(Hunger);
        delay MaxDuration;
    end loop;
exception
    when Finish =>
        for i in Index loop
            abort P(i).all;
            abort Sticks(i);
        end loop;
        abort Arbitr;
end phil_arbitr;
