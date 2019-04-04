--
-- Testing concurrency. Dining philosophers, base version
-- no protection, no guards, shared vars, etc.
-- This should produce collisions/locks..
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

use Ada.Text_IO;

procedure phil0 is

    Finish : exception;

    MaxDuration : constant Duration := 0.01;
    waitTick    : constant Duration := 0.001;
    --type Durations is new Positive range 1 .. MaxDuration;
    --package Rnd is new Ada.Numerics.Discrete_Random(Durations);
    package Rnd renames Ada.Numerics.Float_Random;

    type Index is new Natural range 0 .. 4;
    package IO is new Ada.Text_IO.Integer_IO(Index);

    function Succ(i : Index) return Index is
    begin
        return (i+1) mod 5;
    end;
    function Pred(i : Index) return Index is
    begin
        return (i+4) mod 5;
    end;

    ---- stick handling -----------------
    type Stick_State is (Free,Taken);
    type Stick_Array is array(Index) of Stick_State;

    procedure printSticks(S : Stick_Array) is
    begin
        New_Line;
        Put("Sticks: ");
        for i in Index loop
            Put(if S(i) = Taken then "T" else "F");
        end loop;
        New_Line;
    end;

    Stick : Stick_Array := (others => Free);
    procedure pickup(N : Index);
    procedure release(N : Index);


    ---- hunger monitoring ------------------
    type HungerState_Rec is record
        curHunger : Natural;
        lastEaten : Ada.Calendar.Time;
    end record;
    type Hunger_Array is array (Index) of HungerState_Rec;

    type Hunger_State is (Fed, Hungry, Starved, Dead);
    -- we go here with 1 unit of hunger per MaxDuration;

    function getHunger(H : Hunger_Array; N : Index) return Hunger_State is
    begin
        if H(N).curHunger < 6 then
            return Fed;
        elsif H(N).curHunger < 11 then
            return Hungry;
        elsif H(N).curHunger < 21 then
            return Starved;
        else
            return Dead;
        end if;
    end;


    Hunger : Hunger_Array := (others => (0,Ada.Calendar.Clock));
    procedure incHunger(N : Index);
    procedure decHunger(N : Index);



    --------------------------------------------------------
    --  Implementation area
    procedure pickup(N : Index) is
    begin
        while Stick(N) = Taken loop
            delay waitTick;
            incHunger(N);
        end loop;
        Stick(N) := Taken;
    end;

    procedure release(N : Index) is
    begin
        Stick(N) := Free;
    end;

    procedure incHunger(N : Index) is
        use Ada.Calendar;
        durSinceEaten : Duration := Clock - Hunger(N).lastEaten;
    begin
        Hunger(N).curHunger := Natural(durSinceEaten/MaxDuration);
    end;

    procedure decHunger(N : Index) is
        use Ada.Calendar;
    begin
        Hunger(N).curHunger := 0;
        Hunger(N).lastEaten := Clock;
    end;


    ------ hunger and other stats reporting
    procedure printState(S : Stick_Array; H : Hunger_Array) is
        use Ada.Integer_Text_IO;
    begin
        New_Line;
        Put("sticks: [");
        for i in Index loop
            Put(if S(i) = Taken then "|" else "_");
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
            if getHunger(H,i) = Hungry then
                starving := True;
                Put("P");Put(i,1);Put(" is hungry, ");
            elsif getHunger(H,i) = Starved then
                starving := True;
                Put("P");Put(i,1);Put(" is starved,  ");
            elsif getHunger(H,i) = Dead then
                New_Line;
                Put("P");Put(i,1);Put_Line(" is Dead!!  Aborting.. ");
                raise Finish;
            end if;
        end loop;
        if starving then New_Line; end if;
    end;



    -----------------------------------------------
    --  now the processes
    procedure rndWait(G : Rnd.Generator; N : Index) is
        dur : Duration := MaxDuration*Duration(Rnd.Random(G));
    begin
        --Put_Line("P"&N'Img&" waiting "&dur'Img);
        delay dur;
    end;


    task type Phil_Type_base (N : Index);
    task type Phil_Type_alter (N : Index);

    task body Phil_Type_alter is
        G : Rnd.Generator;
        use IO;
    begin
        Put_Line("start of P"&N'Img);
        Rnd.Reset(G);
        loop
            rndWait(G, N);
            incHunger(N);
            -- try alternating hands
            if N mod 2 = 0 then
                Put("P");Put(N,1);Put(" pickup L, ");
                pickUp(N);
                Put("P");Put(N,1);Put(" pickup R, ");
                pickUp(Succ(N));
            else
                Put("P");Put(N,1);Put(" pickup R, ");
                pickUp(Succ(N));
                Put("P");Put(N,1);Put(" pickup L, ");
                pickUp(N);
            end if;
            Put("P");Put(N,1);Put_Line(" eating.");
            --
            rndWait(G, N);
            decHunger(N);  --eating
            --
            Put("P");Put(N,1);Put(" released, ");
            release(N);release(Succ(N));
            Put("P");Put(N,1);Put_Line(" done,  now sleeping..  ");
        end loop;
    end;

    task body Phil_Type_base is
        G : Rnd.Generator;
        use IO;
    begin
        Put_Line("start of P"&N'Img);
        Rnd.Reset(G);
        loop
            rndWait(G, N);
            incHunger(N);
            -- try alternating hands
            Put("P");Put(N,1);Put(" pickup L, ");
            pickUp(N);
            Put("P");Put(N,1);Put(" pickup R, ");
            pickUp(Succ(N));
            Put("P");Put(N,1);Put_Line(" eating.");
            --
            rndWait(G, N);
            decHunger(N);  --  actual eating
            --
            Put("P");Put(N,1);Put(" released, ");
            release(N);release(Succ(N));
            Put("P");Put(N,1);Put_Line(" done,  now sleeping..  ");
        end loop;
    end;


    P0 : Phil_Type_base(0);
    P1 : Phil_Type_base(1);
    P2 : Phil_Type_base(2);
    P3 : Phil_Type_base(3);
    P4 : Phil_Type_base(4);

begin  -- main
    Put_Line("main program start");
    loop
        printState(Stick, Hunger);
        checkAlive(Hunger);
        delay MaxDuration;
    end loop;
exception
    when Finish =>
        -- nothing to do in main, but we need to abort started tasks..
        abort P0, P1, P2, P3, P4;
end phil0;
