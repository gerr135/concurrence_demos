--
-- <one line to give the program's name and a brief idea of what it does.>
-- Copyright (C) 2017  George Shapovalov <gshapovalov@gmail.com>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.
--

with Ada.Integer_Text_IO, Ada.Float_Text_IO;

package body Phil_Common is

    procedure rndWait(G : Rnd.Generator) is
        dur : Duration := MaxDuration*Duration(Rnd.Random(G));
    begin
        --Put_Line("P"&N'Img&" waiting "&dur'Img);
        delay dur;
    end;

    protected body IO_Lock is
        entry Set when not Lock is
        begin
            Lock := True;
        end;
        entry Release when Lock is
        begin
            Lock := False;
        end;
    end;

    ---  Sticks  ------------------------
    function State(S : Stick_Interface'Class) return Character is
    begin
        return (if S.State = Taken then '|' else '_');
    end;

    protected body Prot_Stick_Type is
        entry pickup when S = Free is
        begin
            S := Taken;
        end;
        --
        procedure release is
        begin
            S := Free;
        end;
        --
        function State return Stick_State is
        begin
            return S;
        end;
    end;

    task body Task_Stick_Type is
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

    function State(S : Task_Stick_Type) return Stick_State is
        X : Stick_State;
    begin
        S.GetState(X);
        return X;
    end;


    ---  Hunger  -----------------------------------------
    function getHunger(H : State_Rec) return Hunger_State is
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

    procedure updateHunger(H : in out State_Rec) is
        use Ada.Calendar;
        durSinceEaten : Duration := Clock - H.lastEaten;
    begin
        H.curHunger := Natural(durSinceEaten/MaxDuration);
    end;

    procedure satiate(H : in out State_Rec) is
        use Ada.Calendar;
    begin
        H.curHunger := 0;
        H.lastEaten := Clock;
    end;


    ----------------------------------------
    ---  Event logging
    --
    -- first the "big thing" - the output routine. DOne once for all class-wide
    procedure PrintEvent(ev : Event_Rec) is
        use Index_IO, Ada.Text_IO;
    begin
        IO_Lock.Set;
        Put("P");Put(ev.NPhil,1);
        case ev.action is
            when pickup => Put("_pick_"&(case ev.hand is when Left=>"L  ", when Right=>"R  ", when None=>"N "));
            when putDwn => Put("_rels_"&(case ev.hand is when Left=>"L  ", when Right=>"R  ", when None=>"N "));
            when Eat   => Put_Line("_eating");
            when Think => Put_Line("_thinking");
        end case;
        IO_Lock.Release;
    end;

    procedure PrintEvent(LI : in out EventLogger_Interface'Class) is
        ev : Event_Rec;
    begin
--         Ada.Text_IO.Put_Line("LI.PrintEvent called");
--         delay 0.0;
        LI.GetEvent(ev);
--         Ada.Text_IO.Put_Line("LI.PrintEvent got event N="&ev.NPhil'Img & " action="&ev.action'Img);
        PrintEvent(ev);
    end;

    protected body EventLogger_Type is
        entry LogEvent(N : PIndex; action : Action_Type; hand : Hand_Type := None)
        when Last - First < maxBuf is
        begin
--             Ada.Text_IO.Put_Line("logEvent N="&N'Img&" action="&action'Img &
--                                  ";   First="&First'Img & ", Last="&Last'Img);
            Events(Last) := (N, action, hand);
            Last  := Last + 1;
        end;
        --
        entry GetEvent(ev : out Event_Rec) when Last - First > 0 is
        begin
--             Ada.Text_IO.Put_Line("getEvent N="&ev.NPhil'Img&" action="&ev.action'Img);
            ev := Events(First);
            First := First + 1;
        end;
    end;


    task body Logger_Task is
        use Ada.Text_IO;
        ev : Event_Rec;
    begin
        Put_Line("Log printing started. Actual output = " & doPrinting'Img);
        if doPrinting then
            loop
                LB.PrintEvent;
            end loop;
        else
            loop
                LB.GetEvent(ev);  -- just discard logs
            end loop;
        end if;
    end;



    ----------------------------------------
    --  State monitoring
    procedure updateState(H : in out PhilState_Array) is
    begin
        -- just the hunger for now
        for i in PIndex loop
            updateHunger(H(i));
        end loop;
    end;

    procedure printState(H : PhilState_Array) is
        use Ada.Text_IO, Ada.Integer_Text_IO, Index_IO;
    begin
        Put(" Philosophers: [");
        for i in PIndex loop
            Put(case H(i).state is when Eating=>"E", when Thinking=>"T", when Waiting=>"W");
        end loop;
        Put("];    hand state: [");
        for i in 0 .. PIndex'Last - 1 loop
            Put(case H(i).left  is when Empty=>"_", when Holding=>"^");
            Put(i,1);
            Put(case H(i).right is when Empty=>"_", when Holding=>"^");
            Put("  ");
        end loop;
        Put(case H(PIndex'Last).left  is when Empty=>"_", when Holding=>"^");
        Put(PIndex'Last,1);
        Put(case H(PIndex'Last).right is when Empty=>"_", when Holding=>"^");
        Put("];    hunger: [");
        for i in 0 .. PIndex'Last - 1 loop
            Put(H(i).curHunger, 2);Put(",");
        end loop;
        Put(H(PIndex'Last).curHunger, 2);
        Put_Line("];");
    end;

    procedure printSticks(SS : Stick_Array) is
        use Ada.Text_IO, Ada.Integer_Text_IO, Index_IO;
    begin
        New_Line;
        Put("Sticks: [");
        for i in PIndex loop
            Put(SS(i).State);
        end loop;
        Put("];   ");
    end;

    procedure printFullState(H : PhilState_Array; S : Stick_Array) is
    begin
        IO_Lock.Set;
        printSticks(S);
        printState (H);
        IO_Lock.Release;
    end;


    ---------------------------------------
    --  now finally actions
    procedure pickupStick (who : PIndex; which : Hand_Type; S : in out Stick_Array;
                           CR : in out CombinedInfo_Rec) is
    begin
        CR.LB.logEvent(who,pickup,which);
        CR.State(who).state := Waiting;
        case which is
            when Left  =>
                S(who).pickUp;
                CR.State(who).left  := Holding;
            when Right =>
                S(who+1).pickUp;
                CR.State(who).right := Holding;
            when None  =>
                raise InvalidAction;
        end case;
        CR.Stats.NActs(pickup) := CR.Stats.NActs(pickup) + 1;
    end;

    procedure releaseStick(who : PIndex; which : Hand_Type; S : in out Stick_Array;
                           CR : in out CombinedInfo_Rec) is
    begin
        CR.LB.logEvent(who,Putdwn,which);
        CR.State(who).state := Waiting;
        case which is
            when Left  =>
                S(who).release;
                CR.State(who).left  := Empty;
            when Right =>
                S(who+1).release;
                CR.State(who).right := Empty;
            when None  =>
                raise InvalidAction;
        end case;
        CR.Stats.NActs(Putdwn) := CR.Stats.NActs(Putdwn) + 1;
    end;

    procedure Eat  (who : PIndex; CR : in out CombinedInfo_Rec) is
    begin
        CR.LB.logEvent(who,Eat,None);
        CR.State(who).state := Eating;
        rndWait(CR.State(who).G);
        satiate(CR.State(who));
        CR.Stats.NActs(Eat) := CR.Stats.NActs(Eat) + 1;
    end;

    procedure Think(who : PIndex; CR : in out CombinedInfo_Rec) is
    begin
        CR.LB.logEvent(who,Think,None);
        CR.State(who).state := Thinking;
        rndWait(CR.State(who).G);
        updateHunger(CR.State(who));
        CR.Stats.NActs(Think) := CR.Stats.NActs(Think) + 1;
    end;


    --------------------
    --  statistics
    procedure updateStats(SR : in out Stats_Rec; H : PhilState_Array) is
        nw : PIndex := 0;
    begin
        SR.iter := SR.iter + 1;
        for i in PIndex loop
            if H(i).state = Waiting then
                nw := nw + 1;
            end if;
        end loop;
        SR.NWaits(nw) := SR.NWaits(nw) + 1;
    end;

    procedure printStats (SR: Stats_Rec) is
        use Ada.Text_IO, Ada.Float_Text_IO, Ada.Calendar;
    begin
        IO_Lock.Set;
        Put("!!Stats!!  ");
        Put("iter="&SR.iter'Img);
        Put(", tick expansion=");
        Put(Float(Clock-SR.StartTime)/Float(SR.iter)/Float(MaxDuration), 1,3,0);
        Put(", ops/tick (");
        Put("P=");  Put(Float(SR.NActs(pickup))/Float(SR.iter), 1,3,0);
        Put(", R=");Put(Float(SR.NActs(Putdwn))/Float(SR.iter), 1,3,0);
        Put(", E=");Put(Float(SR.NActs(Eat))/Float(SR.iter), 1,3,0);
        Put(", T=");Put(Float(SR.NActs(Think))/Float(SR.iter), 1,3,0);
        Put("),  Nwaits: [");
        for i in 0 .. PIndex'Last - 1 loop
            Put(Float(SR.NWaits(i))/Float(SR.iter),1,3,0);
            Put(", ");
        end loop;
        Put(Float(SR.NWaits(PIndex'Last))/Float(SR.iter),1,3,0); Put_Line("]");
        IO_Lock.Release;
    end;

    --  monitoring services
    procedure checkAlive(H : PhilState_Array) is
        use Index_IO, Ada.Text_IO;
        starving : Boolean := False;
    begin
        for i in PIndex loop
            if getHunger(H(i)) = Hungry then
                starving := True;
                Put("P");Put(i,1);Put(" is hungry, ");
            elsif getHunger(H(i)) = Starved then
                starving := True;
                Put("P");Put(i,1);Put(" is starved,  ");
            elsif getHunger(H(i)) = Dead then
                New_Line;
                Put("P");Put(i,1);Put_Line(" is Dead!!  Aborting.. ");
                raise Phil_Death;
            end if;
        end loop;
        if starving then New_Line; end if;
    end;

    ---  Constructors
    --  all values are set in the record definitions already, but we need to reset random generators..
    function  Reset_PhilState return PhilState_Array is
    begin
        return SA : PhilState_Array do
            for i in PIndex loop
                Rnd.Reset(SA(i).G);
            end loop;
        end return;
    end;

    function Reset_CR return CombinedInfo_Rec is
    begin
        return CR : CombinedInfo_Rec do
            for i in PIndex loop
                Rnd.Reset(CR.State(i).G);
            end loop;
        end return;
    end;


end Phil_Common;
