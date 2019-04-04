--
-- Common types, services and monitoring..
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

with Ada.Numerics.Float_Random;
with Ada.Text_IO;
with Ada.Calendar;

package Phil_Common is

    waitTick    : constant Duration := 0.0001;
    MaxDuration : constant Duration := 0.001;

    type PIndex is mod 5;

    type Action_Type is (Pickup,Putdwn,Eat,Think);
    type Hand_Type is   (Left, Right, None);

    type Stick_State is (Free,Taken);
    type Hand_State  is (Empty,Holding);
    type Phil_State is (Eating, Thinking, Waiting);
    type Hunger_State is (Fed, Hungry, Starved, Dead);

    Phil_Death : exception;

    type Stick_Interface is synchronized interface;
    procedure pickup (S : in out Stick_Interface) is abstract;
    procedure release(S : in out Stick_Interface) is abstract;
    function State(S : Stick_Interface) return Stick_State is abstract;
    function State(S : Stick_Interface'Class) return Character;

    type Stick_Access is access all Stick_Interface'Class;
    type Stick_Array is array(PIndex) of Stick_Access;
    type StickArray_Access is access all Stick_Array;

    procedure printSticks(SS : Stick_Array);

    ---  Actual Stick implementations
    protected type Prot_Stick_Type is new Stick_Interface with
        overriding entry     pickup;
        overriding procedure release;
        overriding function State return Stick_State;
    private
        S : Stick_State := Free;
    end;

    task type Task_Stick_Type is new Stick_Interface with
        overriding entry pickup;
        overriding entry release;
        entry GetState(S : out Stick_State);
    end;

    overriding
    function State(S : Task_Stick_Type) return Stick_State;


    ---  Event logging  ---------------------
    --  first the passed data
    type Event_Rec is private;
    procedure PrintEvent(ev : Event_Rec);
    --
    -- now the abstraction - we can try both a protected object and a task later..
    type EventLogger_Interface is synchronized interface;
    type EventLogger_Access is access all EventLogger_Interface'Class;
    procedure LogEvent(LI : in out EventLogger_Interface;
                       N : PIndex; action : Action_Type; hand : Hand_Type := None) is abstract;
    procedure GetEvent(LI : in out EventLogger_Interface; ev : out Event_Rec) is abstract;
    procedure PrintEvent(LI : in out EventLogger_Interface'Class);


    EventBufSize : constant := 50;
    type EventIndex is mod EventBufSize;
    maxBuf : constant EventIndex := EventIndex'Last;

    type EventArray is array (EventIndex) of Event_Rec;

    protected type EventLogger_Type is new EventLogger_Interface with
        overriding
        entry LogEvent(N : PIndex; action : Action_Type; hand : Hand_Type := None);
        overriding
        entry GetEvent(ev : out Event_Rec);
    private
        First, Last : EventIndex := 0;
        Events : EventArray;
    end;

    --- The logger daemon
    --  we neeed a separate task, otherwise we block monitoring (if we try to interleave with it in main body)
    task type Logger_Task(LB : EventLogger_Access; doPrinting : Boolean);
--     type Logger_Access is access Logger_Task;


    ----------------------------------------------
    ---  Monitoring -------------
    type State_Rec is limited private;
    -- we go here with 1 unit of hunger per MaxDuration;
    function  getHunger(H : State_Rec) return Hunger_State;
    procedure updateHunger(H : in out State_Rec);
    procedure satiate(H : in out State_Rec);

    type PhilState_Array is array (PIndex) of State_Rec;
    function  Reset_PhilState return PhilState_Array;
    procedure updateState(H : in out PhilState_Array);
    procedure printState (H : PhilState_Array);
    procedure printFullState(H : PhilState_Array; S : Stick_Array);
    procedure checkAlive(H : PhilState_Array);

    -----------------------------------
    --  Statistics
    type Stats_Rec is private;
    procedure updateStats(SR : in out Stats_Rec; H : PhilState_Array);
    procedure printStats (SR: Stats_Rec);

    type CombinedInfo_Rec is record
        State : PhilState_Array;
        LB    : aliased EventLogger_Type;
        Stats : Stats_Rec;
    end record;
    function Reset_CR return CombinedInfo_Rec;


    -----------------------------------
    ---  Finally the actions - to be used by philosopher tasks.
    --  Actions combine waiting, logging and actual stick and state changes..
    procedure pickupStick (who : PIndex; which : Hand_Type; S : in out Stick_Array;
                           CR : in out CombinedInfo_Rec);

    procedure releaseStick(who : PIndex; which : Hand_Type; S : in out Stick_Array;
                           CR : in out CombinedInfo_Rec);

    procedure Eat  (who : PIndex; CR : in out CombinedInfo_Rec);

    procedure Think(who : PIndex; CR : in out CombinedInfo_Rec);

    InvalidAction : exception;
    --  raised upon attempt to do an invalid thing, like use None hand, etc..


private

    protected IO_Lock is
        entry Set;
        entry Release;
    private
        Lock : Boolean := False;
    end;

    package Index_IO is new Ada.Text_IO.Modular_IO(PIndex);

    package Rnd renames Ada.Numerics.Float_Random;
    procedure rndWait(G : Rnd.Generator);

    type Event_Rec is record
        NPhil  : PIndex;
        action : Action_Type;
        Hand   : Hand_Type;
    end record;

    type State_Rec  is record
        curHunger : Natural := 0;
        lastEaten : Ada.Calendar.Time := Ada.Calendar.Clock;
        state : Phil_State := Thinking;
        left, right : Hand_State := Empty;
        G : Rnd.Generator;
    end record;

    type waitArray is array (PIndex) of Natural;
    type ActionArray is array (Action_Type) of Natural;

    type Stats_Rec is record
        StartTime : Ada.Calendar.Time := Ada.Calendar.Clock;
        NWaits : waitArray   := (others=>0);
        NActs  : ActionArray := (others=>0);
        iter   : Natural := 0; -- assume periodic calls, perhaps each MaxDuration
    end record;


end Phil_Common;
