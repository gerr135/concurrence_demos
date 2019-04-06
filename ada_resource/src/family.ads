--
-- A package containinf family members definitions (tasks) and flow control.
-- Copyright (C) 2018  George Shapovalov <gshapovalov@gmail.com>
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
with Ada.Numerics.Discrete_Random;


generic
    type Resource is range <>;
package family is

    Debug : Boolean := False;
    --  set this to True to get some reporting on the go..

    type ParentIdx is new Integer range 1 .. 2;
    type ChildIdx  is new Integer;

    type FamilyType(Nch : ChildIdx) is limited private;
    type FamilyAccess is access all FamilyType;
    
    task type Parent(N : ParentIdx; family : FamilyAccess);
    type ParentPtr is access Parent;
    task type Child (N : ChildIdx;  family : FamilyAccess);
    type ChildPtr is access Child;
    -- Both task types follow similar pattern - wait and put/get money (resource)
    -- the only difference is that parents are puuters, and children are getters.
    --
    -- All tasks are parametrizewd by
    type ParamRec is record
        T, dT : Duration; -- mean and delta interval between bank actions
        A, dA : Resource; -- mean and delta deposit/withdrawal amount
    end record;

    -- All tasks also access
    --  log  : LoggerInt; -- logger interface - use specific cmdline or GUI logger
    --  bank : BankInt    -- bank interface, there will be 2 implementations

    -- task parametrization
    type ParentParams is array (ParentIdx) of paramRec;
    type ChildParams  is array (ChildIdx range <>) of paramRec;
    type Params(Nch : ChildIdx) is record
        par : ParentParams;
        ch  : ChildParams(1 .. NCh);
    end record;


    function  Create_and_start(NCh : ChildIdx; pp : Params) return FamilyAccess;
    procedure Stop_and_Destroy(family : in out FamilyAccess);


private

    -- task termination synchronization
    -- this is a slight mod of a classic barrier:
    --   child tasks only tick-off, but are not blocked (they terminate afterwards anyway)
    --   the master is waiting, but does not enter the barrier itself..
    protected type TerminationCounter is
        procedure tickOff; -- by terminating task
        procedure reset(NTasks : Positive);   -- by master upon task initialization
        entry wait;  -- master for termination of all tasks
    private
        NActive : Natural := 0; -- set to N total tasks when all are started, counts down to 0 to release master
    end;
        
    
    type ParentArray is array(ParentIdx) of ParentPtr;
    type ChildArray  is array(ChildIdx range <>) of ChildPtr;
    
    type FamilyType(NCh : ChildIdx) is limited record
        parent : ParentArray;
        child  : ChildArray (1 .. NCh);
        -- NOTE: these vars below are only written to by Family, with only reads by individual members
        -- if Parents/Children ever need to modify these values, then this will have to be 
        -- converted to a protected type
        pp : Params(NCh);
        running : Boolean;
        --
        TC : TerminationCounter;
    end record;


    --  common actions and utility
    package RndT renames Ada.Numerics.Float_Random;
    procedure rndWait(G : RndT.Generator; T, dT : Duration);

    package RndA is new Ada.Numerics.Discrete_Random(Resource);
    function  rndAmount(G : RndA.Generator; A, dA : Resource) return Resource;

    
end family;
