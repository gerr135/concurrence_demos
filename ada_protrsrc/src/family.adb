with Ada.Unchecked_Deallocation;

with globals; use globals;

package body family is

    ------------
    -- Parent --
    ------------

    task body Parent is
        GT : RndT.Generator;
        GA : RndA.Generator;
        amount : Resource;
    begin
        RndT.Reset(GT);
        RndA.Reset(GA);
        log.LogBuffer.put((Init,AParent));
        while family.running loop
            rndWait(GT, family.pp.par(N).T, family.pp.par(N).dT);
            amount := rndAmount(GA, family.pp.par(N).A, family.pp.par(N).dA);
            log.LogBuffer.put(AParent,Funds);
            --  access bank
            pragma Compile_Time_Warning (Standard.True, "Parent unimplemented");
            raise Program_Error with "Unimplemented task Parent";
        end loop;
        family.TC.tickOff;
    end Parent;

    -----------
    -- Child --
    -----------

    task body Child is
        GT : RndT.Generator;
        GA : RndA.Generator;
        amount : Resource;
    begin
        RndT.Reset(GT);
        RndA.Reset(GA);
        while family.running loop
            rndWait(GT, family.pp.ch(N).T, family.pp.ch(N).dT);
            amount := rndAmount(GA, family.pp.ch(N).A, family.pp.ch(N).dA);
            --  access bank
            pragma Compile_Time_Warning (Standard.True, "Child unimplemented");
            raise Program_Error with "Unimplemented task Parent";
        end loop;
        -- all done, don;t forget to tickoff termination barrier
        -- this should be the last action before end..
        family.TC.tickOff;
    end Child;


    -------------------------------
    --  Family

    function  Create_and_start(NCh : ChildIdx; pp : Params) return FamilyAccess is
        fa : FamilyAccess := new FamilyType(NCh);
    begin
        -- first set the params, as tasks will start executing as soon as they are created
        fa.pp := pp;
        fa.running := True;
        -- now we are ready to start tasks
        fa.parent(1) := new Parent(1,fa);
        fa.parent(2) := new Parent(2,fa);
        for i in 1 .. NCh loop
            fa.child(i) := new Child(i, fa);
        end loop;
        -- reset counter of termination barrier
        fa.TC.reset(2 + Positive(NCh));
        return fa;
    end;

    procedure Stop_and_Destroy(family : in out FamilyAccess) is
        procedure Free is new Ada.Unchecked_Deallocation(FamilyType, FamilyAccess);
        procedure Free is new Ada.Unchecked_Deallocation(Parent, ParentPtr);
        procedure Free is new Ada.Unchecked_Deallocation(Child, ChildPtr);
    begin
        -- signal task termination
        family.running := False;
        -- wait for termination (as all tasks are dynamic with Ptr at library level,
        -- there is no specific block that waits for them implicitly).
        family.TC.wait;
        -- force task shutdown - TBD
        -- delete structures
        Free(family.parent(1));
        Free(family.parent(2));
        for i in 1 .. family.NCh loop
            Free(family.child(i));
        end loop;
        Free(family);
    end;


    -----------------
    -- utility

    procedure rndWait (G : RndT.Generator; T, dT : Duration) is
    begin
        if (T = 0.0) or (dT = 0.0) then
            delay T;
        else
            delay T - dT + 2*dT*Duration(RndT.Random(G));
        end if;
    end rndWait;

    function rndAmount (G : RndA.Generator; A, dA : Resource) return Resource is
    begin
        if (A = 0) or (dA = 0) then
            return A;
        else
            return A - dA + 2*dA*RndA.Random(G);
        end if;
    end rndAmount;



    ------------------------
    -- TerminationCounter --
    ------------------------

    protected body TerminationCounter is

        procedure tickOff is
        begin
            NActive := NActive - 1;
        end tickOff;

        procedure reset(NTasks : Positive) is
        begin
            NActive := NTasks;
        end reset;

        entry wait when NActive = 0 is
        begin
            Null; -- just a barrier to wait for all tasks to finish. All action is in the guard..
        end wait;

    end TerminationCounter;

end family;
