--
-- A holder for all global vars 
--
-- The intention here is to support cmdline and GUI versions with as little code duplication as possible.
-- while all this could be held by the top of a 
--

with logging;

package Globals is

    --  setup logging types, etc
    type Actor_Type  is (AParent,AChild,ABank);
    type Action_Type is (Init,Finish,Funds);

    type MessageRec(action : Action_Type) is record
        actor  : Actor_Type;
        case action is
            when Init | Finish => null;
            when Funds =>
                amount : Natural;
        end case;
    end record;
    
    function ToString(S : String) return String;
    package log is new logging(MessageRec);
    

end Globals;
