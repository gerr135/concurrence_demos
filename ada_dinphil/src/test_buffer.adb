-- The main units were producing a weird lock/logging underrun.
-- This is a simple code to test the logging itself..

with Ada.Text_IO;use Ada.Text_IO;
with test_buf_pkg; use test_buf_pkg;
procedure test_buffer is


    Buffer : Buffer_Type;

    task Putter;
    task body Putter is
    begin
        Put_Line("Putter started");
        for i in Item_Type range 0 .. 12 loop
--             Put_Line("putting i="&i'Img);
            Buffer.Put(i);
        end loop;
    end;

--     task Getter;
--     task body Getter is
--         X : Item_Type;
--     begin
--         Put_Line("Getter started");
--         for i in 0 .. 25 loop
--             Put_Line("getting X..");
--             Buffer.Get(X);
--             Put_Line(",  got X="&X'Img);
--         end loop;
--     end;

--     X : Item_Type;
begin
    for i in 0 .. 12 loop
        Buffer.printItem;
--         Buffer.Get(X);
--         Put_Line("got X="&X'Img);
    end loop;
    Null;
end test_buffer;
