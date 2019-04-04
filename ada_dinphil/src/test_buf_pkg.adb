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

with Ada.Text_IO;

package body test_buf_pkg is

    procedure printItem(I : Item_Type) is
    begin
        Ada.Text_IO.Put_Line(" got X="&I'Img);
    end;

    procedure PrintItem(Buf : in out Buffer_Interface'Class) is
        I : Item_Type;
    begin
        Buf.Get(I);
        printItem(I);
    end;

    protected body Buffer_Type is
        entry Put(X : in  Item_Type) when Last - First < maxCount is
        begin
            Ada.Text_IO.Put_Line("Put X="&X'Img & "; First="&First'Img&", Last="&Last'Img);
            buf(Last) := X;
            Last  := Last + 1;
        end;
        --
        entry Get(X : out Item_Type) when Last - First > 0 is
        begin
            X := buf(First);
            First := First + 1;
            Ada.Text_IO.Put_Line("Get X="&X'Img & "; First="&First'Img&", Last="&Last'Img);
        end;
    end;



end test_buf_pkg;
