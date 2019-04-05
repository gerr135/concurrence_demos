--
-- Logging package. Some base hierarchy to support GUI later..
--
--  General design:
--  A common protected buffer to hold generated logs,
--  to be read by tasks that would either spit out contents to cmd line
--  or update gui elements..
-- 
-- This defines the interface and gives a basic Text_IO implementation.
--
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


generic
    type MessageRec is private;
    LogSize : Positive := 100;
package logging is

    type LogArray is array (Positive range <>) of MessageRec;
    
    protected LogBuffer is
        entry put(msg : in  MessageRec);
        entry get(msg : out MessageRec);
    private
        LB : LogArray(1 .. LogSize);
    end;
    

end logging;
