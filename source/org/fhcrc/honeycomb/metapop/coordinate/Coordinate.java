/**
 * Copyright 2014 Adam Waite
 *
 * This file is part of metapop.
 *
 * metapop is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.  
 *
 * metapop is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with metapop.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.fhcrc.honeycomb.metapop.coordinate;

/**
 * Cartesian coordinates.
 *
 * Created on 10 Apr, 2013
 * @author Adam Waite
 * @version $Rev: 2393 $, $Date: 2014-05-24 19:17:59 -0400 (Sat, 24 May 2014) $, $Author: ajwaite $
 *
 */
public class Coordinate {
    final int row;
    final int col;

    public Coordinate(final int row, final int col) {
        this.row = row;
        this.col = col;
    }

    public Coordinate(Coordinate coord) { 
        this(coord.getRow(), coord.getCol());
    }

    public int getRow() { return row; }
    public int getCol() { return col; }

    @Override
    public String toString() { 
        return "row: "+row+" col: "+col;
    }

    @Override
    public boolean equals(Object obj) { 
        if (this == obj) return true;
        if (!(obj instanceof Coordinate)) return false;

        Coordinate c = (Coordinate) obj;
        return c.getRow() == row && c.getCol() == col;
    }

    @Override
    public int hashCode() {
        return (Integer.toString(row) + Integer.toString(col)).hashCode();
    }
}
