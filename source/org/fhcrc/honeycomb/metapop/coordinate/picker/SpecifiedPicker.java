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

package org.fhcrc.honeycomb.metapop.coordinate.picker;

import org.fhcrc.honeycomb.metapop.coordinate.Coordinate;

import java.util.Queue;
import java.util.LinkedList;
import java.util.List;
import java.util.ArrayList;

/** 
 * Picks from a list of specified locations.
 *
 * Created on 11 Apr, 2013.
 * @author Adam Waite
 * @version $Rev: 2393 $, $Date: 2014-05-24 19:17:59 -0400 (Sat, 24 May 2014) $, $Author: ajwaite $
 *
 * @see Coordinate
 *
 */
public class SpecifiedPicker extends CoordinatePicker {
    private Queue<Coordinate> coords;

    public SpecifiedPicker(final List<Coordinate> coords) {
        super();
        if (coords == null) {
            throw new IllegalArgumentException("Coordinate list is null!");
        }

        this.coords = new LinkedList<Coordinate>();
        for (Coordinate coord : coords) {
            this.coords.add(new Coordinate(coord));
        }
    }

    @Override
    public Coordinate pick() {
        return coords.remove();
    }

    @Override
    public List<Coordinate> pick(int n) {
        List<Coordinate> ret = new ArrayList<Coordinate>(n);
        for (int i=0; i<n; i++) ret.add(coords.remove());
        return ret;
    }

    @Override
    public String getType() { return "specified"; }

    @Override
    public String toString() {
        return this.getClass().getSimpleName();
    }
}
