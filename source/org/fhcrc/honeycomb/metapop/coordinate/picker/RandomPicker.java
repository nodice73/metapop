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

import org.fhcrc.honeycomb.metapop.RandomNumberUser;
import org.fhcrc.honeycomb.metapop.coordinate.Coordinate;
import org.fhcrc.honeycomb.metapop.coordinate.CoordinateProvider;

import java.util.List;
import java.util.ArrayList;

/** 
 * Generates a random <code>Coordinate</code>.
 *
 * Created on 8 Feb, 2012
 * @author Adam Waite
 * @version $Rev: 2393 $, $Date: 2014-05-24 19:17:59 -0400 (Sat, 24 May 2014) $, $Author: ajwaite $
 *
 */
public class RandomPicker extends CoordinatePicker {

    public RandomPicker(int max_row, int max_col,
                        boolean exclude_current, 
                        CoordinateProvider provider, RandomNumberUser rng) 
    {
        super(max_row, max_col, exclude_current, provider, rng);
    }

    public RandomPicker(int max_row, int max_col, 
                        boolean exclude_current,
                        RandomNumberUser rng) 
    {
        this(max_row, max_col, exclude_current, null, rng);
    }


    public RandomPicker(int max_row, int max_col, RandomNumberUser rng) {
        this(max_row, max_col, false, rng);
    }


    @Override
    public Coordinate pick() {
        Coordinate coord = new Coordinate(rng.getNextInt(1, max_row),
                                          rng.getNextInt(1, max_col));
        if (exclude_current == true) {
            if (provider.getCoordinate() == null) {
                throw new NullPointerException(
                        "Can't exclude: current coordinate is null!");
            }

            Coordinate current = provider.getCoordinate();
            while (coord.equals(current)) {
                coord = new Coordinate(rng.getNextInt(1, max_row),
                                       rng.getNextInt(1, max_col));
            }
        }
        return coord;
    }

    @Override
    public List<Coordinate> pick(final int n) {
        List<Coordinate> coordinates = new ArrayList<Coordinate>(n);
        while (coordinates.size() < n) coordinates.add(pick());
        return coordinates;
    }

    @Override
    public String getType() {
        return "global";
    }
}
