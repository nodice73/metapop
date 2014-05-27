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

import java.util.Random;
import java.util.List;
import java.util.ArrayList;
import java.util.Set;
import java.util.HashSet;

/** 
 * Generates unique, random {@link Coordinate}s.
 *
 * Created on 8 Feb, 2012
 * @author Adam Waite
 * @version $Rev: 2393 $, $Date: 2014-05-24 19:17:59 -0400 (Sat, 24 May 2014) $, $Author: ajwaite $
 *
 */
public class UniqueRandomPicker extends RandomPicker {
    private Set<Coordinate> picked = new HashSet<Coordinate>();

    public UniqueRandomPicker(int max_rows, int max_cols, 
                              boolean exclude_current,
                              CoordinateProvider provider,
                              RandomNumberUser rng)
    {
        super(max_rows, max_cols, exclude_current, provider, rng);
    }

    public UniqueRandomPicker(int max_rows, int max_cols, 
                              CoordinateProvider provider,
                              RandomNumberUser rng)
    {
        this(max_rows, max_cols, false, provider, rng);
    }

    public UniqueRandomPicker(int max_rows, int max_cols, 
                              RandomNumberUser rng)
    {
        this(max_rows, max_cols, false, null, rng);
    }

    @Override
    public Coordinate pick() { 
        Coordinate coord;
        do {
            coord = super.pick();
        } while (picked.contains(coord));
        picked.add(coord);
        return coord;
    }

    @Override
    public List<Coordinate> pick(final int n) {
        int max_n = max_row * max_col;

        if (n > max_n) {
            throw new IllegalArgumentException(
                "Requested number of coordinates ("+ n +
                ") is larger than the number of coordinates (" + max_n + ")");
        }

        Set<Coordinate> coordinates = new HashSet<Coordinate>(n);
        while (coordinates.size() < n) coordinates.add(super.pick());
        return new ArrayList<Coordinate>(coordinates);
    }

    @Override
    public String getType() {
        return "global";
    }
}
