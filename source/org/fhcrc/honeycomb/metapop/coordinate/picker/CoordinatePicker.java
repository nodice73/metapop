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

/** 
 * Generates <code>Coordinate</code>s for <code>Location</code>s.
 *
 * Created on 10 Apr, 2013
 * @author Adam Waite
 * @version $Rev: 2393 $, $Date: 2014-05-24 19:17:59 -0400 (Sat, 24 May 2014) $, $Author: ajwaite $
 *
 * @see Coordinate
 *
 */
public abstract class CoordinatePicker {
    protected int max_row;
    protected int max_col;
    protected boolean exclude_current;
    protected CoordinateProvider provider;
    protected RandomNumberUser rng;

    protected CoordinatePicker() {}

    protected CoordinatePicker(int max_row, int max_col,
                               boolean exclude_current,
                               CoordinateProvider provider,
                               RandomNumberUser rng)
    {
        this.max_row = max_row;
        this.max_col = max_col;
        this.exclude_current = exclude_current;
        this.rng = rng;

        if (provider != null) setProvider(provider);
    }

    public CoordinatePicker(int max_row, int max_col,
                            boolean exclude_current,
                            RandomNumberUser rng)
    {
        this(max_row, max_col, exclude_current, null, rng);
    }

    public void setProvider(CoordinateProvider provider) {
        this.provider = provider;
    }

    public CoordinateProvider getProvider() { return provider; }

    public RandomNumberUser getRNG() { return rng; }

    public abstract Coordinate pick();
    public abstract List<Coordinate> pick(int n);

    public abstract String getType();

    @Override
    public String toString() {
        return String.format("%s, exclude=%s, seed=%d",
                             this.getClass().getSimpleName(), exclude_current,
                             rng.getSeed());
    }
}
