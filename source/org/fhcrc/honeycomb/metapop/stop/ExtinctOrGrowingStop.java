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

package org.fhcrc.honeycomb.metapop.stop;

import org.fhcrc.honeycomb.metapop.World;
import org.fhcrc.honeycomb.metapop.Population;

/** 
 * Stops when Populations are either extinct or have achieved some minimum
 * size.
 *
 * Created on 21 Jun, 2013
 * @author Adam Waite
 * @version $Rev: 2393 $, $Date: 2014-05-24 19:17:59 -0400 (Sat, 24 May 2014) $, $Author: ajwaite $
 *
 */
public class ExtinctOrGrowingStop extends StopCondition {
    private int min_pop_size;

    public ExtinctOrGrowingStop(int min_pop_size) {
        this.min_pop_size = min_pop_size;
    }

    public boolean isMet() {
        for (Population pop:world.getOccupiedLocations().getList()) {
            int size = pop.getSize();
            if (size < min_pop_size && size > 0) return false;
        }
        System.out.println("Extinct or growing at step " + world.getStep());
        return true;
    }
}
