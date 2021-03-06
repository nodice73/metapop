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

/** 
 * Stops if all cooperators go extinct.
 *
 * Created on 22 Oct, 2014
 * @author Adam Waite
 *
 */
public class CoopExtinctStop extends StopCondition {

    public boolean isMet() {
        boolean coops_extinct = false;

        if ((world.getSizeById("anc_coop") == 0 &&
             world.getSizeById("evo_coop") == 0))
        {
            System.out.println("coops extinct at step " + world.getStep());
            coops_extinct = true;
        }

        return coops_extinct;
    }

    @Override
    public String toString() {
        return this.getClass().getSimpleName();
    }
}
