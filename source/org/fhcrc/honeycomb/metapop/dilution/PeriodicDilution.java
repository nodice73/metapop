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

package org.fhcrc.honeycomb.metapop.dilution;

import org.fhcrc.honeycomb.metapop.coordinate.Coordinate;
import org.fhcrc.honeycomb.metapop.Population;
import org.fhcrc.honeycomb.metapop.StepProvider;

import java.util.List;
import java.util.Map;
import java.util.HashMap;

/** Dilution occurs every set number of timesteps.
 *
 * Created on 26 Apr, 2013
 * @author Adam Waite
 * @version $Rev: 2393 $, $Date: 2014-05-24 19:17:59 -0400 (Sat, 24 May 2014) $, $Author: ajwaite $
 *
 */
public class PeriodicDilution implements DilutionRule {
    private double fraction;
    private int every;
    private StepProvider step_provider;

    public PeriodicDilution(double fraction, int every,
                            StepProvider step_provider)
    {
        this.fraction = fraction;
        this.every = every;
        this.step_provider = step_provider;
    }

    public PeriodicDilution(double fraction, int every)
    {
        this(fraction, every, null);
    }

    @Override
    public Map<Coordinate, Double> generate(List<Population> pops) {
        Map<Coordinate, Double> dilution_map = 
            new HashMap<Coordinate, Double>(pops.size());

        int step = step_provider.getStep();

        if (step > 0 && step % every == 0) {
            for (Population pop:pops) {
                dilution_map.put(pop.getCoordinate(), fraction);
            }
        }
        return dilution_map;
    }

    @Override
    public void setStepProvider(StepProvider sp) {
        this.step_provider = sp;
    }

    @Override
    public String toString() {
        return super.toString() + ", fraction=" + fraction + 
               ", every=" + every;
                          
    }
}
