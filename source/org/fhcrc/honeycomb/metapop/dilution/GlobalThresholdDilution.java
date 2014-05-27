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

import java.util.List;
import java.util.Map;
import java.util.HashMap;

/** Dilution occurs globally if a population hits a maximum size.
 *
 * Created on 26 Apr, 2013
 * @author Adam Waite
 * @version $Rev: 2393 $, $Date: 2014-05-24 19:17:59 -0400 (Sat, 24 May 2014) $, $Author: ajwaite $
 */
public class GlobalThresholdDilution extends ThresholdDilution {
    public GlobalThresholdDilution(double fraction, double threshold) {
        super(fraction, threshold);
    }

    @Override
    public Map<Coordinate, Double> generate(List<Population> pops) {
        Map<Coordinate, Double> dilution_map = 
            new HashMap<Coordinate, Double>(pops.size());

        for (Population pop1:pops) {
            if (pop1.getSize() >= threshold) {
                for (Population pop2:pops) {
                    dilution_map.put(pop2.getCoordinate(), fraction);
                }
                break;
            }
        }
        return dilution_map;
    }
}
