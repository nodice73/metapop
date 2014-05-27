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

package org.fhcrc.honeycomb.metapop.experiment;

import org.fhcrc.honeycomb.metapop.dilution.DilutionRule;
import org.fhcrc.honeycomb.metapop.dilution.GlobalThresholdDilution;

/** 
 * Implements AdaptiveRace with global dilution
 * Created on 24 Jun, 2013.
 *
 * @author Adam Waite
 * @version $Rev: 2393 $, $Date: 2014-05-24 19:17:59 -0400 (Sat, 24 May 2014) $, $Author: ajwaite $
 *
 */
public class GlobalDilutionAR extends AdaptiveRace {

    public GlobalDilutionAR(String args[]) { super(args); }
    
    @Override
    public DilutionRule makeDilutionRule() {
        double dilution_fraction = 0.5;
        double threshold = 1e7;
        return new GlobalThresholdDilution(dilution_fraction, threshold);
    }

    public static void main(String args[]) {
        AdaptiveRace ar = new GlobalDilutionAR(args);
        ar.run();
    }
}
