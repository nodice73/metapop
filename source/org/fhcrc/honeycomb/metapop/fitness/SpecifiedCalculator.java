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

package org.fhcrc.honeycomb.metapop.fitness;

/**
 * Returns the value it was initialized with regardless of the resource.
 *
 * Created on 24 Apr, 2013
 * @author Adam Waite
 * @version $Rev: 2393 $, $Date: 2014-05-24 19:17:59 -0400 (Sat, 24 May 2014) $, $Author: ajwaite $
 */
public class SpecifiedCalculator extends FitnessCalculator {
    private double growth_rate;
    private double death_rate;

    public SpecifiedCalculator(double growth_rate, double death_rate) {
        this.growth_rate = growth_rate;
        this.death_rate = death_rate;
    }

    @Override
    public double getMaxGrowthRate() { return growth_rate; }

    @Override
    public double calculateGrowthRate(double param) { return growth_rate; }

    @Override
    public double calculateDeathRate(double param) { return death_rate; }
}
