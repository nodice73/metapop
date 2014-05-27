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
 * Calculates fitness.
 *
 * Created on 17 Nov, 2013
 * @author Adam Waite
 * @version $Rev: 2372 $, $Date: 2013-11-03 21:39:44 -0500 (Sun, 03 Nov 2013) $, $Author: ajwaite $
 */
public abstract class DensityDependentFitnessCalculator {
    abstract public double getMaxGrowthRate();
    abstract public double calculateGrowthRate(double resource, 
                                               double pop_size);
    abstract public double calculateDeathRate(double resource, 
                                              double pop_size);

    @Override
    public String toString() {
        return this.getClass().getSimpleName();
    }
}
