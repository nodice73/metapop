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
 * Monod fitness with density-dependent death rate.
 *
 * Created 17 Nov, 2013
 * @author Adam Waite
 * @version $Rev: 2393 $ $Date: 2014-05-24 19:17:59 -0400 (Sat, 24 May 2014) $ $Author: ajwaite $
 */
public class MonodScaledDeath extends DensityDependentFitnessCalculator {
    private double vmax;
    private double km;
    private double d;
    private int    max_pop;

    public MonodScaledDeath(double vmax, double km, double d, int max_pop,
                            double scale)
    {
        this.vmax = (vmax+d)/scale;
        this.km   = km;
        this.d    = d/scale;
        this.max_pop = max_pop;
    }

    public MonodScaledDeath(double vmax, double km, double d, int max_pop) {
        this(vmax, km, d, max_pop, 1);
    }

    @Override
    public double calculateDeathRate(double nutrient_conc, double pop_size) {
        return (vmax-d)/max_pop * pop_size + d;
    }

    @Override
    public double getMaxGrowthRate() { return vmax; }

    @Override
    public double calculateGrowthRate(double nutrient_conc, double pop_size) {
        return (vmax*nutrient_conc) / (km+nutrient_conc);
    }
}
