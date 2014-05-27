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

package org.fhcrc.honeycomb.metapop.environment;

import org.fhcrc.honeycomb.metapop.RandomNumberUser;

/** 
 * Changes the environment as a Binomial process with a specified probability.
 *
 * Created on 26 Apr, 2013
 * @author Adam Waite
 * @version $Rev: 2393 $, $Date: 2014-05-24 19:17:59 -0400 (Sat, 24 May 2014) $, $Author: ajwaite $
 */
public class BinomialEnvironmentChanger implements EnvironmentChanger {
    private double prob;
    private RandomNumberUser rng;

    /**
     * Constructor.
     * @param prob the probability that the environment changes.
     * @param rng  the {@link RandomNumberUser}.
     *
     */
    public BinomialEnvironmentChanger(double prob, RandomNumberUser rng) {
        this.prob = prob;
        this.rng = rng;
    }

    @Override
    public boolean environmentChanged() {
        return rng.getNextBinomial(1, prob) == 1 ? true : false;
    }

    @Override
    public double getProb() {
        return prob;
    }

    @Override
    public RandomNumberUser getRNG() {
        return rng;
    }

    @Override
    public String toString() {
        return "BinomialEnvironmentChanger, prob=" + prob + 
               ", seed=" + getRNG();
    }
}
