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
import org.fhcrc.honeycomb.metapop.StepProvider;

import java.util.Queue;
import java.util.List;
import java.util.LinkedList;

/** 
 * Changes the environment at the specified intervals.
 *
 * Created on 26 Apr, 2013
 * @author Adam Waite
 * @version $Rev: 2393 $, $Date: 2014-05-24 19:17:59 -0400 (Sat, 24 May 2014) $, $Author: ajwaite $
 */
public class SpecifiedEnvironmentChanger implements EnvironmentChanger {
    private StepProvider step_provider;
    private Queue<Integer> change_at;

    public SpecifiedEnvironmentChanger(StepProvider sp,
                                       List<Integer> change_at)
    {
        if (change_at == null || change_at.size() == 0) {
            throw new IllegalArgumentException("List cannot be empty.");
        }
        this.step_provider = sp;
        this.change_at = new LinkedList<Integer>(change_at);
    }

    public SpecifiedEnvironmentChanger(List<Integer> change_at)
    {
        this(null, change_at);
    }

    @Override
    public boolean environmentChanged() {
        if (step_provider == null) {
            throw new UnsupportedOperationException("need a StepProvider");
        }
        return checkEnvironment(step_provider.getStep());
    }

    public boolean checkEnvironment(int step) {
        if (change_at.isEmpty()) {
            return false;
        } else if (step == change_at.peek()) {
            change_at.remove();
            return true;
        }
        return false;
    }

    // The environment will change, so return 1.
    @Override
    public double getProb() { return 1.0; }

    @Override
    public RandomNumberUser getRNG() { return null; }

    @Override
    public String toString() {
        return "SpecifiecEnvironmentChanger, " + "change at=" + change_at;
    }
}
