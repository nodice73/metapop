/**
 * Copyright 2015 Adam Waite
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

package org.fhcrc.honeycomb.metapop.mutation;

import org.fhcrc.honeycomb.metapop.Population;
import org.fhcrc.honeycomb.metapop.Subpopulation;
import org.fhcrc.honeycomb.metapop.RandomNumberUser;
import org.fhcrc.honeycomb.metapop.mutation.MutateCoopToCheat;
import org.fhcrc.honeycomb.metapop.mutation.MutateAncToEvo;

import java.util.List;

/**
 * Combines cooperator to cheater and ancestor to evolved mutation
 * rules.
 *
 * Created on 14 May, 2015
 * @author Adam Waite
 */
public class MutateCoopToCheatAncToEvoDelay implements MutationRule {
    private double coop_to_cheat;
    private double cheat_to_coop;
    private double anc_to_evo;
    private double delay_hrs;
    private MutateAncToEvo anc_evo;
    private MutateCoopToCheat coop_cheat;
    private RandomNumberUser rng;

    /**
     * Constructor
     */
    public MutateCoopToCheatAncToEvoDelay(double coop_to_cheat,
                                     double cheat_to_coop,
                                     double anc_to_evo, 
                                     double delay_hrs, RandomNumberUser rng)
    {
        this.coop_to_cheat = coop_to_cheat;
        this.cheat_to_coop = cheat_to_coop;
        this.anc_to_evo = anc_to_evo;
        this.coop_cheat = new MutateCoopToCheat(coop_to_cheat, cheat_to_coop,
                                                rng);
        this.anc_evo = new MutateAncToEvo(anc_to_evo, rng);
        this.delay_hrs = delay_hrs;
    }

    public void mutate(World world) {
        pops = occupied_locations.getList()
        coop_cheat.mutate(pops);
        if world.getStep()/world.getTimestepScale() > delay_hrs {
            anc_evo.mutate(pops);
        }
    }

    @Override
    public String toString() {
        return String.format("%s, coop_to_cheat=%.2e, cheat_to_coop=%.2e, anc_to_evo=%.2e", 
                             this.getClass().getSimpleName(), coop_to_cheat, cheat_to_coop,
                             anc_to_evo);
    }
}
