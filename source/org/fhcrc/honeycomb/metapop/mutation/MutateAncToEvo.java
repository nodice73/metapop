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

package org.fhcrc.honeycomb.metapop.mutation;

import org.fhcrc.honeycomb.metapop.Population;
import org.fhcrc.honeycomb.metapop.Subpopulation;
import org.fhcrc.honeycomb.metapop.RandomNumberUser;

import java.util.List;

/** 
 * Mutates cooperators to cheaters and cheaters to cooperators.
 *
 * Created on 27 May, 2013
 *
 * @author Adam Waite
 * @version $Rev: 2393 $, $Date: 2014-05-24 19:17:59 -0400 (Sat, 24 May 2014) $, $Author: ajwaite $
 *
 */
public class MutateAncToEvo implements MutationRule {
    private double anc_to_evo;
    private RandomNumberUser rng;

    /**
     * constructor
     */
    public MutateAncToEvo(double anc_to_evo, RandomNumberUser rng)
    {
        this.anc_to_evo = anc_to_evo;
        this.rng = rng;
    }

    public void mutate(List<Population> pops) {
        if (anc_to_evo == 0) {
            return;
        }

        for (Population pop:pops) {
            int new_coop_evo = 0;
            int new_cheat_evo = 0;

            Subpopulation anc_coop = pop.getSubpopById("anc_coop");
            Subpopulation evo_coop = pop.getSubpopById("evo_coop");
            Subpopulation anc_cheat = pop.getSubpopById("anc_cheat");
            Subpopulation evo_cheat = pop.getSubpopById("evo_cheat");

            new_coop_evo = rng.getNextBinomial(
                    anc_coop.getSize(), anc_to_evo);
            new_cheat_evo = rng.getNextBinomial(
                    anc_cheat.getSize(), anc_to_evo);

            anc_coop.setSize(anc_coop.getSize() - new_coop_evo);
            evo_coop.setSize(evo_coop.getSize() + new_coop_evo);
            anc_cheat.setSize(anc_cheat.getSize() - new_cheat_evo);
            evo_cheat.setSize(evo_cheat.getSize() + new_cheat_evo);
        }
    }

    @Override
    public String toString() {
        return String.format("%s, anc_to_evo=%.2e", 
                             this.getClass().getSimpleName(), anc_to_evo);
    }
}
