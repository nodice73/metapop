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
public class MutateCoopCheat implements MutationRule {
    private double coop_to_cheat;
    private double cheat_to_coop;
    private RandomNumberUser rng;

    /**
     * constructor
     */
    public MutateCoopCheat(double coop_to_cheat, double cheat_to_coop,
                           RandomNumberUser rng)
    {
        this.coop_to_cheat = coop_to_cheat;
        this.cheat_to_coop = cheat_to_coop;
        this.rng = rng;
    }

    public void mutate(List<Population> pops) {
        if (coop_to_cheat != 0 || cheat_to_coop != 0) {
            for (Population pop:pops) {
                int anc_coop_muts = 0;
                int anc_cheat_muts = 0;
                int evo_coop_muts = 0;
                int evo_cheat_muts = 0;

                Subpopulation anc_coop = pop.getSubpopById("coop_0.450_10.0");
                Subpopulation evo_coop = pop.getSubpopById("coop_0.315_1.0");
                Subpopulation anc_cheat = pop.getSubpopById("cheat_0.540_10.0");
                Subpopulation evo_cheat = pop.getSubpopById("cheat_0.378_1.0");

                if (coop_to_cheat > 0.0) {
                    anc_coop_muts = rng.getNextBinomial(
                            anc_coop.getSize(), coop_to_cheat);
                    evo_coop_muts = rng.getNextBinomial(
                            evo_coop.getSize(), coop_to_cheat);
                }

                if (cheat_to_coop > 0.0) {
                    anc_cheat_muts = rng.getNextBinomial(
                            anc_cheat.getSize(), cheat_to_coop);
                    evo_cheat_muts = rng.getNextBinomial(
                            evo_cheat.getSize(), cheat_to_coop);
                }

                anc_coop.setSize(
                        anc_coop.getSize() - anc_coop_muts + anc_cheat_muts);
                evo_coop.setSize(
                        evo_coop.getSize() - evo_coop_muts + evo_cheat_muts);
                anc_cheat.setSize(
                        anc_cheat.getSize() - anc_cheat_muts + anc_coop_muts);
                evo_cheat.setSize(
                        evo_cheat.getSize() - evo_cheat_muts + evo_coop_muts);
            }
        }
    }

    @Override
    public String toString() {
        return String.format("%s, coop_to_cheat=%.2e, cheat_to_coop=%.2e",
                             this.getClass().getSimpleName(), coop_to_cheat,
                             cheat_to_coop);
    }
}
