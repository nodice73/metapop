package org.fhcrc.honeycomb.metapop;

import org.fhcrc.honeycomb.metapop.Population;
import org.fhcrc.honeycomb.metapop.Subpopulation;

import org.fhcrc.honeycomb.metapop.coordinate.Coordinate;
import org.fhcrc.honeycomb.metapop.RandomNumberUser;
import org.fhcrc.honeycomb.metapop.fitness.FitnessCalculator;
import org.fhcrc.honeycomb.metapop.fitness.MonodCalculator;

import org.fhcrc.honeycomb.metapop.mutation.MutationRule;
import org.fhcrc.honeycomb.metapop.mutation.MutateCoopCheat;
import org.fhcrc.honeycomb.metapop.mutation.MutateEachGrowth;

import java.util.List;
import java.util.ArrayList;
import java.util.Arrays;

import org.junit.*;
import static org.junit.Assert.*;

/** 
 * Tests MutationRules.
 *
 * Created on 24 Apr, 2013
 * @author Adam Waite
 * @version $Id: MutationTest.java 2094 2013-05-31 01:48:48Z ajwaite $
 *
 */
public class MutationTest {
    private double conf = 0.95;

    private RandomNumberUser rng = new RandomNumberUser(1);
    private FitnessCalculator fc_anc_coop = new MonodCalculator(0.45, 10.0, 0.1, 200);
    private FitnessCalculator fc_anc_cheat = new MonodCalculator(0.54, 10.0, 0.083, 200);
    private FitnessCalculator fc_evo_coop = new MonodCalculator(0.315, 1.0, 0.05, 200);
    private FitnessCalculator fc_evo_cheat = new MonodCalculator(0.378, 1.0, 0.042, 200);
    private Coordinate coord = new Coordinate(1,1);

    private double resource = 0.0;
    private int large_subpop_size = 10000;
    private int small_subpop_size = 300;
    private String anc_coop_id = "coop_0.450_10.0";
    private String anc_cheat_id = "cheat_0.540_10.0";
    private String evo_coop_id = "coop_0.315_1.0";
    private String evo_cheat_id = "cheat_0.378_1.0";
    private Subpopulation anc_coop;
    private Subpopulation anc_cheat; 
    private Subpopulation evo_coop;
    private Subpopulation evo_cheat; 
    private List<Subpopulation> subpops;

    private Population pop;
    private List<Population> pops;

    private double [] mutation_rates =
        {0, 0.01, 0.1, 0.3, 0.5, 0.7, 0.9, 0.99, 1};

    @Before
    public void setUp() {
        anc_coop = new Subpopulation(large_subpop_size, fc_anc_coop, anc_coop_id, rng);
        anc_cheat = 
            new Subpopulation(large_subpop_size, fc_anc_cheat, anc_cheat_id, rng);
        evo_coop = new Subpopulation(small_subpop_size, fc_evo_coop, evo_coop_id, rng);
        evo_cheat = 
            new Subpopulation(small_subpop_size, fc_evo_cheat, evo_cheat_id, rng);
        subpops = 
            new ArrayList<Subpopulation>(Arrays.asList(anc_coop, anc_cheat,
                                                       evo_coop, evo_cheat));

        pop = new Population(subpops, coord, resource, rng);
    }

    @Test
    public void mutateCoopCheat() {
        Estimate expect_fail =
            IntegerEstimate.binomialEstimate(mutation_rates.length,
                                             1-conf, conf);
        int failed = 0;
        for (double coop_to_cheat:mutation_rates) {
            setUp();
            int coop_before = anc_coop.getSize();
            int cheat_before = anc_cheat.getSize();
            Estimate expected =
                IntegerEstimate.binomialEstimate(coop_before,
                                                 coop_to_cheat, conf);

            List<Population> pops =
                new ArrayList<Population>(Arrays.asList(pop));
            Population pop_copy = pops.get(0);
            List<Population> pops_new = 
                new ArrayList<Population>(Arrays.asList(pop));
            Population pop_copy_new = pops_new.get(0);

            MutationRule mutate_coop_cheat =
                new MutateCoopCheat(coop_to_cheat, 0, rng);

            mutate_coop_cheat.mutate(pops);

            int coop_after = pop_copy.getSubpopById("coop_0.450_10.0").getSize();
            int cheat_after = pop_copy.getSubpopById("cheat_0.540_10.0").getSize();
            int coop_change = Math.abs(coop_before-coop_after);
            int cheat_change = Math.abs(cheat_before-cheat_after);

            MutationRule mutate_each_growth = 
                new MutateEachGrowth(0, coop_to_cheat, 0, rng);

            mutate_each_growth.mutate(pops_new);

            int coop_after_new = pop_copy_new.getSubpopById("coop_0.450_10.0").getSize();
            int cheat_after_new = pop_copy_new.getSubpopById("cheat_0.540_10.0").getSize();
            int coop_change_new = Math.abs(coop_before-coop_after_new);
            int cheat_change_new = Math.abs(cheat_before-cheat_after_new);

            int coop_change_difference = Math.abs(coop_change - coop_change_new);
            int cheat_change_difference = Math.abs(cheat_change - cheat_change_new);

            assertEquals("exchange wrong", coop_change, cheat_change, 0.0);

            double[] ci = expected.getCI();
            if (coop_change < ci[0] || coop_change > ci[1] || 
                coop_change_new < ci[0] || coop_change_new > ci[1]) failed++;
            if (coop_change_difference != 0) System.out.println("Warning: discrepancy of " + coop_change_difference);
        }
        FailReport.report("[PopulationTest] mutateCoopCheat()", failed,
                          mutation_rates.length, expect_fail);
    }

    @Test
    public void mutateCheatCoop() {
        Estimate expect_fail =
            IntegerEstimate.binomialEstimate(mutation_rates.length,
                                             1-conf, conf);
        int failed = 0;
        for (double cheat_to_coop:mutation_rates) {
            setUp();
            int coop_before = anc_coop.getSize();
            int cheat_before = anc_cheat.getSize();
            Estimate expected =
                IntegerEstimate.binomialEstimate(coop_before,
                                                 cheat_to_coop, conf);

            List<Population> pops =
                new ArrayList<Population>(Arrays.asList(pop));
            Population pop_copy = pops.get(0);
            List<Population> pops_new = 
                new ArrayList<Population>(Arrays.asList(pop));
            Population pop_copy_new = pops_new.get(0);

            MutationRule mutate_coop_cheat =
                new MutateCoopCheat(0, cheat_to_coop, rng);

            mutate_coop_cheat.mutate(pops);

            int coop_after = pop_copy.getSubpopById("coop_0.450_10.0").getSize();
            int cheat_after = pop_copy.getSubpopById("cheat_0.540_10.0").getSize();
            int coop_change = Math.abs(coop_before-coop_after);
            int cheat_change = Math.abs(cheat_before-cheat_after);

            MutationRule mutate_cheats = 
                new MutateEachGrowth(0, 0, cheat_to_coop, rng);

            mutate_cheats.mutate(pops_new);

            int coop_after_new = pop_copy_new.getSubpopById("coop_0.450_10.0").getSize();
            int cheat_after_new = pop_copy_new.getSubpopById("cheat_0.540_10.0").getSize();
            int coop_change_new = Math.abs(coop_before-coop_after_new);
            int cheat_change_new = Math.abs(cheat_before-cheat_after_new);

            int coop_change_difference = Math.abs(coop_change - coop_change_new);
            int cheat_change_difference = Math.abs(cheat_change - cheat_change_new);

            assertEquals("exchange wrong", coop_change, cheat_change, 0.0);

            double[] ci = expected.getCI();
            if (cheat_change < ci[0] || cheat_change > ci[1] || 
                cheat_change_new < ci[0] || cheat_change_new > ci[1]) failed++;
            if (cheat_change_difference != 0) System.out.println("Warning: discrepancy of " + coop_change_difference);
        }
        FailReport.report("[PopulationTest] mutateCheatCoop()", failed,
                          mutation_rates.length, expect_fail);
    }
}
