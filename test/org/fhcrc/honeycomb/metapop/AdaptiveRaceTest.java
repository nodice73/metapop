package org.fhcrc.honeycomb.metapop;

import org.fhcrc.honeycomb.metapop.experiment.GlobalDilutionAR;
import java.util.Arrays;

import org.junit.*;
import static org.junit.Assert.*;

public class AdaptiveRaceTest {

    @Test
    public void adaptiveRace() {
        String migration_range = "local";
        String migration_type = "indv";
        String initial_pop_size = "1.00e+05";
        String mutant_freq = "5.00e-01";
        String coop_release = "2.60e+00"; 
        String amount_needed = "5.50e+00";
        String coop_freq = "5.00e-01";
        String base_km = "1.00e+01";
        String cheat_adv = "1.20e+00";
        String evolved_km_adv = "1.00e+01";
        String evolved_death_adv = "2.00e+01";
        String evolved_vmax_tradeoff = "7.00e-01";
        String initial_resource = "0.00e+00";
        String row_col_size = "1.00e+01";
        String frac_occupied = "5.00e-01";
        String migration_rate = "1.00e-04";
        String coop_to_cheat_mutation_rate = "0.0";
        String cheat_to_coop_mutation_rate = "0.0";
        String anc_to_evo_mutation_rate = "0.0";
        String randomize = "False";
        String population_seed = "1370969005";
        String location_seed = "1370969006";
        String migration_seed = "1370969007";
        String mutation_seed = "1370969008";
        String env_change_seed = "1370969009";
        String hours = "1.00e+00";
        String save_every = "1.00e+00";
        String output_location = 
            "output_test/adaptive_race_test/";

        System.out.println("Running AdaptiveRaceTest\n");
        GlobalDilutionAR.main( new String [] {
            migration_range,
            migration_type,
            initial_pop_size,
            mutant_freq,
            coop_release,
            amount_needed,
            coop_freq,
            base_km,
            cheat_adv,
            evolved_km_adv,
            evolved_death_adv,
            evolved_vmax_tradeoff,
            initial_resource,
            row_col_size,
            frac_occupied,
            migration_rate,
            coop_to_cheat_mutation_rate,
            cheat_to_coop_mutation_rate,
            anc_to_evo_mutation_rate,
            randomize,
            population_seed,
            location_seed,
            migration_seed,
            mutation_seed,
            env_change_seed,
            hours,
            save_every,
            output_location
        });
    }
}
