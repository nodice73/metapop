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

package org.fhcrc.honeycomb.metapop.lookuptable;

import java.util.Map;
import java.util.HashMap;

/** 
 * Makes keys out of current resource and population sizes.
 *
 * Created on 05 Aug, 2013
 * @author Adam Waite
 * @version $Rev: 2393 $, $Date: 2014-05-24 19:17:59 -0400 (Sat, 24 May 2014) $, $Author: ajwaite $
 *
 */
public class ConcatLookupTable implements LookupTable {
    private Map<String, Double> map;

    public ConcatLookupTable() { this(100); }

    public ConcatLookupTable(int initial_capacity) {
        map = new HashMap<String, Double>(initial_capacity);
    }

    public double getValue(double ... keys) { 
        String key = concat(keys);
        Double val = map.get(key);
        if (val == null) {
            return -1.0;
        } else {
            return val;
        }
    }

    public void setValue(double val, double ... keys) {
        String key = concat(keys);
        map.put(key, val);
    }

    private String concat(double ... args) {
        StringBuilder x = new StringBuilder();
        for (double arg:args) { x.append("-").append(Double.toString(arg)); }
        return x.toString();
    }

    public void print() {
        for (Map.Entry<String, Double> entry : map.entrySet()) {
            System.out.println(entry.getKey() + "\t" + entry.getValue());
        }
    }
}
