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

package org.fhcrc.honeycomb.metapop;

import java.util.Map;
import java.util.List;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

/**
 * Saves the state of {@code Saveable} objects.
 *
 * Created on 28 Apr, 2013.
 * @author Adam Waite
 * @version $Rev: 2393 $, $Date: 2014-05-24 19:17:59 -0400 (Sat, 24 May 2014) $, $Author: ajwaite $
 */
public class StateSaver {
    private final Saveable saveable;
    private final File data_path;
    private final Map<String, String> initialization_data;
    private final String headers;

    public StateSaver(Saveable saveable) {
        this.saveable = saveable;
        this.data_path = saveable.getDataPath();
        this.headers = saveable.getHeaders();
        this.initialization_data = saveable.getInitializationData();

        try {
            writeInitializationData();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public void writeInitializationData() throws IOException {
        if (!data_path.exists()) {
            boolean made = data_path.mkdirs();
            if (!made) {
                throw new IOException("mkdirs() failed!");
            }
        }
        String filename = initialization_data.get("filename");

        File write_to = new File(data_path, filename);
        BufferedWriter writer = null; 
        try {
            if (!write_to.exists()) {
                boolean created = write_to.createNewFile();
                if (!created) throw new IOException("Couldn't create file!");
            }
            writer = new BufferedWriter(new FileWriter(write_to));
            writer.write(initialization_data.get("info"));
            writer.close();
        } catch (IOException e) {
            System.out.println("Couldn't write to file " + 
                               write_to.toString());
            e.printStackTrace();
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            if (writer != null) {
                try {
                    writer.close();
                } catch (Exception e2) {
                }
            }
        }
    }

    public void saveState() throws IOException 
    {
        if (!data_path.exists()) {
            boolean made = data_path.mkdirs();
            if (!made) {
                throw new IOException("mkdirs() failed!");
            }
        }

        String file_name = saveable.getFilename();
        File write_to = new File(data_path, file_name);
        BufferedWriter writer = null; 
        try {
            if (!write_to.exists()) {
                boolean created = write_to.createNewFile();
                if (!created) throw new IOException("Couldn't create file!");
            }
            writer = new BufferedWriter(new FileWriter(write_to));
            writer.write(report());
            writer.close();
        } catch (IOException e) {
            System.out.println("Couldn't write to file " + 
                               write_to.toString());
            e.printStackTrace();
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            if (writer != null) {
                try {
                    writer.close();
                } catch (Exception e2) {
                }
            }
        }
    }

    public String report() {
        StringBuilder report = new StringBuilder();
        report.append(headers).append("\n").append(saveable.getData());
        return report.toString();
    }
}
