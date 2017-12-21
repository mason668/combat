package interpreter;

import java.io.BufferedReader;
import java.io.FileReader;
import java.util.StringTokenizer;
import java.util.Vector;

import data.CSData;
import data.csd.PHTable;
import data.csd.PKTable;
import data.csd.Platform;
import data.csd.Weapon;
import sim.Constants;
import utils.Logger;

public class Interpreter2 extends Interpreter {
	
	/*
	public Interpreter2(Object object){
		super(object);
	}
	*/
	
	/*
	public void interpret(Object object, Vector<String> vector) {
		if (vector.isEmpty()) return;
		String command = vector.remove(0);
//		Logger.log(Logger.FINEST, "Interpreter: " + command);
		if (command.compareToIgnoreCase("") == 0){
		} else if (command.compareToIgnoreCase("load") == 0){
			if (vector.isEmpty()) return;
			String fileName = vector.remove(0);
			read(fileName);
		} else if (command.compareToIgnoreCase("new") == 0){
			if (!vector.isEmpty()){
				make (vector);
			}
		} else if (command.compareToIgnoreCase("platform") == 0){
			if (!vector.isEmpty()){
				platform(vector);
			}
		} else {
			Logger.log(Logger.FINEST, "invalid command " + command);				
		}
	}
	
	private void read(String fileName){
//		Logger.log(Logger.INFO,"FileLoader: reading from file " + fileName);
    	BufferedReader b = null;
    	try {
    		b = new BufferedReader(new FileReader(fileName));
    		String record = null;
    		while ((record=b.readLine()) != null){
    			if (record.compareTo("")==0 ) continue;
    			if (record.startsWith("#")) continue;
    			if (record.startsWith("!")) continue;
    			this.interpret(record);
    		}
            b.close();
    	} catch (Exception e) {
    		Logger.err(Logger.WARNING, "Unable to open file " + fileName);
    	}
	}

	private void make(Vector<String> vector){
		String command = vector.remove(0);
		if (vector.isEmpty()){
			return;
		}
		String name = vector.remove(0);
//		Logger.log(Logger.FINE, "Interpreter: attempting to make " + 
//				command + " " + name);
		// collect optional file name from vector
		String fileName = null;
		if ( ! vector.isEmpty()){
			fileName = vector.remove(0);
			Logger.log(Logger.FINEST, "filename " + fileName);
		}
		// interpret based on command
		if (command.compareToIgnoreCase("") == 0){
		} else if (command.compareToIgnoreCase("phtable") == 0){
			PHTable p = new PHTable (name);
//			if ( fileName != null) p.interpret(Parser.convert("load " + fileName));
			if (csdata != null){
				csdata.getPHTableList().add(p);
			}
		} else if (command.compareToIgnoreCase("pktable") == 0){
			PKTable p = new PKTable (name);
//			if ( fileName != null) p.interpret(Parser.convert("load " + fileName));
			if (csdata != null){
				csdata.getPKTableList().add(p);
			}
		} else if (command.compareToIgnoreCase("platform") == 0){
			Platform item = new Platform (name);
//			if ( fileName != null) item.interpret(Parser.convert("load " + fileName));
			if (csdata != null){
				csdata.getPlatformList().add(item);
			}
		} else if (command.compareToIgnoreCase("weapon") == 0){
			Weapon item = new Weapon (name);
//			if ( fileName != null) item.interpret(Parser.convert("load " + fileName));
			if (csdata != null){
				csdata.getWeaponList().add(item);
			}
		}
	}
	
	private void platform(Vector<String> vector){
		String name = vector.remove(0);
		Platform platform = csdata.getPlatformList().getPlatform(name);
		if ( platform == null) return;
		String command = vector.remove(0);
		if (vector.isEmpty()){
			return;
		}
		if (command.compareToIgnoreCase("") == 0){
		} else if (command.compareToIgnoreCase("load") == 0){
			if (vector.isEmpty()) return;
			String fileName = vector.remove(0);
			read(fileName);
		}
	}
*/

}
