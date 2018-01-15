package interpreter;

import java.io.BufferedReader;
import java.io.FileReader;
import java.util.StringTokenizer;
import java.util.Vector;

import data.map.Area;
import data.map.AreaType;
import data.map.Coordinate;
import data.map.Map;
import sim.entity.Entity;
import utils.Logger;
import utils.Tracer;

public class MapInterpreter extends Interpreter{
	private Map myMap;
	
	public static void main(String[] args){
		Tracer.setEcho(true);
		MapInterpreter me = new MapInterpreter();
		me.setTrace(true);
		Map map = new Map();
		me.setMap(map);
		me.test(args);
		map.trace();
	}
	
	public void setMap(Map map){
		if (map == null) return;
		myMap = map;
	}
	
	public void doCommand(Map map, String command, Vector<String> vector){
		if (map == null) return;
		myMap = map;
		doCommand(command, vector);
		myMap = null;
	}

	protected void doCommand (String command, Vector<String> vector){
		if (trace){
			Tracer.write(this.getClass().getName() + ": interpreting " + command + ":" + vector);
		}
		if (myMap == null) return;
		if (command.compareToIgnoreCase("") == 0){
		} else if (command.compareToIgnoreCase("area_type") == 0){
			if (vector.isEmpty()) return;
			String string = vector.remove(0);
			AreaType areaType = myMap.getAreaType(string);
			if (areaType == null){
				areaType = new AreaType(string);
				myMap.addAreaType(areaType);
			}
			this.doAreaType(areaType, vector);
		} else if (command.compareToIgnoreCase("areas") == 0){
			if (vector.isEmpty()) return;
			String fileName = vector.remove(0);
			Logger.err(Logger.INFO, "loading area file " + fileName);
			readAreas(fileName);
		} else if (command.compareToIgnoreCase("load") == 0){
			if (vector.isEmpty()) return;
			String fileName = vector.remove(0);
			Logger.err(Logger.INFO, "loading file " + fileName);
			read(fileName);
		} else if (command.compareToIgnoreCase("name") == 0){
			if (vector.isEmpty()) return;
			String string = vector.remove(0);
			myMap.setName(string);
		} else if (command.compareToIgnoreCase("xll") == 0){
			if (vector.isEmpty()) return;
			try{
				double d = Double.parseDouble(vector.remove(0)); 
				myMap.setXLL(d);
			} catch (Exception e){}
		} else if (command.compareToIgnoreCase("xur") == 0){
			if (vector.isEmpty()) return;
			try{
				double d = Double.parseDouble(vector.remove(0)); 
				myMap.setXUR(d);
			} catch (Exception e){}
		} else if (command.compareToIgnoreCase("yll") == 0){
			if (vector.isEmpty()) return;
			try{
				double d = Double.parseDouble(vector.remove(0)); 
				myMap.setYLL(d);
			} catch (Exception e){}
		} else if (command.compareToIgnoreCase("yur") == 0){
			if (vector.isEmpty()) return;
			try{
				double d = Double.parseDouble(vector.remove(0)); 
				myMap.setYUR(d);
			} catch (Exception e){}
		}
	}
	
	private void doAreaType(AreaType areaType, Vector<String> vector){
		if (vector.size()<=0) return;
		String command = vector.remove(0);
		if (command.compareToIgnoreCase("") == 0){
		} else if (command.compareToIgnoreCase("color")==0){
			if (vector.size()<3) return;
			try{
				String string = vector.remove(0);
				int red = Integer.parseInt(string);
				string = vector.remove(0);
				int green = Integer.parseInt(string);
				string = vector.remove(0);
				int blue = Integer.parseInt(string);
				if (trace){
					Tracer.write(this.getClass().getName() + "setting color to " + red + " " +
							green + " " + blue);
				}
				areaType.setColor(red, green, blue);
			} catch (Exception e){}
		}
	}
	
	protected void readAreas(String fileName){
		if (trace){
			Tracer.write("Interpreter: reading from area file " + fileName);
		}
    	BufferedReader b = null;
    	try {
    		b = new BufferedReader(new FileReader(fileName));
    		String record = null;
    		Area area = null;
    		int nodes = 0;
    		while ((record=b.readLine()) != null){
    			if (trace){
        			Tracer.write("read " + record);
    			}
    			if (record.compareTo("")==0 ) continue;
    			if (record.startsWith("#")) continue;
    			if (record.startsWith("!")) continue;
    			Vector<String> vector = new Vector<String>();
    			StringTokenizer tokenizer = new StringTokenizer(record);
    			while (tokenizer.hasMoreTokens()){
    				String word = tokenizer.nextToken();
    				vector.addElement(word);
    			}
    			if (area == null){
        			if (vector.size()>1){
        				String string = vector.remove(0);
        				String number = vector.remove(0);
        				try{
        					nodes = Integer.parseInt(number);
        				} catch (Exception e){}
            			AreaType areaType = myMap.getAreaType(string);
            			if (areaType != null){
                			if (trace){
                    			Tracer.write("new area " + areaType.getName() + " : " + nodes);
                			}
                			area = new Area(areaType);
                			this.myMap.addArea(area);
            			}
        			}
    			} else { // area != null
        			if (vector.size()>1){
        				String stringx = vector.remove(0);
        				String stringy = vector.remove(0);
        				try{
        					double x = Double.parseDouble(stringx);
        					double y = Double.parseDouble(stringy);
        					nodes--;
        					Coordinate c = new Coordinate(x,y);
        					area.addNode(c);
        					if (nodes <= 0) area=null;
        				} catch (Exception e){}
        			}
    			}
    			//this.interpret(record);
    		}
            b.close();
    	} catch (Exception e) {
    		Logger.err(Logger.WARNING, "Unable to open file " + fileName);
    	}
	}


	
}
