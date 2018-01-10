package interpreter;

import java.util.Vector;

import data.csd.Weapon;
import data.map.Map;
import utils.Tracer;

public class MapInterpreter extends Interpreter{
	private Map myMap;
	
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
		}
	}
	
}
