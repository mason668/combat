package interpreter;

import java.util.Vector;

import data.csd.Sensor;
import utils.Tracer;

public class SensorInterpreter extends Interpreter{
	private Sensor mySensor;
	
	public void doCommand(Sensor sensor, String command, Vector<String> vector){
		if (sensor == null) return;
		mySensor = sensor;
		doCommand(command, vector);
		mySensor = null;
	}

	protected void doCommand (String command, Vector<String> vector){
		if (trace){
			Tracer.write(this.getClass().getName() + ": interpreting " + command + ":" + vector);
		}
		if (mySensor == null) return;
		if (command.compareToIgnoreCase("") == 0){
		}
	}
	
}
