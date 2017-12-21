package interpreter;

import java.util.Vector;

import data.CSData;
import data.csd.Platform;
import data.csd.Sensor;
import sim.Constants;
import utils.Logger;
import utils.Tracer;

public class PlatformInterpreter extends Interpreter{
	private Platform myPlatform;
	private CSData myData;
	
	public PlatformInterpreter(CSData data){
		super();
		myData = data;
	}
	
	public void doCommand(Platform platform, String command, Vector<String> vector){
		if (platform == null) return;
		myPlatform = platform;
		doCommand(command, vector);
		myPlatform = null;
	}

	protected void doCommand (String command, Vector<String> vector){
		if (trace){
			Tracer.write(this.getClass().getName() + ": interpreting " + command + ":" + vector);
		}
		if (myPlatform == null) return;
		if (command.compareToIgnoreCase("") == 0){
		} else if (command.compareToIgnoreCase("crawl_speed") == 0){
			if (vector.isEmpty()) return;
			try{
				double d = Double.parseDouble(vector.remove(0)); 
				myPlatform.setSpeedCrawling(d);
			} catch (Exception e){
				Logger.err(this,0, "invalid platform crawl speed " + myPlatform.getName());
			}
		} else if (command.compareToIgnoreCase("foot") == 0){
			myPlatform.setMoverType(Constants.MOVER_FOOT);
		} else if (command.compareToIgnoreCase("load") == 0){
			if (vector.isEmpty()) return;
			String fileName = vector.remove(0);
			read(fileName);
		} else if (command.compareToIgnoreCase("reverse_speed") == 0){
			if (vector.isEmpty()) return;
			try{
				double d = Double.parseDouble(vector.remove(0)); 
				myPlatform.setSpeedReverse(d);
			} catch (Exception e){
				Logger.err(this,0, "invalid platform reverse speed " + myPlatform.getName());
			}
		} else if (command.compareToIgnoreCase("road_speed") == 0){
			if (vector.isEmpty()) return;
			try{
				double d = Double.parseDouble(vector.remove(0)); 
				myPlatform.setSpeedRoad(d);
			} catch (Exception e){
				Logger.err(this,0, "invalid platform road_speed " + myPlatform.getName());
			}
		} else if (command.compareToIgnoreCase("run_speed") == 0){
			if (vector.isEmpty()) return;
			try{
				double d = Double.parseDouble(vector.remove(0)); 
				myPlatform.setSpeedRunning(d);
			} catch (Exception e){
				Logger.err(this,0, "invalid platform run speed " + myPlatform.getName());
			}
		} else if (command.compareToIgnoreCase("sensor") == 0){
			if (vector.isEmpty()) return;
			String sensorName = vector.remove(0);
			Sensor sensor = myData.getSensorList().getSensor(sensorName);
			if (sensor == null) return;
			myPlatform.addSensor(sensor);
		} else if (command.compareToIgnoreCase("speed") == 0){
			if (vector.isEmpty()) return;
			try{
				double d = Double.parseDouble(vector.remove(0)); 
				myPlatform.setSpeedCountry(d);
			} catch (Exception e){
				Logger.err(this,0, "invalid platform speed " + myPlatform.getName());
			}
		} else if (command.compareToIgnoreCase("suppression_factor_move")==0){
			if (vector.isEmpty()) return;
			try{
				double d = Double.parseDouble(vector.remove(0)); 
				myPlatform.setSuppressionFactorMove(d);
			} catch (Exception e){
				Logger.err(this,0, "invalid suppression speed");
			}
		} else if (command.compareToIgnoreCase("track") == 0){
			myPlatform.setMoverType(Constants.MOVER_TRACK);
		} else if (command.compareToIgnoreCase("wheel") == 0){
			myPlatform.setMoverType(Constants.MOVER_WHEEL);
		} else {
			Logger.err(this,Logger.WARNING, "invalid command " + command);
		}
	}
	
}
