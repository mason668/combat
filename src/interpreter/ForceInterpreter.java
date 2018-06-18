package interpreter;

import java.util.Vector;

import sim.Constants;
import sim.forces.Force;
import utils.Logger;
import utils.Tracer;

public class ForceInterpreter extends Interpreter{
	private Force myForce;
	
	public void doCommand(Force force, String command, Vector<String> vector){
		if (force == null) return;
		myForce = force;
		doCommand(command, vector);
		myForce = null;
	}

	protected void doCommand (String command, Vector<String> vector){
		if (trace){
			Tracer.write(this.getClass().getName() + ": interpreting " + command + ":" + vector);
		}
		if (myForce == null) return;
		if (command.compareToIgnoreCase("") == 0){
		} else if (command.compareToIgnoreCase("hostility") == 0){
			if (vector.size()<2) return;
			String otherForce = vector.remove(0);
			String hostility = vector.remove(0);
			int hostilityValue = Constants.HOSTILITY_FRIEND;
			if (hostility.compareToIgnoreCase("enemy")== 0){
				hostilityValue = Constants.HOSTILITY_ENEMY;
			} else if (hostility.compareToIgnoreCase("neutral")== 0) {
				hostilityValue = Constants.HOSTILITY_NEUTRAL;
			}
			myForce.setHostility(otherForce, hostilityValue);
		} else if (command.compareToIgnoreCase("scan_cycle") == 0){
			String arg = vector.remove(0);
			try{
				double clock = Double.parseDouble(arg);
				myForce.setScanTime(clock);
			} catch (Exception e){
				Logger.err(this,0, "invalid scan_cycle");
			}
		} else if (command.compareToIgnoreCase("speed") == 0){
			if (vector.isEmpty()) return;
			String arg = vector.remove(0);
			try{
				double d = Double.parseDouble(arg);
				myForce.setSpeed(d);
			} catch (Exception e){
				Logger.err(this,0, "invalid force speed");
			}
		} else {
			Logger.err(this,Logger.WARNING, "invalid command " + command);
		}
	}
	
}
