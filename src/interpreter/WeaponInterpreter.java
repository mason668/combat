package interpreter;

import java.util.Vector;

import data.csd.Weapon;
import utils.Tracer;

public class WeaponInterpreter extends Interpreter{
	private Weapon myWeapon;
	
	public void doCommand(Weapon weapon, String command, Vector<String> vector){
		if (weapon == null) return;
		myWeapon = weapon;
		doCommand(command, vector);
		myWeapon = null;
	}

	protected void doCommand (String command, Vector<String> vector){
		if (trace){
			Tracer.write(this.getClass().getName() + ": interpreting " + command + ":" + vector);
		}
		if (myWeapon == null) return;
		if (command.compareToIgnoreCase("") == 0){
		}
	}
	
}
