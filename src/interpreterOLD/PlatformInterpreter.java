package interpreterOLD;

import java.util.Vector;

import data.csd.Platform;

public class PlatformInterpreter extends InterpreterOld{

	public static void main(String[] args){
		InterpreterOld me = new PlatformInterpreter();
		Platform o = new Platform();
		me.test(o);
	}
	
	protected void interpret (Platform o, String command, Vector<String> v){
		if (command.compareToIgnoreCase("two") == 0){
			System.out.println("*    " + command);
		} else if (command.compareToIgnoreCase("three") == 0){
			System.out.println("*    " + command);
		} else {
			super.doCommand(o, command, v);
		}
		if (v.isEmpty()) return;
		command = v.remove(0);
		this.interpret(o, command, v);
	}

}
