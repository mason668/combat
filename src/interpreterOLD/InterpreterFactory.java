package interpreterOLD;

import data.csd.Platform;
import data.csd.ProbabilityTable;
import data.csd.Sensor;
import data.csd.Weapon;

public class InterpreterFactory {
	public static void main(String[] args){
		System.out.println("InterpreterFactory");
		InterpreterFactory me = new InterpreterFactory();
		Object o = new Object();
		InterpreterOld i = me.getInterpreter(o);
		System.out.println("made "+ i.getClass().getCanonicalName());
		Platform p = new Platform();
		i = me.getInterpreter(p);
		System.out.println("made "+ i.getClass().getCanonicalName());
	}
	
	public InterpreterOld getInterpreter(Object o){
		return new InterpreterOld();
	}

	public InterpreterOld getInterpreter(Platform p){
		return new PlatformInterpreter();
	}

	public InterpreterOld getInterpreter(Sensor s){
		return new InterpreterOld();
	}

	public InterpreterOld getInterpreter(Weapon w){
		return new InterpreterOld();
	}

	public InterpreterOld getInterpreter(ProbabilityTable t){
		return new InterpreterOld();
	}

}
