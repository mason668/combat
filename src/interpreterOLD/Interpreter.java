package interpreterOLD;

import java.util.Vector;

import data.CSData;
import data.csd.PHTable;
import data.csd.PKTable;
import data.csd.Platform;
import data.csd.Weapon;
import utils.Logger;
import utils.Parser;

public class Interpreter implements InterpreterInterface {
	
	private CSData csdata;

	public static void main(String args[]){
//		Logger.setEcho(false);
		Interpreter me = new Interpreter();
		me.test();
	}
	
	public void setData(CSData data){
		csdata = data;
	}
	
	public void test(){
		Logger.log(Logger.FINEST,"testing class " + this.getClass().getCanonicalName());
		csdata = new CSData();
		this.interpret(Parser.convert(""
				+ "load interpreter_test_file.txt "
				));
	}

	public void interpret(Vector<String> v) {
		if (v.isEmpty()) return;
		String command = v.remove(0);
		Logger.log(Logger.FINEST, "Interpreter: " + command);
		if (command.compareToIgnoreCase("") == 0){
		} else if (command.compareToIgnoreCase("load") == 0){
			if (!v.isEmpty()){
				FileLoader f = new FileLoader(v.remove(0), this);
			}
		} else if (command.compareToIgnoreCase("new") == 0){
			if (!v.isEmpty()){
				make (v);
			}
		} else {
			Logger.log(Logger.FINEST, "invalid command " + command);				
		}
	}
	
	private void make(Vector<String> v){
		String command = v.remove(0);
		if (v.isEmpty()){
			return;
		}
		String name = v.remove(0);
		Logger.log(Logger.FINE, "Interpreter: attempting to make " + command + " " + name);
		String fileName = null;
		if ( ! v.isEmpty()){
			fileName = v.remove(0);
			Logger.log(Logger.FINEST, "filename " + fileName);
		}
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

}
