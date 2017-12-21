package interpreterOLD;

import java.util.Vector;

import data.csd.Platform;
import data.csd.ProbabilityTable;

public class ProbabilityTableInterpreter extends InterpreterOld{

	public static void main(String[] args){
		InterpreterOld me = new ProbabilityTableInterpreter();
		ProbabilityTable o = new ProbabilityTable();
		if (args.length > 0){
			me.interpret(o, args);
		} else {
			String s = "rows 4 cols 3 row 2 0.5 0.5 0.5";
			me.interpret(o, s);	
		}
		o.test();
	}
	
	protected void test(Object o){
//		super.test(o);
		String s = "rows 4 cols 3 row 2 0.5 0.5 0.5";
		this.interpret(o, s);
	}
	
	protected void doCommand (Object o, String command, Vector<String> v){
		if (!(o instanceof ProbabilityTable))return;
		ProbabilityTable p = (ProbabilityTable) o;
		if (command.compareToIgnoreCase("") == 0){
		} else if (command.compareToIgnoreCase("cols") == 0){
			if (v.isEmpty()) return;
			String value = v.remove(0);
			try {
				int i = Integer.parseInt(value);
				p.setCols(i);
			} catch (Exception e){} //TODO
		} else if (command.compareToIgnoreCase("row") == 0){
			if (v.isEmpty()) return;
			String value = v.remove(0);
			try {
				int row = Integer.parseInt(value);
				if (v.size()< p.getCols()) {
					return; //TODO
				}
				for (int col=1;col<=p.getCols();col++){
					value = v.remove(0);
					double data = Double.parseDouble(value);
					p.setData(row, col, data);
				}
			} catch (Exception e){} //TODO
		} else if (command.compareToIgnoreCase("rows") == 0){
			if (v.isEmpty()) return;
			String value = v.remove(0);
			try {
				int i = Integer.parseInt(value);
				p.setRows(i);
			} catch (Exception e){}
		} else {
			super.doCommand(o, command, v);
		}
		if (v.isEmpty()) return;
		command = v.remove(0);
		this.doCommand(o, command, v);
	}

}

/*
 * 
	@Override
	public void interpret(Vector<String> v) {
		while (! v.isEmpty()){
			String command = v.remove(0);
			if (command.compareToIgnoreCase("") == 0){
			} else if (command.compareToIgnoreCase("cols") == 0){
				if (v.isEmpty()) return;
				String value = v.remove(0);
				try {
					int i = Integer.parseInt(value);
					this.setCols(i);
				} catch (Exception e){} //TODO
			} else if (command.compareToIgnoreCase("load") == 0){
				if (v.isEmpty()) return;
				String value = v.remove(0);
				FileLoader f = new FileLoader(value,this);
			} else if (command.compareToIgnoreCase("posture") == 0){
				if (v.size()<(cols+1)) {
					v.clear();
					return;
				}
				String posture = v.remove(0);
				int row = this.getRow(posture);
				if ( row <1){
					v.clear();
					return;
				}
				try {
					for (int i=1; i<= cols;i++){
						String value = v.remove(0);
						double d = Double.parseDouble(value);
						this.setData(row, i, d);
					}
				} catch (Exception e){} //TODO
			} else if (command.compareToIgnoreCase("ranges") == 0){
				if (v.isEmpty()) return;
				if (v.size()< this.getCols()) {
					v.clear();
					return; //TODO
				}
				try {
					for (int i=1; i<= cols;i++){
						String value = v.remove(0);
						double d = Double.parseDouble(value);
						ranges[i-1] = d;
					}
				} catch (Exception e){} //TODO
				
			} else if (command.compareToIgnoreCase("rows") == 0){
				if (v.isEmpty()) return;
				String value = v.remove(0);
				try {
					int i = Integer.parseInt(value);
					this.setRows(i);
				} catch (Exception e){}
			}
			
		}
	}
	
*/