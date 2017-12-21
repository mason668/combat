package interpreter;

import java.io.BufferedReader;
import java.io.FileReader;
import java.util.StringTokenizer;
import java.util.Vector;

import utils.Logger;
import utils.Tracer;

public class Interpreter {
	
	protected boolean trace = false;
	
	public Interpreter (){
	}
	
	public void setTrace(boolean b){
		trace = b;
	}
	
	public static void main(String args[]){
		Interpreter me = new Interpreter();
		me.test(args);
	}
	
	private void doArgs(String[] args){
		for (int i = 0;i<args.length;i++){
			String s = args[i];
			if (s.compareToIgnoreCase("-")==0){
			} else if (s.compareToIgnoreCase("-file")==0){
				i++;
				String filename = args[i];
				read (filename);
			}
		}
	}
	
	protected void test(String [] args){
		this.interpret(args);
		this.interpret(testData());
	}
	
	public void interpret (String string){
		Vector<String> vector = new Vector<String>();
		StringTokenizer tokenizer = new StringTokenizer(string);
		while (tokenizer.hasMoreTokens()){
			String word = tokenizer.nextToken();
			vector.addElement(word);
		}
		this.interpret(vector);
	}
	
	public void interpret (String[] array){
		Vector<String> vector = null;
		for (int i=0;i<array.length;i++){
			String s = array[i];
			if (s.length()>2){
				if ((s.substring(0, 2)).compareToIgnoreCase("--") == 0){
					if (vector != null){
						this.interpret(vector);
					}
					vector = new Vector<String>();
					vector.add(s.substring(2));
					continue;
				}
			}
			if (vector != null){
				vector.add(s.substring(0));
			}
		}
		if (vector != null){
			this.interpret(vector);
		}
	}
	
	public void interpret(Vector<String> vector) {
		if (vector.isEmpty()) return;
		String command = vector.remove(0);
		this.doCommand(command, vector);
	}

	protected void doCommand (String command, Vector<String> vector){
		if (vector.isEmpty()) return;
		Logger.say(command + " : " + vector);
	}
	
	protected void read(String fileName){
		if (trace){
			Tracer.write("Interpreter: reading from file " + fileName);
		}
    	BufferedReader b = null;
    	try {
    		b = new BufferedReader(new FileReader(fileName));
    		String record = null;
    		while ((record=b.readLine()) != null){
    			if (record.compareTo("")==0 ) continue;
    			if (record.startsWith("#")) continue;
    			if (record.startsWith("!")) continue;
    			this.interpret(record);
    		}
            b.close();
    	} catch (Exception e) {
    		Logger.err(Logger.WARNING, "Unable to open file " + fileName);
    	}
	}
	
	protected Vector<String> testData(){
		Vector<String> vector = new Vector<String>();
		vector.add("line 1");
		vector.add("line 2");
		return vector;
	}

}
