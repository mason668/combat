package interpreterOLD;

import java.io.BufferedReader;
import java.io.FileReader;
import java.util.Vector;

import utils.Logger;
import utils.Parser;

public class FileLoader implements InterpreterInterface{
	
	public static void main(String args[]){
		FileLoader me = new FileLoader();
		me.read("FileLoaderTest.txt", me);
	}
	
	private FileLoader(){
	}
	
	public FileLoader (String fileName, InterpreterInterface interpreter){
		read(fileName, interpreter);
	}
	
	private void read(String fileName, InterpreterInterface interpreter){
		Logger.log(Logger.INFO,"FileLoader: reading from file " + fileName);
    	BufferedReader b = null;
    	try {
    		b = new BufferedReader(new FileReader(fileName));
    		String s = null;
    		while ((s=b.readLine()) != null){
    			if (s.compareTo("")==0 ) continue;
    			if (s.startsWith("#")) continue;
    			if (s.startsWith("!")) continue;
    			interpreter.interpret(Parser.convert(s));
    		}
            b.close();
    	} catch (Exception e) {
    		Logger.err(Logger.WARNING, "Unable to open file " + fileName);
    	}
	}

	@Override
	public void interpret(Vector<String> v) {
		while (! v.isEmpty()){
			Logger.log(Logger.FINEST, v.remove(0));
		}
	}
}

