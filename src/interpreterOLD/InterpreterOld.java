/**
 * 
 * @author Todd
 */
package interpreterOLD;

import java.util.StringTokenizer;
import java.util.Vector;

/**
 * A generic base class that implements  a simple interpreter. Subclasses should
 * implement their own doCommand method and probably their own test method.
 */
public class InterpreterOld {
	public static void main(String[] args){
		InterpreterOld me = new InterpreterOld();
		Object o = new Object();
		me.test(o);
	}
	
	protected void test(Object o){
		System.out.println("testing " + this.getClass().getCanonicalName() + 
				" with " + o.getClass().getCanonicalName());
		String[] a = {"one", "two", "three"};
		this.interpret(o, a);
		String s = "four five six";
		this.interpret(o, s);
		Vector<String> v = new Vector<String>();
		v.addElement("seven");
		v.addElement("eight");
		v.addElement("nine");
		this.interpret(o, v);		
	}
	
	public InterpreterOld (){
	}
	
	protected void interpret (Object o, String s){
		Vector<String> v = new Vector<String>();
		StringTokenizer t = new StringTokenizer(s);
		while (t.hasMoreTokens()){
			String e = t.nextToken();
			v.addElement(e);
		}
		this.interpret(o, v);
	}
	protected void interpret (Object o, String[] s){
		Vector<String> v = new Vector<String>();
		for (int i=0;i<s.length;i++){
			v.addElement(s[i]);
		}
		this.interpret(o, v);
	}

	protected void interpret (Object o, Vector<String> v){
		if (v.isEmpty()) return;
		String command = v.remove(0);
		this.doCommand(o, command, v);
	}
	
	protected void doCommand (Object o, String command, Vector<String> v){
		System.out.println("    " + command);
		if (v.isEmpty()) return;
		command = v.remove(0);
		this.doCommand(o, command, v);
	}

}
