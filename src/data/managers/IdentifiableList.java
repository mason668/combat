package data.managers;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Set;

import sim.entity.Entity;
import utils.Logger;

/**
 * Base class for handling lists of objects.
 * Uses simple "add" and "get" methods and uses a string as an unique key.
 * This class should be extended to restrict it to each type of object
 * that will be stored in the list.
 * <p>
 * This class makes use of the Logger class for reporting errors.
 *
 */
public class IdentifiableList {
	
	/**
	 * The class is executable and will run a simple test if executed.
	 * @param args - not used.
	 */
	public static void main(String[] args){
		IdentifiableList me = new IdentifiableList();
		String p1 = new String("one");
		String p2 = new String("two");
//FIXME		Logger.say("attempting to add " + p1);
//FIXME		me.add(p1, p1);
//FIXME		Logger.say("attempting to add " + p2);
//FIXME		me.add(p2, p2);
		Logger.say("attempting to add " + p2);
//FIXME		me.add(p2, p2);
		
		Logger.say("list contents: " + me.list.keySet());
		Object p3 = me.get("three");
		Logger.say("looking for three");
		if (p3 == null){
			Logger.say("unable fo find three in the collection");
		} else {
			Logger.say("found three in the collection");
		}
		Logger.say("looking for one");
		Object p4 = me.get("one");
		if (p4 == null){
			Logger.say("unable fo find one in the collection");
		} else {
			Logger.say("found one in the collection");
		}
	}

	/**
	 * Store the objects as a hashmap indexed by a string key. 
	 */
	protected HashMap<String, Identifiable> list = new HashMap<String,Identifiable>();
	protected ArrayList<Identifiable> objectArray = new ArrayList<Identifiable>();
	protected ArrayList<String> sortedNames = new ArrayList<String>();
	
	/**
	 * Add an object to the collection.
	 * @param key The key used to index the object.
	 * @param object The object to be stored with the key.
	 * @return the unique numeric id of the object if it was added, or -1 if it failed.
	 * 
	 */
	protected int add (String key, Identifiable object){
		if ( object == null){
			Logger.err(Logger.WARNING,"unable to add null object");
			return -1;
		}
		if ( list.containsKey(key)) {
			Logger.err(Logger.WARNING, "list already contains an object named " + key);
			return -1;
		}
		list.put(key, object);
		this.objectArray.add(object);
		this.sortedNames.add(key);
		int index = this.objectArray.size()-1;
		object.setNumber(index);
		return index;
	}
	
	public int getIndex(String key){
		if (!this.contains(key)) return -1;
		Identifiable object = this.get(key);
		return object.getNumber();
	}
	
	/**
	 * Retrieve the object from the collection using its key.
	 * @param key The key used to identify the object.
	 * @return The object if found or null if not.
	 */
	protected Identifiable get(String key){
		return list.get(key);
	}
	
	
	public Identifiable get(int index){
		if ( index >= this.objectArray.size()) return null;
		if (index < 0) return null;
		return this.objectArray.get(index);
	}

	/**
	 * Determine if the collection contains an object 
	 * matching the key.
	 * @param key The key to search for.
	 * @return True if the object exists and false if not.
	 */
	public boolean contains(String key){
		return list.containsKey(key);
	}

	/**
	 * Return the number of objects in the collection.
	 * @return The number of objects in the collection. Zero if the collection
	 * is empty.
	 */
	public int getSize(){
		return this.list.size();
	}
	
	/**
	 * Return the list of keys in the collection.
	 * @return The set of keys (Strings) in the collection.
	 */
	public Set<String> keySet(){
		return this.list.keySet();
	}
	
	/**
	 * List the keys as a String with embedded line breaks.
	 * This function is primarily provided for testing purposes. 
	 * @return A String containing a list of the keys separated by line breaks. 
	 */
	public String list(){
		String s = "";
		for (String key: this.keySet()){
			s = s + key + "\n";
		}
		return s;
	}

}
