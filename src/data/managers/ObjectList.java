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
public class ObjectList {
	
	/**
	 * The class is executable and will run a simple test if executed.
	 * @param args - not used.
	 */
	public static void main(String[] args){
		ObjectList me = new ObjectList();
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
	protected HashMap<String, Object> list = new HashMap<String,Object>();
	
	/**
	 * Add an object to the collection.
	 * @param key The key used to index the object.
	 * @param object The object to be stored with the key.
	 * @return the unique numeric id of the object if it was added, or -1 if it failed.
	 * 
	 */
	protected boolean add (String key, Object object){
		if ( object == null){
			Logger.err(Logger.WARNING,"unable to add null object");
			return false;
		}
		if ( list.containsKey(key)) {
			Logger.err(Logger.WARNING, "list already contains an object named " + key);
			return false;
		}
		list.put(key, object);
		return true;
	}
	
	/**
	 * Retrieve the object from the collection using its key.
	 * @param key The key used to identify the object.
	 * @return The object if found or null if not.
	 */
	protected Object get(String key){
		return list.get(key);
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
	
	public Object getFirst(){
		for (String s: list.keySet()){
			return list.get(s);
		}
		return null;
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
}
