package data.managers;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Vector;

import data.csd.Platform;
import data.map.Coordinate;
import sim.entity.Entity;
import sim.forces.Force;
import utils.Logger;
import utils.Parser;

/**
 * Class to manage a collection of Entities.
 * <p> 
 * In addition to the inherited indexed collection, this class also includes 
 * access to the contents sorted by the x-coordinate of their location. 
 * <p>
 * This class makes use of the Logger class to report errors.
 */
public class EntityList extends ObjectList {
	/*
	 * The core functionality of this class is inherited directly from the
	 * base class ObjectList.
	 * This provides the basic storage of objects indexed by a string key - name.
	 * 
	 * See the methods: get(String), add(String, Object), getSize(), keySet()
	 * 
	 * get(String) returns the object with the 
	 */
	
	private ArrayList<Entity> xarray = new ArrayList<Entity>();
	private Vector<Entity> entityVector = new Vector<Entity>();
	private HashMap<String, Integer> entityHash = new HashMap<String,Integer>();
//	private ArrayList<Entity> entityArray = new ArrayList<Entity>();
//	private ArrayList<String> sortedNames = new ArrayList<String>();

	/**
	 * Get an Entity from the collection using its name as an index key.
	 * @param name The name of the entity to find.
	 * @return The entity if it was found, otherwise null.
	 */
	public Entity getEntity (String name){
		Object o = this.get(name);
		if ( !( o instanceof Entity)) return null;
		return (Entity) o;
	}
	
	public Entity getNext(int index){
		if (index >=xarray.size()-1) return null;
		return this.xarray.get(index+1);
	}

	/**
	 * Add an entity to the collection. This method uses the name attribute 
	 * of the entity to provide an unique key.
	 * @param entity The Entity to add.
	 * @return True if the entity was added, false if not.
	 */
	public boolean add (Entity entity){
		if ( !( entity instanceof Entity)) return false;
		String name = entity.getName();
		if (super.add(name, entity)) {
			entity.addEntityList(this);
			this.xarray.add(entity);
			entity.setXarrayPointer(xarray.size()-1);
			shuffle(entity);
			this.entityVector.add(entity);
			Integer i = this.entityVector.size()-1;
			entityHash.put(entity.getName(), i);
//			this.entityArray.add(entity);
//			this.sortedNames.add(entity.getName());
			return true;
		}
		return false;
	}
	
	public int getIndex(String name){
		Integer i = entityHash.get(name);
		if (i== null) return 0;
		return i.intValue();
	}
	
	/**
	 * Shuffle sort the entity through the array to its correct location.
	 * @param entity
	 */
	public void shuffle(Entity entity){
		int pointer = entity.getXarrayPointer();
		if (xarray.get(pointer)!= entity){
			Logger.err(Logger.WARNING, "EntityList xaray has become corrupted");
			return;
		}
		int next = pointer +1;
		up:
		while (next < xarray.size()){
			Entity e1 = xarray.get(next-1);
			Entity e2 = xarray.get(next);
			if (e1.getLocation().getX() > e2.getLocation().getX()){
				xarray.set(next-1,e2);
				e2.setXarrayPointer(next-1);
				xarray.set(next,e1);
				e1.setXarrayPointer(next);
			} else break up;
			next++;
		}
		pointer = entity.getXarrayPointer();
		next = pointer - 1;
		down:
		while (next >=0){
			Entity e1 = xarray.get(next+1);
			Entity e2 = xarray.get(next);
			if (e1.getLocation().getX() < e2.getLocation().getX()){
				xarray.set(next+1,e2);
				e2.setXarrayPointer(next+1);
				xarray.set(next,e1);
				e1.setXarrayPointer(next);
			} else break down;
			next--;
		}
	}
	
	public Entity getEntity(Coordinate coordinate, double range){
		//TODO should probably rename to nearestEntity
		for (Entity entity: findBetween(coordinate.getX()-range,coordinate.getX()+range)){
			if (Coordinate.distance2D(coordinate, entity.getLocation())<=range){
				return entity;
			}
		}
		return null;
	}
	
	/**
	 * Find the entity with the lowest x coordinate that is 
	 * greater than the supplied x value. This equates to the entity farthest west
	 * that is east of the supplied x value.
	 * @param x The x comparison value.
	 * @return The entity meeting the criteria or null if no such entity exists.
	 */
	public Entity findLowestX(double x){
		if (this.xarray.size()<1) return null;
		if (this.xarray.get(this.xarray.size()-1).getLocation().getX() < x) { 
			return null;
		}
		int low = 0;
		int high = this.xarray.size()-1;
		while (low<high){
			int mid = (high+low)/2;
			if (x > this.xarray.get(mid).getLocation().getX()) {
				low = mid +1;
			} else {
				high = mid;
			} 
		}
		return this.xarray.get(low);
	}
	
	/**
	 * Find the entity with the highest x coordinate that is 
	 * lower than the supplied x value. This equates to the entity farthest east
	 * that is west of the supplied x value.
	 * @param x The x comparison value.
	 * @return The entity meeting the criteria or null if no such entity exists.
	 */
	public Entity findHighestX(double x){
		if (this.xarray.size()<1) return null;
		if (this.xarray.get(0).getLocation().getX() > x) { 
			return null;
		}
		int low = 0;
		int high = this.xarray.size()-1;
		while (low<high){
			int mid = 1 + (high+low)/2;
			if (x >= this.xarray.get(mid).getLocation().getX()) {
				low = mid;
			} else {
				high = mid-1;
			} 
		}
		return this.xarray.get(high);
	}
	
	/**
	 * Find all entities with a x coordinate between the low and high
	 * values supplied (inclusive). Note, the y coordinate is not taken 
	 * into consideration.
	 * @param low The lowest or most westerly x value.
	 * @param high The highest or most easterly x value. 
	 * @return A Vector containing all entities that meet the search criteria.
	 */
	public Vector<Entity> findBetween(double low, double high){
		Vector<Entity> vector = new Vector<Entity>();
		if ( low > high) return vector;
		Entity entity = findLowestX(low);
		if (entity == null) return vector;
		int pointer = entity.getXarrayPointer();
		while (entity.getLocation().getX() <= high){
			vector.add(entity);
			pointer++;
			if (pointer>= this.xarray.size()) break;
			entity = this.xarray.get(pointer);
		}
		return vector;
	}
	
	/**
	 * Find all entities with a circle centred on the supplied coordinate. Note the y coordinate 
	 * of the entity is taken into account. 
	 * @param coordinate The coordinate denoting the centre of the circle.
	 * @param radius The radius of the circle in km.
	 * @return A Vector containing all entities that meet the search criteria.
	 */
	public Vector<Entity> findWithin(Coordinate coordinate, double radius){
		Vector<Entity> vector = new Vector<Entity>();
		if ( radius < 0) return vector;
		double x = coordinate.getX();
		double low = x - radius;
		double high = x + radius;
		Entity entity = findLowestX(low);
		if (entity == null) return vector; //FIXME what if there is no entity west, but are east?
		int pointer = entity.getXarrayPointer();
		while (entity.getLocation().getX() <= high){
			double distance = Coordinate.distance2D(coordinate, entity.getLocation());
			if ( distance <= radius){
				vector.add(entity);
			}
			pointer++;
			if (pointer>= this.xarray.size()) break;
			entity = this.xarray.get(pointer);
		}
		return vector;
	}
	
	public String listDetails(){
		String s = "";
		for (String key: this.keySet()){
			Entity entity = this.getEntity(key);
			s = s + entity.getName();
			s = s + " : " + entity.getPlatform().getName();
			Force force = entity.getForce();
			if (force == null){
				s = s + " : null";
			} else {
				s = s + " : " + force.getName();
			}
			s = s + "\n";
		}
		return s;

	}

	/**
	 * The class is executable and will run a simple test.
	 * @param args Not used.
	 */
	public static void main(String[] args){
		EntityList me = new EntityList();
		me.doArgs(args);
		me.test();
	}
	
	/*
	 * local variables used for testing
	 */
	
	private int testNumberOfEntities = 10;
	private double testXmin = 0.0;
	private double testXmax = 100.0;
	private double testYmin = 0.0;
	private double testYmax = 100.0;
	private double testXcentre = 50.0;
	private double testYcentre = 50.0;
	private double testRadius = 20.0;
	
	private void doArgs(String[] args){
		for (int i = 0;i<args.length;i++){
			String s = args[i];
			if (s.compareToIgnoreCase("-")==0){
			} else if (s.compareToIgnoreCase("-centre")==0){
				try{
					i++;
					this.testXcentre = Double.parseDouble(args[i]);
					i++;
					this.testYcentre = Double.parseDouble(args[i]);
				} catch (Exception e){
					Logger.err(0, "invalid centre coordinate");
				}
			} else if (s.compareToIgnoreCase("-number")==0){
				i++;
				try{
					this.testNumberOfEntities = Integer.parseInt(args[i]);
				} catch (Exception e){
					Logger.err(0, "invalid number of entities");
				}
			} else if (s.compareToIgnoreCase("-radius")==0){
				i++;
				try{
					this.testRadius = Double.parseDouble(args[i]);
				} catch (Exception e){
					Logger.err(0, "invalid radius");
				}
			} else if (s.compareToIgnoreCase("-xmax")==0){
				i++;
				try{
					this.testXmax = Double.parseDouble(args[i]);
				} catch (Exception e){
					Logger.err(0, "invalid xmax");
				}
			} else if (s.compareToIgnoreCase("-xmin")==0){
				i++;
				try{
					this.testXmin = Double.parseDouble(args[i]);
				} catch (Exception e){
					Logger.err(0, "invalid xmin");
				}
			} else if (s.compareToIgnoreCase("-xmax")==0){
				i++;
				try{
					this.testYmax = Double.parseDouble(args[i]);
				} catch (Exception e){
					Logger.err(0, "invalid ymax");
				}
			} else if (s.compareToIgnoreCase("-xmin")==0){
				i++;
				try{
					this.testYmin = Double.parseDouble(args[i]);
				} catch (Exception e){
					Logger.err(0, "invalid ymin");
				}
			}
		}
	}
	
	/**
	 * Perform a simple test.
	 */
	private void test(){
		Platform p = new Platform();
		for (int i=0;i<this.testNumberOfEntities;i++){
			Entity entity = new Entity("entity_" + Parser.pad((""+ (i+1)), 4, "0"),p);
			entity.setLocation(new Coordinate(
					this.testXmin + Math.random() * (this.testXmax - this.testXmin),
					this.testYmin + Math.random() * (this.testYmax - this.testYmin)));
			this.add(entity);
		}
		Logger.log("---------");
		Logger.log("array");
		for (int i=0;i<xarray.size();i++){
			Entity entity = xarray.get(i);
			Logger.log(entity.getName() + " : " + entity.getLocation().toString() + 
					" : " + entity.getXarrayPointer());
		}

		Logger.log("---------");
		for (int i=0;i<5;i++){
			int j = (int) Math.round((this.testNumberOfEntities-1) * Math.random());
			Entity entity = xarray.get(j);
			entity.setX(this.testXmin + Math.random() * (this.testXmax - this.testXmin));
			Logger.log("shuffling " + entity.getName() + " : " + entity.getLocation().toString());
			shuffle(entity);
		}
		
		Logger.log("---------");
		Logger.log("array");
		for (int i=0;i<xarray.size();i++){
			Entity entity = xarray.get(i);
			Logger.log(entity.getName() + " : " + entity.getLocation().toString() + " : " + entity.getXarrayPointer());
		}
		Logger.log("---------");
		
		testLowHigh(this.testXcentre);
		testLowHigh(this.xarray.get(3).getLocation().getX());
		testLowHigh(this.xarray.get(0).getLocation().getX());
		testLowHigh(this.xarray.get(this.xarray.size()-1).getLocation().getX());
		testLowHigh(this.testXmin-0.1);
		testLowHigh(this.testXmax+ 0.1);

		testBetween(this.testXmin,this.testXmax);
		testBetween(this.testXmin + 0.5 * (this.testXcentre - this.testXmin),
				this.testXcentre + 0.5 * (this.testXmax - this.testXcentre));
		testBetween(this.xarray.get(3).getLocation().getX(),
				this.xarray.get(this.xarray.size()-3).getLocation().getX());
		testBetween(this.testXmax+0.1,this.testXmax*2);
		
		testCircle(new Coordinate(this.testXcentre,this.testYcentre), this.testRadius);
	}
	
	private void testBetween(double low, double high){
		Logger.log("---------");
		Logger.log("entities between " + low + " and " + high + " are");
		Vector<Entity> vector = this.findBetween(low, high);
		for(Entity entity: vector){
			Logger.log(entity.getName() + " : " + entity.getLocation().getX());
		}
	}
	
	private void testLowHigh(double x){
		Entity entity = null;
		Logger.log("the most westerly entity east of " + x + " is");
		entity = this.findLowestX(x);
		if (entity != null){
			Logger.log(entity.getName() + " : " + entity.getLocation().getX());
		} else {
			Logger.log("null");
		}
		Logger.log("the most easterly entity west of " + x + " is");
		entity = this.findHighestX(x);
		if (entity != null){
			Logger.log(entity.getName() + " : " + entity.getLocation().getX());
		} else {
			Logger.log("null");
		}
	}
	
	private void testCircle (Coordinate coordinate, double radius){
		Logger.log("---------");
		Logger.log("entities with circle " + coordinate.toString() + " radius " + radius);
		Vector<Entity> vector = this.findWithin(coordinate, radius);
		for (Entity entity : vector){
			Logger.log(entity.getName() + " : " + entity.getLocation().toString());
		}
	}
	
}
