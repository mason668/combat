package data.managers;

import java.util.ArrayList;

public class SortedList {
	private ArrayList<KeyedObject> array = new ArrayList<KeyedObject>();
	
	public void add(KeyedObject object){
		array.add(object);
		double x = object.getKey();
		int pointer = array.size()-1;
		if (pointer == 0) return;
		int next = pointer - 1;
		down:
		while (next >=0){
			KeyedObject other = array.get(next);
			double otherX = other.getKey();
			if (otherX > x){
				array.set(pointer,other);
				array.set(next, object);
				pointer--;
				next--;
			} else break down;
		}
	}
	
	public int size(){return array.size();}
	public KeyedObject get(int index){
		return array.get(index);
	}
	
}
