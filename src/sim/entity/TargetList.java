package sim.entity;

import java.util.Iterator;
import java.util.Vector;

public class TargetList {
	
	private Vector<DetectedEntity> list = new Vector<DetectedEntity>();
	
	public int getSize(){return list.size();}
	public void add(DetectedEntity item){
		list.add(item);
	}
	
	public DetectedEntity get(int index){
		if (index <= list.size()){
			return list.get(index);
		}
		return null;
	}
	
	public Iterator<DetectedEntity> iterator(){
		return list.iterator();
	}

}
