package sim.route;

import java.util.Vector;

public class Route {
	
	private Vector<Node> myRoute = new Vector<Node>();
	
	public void add(Node node){
		if (node == null) return;
		myRoute.add(node);
	}
	public Node getNextNode(){
		if (myRoute.isEmpty()) return null;
		Node node = myRoute.elementAt(0);
		if (node == null){
			myRoute.remove(0);
			return getNextNode();
		}
		return node;
	}
	
	public void clearNode(){
		if (myRoute.isEmpty()) return;
		Node node = myRoute.elementAt(0);
		if (node == null) return;
		myRoute.remove(0);
	}
}
