package data.map;

public class River extends LinearFeature{
	
	public River(RiverType type){
		super();
		riverType = type;
	}
	
	private RiverType riverType;
	public RiverType getType(){
		return riverType;
	}
	

}
