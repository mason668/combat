package data.map;

public class BuildingType extends FeatureType{

	public BuildingType (String name){
		buildingName = name.substring(0); //TODO should remove spaces
	}
	
	private String buildingName = "generic_building";
	public String getName(){
		return buildingName.substring(0);
	}

}
