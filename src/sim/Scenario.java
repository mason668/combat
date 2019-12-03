package sim;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Vector;

import data.managers.EntityList;
import data.managers.ForceList;
import data.map.Map;
import models.EventQueue;
import models.detection.AcquisitionMatrix;
import sim.entity.DetectedEntity;
import sim.entity.EntityListener;
import sim.entity.MoverEntity;
import sim.entity.ObserverEntity;
import sim.forces.Force;
import sim.postp.PostProcessor;
import utils.Logger;
import view.ClockListener;

public class Scenario {
	private Vector<EntityListener> entityListeners = new Vector<EntityListener>();
	private Parameters parameters;
	private EventQueue eventQueue = new EventQueue();
	private String name = "test_scenario";
	private EntityList entityList = new EntityList();
	private ForceList forceList = new ForceList();
	private Map scenarioMap = new Map();
	private PostProcessor postp = new PostProcessor();
	private AcquisitionMatrix acquisitionMatrix = new AcquisitionMatrix();
	
	public EventQueue getEventQueue(){return eventQueue;}
	public Parameters getParameters(){return parameters;}
	
	public Scenario(){
		init();
	}
	public Scenario (String s){
		if (s != null){
			name = s.substring(0);
		}
		init();
	}
	
	public void addEntityListener(EntityListener listener){
		if (listener == null) return;
		entityListeners.add(listener);
	}

	private void init(){
		parameters = new Parameters();
	}
	
	public String getName(){return name.substring(0);}
	public void setName(String name){
		this.name = name.substring(0);
	}
	
	public EntityList getEntityList(){return entityList;}
	
	public ForceList getForceList(){return forceList;}
	public void addForce(String forceName){
		if (forceList.contains(forceName)) return;
		Force force = new Force(forceName);
		forceList.add(force);
		for (String name : forceList.keySet()){
			force.setHostility(name, Constants.HOSTILITY_FRIEND);
			forceList.getForce(name).setHostility(forceName,Constants.HOSTILITY_FRIEND);
		}
	}
	
	public Map getMap(){return scenarioMap;}
	public void setMap(Map map){
		this.scenarioMap = map;
	}
	
	public int getHostility(ObserverEntity observer, DetectedEntity target){ //TODO need to compare forces etc
		return Constants.HOSTILITY_ENEMY;
	}
	
	public void updateEntity(MoverEntity entity){} //FIXME temp interface to gui to update entities
	// shouldn't do the update, but flag entities requiring update
	
	public PostProcessor getPostProcessor(){
		return postp;
	}
	
	public AcquisitionMatrix getAcquisitionMatrix(){return acquisitionMatrix;}
	
//	private String movementModel = "models.movement.MoveEvent";
//	private String movementModel = "models.janus.JanusMoveEvent";
	private String movementModel = "";
	public String getMovementModel(){
		return movementModel.substring(0);
	}
	public void setMovementModel(String model){
		movementModel = model.substring(0);
	}
	private String scanModel = "";
	public String getScanModel(){
		return scanModel.substring(0);
	}
	public void setScanModel(String model){
		scanModel = model.substring(0);
	}
	private String detectModel = "";
	public String getDetectModel(){
		return detectModel.substring(0);
	}
	public void setDetectModel(String model){
		detectModel = model.substring(0);
	}
	private String shootModel = "";
	public String getShootModel(){
		return shootModel.substring(0);
	}
	public void setShootModel(String model){
		shootModel = model.substring(0);
	}
	private String suppressionModel = "";
	public String getSuppressionModel(){
		return suppressionModel.substring(0);
	}
	public void setSuppressionModel(String model){
		shootModel = model.substring(0);
	}
	private String resupplyModel = "";
	public String getResupplyModel(){
		return resupplyModel.substring(0);
	}
	public void setResupplyModel(String model){
		resupplyModel = model.substring(0);
	}
	private String casualtyModel = "";
	public String getCasualtyModel(){
		return casualtyModel.substring(0);
	}
	public void setCasualtyModel(String model){
		casualtyModel = model.substring(0);
	}
	private String detectObstacleModel = "";
	public String getDetectObstacleModel(){
		return detectObstacleModel.substring(0);
	}
	public void setDetectObstacleModel(String model){
		detectObstacleModel = model.substring(0);
	}
	
}
