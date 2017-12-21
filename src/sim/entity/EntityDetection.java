package sim.entity;

import sim.Constants;

public class EntityDetection {
	
	private DetectedEntity entity = null;
	private int observationLevel = 0;
	
	public EntityDetection( DetectedEntity e, int level){
		entity = e;
		this.setObservationLevel(level);
	}
	
	public DetectedEntity getEntity(){return entity;}
	public int getObservationLevel(){return observationLevel;}
	public void setObservationLevel(int i){
		if (i < Constants.OBSERVATION_LEVEL_UNSEEN) return;
		if (i > Constants.OBSERVATION_LEVEL_IDENTIFIED) return;
		observationLevel = i;
	}

}
