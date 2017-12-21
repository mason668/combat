package sim.entity;

//TODO this is a facade for entities than can lay bridges

public interface BridgeEntity {
	
	public String getName();
	
	public void removeFromBridge(MoverEntity entity);

}
