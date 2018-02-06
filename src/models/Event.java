package models;

import utils.Logger;

/**
 * 
 *
 */
public class Event {
	/***************** 
	 * internal attributes
	 *****************/
	
	/**
	 * The simulation time for the event to occur (seconds)
	 */
	private double eventTime = 0.0;
	
	/**
	 * Optional name for the event. This is really only used for testing.
	 */
	private String name = "";

	/**
	 * Local variable to indicate if tracing is being conducted.
	 */
	protected boolean tracing = false;
	
	/**
	 * Constructor uses a name and random time.
	 * @param string
	 */
	public Event (String string){
		name = string.substring(0);
	}
	
	/**
	 * Default constructor does nothing, but is required for subclasses.
	 */
	public Event(){}
	
	/**************
	 * Public methods
	 **************/

	/**
	 * Set or change the event time.
	 * @param time
	 */
	public void setTime(double time){eventTime = time;}
	
	/**
	 * Return the event time.
	 */
	public double getTime(){return eventTime;}

	/**
	 * Process the event. This method should be overridden by subclasses to 
	 * do the actual work of the respective event type.
	 * 
	 * For the purpose of testing, this default event does nothing,
	 * except increment the event time by a random amount.
	 * 
	 * @return
	 * Return a reference to an event object. The assumption is that if a valid
	 * event is returned, it will be reinserted back into an event queue. If a 
	 * null is returned,nothing will be added to the queue. 
	 */
	public Event doEvent(){
		Logger.say("doing event " + this.name + " at time " + eventTime);
		eventTime += (5.0 * Math.random());
		return this;
	}
	
}
