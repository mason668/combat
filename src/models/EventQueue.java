package models;

import java.util.PriorityQueue;
import sim.Constants;
import utils.Logger;
import utils.Parser;

public class EventQueue {

	// events will be sorted by a priority queue
	private PriorityQueue<Event> queue;
	
	// we need to define a comparator function so the queue knows how to sort
	private EventComparator comparator;
	
	// set the initial size of the queue
	private int MAX_SIZE = 1000;
	
	/**
	 * Default constructor.
	 */
	public EventQueue(){
		init (MAX_SIZE);
	}
	/**
	 * Constructor that sets the maximum size of the queue
	 * @param size the maximum size
	 */
	public EventQueue(int size){
		init (size);
	}
	
	private void init(int size){
		comparator = new EventComparator();
		queue = new PriorityQueue<Event>(size, comparator);
	}
	
	/**
	 * True if the queue is empty
	 * @return
	 */
	public boolean isEmpty(){
		return (queue.size()<=0);
	}
	
	/**
	 * Return the number of elements in the queue
	 * @return
	 */
	public int getSize(){
		return queue.size();
	}

	/**
	 * Get the time for the first event in the queue.
	 * @return the time of the first event in the queue or NEVER if the queue is empty
	 */
	public double nextTime(){
		if (this.isEmpty()){
			return Constants.NEVER;
		} else {
			return queue.peek().getTime();
		}
	}
	
	/**
	 * Pop the first event in the queue and return it.
	 * If there are no events, return null.
	 * @return
	 */
	public Event nextEvent(){
		if ( isEmpty()) return null;
		Event e = queue.remove();
		return e;
	}
	
	/**
	 * Add an event to the queue
	 * @param e
	 */
	public void add(Event e){
		if ( e != null){
			queue.add(e);
		}
	}
	
	/**
	 * Remove an event from the queue
	 * @param e Event to remove
	 */
	public void remove(Event e){
		// if b is true, queue contains e otherwise it didn't
		queue.remove(e);
	}
	
	public Object[] toArray(){
		return queue.toArray();
	}
	/**
	 * Process events in the queue until the simulation time is reached
	 * @param time
	 */
	public void doEvents(double time){
		while (this.nextTime() <= time ){
			Event event = nextEvent();
			if ( event != null){
				event = event.doEvent();
				if ( event != null){
					this.add(event);
				}
			}
		}
	}
	
	private int testNumEvents = 10;
	private double testClock = 500.0;
	
	/**
	 * Test the class
	 * @param args -clock <double>, -events <int>
	 */
	public static void main(String[] args){
		EventQueue queue = new EventQueue();
		queue.doArgs(args);
		queue.test(queue.testNumEvents, queue.testClock);
	}
	
	private void doArgs(String[] args){
		for (int i = 0;i<args.length;i++){
			String s = args[i];
			if (s.compareToIgnoreCase("-clock")==0){
				i++;
				try{
					this.testClock = Double.parseDouble(args[i]);
				} catch (Exception e){
					Logger.err(0, "invalid time");
				}
			}
			if (s.compareToIgnoreCase("-events")==0){
				i++;
				try{
					this.testNumEvents = Integer.parseInt(args[i]);
				} catch (Exception e){
					Logger.err(0, "invalid number of events");
				}
			}
		}
	}
	
	/**
	 * A simple self test method.
	 * <p>
	 * This method creates the specified number of events and executes the 
	 * doEvents method until the simulation clock reaches the specified time.
	 * @param numEvents
	 * @param clock
	 * @return true if tests were run successfully.
	 */
	public boolean test(int numEvents, double clock){
		Logger.say("testing event queue");
		Logger.say("number of events: " + this.testNumEvents);
		Logger.say("End time " + Parser.formatTime(this.testClock) + 
				" " + this.testClock);
		long startTime = System.currentTimeMillis();
		Logger.say("start " + startTime);
		Event[] array = new Event[numEvents];
		for (int i=0;i<numEvents;i++){
			array[i] = new Event("event" + Parser.pad(Integer.toString(i+1),4,"0"));
			array[i].setTime(Math.random());
			this.add(array[i]);
		}
		this.doEvents(clock);
		long endTime = System.currentTimeMillis();
		Logger.say("end  " + endTime);
		Logger.say("elapsed time  " + (endTime - startTime) + " mils");
		return true;
	}

}

