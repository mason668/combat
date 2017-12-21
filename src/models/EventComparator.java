package models;

import java.util.Comparator;

public class EventComparator implements Comparator<Event>{

	@Override
	public int compare(Event e1, Event e2) {
		double t1 = e1.getTime();
		double t2 = e2.getTime();
		if (t1 < t2) return -1;
		if (t1 > t2) return 1;
		return 0;
	}

}
