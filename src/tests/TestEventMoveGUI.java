package tests;

public class TestEventMoveGUI extends TestEventGUI{

	private TestEvent testEvent; 
	private static String label = "TestEventMove";
	private static String testName = "TestMoveEvent";

	public static void main(String[] args){
		TestEventMoveGUI test = new TestEventMoveGUI(label, testName, args);
	}

	public TestEventMoveGUI(String label, String testName, String[] args) {
		super(label, testName, args);
		testEvent = new TestEventMove(args);
	}

}
