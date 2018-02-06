package sim;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.Vector;

import data.managers.EntityList;
import data.managers.ForceList;
import data.map.Map;
import models.Event;
import models.EventQueue;
import models.detection.AcquisitionMatrix;
import models.movement.MoveEvent;
import sim.entity.DetectedEntity;
import sim.entity.Entity;
import sim.entity.MoverEntity;
import sim.entity.ObserverEntity;
import sim.forces.Force;
import sim.postp.PostProcessor;
import utils.Logger;
import utils.Parser;
import utils.Tracer;
import view.ClockListener;

public class Scenario {
	private Parameters parameters;
	public Parameters getParameters(){return parameters;}
	private EventQueue eventQueue = new EventQueue();
	public EventQueue getEventQueue(){return eventQueue;}
	
	private boolean amIRunnable = true; // define whether the "start" option is available or not. 
	//It is useful for some tests to have a non-running scenario
	public void setRunabble(boolean b){amIRunnable = b;}
	
	public Scenario(){
		init();
	}
	public Scenario (String s){
		if (s != null){
			name = s.substring(0);
		}
		init();
	}
	public Scenario (boolean runnable ){
		amIRunnable = runnable;
		init();
	}
	public Scenario (String s, boolean runnable){
		if (s != null){
			name = s.substring(0);
		}
		amIRunnable = runnable;
		init();
	}
	
	private void init(){
		parameters = new Parameters();
		//jansetup
		
		/*
		 * 
        CHARACTER*80    FILENAME$       !/ 'directory: filenamexxx.DAT' /

	CHARACTER*3	LAND$
	CHARACTER*3	SEAS$
	LOGICAL 	EDEINC
	LOGICAL 	IRET
	INTEGER		ICNTEDE(IEDEMAX)
	INTEGER*4	IBPNUM, IBATCHMAX

	INTEGER*4	IRUNFLAG, IERR, IRANTP, IDPLC

        CHARACTER*24    LONGTIME$                     ! UNIX
	KREPORTMODE = 0

	KKD = 0
	KRESET = 0 ! init that game has not ended

	IF ( IRESTART .LE. 0 ) THEN

	  CALL GETSCEN ( KRUNNUM, ISCENNUM ) ! get the runtime parameters from
					! the user

	  IRUNFLAG = 1

	  CALL JANUS_MENU (IBATCHMAX)
	  IF( IRUNTYP .EQ. IRUNBATCH)  THEN
	    IRESTART = 1
	  ENDIF

	ELSE
	  IF( IRUNTYP .EQ. IRUNBATCH)  THEN
	    KRUNNUM = KRUNNUM + 1
	  ENDIF

	  IF (KRUNNUM .GE. IBATCHMAX ) THEN
	    IRESTART = 0
	  ENDIF
	ENDIF

	CALL LIB$ERASE_PAGE (1,1)

c        ENCODE ( 5, 10001, SCNRUN$(1:) ) KRUNNUM + (100*ISCENNUM)
10001   FORMAT( I5.5 )

	CALL GETFILENAME_EXT('PRODAT:', 'JANUS', '.LOG',ISCENNUM, KRUNNUM, FILENAME$)

	print *,'opening log file'
	print *,filename$

	OPEN ( IFILELOG,
     *			NAME	      =   FILENAME$     ,
     *			ACCESS        =  'SEQUENTIAL'   ,
     *			FORM          =  'FORMATTED'  ,
     *			SHARED				,
     *			STATUS        =  'UNKNOWN'          )

	LOGFILE_OPEN = 1

	write (IFILELOG,"('Janus run begins')")
        CALL GETTIMEF ( LONGTIME$ )
	write (IFILELOG,"('Wall clock ',A24)") LONGTIME$
	write (IFILELOG,"('Scenario ',i3.3,' run ',i2.2)") ISCENNUM, KRUNNUM

	! use network screen for data input - see janus_menu
	CALL GETFLAGS

	write (IFILELOG,"('Network Flags:')")
	write (IFILELOG,"('NET_CONECT=     ',I4)") NET_CONNECT
	write (IFILELOG,"('NET_MOVE=       ',I4)") NET_MOVE
	write (IFILELOG,"('NET_DETECT=     ',I4)") NET_DETECT
	write (IFILELOG,"('NET_ART_IMPACT= ',I4)") NET_ART_IMPACT
	write (IFILELOG,"('NET_ARTILLERY=  ',I4)") NET_ARTILLERY
	write (IFILELOG,"('NET_DF_IMPACT=  ',I4)") NET_DF_IMPACT
	write (IFILELOG,"('NET_STATUSRPT=  ',I4)") NET_STATUSRPT
	write (IFILELOG,"('NET_HULK=       ',I4)") NET_HULK
	write (IFILELOG,"('NET_FILTER_MOVE=',I4)") NET_FILTER_MOVE

	IF( IRUNTYP .GT. IRUNCKPT .AND. IRUNTYP .NE. IRUNALONE .AND. IRUNTYP .NE. IRUNBATCH)  THEN

C---------- Get the Branchpoint Number from the user

	    IBPNUM = 0

	    CALL READKB ( 1, 4,
     *		'Enter the Branchpoint Number for this run (1-99):',
     *		  2, IBPNUM, *999, 1, 99 )

	    CALL LIB$ERASE_PAGE (1,1)

C	  PRINT *
C	  PRINT *,'I think you typed ',IBPNUM
C	  call pressret

	  IF ( IRUNTYP .EQ. IRUNBRPT ) THEN
	    CALL JSCRN02
	    CALL LIB$ERASE_PAGE (1,1)
	  ENDIF

	ENDIF

	PRINT *,'Start of JANUS - Data Being initialised.'
	PRINT *,' Data Files Being Read - Please wait'

csar -----------------------
c	open trace log file(s)

	CALL GETFILENAME('PRODAT:', 'TRACE', ISCENNUM, KRUNNUM, FILENAME$)

	print *,'opening trace file'
	print *,filename$

	OPEN ( IFILETRACE,
     *			NAME	      =   FILENAME$     ,
     *			ACCESS        =  'SEQUENTIAL'   ,
     *			FORM          =  'FORMATTED'  ,
     *			ACTION = 'WRITE',
     *			SHARED				,
     *			STATUS        =  'REPLACE'          )

	write (KLINE$,"('Janus run begins')")
	CALL TRACEOUT (0,0,0)

	write (KLINE$,"('Scenario ',i3.3,' run ',i2.2)")
     *		ISCENNUM, KRUNNUM
	CALL TRACEOUT (0,0,0)

	CALL GETTIMEF ( LONGTIME$ )

	write (KLINE$,"('Wall clock ',A24)") LONGTIME$
	CALL TRACEOUT (0,0,0)
	PRINT *,LONGTIME$

C------ Read the scenario data files

	write (IFILELOG,"('Reading data files')")

	CALL FORCREAD (ISCENNUM)
	CALL SYSTMREAD (ISCENNUM,IERR)
        CALL ADREAD (ISCENNUM)
	CALL MAPIO (TERRAIN)

	PRINT *,'Main data files loaded'

	IF( IRUNTYP .EQ. IRUNNORM )  THEN
	    CALL DPLOYREAD ( ISCENNUM )
	    DO_ROUTE_LOCKS = IOFF
	ELSE IF( IRUNTYP .EQ. IRUNCKPT )  THEN
	    CALL CKPTRD ( ISCENNUM, 1 )
	    CALL RESETOBS
	ELSEIF( IRUNTYP .EQ. IRUNALONE .OR. IRUNTYP .EQ. IRUNBATCH)  THEN
	    CALL DPLOYREAD ( ISCENNUM )
	    DO_ROUTE_LOCKS = IOFF
	ELSE
	    CALL CKPTRD ( IBPNUM, 0 )
	    CALL RESETOBS
	ENDIF

	CALL CHANGE_DAY_NIGHT (DAYNIGHT)

	PRINT *,'Deployment data loaded'

	CALL MESSAGE_QUEUE_INIT

csar -----------------------

	IF ( IRUNTYP .NE. IRUNALONE .AND. IRUNTYP .NE. IRUNBATCH) THEN
	  PRINT *,'Initialising RTX'
	  CALL SETUPRTX
	ENDIF

C------ Initialize the data values

	KKD     =  IRUNTYP
	PERIOD  =  RPERIOD

C------ Init Random Number Generator Seed

	IRANTP = IRANTYP
	IDPLC = IDPLCZ

	IF( IRANTP .EQ. 2 )  THEN
	    CALL SETRND
	ELSE IF( IRANTP .EQ. 1 )  THEN
	    ISEED  =  IDPLC
	    ISEED  =  ISEED .OR. 1
	ELSE
	    ISEED  =  54694883
	ENDIF

        KSAVSEED = ISEED

C-------------------------------------------------------------------------
C---------------------- If not a "Normal" run, prep Recording Files
C-------------------------------------------------------------------------

	IF( KKD .NE. IRUNNORM .AND. KKD .NE. IRUNALONE .AND. KKD .NE. IRUNBATCH)  THEN

	  write (IFILELOG,"('Opening post procesor files')")
	  CALL OPEN_POSTP (0)

	ENDIF

C	CALL INITVIEWS

	IF (IRUNTYP .EQ. IRUNNORM) THEN
	  CALL VFYNODES
	  CALL INITVIEWS
	ELSEIF ( IRUNTYP .EQ. IRUNALONE) THEN
	  CALL VFYNODES
	ELSEIF ( IRUNTYP .EQ. IRUNBATCH) THEN
	  CALL VFYNODES
	ELSEIF ( IRUNTYP .EQ. IRUNBRPT) THEN
	  CALL INITVIEWS
	ENDIF

C------ Normal Exit point for SETUP

	write (IFILELOG,"('Exit jansetup')")

	RETURN

10005	FORMAT( I3.3 )
10010	FORMAT( I4.4 )
10901	FORMAT (A3)

C------ User requested JANUS termination

  999	CONTINUE
	TYPE *
	TYPE *
	STOP  '     JANUS  -  Run Terminated.'
	END

		 */
	}
	
	private double clock = 0.0;
	private boolean clockStarted = false;
	private boolean clockPaused = false;
	public void setClock(double time){
		clock = time;
		this.updateClockListeners();
	}
	public double getClock(){return clock;}
	public void incrementClock(double time){
		if (time <= 0) return;
		if (!clockStarted) startClock();
		if (clockPaused) return;
		if (this.getParameters().getRealTimeSynch()){
			long deltaWall = System.currentTimeMillis() - then;
			long deltaSim = (long) (time * 1000 * this.getParameters().getRealTimeRatio());
			while (deltaSim > deltaWall){
				try {
					Thread.sleep(10);
				} catch (Exception e){}
				deltaWall = System.currentTimeMillis() - then;
			}
		}
		double newTime = clock + time;
		setClock(newTime);
		setSynchPoint();
	}
	
	public ActionListener getClockController(){
		return new ActionListener(){
			public void actionPerformed(ActionEvent event) {
				String s = event.getActionCommand();
				if ( s.compareToIgnoreCase("") == 0){
				}
				else if ( s.compareToIgnoreCase("pause") == 0){
					if (clockStarted){
						clockPaused = ! clockPaused;
					}
				}
				else if ( s.compareToIgnoreCase("fast") == 0){
					getParameters().setRealTimeSynch(true);
					double d = getParameters().getRealTimeRatio();
					d = d * 0.667;
					getParameters().setRealTimeRatio(d);
				}
				else if ( s.compareToIgnoreCase("slow") == 0){
					getParameters().setRealTimeSynch(true);
					double d = getParameters().getRealTimeRatio();
					d = d * 1.5;
					getParameters().setRealTimeRatio(d);
				}
				else if ( s.compareToIgnoreCase("1:1") == 0){
					getParameters().setRealTimeSynch(true);
					getParameters().setRealTimeRatio(1.0);
				}
				else if ( s.compareToIgnoreCase("no synch") == 0){
					getParameters().setRealTimeSynch(false);
				}
			}
		};
	}

	private long then = 0;
	public void startClock(){
		setSynchPoint();
		clockStarted = true;
	}
	public void setSynchPoint(){
		then = System.currentTimeMillis();
	}
	
	private Vector<ClockListener> clockListeners = new Vector<ClockListener>();
	public void addClockListener(ClockListener listener){
		clockListeners.addElement(listener);
	}
	private void updateClockListeners(){
		for (ClockListener listener : clockListeners){
			listener.updateClock(clock);
		}
	}
	
	private void initQueue(){ //TODO add all the extra event types
		Logger.say("initialising queues");
		for (String entityName : this.getEntityList().keySet()){
			Entity entity = this.getEntityList().getEntity(entityName);
			Logger.log(this, "entity " + entityName);
			addMoveEvent(entity);
		}
	}
	
	private void addMoveEvent(Entity entity) {
		Class<?> eventClass = makeEventClass(entity.getMovementModel());
		if (eventClass == null) return;
		double time = this.getParameters().getStartTime() + 
				(Math.random() * this.getParameters().getMovementCycleTime());
		try{
			Constructor<?> con = eventClass.getConstructor(double.class, 
					Scenario.class, 
					MoverEntity.class);
			Object o = con.newInstance(time, this, entity);
			if (!(o instanceof MoveEvent)) return;
			Logger.log("made object");
			Event event = (MoveEvent) o;
			eventQueue.add(event);
			Logger.log("added event");
		} catch (NoSuchMethodException e){
			Logger.err(this, Logger.ERROR, "NoSuchMethodException");
		} catch (InvocationTargetException e){
			Logger.err(this, Logger.ERROR, "InvocationTargetException");
		} catch (InstantiationException e){
			Logger.err(this, Logger.ERROR, "InstantiationException");
		} catch (IllegalAccessException e){
			Logger.err(this, Logger.ERROR, "IllegalAccessException");
		}
	}
	
	private Class<?> makeEventClass(String modelName){
		if (modelName == null) return null;
		if (modelName.compareTo("")== 0) return null;
		Class<?> eventClass = null;
		try{
			eventClass = Class.forName(modelName);
		} catch (ClassNotFoundException e){
			Logger.err(this, Logger.ERROR, "ClassNotFoundException");
		}
		return eventClass;
	}
	
	private boolean running = false; //TODO test doesn't seem to use this
	public boolean isRunning(){return running;}
	public void start(){
		if (running) return;
		if (!amIRunnable) return; // this is needed because the interpreter may issue the start command
		Logger.say("starting scenario " + name);
		initQueue();
		//TODO at this point the event queues should be initialised and data validated etc
		running = true;
		Runnable runnable = new RunSim(eventQueue, this);
		Thread thread = new Thread(runnable);
		thread.start();
	}
	
	class RunSim implements Runnable{
		
		EventQueue myQueue;
		private Scenario myScenario;
		
		public RunSim(EventQueue queue, Scenario scenario){
			myQueue = queue;
			myScenario = scenario;
		}

		@Override
		public void run() {
			if (myScenario == null) return;
			if (myQueue == null) return;
			Logger.log("start time " + Parser.formatTime(myScenario.getParameters().getStartTime()));
			myScenario.setClock(myScenario.getParameters().getStartTime());
			Logger.log("scenario starting at " + Parser.formatTime(myScenario.getClock()));
			Logger.log("until " + Parser.formatTime(myScenario.getParameters().getEndTime()));
			while (myScenario.getClock() < myScenario.getParameters().getEndTime()){
				myQueue.doEvents(myScenario.getClock());
				myScenario.incrementClock(myScenario.getParameters().getIncrementAmount());
			}
			Logger.log("scenario ended at " + Parser.formatTime(myScenario.getClock()));
		}
	}

	private String name = "test_scenario";
	public String getName(){return name.substring(0);}
	public void setName(String name){
		this.name = name.substring(0);
	}
	
	private EntityList entityList = new EntityList();
	public EntityList getEntityList(){return entityList;}
	
	private ForceList forceList = new ForceList();
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
	
	private Map scenarioMap = new Map();
	public Map getMap(){return scenarioMap;}
	public void setMap(Map map){
		this.scenarioMap = map;
	}
	
	public int getHostility(ObserverEntity observer, DetectedEntity target){ //TODO need to compare forces etc
		return Constants.HOSTILITY_ENEMY;
	}
	
	public void updateEntity(MoverEntity entity){} //FIXME temp interface to gui to update entities
	// shouldn't do the update, but flag entities requiring update
	
	private PostProcessor postp = new PostProcessor();
	public PostProcessor getPostProcessor(){
		return postp;
	}
	
	private AcquisitionMatrix acquisitionMatrix = new AcquisitionMatrix();
	public AcquisitionMatrix getAcquisitionMatrix(){return acquisitionMatrix;}

}
