package models.detection;

import data.csd.Sensor;
import data.managers.EntityList;
import sim.entity.DetectedEntity;
import sim.entity.ObserverEntity;
import utils.Logger;

public class AcquisitionMatrix {
	// data from globacq.f
	
	private static final int numPairs = 100; // globacq.numpairs
	private static final int numPerfs = 10; // globacq.numperfs FIXME originally 1000
	private static final int numVisbl = 15; // globacq.numvisbl
	private static final int numLongTL = 30; // globacq.numlongtl
	
	/**
	 * Array of detection performance values
	 */
	private static final double[] pairsval = { // see janus.initacq
	0.285,  0.730,  0.988,  1.120,  1.221,
	1.323,  1.408,  1.494,  1.564,  1.634,
	1.704,  1.774,  1.844,  1.898,  1.953,
	2.023,  2.093,  2.131,  2.170,  2.240,
	2.279,  2.318,  2.372,  2.411,  2.465,
	2.504,  2.543,  2.597,  2.636,  2.674,
	2.713,  2.752,  2.791,  2.829,  2.868,
	2.922,  2.961,  3.000,  3.039,  3.077,
	3.116,  3.155,  3.194,  3.232,  3.287,
	3.326,  3.364,  3.403,  3.442,  3.481,
	3.519,  3.558,  3.597,  3.636,  3.690,
	3.729,  3.767,  3.822,  3.861,  3.915,
	3.954,  3.992,  4.047,  4.086,  4.140,
	4.210,  4.249,  4.287,  4.357,  4.412,
	4.451,  4.505,  4.575,  4.645,  4.715,
	4.785,  4.855,  4.925,  4.995,  5.065,
	5.135,  5.205,  5.275,  5.345,  5.446,
	5.579,  5.680,  5.812,  5.945,  6.077,
	6.210,  6.342,  6.537,  6.732,  6.990,
	7.310,  7.630,  8.075,  8.770, 12.340
	};
	
	private int kacqPerf[][] = new int[numPerfs][numPerfs];
	
	public static void main(String[] args){
		AcquisitionMatrix test = new AcquisitionMatrix();
		for (int i=0;i<numPerfs;i++){
			for (int j=0; j< numPerfs;j++){
				Logger.say("matrix[" + i + "][" + j + "] = " + test.kacqPerf[i][j]);
			}
		}
		
	}

	public AcquisitionMatrix(){
		initArray();
		writeData();
	}
	
	private void initArray(){
		/*
C	PURPOSE: To initialize random detection performance capability	C
C		 of every unit (as a sensor) against every unit (as a	C
C		 target) on the other side, for both sides.		C
C		 Present data is for a median of 3.5 cycles.		C
C		 A program which can generate a data statement for 	C
C		 various thresholds is in [TASHIRO.SEEIT]TACQGEN.FOR.	C
		 * 
		 */
		//TODO could use options to decide how to randomise
		for (int i=0;i<numPerfs;i++){
			for (int j=0; j< numPerfs;j++){
				int index = (int)(Math.random()* numPairs); // totally random
//				int index = (int)(Math.random()* (numPairs-20)) +10; // random average
//				int index = 50; // exact average
				kacqPerf[i][j] = index;
			}
		}
	}
	
	private void writeData(){
		//TODO should allow file name spec etc
		/*
	TYPE *,'Opening pp acquisition file.',filename$

c--------- create filename to be used - scnario, run numbers in globsetup
        CALL GETFILENAME('PRODAT:', 'PPACQU',ISCENNUM, KRUNNUM, FILENAME$)
c	FILENAME$  =  'PRODAT:PPACQU' // SCNRUN$ // '.DAT'
200	    OPEN ( IFILESCRCH,	NAME	      =   FILENAME$     ,
     *			ACCESS        =  'SEQUENTIAL'   ,
     *			FORM          =  'UNFORMATTED'  ,
c     *			ORGANIZATION  =  'SEQUENTIAL'   ,
     *			SHARED				,
c     *			EXTENDSIZE     = 50,
     *			STATUS        =  'UNKNOWN'          )

	WRITE (IFILESCRCH,ERR=990) 

     *		KACQPERF,
     *		PAIRSVAL

C     *		IHAVTARS,
C     *		IOBSERV,
C     *		KUSESENS,
C     *		MYTARLIST,
C     *		KLONGTLU,
C     *		KLONGTLS,
C     *		NMYUNIT,
C     *		KDETECTD

	CLOSE (IFILESCRCH)

	RETURN

990	CONTINUE
	CALL FILERR
	CLOSE (IFILESCRCH)
	GOTO 200

	END
		 * 
		 */
	}
	
	
	
	/* original janus comments
C				DICTIONARY				C
C									C
C	KACQPERF - Array containing pointers to Detection Performance	C
C		   capability values in array PAIRSVAL, for every	C
C		   sensor/target combination. This array is initialized C
C		   by subroutine INITACQ, unless the run is a		C
C		   CHECKPOINT/RESTART Run.				C
C									C
C	PAIRSVAL - Array (table) of Detection Performance values.	C
C									C
C	IHAVTARS - Array containing a flag for each unit to indicate	C
C		   if it currently has entries in its target list.	C
C		   This array needs no initialization, but must be	C
C		   read in if the run is a CHECKPOINT/RESTART Run.	C
C									C
C	IOBSERV  - Array containing a flag for each unit for each view	C
C		   to indicate if unit is observed by view:		C
C			0 = unit not observed by View			C
C			1 = unit is  observed by View			C
C									C
C	KUSESENS - Which sensor (primary or secondary) is currently	C
C		   being used by this unit				C
C									C
C									C
C		"NORMAL TARGET LIST"  (For each unit of each side)	C
C									C
C	NMYUNIT  - Unit ID number of enemy target unit			C
C	KDETECTD - Detected flag,  0 = not detected			C
C				   1 = detected				C
C									C
C		"LONG TARGET LIST"  (Pool of "long" target lists)	C
C									C
C	KLONGTLU  - Unit ID number of enemy target unit			C
C	KLONGTLS  - Detected status,  0 = not detected			C
C				      1 = detected			C
C									C
C	MYTARLIST - Pointer to Long Target List slot (for each unit)	C
C									C
C									C
C-----------------------------------------------------------------------C


	BYTE IHAVTARS(NUMUNITS,NUMSIDES)
	COMMON / IHAVTARS / IHAVTARS

	LOGICAL*1 IOBSERV(NUMUNITS,NUMSTAT,NUMSIDES)
	COMMON / IOBSERV / IOBSERV

	BYTE KUSESENS(NUMUNITS,NUMSIDES)
	COMMON / KUSESENS / KUSESENS

	BYTE MYTARLIST( NUMUNITS, NUMSIDES )
	COMMON / MYTARLIST / MYTARLIST

	INTEGER*2	KLONGTLU( NUMVISBL,NUMLONGTL )
	COMMON / KLONGTLU / KLONGTLU

	BYTE KLONGTLS( NUMVISBL,NUMLONGTL )
	COMMON / KLONGTLS / KLONGTLS

	INTEGER*2 NMYUNIT( NMVISB, NUMUNITS, NUMSIDES )
	COMMON / NMYUNIT / NMYUNIT

	BYTE KDETECTD( NMVISB, NUMUNITS, NUMSIDES )
	COMMON / KDETECTD / KDETECTD
	 * 
	 */
	
	/*
	 * 	SUBROUTINE  INITACQ

C-----------------------------------------------------------------------C
C									C
C									C
C-----------------------------------------------------------------------C

	 */
	
	public double getAcquisitionThreshold(EntityList list, ObserverEntity observer, DetectedEntity target){
		int index1 = list.getIndex(observer.getName());
		int index2 = list.getIndex(target.getName());
		index1 = index1 % numPerfs;
		index2 = index2 % numPerfs;
		int ival = kacqPerf[index1][index2];
		if (ival > numPairs) ival = 50;
		if (ival < 0) ival = 50;
		return this.pairsval[ival];
	}
	
	public double pairs (Sensor sensor, int ContrastClass, double range, double size, double opticalLength){
		double bars = 10.0;
		return bars;
		/* TODO pairs.f
	SUBROUTINE  PAIRS ( ISENS, JCONCLAS, RANGE, SIZE, OLEN, BARS, IDEBUG )

C-----------------------------------------------------------------------C
C									C
C	Purpose:  To compute cycles resolved on a target.		C
C									C
C-----------------------------------------------------------------------C
C									C
C    Input:								C
C									C
C									C
C	ISENS	  -  Janus sensor number				C
C	JCONCLAS  -  Janus contrast class of target			C
C	RANGE	  -  Range (km) to target				C
C	SIZE	  -  Target minimum presented dimension (meters)	C
C									C
C									C
C    Output:								C
C									C
C	BARS	  -  Cycles resolved on Target				C
C									C
C-----------------------------------------------------------------------C
C									C
C    NOTES:								C
C									C
C	Temperature and contrast variables are the natural log of the	C
C	temperature or contrast in question.				C
C									C
C	The formula for attenuation of the signature of a target	C
C	(temperature or optical contast) can be written as:		C
C									C
C	Actual_signature  =  Signature * T1 * T2			C
C									C
C	Where T1 is the transmission of the atmosphere and T2 is the	C
C	transmission of any smoke along the LOS.  The LN of a trans-	C
C	mission is called an "extinction".  If the LOS involves a	C
C	range of R ( through the atmosphere or through smoke) and we	C
C	write  LN(Ti)  as   -alpha * R,  then alpha is the "extinc-	C
C	tion coefficient".						C
C									C
C	Since this routine is working with logs, the extinctions are	C
C	represented.  i.e. we represent the calculation			C
C	    LN(actual_signture)  =  LN(signature) + LN(T1) + LN(T2)	C
C									C
C	The atmospheric extinction for optical sensors is computed	C
C	from the curve OEXTCURVE.  This quantity is negative and so	C
C	no subtraction is needed.  For thermal sensors the quantity	C
C	EXTINCTION() represents the extinction coefficient.  The log	C
C	of the extinction is thus  - EXTINTION() * RANGE.  Thus sub-	C
C	traction is performed.  The extinction due to smoke, OLEN,	C
C	is assumed to be input with the "wrong" sign;  it is assumed	C
C	to be positive.  Thus it is subtracted.				C
C									C
C-----------------------------------------------------------------------C

	include		'glbparam.f'
	include		'glbsensr.f'
	include		'globfiles.f'
	include		'globrpt.f'

	PARAMETER	RANGEMIN   =  .01

C------ Convert JANUS Contrast Class to LOG(temperature)

	DIMENSION	TGTTEMP(12)

	DATA	  TGTTEMP /
     *			-0.693147,
     *			0.000000,
     *			0.405465,
     *			0.693147,
     *			0.916291,
     *			1.098612,
     *			1.252763,
     *			1.386294,
     *			1.504077,
     *			1.609438,
     *			1.791759,
     *			1.945910      /

	IF( RANGE .LT. RANGEMIN )  THEN
	  BARS  =  100.0 * SIZE
	  GOTO 999
	ENDIF

c	  WRITE (KLINE$,"('pairs ',I4,XX, I2,XX,F12.3,XX,F12.3,XX,F12.3,XX,F12.3,XX,I2)")
c     *		isens, jconclas, RANGE, SIZE, OLEN, BARS, IDEBUG
c	  CALL TRACEOUT (0,0,0)

	IF ( ISENS .LE. 0 .OR. ISENS .GT. NUMSENSTYPS) THEN
	  PRINT *,'WHY IS THE SENSOR TYPE SET TO ',ISENS
	  BARS = 0.0
	  GOTO 999
	ENDIF

D	IF (IDEBUG .GT. 0 ) THEN
D	  WRITE (KLINE$,"(8X,'MODULE:PAIRS')")
D	  CALL TRACEOUT (0,0,0)
D	  WRITE (KLINE$,"(8X,'max rng for sensor = ',f12.3)") sensrmax(isens)
D	  CALL TRACEOUT (0,0,0)
D	ENDIF

	IF( RANGE .GE. SENSRMAX(ISENS) )  THEN
	  BARS = 0.0
	  GOTO 999
	END IF

	IBAND  =  KSENSCLAS(ISENS)

D	IF (IDEBUG .GT. 0 ) THEN
D	  WRITE (KLINE$,"(8X,'sensor band = ',I2)") iband
D	  CALL TRACEOUT (0,0,0)
D	ENDIF

	IF( IBAND .GT. 2 )  GOTO 200

C--------------------- Optical sensors (bands 1 and 2)

D	IF (IDEBUG .GT. 0 ) THEN
D	  WRITE (KLINE$,"(8X,'Optical Sensor')")
D	  CALL TRACEOUT (0,0,0)
D	  WRITE (KLINE$,"(8X,'max rng for band = ',f12.3)") oextrmax(iband)
D	  CALL TRACEOUT (0,0,0)
D	ENDIF

	IF( RANGE .LT. OEXTRMAX(IBAND) )  THEN
	  INDEX  =  INT( RANGE * OEXTDR(IBAND) )

C---------- OPTCONLOG is natural log of OPTICONTR

D	  IF (IDEBUG .GT. 0 ) THEN
D	    WRITE (KLINE$,"(8X,'index = ',I4)") index
D	    CALL TRACEOUT (0,0,0)
D	    WRITE (KLINE$,"(8X,'optconlog = ',f12.3)") optconlog
D	    CALL TRACEOUT (0,0,0)
D	    WRITE (KLINE$,"(8X,'olen = ',f12.3)") olen
D	    CALL TRACEOUT (0,0,0)
D	    WRITE (KLINE$,"(8X,'oext 1 = ',f12.3)") oextcurve(1,index,iband)
D	    CALL TRACEOUT (0,0,0)
D	    WRITE (KLINE$,"(8X,'oext 2 = ',f12.3)") oextcurve(2,index,iband)
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF

	  CONTRAST  =    OPTCONLOG
     *			+  OEXTCURVE(1,INDEX,IBAND) * RANGE
     *			+  OEXTCURVE(2,INDEX,IBAND) -  OLEN

D	  IF (IDEBUG .GT. 0 ) THEN
D	    WRITE (KLINE$,"(8X,'contrast = ',f12.3)") contrast
D	    CALL TRACEOUT (0,0,0)
D	    WRITE (KLINE$,"(8X,'sensemintemp = ',f12.3)") sensmintemp(isens)
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF

	  PLACE  =  CONTRAST - SENSMINTEMP(ISENS)

	  IF( PLACE .GT. 0 )  THEN
	    INDEX  =  INT(PLACE * SENSDT(ISENS))
	    IF( INDEX .LE. NUMSENPTS )  THEN
	      CPM  =  SENCURVE(1,INDEX,ISENS) * CONTRAST + SENCURVE(2,INDEX,ISENS)
	    ELSE
	      CPM  =  SENSMAXCPM(ISENS)
	    ENDIF

D	    IF (IDEBUG .GT. 0 ) THEN
D	      WRITE (KLINE$,"(12X,'cpm = ',f12.3)") cpm
D	      CALL TRACEOUT (0,0,0)
D	    ENDIF

	    BARS  =  CPM * SIZE/RANGE

	    GOTO 999

	  ELSE
		BARS  =  0.0
		GOTO 999
	  ENDIF
	ELSE
	  BARS  =  0.0
	  GOTO 999
	ENDIF

  200	CONTINUE

C------------------------- Thermal sensors (bands 3 & 4)

D	IF (IDEBUG .GT. 0 ) THEN
D	  WRITE (KLINE$,"(8X,'Thermal Sensor')")
D	  CALL TRACEOUT (0,0,0)
D	  WRITE (KLINE$,"(8X,'Contrast class ',I2)") JCONCLAS
D	  CALL TRACEOUT (0,0,0)
D	  WRITE (KLINE$,"(8X,'Temp for class (TGTTEMP) ',F12.4)") TGTTEMP(JCONCLAS)
D	  CALL TRACEOUT (0,0,0)
D	  WRITE (KLINE$,"(8X,'Extinction for band ',F12.4)") EXTINCTION(IBAND)
D	  CALL TRACEOUT (0,0,0)
D	  WRITE (KLINE$,"(8X,'Range ',F12.3)") RANGE
D	  CALL TRACEOUT (0,0,0)
D	  WRITE (KLINE$,"(8X,'Olen ',F12.3)") OLEN
D	  CALL TRACEOUT (0,0,0)
D	  WRITE (KLINE$,"(8X,'SENSMINTEMP ',F12.3)") SENSMINTEMP(ISENS)
D	  CALL TRACEOUT (0,0,0)
D	  WRITE (KLINE$,"(8X,'SENSDT ',F12.3)") SENSDT(ISENS)
D	  CALL TRACEOUT (0,0,0)
D	ENDIF

	TTEMP  =  TGTTEMP(JCONCLAS) - EXTINCTION(IBAND) * RANGE - OLEN
	PLACE  =  TTEMP - SENSMINTEMP(ISENS)

D	IF (IDEBUG .GT. 0 ) THEN
D	  WRITE (KLINE$,"(8X,'TTEMP = TGTTEMP - EXTINCTION * RANGE - OLEN')")
D	  CALL TRACEOUT (0,0,0)
D	  WRITE (KLINE$,"(12X,'TTEMP = ', F12.3)") TTEMP
D	  CALL TRACEOUT (0,0,0)
D	  WRITE (KLINE$,"(8X,'Place ',F12.3)") PLACE
D	  CALL TRACEOUT (0,0,0)
D	ENDIF

	IF( PLACE .GT. 0 )  THEN
	  INDEX  =  INT( PLACE * SENSDT(ISENS) )
	  IF( INDEX .LE. NUMSENPTS )  THEN
	    CPM  =  SENCURVE(1,INDEX,ISENS) * TTEMP + SENCURVE(2,INDEX,ISENS)
	  ELSE
	    CPM  =  SENSMAXCPM(ISENS)
	  ENDIF

D	  IF (IDEBUG .GT. 0 ) THEN
D	    WRITE (KLINE$,"(8X,'CPM = ',F12.3)") CPM
D	    CALL TRACEOUT (0,0,0)
D	  ENDIF

	  BARS  =  CPM * ( SIZE / RANGE )
	ELSE
	  BARS  =  0.0
	  GOTO 999
	ENDIF

  999	CONTINUE

D	IF (IDEBUG .GT. 0 ) THEN
D	  WRITE (KLINE$,"(8X,'BARS = ',F12.3)") BARS
D	  CALL TRACEOUT (0,0,0)
D	ENDIF

	RETURN
	END
		 * 
		 */
	}



}
