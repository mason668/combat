package data;

public class A {
	/*
	 * 
	 * List all common files used by janus database
	INCLUDE 	'globparc.f' defines all constants
	INCLUDE 	'globartc.f' defines indirect fire data
	INCLUDE 	'globsysc.f' platform
	INCLUDE 	'globchmc.f'
	INCLUDE		'globcldc.f'
	INCLUDE 	'globcsvc.f'
	INCLUDE		'globengc.f'
	INCLUDE 	'globminc.f'
	INCLUDE		'globnamc.f'
	INCLUDE 	'globnucc.f'
	INCLUDE		'globradc.f'
	INCLUDE 	'globsnsc.f'
	INCLUDE 	'globskrc.f'
	INCLUDE 	'globwpnc.f' Weapon
	INCLUDE 	'globwthc.f'
	INCLUDE		'globmaintc.f'
	INCLUDE 	'glbairdefc.f'
	INCLUDE		'glbflyerc.f' Flyer - done
    INCLUDE     'glbbcisc.f'
	INCLUDE		'globtgmc.f'
	INCLUDE		'globcbrc.f'
	INCLUDE     'globtankerc.f' Tanker data
	INCLUDE     'globfiles.f'
	INCLUDE 	'globsarc.f'

	INTEGER*4	I,JTYPE, ITYPE, IPRESS, ISLOT, J, K, IMINE, IENG
	REAL*4		SIZEMIN

	INTEGER*4	NUMRELWPNS_OLD
	PARAMETER (NUMRELWPNS_OLD = 15)

	REAL*4		CWPN_TIME_SETUP_OLD(NUMRELWPNS_OLD, NUMCSDTYPES)
	REAL*4		CWPN_TIME_PACK_OLD(NUMRELWPNS_OLD, NUMCSDTYPES)
	INTEGER*4	KBLOADC_OLD(NUMRELWPNS_OLD, NUMCSDTYPES)
	INTEGER*4	KSLOADC_OLD(NUMRELWPNS_OLD, NUMCSDTYPES)
	INTEGER*2	KWEAPNC_OLD(NUMRELWPNS_OLD, NUMCSDTYPES)
	INTEGER*2	KUPLDTIMC_OLD(NUMRELWPNS_OLD, NUMCSDTYPES)
	INTEGER*2	SQUADWPNC_OLD(NUMRELWPNS_OLD, NUMCSDTYPES)
	BYTE		KALTWPNC_OLD(NUMRELWPNS_OLD, NUMCSDTYPES)
	BYTE		KAMMO_ONLYC_OLD(NUMRELWPNS_OLD, NUMCSDTYPES)
	REAL*4		WPNXFERTIMEC_OLD(NUMRELWPNS_OLD, NUMCSDTYPES)

	CHARACTER*80	FILENAME$	! COMDAT:COMSYS.DAT

	CHARACTER*1 	ANSW$

	DATA		IBEEP		/ 7 /

	CALL GETFILENAME ( 'COMDAT:', 'COMSYS', -1, -1, FILENAME$ )

        DO I = 80, 1, -1
            IF( FILENAME$(I:I) .NE. ' ' )  THEN
                WRITE (6, 10010) FILENAME$(1:I)
10010           FORMAT( ' Reading file ', A)
                GOTO 100
            ENDIF
        ENDDO

	WRITE (6,"(' ')")
	PRINT *
	PRINT *

  100	CONTINUE

	OPEN (   IFILECSD,	NAME	     =  FILENAME$     ,
     *			ERR	     =  999	      ,
     *			ACCESS	     = 'SEQUENTIAL'   ,
     *			FORM	     = 'UNFORMATTED'  ,
     *			SHARED                        ,
     *			READONLY                      ,
     *			STATUS	     = 'OLD'      )

C***********************************************************************
C
	READ (IFILECSD)
     *      CAIMERRD,
     *      CAIMERRG,
     *      CAMUNREL,
     *      CANGLEOF,
     *      CAPLATIM,
     *      CALAYTIM,
     *      CARELOAD,
     *      CARANGE,
     *      CBALERRD,
     *      CBALERRG,
     *      CEFFICSL,
     *      CEFFICIN,
     *      CSMUNREL,
     *      CTOFARTY,
     *      CRAPLAFC,
     *      CWPHEFAC,
     *      CFLICFAC

C***********************************************************************
C
	READ (IFILECSD)
     *       CANGLTHL,
     *       CARTHELTH,
     *       CARTICLTH,
     *       CARTHEBR,
     *       CTDWNTIM,
     *       CSETUPTIM,
     *       KBOMBLC,
     *       KFASRNDC,
     *       KTUBESC,
     *       KALLOT,
     *       KATRALGC,
     *       KVULCATC,
     *       CHEPKIL,			      ! Arty Kill Cat. Probabilities
     *       CICMPKIL,
     *       CHEPCREW,
     *       CICMPCREW,	
     *	     KARTCALC

C***********************************************************************
C
	READ (IFILECSD)
     *      CTIMEMASK, 
     *      CTIMEALARM,
     *      CDOSAGL1,
     *      CD2MEAN, CD2SIGM,
     *      CD3MEAN, CD3SIGM,
     *      CDOSAGL4,
     *      CRESPNL1, CRESPNL2,
     *      CRESPNL3, CRESPNL4,
     *      CCHEMTRF,
     *      CCHSIGX0, CCHSIGZ0, 
     *      CCHSIGX100, CCHSIGZ100,
     *      CCHALPHA, CCHBETA,
     *      CCHMASS, CCHDENS

C***********************************************************************
C
       READ (IFILECSD)
     *     CSPEEDMOPP,
     *     CFOVMOPP,
     *     CEFFICMOPP,
     *     CPHMOPP,
     *     CCTMPINI,
     *     CCTMPMAX,
     *     CCTMPMIN,
     *     CCLINSL,
     *     CEIMPDN,
     *     CWORKRATE

C***********************************************************************
C
	READ (IFILECSD)
     *      ACLDMAN,
     *      SMKDIMC,
     *      DBRNTIMEC,
     *      PROJDISC,
     *      SMKLASC,      
     *      KLDEXPA,
     *      KCLASS,
     *      KSMOKLODC,
     *      SFUELTANKC,
     *      SFUELRATEC

C***********************************************************************
C
	READ (IFILECSD)
     *      CPMINDUD,
     *      CDETBUT,
     *      CDETUNB,
     *      CDETINF,
     *      CVISDIS,
     *      CAPWLEN,
     *      CAPANGL1, CAPANGL2,
     *      CPKMRANG,
     *      CPKSOPEN,CPKSWOOD,CPKSTOWN,
     *      CXLENDISP,CYLENDISP,
     *      KMWIRESC,
     *      KTYPMINESC

C***********************************************************************
C
	READ (IFILECSD)
     *      CSYSTRACK, CSYSBELLY, CSYSSHADW,
     *      CPATRACK,  CPABELLY,  CPASHADW,
     *      CPKTRACK,  CPKBELLY,  CPKSHADW,
     *      CMINPCREW, CMINPPASS,
     *      CCLRVEL, 
     *      CCLRWID,
     *      CCLRREL,
     *      CCLRSUR

C***********************************************************************
C
	READ (IFILECSD)
     *      CRADS,
     *      RLBLC,
     *      ITFNEUC,
     *      ITFGMAC,
     *      ICEPIC,
     *      IYIELDC,
     *      IERYLDC,
     *      ISC,
     *      IPSIMC,
     *      ICALMC

C***********************************************************************
C
	READ (IFILECSD)
     *      CSENSFOV,
     *	    CCNVRTCPM,
     *      SENCYCC,
     *      SENTMPC, 
     *      CTHRMCON,
     *      COPTCON,
     *      KSENSCLASC

C***********************************************************************
C
	READ (IFILECSD)
     *      CDESGPWR,
     *      CFALSETG,
     *      CFPOUTR,
     *      CFPINNR,
     *      CSEEVERT,
     *      CSEEHORZ,
     *      CSKRFALL,
     *      CSKRTIME,
     *      CSKRRNG,
     *      CSKRSCAN,
     *      CSKRTHRS,
     *      CSKRVEL,
     *      CSMAXTIM, CSMINTIM,
     *      KSKRPNTC,
     *      KSBANDC

C***********************************************************************
C
	READ (IFILECSD)
     *      CPRIMRNG,
     *      CSYSLEN,
     *      CSYSWID,
     *      CSYSHGT,
     *      KCREWC,
     *      KINSYMC,
     *      KMAXVELC,
     *      KELEDISTC,
     *      KSYSHEIC, 
     *      MAXVISC

C***********************************************************************
C
	READ (IFILECSD)
     *      CRNGBRK,
     *      KBLOADC_OLD,
     *      KWEAPNC_OLD,
     *      KUPLDTIMC_OLD,
     *      KWPNRELC,
     *      KALTWPNC_OLD

C***********************************************************************
C
	READ (IFILECSD)
     *      CSENSTYP,
     *      CKANPOP,
     *      CDESGNTR,
     *      KMINDISPC,
     *      CENGNEER,
     *      CFIRERS,
     *      CFLYERS,
     *      CLOGSTCS,
     *      CMOVERS,
     *      CRADARS,
     *      KSMKDISPC,
     *      CSURVEIL,
     *      CSWIMMRS,
     *      CKANHOST,
     *      CKFIRPRI,
     *      CSYMRECOG,
     *      KMEDICC,
     *      KREPAIRC,
     *      KCREWMENC

C***********************************************************************
C
	READ (IFILECSD)
     *      CKPOLCARRY,
     *      KSYSWGTC,
     *      KSYSVOLC,
     *      KCRYWGTC,
     *      KCRYVOLC,
     *      CKAPACITY,
     *      CKONSUMPTN,
     *      CKFUELTYPE

C***********************************************************************
C
	READ (IFILECSD)
     *      CROUNVEL,
     *      CTIMEFIR,
     *      CTIMLOAD,
     *      CTIMELAY,
     *      CSSKPMIN,
     *      KFIRMOVC,
     *      KGUIDEC,
     *      KPULLSC,
     *      KROUNDSC,
     *      KRITALTC,
     *      KWPNSNSRC

C***********************************************************************
C
	READ (IFILECSD)
     *      CSDPHC,
     *      CSDPKC

C***********************************************************************
C
	READ (IFILECSD)
     *      CCEILING,
     *      CRELHUMD,
     *      CTEMPER,
     *      CVISBLT,
     *      CWINDIR,
     *      CWINDSP,
     *      EXTINCC,
     *      SUNANGC,
     *      SOGBRTC,
     *      KAIRMSC,
     *      KXSCALC,
     *      INVERSNC,
     *      KLITLEVLC

C***********************************************************************
C
	READ (IFILECSD)
     *      CEDELAY,
     *      CAVLBDLAY,
     *      CHULK1SIZE,
     *      CHULK2SIZE,
     *      COBSDETDIST

C***********************************************************************
C
	READ (IFILECSD)
     *      CFLYALTUD,
     *      CFLYVELOC,
     *      CHEIMAST,
     *      CPOPTIM,
     *      KFLYCLASSC

C***********************************************************************
C
	READ (IFILECSD)
     *      CADSCAN,
     *      CADMINVEL,
     *      CADTRKTIM,
     *      CADLAUTIM,
     *      CADTIMACQ,
     *      CADRANG,
     *      CADSRCH,
     *      KADCLASSC

C***********************************************************************
C
	READ (IFILECSD)
     *      CPDFRPR,
     *      CPIFRPR,
     *      CPMIRPR,
     *      CFIXTIME

C***********************************************************************
C
	READ (IFILECSD)
     *      KADTRACKC,
     *      KFUSLSETC,
     *      KROTRCAPC,
     *      KFLYFUSEC,
     *      KFLYROTRC,
     *      CADROTRAD,
     *      CADPTRACK,
     *      CADRTRACQ,
     *      CADALTUDE,
     *      CADVELCTY

C***********************************************************************
C
	READ (IFILECSD)
     *      CFUSELRCS,
     *      CROTORRCS,
     *      CADFPOWER,
     *      CADFEOTIM,
     *      KADFRESPC,
     *      KJAMREFFC,
     *      CPOWERJAM,
     *      CANGLEJAM,
     *      CEFFEXJAM,
     *      CRDDBDOWN,
     *      KJAMSIMLC,
     *      KFUSEJAMC

C***********************************************************************
C
	READ (IFILECSD)
     *      CBCIPROTIM,
     *      CBCIBEMWID,
     *      CBCIRNGGAT,
     *      KBCISTYPEC,
     *      KBCISFUNCC

C***********************************************************************
C
	READ (IFILECSD)
     *      CTGMPACQ,
     *      CTGMPHIT,
     *      CTGMPKIL,
     *      CTGMCREW,
     *      CTGMMREL,
     *      CTGMSMREL,
     *      CTGMFALS,
     *      KTGMTYPC,
     *      KTGMSUBMC,
     *	    KTGMEFFRC,
     *	    KTGMOFFSETC

C***********************************************************************
C
        READ (IFILECSD)
     *      CBRRANGEC,
     *      CBRSCANC,
     *      CBRUPTIMEC,
     *      CBRDNTIMEC,
     *      CBRFACTORC

C***********************************************************************
C       READ COMMON BLOCK 'GLOBTANKERC'

        READ (IFILECSD)
     *      CPUMPRATE,
     *      KPOLCARRYC,
     *	    KLGPOLTYPC,
     *	    KTANKRFNCC,
     *	    KNOZZLESC

	READ (IFILECSD, ERR=501)
     *		MAX_OBSC,
     *		CFOVMIN,
     *		CFOVMAX

	READ (IFILECSD, ERR=502)
     *      CSDPHPASC,
     *      CSDPKPASC

	READ (IFILECSD, ERR=503)
     *		SUPPHFCTC,
     *		SUPPDFCTC,
     *		SUPMVFCTC,
     *		WPNSUPTIMC

	READ (IFILECSD, ERR=505)
     *		SAR_TYPE_LIST_C,
     *		SAR_DETECTION_TABLE_C,
     *		SAR_SENSOR_TARGET_C,
     *          FALSE_TARGET_PROB_C,
     *          SAR_PD_TRRN_C,
     *          SAR_PD_WTHR_C

	READ (IFILECSD, ERR=506)
     *		SQUADWPNC_OLD,
     *		CRAWLSPEEDC,
     *		CRAWLSIZEC,
     *		DETECTSIZEC,
     *		PARTIALSIZEC,
     *		DEFILADESIZEC,
     *		WEAPON_RANGEC,
     *		WEAPON_MAX_ALTITUDEC,
     *		STOP_ENGINEC

	READ (IFILECSD, ERR=507)
     *		SYMSIZEC

	READ (IFILECSD, ERR=508)
     *		KILLRADC,
     *		SUPPRADC,
     *		CSLOPE_DATA,
     *		CBEATEN_X,
     *		CBEATEN_Y

	READ (IFILECSD, ERR=509)
     *		KAMMO_ONLYC_OLD,
     *		CWPN_TIME_SETUP_OLD,
     *		CWPN_TIME_PACK_OLD

	READ (IFILECSD, ERR=510)
     *		MOVING_ONLYC,
     *		MIN_SNSR_SPEEDC,
     *		DETECT_AIRCRAFTC

	READ (IFILECSD, ERR=511)
     *		CSYSENSORS

	READ (IFILECSD, ERR=512)
     *		CSYSENSHI,
     *		CSYSHEIGHT ! not used anymore

	READ (IFILECSD, ERR=513)
     *		CSYSMCLR_WIDTH,
     *		CSYSMCLR_LENGTH,
     *		CSYSMCLR_SPEED,
     *		CSYSMCLR_RELIABILITY,
     *		CSYSMCLR_SURVIVABILITY,
     *		CSYSMCLR_DETECTION,
     *		CSYSMCLR_DISTANCE,
     *		CSYSMCLR_MODE,
     *		CSYSMCLR_DEVICES,
     *		CFORDING_DEPTH

	READ (IFILECSD,ERR=514)
     *		CPANEL_WIDTH,
     *		CPANEL_DEPTH,
     *		CPANEL_SYMBOL,
     *		CPANEL_DESTRUCT,
     *		CPANEL_MINE_TYPE,
     *		CPANEL_MINES_PER_ROW,
     *		CPANEL_ROW_START,
     *		CPANEL_ROW_END,
     *		CPANEL_ROW_OFFSET,
     *		CPANEL_ROW_PATTERN,
     *		CMINE_DESTRUCT,
     *		CMINE_DESTRUCT_PROB

	READ (IFILECSD,ERR=515)
     *		CPANEL_DIMENSION,
     *		MINE_DISPENSERC,
     *		MINE_DELAYC,
     *		MINEFIELD_LOADC,
     *		CFASCAM_WIDTH,
     *		CFASCAM_DEPTH,
     *		FASCAM_MINEC,
     *		FASCAM_TYPEC,
     *		CARTY_PLAN_TIME,
     *		CARTY_LAY_TIME,
     *		CARTY_RELOAD_TIME,
     *		CACTIVE_SENSOR,
     *		CKAN_DETECT_SENSOR

	READ (IFILECSD,ERR=516)
     *		SPEED_REVERSEC,
     *		SPEED_COUNTRYC

	READ (IFILECSD,ERR=517)
     *		PIT_PHC,
     *		PIT_PKC,
     *		ARTILLERY_VULNERABILITYC,
     *		WEAPON_MIN_ALTITUDEC,
     *		PITSIZEC

	READ (IFILECSD,ERR=518)
     *		SYS_POINT_DEFENCEC,
     *		SYS_POINT_DEFENCE_ROUNDSC,
     *		SYS_POINT_DEFENCE_STOREDC,
     *		POINT_DEFENCE_TRIGGERC,
     *		POINT_DEFENCE_EFFECTC,
     *		SYS_ARMOURC,
     *		SYS_ARMOUR_PANELSC,
     *		ARMOUR_TRIGGERC,
     *		ARMOUR_EFFECTC,
     *		ARMOUR_DESTROYEDC,
     *		KSMOKSTORC

	READ (IFILECSD,ERR=519)
     *		CTARGET_HEIGHT,
     *		CPOSTURE_HEIGHT

	READ (IFILECSD,ERR=520)
     *		LOS_MAX_THRESHHOLDC, LOS_MIN_THRESHHOLDC, LOS_FACTORC,
     *		WIRE_MAX_THRESHHOLDC, WIRE_MIN_THRESHHOLDC, WIRE_FACTORC

	READ (IFILECSD,ERR=521)
     *		KSLOADC_OLD, WPNXFERTIMEC_OLD, CFOVPRIMARY

	READ (IFILECSD,ERR=522)
     *		OBST_CLEAR_TIMEC,
     *		OBST_CROSS_TIMEC,
     *		OBST_PMOBC,
     *		OBSTACLE_DET_DIMENSIONC,
     *		OBSTACLE_DEFAULT_STATEC,
     *		OBSTACLE_MODEC,
     *		SPEED_RUNNINGC,
     *		CDF_SHEAF_X,
     *		CDF_SHEAF_Y,
     *		CDF_PRE_FIRE,
     *		CDF_POST_FIRE,
     *		CDF_NEW_TARGET,
     *		CDETECT_PRIORITY,
     *		SYSTEM_RECOGC,
     *		SYSTEM_IDENTC

	READ (IFILECSD,ERR=523)
     *		CWPN_TIME_SETUP,
     *		CWPN_TIME_PACK,
     *		KBLOADC,
     *		KSLOADC,
     *		WPNXFERTIMEC,
     *		KWEAPNC,
     *		KUPLDTIMC,
     *		SQUADWPNC,
     *		KALTWPNC,
     *		KAMMO_ONLYC

	READ (IFILECSD,ERR=524)
     *		CSDPKPASKILLC

	READ (IFILECSD,ERR=525)
     *		BRIDGE_LIMIT_VEHICLESC,
     *		BRIDGE_LIMIT_FOOTC,
     *		BRIDGE_LIMIT_WEIGHTC,
     *		BRIDGE_LENGTHC,
     *		BRIDGE_SETUP_TIMEC,
     *		BRIDGE_PACKUP_TIMEC,
     *		BRIDGE_VUL_CLASSC,
     *		BRIDGE_LAYERC,
     *		BRIDGE_CROSS_TIMEC

	READ (IFILECSD,ERR=526)
     *		MAXVISC_DND

	READ (IFILECSD,ERR=527)
     *		KMAXVELC_DND,
     *		SPEED_COUNTRYC_DND

	READ (IFILECSD,ERR=528)
     *		LASER_RANGEC


499	CONTINUE

C***********************************************************************

	CLOSE (UNIT=IFILECSD)

C***********************************************************************

        DO  JTYPE = 1, NUMCSDTYPES
            DO  ITYPE = 1, NUMCSDTYPES

                IF( (KWPNRELC(1,ITYPE,JTYPE) .LT. 0) .OR. 
     *		    (KWPNRELC(1,ITYPE,JTYPE) .GT. NUMRELWPNS))  THEN
                    TYPE *,' ERROR:  ITYPE, JTYPE =', ITYPE,JTYPE
                    KWPNRELC(1,ITYPE,JTYPE)  =  0
                    IPRESS  =  1
                ENDIF

                IF( (KWPNRELC(2,ITYPE,JTYPE) .LT. 0) .OR.
     *              (KWPNRELC(2,ITYPE,JTYPE) .GT. NUMRELWPNS))  THEN
                    TYPE *,' ERROR:  ITYPE, JTYPE =', ITYPE,JTYPE
                    KWPNRELC(2,ITYPE,JTYPE)  =  0
                    IPRESS  =  1
                ENDIF

            ENDDO
        ENDDO

        DO  ITYPE = 1, NUMCSDTYPES
            DO  ISLOT = 1, NUMRELWPNS

                IF( KALTWPNC(ISLOT,ITYPE) .LT. 0 )  THEN
                    TYPE *,' ERROR:  ITYPE, ISLOT =', ITYPE,ISLOT
                    KALTWPNC(ISLOT,ITYPE)  =  0
                    IPRESS  =  1
                ENDIF

                IF( CMOVERS(ITYPE) .EQ. 4 )    CMOVERS(ITYPE)   =   0
                IF( KSYSHEIC(ITYPE) .GT. 99 )  KSYSHEIC(ITYPE)  =  99

            ENDDO
        ENDDO

	IF( IPRESS .EQ. 1 )  THEN
	    TYPE *
	    CALL PRESSRET
	ENDIF

	WRITE (6,10020)
10020	FORMAT( ' Finished.' )

1	RETURN

501	CONTINUE

	print *,'Initialising sensor resolution level'

	DO I=1, NUMCSDSENS
	  MAX_OBSC(I) = 4
	ENDDO

	DO I=1, NUMCSDTYPES
c		set min and max FOV for each system
	  CFOVMIN(I) = 30
	  CFOVMAX(I) = 360
	ENDDO

502	CONTINUE

	print *,'Initialising passenger PH/PK pointers'

503	CONTINUE
	DO I=1, NUMCSDTYPES
	  IF( CMOVERS(I) .NE. 3 ) THEN
	    SUPPHFCTC(I) = 0.5
	    SUPPDFCTC(I) = 0.5
	    SUPMVFCTC(I) = 1.0
	  ELSE
	    SUPPHFCTC(I) = 0.1
	    SUPPDFCTC(I) = 0.1
	    SUPMVFCTC(I) = 0.0
	  ENDIF
	ENDDO

	DO J=1, NUMCSDWPNS
	  DO I=1, NUMARTVULS
	    WPNSUPTIMC(J,I) = 30 ! seconds
	  ENDDO
	ENDDO

505	CONTINUE

	PRINT *,'Initialising MMR data'

	DO I=1, MAX_SAR_TYPES_C
	  SAR_TYPE_LIST_C(I).SAR_TYPE = I
	  SAR_TYPE_LIST_C(I).NAME = 'MMR'
	  SAR_TYPE_LIST_C(I).MAX_SARDETECT_SPEED = 10
	  SAR_TYPE_LIST_C(I).MIN_GMTIDETECT_SPEED = 5
	  SAR_TYPE_LIST_C(I).MAX_GMTIDETECT_SPEED = 25
	  SAR_TYPE_LIST_C(I).STRIPMAP_WIDTH = 20
	  SAR_TYPE_LIST_C(I).STRIPMAP_HEIGHT = 5
	  SAR_TYPE_LIST_C(I).SPOTLIGHT_RADIUS = 10
	  SAR_TYPE_LIST_C(I).SAR_DISTANCE = 25
	  SAR_TYPE_LIST_C(I).SAR_BEARING = 0.0
	ENDDO

	DO I=1, MAX_SAR_PD_TABLES_C
	  SAR_DETECTION_TABLE_C(I).PD_NUMBER = I
	  SAR_DETECTION_TABLE_C(I).SAR_SPOTLIGHT = 0.8
	  SAR_DETECTION_TABLE_C(I).SAR_SPOTLIGHT_DEFL = 0.6
	  SAR_DETECTION_TABLE_C(I).SAR_SPOTLIGHT_DET = 0.2
	  SAR_DETECTION_TABLE_C(I).SAR_SPOTLIGHT_REC = 0.1
	  SAR_DETECTION_TABLE_C(I).SAR_STRIPMAP = 0.6
	  SAR_DETECTION_TABLE_C(I).SAR_STRIPMAP_DEFL = 0.4
	  SAR_DETECTION_TABLE_C(I).GMTI_SPOTLIGHT = 0.9
	  SAR_DETECTION_TABLE_C(I).GMTI_STRIPMAP = 0.3
	ENDDO

	DO I=1, MAX_SAR_TYPES_C
	  DO J=1, NUMCSDTYPES
	    SAR_SENSOR_TARGET_C(I,J) = 1
	  ENDDO
	  DO J=1, NUMSAR_STATES_C
	    DO K=1, NUMCSDTRRN
	      SAR_PD_TRRN_C(I, K, J) = 1.0
	    ENDDO
	    DO K=1, NCSDWTHR
	      SAR_PD_WTHR_C(I, K, J) = 1.0
	    ENDDO
	  ENDDO
	  DO J=1, NUMCSDTRRN
	    FALSE_TARGET_PROB_C(I, J) = 1.0
	  ENDDO
	ENDDO

506	CONTINUE
	! defaults for weapon range, altitide, crawlspeed, size
	! squad weapon

	print *,'setting squad weapon to defaults'

	DO I=1, NUMCSDTYPES
	DO J=1, NUMRELWPNS
	  SQUADWPNC(J,I) = 0
	ENDDO
	ENDDO

	DO I=1, NUMCSDTYPES
	  CRAWLSPEEDC(I) = FLOAT(KMAXVELC(I)) / 10.0
	  SIZEMIN = CSYSLEN(I)
	  IF ( CSYSWID(I) .LT. SIZEMIN) SIZEMIN = CSYSWID(I)
	  IF ( CSYSHGT(I) .LT. SIZEMIN) SIZEMIN = CSYSHGT(I)
	  CRAWLSIZEC(I) = SIZEMIN * 0.33
	  DETECTSIZEC(I) = SIZEMIN
	  PARTIALSIZEC(I) = SIZEMIN * 0.33
	  DEFILADESIZEC(I) = SIZEMIN * 0.2

	  STOP_ENGINEC(I) = 5.0 ! stop engine after 5 minutes
	ENDDO

	DO J=1, NUMCSDWPNS
	  WEAPON_RANGEC(J) = 10.0
	  WEAPON_MAX_ALTITUDEC(J) = 5000.0
	ENDDO

507	CONTINUE

	DO I=1, NUMCSDTYPES
	  SYMSIZEC(I) = 1
	ENDDO

508	CONTINUE
	PRINT *,'Setting default DF radii'

	DO J = 1, NUMCSDWPNS
	  DO I=1,NUMARTVULS
	    WPNSUPTIMC(J,I) = 30
	    SUPPRADC(J,I)   = 2.0
	    KILLRADC(J,I)   = 0.5
	  ENDDO
	ENDDO

	PRINT *,'Setting slope data'

	DO I=1, NUMCSDTYPES

	  CSLOPE_DATA(I,1) = 0.15
	  CSLOPE_DATA(I,3) = 0.49
	  CSLOPE_DATA(I,2) = -0.02

	  IF( CMOVERS(I) .EQ. 3 ) THEN
	    CSLOPE_DATA(I,3) = 0.59
	    CSLOPE_DATA(I,2) = -0.01667
	  ENDIF
	  IF( CMOVERS(I) .EQ. 1 ) THEN
	    CSLOPE_DATA(I,3) = 0.59
	    CSLOPE_DATA(I,2) = -0.01667
	  ENDIF

	ENDDO

	PRINT *,'Setting wpn beaten zones'

	DO J=1, NUMCSDWPNS
	  CBEATEN_X(J) = 50.0
	  CBEATEN_Y(J) = 150.0
	ENDDO

509	CONTINUE
	PRINT *,'Initiallising ammo only flag'

510	CONTINUE
	PRINT *,'Initialising sensor moving only flag'

511	CONTINUE
	PRINT *,'Initialising system sensors'

	DO I=1, 3
	  DO J = 1, NUMCSDTYPES
	    CSYSENSORS(I,J) = CSENSTYP(I,J)
	  ENDDO
	ENDDO

512	CONTINUE
	PRINT *,'Initialising system sensors heights'

	DO J = 1, NUMCSDTYPES
	  DO I=1, 8
	    CSYSENSHI(I,J) = KSYSHEIC(J)
	  ENDDO
	  DO I=1, 10
	    CSYSHEIGHT(J,I) = CSYSHGT(J)
	  ENDDO
	ENDDO

513	CONTINUE
	PRINT *,'Initialising system mine clearing data'

	DO J = 1, NUMCSDTYPES
	  CFORDING_DEPTH(J) = 0.0 ! cant ford at all
	  CSYSMCLR_WIDTH(J) = 0.001 ! 1m
	  CSYSMCLR_LENGTH(J) = 0.005 ! 5m
	  CSYSMCLR_SPEED(J) = 1.0 ! 1km/hr
	  DO IMINE = 1, NUMAAMC
	    CSYSMCLR_RELIABILITY(IMINE,J) = 0.5
	    CSYSMCLR_SURVIVABILITY(IMINE,J) = 0.5
	    CSYSMCLR_DETECTION(IMINE,J,1) = 0.5
	    CSYSMCLR_DETECTION(IMINE,J,2) = 0.5
	    CSYSMCLR_DISTANCE(IMINE,J,1) = 0.1
	    CSYSMCLR_DISTANCE(IMINE,J,2) = 0.1
	  ENDDO
	  CSYSMCLR_MODE(J) = 1 ! plough
	  CSYSMCLR_DEVICES(J) = 1
	ENDDO

514	CONTINUE
	PRINT *,'Setting minefield panel data to default'

	DO I=1, CNUM_PANEL_TYPES
	  CPANEL_SYMBOL(I) = 0
	  CPANEL_DESTRUCT(I) = 1
	  DO J=1, CNUM_PANEL_ROWS
	    CPANEL_MINE_TYPE(I,J) = 0
	  ENDDO
	ENDDO

	! panel 1 disrupt
	CPANEL_SYMBOL(1) = 1
	CPANEL_WIDTH(1) = 0.250 ! 250m
	CPANEL_DEPTH(1) = 0.100 ! 100m

	CPANEL_MINE_TYPE(1,1) = 1
	CPANEL_MINES_PER_ROW(1,1) = 42
	CPANEL_ROW_START(1,1) = 0.01
	CPANEL_ROW_END(1,1)   = 0.01
	CPANEL_ROW_OFFSET(1,1) = 0.0
	CPANEL_ROW_PATTERN(1,1) = CPANEL_PATTERN_LINE

	CPANEL_MINE_TYPE(1,2) = 2
	CPANEL_MINES_PER_ROW(1,2) = 42
	CPANEL_ROW_START(1,2) = 0.03
	CPANEL_ROW_END(1,2)   = 0.07
	CPANEL_ROW_OFFSET(1,2) = 0.001
	CPANEL_ROW_PATTERN(1,2) = CPANEL_PATTERN_ZIGZAG

	CPANEL_MINE_TYPE(1,3) = 2
	CPANEL_MINES_PER_ROW(1,3) = 42
	CPANEL_ROW_START(1,3) = 0.09
	CPANEL_ROW_END(1,3)   = 0.09
	CPANEL_ROW_OFFSET(1,3) = 0.002
	CPANEL_ROW_PATTERN(1,3) = CPANEL_PATTERN_LINE

	! panel 2 fix

	CPANEL_SYMBOL(2) = 2
	CPANEL_WIDTH(2) = 0.250 ! 250m
	CPANEL_DEPTH(2) = 0.100 ! 100m

	CPANEL_MINE_TYPE(2,1) = 2
	CPANEL_MINES_PER_ROW(2,1) = 21
	CPANEL_ROW_START(2,1) = -0.02
	CPANEL_ROW_END(2,1)   = 0.005
	CPANEL_ROW_PATTERN(2,1) = CPANEL_PATTERN_RANDOM

	CPANEL_MINE_TYPE(2,2) = 1
	CPANEL_MINES_PER_ROW(2,2) = 42
	CPANEL_ROW_START(2,2) = 0.01
	CPANEL_ROW_END(2,2)   = 0.01
	CPANEL_ROW_OFFSET(2,2) = 0.000
	CPANEL_ROW_PATTERN(2,2) = CPANEL_PATTERN_LINE

	CPANEL_MINE_TYPE(2,3) = 2
	CPANEL_MINES_PER_ROW(2,3) = 42
	CPANEL_ROW_START(2,3) = 0.03
	CPANEL_ROW_END(2,3)   = 0.07
	CPANEL_ROW_OFFSET(2,3) = 0.002
	CPANEL_ROW_PATTERN(2,3) = CPANEL_PATTERN_ZIGZAG

	CPANEL_MINE_TYPE(2,4) = 2
	CPANEL_MINES_PER_ROW(2,4) = 42
	CPANEL_ROW_START(2,4) = 0.09
	CPANEL_ROW_END(2,4)   = 0.09
	CPANEL_ROW_OFFSET(2,4) = 0.003
	CPANEL_ROW_PATTERN(2,4) = CPANEL_PATTERN_LINE

	! panel 3 turn

	CPANEL_SYMBOL(3) = 3
	CPANEL_WIDTH(3) = 0.500 ! 500m
	CPANEL_DEPTH(3) = 0.300 ! 300m

	CPANEL_MINE_TYPE(3,1) = 1
	CPANEL_MINES_PER_ROW(3,1) = 84
	CPANEL_ROW_START(3,1) = 0.010
	CPANEL_ROW_END(3,1)   = 0.010
	CPANEL_ROW_OFFSET(3,1) = 0.001
	CPANEL_ROW_PATTERN(3,1) = CPANEL_PATTERN_LINE

	CPANEL_MINE_TYPE(3,2) = 1
	CPANEL_MINES_PER_ROW(3,2) = 84
	CPANEL_ROW_START(3,2) = 0.02
	CPANEL_ROW_END(3,2)   = 0.12
	CPANEL_ROW_OFFSET(3,2) = 0.002
	CPANEL_ROW_PATTERN(3,2) = CPANEL_PATTERN_ZIGZAG

	CPANEL_MINE_TYPE(3,3) = 2
	CPANEL_MINES_PER_ROW(3,3) = 84
	CPANEL_ROW_START(3,3) = 0.14
	CPANEL_ROW_END(3,3)   = 0.14
	CPANEL_ROW_OFFSET(3,3) = 0.003
	CPANEL_ROW_PATTERN(3,3) = CPANEL_PATTERN_LINE

	CPANEL_MINE_TYPE(3,4) = 2
	CPANEL_MINES_PER_ROW(3,4) = 84
	CPANEL_ROW_START(3,4) = 0.16
	CPANEL_ROW_END(3,4)   = 0.16
	CPANEL_ROW_OFFSET(3,4) = 0.000
	CPANEL_ROW_PATTERN(3,4) = CPANEL_PATTERN_LINE

	CPANEL_MINE_TYPE(3,5) = 1
	CPANEL_MINES_PER_ROW(3,5) = 84
	CPANEL_ROW_START(3,5) = 0.18
	CPANEL_ROW_END(3,5)   = 0.28
	CPANEL_ROW_OFFSET(3,5) = 0.001
	CPANEL_ROW_PATTERN(3,5) = CPANEL_PATTERN_ZIGZAG

	CPANEL_MINE_TYPE(3,6) = 2
	CPANEL_MINES_PER_ROW(3,6) = 84
	CPANEL_ROW_START(3,6) = 0.29
	CPANEL_ROW_END(3,6)   = 0.29
	CPANEL_ROW_OFFSET(3,6) = 0.002
	CPANEL_ROW_PATTERN(3,6) = CPANEL_PATTERN_LINE

	! panel 4 block

	CPANEL_SYMBOL(4) = 4
	CPANEL_WIDTH(4) = 0.500 ! 500m
	CPANEL_DEPTH(4) = 0.300 ! 300m

	CPANEL_MINE_TYPE(4,1) = 2
	CPANEL_MINES_PER_ROW(4,1) = 42
	CPANEL_ROW_START(4,1) = -0.02
	CPANEL_ROW_END(4,1)   = 0.005
	CPANEL_ROW_OFFSET(4,1) = 0.000
	CPANEL_ROW_PATTERN(4,1) = CPANEL_PATTERN_RANDOM

	CPANEL_MINE_TYPE(4,2) = 1
	CPANEL_MINES_PER_ROW(4,2) = 84
	CPANEL_ROW_START(4,2) = 0.010
	CPANEL_ROW_END(4,2)   = 0.010
	CPANEL_ROW_OFFSET(4,2) = 0.001
	CPANEL_ROW_PATTERN(4,2) = CPANEL_PATTERN_LINE

	CPANEL_MINE_TYPE(4,3) = 1
	CPANEL_MINES_PER_ROW(4,3) = 84
	CPANEL_ROW_START(4,3) = 0.02
	CPANEL_ROW_END(4,3)   = 0.12
	CPANEL_ROW_OFFSET(4,3) = 0.001
	CPANEL_ROW_PATTERN(4,3) = CPANEL_PATTERN_ZIGZAG

	CPANEL_MINE_TYPE(4,4) = 2
	CPANEL_MINES_PER_ROW(4,4) = 84
	CPANEL_ROW_START(4,4) = 0.14
	CPANEL_ROW_END(4,4)   = 0.14
	CPANEL_ROW_OFFSET(4,4) = 0.002
	CPANEL_ROW_PATTERN(4,4) = CPANEL_PATTERN_LINE

	CPANEL_MINE_TYPE(4,5) = 2
	CPANEL_MINES_PER_ROW(4,5) = 84
	CPANEL_ROW_START(4,5) = 0.16
	CPANEL_ROW_END(4,5)   = 0.16
	CPANEL_ROW_OFFSET(4,5) = 0.003
	CPANEL_ROW_PATTERN(4,5) = CPANEL_PATTERN_LINE

	CPANEL_MINE_TYPE(4,6) = 1
	CPANEL_MINES_PER_ROW(4,6) = 84
	CPANEL_ROW_START(4,6) = 0.18
	CPANEL_ROW_END(4,6)   = 0.28
	CPANEL_ROW_OFFSET(4,6) = 0.000
	CPANEL_ROW_PATTERN(4,6) = CPANEL_PATTERN_ZIGZAG

	CPANEL_MINE_TYPE(4,7) = 2
	CPANEL_MINES_PER_ROW(4,7) = 84
	CPANEL_ROW_START(4,7) = 0.29
	CPANEL_ROW_END(4,7)   = 0.29
	CPANEL_ROW_OFFSET(4,7) = 0.001
	CPANEL_ROW_PATTERN(4,7) = CPANEL_PATTERN_LINE

	CALL PRESSRET

515	CONTINUE

	DO I=1, CNUM_PANEL_TYPES
	  CPANEL_DIMENSION(I) = 1.0
	ENDDO

	DO ITYPE = 1, NUMCSDTYPES
	  SPEED_REVERSEC(ITYPE) = KMAXVELC(ITYPE) * 0.25
	  IF ( KMINDISPC(ITYPE) .GT. 0 ) THEN
	    DO I=1,3
	      MINE_DISPENSERC(ITYPE,I) = KMINDISPC(ITYPE)
	      MINE_DELAYC(ITYPE,I) = 60.00 ! seconds
	      MINEFIELD_LOADC(ITYPE,I) = 3 ! number of panels that can be deployed
	    ENDDO
	  ELSE
	    DO I=1,3
	      MINE_DISPENSERC(ITYPE,I) = 0
	      MINE_DELAYC(ITYPE,I) = 0.0
	      MINEFIELD_LOADC(ITYPE,I) = 0
	    ENDDO
	  ENDIF
	ENDDO

	DO I=1, NUMFASCAMC
	  CFASCAM_WIDTH(I) = I * 0.1
	  CFASCAM_DEPTH(I) = I * 0.05
	  FASCAM_MINEC(I) = I * 100
	  FASCAM_TYPEC(I) = I
	ENDDO

	DO ITYPE = 1, NUMCSDINDF
	  DO I = 1, NUMPROJ
	    CARTY_PLAN_TIME(ITYPE,I) = CAPLATIM(ITYPE)
	    CARTY_LAY_TIME(ITYPE,I) = CALAYTIM(ITYPE)
	    CARTY_RELOAD_TIME(ITYPE,I) = CARELOAD(ITYPE)
	  ENDDO
	ENDDO

C	  DO IWPN = 1, NWEAPNS
C	    ! average rof
C	    RATE_OF_FIRE(IWPN, ISIDE, 1, 1) = TIMELAY(IWPN,ISIDE)
C	    RATE_OF_FIRE(IWPN, ISIDE, 2, 1) = TIMEFIR(IWPN,ISIDE)
C
C	    ! fast rof
C	    RATE_OF_FIRE(IWPN, ISIDE, 1, 2) = TIMELAY(IWPN,ISIDE) * 0.5
C	    RATE_OF_FIRE(IWPN, ISIDE, 2, 2) = TIMEFIR(IWPN,ISIDE) * 0.5
C
C	    ! slow rof
C	    RATE_OF_FIRE(IWPN, ISIDE, 1, 3) = TIMELAY(IWPN,ISIDE) * 2.0
C	    RATE_OF_FIRE(IWPN, ISIDE, 2, 3) = TIMEFIR(IWPN,ISIDE) * 2.0
C	  ENDDO

516	CONTINUE

	DO ITYPE = 1, NUMCSDTYPES
	  IF( CMOVERS(ITYPE) .NE. 3 )  THEN
	    SPEED_REVERSEC(ITYPE) = FLOAT(KMAXVELC(ITYPE)) *0.25
	    SPEED_COUNTRYC(ITYPE) = FLOAT(KMAXVELC(ITYPE)) *0.75
	  ELSE
	    SPEED_REVERSEC(ITYPE) = 0
	    SPEED_COUNTRYC(ITYPE) = FLOAT(KMAXVELC(ITYPE))
	  ENDIF
	ENDDO

517	CONTINUE

	DO I=1, NUMCSDWPNS
	  WEAPON_MIN_ALTITUDEC(I) = 0.0
	  WEAPON_MAX_ALTITUDEC(I) = 5000.0
	  DO J=1, NUMARTVULS
	    PIT_PHC (I,J) = 1.0
	    PIT_PKC (I,J) = 1.0
	  ENDDO
	ENDDO

	DO I=1, NUMCSDTYPES
	  PITSIZEC(I) = PARTIALSIZEC(I)

	  ARTILLERY_VULNERABILITYC(1,I) = KVULCATC(1,I)
	  ARTILLERY_VULNERABILITYC(2,I) = KVULCATC(2,I)
	  ARTILLERY_VULNERABILITYC(3,I) = KVULCATC(2,I)
	  ARTILLERY_VULNERABILITYC(4,I) = KVULCATC(2,I)
	  DO J=5, 10
	    ARTILLERY_VULNERABILITYC(J,I) = 0
	  ENDDO
	ENDDO

518	CONTINUE

	PRINT *,'Initialising active defence systam data'

	DO I=1, NUMCSDTYPES
	  SYS_POINT_DEFENCEC(I) = 0
	  SYS_POINT_DEFENCE_ROUNDSC(I) = 0
	  SYS_POINT_DEFENCE_STOREDC(I) = 0
	ENDDO

	DO I=1, NUMCSDTYPES
	  SYS_ARMOURC(I) = 0
	  SYS_ARMOUR_PANELSC(I) = 0
	  IF ( KSMOKLODC(I) .GT. 0 ) THEN
	    KSMOKSTORC(I) = 2
	  ELSE
	    KSMOKSTORC(I) = 0
	  ENDIF
	ENDDO

	DO I=1, NUM_POINT_DEFENCE_TYPEC
	  DO J=1, NUMCSDWPNS
	    POINT_DEFENCE_TRIGGERC(I,J) = 0.0
	    POINT_DEFENCE_EFFECTC(I,J) = 0.0
	  ENDDO
	ENDDO

	DO I=1, NUM_ARMOUR_TYPEC
	  DO J=1, NUMCSDWPNS
	    ARMOUR_TRIGGERC(I,J) = 0.0
	    ARMOUR_EFFECTC(I,J) = 0.0
	    ARMOUR_DESTROYEDC(I,J) = 0
	  ENDDO
	ENDDO

519	CONTINUE

	PRINT *,'Initialising system sensors heights'

	DO I = 1, NUMCSDTYPES
	  CPOSTURE_HEIGHT(I,0) = KSYSHEIC(I)
	  CTARGET_HEIGHT(I,0) = KSYSHEIC(I)
	  CPOSTURE_HEIGHT(I,1) = KSYSHEIC(I) * 0.5
	  CTARGET_HEIGHT(I,1) = KSYSHEIC(I) * 0.5
	  CPOSTURE_HEIGHT(I,2) = KSYSHEIC(I) * 0.2
	  CTARGET_HEIGHT(I,2) = KSYSHEIC(I) * 0.2
	  CPOSTURE_HEIGHT(I,3) = KSYSHEIC(I) * 0.2
	  CTARGET_HEIGHT(I,3) = KSYSHEIC(I) * 0.2
	  DO J=4, 10
	    CPOSTURE_HEIGHT(I,J) = 0.0
	    CTARGET_HEIGHT(I,J) = 0.0
	  ENDDO
	ENDDO

520	CONTINUE

	DO I= 1, NUMCSDWPNS
	  DO J=1, 2
	  LOS_MAX_THRESHHOLDC(I,J) = 0.0
	  LOS_MIN_THRESHHOLDC(I,J) = 0.0
	  LOS_FACTORC(I,J) = 1.0
	  WIRE_MAX_THRESHHOLDC(I,J) = 0.0
	  WIRE_MIN_THRESHHOLDC(I,J) = 0.0
	  WIRE_FACTORC(I,J) = 1.0
	ENDDO
	ENDDO

521	CONTINUE

	DO I=1, NUMCSDTYPES
	  CFOVPRIMARY(I) = CFOVMAX(I)
	ENDDO

522	CONTINUE

	DO I=1, NUMCSDTYPES
	  DO J=1, NCSDWTHR
	  DO K= 1, MAX_OBST_TYPEC
	    OBST_CLEAR_TIMEC(J,I,K) = 40
	    OBST_CROSS_TIMEC(J,I,K) = 15
	    OBST_PMOBC(J,I,K) = 0.8
	  ENDDO
	  ENDDO

	  SYSTEM_RECOGC(I) = 3.5
	  SYSTEM_IDENTC(I) = 6.4
	  SPEED_RUNNINGC(I) = FLOAT(KMAXVELC(I)) * 1.25

	  DO J=1, NUMCSDTYPES
	    CDETECT_PRIORITY(I,J) = CKFIRPRI(I,J)
	  ENDDO
	ENDDO

	DO I= 1, NUMCSDWPNS
	  CDF_SHEAF_X(I, 2) = CBEATEN_X(I)
	  CDF_SHEAF_Y(I, 2) = CBEATEN_Y(I)
	  CDF_SHEAF_X(I, 1) = CBEATEN_X(I)* 0.5
	  CDF_SHEAF_Y(I, 1) = CBEATEN_Y(I) * 0.5
	  CDF_SHEAF_X(I, 3) = CBEATEN_X(I) * 2.0
	  CDF_SHEAF_Y(I, 3) = CBEATEN_Y(I) * 2.0

	  CDF_PRE_FIRE(I, 1) = CTIMEFIR (I)
	  CDF_POST_FIRE(I, 1) = 0.0
	  CDF_NEW_TARGET(I, 1) = CTIMELAY(I)
	  CDF_PRE_FIRE(I, 2) = CTIMEFIR (I) * 2.0
	  CDF_POST_FIRE(I, 2) = 0.0
	  CDF_NEW_TARGET(I, 2) = CTIMELAY(I) * 2.0
	  CDF_PRE_FIRE(I, 3) = CTIMEFIR (I) * 4.0
	  CDF_POST_FIRE(I, 3) = 0.0
	  CDF_NEW_TARGET(I, 3) = CTIMELAY(I) *4.0

	ENDDO

C	PRINT *,'Initialising new obstacle data'

	DO I=1, 5
	  OBSTACLE_DET_DIMENSIONC(I) = 1.0
	  OBSTACLE_DEFAULT_STATEC(I) = 1
	  OBSTACLE_MODEC(I) = 0
	ENDDO

523	CONTINUE
	DO I=1, NUMRELWPNS_OLD
	  DO J=1, NUMCSDTYPES

	    CWPN_TIME_SETUP(I,J) = CWPN_TIME_SETUP_OLD(I,J)
	    CWPN_TIME_PACK(I,J) = CWPN_TIME_PACK_OLD(I,J)
	    KBLOADC(I,J) = KBLOADC_OLD(I,J)
	    KSLOADC(I,J) = KSLOADC_OLD(I,J)
	    KWEAPNC(I,J) = KWEAPNC_OLD(I,J)
	    KUPLDTIMC(I,J) = KUPLDTIMC_OLD(I,J)
	    SQUADWPNC(I,J) = SQUADWPNC_OLD(I,J)
	    KALTWPNC(I,J) = KALTWPNC_OLD(I,J)
	    KAMMO_ONLYC(I,J) = KAMMO_ONLYC_OLD(I,J)
	    WPNXFERTIMEC(I,J) = WPNXFERTIMEC_OLD(I,J)

	  ENDDO
	ENDDO

524	CONTINUE

	DO I = 1, NUMCSDWPNS
	DO J = 1, NUMCSDTYPES
	  CSDPKPASKILLC(I,J)  = CSDPKC(I,J)
	ENDDO
	ENDDO


525	CONTINUE

	DO I=1, NUM_BRIDGE_TYPEC

	  BRIDGE_LIMIT_VEHICLESC(I) = 1
	  BRIDGE_LIMIT_FOOTC(I) = 1
	  BRIDGE_LIMIT_WEIGHTC(I) = -1
	  BRIDGE_LENGTHC(I) = -1
	  BRIDGE_SETUP_TIMEC(I) = 2.5
	  BRIDGE_PACKUP_TIMEC(I) = 3.5
	  BRIDGE_VUL_CLASSC(I) = 28

	  DO J = 1, NUMCSDMOVERS
	    BRIDGE_CROSS_TIMEC(I,J) = 1.0 * I + (j/10.0)
	  ENDDO

	ENDDO


	DO I=1, NUMCSDTYPES
	  DO J=1, NCSDWTHR
	    DO K= 1, MAX_OBST_TYPEC
	      OBST_CLEAR_TIMEC(J,I,K) = -1
	      OBST_CROSS_TIMEC(J,I,K) = -1
	      OBST_PMOBC(J,I,K) = 0.0

	      IF ( K .EQ. 1 .or. K .EQ. 2) THEN ! ditch or wire
	        IENG = CENGNEER(I)
	        IF (CEDELAY(J,K,IENG) .GT. 0 ) THEN
	          OBST_CLEAR_TIMEC(J,I,K) = CEDELAY(J,K,IENG) *60.0
	        ENDIF
	      ENDIF

	      IF ( K .EQ. 7 ) THEN ! ditch or wire
	        OBST_CROSS_TIMEC(J,I,K) = 5.0
	        OBST_CLEAR_TIMEC(J,I,K) = 5.0
	      ENDIF

	    ENDDO
	  ENDDO
	ENDDO

	DO I=1, NUMCSDTYPES
	  IF ( CENGNEER(I) .EQ. 1 ) THEN
	    BRIDGE_LAYERC(I) = 1
	  ENDIF
	ENDDO

526	CONTINUE

	DO J = 1, NUMCSDTYPES
	  MAXVISC_DND(J,1) = MAXVISC(J)
	  MAXVISC_DND(J,2) = MAXVISC(J)! / 2
	  MAXVISC_DND(J,3) = MAXVISC(J)! / 3
	ENDDO

527	CONTINUE

	DO J = 1, NUMCSDTYPES
	  KMAXVELC_DND(J,1) = KMAXVELC(J)
	  KMAXVELC_DND(J,2) = KMAXVELC(J)! / 2
	  KMAXVELC_DND(J,3) = KMAXVELC(J)! / 3
	  SPEED_COUNTRYC_DND(J,1) = SPEED_COUNTRYC(J)
	  SPEED_COUNTRYC_DND(J,2) = SPEED_COUNTRYC(J)
	  SPEED_COUNTRYC_DND(J,3) = SPEED_COUNTRYC(J)
	ENDDO

528	CONTINUE

	DO J = 1, NUMCSDLASERS
	  LASER_RANGEC(J) = 3.0
	ENDDO

529	CONTINUE

599	CONTINUE

	PRINT *
	PRINT *,'New data fields have been initialised'
	PRINT *

	CALL PRESSRET

	GOTO 499

C-----------------------------------------------------------------------
C------ Error performing file open (access conflict)

  999	CONTINUE

	CALL LIB$ERASE_PAGE (1,1)
	PRINT *,'It seems there is a problem opening the file ',FILENAME$
	PRINT *,'If you choose to continue, an empty database will be created.'
	PRINT *,'Otherwise, the program will terminate.'

        CALL READKB ( 3, 4,
     *       ' Continue? (Answer Y/N)',
     *       1, %REF(ANSW$), *1000, 0, 99 )

        IF( ANSW$ .EQ. 'Y' .OR. ANSW$ .EQ. 'y' )  GOTO 1000

	STOP	

 1000   CONTINUE

        RETURN
        END
	 */

}
