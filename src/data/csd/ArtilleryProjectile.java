package data.csd;

/**
 * Define data for projectile types.
 * See globartc.f
 *
 */
public class ArtilleryProjectile {
	public static final int NUMBER_OF_RANGES = 4; // numerrrngs

	/*
	 * 
        REAL*4          CAIMERRD(NUMERRRNGS,NUMPROJ,NUMCSDINDF)
        REAL*4          CAIMERRG(NUMERRRNGS,NUMPROJ,NUMCSDINDF)
     *            CAIMERRD,       ! Deflection aiming errors, by range
     *            CAIMERRG,       ! Range aiming errors, by range
        REAL*4          CANGLEOF(NUMERRRNGS,NUMPROJ,NUMCSDINDF)
        REAL*4          CARTY_PLAN_TIME(NUMCSDINDF, NUMPROJ)
        REAL*4          CARTY_LAY_TIME(NUMCSDINDF, NUMPROJ)
        REAL*4          CARTY_RELOAD_TIME(NUMCSDINDF, NUMPROJ)
     *            CANGLEOF,       ! Angles of fall, in mils, by range
     *            CAPLATIM,       ! Mission Plan time
     *            CALAYTIM,       ! Time to lay on a target
     *            CARELOAD,       ! Delay time between firings

     *		CARTY_PLAN_TIME,	! time to plan mission using this type of ammo
     *		CARTY_LAY_TIME,		! time to lay?
     *		CARTY_RELOAD_TIME,	! time between rounds for this ammo


        REAL*4          CARANGE(NUMERRRNGS,NUMPROJ,NUMCSDINDF)
        REAL*4          CBALERRD(NUMERRRNGS,NUMPROJ,NUMCSDINDF)
        REAL*4          CBALERRG(NUMERRRNGS,NUMPROJ,NUMCSDINDF)
     *            CARANGE,        ! Artillery ranges used for time of flights,
                                  ! fall angles, deflection errors, and range
                                  ! errors
     *            CBALERRD,       ! Ballistic errors in deflection, by range
     *            CBALERRG,       ! Ballistic errors in range, by range
        
        REAL*4          CTOFARTY(NUMERRRNGS,NUMPROJ,NUMCSDINDF)
     *            CTOFARTY,       ! Time of flight, by range & projectile

        INTEGER*2       KALLOT(NUMPROJ,NUMCSDINDF)
     *            KALLOT,         ! Rounds per tube per day
	 */
}
