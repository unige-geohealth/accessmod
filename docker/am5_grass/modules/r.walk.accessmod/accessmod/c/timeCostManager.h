#include <stdio.h>
#include <math.h>
#include "bicycleSpeed.h"

int debug = 0;
float cellNum = 0;

/*-------------------------------------------------------------------             
 *              Bicyle  function 
 *
 * Based on 
 *  speed to watt : http://bikecalculator.com/wattsMetric.html
 *  watt to speed : http://bikecalculator.com/veloMetric.html
 *   
 *-----------------------------------------------------------------*/
double speedBicycle(double speed, double slope)
{
  if( debug == 1 ){
  cellNum = cellNum + 1;
  }

  double speedFinal = bicycleSpeed(speed, slope);

  if( speedFinal >= speed * 2){
    speedFinal  = speed * 2;
  }

  if( speedFinal < 0 ){
    speedFinal  = 0;
  }

  if( debug == 1 ){
  if( cellNum < 100 ){
     printf("Slope = %lf speed = %lf speed final = %lf \n",slope,speed,speedFinal);
   }
  }
  
  return speedFinal;
};
/*------------------------------------------------------------------
 *               hiking function
 *
 * Tobler hiking function. Return expected speed under given slope.
 * based on https://github.com/SrNetoChan/WalkingTime/blob/master/walkingtime.py
 *-----------------------------------------------------------------*/
double speedWalk(double speed, double slope)
{
  /* use fabs instead of abs...*/
  double topSpeed,speedFinal,testVal;
  topSpeed = speed/exp(-0.175); /* -0.175 = -3.5*0.05 */
  speedFinal = exp(-3.5*fabs(slope+0.05))*topSpeed;
  return speedFinal;
};

/*-------------------------------------------------------------------             
 *              Motorized vehicle  function 
 *
 * doesnt take in account slope.
 *   
 *-----------------------------------------------------------------*/
double speedMotor(double speed, double slope)
{
  double speedFinal = speed;
  return speedFinal;
};




/*-------------------------------------------------------------------             
 *              modSwitcher
 *  switch through mod of one cell to determine
 * the appropriate function to use for final speed.
 *
 *-----------------------------------------------------------------*/
double modSwitcher(int mod, double speed, double slope)
{
  double v;
  switch(mod)
  {
    case 1:
      v=speedWalk(speed,slope);
      break;
    case 2:
      v=speedBicycle(speed,slope);
      break;
    case 3:
      v=speedMotor(speed,slope);
      break;
    default: 
      v=0;
  };
  return v;
};

/*-------------------------------------------------------------------             
 *              costManager
 *          -- main function --
 * Check for the group of cells (2 if knightmove is false, 4 otherwise) to
 * extract the mode of transportation and speed from the speed map 
 * ------------- Value of mode 
 * speed map is encoded by step of thousand
 * 1 = walking 
 * 2 = bicycle
 * 3 = motorized 
 * ------------- Additional informations
 * modSpeedAdj1-3 = extracted from speed map. E.g. 2004 -> 2 is for bicycle, 4 the speed in kmh
 * slope = slope in % (r.walk :: check_dtm)
 * dist = distance between cells (r.walk :: E,W,S,N_fac or Diag_fac or V_DIAG_fac ) 
 * total_reviewed = if knight's move, 16, else 8. (r.walk total_reviewed)
 *-----------------------------------------------------------------*/
double costManager(double modSpeed,double modSpeedAdj1,double modSpeedAdj2,double modSpeedAdj3, double slope, double dist, int total_reviewed,int returnPath,double dnullval)
{

  /* if return path == true, slope is negative*/

  if(returnPath==1) slope = -slope;
  /* current cell values */
  int mod       = floor(modSpeed/1e6);
  double speed  = (modSpeed-(mod*1e6))/1e3;
  /* adjacent cell mod of transportations*/
  int modAdj1   = floor(modSpeedAdj1/1e6);
  int modAdj2   = floor(modSpeedAdj2/1e6);
  int modAdj3   = floor(modSpeedAdj3/1e6);
  /* adjacent cell speed in km/h */
  double speedAdj1 = (modSpeedAdj1-(modAdj1*1e6))/1e3;
  double speedAdj2 = (modSpeedAdj2-(modAdj2*1e6))/1e3;
  double speedAdj3 = (modSpeedAdj3-(modAdj3*1e6))/1e3;

  /*G_message("Mod is %d", mod);*/

  /* output var */
  double speedCurrent;
  double costTimeFinal;
  double d2 ;
  double d4 ;

  /* half distance in km*/
  d2 = dist/2000 ;
  d4 = dist/4000 ;

  /*maxVal is the maximum time allowed for crossing a cell : 18.2 hour 
   * it's set to avoid to much depth in final map, e.g. in case of dividing by a speed of zero..*/

  /* get speed for the present cell according to its mode,speed and slope*/
  speedCurrent = modSwitcher(mod,speed,slope);
  /*case of knight's move (3 adjacent cells) there is 4 cell to compute.
   * TODO check for equality between cells mode AND speed to avoid recalculation 
   * at every step. Caution : in the worst case, all 4 cells have different speed
   * AND mode of transportation. A lot of possibilities. */
  if(total_reviewed==16){
    costTimeFinal=(
        1/(speedCurrent/d4)+
        1/(modSwitcher(modAdj1,speedAdj1,slope)/d4)+
        1/(modSwitcher(modAdj2,speedAdj2,slope)/d4)+
        1/(modSwitcher(modAdj3,speedAdj3,slope)/d4)
        ); 
  }else{
     costTimeFinal=(
        1/(speedCurrent/d2)+
        1/(modSwitcher(modAdj1,speedAdj1,slope)/d2)
        ); 
  };

  /* Return cost (s) for the provided distance*/
  /*costTimeFinal=(1/(speedFinal/3.6))*dist;*/
/* if the result is infinity, remove it */
  if(costTimeFinal == INFINITY) costTimeFinal=dnullval ;
  return costTimeFinal*3600;
}

