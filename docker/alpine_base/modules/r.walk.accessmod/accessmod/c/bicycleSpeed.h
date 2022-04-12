
/*-------------------------------------------------------------------             
 *              Bicyle  function 
 *
 * Based on 
 *  speed to watt : http://bikecalculator.com/wattsMetric.html
 *  watt to speed : http://bikecalculator.com/veloMetric.html
 *   
 *-----------------------------------------------------------------*/


#include <stdio.h>
#include <math.h>

#ifndef BICYCLESPEED_H
#define BICYCLESPEED_H


/* Fixed constant */
static const int maxRatio = 2; // as in accessmod 4.0
static const int slopeFlat = 0;
static const int weightRider = 80;
static const int weightBicycle = 15;
static const float resistanceRolling = 0.012; // orig  0.004, 0.005, 0.012
static const float areaFrontal = 0.445 ; // orig 0.388, 0.445, 0.420, 0.300, 0.233, 0.200
static const float speedWind = 0;
static const float temperature = 20;
static const float elevation = 500;
static const float efficiencyTransmission = 0.90;


/* Solve non linear equation */
float newton(float aero, float hw, float tr, float tran, float p) {

  static const int MAX = 10;       // maximum iterations
  float vel = 20;       // Initial guess
  float TOL = 0.05;     // tolerance
  int i ;
  float tv,f,fp,vNew;

  for ( i = 1; i < MAX; i = i + 1) {
    tv = vel + hw;
    f = vel * (aero * tv * tv + tr) - tran * p; // the function
    fp = aero * (3.0 * vel + hw) * tv + tr;     // the derivative
    vNew = vel - f / fp;

    if (fabs(vNew - vel) < TOL) return vNew;  // success

    vel = vNew;
  }
  return 0.0;  // failed to converge
}

/* Actual function to get final speed. 
 *
 * @param speed : expected speed on flat
 * @param slote : slope in degree
 * */
float bicycleSpeed(float speed, float slope)
{ 

  float densityAir = ( 1.293 - 0.00426 * temperature ) * exp( - elevation / 7000.0 );
  float weightTotal = 9.8 * ( weightRider + weightBicycle ); // newton
  float resistanceAir = 0.5 * areaFrontal * densityAir;  // full air resistance parameter


  float speedBike = speed / 3.6;  // converted to m/s;
  float slopeOut = slope * 0.01;

  float resistanceSlopeTireFlat = weightTotal * ( slopeFlat + resistanceRolling ); // gravity and rolling resistance
  float resistanceSlopeTireUp = weightTotal * ( slopeOut + resistanceRolling ); // gravity and rolling resistance
  float speedTotal = speedBike + speedWind; 
  float powerFlat = (speedBike * resistanceSlopeTireFlat + speedBike * speedTotal * speedTotal * resistanceAir) / efficiencyTransmission;

  speedBike = newton(resistanceAir, speedWind, resistanceSlopeTireUp, efficiencyTransmission, powerFlat) * 3.6; 

  return speedBike;

}


#endif /* !BICYCLESPEED_H */
