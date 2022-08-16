#include "bicycleSpeed.h"
#include <math.h>
#include <stdio.h>

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
double speedBicycle(double speed, double slope) {
  if (debug == 1) {
    cellNum = cellNum + 1;
  }

  double speedFinal = bicycleSpeed(speed, slope);

  if (speedFinal >= speed * 2) {
    speedFinal = speed * 2;
  }

  if (speedFinal < 0) {
    speedFinal = 0;
  }

  if (debug == 1) {
    if (cellNum < 100) {
      printf("Slope = %lf speed = %lf speed final = %lf \n", slope, speed,
             speedFinal);
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
double speedWalk(double speed, double slope) {
  /* use fabs instead of abs...*/
  double topSpeed, speedFinal, testVal;
  topSpeed = speed / exp(-0.175); /* -0.175 = -3.5*0.05 */
  speedFinal = exp(-3.5 * fabs(slope + 0.05)) * topSpeed;
  return speedFinal;
};

/*-------------------------------------------------------------------
 *              Motorized vehicle  function
 *
 * doesnt take in account slope.
 *
 *-----------------------------------------------------------------*/
double speedMotor(double speed, double slope) {
  double speedFinal = speed;
  return speedFinal;
};

/*-------------------------------------------------------------------
 *              modSwitcher
 *  switch through mod of one cell to determine
 * the appropriate function to use for final speed.
 *
 *-----------------------------------------------------------------*/
double modSwitcher(int mod, double speed, double slope) {
  double v;
  switch (mod) {
  case 1:
    v = speedWalk(speed, slope);
    break;
  case 2:
    v = speedBicycle(speed, slope);
    break;
  case 3:
    v = speedMotor(speed, slope);
    break;
  default:
    v = 0;
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
 * mod_speed_adj1-3 = extracted from speed map. E.g. 3050000.00 -> 3 is for motor, 
 * at 50kmh slope = slope in % (r.walk :: check_dtm) dist = distance
 * between cells (r.walk :: E,W,S,N_fac or Diag_fac or V_DIAG_fac )
 *
 * Crossing 4 cells = same distance in each cell ( poorly drawn, but it checks) 
 *               ┌───────────┐
 *               │           │
 *               │           │
 *               │     4     │
 *               │     /     │
 *               │    /      │
 *               │   /       │
 *   ┌───────────┼───────────┤
 *   │           │  /        │
 *   │           │ /         │
 *   │     2     │/    3     │
 *   │          /│           │
 *   │         / │           │
 *   │        /  │           │
 *   ├───────────┼───────────┘
 *   │       /   │
 *   │      /    │
 *   │     /     │
 *   │     1     │
 *   │           │
 *   │           │
 *   └───────────┘
 *-----------------------------------------------------------------*/
double costManager(
    double mod_speed,
    double mod_speed_adj_1,
    double mod_speed_adj_2,
    double mod_speed_adj_3,
    double slope,
    double dist,
    bool knight,
    int returnPath,
    double dnullval
    ) {

  /* Output cost */
  double cost;

  /* if return path == true, slope is negative*/
  if (returnPath == 1) {
    slope = -slope;
  }

  /**
   * Convert float modspeed  like 3050000.000
   * to mod = 3
   * and speed = 50.000 km/h
   */

  /* current cell values */
  int mod = floor(mod_speed / 1e6);
  double speed = (mod_speed - (mod * 1e6)) / 1e3;

  /* adjacent cell mode of transportations*/
  int mod_adj_1 = floor(mod_speed_adj_1 / 1e6);

  /* adjacent cell speed in km/h */
  double speed_adj_1 = (mod_speed_adj_1 - (mod_adj_1 * 1e6)) / 1e3;

  /* Distance divider in km */
  double div;

  if (knight) {
    div = dist / 4000;
  } else {
    div = dist / 2000;
  }

  double cost_current = 1 / (modSwitcher(mod, speed, slope) / div);
  double cost_adj_1 = 1 / (modSwitcher(mod_adj_1, speed_adj_1, slope) / div);
  cost = cost_current + cost_adj_1;

  if (knight) {
    int mod_adj_2 = floor(mod_speed_adj_2 / 1e6);
    int mod_adj_3 = floor(mod_speed_adj_3 / 1e6);
    double speed_adj_2 = (mod_speed_adj_2 - (mod_adj_2 * 1e6)) / 1e3;
    double speed_adj_3 = (mod_speed_adj_3 - (mod_adj_3 * 1e6)) / 1e3;
    double cost_adj_2 = 1 / (modSwitcher(mod_adj_2, speed_adj_2, slope) / div);
    double cost_adj_3 = 1 / (modSwitcher(mod_adj_3, speed_adj_3, slope) / div);
    cost = cost + cost_adj_2 + cost_adj_3;
  }

  /* if the result is infinity, remove it */
  if (cost == INFINITY) {
    cost = dnullval;
  }

  return cost * 3600;
}
