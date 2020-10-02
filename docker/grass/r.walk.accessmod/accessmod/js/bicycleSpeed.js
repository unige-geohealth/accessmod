/*jshint esversion: 6 */

/*http://bikecalculator.com/wattsMetric.html*/
/*http://bikecalculator.com/veloMetric.html*/


tireValues = [0.005, 0.004, 0.012];
aeroValues = [0.388, 0.445, 0.420, 0.300, 0.233, 0.200];



function newton(aero, hw, tr, tran, p) {        /* Newton's method */

  var vel = 20;       // Initial guess
  const MAX = 10;       // maximum iterations
  var TOL = 0.05;     // tolerance

  for (i=1; i < MAX; i++) {
    var tv = vel + hw;
    var f = vel * (aero * tv * tv + tr) - tran * p; // the function
    var fp = aero * (3.0 * vel + hw) * tv + tr;     // the derivative
    var vNew = vel - f / fp;
    if (Math.abs(vNew - vel) < TOL) return vNew;  // success
    vel = vNew;
  }
  return 0.0;  // failed to converge
}

function bicycleSpeed( speed, slope ) {

  speedBike = speed / 3.6;  // converted to m/s;
  slope = slope * 0.01;
  // fixed
  const maxRatio = 2;
  const slopeFlat = 0;
  const weightRider = 80;
  const weightBicycle = 15;
  const resistanceRolling = 0.012; // orig  0.004, 0.005, 0.012
  const areaFrontal = 0.5 ; // orig 0.388, 0.445, 0.420, 0.300, 0.233, 0.200
  const speedWind = 0;
  const temperature = 20;
  const elevation = 500;
  const efficiencyTransmission = 0.90;

  const densityAir = ( 1.293 - 0.00426 * temperature ) * Math.exp( - elevation / 7000.0 );
  const weightTotal = 9.8 * ( weightRider + weightBicycle ); // newton
  const resistanceAir = 0.5 * areaFrontal * densityAir;  // full air resistance parameter

  var resistanceSlopeTireFlat = weightTotal * ( slopeFlat + resistanceRolling ); // gravity and rolling resistance
  var resistanceSlopeTireUp = weightTotal * ( slope + resistanceRolling ); // gravity and rolling resistance
  var speedTotal = speedBike + speedWind; 
  var powerFlat = (speedBike * resistanceSlopeTireFlat + speedBike * speedTotal * speedTotal * resistanceAir) / efficiencyTransmission;

  var v = newton(resistanceAir, speedWind, resistanceSlopeTireUp, efficiencyTransmission, powerFlat) * 3.6; 
  
  return(v);

}

/*
* Set threshold : max 2 times input speed and no less than 0 (weird case with high negative slope)
*/
function speedBicycle(speed, slope){

   var speedFinal =  bicycleSpeed(speed, slope);
   
  if( speedFinal >= speed * 2){
    speedFinal  = speed * 2;
  }

  if( speedFinal < 0 ){
    speedFinal  = 0;
  }

  return speedFinal;

}

console.log(speedBicycle(process.argv[2]*1,process.argv[3])*1);

