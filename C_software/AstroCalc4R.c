/*
**  Version 1.0 (March 7, 2011)
**
**  27 July 2010 - A.W. Seaver
**  Astronomical Calculations for use with Survey databases
**  Based on "Astronomical Algorithms" by Jean Meeus, 2nd Edtion, August 2009
**  Recommended by NOAA ESRL  - GMD "NOAA Solar Calculator"
*/

/*
**  17 February 2011 - L. Tang and L. Jacobson
**  The original AstroCalc program by Al Seaver was modified 
**  to be easier to call from R using the .C function.
**  In particular, all functions visible to R are of type VOID.
**  Also modified to accept vector arguments as pointers and
**  to return vector results.  
**
**  The easiest way to use this code from R is  by calling 
**  AstroCalc4R.dll in Windows or AstroCalc4R.so in linux. 
**  
*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

/*
** The include file myfuncs1.c includes a number of functions
** called by AstroCalc4R.  We need to hide them from R because
** they are not of type VOID.
*/
#include "myfuncs1.c"

void AstroCalc4R(int *nrec, int *tzone, int *day,int *month,int *year, double *hhour,double *xlat,double *xlon, \
				 double *noon,double *sunrise,double *sunset,double *azimuth,double *zenith, \
				 double *eqtime,double *declin, double *daylength, double *par)
{ 

/* 
** XDEGRAD is used by AstroCalc4R and by functions that it calls. 
*/
    const double XDEGRAD=3.141592654 / 180.;

	int dm;
	double xd, xm, xy;
	double jd;
	double jc;
	double xx;
	double yy;
	double gmls;
	double gmas;
	double eeo;
	double scx;
	double stl;
	double omega;
	double lambda;
	double epsilon;
	double oblx;
	double gamma;
	double etime;
	double hzero;
	double hangle;
	double phi;
	double tst;
	double tsa;
	double elev;

/* Scalar values extracted from vectors passed to this 
   function */
	double daytemp;
	double monthtemp;
	double yeartemp;
	double hhourtemp;
	double xlattemp;
	double xlontemp;

	for (int i=0; i<*nrec; i++)
	{
		daytemp = day[i];
		monthtemp = month[i];
		yeartemp = year[i];
		hhourtemp = hhour[i];
		xlattemp = xlat[i];
		xlontemp = xlon[i];

		/* Corrrect Time for GMT */

		hhourtemp = hhourtemp - (double) *tzone;

		if (hhourtemp > 24.0)
		{
			hhourtemp = hhourtemp - 24.0;
			dm = daymonth(monthtemp,yeartemp);
			if (daytemp < dm)
				daytemp++;
			else
			{
				daytemp = 1;
				if (monthtemp < 12)
					monthtemp++;
				else
				{
					monthtemp = 1;
					yeartemp++;
				}
			}
		}

		/* Calculate Julian Day Starting at 4712 BCE
		** Method from "Astronomical Algorithms" P. 61
		*/

		xy = (double) yeartemp;
		xm = (double) monthtemp;
		xd = (double) daytemp + hhourtemp / 24.0; 


		jd = JulianDay(xd,xm,xy);

		/*  Calculate Julian Century
		**  "Astronomical Algoritms" Eq. 25.1
		*/

		jc = (jd - 2451545.0) / 36525.0;

		/* Calculate Geometric Mean Longitude of the Sun
		** "Astronomical Algoritms" Eq. 25.2
		*/

		xx = 280.46646 + jc * (36000.76983 + 0.0003032 * jc);
		gmls = fmod(xx,360.0);

		/* Calculate Mean Anomaly of the Sun
		** "Astronomical Algoritms" Eq. 25.3
		*/

		xx = 357.52911 + jc * (35999.05029 - 0.0001537 * jc);
		gmas = fmod(xx,360.0);

		/* Calculate Eccentricity of the Earth's orbit
		** "Astronomical Algoritms" Eq. 25.4
		*/

		eeo = 0.016708634 - jc * (0.000042037 + 1.267E-07 * jc); 
		/* Calculate Sun's Equation of the Center
		** "Astronomical Algoritms" p. 164
		*/

		xx = gmas * XDEGRAD;
		scx = (1.914602 - jc * (0.004817 + 1.4E-05 * jc)) * sin(xx);
		scx += (0.019993 - 0.000101 * jc) * sin(2.0*xx);
		scx += 0.000289 * sin(3.0*xx);

		
		/* Calculate Sun's True Longitude &
		** Sun's True Anomaly
		** "Astronomical Algoritms" p. 164
		*/

		stl = gmls + scx;

		/* Calculate the Sun's Radius Vector
		** "Astronomical Algoritms" Eq. 25.5
		*/


		/* Calculate Sun's Apparent Longitude
		** "Astronomical Algoritms" p. 164
		*/

		omega = 125.04 - 1934.136 * jc;
		omega = omega * XDEGRAD;
		lambda = stl - 0.00569 - 0.00478 * sin(omega);

			/* Calculate Sun's Equation of the Center
		** "Astronomical Algoritms" p. 164
		*/

		xx = gmas * XDEGRAD;
		scx = (1.914602 - jc * (0.004817 + 1.4E-05 * jc)) * sin(xx);
		scx += (0.019993 - 0.000101 * jc) * sin(2.0*xx);
		scx += 0.000289 * sin(3.0*xx);

		/* Calculate Sun's True Longitude &
		** Sun's True Anomaly
		** "Astronomical Algoritms" p. 164
		*/

		stl = gmls + scx;

		/* Calculate the Sun's Radius Vector
		** "Astronomical Algoritms" Eq. 25.5
		*/


		/* Calculate Sun's Apparent Longitude
		** "Astronomical Algoritms" p. 164
		*/

		omega = 125.04 - 1934.136 * jc;
		omega = omega * XDEGRAD;
		lambda = stl - 0.00569 - 0.00478 * sin(omega);

		/* Calculate Mean Obliquity of the Ecliptic
		** "Astronomical Algoritms" Eq. 22.2
		*/

		epsilon = 23.0 + (26 + ((21.448 - jc * (46.8150 + jc * (0.00059 - 0.001813 * jc)))) / 60.) / 60.;


		/* Calculate Obliquity Correction 
		** "Astronomical Algoritms" Eq. 25.8
		*/

		oblx = 0.00256 * cos(omega);

		epsilon = epsilon + oblx;

		/* Calculate Sun's Right Ascension
		** "Astronomical Algoritms" Eq. 25.6
		*/

		lambda = lambda * XDEGRAD;
		epsilon = epsilon * XDEGRAD;


		/* Calculate Sun's Declination
		** "Astronomical Algoritms" Eq. 25.7
		*/

		xx = sin(epsilon) * sin(lambda);

		gamma = asin(xx);
		declin[i] = gamma / XDEGRAD;

			/* Calculate Equation of Time
		** "Astronomical Algoritms" Eq. 28.3
		*/

		xx = gmls * XDEGRAD;
		yy = gmas * XDEGRAD;

		etime = EquationTime(epsilon,xx,eeo,yy);

		eqtime[i] = etime;

		/* Calculate Hour Angle
		** "Astronomical Algoritms" Eq. 15.1
		*/

		/* Standard value for hzero = -0.83333 degrees */

		hzero = -0.83333 * XDEGRAD;

		phi = xlattemp * XDEGRAD;

		xx = (sin(hzero) - sin(phi) * sin(gamma)) / cos(phi) / cos(gamma);
		
		hangle = acos(xx);

		hangle = hangle / XDEGRAD;

		/* Calculate the Solar Noon (LST)
		** Each 15 Degrees of Longitude = 1 Hour
		** Each Time zone = 1 hour
		** 1440 Minutes in Day
		*/

		xx = (double) *tzone * 60.0;

		noon[i] = (720. - 4.0 * xlontemp + xx - etime) / 1440.0;


		/* Calculate Sunrise & Sunset */
		

		sunrise[i] = ((noon[i] * 1440. - hangle * 4.0) / 1440.0) * 24. ;
		sunset[i]  = ((noon[i] * 1440. + hangle * 4.0) / 1440.0) * 24. ;
	    noon[i] = noon[i] * 24. ;

		/* Calculate Length of Day */

		daytemp = hangle * 8.0;

		/* Calculate True Solar Time (minutes) */

		xx = hhourtemp * 60.0 + etime + 4.0 * xlontemp;
		tst = fmod(xx,1440.0);

		/* Calculate the True Solar Angle (degrees) */

		if (tst < 0.0)
			tsa = tst / 4.0 + 180.0;
		else
			tsa = tst / 4.0 - 180.0;


		/* Calculate Zenith
		** "Astronomical Algoritms" Eq. 13.6
		*/

		xx = tsa * XDEGRAD;

		yy = sin(phi) * sin(gamma) + cos(phi) * cos(gamma) * cos(xx);

		xx = asin(yy);

		elev = xx / XDEGRAD;

		zenith[i] = 90.0 - elev;

		/* Calculate Azimuth (degress clockwise from N 
		** "Astronomical Algoritms" P. 94
		*/

		yy = (sin(phi) * sin(xx) - sin(gamma)) / cos(phi) / cos(xx);
		
		xx = acos(yy) / XDEGRAD;

		xx = xx + 180.0;

		if (tsa > 0.0)
			azimuth[i] = fmod(xx,360.0);
		else
			azimuth[i] = 360.0 - fmod(xx,360.0);

		daylength[i] = daytemp / 60.0;	
		
		par[i] = parcalc((double) zenith[i]);
    }	
	
}
