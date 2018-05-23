/*
 * Copyright (C) 2017 Swift Navigation Inc.
 * Contact: Swift Navigation <dev@swiftnav.com>
 *
 * This source is subject to the license found in the file 'LICENSE' which must
 * be distributed together with this source. All other rights reserved.
 *
 * THIS CODE AND INFORMATION IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND,
 * EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef LIBRTCM_CONSTANTS_H
#define LIBRTCM_CONSTANTS_H

#define PRUNIT_GPS 299792.458 /**< RTCM v3 Unit of GPS Pseudorange (m) */
#define PRUNIT_GLO 599584.916 /**< RTCM v3 Unit of GLO Pseudorange (m) */
#define RTCM_MAX_SATS 32
#define CP_INVALID 0xFFF80000    /* Unsigned bit pattern 0x80000 */
#define PR_L1_INVALID 0xFFF80000 /* Unsigned bit pattern 0x80000 */
#define PR_L2_INVALID 0xFFFFE000 /* Unsigned bit pattern 0x20000 */
#define MSM_MAX_CELLS 64         /* Maximum number of cells in MSM message */
#define MSM_ROUGH_RANGE_INVALID 0xFF  /* Unsigned bit pattern 0xFF */
#define MSM_ROUGH_RATE_INVALID 0x2000 /* Unsigned bit pattern 0x2000 */
#define MSM_PR_INVALID -16384         /* Signed bit pattern 0x4000 */
#define MSM_PR_EXT_INVALID -8388608   /* Signed bit pattern 0x800000 */
#define MSM_CP_INVALID -2097152       /* Signed bit pattern 0x200000 */
#define MSM_CP_EXT_INVALID -8388608   /* Signed bit pattern 0x800000 */
#define MSM_DOP_INVALID -16384        /* Signed bit pattern 0x4000 */

#define MSM_GLO_FCN_OFFSET 7 /* Offset for FCN coding in sat_info */

/** 2^-4 */
#define C_1_2P4 0.0625
/** 2^-24 */
#define C_1_2P24 5.960464477539063e-08
/** 2^-29 */
#define C_1_2P29 1.862645149230957e-09
/** 2^-31 */
#define C_1_2P31 4.656612873077393e-10

/** The official GPS value of the speed of light in m / s.
 * \note This is the exact value of the speed of light in vacuum (by the
 * definition of meters). */
#define GPS_C 299792458.0

/** The GPS L1 center frequency in Hz. */
#define GPS_L1_HZ 1.57542e9

/** The GPS L2 center frequency in Hz. */
#define GPS_L2_HZ 1.22760e9

/** The GPS L5 center frequency in Hz. */
#define GPS_L5_HZ (115 * 10.23e6)

/** The GLO L1 center frequency in Hz. */
#define GLO_L1_HZ 1.602e9

/** The GLO L2 center frequency in Hz. */
#define GLO_L2_HZ 1.246e9

/** Frequency range between two adjacent GLO channel in Hz for L1 band*/
#define GLO_L1_DELTA_HZ 5.625e5

/** Frequency range between two adjacent GLO channel in Hz for L2 band */
#define GLO_L2_DELTA_HZ 4.375e5

/** Centre frequency of SBAS L1 */
#define SBAS_L1_HZ (1.023e6 * 1540)

/** Centre frequency of SBAS L5 */
#define SBAS_L5_HZ (1.023e6 * 1150)

/** Centre frequency of Beidou2 B11 */
#define BDS2_B11_HZ (1.023e6 * (1540 - 14))

/** Centre frequency of Beidou2 B2 */
#define BDS2_B2_HZ (1.023e6 * 1180)

/** Centre frequency of Galileo E1 */
#define GAL_E1_HZ (1.023e6 * 1540)

/** Centre frequency of Galileo E6 */
#define GAL_E6_HZ (1.023e6 * 1250)

/** Centre frequency of Galileo E5b */
#define GAL_E7_HZ (1.023e6 * 1180)

/** Centre frequency of Galileo E5AltBOC */
#define GAL_E8_HZ (1.023e6 * 1165)

/** Centre frequency of Galileo E5a */
#define GAL_E5_HZ (1.023e6 * 1150)

/** Centre frequency of QZSS L1CA */
#define QZS_L1_HZ (1.023e6 * 1540)

/** Centre frequency of QZSS L2C */
#define QZS_L2_HZ (1.023e6 * 1200)

/** Centre frequency of QZSS L5 */
#define QZS_L5_HZ (1.023e6 * 1150)

#define GPS_FIRST_PRN 1
#define SBAS_FIRST_PRN 120
#define GLO_FIRST_PRN 1
#define BDS2_FIRST_PRN 1
#define GAL_FIRST_PRN 1
#define QZS_FIRST_PRN 193

#endif /* LIBRTCM_CONSTANTS_H */
