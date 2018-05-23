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

/** 2^-4 */
#define C_1_2P4 0.0625
/** 2^-24 */
#define C_1_2P24 5.960464477539063e-08
/** 2^-29 */
#define C_1_2P29 1.862645149230957e-09
/** 2^-31 */
#define C_1_2P31 4.656612873077393e-10

static const double CLIGHT = 299792458.0; /* speed of light (m/s) */

static const double GPS_L1_FREQ = 1.57542e9;     /* GPS L1 frequency (Hz) */
static const double GPS_L2_FREQ = 1.22760e9;     /* GPS L2 Frequency */
static const double GLO_L1_FREQ = 1.602e9;       /* GLO L1 frequency (Hz) */
static const double GLO_L2_FREQ = 1.246e9;       /* GLO L2 Frequency */
static const double GLO_L1_CH_OFFSET = 0.5625e6; /* GLO L1 Channel offset */
static const double GLO_L2_CH_OFFSET = 0.4375e6; /* GLO L2 Channel offset */

#endif /* LIBRTCM_CONSTANTS_H */
