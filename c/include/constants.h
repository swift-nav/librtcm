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

static const double CLIGHT = 299792458.0; /* speed of light (m/s) */

static const double GPS_L1_FREQ = 1.57542e9; /* GPS L1 frequency (Hz) */
static const double GPS_L2_FREQ = 1.22760e9; /* GPS L2 Frequency */
static const double GLO_L1_FREQ = 1.602e9; /* GLO L1 frequency (Hz) */
static const double GLO_L2_FREQ = 1.246e9; /* GLO L2 Frequency */
static const double GLO_L1_CH_OFFSET = 0.5625e6;    /* GLO L1 Channel offset */
static const double GLO_L2_CH_OFFSET = 0.4375e6; /* GLO L2 Channel offset */



#endif //LIBRTCM_CONSTANTS_H
