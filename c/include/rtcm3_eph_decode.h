/*
 * Copyright (C) 2018 Swift Navigation Inc.
 * Contact: Swift Navigation <dev@swiftnav.com>
 *
 * This source is subject to the license found in the file 'LICENSE' which must
 * be distributed together with this source. All other rights reserved.
 *
 * THIS CODE AND INFORMATION IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND,
 * EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef SWIFTNAV_RTCM3_EPH_DECODE_H
#define SWIFTNAV_RTCM3_EPH_DECODE_H

#include "rtcm3_messages.h"

rtcm3_rc rtcm3_decode_gps_eph(const uint8_t buff[], rtcm_msg_eph *msg_eph);
rtcm3_rc rtcm3_decode_glo_eph(const uint8_t buff[], rtcm_msg_eph *msg_eph);
rtcm3_rc rtcm3_decode_gal_eph(const uint8_t buff[], rtcm_msg_eph *msg_eph);
rtcm3_rc rtcm3_decode_bds_eph(const uint8_t buff[], rtcm_msg_eph *msg_eph);

#endif /* SWIFTNAV_RTCM3_EPH_DECODE_H */
