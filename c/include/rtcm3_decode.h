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

#ifndef SWIFTNAV_RTCM3_DECODE_H
#define SWIFTNAV_RTCM3_DECODE_H

#ifdef __cplusplus
extern "C" {
#endif

#include "rtcm3_messages.h"
#include "rtcm3_msm_utils.h"

rtcm3_rc rtcm3_decode_1001(const uint8_t buff[], rtcm_obs_message *msg_1001);
rtcm3_rc rtcm3_decode_1002(const uint8_t buff[], rtcm_obs_message *msg_1002);
rtcm3_rc rtcm3_decode_1003(const uint8_t buff[], rtcm_obs_message *msg_1003);
rtcm3_rc rtcm3_decode_1004(const uint8_t buff[], rtcm_obs_message *msg_1004);
rtcm3_rc rtcm3_decode_1005(const uint8_t buff[], rtcm_msg_1005 *msg_1005);
rtcm3_rc rtcm3_decode_1006(const uint8_t buff[], rtcm_msg_1006 *msg_1006);
rtcm3_rc rtcm3_decode_1007(const uint8_t buff[], rtcm_msg_1007 *msg_1007);
rtcm3_rc rtcm3_decode_1008(const uint8_t buff[], rtcm_msg_1008 *msg_1008);
rtcm3_rc rtcm3_decode_1010(const uint8_t buff[], rtcm_obs_message *msg_1010);
rtcm3_rc rtcm3_decode_1012(const uint8_t buff[], rtcm_obs_message *msg_1012);
rtcm3_rc rtcm3_decode_1029(const uint8_t buff[], rtcm_msg_1029 *msg_1029);
rtcm3_rc rtcm3_decode_1033(const uint8_t buff[], rtcm_msg_1033 *msg_1033);
rtcm3_rc rtcm3_decode_1230(const uint8_t buff[], rtcm_msg_1230 *msg_1230);
rtcm3_rc rtcm3_decode_msm4(const uint8_t buff[],
                           const uint8_t glo_sv_id_fcn_map[],
                           rtcm_msm_message *msg);
rtcm3_rc rtcm3_decode_msm5(const uint8_t buff[], rtcm_msm_message *msg_msm5);
rtcm3_rc rtcm3_decode_msm6(const uint8_t buff[],
                           const uint8_t glo_sv_id_fcn_map[],
                           rtcm_msm_message *msg);
rtcm3_rc rtcm3_decode_msm7(const uint8_t buff[], rtcm_msm_message *msg_msm7);

#ifdef __cplusplus
}
#endif

#endif /* SWIFTNAV_RTCM3_DECODE_H */
