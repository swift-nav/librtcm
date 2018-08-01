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

#include <rtcm3_eph_decode.h>
#include <rtcm3_msm_utils.h>
#include "bits.h"


void decode_ssr_header(const uint8_t buff[], uint16_t *bit, rtcm_msg_ssr_header *msg_header){
  msg_header->message_num = getbitu(buff, *bit, 12);
  *bit += 12;
  if(to_constellation(msg_header->message_num) == CONSTELLATION_GPS) {
    msg_header->epoch_time = getbitu(buff, *bit, 20);
    *bit += 20;
  } else if((to_constellation(msg_header->message_num) == CONSTELLATION_GLO)) {
    msg_header->epoch_time = getbitu(buff, *bit, 17);
    *bit += 17;
  }
  msg_header->update_interval = getbitu(buff, *bit, 4);
  *bit += 4;
  msg_header->multi_message = getbitu(buff, *bit, 1);
  *bit += 1;
  if(msg_header->message_num == 1060){
    msg_header->sat_ref_datum = getbitu(buff, *bit, 1);
    *bit += 1;
  }
  msg_header->iod_ssr = getbitu(buff, *bit, 4);
  *bit += 4;
  msg_header->ssr_provider_id = getbitu(buff, *bit, 16);
  *bit += 16;
  msg_header->ssr_solution_id = getbitu(buff, *bit, 4);
  *bit += 4;
  if(msg_header->message_num >= 1265 && msg_header->message_num <= 1270 ){
    msg_header->dispersive_bias_consistency = getbitu(buff, *bit, 1);
    *bit += 1;
    msg_header->melbourne_wubbena_consistency = getbitu(buff, *bit, 1);
    *bit += 1;
  }
  msg_header->num_sats = getbitu(buff, *bit, 6);
  *bit += 6;
}

/** Decode an RTCMv3 Combined SSR Orbit and Clock message
 *
 * \param buff The input data buffer
 * \param RTCM message struct
 * \return  - RC_OK : Success
 *          - RC_MESSAGE_TYPE_MISMATCH : Message type mismatch
 *          - RC_INVALID_MESSAGE : Cell mask too large or invalid TOW
 */
rtcm3_rc rtcm3_decode_orbit_clock(const uint8_t buff[], rtcm_msg_orbit_clock *msg_orbit_clock) {
  uint16_t bit = 0;
  decode_ssr_header(buff,&bit,&msg_orbit_clock->header);
  if(msg_orbit_clock->header.message_num != 1060) {
    return RC_MESSAGE_TYPE_MISMATCH;
  }

  for (int i = 0; i < msg_orbit_clock->header.num_sats; i++) {
    rtcm_msg_ssr_orbit_corr *orbit = &msg_orbit_clock->orbit[i];
    rtcm_msg_ssr_clock_corr *clock = &msg_orbit_clock->clock[i];

    orbit->sat_id = getbitu(buff, bit, 6);
    clock->sat_id = orbit->sat_id;
    bit += 6;
    orbit->iode = getbitu(buff, bit, 8);
    bit += 8;
    orbit->radial = getbits(buff, bit, 22);
    bit += 22;
    orbit->along_track = getbits(buff, bit, 20);
    bit += 20;
    orbit->cross_track = getbits(buff, bit, 20);
    bit += 20;
    orbit->dot_radial = getbits(buff, bit, 21);
    bit += 21;
    orbit->dot_along_track = getbits(buff, bit, 19);
    bit += 19;
    orbit->dot_cross_track = getbits(buff, bit, 19);
    bit += 19;

    clock->c0 = getbits(buff, bit, 22);
    bit += 22;
    clock->c1 = getbits(buff, bit, 21);
    bit += 21;
    clock->c2 = getbits(buff, bit, 27);
    bit += 27;
  }
  return RC_OK;
}

/** Decode an RTCMv3 Code bias message
 *
 * \param buff The input data buffer
 * \param RTCM message struct
 * \return  - RC_OK : Success
 *          - RC_MESSAGE_TYPE_MISMATCH : Message type mismatch
 *          - RC_INVALID_MESSAGE : Cell mask too large or invalid TOW
 */
rtcm3_rc rtcm3_decode_code_bias(const uint8_t buff[], rtcm_msg_code_bias *msg_code_bias) {
  uint16_t bit = 0;
  decode_ssr_header(buff,&bit,&msg_code_bias->header);
  if(msg_code_bias->header.message_num != 1065) {
    return RC_MESSAGE_TYPE_MISMATCH;
  }

  for (int i = 0; i < msg_code_bias->header.num_sats; i++) {
    rtcm_msg_ssr_code_bias_sat *sat = &msg_code_bias->sats[i];
    sat->sat_id = getbitu(buff, bit, 6);
    bit += 6;
    sat->num_code_biases = getbitu(buff, bit, 5);
    bit += 5;

    for(int j = 0; j < sat->num_code_biases; j++) {
      sat->signals[j].signal_id = getbitu(buff, bit, 5);
      bit += 5;
      sat->signals[j].code_bias = getbits(buff, bit, 14);
      bit += 14;
    }
  }
  return RC_OK;
}

/** Decode an RTCMv3 Phase bias message
 *
 * \param buff The input data buffer
 * \param RTCM message struct
 * \return  - RC_OK : Success
 *          - RC_MESSAGE_TYPE_MISMATCH : Message type mismatch
 *          - RC_INVALID_MESSAGE : Cell mask too large or invalid TOW
 */
rtcm3_rc rtcm3_decode_phase_bias(const uint8_t buff[], rtcm_msg_phase_bias *msg_phase_bias) {
  uint16_t bit = 0;
  decode_ssr_header(buff,&bit,&msg_phase_bias->header);
  if(msg_phase_bias->header.message_num != 1264) {
    return RC_MESSAGE_TYPE_MISMATCH;
  }

  for (int i = 0; i < msg_phase_bias->header.num_sats; i++) {
    rtcm_msg_ssr_phase_bias_sat *sat = &msg_phase_bias->sats[i];
    sat->sat_id = getbitu(buff, bit, 6);
    bit += 6;
    sat->num_phase_biases = getbitu(buff, bit, 5);
    bit += 5;
    sat->yaw_angle = getbitu(buff, bit, 9);
    bit += 9;
    sat->yaw_rate = getbits(buff, bit, 8);
    bit += 8;

    for(int j = 0; j < sat->num_phase_biases; j++) {
      sat->signals[j].signal_id = getbitu(buff, bit, 5);
      bit += 5;
      sat->signals[j].integer_indicator = getbitu(buff, bit, 1);
      bit += 1;
      sat->signals[j].widelane_indicator = getbitu(buff, bit, 2);
      bit += 2;
      sat->signals[j].discontinuity_indicator = getbitu(buff, bit, 4);
      bit += 4;
      sat->signals[j].phase_bias = getbits(buff, bit, 20);
      bit += 20;
    }
  }
  return RC_OK;
}
