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

#include <rtcm3/bits.h>
#include <rtcm3/msm_utils.h>
#include <rtcm3/ssr_decode.h>
#include <stdio.h>

/** Get the numbers of bits for the  Epoch Time 1s field
 * \param constellation Message constellation
 * \return Number of bits
 */
rtcm3_rc get_number_of_bits_for_epoch_time(rtcm_constellation_t constellation,
                                           uint8_t *num_bit) {
  switch (constellation) {
    case RTCM_CONSTELLATION_GPS:
    case RTCM_CONSTELLATION_GAL:
    case RTCM_CONSTELLATION_BDS:
    case RTCM_CONSTELLATION_QZS:
    case RTCM_CONSTELLATION_SBAS: {
      *num_bit = 20;
      return RC_OK;
    }
    case RTCM_CONSTELLATION_GLO: {
      *num_bit = 17;
      return RC_OK;
    }
    case RTCM_CONSTELLATION_INVALID:
    case RTCM_CONSTELLATION_COUNT:
    default:
      return RC_INVALID_MESSAGE;
  }
}

/** Get the numbers of bits for the sat ID field
 * \param constellation Message constellation
 * \return Number of bits
 */
rtcm3_rc get_number_of_bits_for_sat_id(rtcm_constellation_t constellation,
                                       uint8_t *num_bit) {
  switch (constellation) {
    case RTCM_CONSTELLATION_GPS:
    case RTCM_CONSTELLATION_GAL:
    case RTCM_CONSTELLATION_BDS:
    case RTCM_CONSTELLATION_SBAS: {
      *num_bit = 6;
      return RC_OK;
    }
    case RTCM_CONSTELLATION_GLO: {
      *num_bit = 5;
      return RC_OK;
    }
    case RTCM_CONSTELLATION_QZS: {
      *num_bit = 4;
      return RC_OK;
    }
    case RTCM_CONSTELLATION_INVALID:
    case RTCM_CONSTELLATION_COUNT:
    default:
      return RC_INVALID_MESSAGE;
  }
}

/** Get the numbers of bits for the IODE field
 * \param constellation Message constellation
 * \return Number of bits
 */
enum rtcm3_rc_e get_number_of_bits_for_iode(
    const rtcm_constellation_t constellation, uint8_t *num_bit) {
  switch (constellation) {
    case RTCM_CONSTELLATION_GPS:
    case RTCM_CONSTELLATION_GLO:
    case RTCM_CONSTELLATION_QZS: {
      *num_bit = 8;
      return RC_OK;
    }
    case RTCM_CONSTELLATION_GAL:
    case RTCM_CONSTELLATION_BDS: {
      *num_bit = 10;
      return RC_OK;
    }
    case RTCM_CONSTELLATION_SBAS: {
      *num_bit = 9;
      return RC_OK;
    }
    case RTCM_CONSTELLATION_INVALID:
    case RTCM_CONSTELLATION_COUNT:
    default:
      return RC_INVALID_MESSAGE;
  }
}

/*
*            TYPE       GPS     GLOASS    GALILEO    QZSS     BEIDOU     SBAS
*         ----------------------------------------------------------------------
*
*          SSR OBT   : 1057      1063      1240*     1246*     1258*       -
*              CLK   : 1058      1064      1241*     1247*     1259*       -
*              BIAS  : 1059      1065      1242*     1248*     1260*       -
*              OBTCLK: 1060      1066      1243*     1249*     1261*       -
*              URA   : 1061      1067      1244*     1250*     1262*       -
*              HRCLK : 1062      1068      1245*     1251*     1263*       -
*              PHBIAS: 1265*     1266*     1267*     1268*     1270*      1269*
*                    (* means that these RTCM messages are still draft )
*/

bool is_ssr_orbit_clock_message(const uint16_t message_num) {
  return message_num == 1060 || message_num == 1066 || message_num == 1243 ||
         message_num == 1249 || message_num == 1261;
}

bool is_ssr_code_biases_message(const uint16_t message_num) {
  return message_num == 1059 || message_num == 1065 || message_num == 1242 ||
         message_num == 1248 || message_num == 1260;
}

bool is_ssr_phase_biases_message(const uint16_t message_num) {
  return message_num >= 1265 && message_num <= 1270;
}

enum rtcm3_rc_e decode_ssr_header(const uint8_t buff[],
                                  uint16_t *bit,
                                  rtcm_msg_ssr_header *msg_header) {
  msg_header->message_num = rtcm_getbitu(buff, *bit, 12);
  *bit += 12;
  uint8_t number_of_bits_for_epoch_time;
  if (!(RC_OK == get_number_of_bits_for_epoch_time(
                     to_constellation(msg_header->message_num),
                     &number_of_bits_for_epoch_time))) {
    return RC_INVALID_MESSAGE;
  }
  msg_header->epoch_time =
      rtcm_getbitu(buff, *bit, number_of_bits_for_epoch_time);
  *bit += number_of_bits_for_epoch_time;
  msg_header->constellation = to_constellation(msg_header->message_num);

  msg_header->update_interval = rtcm_getbitu(buff, *bit, 4);
  *bit += 4;
  msg_header->multi_message = rtcm_getbitu(buff, *bit, 1);
  *bit += 1;
  if (is_ssr_orbit_clock_message(msg_header->message_num)) {
    msg_header->sat_ref_datum = rtcm_getbitu(buff, *bit, 1);
    *bit += 1;
  }
  msg_header->iod_ssr = rtcm_getbitu(buff, *bit, 4);
  *bit += 4;
  msg_header->ssr_provider_id = rtcm_getbitu(buff, *bit, 16);
  *bit += 16;
  msg_header->ssr_solution_id = rtcm_getbitu(buff, *bit, 4);
  *bit += 4;
  if (is_ssr_phase_biases_message(msg_header->message_num)) {
    msg_header->dispersive_bias_consistency = rtcm_getbitu(buff, *bit, 1);
    *bit += 1;
    msg_header->melbourne_wubbena_consistency = rtcm_getbitu(buff, *bit, 1);
    *bit += 1;
  }
  msg_header->num_sats = rtcm_getbitu(buff, *bit, 6);
  *bit += 6;
  return RC_OK;
}

/** Decode an RTCMv3 Combined SSR Orbit and Clock message
 *
 * \param buff The input data buffer
 * \param RTCM message struct
 * \return  - RC_OK : Success
 *          - RC_MESSAGE_TYPE_MISMATCH : Message type mismatch
 *          - RC_INVALID_MESSAGE : Cell mask too large or invalid TOW
 */
rtcm3_rc rtcm3_decode_orbit_clock(const uint8_t buff[],
                                  rtcm_msg_orbit_clock *msg_orbit_clock) {
  uint16_t bit = 0;
  if (!(RC_OK == decode_ssr_header(buff, &bit, &msg_orbit_clock->header))) {
    return RC_INVALID_MESSAGE;
  }

  if (!is_ssr_orbit_clock_message(msg_orbit_clock->header.message_num)) {
    return RC_MESSAGE_TYPE_MISMATCH;
  }

  for (int sat_count = 0; sat_count < msg_orbit_clock->header.num_sats;
       sat_count++) {
    rtcm_msg_ssr_orbit_corr *orbit = &msg_orbit_clock->orbit[sat_count];
    rtcm_msg_ssr_clock_corr *clock = &msg_orbit_clock->clock[sat_count];

    uint8_t number_of_bits_for_sat_id;
    if (!(RC_OK ==
          get_number_of_bits_for_sat_id(msg_orbit_clock->header.constellation,
                                        &number_of_bits_for_sat_id))) {
      return RC_INVALID_MESSAGE;
    }

    orbit->sat_id = rtcm_getbitu(buff, bit, number_of_bits_for_sat_id);
    bit += number_of_bits_for_sat_id;

    clock->sat_id = orbit->sat_id;

    uint8_t number_of_bits_for_iode;
    if (!(RC_OK ==
          get_number_of_bits_for_iode(msg_orbit_clock->header.constellation,
                                      &number_of_bits_for_iode))) {
      return RC_INVALID_MESSAGE;
    }
    orbit->iode = rtcm_getbitu(buff, bit, number_of_bits_for_iode);
    bit += number_of_bits_for_iode;
    if (msg_orbit_clock->header.constellation == RTCM_CONSTELLATION_BDS ||
        msg_orbit_clock->header.constellation == RTCM_CONSTELLATION_SBAS) {
      orbit->iodcrc = rtcm_getbitu(buff, bit, 24);
      bit += 24;
    }

    orbit->radial = rtcm_getbits(buff, bit, 22);
    bit += 22;
    orbit->along_track = rtcm_getbits(buff, bit, 20);
    bit += 20;
    orbit->cross_track = rtcm_getbits(buff, bit, 20);
    bit += 20;
    orbit->dot_radial = rtcm_getbits(buff, bit, 21);
    bit += 21;
    orbit->dot_along_track = rtcm_getbits(buff, bit, 19);
    bit += 19;
    orbit->dot_cross_track = rtcm_getbits(buff, bit, 19);
    bit += 19;

    clock->c0 = rtcm_getbits(buff, bit, 22);
    bit += 22;
    clock->c1 = rtcm_getbits(buff, bit, 21);
    bit += 21;
    clock->c2 = rtcm_getbits(buff, bit, 27);
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
rtcm3_rc rtcm3_decode_code_bias(const uint8_t buff[],
                                rtcm_msg_code_bias *msg_code_bias) {
  uint16_t bit = 0;
  if (!(RC_OK == decode_ssr_header(buff, &bit, &msg_code_bias->header))) {
    return RC_INVALID_MESSAGE;
  }

  if (!is_ssr_code_biases_message(msg_code_bias->header.message_num)) {
    return RC_MESSAGE_TYPE_MISMATCH;
  }

  for (int i = 0; i < msg_code_bias->header.num_sats; i++) {
    rtcm_msg_ssr_code_bias_sat *sat = &msg_code_bias->sats[i];

    uint8_t number_of_bits_for_sat_id;
    if (!(RC_OK ==
          get_number_of_bits_for_sat_id(msg_code_bias->header.constellation,
                                        &number_of_bits_for_sat_id))) {
      return RC_INVALID_MESSAGE;
    }

    sat->sat_id = rtcm_getbitu(buff, bit, number_of_bits_for_sat_id);
    bit += number_of_bits_for_sat_id;

    sat->num_code_biases = rtcm_getbitu(buff, bit, 5);
    bit += 5;

    for (int j = 0; j < sat->num_code_biases; j++) {
      sat->signals[j].signal_id = rtcm_getbitu(buff, bit, 5);
      bit += 5;
      sat->signals[j].code_bias = rtcm_getbits(buff, bit, 14);
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
rtcm3_rc rtcm3_decode_phase_bias(const uint8_t buff[],
                                 rtcm_msg_phase_bias *msg_phase_bias) {
  uint16_t bit = 0;
  if (!(RC_OK == decode_ssr_header(buff, &bit, &msg_phase_bias->header))) {
    return RC_INVALID_MESSAGE;
  }

  if (!is_ssr_phase_biases_message(msg_phase_bias->header.message_num)) {
    return RC_MESSAGE_TYPE_MISMATCH;
  }

  for (int i = 0; i < msg_phase_bias->header.num_sats; i++) {
    rtcm_msg_ssr_phase_bias_sat *sat = &msg_phase_bias->sats[i];

    uint8_t number_of_bits_for_sat_id;
    if (!(RC_OK ==
          get_number_of_bits_for_sat_id(msg_phase_bias->header.constellation,
                                        &number_of_bits_for_sat_id))) {
      return RC_INVALID_MESSAGE;
    }

    sat->sat_id = rtcm_getbitu(buff, bit, number_of_bits_for_sat_id);
    bit += number_of_bits_for_sat_id;

    sat->num_phase_biases = rtcm_getbitu(buff, bit, 5);
    bit += 5;
    sat->yaw_angle = rtcm_getbitu(buff, bit, 9);
    bit += 9;
    sat->yaw_rate = rtcm_getbits(buff, bit, 8);
    bit += 8;

    for (int j = 0; j < sat->num_phase_biases; j++) {
      sat->signals[j].signal_id = rtcm_getbitu(buff, bit, 5);
      bit += 5;
      sat->signals[j].integer_indicator = rtcm_getbitu(buff, bit, 1);
      bit += 1;
      sat->signals[j].widelane_indicator = rtcm_getbitu(buff, bit, 2);
      bit += 2;
      sat->signals[j].discontinuity_indicator = rtcm_getbitu(buff, bit, 4);
      bit += 4;
      sat->signals[j].phase_bias = rtcm_getbits(buff, bit, 20);
      bit += 20;
    }
  }
  return RC_OK;
}
