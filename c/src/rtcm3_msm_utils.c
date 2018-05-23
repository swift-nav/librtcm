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

#include "rtcm3_msm_utils.h"

double msm_signal_frequency(const constellation_t cons,
                            const uint8_t signal_index,
                            const bool signal_mask[],
                            const uint8_t sat_info) {
  (void)signal_mask;
  (void)sat_info;

  /* TODO: constellation support */

  switch (cons) {
    case CONSTELLATION_GPS:
      if (0 == signal_index) {
        return GPS_L1_FREQ;
      } else {
        return GPS_L2_FREQ;
      }
    case CONSTELLATION_SBAS:
    case CONSTELLATION_GLO:
    case CONSTELLATION_BDS2:
    case CONSTELLATION_QZS:
    case CONSTELLATION_GAL:
    case CONSTELLATION_INVALID:
    case CONSTELLATION_COUNT:
    default:
      return 0;
  }
}

/* Convert message number into MSM message type */
msm_enum to_msm_type(uint16_t msg_num) {
  if (msg_num < 1071 || msg_num > 1127) {
    return MSM_UNKNOWN;
  }
  switch (msg_num % 10) {
    case 1:
      return MSM1;
    case 2:
      return MSM2;
    case 3:
      return MSM3;
    case 4:
      return MSM4;
    case 5:
      return MSM5;
    case 6:
      return MSM6;
    case 7:
      return MSM7;
    default:
      return MSM_UNKNOWN;
  }
}

/* Convert message number into constellation enum */
constellation_t to_constellation(uint16_t msg_num) {
  if (msg_num < 1071) {
    return CONSTELLATION_INVALID;
  }
  if (msg_num < 1080) {
    return CONSTELLATION_GPS;
  }
  if (msg_num < 1090) {
    return CONSTELLATION_GLO;
  }
  if (msg_num < 1100) {
    return CONSTELLATION_GAL;
  }
  if (msg_num < 1110) {
    return CONSTELLATION_SBAS;
  }
  if (msg_num < 1120) {
    return CONSTELLATION_QZS;
  }
  if (msg_num < 1130) {
    return CONSTELLATION_BDS2;
  }
  return CONSTELLATION_INVALID;
}

uint8_t count_mask_bits(uint16_t mask_size, const bool mask[]) {
  uint8_t ret = 0;
  for (uint16_t i = 0; i < mask_size; i++) {
    ret += mask[i];
  }
  return ret;
}
