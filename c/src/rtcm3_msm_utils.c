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
#include <assert.h>

/** Find the frequency of an MSM signal
 *
 * \param header Pointer to message header
 * \param signal_index 0-based index into the signal mask
 * \param sat_info The decoded sat info field (contains GLO FCN)
 * \return frequency [Hz], or 0 in signal was invalid
 */
double msm_signal_frequency(const rtcm_msm_header *header,
                            const uint8_t signal_index,
                            const uint8_t sat_info) {
  code_t code = msm_signal_to_code(header, signal_index);

  /* TODO: use sid_to_carr_freq from LNSP */

  switch (code) {
    case CODE_GPS_L1CA:
    case CODE_GPS_L1P:
      return GPS_L1_HZ;
    case CODE_GPS_L2CM:
    case CODE_GPS_L2P:
    case CODE_GPS_L2CL:
    case CODE_GPS_L2CX:
      return GPS_L2_HZ;
    case CODE_GPS_L5I:
    case CODE_GPS_L5Q:
    case CODE_GPS_L5X:
      return GPS_L5_HZ;
    case CODE_GLO_L1OF: {
      int8_t fcn = sat_info - MSM_GLO_FCN_OFFSET;
      return GLO_L1_HZ + fcn * GLO_L1_DELTA_HZ;
    }
    case CODE_GLO_L2OF: {
      int8_t fcn = sat_info - MSM_GLO_FCN_OFFSET;
      return GLO_L2_HZ + fcn * GLO_L2_DELTA_HZ;
    }
    case CODE_BDS2_B11:
      return BDS2_B11_HZ;
    case CODE_BDS2_B2:
      return BDS2_B2_HZ;
    case CODE_SBAS_L1CA:
      return SBAS_L1_HZ;
    case CODE_GAL_E1B:
    case CODE_GAL_E1C:
    case CODE_GAL_E1X:
      return GAL_E1_HZ;
    case CODE_GAL_E7I:
    case CODE_GAL_E7Q:
    case CODE_GAL_E7X:
      return GAL_E7_HZ;
    case CODE_GAL_E5I:
    case CODE_GAL_E5Q:
    case CODE_GAL_E5X:
      return GAL_E5_HZ;
    case CODE_GAL_E6B:
    case CODE_GAL_E6C:
    case CODE_GAL_E6X:
      return GAL_E6_HZ;
    case CODE_GAL_E8:
      return GAL_E8_HZ;
    case CODE_QZS_L1CA:
      return QZS_L1_HZ;
    case CODE_QZS_L2CM:
    case CODE_QZS_L2CL:
    case CODE_QZS_L2CX:
      return QZS_L2_HZ;
    case CODE_QZS_L5I:
    case CODE_QZS_L5Q:
    case CODE_QZS_L5X:
      return QZS_L5_HZ;
    case CODE_INVALID:
    case CODE_COUNT:
    default:
      return 0;
  }
}

/** Convert message number into MSM message type
 *
 * \param msg_num RTCM message number
 * \return MSM type enum
 */
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

/** Convert message number into constellation enum
 *
 * \param msg_num RTCM message number
 * \return constellation enum
 */
constellation_t to_constellation(uint16_t msg_num) {
  if (msg_num >= 1071 && msg_num <= 1077) {
    return CONSTELLATION_GPS;
  }
  if (msg_num >= 1081 && msg_num <= 1087) {
    return CONSTELLATION_GLO;
  }
  if (msg_num >= 1091 && msg_num <= 1097) {
    return CONSTELLATION_GAL;
  }
  if (msg_num >= 1101 && msg_num <= 1107) {
    return CONSTELLATION_SBAS;
  }
  if (msg_num >= 1111 && msg_num <= 1117) {
    return CONSTELLATION_QZS;
  }
  if (msg_num >= 1121 && msg_num <= 1127) {
    return CONSTELLATION_BDS2;
  }
  return CONSTELLATION_INVALID;
}

static uint8_t get_msm_gps_prn(uint8_t sat_id) {
  /*RTCM 10403.3 Table 3.5-90 */
  return sat_id + GPS_FIRST_PRN;
}

static code_t get_msm_gps_code(uint8_t signal_id) {
  /* RTCM 10403.3 Table 3.5-91 */
  switch (signal_id) {
    case 2: /* 1C */
      return CODE_GPS_L1CA;
    case 3: /* 1P */
      return CODE_GPS_L1P;
    /* case 4: 1W */
    /* case 8: 2C */
    case 9: /* 2P */
      return CODE_GPS_L2P;
    /* case 10: 2W */
    case 15: /* 2S */
      return CODE_GPS_L2CM;
    case 16: /* 2L */
      return CODE_GPS_L2CL;
    case 17: /* 2X */
      return CODE_GPS_L2CX;
    case 22: /* 5I */
      return CODE_GPS_L5I;
    case 23: /* 5Q */
      return CODE_GPS_L5Q;
    case 24: /* 5X */
      return CODE_GPS_L5X;
    /* case 30: 1S */
    /* case 31: 1L */
    /* case 32: 1X */
    default:
      return CODE_INVALID;
  }
}

static uint8_t get_msm_glo_prn(uint8_t sat_id) {
  /*RTCM 10403.3 Table 3.5-95 */
  return sat_id + GLO_FIRST_PRN;
}

static code_t get_msm_glo_code(uint8_t signal_id) {
  /* RTCM 10403.3 Table 3.5-96 */
  switch (signal_id) {
    case 2: /* 1C */
      return CODE_GLO_L1OF;
    /* case 3: 1P */
    case 8: /* 2C */
      return CODE_GLO_L2OF;
    /* case 9: 2P */
    default:
      return CODE_INVALID;
  }
}

static uint8_t get_msm_gal_prn(uint8_t sat_id) {
  /*RTCM 10403.3 Table 3.5-98 */

  /* Note: need to check how these are encoded in SBP:
   *  51 - GIOVE-A
   *  52 - GIOVE-B
   */

  return sat_id + GAL_FIRST_PRN;
}

static code_t get_msm_gal_code(uint8_t signal_id) {
  /* RTCM 10403.3 Table 3.5-99 */
  switch (signal_id) {
    case 2: /* 1C */
      return CODE_GAL_E1C;
    /* case 3: 1A */
    case 4: /* 1B */
      return CODE_GAL_E1B;
    case 5: /* 1X */
      return CODE_GAL_E1X;
    /* case 6: 1Z */
    case 8: /* 6C */
      return CODE_GAL_E6C;
    /* case 9: 6A */
    case 10: /* 6B */
      return CODE_GAL_E6B;
    case 11: /* 6X */
      return CODE_GAL_E6X;
    /* case 12: 6Z */
    case 14: /* 7I */
      return CODE_GAL_E7I;
    case 15: /* 7Q */
      return CODE_GAL_E7Q;
    case 16: /* 7X */
      return CODE_GAL_E7X;
    case 18:              /* 8I */
    case 19:              /* 8Q */
    case 20:              /* 8X */
      return CODE_GAL_E8; /* ? */
    case 22:              /* 5I */
      return CODE_GAL_E5I;
    case 23: /* 5Q */
      return CODE_GAL_E5Q;
    case 24: /* 5X */
      return CODE_GAL_E5X;
    default:
      return CODE_INVALID;
  }
}

static uint8_t get_msm_sbas_prn(uint8_t sat_id) {
  /*RTCM 10403.3 Table 3.5-101 */
  return sat_id + SBAS_FIRST_PRN;
}

static code_t get_msm_sbas_code(uint8_t signal_id) {
  /* RTCM 10403.3 Table 3.5-102 */
  switch (signal_id) {
    case 2: /* 1C */
      return CODE_SBAS_L1CA;
    /* case 22: 5I */
    /* case 23: 5Q */
    /* case 24: 5X */
    default:
      return CODE_INVALID;
  }
}

static uint8_t get_msm_qzs_prn(uint8_t sat_id) {
  /*RTCM 10403.3 Table 3.5-104 */
  return sat_id + QZS_FIRST_PRN;
}

static code_t get_msm_qzs_code(uint8_t signal_id) {
  /* RTCM 10403.3 Table 3.5-105 */
  switch (signal_id) {
    case 2: /* 1C */
      return CODE_QZS_L1CA;
    /* case 9:  6S */
    /* case 10: 6L */
    /* case 11: 6X */
    case 15: /* 2S */
      return CODE_QZS_L2CM;
    case 16: /* 2L */
      return CODE_QZS_L2CL;
    case 17: /* 2X */
      return CODE_QZS_L2CX;
    case 22: /* 5I */
      return CODE_QZS_L5I;
    case 23: /* 5Q */
      return CODE_QZS_L5Q;
    case 24: /* 5X */
      return CODE_QZS_L5X;
    /* case 30: 1S */
    /* case 31: 1L */
    /* case 32: 1X */
    default:
      return CODE_INVALID;
  }
}

static uint8_t get_msm_bds2_prn(uint8_t sat_id) {
  /*RTCM 10403.3 Table 3.5-107 */
  return sat_id + BDS2_FIRST_PRN;
}

static code_t get_msm_bds2_code(uint8_t signal_id) {
  /* RTCM 10403.3 Table 3.5-108 */
  switch (signal_id) {
    case 2: /* 2I */
      return CODE_BDS2_B11;
    /* case 3:  2Q */
    /* case 4:  2X */
    /* case 8:  6I */
    /* case 9:  6Q */
    /* case 10:  6X */
    case 14: /* 7I */
      return CODE_BDS2_B2;
    /* case 15:  7Q */
    /* case 16:  7X */
    default:
      return CODE_INVALID;
  }
}

/** Count the true values in a Boolean array
 *
 * \param mask_size
 * \param bool_mask Boolean array
 * \return number of true values in the array
 */
uint8_t count_mask_values(uint8_t mask_size, const bool mask[]) {
  uint8_t ret = 0;
  for (uint8_t i = 0; i < mask_size; i++) {
    ret += mask[i] ? 1 : 0;
  }
  return ret;
}

/** Return the position of the nth true value in a Boolean array
 *
 * \param mask_size
 * \param bool_mask Boolean array
 * \param n A number between 1 and count_mask_values (causes an assert if not)
 * \return The 0-based position of the nth true value in the array
 */
uint8_t find_nth_mask_value(const uint8_t mask_size,
                            const bool mask[],
                            const uint8_t n) {
  assert(n > 0);
  uint8_t trues_found = 0;
  for (uint8_t pos = 0; pos < mask_size; pos++) {
    trues_found += mask[pos] ? 1 : 0;
    if (n == trues_found) {
      /* this is the nth set value in the array, return its position */
      return pos;
    }
  }
  assert(!"n is larger than count_mask_values");
  /* this will never be reached */
  return 0;
}

/** Get the code enum of an MSM signal
 *
 * \param header Pointer to message header
 * \param signal_index 0-based index into the signal mask
 * \return code enum (CODE_INVALID for unsupported codes/constellations)
 */
code_t msm_signal_to_code(const rtcm_msm_header *header, uint8_t signal_index) {
  constellation_t cons = to_constellation(header->msg_num);
  uint8_t code_index =
      find_nth_mask_value(
          MSM_SIGNAL_MASK_SIZE, header->signal_mask, signal_index + 1) +
      1;

  switch (cons) {
    case CONSTELLATION_GPS:
      return get_msm_gps_code(code_index);
    case CONSTELLATION_SBAS:
      return get_msm_sbas_code(code_index);
    case CONSTELLATION_GLO:
      return get_msm_glo_code(code_index);
    case CONSTELLATION_BDS2:
      return get_msm_bds2_code(code_index);
    case CONSTELLATION_QZS:
      return get_msm_qzs_code(code_index);
    case CONSTELLATION_GAL:
      return get_msm_gal_code(code_index);
    case CONSTELLATION_INVALID:
    case CONSTELLATION_COUNT:
    default:
      return CODE_INVALID;
  }
}

/** Get the PRN from an MSM satellite index
 *
 * \param header Pointer to message header
 * \param satellite_index 0-based index into the satellite mask
 * \return PRN (or 0 for invalid constellation)
 */
uint8_t msm_sat_to_prn(const rtcm_msm_header *header, uint8_t satellite_index) {
  constellation_t cons = to_constellation(header->msg_num);
  uint8_t prn_index = find_nth_mask_value(
      MSM_SATELLITE_MASK_SIZE, header->satellite_mask, satellite_index + 1);

  switch (cons) {
    case CONSTELLATION_GPS:
      return get_msm_gps_prn(prn_index);
    case CONSTELLATION_SBAS:
      return get_msm_sbas_prn(prn_index);
    case CONSTELLATION_GLO:
      return get_msm_glo_prn(prn_index);
    case CONSTELLATION_BDS2:
      return get_msm_bds2_prn(prn_index);
    case CONSTELLATION_QZS:
      return get_msm_qzs_prn(prn_index);
    case CONSTELLATION_GAL:
      return get_msm_gal_prn(prn_index);
    case CONSTELLATION_INVALID:
    case CONSTELLATION_COUNT:
    default:
      return 0;
  }
}
