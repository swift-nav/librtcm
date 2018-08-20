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
#include <stdio.h>

#define LIBRTCM_LOG_INTERNAL
#include "rtcm_logging.h"

/** Find the frequency of an MSM signal
 *
 * \param header Pointer to message header
 * \param signal_index 0-based index into the signal mask
 * \param glo_fcn The FCN value for GLO satellites
 * \param glo_fcn_valid Validity flag for glo_fcn
 * \param p_freq Pointer to write the frequency output to
 * \return true if a valid frequency was returned
 */
bool msm_signal_frequency(const rtcm_msm_header *header,
                          const uint8_t signal_index,
                          const uint8_t glo_fcn,
                          const bool glo_fcn_valid,
                          double *p_freq) {
  code_t code = msm_signal_to_code(header, signal_index);

  /* TODO: use sid_to_carr_freq from LNSP */

  switch ((int8_t)code) {
    case CODE_GPS_L1CA:
    case CODE_GPS_L1P:
    case CODE_GPS_L1CI:
    case CODE_GPS_L1CQ:
    case CODE_GPS_L1CX:
      *p_freq = GPS_L1_HZ;
      return true;
    case CODE_GPS_L2CM:
    case CODE_GPS_L2CL:
    case CODE_GPS_L2CX:
    case CODE_GPS_L2P:
      *p_freq = GPS_L2_HZ;
      return true;
    case CODE_GPS_L5I:
    case CODE_GPS_L5Q:
    case CODE_GPS_L5X:
      *p_freq = GPS_L5_HZ;
      return true;
    case CODE_GLO_L1OF:
    case CODE_GLO_L1P:
      /* GLO FCN given in the sat info field, see Table 3.4-6 */
      if (glo_fcn_valid) {
        *p_freq = GLO_L1_HZ + (glo_fcn - MSM_GLO_FCN_OFFSET) * GLO_L1_DELTA_HZ;
        return true;
      } else {
        return false;
      }
    case CODE_GLO_L2OF:
    case CODE_GLO_L2P:
      if (glo_fcn_valid) {
        *p_freq = GLO_L2_HZ + (glo_fcn - MSM_GLO_FCN_OFFSET) * GLO_L2_DELTA_HZ;
        return true;
      } else {
        return false;
      }
    case CODE_BDS2_B1:
      *p_freq = BDS2_B11_HZ;
      return true;
    case CODE_BDS2_B2:
      *p_freq = BDS2_B2_HZ;
      return true;
    case CODE_SBAS_L1CA:
      *p_freq = SBAS_L1_HZ;
      return true;
    case CODE_SBAS_L5I:
    case CODE_SBAS_L5Q:
    case CODE_SBAS_L5X:
      *p_freq = SBAS_L5_HZ;
      return true;
    case CODE_GAL_E1B:
    case CODE_GAL_E1C:
    case CODE_GAL_E1X:
      *p_freq = GAL_E1_HZ;
      return true;
    case CODE_GAL_E7I:
    case CODE_GAL_E7Q:
    case CODE_GAL_E7X:
      *p_freq = GAL_E7_HZ;
      return true;
    case CODE_GAL_E5I:
    case CODE_GAL_E5Q:
    case CODE_GAL_E5X:
      *p_freq = GAL_E5_HZ;
      return true;
    case CODE_GAL_E6B:
    case CODE_GAL_E6C:
    case CODE_GAL_E6X:
      *p_freq = GAL_E6_HZ;
      return true;
    case CODE_GAL_E8I:
    case CODE_GAL_E8Q:
    case CODE_GAL_E8X:
      *p_freq = GAL_E8_HZ;
      return true;
    case CODE_QZS_L1CA:
    case CODE_QZS_L1CI:
    case CODE_QZS_L1CQ:
    case CODE_QZS_L1CX:
      *p_freq = QZS_L1_HZ;
      return true;
    case CODE_QZS_L2CM:
    case CODE_QZS_L2CL:
    case CODE_QZS_L2CX:
      *p_freq = QZS_L2_HZ;
      return true;
    case CODE_QZS_L5I:
    case CODE_QZS_L5Q:
    case CODE_QZS_L5X:
      *p_freq = QZS_L5_HZ;
      return true;
    case CODE_INVALID:
    case CODE_COUNT:
    default:
      return false;
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
 * This also supports SSR messages
 * \param msg_num RTCM message number
 * \return constellation enum
 */
constellation_t to_constellation(uint16_t msg_num) {
  if ((msg_num >= 1071 && msg_num <= 1077) ||
      (msg_num >= 1057 && msg_num <= 1062) || msg_num == 1265) {
    return CONSTELLATION_GPS;
  }
  if ((msg_num >= 1081 && msg_num <= 1087) ||
      (msg_num >= 1063 && msg_num <= 1068) || msg_num == 1266) {
    return CONSTELLATION_GLO;
  }
  if ((msg_num >= 1091 && msg_num <= 1097) ||
      (msg_num >= 1240 && msg_num <= 1245) || (msg_num == 1267)) {
    return CONSTELLATION_GAL;
  }
  if ((msg_num >= 1101 && msg_num <= 1107) || msg_num == 1269) {
    return CONSTELLATION_SBAS;
  }
  if ((msg_num >= 1111 && msg_num <= 1117) ||
      (msg_num >= 1246 && msg_num <= 1251) || msg_num == 1268) {
    return CONSTELLATION_QZS;
  }
  if ((msg_num >= 1121 && msg_num <= 1127) ||
      (msg_num >= 1258 && msg_num <= 1263) || msg_num == 1270) {
    return CONSTELLATION_BDS2;
  }
  return CONSTELLATION_INVALID;
}

static uint8_t get_msm_gps_prn(uint8_t sat_id) {
  /*RTCM 10403.3 Table 3.5-90 */
  uint8_t prn = sat_id + GPS_FIRST_PRN;
  return (prn <= GPS_LAST_PRN) ? prn : PRN_INVALID;
}

static code_t get_msm_gps_code(uint8_t signal_id) {
  /* RTCM 10403.3 Table 3.5-91 */
  switch (signal_id) {
    case 2: /* 1C */
      return CODE_GPS_L1CA;
    case 3: /* 1P */
      return CODE_GPS_L1P;
    case 4: /* 1W */
      return CODE_GPS_L1P;
    /* case 8: 2C */
    case 9: /* 2P */
      return CODE_GPS_L2P;
    case 10: /* 2W */
      return CODE_GPS_L2P;
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
    case 30: /* 1S */
      return CODE_GPS_L1CI;
    case 31: /* 1L */
      return CODE_GPS_L1CQ;
    case 32: /* 1X */
      return CODE_GPS_L1CX;
    default:
      return CODE_INVALID;
  }
}

static uint8_t get_msm_glo_prn(uint8_t sat_id) {
  /*RTCM 10403.3 Table 3.5-95 */
  uint8_t prn = sat_id + GLO_FIRST_PRN;
  return (prn <= GLO_LAST_PRN) ? prn : PRN_INVALID;
}

#define MSM_L1P_WARN_MSG "Received GLO L1P MSM Message from base station"
#define MSM_L2P_WARN_MSG "Received GLO L2P MSM Message from base station"

static code_t get_msm_glo_code(uint8_t signal_id) {
  /* RTCM 10403.3 Table 3.5-96 */
  switch (signal_id) {
    case 3: /* 1P */
      return CODE_GLO_L1P;
    case 2: /* 1C */
      return CODE_GLO_L1OF;
    case 9: /* 2P */
      return CODE_GLO_L2P;
    case 8: /* 2C */
      return CODE_GLO_L2OF;
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

  uint8_t prn = sat_id + GAL_FIRST_PRN;
  return (prn <= GAL_LAST_PRN) ? prn : PRN_INVALID;
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
    case 18: /* 8I */
      return CODE_GAL_E8I;
    case 19: /* 8Q */
      return CODE_GAL_E8Q;
    case 20: /* 8X */
      return CODE_GAL_E8X;
    case 22: /* 5I */
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
  uint8_t prn = sat_id + SBAS_FIRST_PRN;
  return (prn <= SBAS_LAST_PRN) ? prn : PRN_INVALID;
}

static code_t get_msm_sbas_code(uint8_t signal_id) {
  /* RTCM 10403.3 Table 3.5-102 */
  switch (signal_id) {
    case 2: /* 1C */
      return CODE_SBAS_L1CA;
    case 22: /* 5I */
      return CODE_SBAS_L5I;
    case 23: /* 5Q */
      return CODE_SBAS_L5Q;
    case 24: /* 5X */
      return CODE_SBAS_L5X;
    default:
      return CODE_INVALID;
  }
}

static uint8_t get_msm_qzs_prn(uint8_t sat_id) {
  /*RTCM 10403.3 Table 3.5-104 */
  uint8_t prn = sat_id + QZS_FIRST_PRN;
  return (prn <= QZS_LAST_PRN) ? prn : PRN_INVALID;
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
    case 30: /* 1S */
      return CODE_QZS_L1CI;
    case 31: /* 1L */
      return CODE_QZS_L1CQ;
    case 32: /* 1X */
      return CODE_QZS_L1CX;
    default:
      return CODE_INVALID;
  }
}

static uint8_t get_msm_bds2_prn(uint8_t sat_id) {
  /*RTCM 10403.3 Table 3.5-107 */
  uint8_t prn = sat_id + BDS2_FIRST_PRN;
  return (prn <= BDS2_LAST_PRN) ? prn : PRN_INVALID;
}

static code_t get_msm_bds2_code(uint8_t signal_id) {
  /* RTCM 10403.3 Table 3.5-108 */
  switch (signal_id) {
    case 2: /* 2I */
      return CODE_BDS2_B1;
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
      return PRN_INVALID;
  }
}

/** Find the frequency channel number (FCN) of a GLO signal
 *
 * \param header Pointer to message header
 * \param sat_index 0-based index into the satellite mask
 * \param sat_info Array of satellite info
 * \param sat_info_valid Array of satellite info validity flags
 * \param glo_sv_id_fcn_map Optional GLO FCN table (size MAX_GLO_PRN + 1)
 * \param glo_fcn Output pointer for the FCN value
 * \return true if a valid FCN was returned
 */
bool get_glo_fcn(const rtcm_msm_header *header,
                 const uint8_t sat,
                 const uint8_t sat_info[],
                 const bool sat_info_valid[],
                 const uint8_t glo_sv_id_fcn_map[],
                 uint8_t *glo_fcn) {
  if (CONSTELLATION_GLO != to_constellation(header->msg_num)) {
    return false;
  }

  *glo_fcn = MSM_GLO_FCN_UNKNOWN;
  if (sat_info_valid[sat]) {
    /* get FCN from sat_info if given */
    *glo_fcn = sat_info[sat];
  } else if (NULL != glo_sv_id_fcn_map) {
    /* use the lookup table if given */
    uint8_t sat_id = msm_sat_to_prn(header, sat);
    *glo_fcn = glo_sv_id_fcn_map[sat_id];
  }

  /* valid values are from 0 to MSM_GLO_MAX_FCN */
  return (*glo_fcn <= MSM_GLO_MAX_FCN);
}
