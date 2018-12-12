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
#include <rtcm3/eph_decode.h>

/** Decode an RTCMv3 GPS Ephemeris Message
 *
 * \param buff The input data buffer
 * \param RTCM message struct
 * \return  - RC_OK : Success
 *          - RC_MESSAGE_TYPE_MISMATCH : Message type mismatch
 *          - RC_INVALID_MESSAGE : Cell mask too large or invalid TOW
 */
rtcm3_rc rtcm3_decode_gps_eph(const uint8_t buff[], rtcm_msg_eph *msg_eph) {
  uint16_t bit = 0;
  uint16_t msg_num = rtcm_getbitu(buff, bit, 12);
  if (msg_num != 1019) {
    return RC_MESSAGE_TYPE_MISMATCH;
  }
  bit += 12;
  msg_eph->sat_id = rtcm_getbitu(buff, bit, 6);
  bit += 6;
  msg_eph->wn = rtcm_getbitu(buff, bit, 10);
  bit += 10;
  msg_eph->ura = rtcm_getbitu(buff, bit, 4);
  bit += 4;
  /*uint8_t l2_code = */ rtcm_getbitu(buff, bit, 2);
  bit += 2;
  msg_eph->kepler.inc_dot = rtcm_getbits(buff, bit, 14);
  bit += 14;
  msg_eph->kepler.iode = rtcm_getbitu(buff, bit, 8);
  bit += 8;
  msg_eph->toe = rtcm_getbitu(buff, bit, 16);
  bit += 16;
  msg_eph->kepler.af2 = rtcm_getbits(buff, bit, 8);
  bit += 8;
  msg_eph->kepler.af1 = rtcm_getbits(buff, bit, 16);
  bit += 16;
  msg_eph->kepler.af0 = rtcm_getbits(buff, bit, 22);
  bit += 22;
  msg_eph->kepler.iodc = rtcm_getbitu(buff, bit, 10);
  bit += 10;
  msg_eph->kepler.crs = rtcm_getbits(buff, bit, 16);
  bit += 16;
  msg_eph->kepler.dn = rtcm_getbits(buff, bit, 16);
  bit += 16;
  msg_eph->kepler.m0 = rtcm_getbits(buff, bit, 32);
  bit += 32;
  msg_eph->kepler.cuc = rtcm_getbits(buff, bit, 16);
  bit += 16;
  msg_eph->kepler.ecc = rtcm_getbitu(buff, bit, 32);
  bit += 32;
  msg_eph->kepler.cus = rtcm_getbits(buff, bit, 16);
  bit += 16;
  msg_eph->kepler.sqrta = rtcm_getbitu(buff, bit, 32);
  bit += 32;
  msg_eph->kepler.toc = rtcm_getbitu(buff, bit, 16);
  bit += 16;
  msg_eph->kepler.cic = rtcm_getbits(buff, bit, 16);
  bit += 16;
  msg_eph->kepler.omega0 = rtcm_getbits(buff, bit, 32);
  bit += 32;
  msg_eph->kepler.cis = rtcm_getbits(buff, bit, 16);
  bit += 16;
  msg_eph->kepler.inc = rtcm_getbits(buff, bit, 32);
  bit += 32;
  msg_eph->kepler.crc = rtcm_getbits(buff, bit, 16);
  bit += 16;
  msg_eph->kepler.w = rtcm_getbits(buff, bit, 32);
  bit += 32;
  msg_eph->kepler.omegadot = rtcm_getbits(buff, bit, 24);
  bit += 24;
  msg_eph->kepler.tgd_gps_s = rtcm_getbits(buff, bit, 8);
  bit += 8;
  msg_eph->health_bits = rtcm_getbitu(buff, bit, 6);
  bit += 6;
  /* L2 data bit */ rtcm_getbitu(buff, bit, 1);
  bit += 1;
  msg_eph->fit_interval = rtcm_getbitu(buff, bit, 1);
  bit += 1;

  return RC_OK;
}

/** Decode an RTCMv3 GLO Ephemeris Message
 *
 * \param buff The input data buffer
 * \param RTCM message struct
 * \return  - RC_OK : Success
 *          - RC_MESSAGE_TYPE_MISMATCH : Message type mismatch
 *          - RC_INVALID_MESSAGE : Cell mask too large or invalid TOW
 */
rtcm3_rc rtcm3_decode_glo_eph(const uint8_t buff[], rtcm_msg_eph *msg_eph) {
  uint16_t bit = 0;
  uint16_t msg_num = rtcm_getbitu(buff, bit, 12);
  if (msg_num != 1020) {
    return RC_MESSAGE_TYPE_MISMATCH;
  }
  bit += 12;
  msg_eph->sat_id = rtcm_getbitu(buff, bit, 6);
  bit += 6;
  msg_eph->glo.fcn = rtcm_getbitu(buff, bit, 5);
  bit += 5;
  /*alm health ind = */ rtcm_getbitu(buff, bit, 1);
  bit += 1;
  /*alm health ind valid = */ rtcm_getbitu(buff, bit, 1);
  bit += 1;
  msg_eph->fit_interval = rtcm_getbitu(buff, bit, 2);
  bit += 2;
  /* tk */ rtcm_getbitu(buff, bit, 12);
  bit += 12;
  /* MSB of Bn word */ rtcm_getbitu(buff, bit, 1);
  bit += 1;
  /* P2 */ rtcm_getbitu(buff, bit, 1);
  bit += 1;
  msg_eph->glo.t_b = rtcm_getbitu(buff, bit, 7);
  bit += 7;
  msg_eph->glo.vel[0] = rtcm_get_sign_magnitude_bit(buff, bit, 24);
  bit += 24;
  msg_eph->glo.pos[0] = rtcm_get_sign_magnitude_bit(buff, bit, 27);
  bit += 27;
  msg_eph->glo.acc[0] = rtcm_get_sign_magnitude_bit(buff, bit, 5);
  bit += 5;
  msg_eph->glo.vel[1] = rtcm_get_sign_magnitude_bit(buff, bit, 24);
  bit += 24;
  msg_eph->glo.pos[1] = rtcm_get_sign_magnitude_bit(buff, bit, 27);
  bit += 27;
  msg_eph->glo.acc[1] = rtcm_get_sign_magnitude_bit(buff, bit, 5);
  bit += 5;
  msg_eph->glo.vel[2] = rtcm_get_sign_magnitude_bit(buff, bit, 24);
  bit += 24;
  msg_eph->glo.pos[2] = rtcm_get_sign_magnitude_bit(buff, bit, 27);
  bit += 27;
  msg_eph->glo.acc[2] = rtcm_get_sign_magnitude_bit(buff, bit, 5);
  bit += 5;
  /* P3 */ rtcm_getbitu(buff, bit, 1);
  bit += 1;
  msg_eph->glo.gamma = rtcm_get_sign_magnitude_bit(buff, bit, 11);
  bit += 11;
  /* P */ rtcm_getbitu(buff, bit, 2);
  bit += 2;
  uint32_t mln3 = rtcm_getbitu(buff, bit, 1);
  bit += 1;
  msg_eph->glo.tau = rtcm_get_sign_magnitude_bit(buff, bit, 22);
  bit += 22;
  msg_eph->glo.d_tau = rtcm_get_sign_magnitude_bit(buff, bit, 5);
  bit += 5;
  /* EN */ rtcm_getbitu(buff, bit, 5);
  bit += 5;
  /* P4 */ rtcm_getbitu(buff, bit, 1);
  bit += 1;
  msg_eph->ura = rtcm_getbitu(buff, bit, 4);
  bit += 4;
  /* NT */ rtcm_getbitu(buff, bit, 11);
  bit += 11;
  /* M */ rtcm_getbitu(buff, bit, 2);
  bit += 2;
  /* Avail Add Data */ rtcm_getbitu(buff, bit, 1);
  bit += 1;
  /* NA */ rtcm_getbitu(buff, bit, 11);
  bit += 11;
  /* Tc */ rtcm_get_sign_magnitude_bit(buff, bit, 32);
  bit += 32;
  /* N4 */ rtcm_getbitu(buff, bit, 5);
  bit += 5;
  /* Tgps */ rtcm_get_sign_magnitude_bit(buff, bit, 22);
  bit += 22;
  uint32_t mln5 = rtcm_getbitu(buff, bit, 1);
  bit += 1;
  /* reserved */ rtcm_getbitu(buff, bit, 7);
  bit += 7;
  msg_eph->health_bits = mln3 || mln5;
  return RC_OK;
}

/** Decode an RTCMv3 BDS Ephemeris Message
 *
 * \param buff The input data buffer
 * \param RTCM message struct
 * \return  - RC_OK : Success
 *          - RC_MESSAGE_TYPE_MISMATCH : Message type mismatch
 *          - RC_INVALID_MESSAGE : Cell mask too large or invalid TOW
 */
rtcm3_rc rtcm3_decode_bds_eph(const uint8_t buff[], rtcm_msg_eph *msg_eph) {
  uint16_t bit = 0;
  uint16_t msg_num = rtcm_getbitu(buff, bit, 12);
  if (msg_num != 1042) {
    return RC_MESSAGE_TYPE_MISMATCH;
  }
  bit += 12;
  msg_eph->sat_id = rtcm_getbitu(buff, bit, 6);
  if (msg_eph->sat_id <= BEIDOU_GEOS_MAX_PRN) {
    /* We do not support Beidou GEO satellites */
    return RC_INVALID_MESSAGE;
  }
  bit += 6;
  msg_eph->wn = rtcm_getbitu(buff, bit, 13);
  bit += 13;
  msg_eph->ura = rtcm_getbitu(buff, bit, 4);
  bit += 4;
  msg_eph->kepler.inc_dot = rtcm_getbits(buff, bit, 14);
  bit += 14;
  msg_eph->kepler.iode = rtcm_getbitu(buff, bit, 5);
  bit += 5;
  msg_eph->kepler.toc = rtcm_getbitu(buff, bit, 17);
  bit += 17;
  msg_eph->kepler.af2 = rtcm_getbits(buff, bit, 11);
  bit += 11;
  msg_eph->kepler.af1 = rtcm_getbits(buff, bit, 22);
  bit += 22;
  msg_eph->kepler.af0 = rtcm_getbits(buff, bit, 24);
  bit += 24;
  msg_eph->kepler.iodc = rtcm_getbitu(buff, bit, 5);
  bit += 5;
  msg_eph->kepler.crs = rtcm_getbits(buff, bit, 18);
  bit += 18;
  msg_eph->kepler.dn = rtcm_getbits(buff, bit, 16);
  bit += 16;
  msg_eph->kepler.m0 = rtcm_getbits(buff, bit, 32);
  bit += 32;
  msg_eph->kepler.cuc = rtcm_getbits(buff, bit, 18);
  bit += 18;
  msg_eph->kepler.ecc = rtcm_getbitu(buff, bit, 32);
  bit += 32;
  msg_eph->kepler.cus = rtcm_getbits(buff, bit, 18);
  bit += 18;
  msg_eph->kepler.sqrta = rtcm_getbitu(buff, bit, 32);
  bit += 32;
  msg_eph->toe = rtcm_getbitu(buff, bit, 17);
  bit += 17;
  msg_eph->kepler.cic = rtcm_getbits(buff, bit, 18);
  bit += 18;
  msg_eph->kepler.omega0 = rtcm_getbits(buff, bit, 32);
  bit += 32;
  msg_eph->kepler.cis = rtcm_getbits(buff, bit, 18);
  bit += 18;
  msg_eph->kepler.inc = rtcm_getbits(buff, bit, 32);
  bit += 32;
  msg_eph->kepler.crc = rtcm_getbits(buff, bit, 18);
  bit += 18;
  msg_eph->kepler.w = rtcm_getbits(buff, bit, 32);
  bit += 32;
  msg_eph->kepler.omegadot = rtcm_getbits(buff, bit, 24);
  bit += 24;
  msg_eph->kepler.tgd_bds_s[0] = rtcm_getbits(buff, bit, 10);
  bit += 10;
  msg_eph->kepler.tgd_bds_s[1] = rtcm_getbits(buff, bit, 10);
  bit += 10;
  msg_eph->valid = rtcm_getbitu(buff, bit, 1);
  msg_eph->health_bits = msg_eph->valid;
  bit += 1;
  return RC_OK;
}

/** Decode an RTCMv3 GAL (common part) Ephemeris Message
 *
 * \param buff The input data buffer
 * \param msg_eph RTCM message struct
 * \return bit position in the RTCM frame
 */
static uint16_t rtcm3_decode_gal_eph_common(const uint8_t buff[],
                                            rtcm_msg_eph *msg_eph) {
  uint16_t bit = 12;
  msg_eph->sat_id = rtcm_getbitu(buff, bit, 6);
  bit += 6;
  msg_eph->wn = rtcm_getbitu(buff, bit, 12);
  bit += 12;
  msg_eph->kepler.iode = rtcm_getbitu(buff, bit, 10);
  bit += 10;
  msg_eph->ura = rtcm_getbitu(buff, bit, 8);
  bit += 8;
  msg_eph->kepler.inc_dot = rtcm_getbits(buff, bit, 14);
  bit += 14;
  msg_eph->kepler.toc = rtcm_getbitu(buff, bit, 14);
  bit += 14;
  msg_eph->kepler.af2 = rtcm_getbits(buff, bit, 6);
  bit += 6;
  msg_eph->kepler.af1 = rtcm_getbits(buff, bit, 21);
  bit += 21;
  msg_eph->kepler.af0 = rtcm_getbits(buff, bit, 31);
  bit += 31;
  msg_eph->kepler.crs = rtcm_getbits(buff, bit, 16);
  bit += 16;
  msg_eph->kepler.dn = rtcm_getbits(buff, bit, 16);
  bit += 16;
  msg_eph->kepler.m0 = rtcm_getbits(buff, bit, 32);
  bit += 32;
  msg_eph->kepler.cuc = rtcm_getbits(buff, bit, 16);
  bit += 16;
  msg_eph->kepler.ecc = rtcm_getbitu(buff, bit, 32);
  bit += 32;
  msg_eph->kepler.cus = rtcm_getbits(buff, bit, 16);
  bit += 16;
  msg_eph->kepler.sqrta = rtcm_getbitu(buff, bit, 32);
  bit += 32;
  msg_eph->toe = rtcm_getbitu(buff, bit, 14);
  bit += 14;
  msg_eph->kepler.cic = rtcm_getbits(buff, bit, 16);
  bit += 16;
  msg_eph->kepler.omega0 = rtcm_getbits(buff, bit, 32);
  bit += 32;
  msg_eph->kepler.cis = rtcm_getbits(buff, bit, 16);
  bit += 16;
  msg_eph->kepler.inc = rtcm_getbits(buff, bit, 32);
  bit += 32;
  msg_eph->kepler.crc = rtcm_getbits(buff, bit, 16);
  bit += 16;
  msg_eph->kepler.w = rtcm_getbits(buff, bit, 32);
  bit += 32;
  msg_eph->kepler.omegadot = rtcm_getbits(buff, bit, 24);
  bit += 24;
  return bit;
}

/** Decode an RTCMv3 GAL (I/NAV message) Ephemeris Message
 *
 * \param buff The input data buffer
 * \param RTCM message struct
 * \return  - RC_OK : Success
 *          - RC_MESSAGE_TYPE_MISMATCH : Message type mismatch
 *          - RC_INVALID_MESSAGE : Cell mask too large or invalid TOW
 */
rtcm3_rc rtcm3_decode_gal_eph(const uint8_t buff[], rtcm_msg_eph *msg_eph) {
  uint16_t bit = 0;
  uint16_t msg_num = rtcm_getbitu(buff, bit, 12);
  if (msg_num != 1046) {
    return RC_MESSAGE_TYPE_MISMATCH;
  }

  /* parse common I/NAV and F/NAV part */
  bit = rtcm3_decode_gal_eph_common(buff, msg_eph);

  msg_eph->kepler.tgd_gal_s[0] = rtcm_getbits(buff, bit, 10);
  bit += 10;
  msg_eph->kepler.tgd_gal_s[1] = rtcm_getbits(buff, bit, 10);
  bit += 10;
  msg_eph->health_bits = rtcm_getbits(buff, bit, 6);
  bit += 6;
  /* reserved */ rtcm_getbits(buff, bit, 2);
  bit += 2;

  return RC_OK;
}

/** Decode an RTCMv3 GAL (F/NAV message) Ephemeris Message
 *
 * \param buff The input data buffer
 * \param RTCM message struct
 * \return  - RC_OK : Success
 *          - RC_MESSAGE_TYPE_MISMATCH : Message type mismatch
 *          - RC_INVALID_MESSAGE : Cell mask too large or invalid TOW
 */
rtcm3_rc rtcm3_decode_gal_eph_fnav(const uint8_t buff[],
                                   rtcm_msg_eph *msg_eph) {
  uint16_t bit = 0;
  uint16_t msg_num = rtcm_getbitu(buff, bit, 12);
  if (msg_num != 1045) {
    return RC_MESSAGE_TYPE_MISMATCH;
  }

  /* parse common F/NAV and I/NAV part */
  bit = rtcm3_decode_gal_eph_common(buff, msg_eph);

  msg_eph->kepler.tgd_gal_s[0] = rtcm_getbits(buff, bit, 10);
  bit += 10;
  msg_eph->kepler.tgd_gal_s[1] = 0;
  msg_eph->health_bits = rtcm_getbits(buff, bit, 3);
  bit += 3;
  /* reserved */ rtcm_getbits(buff, bit, 7);
  bit += 7;

  return RC_OK;
}
