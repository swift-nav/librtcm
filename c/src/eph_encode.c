/*
 * Copyright (C) 2019 Swift Navigation Inc.
 * Contact: Swift Navigation <dev@swiftnav.com>
 *
 * This source is subject to the license found in the file 'LICENSE' which must
 * be distributed together with this source. All other rights reserved.
 *
 * THIS CODE AND INFORMATION IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND,
 * EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR PURPOSE.
 */

#include "rtcm3/eph_encode.h"
#include <assert.h>
#include <string.h>
#include "rtcm3/bits.h"

uint16_t rtcm3_encode_gps_eph(const rtcm_msg_eph *msg_1019, uint8_t buff[]) {
  assert(msg_1019);
  uint16_t bit = 0;
  rtcm_setbitu(buff, bit, 12, 1019);
  bit += 12;
  rtcm_setbitu(buff, bit, 6, msg_1019->sat_id);
  bit += 6;
  rtcm_setbitu(buff, bit, 10, msg_1019->wn);
  bit += 10;
  rtcm_setbitu(buff, bit, 4, msg_1019->ura);
  bit += 4;
  rtcm_setbitu(buff, bit, 2, msg_1019->kepler.codeL2);
  bit += 2;
  rtcm_setbits(buff, bit, 14, msg_1019->kepler.inc_dot);
  bit += 14;
  rtcm_setbitu(buff, bit, 8, msg_1019->kepler.iode);
  bit += 8;
  rtcm_setbitu(buff, bit, 16, msg_1019->toe);
  bit += 16;
  rtcm_setbits(buff, bit, 8, msg_1019->kepler.af2);
  bit += 8;
  rtcm_setbits(buff, bit, 16, msg_1019->kepler.af1);
  bit += 16;
  rtcm_setbits(buff, bit, 22, msg_1019->kepler.af0);
  bit += 22;
  rtcm_setbitu(buff, bit, 10, msg_1019->kepler.iodc);
  bit += 10;
  rtcm_setbits(buff, bit, 16, msg_1019->kepler.crs);
  bit += 16;
  rtcm_setbits(buff, bit, 16, msg_1019->kepler.dn);
  bit += 16;
  rtcm_setbits(buff, bit, 32, msg_1019->kepler.m0);
  bit += 32;
  rtcm_setbits(buff, bit, 16, msg_1019->kepler.cuc);
  bit += 16;
  rtcm_setbitu(buff, bit, 32, msg_1019->kepler.ecc);
  bit += 32;
  rtcm_setbits(buff, bit, 16, msg_1019->kepler.cus);
  bit += 16;
  rtcm_setbitu(buff, bit, 32, msg_1019->kepler.sqrta);
  bit += 32;
  rtcm_setbitu(buff, bit, 16, msg_1019->kepler.toc);
  bit += 16;
  rtcm_setbits(buff, bit, 16, msg_1019->kepler.cic);
  bit += 16;
  rtcm_setbits(buff, bit, 32, msg_1019->kepler.omega0);
  bit += 32;
  rtcm_setbits(buff, bit, 16, msg_1019->kepler.cis);
  bit += 16;
  rtcm_setbits(buff, bit, 32, msg_1019->kepler.inc);
  bit += 32;
  rtcm_setbits(buff, bit, 16, msg_1019->kepler.crc);
  bit += 16;
  rtcm_setbits(buff, bit, 32, msg_1019->kepler.w);
  bit += 32;
  rtcm_setbits(buff, bit, 24, msg_1019->kepler.omegadot);
  bit += 24;
  rtcm_setbits(buff, bit, 8, msg_1019->kepler.tgd_gps_s);
  bit += 8;
  rtcm_setbitu(buff, bit, 6, msg_1019->health_bits);
  bit += 6;
  rtcm_setbitu(buff, bit, 1, msg_1019->kepler.L2_data_bit);
  bit += 1;
  rtcm_setbitu(buff, bit, 1, msg_1019->fit_interval);
  bit += 1;

  /* Round number of bits up to nearest whole byte. */
  return (bit + 7) / 8;
}

uint16_t rtcm3_encode_glo_eph(const rtcm_msg_eph *msg_1020, uint8_t buff[]) {
  assert(msg_1020);
  uint16_t bit = 0;
  rtcm_setbitu(buff, bit, 12, 1020);
  bit += 12;
  rtcm_setbitu(buff, bit, 6, msg_1020->sat_id);
  bit += 6;
  rtcm_setbitu(buff, bit, 5, msg_1020->glo.fcn);
  bit += 5;
  // Almanac health
  rtcm_setbitu(buff, bit, 1, 1);
  bit += 1;
  // Almanac health availability
  rtcm_setbitu(buff, bit, 1, 0);
  bit += 1;
  rtcm_setbitu(buff, bit, 2, msg_1020->fit_interval);
  bit += 2;
  // T_k
  rtcm_setbitu(buff, bit, 12, 0);
  bit += 12;
  rtcm_setbitu(buff, bit, 1, msg_1020->health_bits);
  bit += 1;
  // P2
  rtcm_setbitu(buff, bit, 1, 0);
  bit += 1;
  rtcm_setbitu(buff, bit, 7, msg_1020->glo.t_b);
  bit += 7;
  rtcm_set_sign_magnitude_bit(buff, bit, 24, msg_1020->glo.vel[0]);
  bit += 24;
  rtcm_set_sign_magnitude_bit(buff, bit, 27, msg_1020->glo.pos[0]);
  bit += 27;
  rtcm_set_sign_magnitude_bit(buff, bit, 5, msg_1020->glo.acc[0]);
  bit += 5;
  rtcm_set_sign_magnitude_bit(buff, bit, 24, msg_1020->glo.vel[1]);
  bit += 24;
  rtcm_set_sign_magnitude_bit(buff, bit, 27, msg_1020->glo.pos[1]);
  bit += 27;
  rtcm_set_sign_magnitude_bit(buff, bit, 5, msg_1020->glo.acc[1]);
  bit += 5;
  rtcm_set_sign_magnitude_bit(buff, bit, 24, msg_1020->glo.vel[2]);
  bit += 24;
  rtcm_set_sign_magnitude_bit(buff, bit, 27, msg_1020->glo.pos[2]);
  bit += 27;
  rtcm_set_sign_magnitude_bit(buff, bit, 5, msg_1020->glo.acc[2]);
  bit += 5;
  // P3
  rtcm_setbitu(buff, bit, 1, 0);
  bit += 1;
  rtcm_set_sign_magnitude_bit(buff, bit, 11, msg_1020->glo.gamma);
  bit += 11;
  // P
  rtcm_setbitu(buff, bit, 2, 0);
  bit += 2;
  rtcm_setbitu(buff, bit, 1, msg_1020->health_bits);
  bit += 1;
  rtcm_set_sign_magnitude_bit(buff, bit, 22, msg_1020->glo.tau);
  bit += 22;
  rtcm_set_sign_magnitude_bit(buff, bit, 5, msg_1020->glo.d_tau);
  bit += 5;
  // EN
  rtcm_setbitu(buff, bit, 5, 0);
  bit += 5;
  // P4
  rtcm_setbitu(buff, bit, 1, 0);
  bit += 1;
  rtcm_setbitu(buff, bit, 4, msg_1020->ura);
  bit += 4;
  // NT
  rtcm_setbitu(buff, bit, 11, 0);
  bit += 11;
  // M
  rtcm_setbitu(buff, bit, 2, 0);
  bit += 2;
  // Additional data
  rtcm_setbitu(buff, bit, 79, 0);
  bit += 79;

  /* Round number of bits up to nearest whole byte. */
  return (bit + 7) / 8;
}

uint16_t rtcm3_encode_bds_eph(const rtcm_msg_eph *msg_1042, uint8_t buff[]) {
  assert(msg_1042);
  uint16_t bit = 0;
  rtcm_setbitu(buff, bit, 12, 1042);
  bit += 12;
  rtcm_setbitu(buff, bit, 6, msg_1042->sat_id);
  bit += 6;
  rtcm_setbitu(buff, bit, 13, msg_1042->wn);
  bit += 13;
  rtcm_setbitu(buff, bit, 4, msg_1042->ura);
  bit += 4;
  rtcm_setbits(buff, bit, 14, msg_1042->kepler.inc_dot);
  bit += 14;
  rtcm_setbitu(buff, bit, 5, msg_1042->kepler.iode);
  bit += 5;
  rtcm_setbitu(buff, bit, 17, msg_1042->kepler.toc);
  bit += 17;
  rtcm_setbits(buff, bit, 11, msg_1042->kepler.af2);
  bit += 11;
  rtcm_setbits(buff, bit, 22, msg_1042->kepler.af1);
  bit += 22;
  rtcm_setbits(buff, bit, 24, msg_1042->kepler.af0);
  bit += 24;
  rtcm_setbitu(buff, bit, 5, msg_1042->kepler.iodc);
  bit += 5;
  rtcm_setbits(buff, bit, 18, msg_1042->kepler.crs);
  bit += 18;
  rtcm_setbits(buff, bit, 16, msg_1042->kepler.dn);
  bit += 16;
  rtcm_setbits(buff, bit, 32, msg_1042->kepler.m0);
  bit += 32;
  rtcm_setbits(buff, bit, 18, msg_1042->kepler.cuc);
  bit += 18;
  rtcm_setbitu(buff, bit, 32, msg_1042->kepler.ecc);
  bit += 32;
  rtcm_setbits(buff, bit, 18, msg_1042->kepler.cus);
  bit += 18;
  rtcm_setbitu(buff, bit, 32, msg_1042->kepler.sqrta);
  bit += 32;
  rtcm_setbitu(buff, bit, 17, msg_1042->toe);
  bit += 17;
  rtcm_setbits(buff, bit, 18, msg_1042->kepler.cic);
  bit += 18;
  rtcm_setbits(buff, bit, 32, msg_1042->kepler.omega0);
  bit += 32;
  rtcm_setbits(buff, bit, 18, msg_1042->kepler.cis);
  bit += 18;
  rtcm_setbits(buff, bit, 32, msg_1042->kepler.inc);
  bit += 32;
  rtcm_setbits(buff, bit, 18, msg_1042->kepler.crc);
  bit += 18;
  rtcm_setbits(buff, bit, 32, msg_1042->kepler.w);
  bit += 32;
  rtcm_setbits(buff, bit, 24, msg_1042->kepler.omegadot);
  bit += 24;
  rtcm_setbits(buff, bit, 10, msg_1042->kepler.tgd_bds_s[0]);
  bit += 10;
  rtcm_setbits(buff, bit, 10, msg_1042->kepler.tgd_bds_s[1]);
  bit += 10;
  rtcm_setbitu(buff, bit, 1, msg_1042->health_bits);
  bit += 1;

  /* Round number of bits up to nearest whole byte. */
  return (bit + 7) / 8;
}

/** Decode an RTCMv3 GAL (common part) Ephemeris Message
 *
 * \param buff The input data buffer
 * \param msg_eph RTCM message struct
 * \return bit position in the RTCM frame
 */
static void rtcm3_encode_gal_eph_common(const rtcm_msg_eph *msg_eph,
                                        uint8_t buff[],
                                        uint16_t *bit) {
  assert(msg_eph);
  rtcm_setbitu(buff, *bit, 6, msg_eph->sat_id);
  *bit += 6;
  rtcm_setbitu(buff, *bit, 12, msg_eph->wn);
  *bit += 12;
  rtcm_setbitu(buff, *bit, 10, msg_eph->kepler.iode);
  *bit += 10;
  rtcm_setbitu(buff, *bit, 8, msg_eph->ura);
  *bit += 8;
  rtcm_setbits(buff, *bit, 14, msg_eph->kepler.inc_dot);
  *bit += 14;
  rtcm_setbitu(buff, *bit, 14, msg_eph->kepler.toc);
  *bit += 14;
  rtcm_setbits(buff, *bit, 6, msg_eph->kepler.af2);
  *bit += 6;
  rtcm_setbits(buff, *bit, 21, msg_eph->kepler.af1);
  *bit += 21;
  rtcm_setbits(buff, *bit, 31, msg_eph->kepler.af0);
  *bit += 31;
  rtcm_setbits(buff, *bit, 16, msg_eph->kepler.crs);
  *bit += 16;
  rtcm_setbits(buff, *bit, 16, msg_eph->kepler.dn);
  *bit += 16;
  rtcm_setbits(buff, *bit, 32, msg_eph->kepler.m0);
  *bit += 32;
  rtcm_setbits(buff, *bit, 16, msg_eph->kepler.cuc);
  *bit += 16;
  rtcm_setbitu(buff, *bit, 32, msg_eph->kepler.ecc);
  *bit += 32;
  rtcm_setbits(buff, *bit, 16, msg_eph->kepler.cus);
  *bit += 16;
  rtcm_setbitu(buff, *bit, 32, msg_eph->kepler.sqrta);
  *bit += 32;
  rtcm_setbitu(buff, *bit, 14, msg_eph->toe);
  *bit += 14;
  rtcm_setbits(buff, *bit, 16, msg_eph->kepler.cic);
  *bit += 16;
  rtcm_setbits(buff, *bit, 32, msg_eph->kepler.omega0);
  *bit += 32;
  rtcm_setbits(buff, *bit, 16, msg_eph->kepler.cis);
  *bit += 16;
  rtcm_setbits(buff, *bit, 32, msg_eph->kepler.inc);
  *bit += 32;
  rtcm_setbits(buff, *bit, 16, msg_eph->kepler.crc);
  *bit += 16;
  rtcm_setbits(buff, *bit, 32, msg_eph->kepler.w);
  *bit += 32;
  rtcm_setbits(buff, *bit, 24, msg_eph->kepler.omegadot);
  *bit += 24;
}

/** Decode an RTCMv3 GAL (I/NAV message) Ephemeris Message
 *
 * \param buff The input data buffer
 * \param RTCM message struct
 * \return  - RC_OK : Success
 *          - RC_MESSAGE_TYPE_MISMATCH : Message type mismatch
 */
uint16_t rtcm3_encode_gal_eph_inav(const rtcm_msg_eph *msg_eph,
                                   uint8_t buff[]) {
  assert(msg_eph);

  uint16_t bit = 0;
  rtcm_setbitu(buff, bit, 12, 1046);
  bit += 12;

  /* parse common I/NAV and F/NAV part */
  rtcm3_encode_gal_eph_common(msg_eph, buff, &bit);

  rtcm_setbits(buff, bit, 10, msg_eph->kepler.tgd_gal_s[0]);
  bit += 10;
  rtcm_setbits(buff, bit, 10, msg_eph->kepler.tgd_gal_s[1]);
  bit += 10;
  rtcm_setbits(buff, bit, 6, msg_eph->health_bits);
  bit += 6;
  /* reserved */
  rtcm_setbits(buff, bit, 2, 0);
  bit += 2;

  /* Round number of bits up to nearest whole byte. */
  return (bit + 7) / 8;
}

/** Decode an RTCMv3 GAL (F/NAV message) Ephemeris Message
 *
 * \param buff The input data buffer
 * \param RTCM message struct
 * \return  - RC_OK : Success
 *          - RC_MESSAGE_TYPE_MISMATCH : Message type mismatch
 */
uint16_t rtcm3_encode_gal_eph_fnav(const rtcm_msg_eph *msg_eph,
                                   uint8_t buff[]) {
  assert(msg_eph);

  uint16_t bit = 0;
  rtcm_setbitu(buff, bit, 12, 1045);
  bit += 12;

  /* parse common F/NAV and I/NAV part */
  rtcm3_encode_gal_eph_common(msg_eph, buff, &bit);

  rtcm_setbits(buff, bit, 10, msg_eph->kepler.tgd_gal_s[0]);
  bit += 10;
  rtcm_setbits(buff, bit, 3, msg_eph->health_bits);
  bit += 3;
  /* reserved */
  rtcm_setbits(buff, bit, 7, 0);
  bit += 7;

  /* Round number of bits up to nearest whole byte. */
  return (bit + 7) / 8;
}
