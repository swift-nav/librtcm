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
uint16_t rtcm3_encode_gal_eph_inav(const rtcm_msg_eph *msg_eph, uint8_t buff[]) {
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
