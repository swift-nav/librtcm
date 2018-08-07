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

#include "rtcm3_eph_decode.h"
#include "bits.h"

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
  uint16_t msg_num = getbitu(buff, bit, 12);
  if(msg_num != 1019) {
    return RC_MESSAGE_TYPE_MISMATCH;
  }
  bit += 12;
  msg_eph->sat_id = getbitu(buff, bit, 6);
  bit += 6;
  msg_eph->wn = getbitu(buff, bit, 10);
  bit += 10;
  msg_eph->ura = getbitu(buff, bit, 4);
  bit += 4;
  /*uint8_t l2_code = */ getbitu(buff, bit, 2);
  bit += 2;
  msg_eph->kepler.inc_dot = getbits(buff, bit, 14);
  bit += 14;
  msg_eph->kepler.iode = getbitu(buff, bit, 8);
  bit += 8;
  msg_eph->toe = getbitu(buff, bit, 16);
  bit += 16;
  msg_eph->kepler.af2 = getbits(buff, bit, 8);
  bit += 8;
  msg_eph->kepler.af1 = getbits(buff, bit, 16);
  bit += 16;
  msg_eph->kepler.af0 = getbits(buff, bit, 22);
  bit += 22;
  msg_eph->kepler.iodc = getbitu(buff, bit, 10);
  bit += 10;
  msg_eph->kepler.crs = getbits(buff, bit, 16);
  bit += 16;
  msg_eph->kepler.dn = getbits(buff, bit, 16);
  bit += 16;
  msg_eph->kepler.m0 = getbits(buff, bit, 32);
  bit += 32;
  msg_eph->kepler.cuc = getbits(buff, bit, 16);
  bit += 16;
  msg_eph->kepler.ecc = getbitu(buff, bit, 32);
  bit += 32;
  msg_eph->kepler.cus = getbits(buff, bit, 16);
  bit += 16;
  msg_eph->kepler.sqrta = getbitu(buff, bit, 32);
  bit += 32;
  msg_eph->kepler.toc = getbitu(buff, bit, 16);
  bit += 16;
  msg_eph->kepler.cic = getbits(buff, bit, 16);
  bit += 16;
  msg_eph->kepler.omega0 = getbits(buff, bit, 32);
  bit += 32;
  msg_eph->kepler.cis = getbits(buff, bit, 16);
  bit += 16;
  msg_eph->kepler.inc = getbits(buff, bit, 32);
  bit += 32;
  msg_eph->kepler.crc = getbits(buff, bit, 16);
  bit += 16;
  msg_eph->kepler.w = getbits(buff, bit, 32);
  bit += 32;
  msg_eph->kepler.omegadot = getbits(buff, bit, 24);
  bit += 24;
  msg_eph->kepler.tgd_gps_s = getbits(buff, bit, 8);
  bit += 8;
  msg_eph->health_bits = getbitu(buff, bit, 6);
  bit += 6;
  /* L2 data bit */ getbitu(buff, bit, 1);
  bit += 1;
  msg_eph->fit_interval = getbitu(buff, bit, 1);
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
  uint16_t msg_num = getbitu(buff, bit, 12);
  if(msg_num != 1020) {
    return RC_MESSAGE_TYPE_MISMATCH;
  }
  bit += 12;
  msg_eph->sat_id = getbitu(buff, bit, 6);
  bit += 6;
  msg_eph->glo.fcn = getbitu(buff, bit, 5);
  bit += 5;
  /*alm health ind = */ getbitu(buff, bit, 1);
  bit += 1;
  /*alm health ind valid = */ getbitu(buff, bit, 1);
  bit += 1;
  /* P1  = */ getbitu(buff, bit, 2);
  bit += 2;
  /* tk */getbitu(buff, bit, 12);
  bit += 12;
  /* MSB of Bn word */getbitu(buff, bit, 1);
  bit += 1;
  /* P2 */getbitu(buff, bit, 1);
  bit += 1;
  msg_eph->glo.t_b = getbitu(buff, bit, 7);
  bit += 7;
  msg_eph->glo.vel[0] = getbits(buff, bit, 24);
  bit += 24;
  msg_eph->glo.pos[0] = getbits(buff, bit, 27);
  bit += 27;
  msg_eph->glo.acc[0] = getbits(buff, bit, 5);
  bit += 5;
  msg_eph->glo.vel[1] = getbits(buff, bit, 24);
  bit += 24;
  msg_eph->glo.pos[1] = getbits(buff, bit, 27);
  bit += 27;
  msg_eph->glo.acc[1] = getbits(buff, bit, 5);
  bit += 5;
  msg_eph->glo.vel[2] = getbits(buff, bit, 24);
  bit += 24;
  msg_eph->glo.pos[2] = getbits(buff, bit, 27);
  bit += 27;
  msg_eph->glo.acc[2] = getbits(buff, bit, 5);
  bit += 5;
  /* P3 */getbitu(buff, bit, 1);
  bit += 1;
  msg_eph->glo.gamma = getbits(buff, bit, 11);
  bit += 11;
  /* P */getbitu(buff, bit, 2);
  bit += 2;
  /* ln */getbitu(buff, bit, 1);
  bit += 1;
  msg_eph->glo.tau = getbits(buff, bit, 22);
  bit += 22;
  msg_eph->glo.d_tau = getbits(buff, bit, 5);
  bit += 5;
  /* EN */getbitu(buff, bit, 5);
  bit += 5;
  /* P4 */getbitu(buff, bit, 1);
  bit += 1;
  /* FT */getbitu(buff, bit, 4);
  bit += 4;
  /* NT */getbitu(buff, bit, 11);
  bit += 11;
  /* M */getbitu(buff, bit, 2);
  bit += 2;
  /* Avail Add Data */getbitu(buff, bit, 1);
  bit += 1;
  /* NA */getbitu(buff, bit, 11);
  bit += 11;
  /* Tc */getbits(buff, bit, 32);
  bit += 32;
  /* N4 */getbitu(buff, bit, 5);
  bit += 5;
  /* Tgps */getbits(buff, bit, 22);
  bit += 22;
  /* ln */getbitu(buff, bit, 1);
  bit += 1;
  /* reserved */getbitu(buff, bit, 7);
  bit += 7;

  return RC_OK;
}

/** Decode an RTCMv3 GAL Ephemeris Message
 *
 * \param buff The input data buffer
 * \param RTCM message struct
 * \return  - RC_OK : Success
 *          - RC_MESSAGE_TYPE_MISMATCH : Message type mismatch
 *          - RC_INVALID_MESSAGE : Cell mask too large or invalid TOW
 */
rtcm3_rc rtcm3_decode_gal_eph(const uint8_t buff[], rtcm_msg_eph *msg_eph) {
  uint16_t bit = 0;
  uint16_t msg_num = getbitu(buff, bit, 12);
  if(msg_num != 1045) {
    return RC_MESSAGE_TYPE_MISMATCH;
  }
  bit += 12;
  msg_eph->sat_id = getbitu(buff, bit, 6);
  bit += 6;
  msg_eph->wn = getbitu(buff, bit, 13);
  bit += 13;
  msg_eph->ura = getbitu(buff, bit, 4);
  bit += 4;
  msg_eph->kepler.inc_dot = getbits(buff, bit, 14);
  bit += 14;
  msg_eph->kepler.iode = getbitu(buff, bit, 5);
  bit += 5;
  msg_eph->kepler.toc = getbitu(buff, bit, 17);
  bit += 17;
  msg_eph->kepler.af2 = getbits(buff, bit, 11);
  bit += 11;
  msg_eph->kepler.af1 = getbits(buff, bit, 22);
  bit += 22;
  msg_eph->kepler.af2 = getbits(buff, bit, 24);
  bit += 24;
  msg_eph->kepler.iodc = getbitu(buff, bit, 5);
  bit += 5;
  msg_eph->kepler.crs = getbits(buff, bit, 18);
  bit += 18;
  msg_eph->kepler.dn = getbits(buff, bit, 16);
  bit += 16;
  msg_eph->kepler.m0 = getbits(buff, bit, 32);
  bit += 32;
  msg_eph->kepler.cuc = getbits(buff, bit, 18);
  bit += 18;
  msg_eph->kepler.ecc = getbitu(buff, bit, 32);
  bit += 32;
  msg_eph->kepler.cus = getbits(buff, bit, 18);
  bit += 18;
  msg_eph->kepler.sqrta = getbitu(buff, bit, 32);
  bit += 32;
  msg_eph->toe = getbitu(buff, bit, 17);
  bit += 17;
  msg_eph->kepler.cic = getbits(buff, bit, 18);
  bit += 18;
  msg_eph->kepler.omega0 = getbits(buff, bit, 32);
  bit += 32;
  msg_eph->kepler.cis = getbits(buff, bit, 18);
  bit += 18;
  /* i0 */ getbits(buff, bit, 32);
  bit += 32;
  msg_eph->kepler.crc = getbits(buff, bit, 18);
  bit += 18;
  msg_eph->kepler.w = getbits(buff, bit, 32);
  bit += 32;
  msg_eph->kepler.omegadot = getbits(buff, bit, 24);
  bit += 24;
  msg_eph->kepler.tgd_bds_s[0] = getbits(buff, bit, 10);
  bit += 10;
  msg_eph->kepler.tgd_bds_s[1] = getbits(buff, bit, 10);
  bit += 10;
  msg_eph->valid = getbits(buff, bit, 1);
  bit += 1;
  return RC_OK;
}

rtcm3_rc rtcm3_decode_bds_eph(const uint8_t buff[], rtcm_msg_eph *msg_eph) {
  uint16_t bit = 0;
  uint16_t msg_num = getbitu(buff, bit, 12);
  if(msg_num != 1042) {
    return RC_MESSAGE_TYPE_MISMATCH;
  }
  bit += 12;
  msg_eph->sat_id = getbitu(buff, bit, 6);
  bit += 6;
  msg_eph->wn = getbitu(buff, bit, 13);
  bit += 13;
  msg_eph->kepler.iode = getbitu(buff, bit, 10);
  bit += 10;
  /* SISA */ getbitu(buff, bit, 8);
  bit += 8;
  msg_eph->kepler.inc_dot = getbits(buff, bit, 14);
  bit += 14;
  msg_eph->kepler.toc = getbitu(buff, bit, 14);
  bit += 14;
  msg_eph->kepler.af2 = getbits(buff, bit, 6);
  bit += 6;
  msg_eph->kepler.af1 = getbits(buff, bit, 21);
  bit += 21;
  msg_eph->kepler.af0 = getbits(buff, bit, 31);
  bit += 31;
  msg_eph->kepler.crs = getbits(buff, bit, 16);
  bit += 16;
  msg_eph->kepler.dn = getbits(buff, bit, 16);
  bit += 16;
  msg_eph->kepler.m0 = getbits(buff, bit, 32);
  bit += 32;
  msg_eph->kepler.cuc = getbits(buff, bit, 16);
  bit += 16;
  msg_eph->kepler.ecc = getbitu(buff, bit, 32);
  bit += 32;
  msg_eph->kepler.cus = getbits(buff, bit, 16);
  bit += 16;
  msg_eph->kepler.sqrta = getbitu(buff, bit, 32);
  bit += 32;
  msg_eph->toe = getbitu(buff, bit, 14);
  bit += 14;
  msg_eph->kepler.cic = getbits(buff, bit, 16);
  bit += 16;
  msg_eph->kepler.omega0 = getbits(buff, bit, 32);
  bit += 32;
  msg_eph->kepler.cis = getbits(buff, bit, 16);
  bit += 16;
  /* i0 */ getbits(buff, bit, 32);
  bit += 32;
  msg_eph->kepler.crc = getbits(buff, bit, 16);
  bit += 16;
  msg_eph->kepler.w = getbits(buff, bit, 32);
  bit += 32;
  msg_eph->kepler.omegadot = getbits(buff, bit, 24);
  bit += 24;
  msg_eph->kepler.tgd_gal_s[0] = getbits(buff, bit, 10);
  bit += 10;
  msg_eph->kepler.tgd_gal_s[1] = getbits(buff, bit, 10);
  bit += 10;
  msg_eph->health_bits = getbits(buff, bit, 6);
  bit += 6;
  /* reserved */ getbits(buff, bit, 2);
  bit += 2;

  return RC_OK;
}
