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

#include "rtcm_encoder.h"
#include <math.h>
#include "bits.h"

/** Convert a lock time in seconds into a RTCMv3 Lock Time Indicator value.
 * See RTCM 10403.1, Table 3.4-2.
 *
 * \param time Lock time in seconds.
 * \return Lock Time Indicator value.
 */
static uint8_t to_lock_ind(uint32_t time) {
  if (time < 24) return time;
  if (time < 72) return (time + 24) / 2;
  if (time < 168) return (time + 120) / 4;
  if (time < 360) return (time + 408) / 8;
  if (time < 744) return (time + 1176) / 16;
  if (time < 937) return (time + 3096) / 32;
  return 127;
}

/** Convert a lock time in seconds into a 4-bit RTCMv3 Lock Time Indicator value
 * See RTCM 10403.1, Table 3.5-74.
 *
 * \param time Lock time in seconds.
 * \return Lock Time Indicator value.
 */
static uint8_t to_msm_lock_ind(double time) {
  if (time < 0.032) return 0;
  if (time < 0.064) return 1;
  if (time < 0.128) return 2;
  if (time < 0.256) return 3;
  if (time < 0.512) return 4;
  if (time < 1.024) return 5;
  if (time < 2.048) return 6;
  if (time < 4.096) return 7;
  if (time < 8.192) return 8;
  if (time < 16.384) return 9;
  if (time < 32.768) return 10;
  if (time < 65.536) return 11;
  if (time < 131.072) return 12;
  if (time < 262.144) return 13;
  if (time < 524.288) return 14;
  return 15;
}

void encode_basic_freq_data(const rtcm_freq_data *freq_data,
                            const double freq,
                            const double *l1_pr,
                            uint8_t buff[],
                            uint16_t *bit) {
  /* Calculate GPS Integer L1 Pseudorange Modulus Ambiguity (DF014). */
  uint8_t amb = (uint8_t)(*l1_pr / PRUNIT_GPS);

  /* Construct L1 pseudorange value as it would be transmitted (DF011). */
  uint32_t calc_l1_pr =
      (uint32_t)roundl((double)(*l1_pr - amb * PRUNIT_GPS) / 0.02);

  /* Calculate GPS Pseudorange (DF011/DF016). */
  uint32_t pr =
      (uint32_t)roundl((freq_data->pseudorange - amb * PRUNIT_GPS) / 0.02);

  double l1_prc = calc_l1_pr * 0.02 + amb * PRUNIT_GPS;

  /* phaserange - L1 pseudorange */
  double cp_pr = freq_data->carrier_phase - l1_prc / (CLIGHT / freq);

  /* TODO (anthony) If the pr and cp diverge, we should adjust the cp ambiguity
   * and reset the lock time indicator */

  /* Calculate PhaseRange – L1 Pseudorange (DF012/DF018). */
  int32_t ppr = roundl(cp_pr * (CLIGHT / freq) / 0.0005);

  if (fabs(freq - GPS_L1_FREQ) < 0.01) {
    setbitu(buff, *bit, 1, 0);
    *bit += 1;
    setbitu(buff, *bit, 24, pr);
    *bit += 24;
  } else {
    setbitu(buff, *bit, 2, 0);
    *bit += 2;
    setbits(buff, *bit, 14, (int32_t)pr - (int32_t)calc_l1_pr);
    *bit += 14;
  }
  setbits(buff, *bit, 20, ppr);
  *bit += 20;
  setbitu(buff,
          *bit,
          7,
          freq_data->flags.valid_lock ? to_lock_ind(freq_data->lock) : 0);
  *bit += 7;
}

void encode_basic_glo_freq_data(const rtcm_freq_data *freq_data,
                                const double freq,
                                const double *l1_pr,
                                const uint8_t fcn,
                                uint8_t buff[],
                                uint16_t *bit) {
  bool L1 = fabs(freq - GLO_L1_FREQ) < 0.01;
  /* Calculate GPS Integer L1 Pseudorange Modulus Ambiguity (DF044). */
  uint8_t amb = (uint8_t)(*l1_pr / PRUNIT_GLO);

  /* Construct L1 pseudorange value as it would be transmitted (DF041). */
  uint32_t calc_l1_pr =
      (uint32_t)roundl((double)(*l1_pr - amb * PRUNIT_GLO) / 0.02);

  /* Calculate GLO Pseudorange (DF041/DF046). */
  uint32_t pr =
      (uint32_t)roundl((freq_data->pseudorange - amb * PRUNIT_GLO) / 0.02);

  double l1_prc = calc_l1_pr * 0.02 + amb * PRUNIT_GLO;

  double glo_freq = 0.0;
  if (L1) {
    glo_freq = freq + (fcn - 7) * GLO_L1_CH_OFFSET;
  } else {
    glo_freq = freq + (fcn - 7) * GLO_L2_CH_OFFSET;
  }
  /* phaserange - L1 pseudorange */
  double cp_pr = freq_data->carrier_phase - l1_prc / (CLIGHT / glo_freq);

  /* Calculate PhaseRange – L1 Pseudorange (DF042/DF048). */
  int32_t ppr = roundl(cp_pr * (CLIGHT / glo_freq) / 0.0005);

  if (L1) {
    setbitu(buff, *bit, 1, 0);
    *bit += 1;
    setbitu(buff, *bit, 5, fcn);
    *bit += 5;
    setbitu(buff, *bit, 25, pr);
    *bit += 25;
  } else {
    setbitu(buff, *bit, 2, 0);
    *bit += 2;
    setbits(buff, *bit, 14, (int32_t)pr - (int32_t)calc_l1_pr);
    *bit += 14;
  }
  setbits(buff, *bit, 20, ppr);
  *bit += 20;
  setbitu(buff,
          *bit,
          7,
          freq_data->flags.valid_lock ? to_lock_ind(freq_data->lock) : 0);
  *bit += 7;
}

/** Write RTCM header for observation message types 1001..1004.
 *
 * The data message header will be written starting from byte zero of the
 * buffer. If the buffer also contains a frame header then be sure to pass a
 * pointer to the start of the data message rather than a pointer to the start
 * of the frame buffer. The RTCM observation header is 8 bytes (64 bits) long.
 *
 * If the Synchronous GNSS Message Flag is set to `0`, it means that no further
 * GNSS observables referenced to the same Epoch Time will be transmitted. This
 * enables the receiver to begin processing the data immediately after decoding
 * the message. If it is set to `1`, it means that the next message will
 * contain observables of another GNSS source referenced to the same Epoch
 * Time.
 *
 * Divergence-free Smoothing Indicator values:
 *
 * Indicator | Meaning
 * --------- | ----------------------------------
 *     0     | Divergence-free smoothing not used
 *     1     | Divergence-free smoothing used
 *
 * GPS Smoothing Interval indicator values are listed in RTCM 10403.1 Table
 * 3.4-4, reproduced here:
 *
 * Indicator | Smoothing Interval
 * --------- | ------------------
 *  000 (0)  |   No smoothing
 *  001 (1)  |   < 30 s
 *  010 (2)  |   30-60 s
 *  011 (3)  |   1-2 min
 *  100 (4)  |   2-4 min
 *  101 (5)  |   4-8 min
 *  110 (6)  |   >8 min
 *  111 (7)  |   Unlimited
 *
 * \param header pointer to the obs header to encode
 * \param num_sats number of satellites in message
 * \param buff A pointer to the RTCM data message buffer.
 */
uint16_t rtcm3_write_header(const rtcm_obs_header *header,
                            uint8_t num_sats,
                            uint8_t buff[]) {
  uint16_t bit = 0;
  setbitu(buff, bit, 12, header->msg_num);
  bit += 12;
  setbitu(buff, bit, 12, header->stn_id);
  bit += 12;
  setbitu(buff, bit, 30, (uint32_t)round(header->tow_ms));
  bit += 30;
  setbitu(buff, bit, 1, header->sync);
  bit += 1;
  setbitu(buff, bit, 5, num_sats);
  bit += 5;
  setbitu(buff, bit, 1, header->div_free);
  bit += 1;
  setbitu(buff, bit, 3, header->smooth);
  bit += 3;
  return bit;
}

/** Write RTCM header for observation message types 1009..1012.
 *
 * The data message header will be written starting from byte zero of the
 * buffer. If the buffer also contains a frame header then be sure to pass a
 * pointer to the start of the data message rather than a pointer to the start
 * of the frame buffer. The RTCM observation header is 8 bytes (61 bits) long.
 *
 * If the Synchronous GNSS Message Flag is set to `0`, it means that no further
 * GNSS observables referenced to the same Epoch Time will be transmitted. This
 * enables the receiver to begin processing the data immediately after decoding
 * the message. If it is set to `1`, it means that the next message will
 * contain observables of another GNSS source referenced to the same Epoch
 * Time.
 *
 * Divergence-free Smoothing Indicator values:
 *
 * Indicator | Meaning
 * --------- | ----------------------------------
 *     0     | Divergence-free smoothing not used
 *     1     | Divergence-free smoothing used
 *
 * GLO Smoothing Interval indicator values are listed in RTCM 10403.1 Table
 * 3.4-4, reproduced here:
 *
 * Indicator | Smoothing Interval
 * --------- | ------------------
 *  000 (0)  |   No smoothing
 *  001 (1)  |   < 30 s
 *  010 (2)  |   30-60 s
 *  011 (3)  |   1-2 min
 *  100 (4)  |   2-4 min
 *  101 (5)  |   4-8 min
 *  110 (6)  |   >8 min
 *  111 (7)  |   Unlimited
 *
 * \param header pointer to the obs header to encode
 * \param num_sats number of satellites in message
 * \param buff A pointer to the RTCM data message buffer.
 */
uint16_t rtcm3_write_glo_header(const rtcm_obs_header *header,
                                uint8_t num_sats,
                                uint8_t buff[]) {
  uint16_t bit = 0;
  setbitu(buff, bit, 12, header->msg_num);
  bit += 12;
  setbitu(buff, bit, 12, header->stn_id);
  bit += 12;
  setbitu(buff, bit, 27, (uint32_t)round(header->tow_ms));
  bit += 27;
  setbitu(buff, bit, 1, header->sync);
  bit += 1;
  setbitu(buff, bit, 5, num_sats);
  bit += 5;
  setbitu(buff, bit, 1, header->div_free);
  bit += 1;
  setbitu(buff, bit, 3, header->smooth);
  bit += 3;
  return bit;
}

uint16_t rtcm3_encode_1001(const rtcm_obs_message *msg_1001, uint8_t buff[]) {
  uint16_t bit = 64; /* Start at end of header. */

  uint8_t num_sats = 0;
  for (uint8_t i = 0; i < msg_1001->header.n_sat; i++) {
    if (msg_1001->sats[i].obs[L1_FREQ].flags.valid_pr &&
        msg_1001->sats[i].obs[L1_FREQ].flags.valid_cp) {
      setbitu(buff, bit, 6, msg_1001->sats[i].svId);
      bit += 6;
      encode_basic_freq_data(&msg_1001->sats[i].obs[L1_FREQ],
                             GPS_L1_FREQ,
                             &msg_1001->sats[i].obs[L1_FREQ].pseudorange,
                             buff,
                             &bit);
      ++num_sats;
    }
  }

  rtcm3_write_header(&msg_1001->header, num_sats, buff);

  /* Round number of bits up to nearest whole byte. */
  return (bit + 7) / 8;
}

/** Encode an RTCMv3 message type 1002 (Extended L1-Only GPS RTK Observables)
 * Message type 1002 has length `64 + n_sat*74` bits. Returned message length
 * is rounded up to the nearest whole byte.
 *
 * \param buff A pointer to the RTCM data message buffer.
 * \param id Reference station ID (DF003).
 * \param t GPS time of epoch (DF004).
 * \param n_sat Number of GPS satellites included in the message (DF006).
 * \param nm Struct containing the observation.
 * \param sync Synchronous GNSS Flag (DF005).
 * \return The message length in bytes.
 */
uint16_t rtcm3_encode_1002(const rtcm_obs_message *msg_1002, uint8_t buff[]) {
  uint16_t bit = 64; /* Start at end of header. */

  uint8_t num_sats = 0;
  for (uint8_t i = 0; i < msg_1002->header.n_sat; i++) {
    if (msg_1002->sats[i].obs[L1_FREQ].flags.valid_pr &&
        msg_1002->sats[i].obs[L1_FREQ].flags.valid_cp) {
      setbitu(buff, bit, 6, msg_1002->sats[i].svId);
      bit += 6;
      encode_basic_freq_data(&msg_1002->sats[i].obs[L1_FREQ],
                             GPS_L1_FREQ,
                             &msg_1002->sats[i].obs[L1_FREQ].pseudorange,
                             buff,
                             &bit);

      /* Calculate GPS Integer L1 Pseudorange Modulus Ambiguity (DF014). */
      uint8_t amb =
          (uint8_t)(msg_1002->sats[i].obs[L1_FREQ].pseudorange / PRUNIT_GPS);

      setbitu(buff, bit, 8, amb);
      bit += 8;
      setbitu(buff,
              bit,
              8,
              (uint8_t)roundl(msg_1002->sats[i].obs[L1_FREQ].cnr * 4.0));
      bit += 8;
      ++num_sats;
    }
  }

  rtcm3_write_header(&msg_1002->header, num_sats, buff);

  /* Round number of bits up to nearest whole byte. */
  return (bit + 7) / 8;
}

uint16_t rtcm3_encode_1003(const rtcm_obs_message *msg_1003, uint8_t buff[]) {
  uint16_t bit = 64; /* Start at end of header. */

  uint8_t num_sats = 0;
  for (uint8_t i = 0; i < msg_1003->header.n_sat; i++) {
    flag_bf l1_flags = msg_1003->sats[i].obs[L1_FREQ].flags;
    flag_bf l2_flags = msg_1003->sats[i].obs[L2_FREQ].flags;
    if (l1_flags.valid_pr && l1_flags.valid_cp && l2_flags.valid_pr &&
        l2_flags.valid_cp) {
      setbitu(buff, bit, 6, msg_1003->sats[i].svId);
      bit += 6;
      encode_basic_freq_data(&msg_1003->sats[i].obs[L1_FREQ],
                             GPS_L1_FREQ,
                             &msg_1003->sats[i].obs[L1_FREQ].pseudorange,
                             buff,
                             &bit);
      encode_basic_freq_data(&msg_1003->sats[i].obs[L2_FREQ],
                             GPS_L2_FREQ,
                             &msg_1003->sats[i].obs[L1_FREQ].pseudorange,
                             buff,
                             &bit);
      ++num_sats;
    }
  }

  rtcm3_write_header(&msg_1003->header, num_sats, buff);

  /* Round number of bits up to nearest whole byte. */
  return (bit + 7) / 8;
}

uint16_t rtcm3_encode_1004(const rtcm_obs_message *msg_1004, uint8_t buff[]) {
  uint16_t bit = 64; /* Start at end of header. */

  uint8_t num_sats = 0;
  for (uint8_t i = 0; i < msg_1004->header.n_sat; i++) {
    flag_bf l1_flags = msg_1004->sats[i].obs[L1_FREQ].flags;
    flag_bf l2_flags = msg_1004->sats[i].obs[L2_FREQ].flags;
    if (l1_flags.valid_pr && l1_flags.valid_cp && l2_flags.valid_pr &&
        l2_flags.valid_cp) {
      setbitu(buff, bit, 6, msg_1004->sats[i].svId);
      bit += 6;
      encode_basic_freq_data(&msg_1004->sats[i].obs[L1_FREQ],
                             GPS_L1_FREQ,
                             &msg_1004->sats[i].obs[L1_FREQ].pseudorange,
                             buff,
                             &bit);

      /* Calculate GPS Integer L1 Pseudorange Modulus Ambiguity (DF014). */
      uint8_t amb =
          (uint8_t)(msg_1004->sats[i].obs[L1_FREQ].pseudorange / PRUNIT_GPS);

      setbitu(buff, bit, 8, amb);
      bit += 8;
      setbitu(buff,
              bit,
              8,
              (uint8_t)roundl(msg_1004->sats[i].obs[L1_FREQ].cnr * 4.0));
      bit += 8;

      encode_basic_freq_data(&msg_1004->sats[i].obs[L2_FREQ],
                             GPS_L2_FREQ,
                             &msg_1004->sats[i].obs[L1_FREQ].pseudorange,
                             buff,
                             &bit);
      setbitu(buff,
              bit,
              8,
              (uint8_t)roundl(msg_1004->sats[i].obs[L2_FREQ].cnr * 4.0));
      bit += 8;
      ++num_sats;
    }
  }

  rtcm3_write_header(&msg_1004->header, num_sats, buff);

  /* Round number of bits up to nearest whole byte. */
  return (bit + 7) / 8;
}

uint16_t rtcm3_encode_1005_base(const rtcm_msg_1005 *msg_1005,
                                uint8_t buff[],
                                uint16_t *bit) {
  setbitu(buff, *bit, 12, msg_1005->stn_id);
  *bit += 12;
  setbitu(buff, *bit, 6, msg_1005->ITRF);
  *bit += 6;
  setbitu(buff, *bit, 1, msg_1005->GPS_ind);
  *bit += 1;
  setbitu(buff, *bit, 1, msg_1005->GLO_ind);
  *bit += 1;
  setbitu(buff, *bit, 1, msg_1005->GAL_ind);
  *bit += 1;
  setbitu(buff, *bit, 1, msg_1005->ref_stn_ind);
  *bit += 1;
  setbitsl(buff, *bit, 38, (int64_t)roundl(msg_1005->arp_x * 10000.0));
  *bit += 38;
  setbitu(buff, *bit, 1, msg_1005->osc_ind);
  *bit += 1;
  setbitu(buff, *bit, 1, 0);
  *bit += 1;
  setbitsl(buff, *bit, 38, (int64_t)roundl(msg_1005->arp_y * 10000.0));
  *bit += 38;
  setbitu(buff, *bit, 2, msg_1005->quart_cycle_ind);
  *bit += 2;
  setbitsl(buff, *bit, 38, (int64_t)roundl(msg_1005->arp_z * 10000.0));
  *bit += 38;

  /* Round number of bits up to nearest whole byte. */
  return (*bit + 7) / 8;
}

uint16_t rtcm3_encode_1005(const rtcm_msg_1005 *msg_1005, uint8_t buff[]) {
  uint16_t bit = 0;
  setbitu(buff, bit, 12, 1005);
  bit += 12;
  return rtcm3_encode_1005_base(msg_1005, buff, &bit);
}

uint16_t rtcm3_encode_1006(const rtcm_msg_1006 *msg_1006, uint8_t buff[]) {
  uint16_t bit = 0;
  setbitu(buff, bit, 12, 1006);
  bit += 12;
  rtcm3_encode_1005_base(&msg_1006->msg_1005, buff, &bit);
  setbitu(buff, bit, 16, (uint16_t)roundl(msg_1006->ant_height * 10000.0));
  bit += 16;

  /* Round number of bits up to nearest whole byte. */
  return (bit + 7) / 8;
}

uint16_t rtcm3_encode_1007_base(const rtcm_msg_1007 *msg_1007,
                                uint8_t buff[],
                                uint16_t *bit) {
  setbitu(buff, *bit, 12, msg_1007->stn_id);
  *bit += 12;
  setbitu(buff, *bit, 8, msg_1007->desc_count);
  *bit += 8;
  for (uint8_t i = 0; i < msg_1007->desc_count; ++i) {
    setbitu(buff, *bit, 8, msg_1007->desc[i]);
    *bit += 8;
  }
  setbitu(buff, *bit, 8, msg_1007->ant_id);
  *bit += 8;

  /* Round number of bits up to nearest whole byte. */
  return (*bit + 7) / 8;
}

uint16_t rtcm3_encode_1007(const rtcm_msg_1007 *msg_1007, uint8_t buff[]) {
  uint16_t bit = 0;
  setbitu(buff, bit, 12, 1007);
  bit += 12;
  return rtcm3_encode_1007_base(msg_1007, buff, &bit);
}

uint16_t rtcm3_encode_1008(const rtcm_msg_1008 *msg_1008, uint8_t buff[]) {
  uint16_t bit = 0;
  setbitu(buff, bit, 12, 1008);
  bit += 12;
  rtcm3_encode_1007_base(&msg_1008->msg_1007, buff, &bit);
  setbitu(buff, bit, 8, msg_1008->serial_count);
  bit += 8;
  for (uint8_t i = 0; i < msg_1008->serial_count; ++i) {
    setbitu(buff, bit, 8, msg_1008->serial_num[i]);
    bit += 8;
  }

  /* Round number of bits up to nearest whole byte. */
  return (bit + 7) / 8;
}

uint16_t rtcm3_encode_1010(const rtcm_obs_message *msg_1010, uint8_t buff[]) {
  uint16_t bit = 61; /* Start at end of header. */

  uint8_t num_sats = 0;
  for (uint8_t i = 0; i < msg_1010->header.n_sat; i++) {
    if (msg_1010->sats[i].obs[L1_FREQ].flags.valid_pr &&
        msg_1010->sats[i].obs[L1_FREQ].flags.valid_cp) {
      const rtcm_sat_data *sat_obs = &msg_1010->sats[i];
      setbitu(buff, bit, 6, sat_obs->svId);
      bit += 6;
      encode_basic_glo_freq_data(&sat_obs->obs[L1_FREQ],
                                 GLO_L1_FREQ,
                                 &sat_obs->obs[L1_FREQ].pseudorange,
                                 sat_obs->fcn,
                                 buff,
                                 &bit);

      /* Calculate GPS Integer L1 Pseudorange Modulus Ambiguity (DF014). */
      uint8_t amb = (uint8_t)(sat_obs->obs[L1_FREQ].pseudorange / PRUNIT_GLO);

      setbitu(buff, bit, 7, amb);
      bit += 7;
      setbitu(buff, bit, 8, (uint8_t)roundl(sat_obs->obs[L1_FREQ].cnr * 4.0));
      bit += 8;
      ++num_sats;
    }
  }

  rtcm3_write_glo_header(&msg_1010->header, num_sats, buff);

  /* Round number of bits up to nearest whole byte. */
  return (bit + 7) / 8;
}

uint16_t rtcm3_encode_1012(const rtcm_obs_message *msg_1012, uint8_t buff[]) {
  uint16_t bit = 61; /* Start at end of header. */

  uint8_t num_sats = 0;
  for (uint8_t i = 0; i < msg_1012->header.n_sat; i++) {
    flag_bf l1_flags = msg_1012->sats[i].obs[L1_FREQ].flags;
    flag_bf l2_flags = msg_1012->sats[i].obs[L2_FREQ].flags;
    if (l1_flags.valid_pr && l1_flags.valid_cp && l2_flags.valid_pr &&
        l2_flags.valid_cp) {
      const rtcm_sat_data *sat_obs = &msg_1012->sats[i];
      setbitu(buff, bit, 6, sat_obs->svId);
      bit += 6;
      encode_basic_glo_freq_data(&sat_obs->obs[L1_FREQ],
                                 GLO_L1_FREQ,
                                 &sat_obs->obs[L1_FREQ].pseudorange,
                                 sat_obs->fcn,
                                 buff,
                                 &bit);

      /* Calculate GLO Integer L1 Pseudorange Modulus Ambiguity (DF014). */
      uint8_t amb = (uint8_t)(sat_obs->obs[L1_FREQ].pseudorange / PRUNIT_GLO);

      setbitu(buff, bit, 7, amb);
      bit += 7;
      setbitu(buff, bit, 8, (uint8_t)roundl(sat_obs->obs[L1_FREQ].cnr * 4.0));
      bit += 8;

      encode_basic_glo_freq_data(&sat_obs->obs[L2_FREQ],
                                 GLO_L2_FREQ,
                                 &sat_obs->obs[L1_FREQ].pseudorange,
                                 sat_obs->fcn,
                                 buff,
                                 &bit);
      setbitu(buff, bit, 8, (uint8_t)roundl(sat_obs->obs[L2_FREQ].cnr * 4.0));
      bit += 8;
      ++num_sats;
    }
  }

  rtcm3_write_glo_header(&msg_1012->header, num_sats, buff);

  /* Round number of bits up to nearest whole byte. */
  return (bit + 7) / 8;
}

uint16_t rtcm3_encode_1029(const rtcm_msg_1029 *msg_1029, uint8_t buff[]) {
  uint16_t bit = 0, byte = 0;

  setbitu(buff, bit, 12, 1029);
  bit += 12;

  setbitu(buff, bit, 12, msg_1029->stn_id);
  bit += 12;

  setbitu(buff, bit, 16, msg_1029->mjd_num);
  bit += 16;

  setbitu(buff, bit, 17, msg_1029->utc_sec_of_day);
  bit += 17;

  setbitu(buff, bit, 7, msg_1029->unicode_chars);
  bit += 7;

  byte = bit / 8;

  buff[byte++] = msg_1029->utf8_code_units_n;
  for (uint8_t i = 0; i < msg_1029->utf8_code_units_n; i++) {
    buff[byte++] = msg_1029->utf8_code_units[i];
  }

  /* Round number of bits up to nearest whole byte. */
  return (bit + 7) / 8;
}

uint16_t rtcm3_encode_1033(const rtcm_msg_1033 *msg_1033, uint8_t buff[]) {
  uint16_t bit = 0;
  setbitu(buff, bit, 12, 1033);
  bit += 12;

  setbitu(buff, bit, 12, msg_1033->stn_id);
  bit += 12;

  setbitu(buff, bit, 8, msg_1033->antenna_desc_counter);
  bit += 8;
  for (uint8_t i = 0; i < msg_1033->antenna_desc_counter; ++i) {
    setbitu(buff, bit, 8, msg_1033->antenna_descriptor[i]);
    bit += 8;
  }

  setbits(buff, bit, 8, msg_1033->antenna_setup_ID);
  bit += 8;

  setbitu(buff, bit, 8, msg_1033->antenna_serial_num_counter);
  bit += 8;
  for (uint8_t i = 0; i < msg_1033->antenna_serial_num_counter; ++i) {
    setbitu(buff, bit, 8, msg_1033->antenna_serial_num[i]);
    bit += 8;
  }

  setbitu(buff, bit, 8, msg_1033->rcv_descriptor_counter);
  bit += 8;
  for (uint8_t i = 0; i < msg_1033->rcv_descriptor_counter; ++i) {
    setbitu(buff, bit, 8, msg_1033->rcv_descriptor[i]);
    bit += 8;
  }

  setbitu(buff, bit, 8, msg_1033->rcv_fw_counter);
  bit += 8;
  for (uint8_t i = 0; i < msg_1033->rcv_fw_counter; ++i) {
    setbitu(buff, bit, 8, msg_1033->rcv_fw_version[i]);
    bit += 8;
  }

  setbitu(buff, bit, 8, msg_1033->rcv_serial_num_counter);
  bit += 8;
  for (uint8_t i = 0; i < msg_1033->rcv_serial_num_counter; ++i) {
    setbitu(buff, bit, 8, msg_1033->rcv_serial_num[i]);
    bit += 8;
  }
  /* Round number of bits up to nearest whole byte. */
  return (bit + 7) / 8;
}

uint16_t rtcm3_encode_1230(const rtcm_msg_1230 *msg_1230, uint8_t buff[]) {
  uint16_t bit = 0;
  setbitu(buff, bit, 12, 1230);
  bit += 12;
  setbitu(buff, bit, 12, msg_1230->stn_id);
  bit += 12;
  setbitu(buff, bit, 8, msg_1230->bias_indicator);
  bit += 1;
  /* 3 reserved bits */
  setbitu(buff, bit, 3, 0);
  bit += 3;
  setbitu(buff, bit, 4, msg_1230->fdma_signal_mask);
  bit += 4;
  if (msg_1230->fdma_signal_mask & 0x08) {
    int16_t bias = round(msg_1230->L1_CA_cpb_meter * 50);
    setbits(buff, bit, 16, bias);
    bit += 16;
  }
  if (msg_1230->fdma_signal_mask & 0x04) {
    int16_t bias = round(msg_1230->L1_P_cpb_meter * 50);
    setbits(buff, bit, 16, bias);
    bit += 16;
  }
  if (msg_1230->fdma_signal_mask & 0x02) {
    int16_t bias = round(msg_1230->L2_CA_cpb_meter * 50);
    setbits(buff, bit, 16, bias);
    bit += 16;
  }
  if (msg_1230->fdma_signal_mask & 0x01) {
    int16_t bias = round(msg_1230->L2_P_cpb_meter * 50);
    setbits(buff, bit, 16, bias);
    bit += 16;
  }

  /* Round number of bits up to nearest whole byte. */
  return (bit + 7) / 8;
}

static uint16_t rtcm3_encode_msm_header(const rtcm_msm_header *header,
                                        uint8_t buff[]) {
  uint16_t bit = 0;
  setbitu(buff, bit, 12, header->msg_num);
  bit += 12;
  setbitu(buff, bit, 12, header->stn_id);
  bit += 12;
  setbitu(buff, bit, 30, header->tow_ms);
  bit += 30;
  setbitu(buff, bit, 1, header->multiple);
  bit += 1;
  setbitu(buff, bit, 3, header->iods);
  bit += 3;
  setbitu(buff, bit, 7, header->reserved);
  bit += 7;
  setbitu(buff, bit, 2, header->steering);
  bit += 2;
  setbitu(buff, bit, 2, header->ext_clock);
  bit += 2;
  setbitu(buff, bit, 1, header->div_free);
  bit += 1;
  setbitu(buff, bit, 3, header->smooth);
  bit += 3;

  for (uint8_t i = 0; i < MSM_SATELLITE_MASK_SIZE; i++) {
    setbitu(buff, bit, 1, header->satellite_mask[i]);
    bit++;
  }
  for (uint8_t i = 0; i < MSM_SIGNAL_MASK_SIZE; i++) {
    setbitu(buff, bit, 1, header->signal_mask[i]);
    bit++;
  }
  uint8_t num_sats =
      count_mask_bits(MSM_SATELLITE_MASK_SIZE, header->satellite_mask);
  uint8_t num_sigs = count_mask_bits(MSM_SIGNAL_MASK_SIZE, header->signal_mask);
  uint8_t cell_mask_size = num_sats * num_sigs;

  for (uint8_t i = 0; i < cell_mask_size; i++) {
    setbitu(buff, bit, 1, header->cell_mask[i]);
    bit++;
  }

  return bit;
}

static void encode_msm_fine_pseudoranges(const uint8_t num_cells,
                                         const double fine_pr[],
                                         const flag_bf flags[],
                                         uint8_t buff[],
                                         uint16_t *bit) {
  /* DF400 */
  for (uint16_t i = 0; i < num_cells; i++) {
    if (flags[i].valid_pr) {
      double fine_pr_ms = fine_pr[i] / PRUNIT_GPS;
      setbits(buff, *bit, 15, (int16_t)(fine_pr_ms / C_1_2P24));
    } else {
      setbits(buff, *bit, 15, MSM_PR_INVALID);
    }
    *bit += 15;
  }
}

static void encode_msm_fine_phaseranges(const uint8_t num_cells,
                                        const double fine_cp[],
                                        const flag_bf flags[],
                                        uint8_t buff[],
                                        uint16_t *bit) {
  /* DF401 */
  for (uint16_t i = 0; i < num_cells; i++) {
    if (flags[i].valid_cp) {
      double fine_carrier_ms = fine_cp[i] / PRUNIT_GPS;
      setbits(buff, *bit, 22, (int32_t)(fine_carrier_ms / C_1_2P29));
    } else {
      setbits(buff, *bit, 22, (int32_t)MSM_CP_INVALID);
    }
    *bit += 22;
  }
}

static void encode_msm_lock_times(const uint8_t num_cells,
                                  const double lock_time[],
                                  const flag_bf flags[],
                                  uint8_t buff[],
                                  uint16_t *bit) {
  /* DF402 */
  for (uint16_t i = 0; i < num_cells; i++) {
    if (flags[i].valid_lock) {
      setbitu(buff, *bit, 4, to_msm_lock_ind(lock_time[i]));
    } else {
      setbitu(buff, *bit, 4, 0);
    }
    *bit += 4;
  }
}

static void encode_msm_hca_indicators(const uint8_t num_cells,
                                      const bool hca_indicator[],
                                      uint8_t buff[],
                                      uint16_t *bit) {
  /* DF420 */
  for (uint16_t i = 0; i < num_cells; i++) {
    setbitu(buff, *bit, 1, hca_indicator[i]);
    *bit += 1;
  }
}

static void encode_msm_cnrs(const uint8_t num_cells,
                            const double cnr[],
                            const flag_bf flags[],
                            uint8_t buff[],
                            uint16_t *bit) {
  /* DF403 */
  for (uint16_t i = 0; i < num_cells; i++) {
    if (flags[i].valid_lock) {
      setbitu(buff, *bit, 6, (uint8_t)cnr[i]);
    } else {
      setbitu(buff, *bit, 6, 0);
    }
    *bit += 6;
  }
}

static void encode_msm_fine_phaserangerates(const uint8_t num_cells,
                                            const double fine_dop[],
                                            const flag_bf flags[],
                                            uint8_t buff[],
                                            uint16_t *bit) {
  /* DF404 */
  for (uint16_t i = 0; i < num_cells; i++) {
    if (flags[i].valid_dop) {
      double fine_range_rate = fine_dop[i] / 0.0001;
      setbits(buff, *bit, 15, (int16_t)(fine_range_rate));
    } else {
      setbits(buff, *bit, 15, (int32_t)MSM_DOP_INVALID);
    }
    *bit += 15;
  }
}

/** Basic GPS MSM4/5 encoder
 *
 * \param msg The input RTCM message struct
 * \param buff Data buffer large enough to hold the message (at worst 742 bytes)
 *             (see RTCM 10403.3 Table 3.5-71)
 * \return Number of bytes written
 */

uint16_t rtcm3_encode_msm(const rtcm_msm_message *msg, uint8_t buff[]) {
  const rtcm_msm_header *header = &msg->header;

  msm_enum msm_type = to_msm_type(header->msg_num);
  if (MSM4 != msm_type && MSM5 != msm_type) {
    return 0;
  }

  constellation_t cons = to_constellation(msg->header.msg_num);
  if (CONSTELLATION_GPS != cons) {
    /* Unexpected message type. */
    return 0;
  }

  uint8_t num_sats =
      count_mask_bits(MSM_SATELLITE_MASK_SIZE, header->satellite_mask);
  uint8_t num_sigs = count_mask_bits(MSM_SIGNAL_MASK_SIZE, header->signal_mask);
  uint8_t cell_mask_size = num_sats * num_sigs;
  uint8_t num_cells = count_mask_bits(cell_mask_size, header->cell_mask);

  /* Header */
  uint16_t bit = rtcm3_encode_msm_header(header, buff);

  /* Satellite Data */

  uint8_t integer_ms[num_sats];
  double rough_range_m[num_sats];
  double rough_rate_m_s[num_sats];

  /* number of integer milliseconds, DF397 */
  for (uint8_t i = 0; i < num_sats; i++) {
    integer_ms[i] = (uint8_t)(msg->sats[i].rough_range_m / PRUNIT_GPS);
    setbitu(buff, bit, 8, integer_ms[i]);
    bit += 8;
  }

  if (MSM5 == msm_type || MSM7 == msm_type) {
    for (uint8_t i = 0; i < num_sats; i++) {
      setbitu(buff, bit, 4, msg->sats[i].sat_info);
      bit += 4;
    }
  }

  /* rough range modulo 1 ms, DF398 */
  for (uint8_t i = 0; i < num_sats; i++) {
    double pr = msg->sats[i].rough_range_m / PRUNIT_GPS;
    /* remove integer ms part */
    double range_modulo_ms = pr - integer_ms[i];
    uint16_t range_modulo_encoded = (uint16_t)round(1024 * range_modulo_ms);
    setbitu(buff, bit, 10, range_modulo_encoded);
    bit += 10;

    rough_range_m[i] =
        PRUNIT_GPS * (integer_ms[i] + (double)range_modulo_encoded / 1024);
  }

  if (MSM5 == msm_type) {
    for (uint8_t i = 0; i < num_sats; i++) {
      /* range rate, m/s, DF399*/
      double range_rate = round(msg->sats[i].rough_range_rate_m_s);
      setbits(buff, bit, 14, (int16_t)range_rate);
      bit += 14;

      rough_rate_m_s[i] = range_rate;
    }
  }

  /* Signal Data */

  double fine_pr[num_cells];
  double fine_cp[num_cells];
  double lock_time[num_cells];
  bool hca_indicator[num_cells];
  double cnr[num_cells];
  double fine_dop[num_cells];
  flag_bf flags[num_cells];

  uint8_t i = 0;
  for (uint8_t sat = 0; sat < num_sats; sat++) {
    for (uint8_t sig = 0; sig < num_sigs; sig++) {
      if (header->cell_mask[sat * num_sigs + sig]) {
        double freq = msm_signal_frequency(
            cons, sig, msg->header.signal_mask, msg->sats[sat].sat_info);

        flags[i] = msg->signals[i].flags;
        if (flags[i].valid_pr) {
          fine_pr[i] = msg->signals[i].pseudorange_m - rough_range_m[sat];
        }
        if (flags[i].valid_cp) {
          fine_cp[i] = msg->signals[i].carrier_phase_cyc * (CLIGHT / freq) -
                       rough_range_m[sat];
        }
        if (flags[i].valid_lock) {
          lock_time[i] = msg->signals[i].lock_time_s;
        }
        hca_indicator[i] = msg->signals[i].hca_indicator;
        if (flags[i].valid_cnr) {
          cnr[i] = msg->signals[i].cnr;
        } else {
          cnr[i] = 0;
        }
        if (MSM5 == msm_type) {
          fine_dop[i] = msg->signals[i].range_rate_Hz * (CLIGHT / freq) -
                        rough_rate_m_s[sat];
        }
        i++;
      }
    }
  }

  encode_msm_fine_pseudoranges(num_cells, fine_pr, flags, buff, &bit);
  encode_msm_fine_phaseranges(num_cells, fine_cp, flags, buff, &bit);
  encode_msm_lock_times(num_cells, lock_time, flags, buff, &bit);
  encode_msm_hca_indicators(num_cells, hca_indicator, buff, &bit);
  encode_msm_cnrs(num_cells, cnr, flags, buff, &bit);
  if (MSM5 == msm_type) {
    encode_msm_fine_phaserangerates(num_cells, fine_dop, flags, buff, &bit);
  }

  /* Round number of bits up to nearest whole byte. */
  return (bit + 7) / 8;
}
