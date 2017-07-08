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

/** Set bit field in buffer from an unsigned integer.
 * Packs `len` bits into bit position `pos` from the start of the buffer.
 * Maximum bit field length is 32 bits, i.e. `len <= 32`.
 *
 * \param buff
 * \param pos Position in buffer of start of bit field in bits.
 * \param len Length of bit field in bits.
 * \param data Unsigned integer to be packed into bit field.
 */
void setbitu(uint8_t *buff, uint32_t pos, uint32_t len, uint32_t data)
{
  uint32_t mask = 1u << (len - 1);

  if (len <= 0 || 32 < len)
    return;

  for (uint32_t i = pos; i < pos + len; i++, mask >>= 1) {
    if (data & mask)
      buff[i / 8] |= 1u << (7 - i % 8);
    else
      buff[i / 8] &= ~(1u << (7 - i % 8));
  }
}

/** Set bit field in buffer from an unsigned integer.
 * Packs `len` bits into bit position `pos` from the start of the buffer.
 * Maximum bit field length is 32 bits, i.e. `len <= 32`.
 *
 * \param buff
 * \param pos Position in buffer of start of bit field in bits.
 * \param len Length of bit field in bits.
 * \param data Unsigned integer to be packed into bit field.
 */
void setbitul(uint8_t *buff, uint32_t pos, uint32_t len, uint64_t data)
{
  uint64_t mask = ((uint64_t)1) << (len - 1);

  if (len <= 0 || 64 < len)
    return;

  for (uint32_t i = pos; i < pos + len; i++, mask >>= 1) {
    if (data & mask)
      buff[i / 8] |= ((uint64_t)1) << (7 - i % 8);
    else
      buff[i / 8] &= ~(((uint64_t)1) << (7 - i % 8));
  }
}

/** Set bit field in buffer from a signed integer.
 * Packs `len` bits into bit position `pos` from the start of the buffer.
 * Maximum bit field length is 32 bits, i.e. `len <= 32`.
 *
 * \param buff
 * \param pos Position in buffer of start of bit field in bits.
 * \param len Length of bit field in bits.
 * \param data Signed integer to be packed into bit field.
 */
void setbits(uint8_t *buff, uint32_t pos, uint32_t len, int32_t data)
{
  setbitu(buff, pos, len, (uint32_t)data);
}

/** Set bit field in buffer from a signed integer.
 * Packs `len` bits into bit position `pos` from the start of the buffer.
 * Maximum bit field length is 32 bits, i.e. `len <= 32`.
 *
 * \param buff
 * \param pos Position in buffer of start of bit field in bits.
 * \param len Length of bit field in bits.
 * \param data Signed integer to be packed into bit field.
 */
void setbitsl(uint8_t *buff, uint32_t pos, uint32_t len, int64_t data)
{
  setbitul(buff, pos, len, (uint64_t)data);
}

/** Convert a lock time in seconds into a RTCMv3 Lock Time Indicator value.
 * See RTCM 10403.1, Table 3.4-2.
 *
 * \param time Lock time in seconds.
 * \return Lock Time Indicator value.
 */
static uint8_t to_lock_ind(uint32_t time)
{
  if (time < 24)
    return time;
  if (time < 72)
    return (time + 24) / 2;
  if (time < 168)
    return (time + 120) / 4;
  if (time < 360)
    return (time + 408) / 8;
  if (time < 744)
    return (time + 1176) / 16;
  if (time < 937)
    return (time + 3096) / 32;
  return 127;
}

void encode_basic_freq_data(const rtcm_freq_data *freq_data, const double freq,
                            const double *l1_pr, uint8_t *buff, uint16_t *bit)
{

  /* Calculate GPS Integer L1 Pseudorange Modulus Ambiguity (DF014). */
  uint8_t amb = (uint8_t)(*l1_pr / PRUNIT_GPS);

  /* Construct L1 pseudorange value as it would be transmitted (DF011). */
  uint32_t calc_l1_pr = (uint32_t)roundl((double)(*l1_pr - amb * PRUNIT_GPS) / 0.02);

  /* Calculate GPS Pseudorange (DF011/DF016). */
  uint32_t pr = (uint32_t)roundl((freq_data->pseudorange - amb * PRUNIT_GPS) / 0.02);

  double l1_prc = calc_l1_pr * 0.02 + amb * PRUNIT_GPS;

  /* phaserange - L1 pseudorange */
  double cp_pr = freq_data->carrier_phase - l1_prc / (CLIGHT / freq);

  // TODO (anthony) If the pr and cp diverge, we should adjust the cp ambiguity and reset the lock time indicator

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
  setbitu(buff, *bit, 7,
          freq_data->flags.valid_lock ? to_lock_ind(freq_data->lock) : 0);
  *bit += 7;
}

void encode_basic_glo_freq_data(const rtcm_freq_data *freq_data, const double freq,
                                const double *l1_pr, const uint8_t fcn, uint8_t *buff, uint16_t *bit)
{
  bool L1 = fabs(freq - GLO_L1_FREQ) < 0.01;
  /* Calculate GPS Integer L1 Pseudorange Modulus Ambiguity (DF044). */
  uint8_t amb = (uint8_t)(*l1_pr / PRUNIT_GLO);

  /* Construct L1 pseudorange value as it would be transmitted (DF041). */
  uint32_t calc_l1_pr = (uint32_t)roundl((double)(*l1_pr - amb * PRUNIT_GLO) / 0.02);

  /* Calculate GLO Pseudorange (DF041/DF046). */
  uint32_t pr = (uint32_t)roundl((freq_data->pseudorange - amb * PRUNIT_GLO) / 0.02);

  double l1_prc = calc_l1_pr * 0.02 + amb * PRUNIT_GLO;

  double glo_freq = 0.0;
  if (L1) {
    glo_freq = freq + (fcn - 7) * GLO_L1_CH_OFFSET;
  } else {
    glo_freq = freq + (fcn - 7) * GLO_L2_CH_OFFSET;
  }
  /* phaserange - L1 pseudorange */
  double cp_pr = freq_data->carrier_phase - l1_prc / (CLIGHT / glo_freq );

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
  setbitu(buff, *bit, 7,
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
uint16_t rtcm3_write_header(const rtcm_obs_header *header, uint8_t num_sats, uint8_t *buff)
{
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
uint16_t rtcm3_write_glo_header(const rtcm_obs_header *header, uint8_t num_sats, uint8_t *buff)
{
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

uint16_t rtcm3_encode_1001(const rtcm_obs_message *msg_1001, uint8_t *buff)
{
  uint16_t bit = 64; /* Start at end of header. */

  uint8_t num_sats = 0;
  for (uint8_t i = 0; i < msg_1001->header.n_sat; i++) {
    if (msg_1001->sats[i].obs[L1_FREQ].flags.valid_pr &&
        msg_1001->sats[i].obs[L1_FREQ].flags.valid_cp) {
      setbitu(buff, bit, 6, msg_1001->sats[i].svId);
      bit += 6;
      encode_basic_freq_data(&msg_1001->sats[i].obs[L1_FREQ], GPS_L1_FREQ,
                             &msg_1001->sats[i].obs[L1_FREQ].pseudorange,
                             buff, &bit);
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
uint16_t rtcm3_encode_1002(const rtcm_obs_message *msg_1002, uint8_t *buff)
{
  uint16_t bit = 64; /* Start at end of header. */

  uint8_t num_sats = 0;
  for (uint8_t i = 0; i < msg_1002->header.n_sat; i++) {
    if (msg_1002->sats[i].obs[L1_FREQ].flags.valid_pr &&
        msg_1002->sats[i].obs[L1_FREQ].flags.valid_cp) {
      setbitu(buff, bit, 6, msg_1002->sats[i].svId);
      bit += 6;
      encode_basic_freq_data(&msg_1002->sats[i].obs[L1_FREQ], GPS_L1_FREQ,
                             &msg_1002->sats[i].obs[L1_FREQ].pseudorange,
                             buff, &bit);

      /* Calculate GPS Integer L1 Pseudorange Modulus Ambiguity (DF014). */
      uint8_t amb =
        (uint8_t)(msg_1002->sats[i].obs[L1_FREQ].pseudorange / PRUNIT_GPS);

      setbitu(buff, bit, 8, amb);
      bit += 8;
      setbitu(buff, bit, 8,
              (uint8_t)roundl(msg_1002->sats[i].obs[L1_FREQ].cnr * 4.0));
      bit += 8;
      ++num_sats;
    }
  }

  rtcm3_write_header(&msg_1002->header, num_sats, buff);

  /* Round number of bits up to nearest whole byte. */
  return (bit + 7) / 8;
}

uint16_t rtcm3_encode_1003(const rtcm_obs_message *msg_1003, uint8_t *buff)
{
  uint16_t bit = 64; /* Start at end of header. */

  uint8_t num_sats = 0;
  for (uint8_t i = 0; i < msg_1003->header.n_sat; i++) {
    flag_bf l1_flags = msg_1003->sats[i].obs[L1_FREQ].flags;
    flag_bf l2_flags = msg_1003->sats[i].obs[L2_FREQ].flags;
    if (l1_flags.valid_pr && l1_flags.valid_cp && l2_flags.valid_pr &&
        l2_flags.valid_cp) {
      setbitu(buff, bit, 6, msg_1003->sats[i].svId);
      bit += 6;
      encode_basic_freq_data(&msg_1003->sats[i].obs[L1_FREQ], GPS_L1_FREQ,
                             &msg_1003->sats[i].obs[L1_FREQ].pseudorange,
                             buff, &bit);
      encode_basic_freq_data(&msg_1003->sats[i].obs[L2_FREQ], GPS_L2_FREQ,
                             &msg_1003->sats[i].obs[L1_FREQ].pseudorange,
                             buff, &bit);
      ++num_sats;
    }
  }

  rtcm3_write_header(&msg_1003->header, num_sats, buff);

  /* Round number of bits up to nearest whole byte. */
  return (bit + 7) / 8;
}

uint16_t rtcm3_encode_1004(const rtcm_obs_message *msg_1004, uint8_t *buff)
{
  uint16_t bit = 64; /* Start at end of header. */

  uint8_t num_sats = 0;
  for (uint8_t i = 0; i < msg_1004->header.n_sat; i++) {
    flag_bf l1_flags = msg_1004->sats[i].obs[L1_FREQ].flags;
    flag_bf l2_flags = msg_1004->sats[i].obs[L2_FREQ].flags;
    if (l1_flags.valid_pr && l1_flags.valid_cp && l2_flags.valid_pr &&
        l2_flags.valid_cp) {
      setbitu(buff, bit, 6, msg_1004->sats[i].svId);
      bit += 6;
      encode_basic_freq_data(&msg_1004->sats[i].obs[L1_FREQ], GPS_L1_FREQ,
                             &msg_1004->sats[i].obs[L1_FREQ].pseudorange,
                             buff, &bit);

      /* Calculate GPS Integer L1 Pseudorange Modulus Ambiguity (DF014). */
      uint8_t amb =
        (uint8_t)(msg_1004->sats[i].obs[L1_FREQ].pseudorange / PRUNIT_GPS);

      setbitu(buff, bit, 8, amb);
      bit += 8;
      setbitu(buff, bit, 8,
              (uint8_t)roundl(msg_1004->sats[i].obs[L1_FREQ].cnr * 4.0));
      bit += 8;

      encode_basic_freq_data(&msg_1004->sats[i].obs[L2_FREQ], GPS_L2_FREQ,
                             &msg_1004->sats[i].obs[L1_FREQ].pseudorange,
                             buff, &bit);
      setbitu(buff, bit, 8,
              (uint8_t)roundl(msg_1004->sats[i].obs[L2_FREQ].cnr * 4.0));
      bit += 8;
      ++num_sats;
    }
  }

  rtcm3_write_header(&msg_1004->header, num_sats, buff);

  /* Round number of bits up to nearest whole byte. */
  return (bit + 7) / 8;
}

uint16_t rtcm3_encode_1005_base(const rtcm_msg_1005 *msg_1005, uint8_t *buff,
                           uint16_t *bit)
{
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

uint16_t rtcm3_encode_1005(const rtcm_msg_1005 *msg_1005, uint8_t *buff)
{
  uint16_t bit = 0;
  setbitu(buff, bit, 12, 1005);
  bit += 12;
  return rtcm3_encode_1005_base(msg_1005, buff, &bit);
}

uint16_t rtcm3_encode_1006(const rtcm_msg_1006 *msg_1006, uint8_t *buff)
{
  uint16_t bit = 0;
  setbitu(buff, bit, 12, 1006);
  bit += 12;
  rtcm3_encode_1005_base(&msg_1006->msg_1005, buff, &bit);
  setbitu(buff, bit, 16, (uint16_t)roundl(msg_1006->ant_height * 10000.0));
  bit += 16;

  /* Round number of bits up to nearest whole byte. */
  return (bit + 7) / 8;
}

uint16_t rtcm3_encode_1007_base(const rtcm_msg_1007 *msg_1007, uint8_t *buff,
                           uint16_t *bit)
{
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

uint16_t rtcm3_encode_1007(const rtcm_msg_1007 *msg_1007, uint8_t *buff)
{
  uint16_t bit = 0;
  setbitu(buff, bit, 12, 1007);
  bit += 12;
  return rtcm3_encode_1007_base(msg_1007, buff, &bit);
}

uint16_t rtcm3_encode_1008(const rtcm_msg_1008 *msg_1008, uint8_t *buff)
{
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

uint16_t rtcm3_encode_1010(const rtcm_obs_message *msg_1010, uint8_t *buff)
{
  uint16_t bit = 61; /* Start at end of header. */

  uint8_t num_sats = 0;
  for (uint8_t i = 0; i < msg_1010->header.n_sat; i++) {
    if (msg_1010->sats[i].obs[L1_FREQ].flags.valid_pr &&
        msg_1010->sats[i].obs[L1_FREQ].flags.valid_cp) {
      const rtcm_sat_data *sat_obs = &msg_1010->sats[i];
      setbitu(buff, bit, 6, sat_obs->svId);
      bit += 6;
      encode_basic_glo_freq_data(&sat_obs->obs[L1_FREQ], GLO_L1_FREQ,
                                 &sat_obs->obs[L1_FREQ].pseudorange, sat_obs->fcn,
                                 buff, &bit);

      /* Calculate GPS Integer L1 Pseudorange Modulus Ambiguity (DF014). */
      uint8_t amb =
        (uint8_t)(sat_obs->obs[L1_FREQ].pseudorange / PRUNIT_GLO);

      setbitu(buff, bit, 7, amb);
      bit += 7;
      setbitu(buff, bit, 8,
              (uint8_t)roundl(sat_obs->obs[L1_FREQ].cnr * 4.0));
      bit += 8;
      ++num_sats;
    }
  }

  rtcm3_write_glo_header(&msg_1010->header, num_sats, buff);

  /* Round number of bits up to nearest whole byte. */
  return (bit + 7) / 8;
}

uint16_t rtcm3_encode_1012(const rtcm_obs_message *msg_1012, uint8_t *buff)
{
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
      encode_basic_glo_freq_data(&sat_obs->obs[L1_FREQ], GLO_L1_FREQ,
                                 &sat_obs->obs[L1_FREQ].pseudorange, sat_obs->fcn,
                                 buff, &bit);

      /* Calculate GLO Integer L1 Pseudorange Modulus Ambiguity (DF014). */
      uint8_t amb =
        (uint8_t)(sat_obs->obs[L1_FREQ].pseudorange / PRUNIT_GLO);

      setbitu(buff, bit, 8, amb);
      bit += 8;
      setbitu(buff, bit, 8,
              (uint8_t)roundl(sat_obs->obs[L1_FREQ].cnr * 4.0));
      bit += 8;

      encode_basic_glo_freq_data(&sat_obs->obs[L2_FREQ], GLO_L2_FREQ,
                                 &sat_obs->obs[L1_FREQ].pseudorange, sat_obs->fcn,
                                 buff, &bit);
      setbitu(buff, bit, 8,
              (uint8_t)roundl(sat_obs->obs[L2_FREQ].cnr * 4.0));
      bit += 8;
      ++num_sats;
    }
  }

  rtcm3_write_glo_header(&msg_1012->header, num_sats, buff);

  /* Round number of bits up to nearest whole byte. */
  return (bit + 7) / 8;
}
