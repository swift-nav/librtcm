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

// pgrgich: need to handle invalid obs...

#include "rtcm3_decode.h"
#include <math.h>

/** Get bit field from buffer as an unsigned integer.
 * Unpacks `len` bits at bit position `pos` from the start of the buffer.
 * Maximum bit field length is 32 bits, i.e. `len <= 32`.
 *
 * \param buff
 * \param pos Position in buffer of start of bit field in bits.
 * \param len Length of bit field in bits.
 * \return Bit field as an unsigned value.
 */
uint32_t getbitu(const uint8_t *buff, uint32_t pos, uint8_t len)
{
  uint32_t bits = 0;

  for (uint32_t i = pos; i < pos + len; i++) {
    bits = (bits << 1) + ((buff[i / 8] >> (7 - i % 8)) & 1u);
  }

  return bits;
}

/** Get bit field from buffer as an unsigned integer.
 * Unpacks `len` bits at bit position `pos` from the start of the buffer.
 * Maximum bit field length is 32 bits, i.e. `len <= 32`.
 *
 * \param buff
 * \param pos Position in buffer of start of bit field in bits.
 * \param len Length of bit field in bits.
 * \return Bit field as an unsigned value.
 */
uint64_t getbitul(const uint8_t *buff, uint32_t pos, uint8_t len)
{
  uint64_t bits = 0;

  for (uint32_t i = pos; i < pos + len; i++) {
    bits = (bits << 1) + ((buff[i / 8] >> (7 - i % 8)) & 1u);
  }

  return bits;
}

/** Get bit field from buffer as a signed integer.
 * Unpacks `len` bits at bit position `pos` from the start of the buffer.
 * Maximum bit field length is 32 bits, i.e. `len <= 32`.
 *
 * This function sign extends the `len` bit field to a signed 32 bit integer.
 *
 * \param buff
 * \param pos Position in buffer of start of bit field in bits.
 * \param len Length of bit field in bits.
 * \return Bit field as a signed value.
 */
int32_t getbits(const uint8_t *buff, uint32_t pos, uint8_t len)
{
  int32_t bits = (int32_t)getbitu(buff, pos, len);

  /* Sign extend, taken from:
   * http://graphics.stanford.edu/~seander/bithacks.html#VariableSignExtend
   */
  int32_t m = 1u << (len - 1);
  return (bits ^ m) - m;
}

/** Get bit field from buffer as a signed integer.
 * Unpacks `len` bits at bit position `pos` from the start of the buffer.
 * Maximum bit field length is 64 bits, i.e. `len <= 64`.
 *
 * This function sign extends the `len` bit field to a signed 64 bit integer.
 *
 * \param buff
 * \param pos Position in buffer of start of bit field in bits.
 * \param len Length of bit field in bits.
 * \return Bit field as a signed value.
 */
int64_t getbitsl(const uint8_t *buff, uint32_t pos, uint8_t len)
{
  int64_t bits = (int64_t)getbitul(buff, pos, len);

  /* Sign extend, taken from:
   * http://graphics.stanford.edu/~seander/bithacks.html#VariableSignExtend
   */
  int64_t m = ((uint64_t)1) << (len - 1);
  return (bits ^ m) - m;
}

void init_data(rtcm_sat_data *sat_data)
{
  for (uint8_t freq = 0; freq < NUM_FREQS; ++freq) {
    sat_data->obs[freq].flags.data = 0;
  }
}

static uint32_t from_lock_ind(uint8_t lock)
{
  if (lock < 24)
    return lock;
  if (lock < 48)
    return 2 * lock - 24;
  if (lock < 72)
    return 4 * lock - 120;
  if (lock < 96)
    return 8 * lock - 408;
  if (lock < 120)
    return 16 * lock - 1176;
  if (lock < 127)
    return 32 * lock - 3096;
  return 937;
}

void decode_basic_gps_l1_freq_data(const uint8_t *buff, uint16_t *bit,
                                   rtcm_freq_data *freq_data, uint32_t *pr,
                                   int32_t *phr_pr_diff)
{
  freq_data->code = getbitu(buff, *bit, 1);
  *bit += 1;
  *pr = getbitu(buff, *bit, 24);
  *bit += 24;
  *phr_pr_diff = getbits(buff, *bit, 20);
  *bit += 20;

  freq_data->lock = from_lock_ind(getbitu(buff, *bit, 7));
  *bit += 7;
  freq_data->flags.valid_lock = 1;

  return;
}

void decode_basic_glo_l1_freq_data(const uint8_t *buff, uint16_t *bit,
                                   rtcm_freq_data *freq_data, uint32_t *pr,
                                   int32_t *phr_pr_diff, uint8_t *fcn)
{
  freq_data->code = getbitu(buff, *bit, 1);
  *bit += 1;
  *fcn = getbitu(buff, *bit, 5);
  *bit += 5;
  *pr = getbitu(buff, *bit, 25);
  *bit += 25;
  *phr_pr_diff = getbits(buff, *bit, 20);
  *bit += 20;
  freq_data->lock = from_lock_ind(getbitu(buff, *bit, 7));
  *bit += 7;
  freq_data->flags.valid_lock = 1;
  return;
}

void decode_basic_l2_freq_data(const uint8_t *buff, uint16_t *bit,
                               rtcm_freq_data *freq_data, int32_t *pr,
                               int32_t *phr_pr_diff)
{
  freq_data->code = getbitu(buff, *bit, 2);
  *bit += 2;
  *pr = getbits(buff, *bit, 14);
  *bit += 14;
  *phr_pr_diff = getbits(buff, *bit, 20);
  *bit += 20;

  freq_data->lock = from_lock_ind(getbitu(buff, *bit, 7));
  *bit += 7;
  freq_data->flags.valid_lock = 1;

  return;
}

uint16_t rtcm3_read_header(const uint8_t *buff, rtcm_obs_header *header)
{
  uint16_t bit = 0;
  header->msg_num = getbitu(buff, bit, 12);
  bit += 12;
  header->stn_id = getbitu(buff, bit, 12);
  bit += 12;
  header->tow_ms = getbitu(buff, bit, 30);
  bit += 30;
  header->sync = getbitu(buff, bit, 1);
  bit += 1;
  header->n_sat = getbitu(buff, bit, 5);
  bit += 5;
  header->div_free = getbitu(buff, bit, 1);
  bit += 1;
  header->smooth = getbitu(buff, bit, 3);
  bit += 3;
  return bit;
}

uint16_t rtcm3_read_glo_header(const uint8_t *buff, rtcm_obs_header *header)
{
  uint16_t bit = 0;
  header->msg_num = getbitu(buff, bit, 12);
  bit += 12;
  header->stn_id = getbitu(buff, bit, 12);
  bit += 12;
  header->tow_ms = getbitu(buff, bit, 27);
  bit += 27;
  header->sync = getbitu(buff, bit, 1);
  bit += 1;
  header->n_sat = getbitu(buff, bit, 5);
  bit += 5;
  header->div_free = getbitu(buff, bit, 1);
  bit += 1;
  header->smooth = getbitu(buff, bit, 3);
  bit += 3;
  return bit;
}

/** Decode an RTCMv3 message type 1001 (L1-Only GPS RTK Observables)
 *
 * \param buff The input data buffer
 * \param RTCM message struct
 * \return If valid then return 0.
 *         Returns a negative number if the message is invalid:
 *          - `-1` : Message type mismatch
 */
int8_t rtcm3_decode_1001(const uint8_t *buff, rtcm_obs_message *msg_1001)
{
  uint16_t bit = 0;
  bit += rtcm3_read_header(buff, &msg_1001->header);

  if (msg_1001->header.msg_num != 1001)
    /* Unexpected message type. */
    return -1;

  for (uint8_t i = 0; i < msg_1001->header.n_sat; i++) {
    init_data(&msg_1001->sats[i]);

    /* TODO: Handle SBAS prns properly, numbered differently in RTCM? */
    msg_1001->sats[i].svId = getbitu(buff, bit, 6);
    bit += 6;

    rtcm_freq_data *l1_freq_data = &msg_1001->sats[i].obs[L1_FREQ];

    uint32_t l1_pr;
    int32_t phr_pr_diff;
    decode_basic_gps_l1_freq_data(buff, &bit, l1_freq_data, &l1_pr,
                                  &phr_pr_diff);

    l1_freq_data->pseudorange = 0.02 * l1_pr;
    l1_freq_data->flags.valid_pr = 1;
    l1_freq_data->carrier_phase =
        (l1_freq_data->pseudorange + 0.0005 * phr_pr_diff) /
        (CLIGHT / GPS_L1_FREQ);
    l1_freq_data->flags.valid_cp = 1;
  }

  return 0;
}

/** Decode an RTCMv3 message type 1002 (Extended L1-Only GPS RTK Observables)
 *
 * \param buff The input data buffer
 * \param RTCM message struct
 * \return If valid then return 0.
 *         Returns a negative number if the message is invalid:
 *          - `-1` : Message type mismatch
 */
int8_t rtcm3_decode_1002(const uint8_t *buff, rtcm_obs_message *msg_1002)
{
  uint16_t bit = 0;
  bit += rtcm3_read_header(buff, &msg_1002->header);

  if (msg_1002->header.msg_num != 1002)
    /* Unexpected message type. */
    return -1;

  for (uint8_t i = 0; i < msg_1002->header.n_sat; i++) {
    init_data(&msg_1002->sats[i]);

    /* TODO: Handle SBAS prns properly, numbered differently in RTCM? */
    msg_1002->sats[i].svId = getbitu(buff, bit, 6);
    bit += 6;

    rtcm_freq_data *l1_freq_data = &msg_1002->sats[i].obs[L1_FREQ];

    uint32_t l1_pr;
    int32_t phr_pr_diff;
    decode_basic_gps_l1_freq_data(buff, &bit, l1_freq_data, &l1_pr,
                                  &phr_pr_diff);

    uint8_t amb = getbitu(buff, bit, 8);
    bit += 8;
    l1_freq_data->cnr = 0.25 * getbitu(buff, bit, 8);
    bit += 8;
    l1_freq_data->flags.valid_cnr = 1;

    l1_freq_data->pseudorange = 0.02 * l1_pr + PRUNIT_GPS * amb;
    l1_freq_data->flags.valid_pr = 1;
    l1_freq_data->carrier_phase =
        (l1_freq_data->pseudorange + 0.0005 * phr_pr_diff) /
        (CLIGHT / GPS_L1_FREQ);
    l1_freq_data->flags.valid_cp = 1;
  }

  return 0;
}

/** Decode an RTCMv3 message type 1003 (L1/L2 GPS RTK Observables)
 *
 * \param buff The input data buffer
 * \param RTCM message struct
 * \return If valid then return 0.
 *         Returns a negative number if the message is invalid:
 *          - `-1` : Message type mismatch
 */
int8_t rtcm3_decode_1003(const uint8_t *buff, rtcm_obs_message *msg_1003)
{
  uint16_t bit = 0;
  bit += rtcm3_read_header(buff, &msg_1003->header);

  if (msg_1003->header.msg_num != 1003)
    /* Unexpected message type. */
    return -1;

  for (uint8_t i = 0; i < msg_1003->header.n_sat; i++) {
    init_data(&msg_1003->sats[i]);

    /* TODO: Handle SBAS prns properly, numbered differently in RTCM? */
    msg_1003->sats[i].svId = getbitu(buff, bit, 6);
    bit += 6;

    rtcm_freq_data *l1_freq_data = &msg_1003->sats[i].obs[L1_FREQ];

    uint32_t l1_pr;
    int32_t l2_pr;
    int32_t phr_pr_diff;
    decode_basic_gps_l1_freq_data(buff, &bit, l1_freq_data, &l1_pr,
                                  &phr_pr_diff);

    l1_freq_data->pseudorange = 0.02 * l1_pr;
    l1_freq_data->flags.valid_pr = 1;
    l1_freq_data->carrier_phase =
        (l1_freq_data->pseudorange + 0.0005 * phr_pr_diff) /
        (CLIGHT / GPS_L1_FREQ);
    l1_freq_data->flags.valid_cp = 1;

    rtcm_freq_data *l2_freq_data = &msg_1003->sats[i].obs[L2_FREQ];

    decode_basic_l2_freq_data(buff, &bit, l2_freq_data, &l2_pr,
                              &phr_pr_diff);

    l2_freq_data->pseudorange = 0.02 * l2_pr + l1_freq_data->pseudorange;
    l2_freq_data->flags.valid_pr = 1;
    l2_freq_data->carrier_phase =
        (l1_freq_data->pseudorange + 0.0005 * phr_pr_diff) /
        (CLIGHT / GPS_L2_FREQ);
    l2_freq_data->flags.valid_cp = 1;
  }

  return 0;
}

/** Decode an RTCMv3 message type 1004 (Extended L1/L2 GPS RTK Observables)
 *
 * \param buff The input data buffer
 * \param RTCM message struct
 * \return If valid then return 0.
 *         Returns a negative number if the message is invalid:
 *          - `-1` : Message type mismatch
 */
int8_t rtcm3_decode_1004(const uint8_t *buff, rtcm_obs_message *msg_1004)
{
  uint16_t bit = 0;
  bit += rtcm3_read_header(buff, &msg_1004->header);

  if (msg_1004->header.msg_num != 1004)
    /* Unexpected message type. */
    return -1;

  for (uint8_t i = 0; i < msg_1004->header.n_sat; i++) {
    init_data(&msg_1004->sats[i]);

    /* TODO: Handle SBAS prns properly, numbered differently in RTCM? */
    msg_1004->sats[i].svId = getbitu(buff, bit, 6);
    bit += 6;

    rtcm_freq_data *l1_freq_data = &msg_1004->sats[i].obs[L1_FREQ];

    uint32_t l1_pr;
    int32_t l2_pr;
    int32_t phr_pr_diff;
    decode_basic_gps_l1_freq_data(buff, &bit, l1_freq_data, &l1_pr,
                                  &phr_pr_diff);

    uint8_t amb = getbitu(buff, bit, 8);
    bit += 8;
    l1_freq_data->cnr = 0.25 * getbitu(buff, bit, 8);
    bit += 8;
    l1_freq_data->flags.valid_cnr = 1;

    l1_freq_data->pseudorange = 0.02 * l1_pr + PRUNIT_GPS * amb;
    l1_freq_data->flags.valid_pr = 1;
    l1_freq_data->carrier_phase =
        (l1_freq_data->pseudorange + 0.0005 * phr_pr_diff) /
        (CLIGHT / GPS_L1_FREQ);
    l1_freq_data->flags.valid_cp = 1;

    rtcm_freq_data *l2_freq_data = &msg_1004->sats[i].obs[L2_FREQ];

    decode_basic_l2_freq_data(buff, &bit, l2_freq_data, &l2_pr,
                              &phr_pr_diff);

    l2_freq_data->cnr = 0.25 * getbitu(buff, bit, 8);
    bit += 8;
    l2_freq_data->flags.valid_cnr = 1;

    l2_freq_data->pseudorange = 0.02 * l2_pr + l1_freq_data->pseudorange;
    l2_freq_data->flags.valid_pr = 1;
    l2_freq_data->carrier_phase =
        (l1_freq_data->pseudorange + 0.0005 * phr_pr_diff) /
        (CLIGHT / GPS_L2_FREQ);
    l2_freq_data->flags.valid_cp = 1;
  }

  return 0;
}

int8_t rtcm3_decode_1005_base(const uint8_t *buff, rtcm_msg_1005 *msg_1005,
                          uint16_t *bit)
{
  msg_1005->stn_id = getbitu(buff, *bit, 12);
  *bit += 12;
  msg_1005->ITRF = getbitu(buff, *bit, 6);
  *bit += 6;
  msg_1005->GPS_ind = getbitu(buff, *bit, 1);
  *bit += 1;
  msg_1005->GLO_ind = getbitu(buff, *bit, 1);
  *bit += 1;
  msg_1005->GAL_ind = getbitu(buff, *bit, 1);
  *bit += 1;
  msg_1005->ref_stn_ind = getbitu(buff, *bit, 1);
  *bit += 1;
  msg_1005->arp_x = (double)(getbitsl(buff, *bit, 38)) / 10000.0;
  *bit += 38;
  msg_1005->osc_ind = getbitu(buff, *bit, 1);
  *bit += 1;
  getbitu(buff, *bit, 1);
  *bit += 1;
  msg_1005->arp_y = (double)(getbitsl(buff, *bit, 38)) / 10000.0;
  *bit += 38;
  msg_1005->quart_cycle_ind = getbitu(buff, *bit, 2);
  *bit += 2;
  msg_1005->arp_z = (double)(getbitsl(buff, *bit, 38)) / 10000.0;
  *bit += 38;

  return 0;
}

/** Decode an RTCMv3 message type 1005 (Stationary RTK Reference Station ARP)
 *
 * \param buff The input data buffer
 * \param RTCM message struct
 * \return If valid then return 0.
 *         Returns a negative number if the message is invalid:
 *          - `-1` : Message type mismatch
 */
int8_t rtcm3_decode_1005(const uint8_t *buff, rtcm_msg_1005 *msg_1005)
{
  uint16_t bit = 0;
  uint16_t msg_num = getbitu(buff, bit, 12);
  bit += 12;

  if (msg_num != 1005)
    /* Unexpected message type. */
    return -1;

  return rtcm3_decode_1005_base(buff, msg_1005, &bit);
}

/** Decode an RTCMv3 message type 1005 (Stationary RTK Reference Station ARP with antenna height)
 *
 * \param buff The input data buffer
 * \param RTCM message struct
 * \return If valid then return 0.
 *         Returns a negative number if the message is invalid:
 *          - `-1` : Message type mismatch
 */
int8_t rtcm3_decode_1006(const uint8_t *buff, rtcm_msg_1006 *msg_1006)
{
  uint16_t bit = 0;
  uint16_t msg_num = getbitu(buff, bit, 12);
  bit += 12;

  if (msg_num != 1006)
    /* Unexpected message type. */
    return -1;

  rtcm3_decode_1005_base(buff, &msg_1006->msg_1005, &bit);
  msg_1006->ant_height = (double)(getbitu(buff, bit, 16)) / 10000.0;
  bit += 16;
  return 0;
}

int8_t rtcm3_decode_1007_base(const uint8_t *buff, rtcm_msg_1007 *msg_1007,
                          uint16_t *bit)
{
  msg_1007->stn_id = getbitu(buff, *bit, 12);
  *bit += 12;
  msg_1007->desc_count = getbitu(buff, *bit, 8);
  *bit += 8;
  for (uint8_t i = 0; i < msg_1007->desc_count; ++i) {
    msg_1007->desc[i] = getbitu(buff, *bit, 8);
    *bit += 8;
  }
  msg_1007->ant_id = getbitu(buff, *bit, 8);
  *bit += 8;

  return 0;
}

/** Decode an RTCMv3 message type 1007 (Antenna Descriptor)
 *
 * \param buff The input data buffer
 * \param RTCM message struct
 * \return If valid then return 0.
 *         Returns a negative number if the message is invalid:
 *          - `-1` : Message type mismatch
 */
int8_t rtcm3_decode_1007(const uint8_t *buff, rtcm_msg_1007 *msg_1007)
{
  uint16_t bit = 0;
  uint16_t msg_num = getbitu(buff, bit, 12);
  bit += 12;

  if (msg_num != 1007)
    /* Unexpected message type. */
    return -1;

  rtcm3_decode_1007_base(buff, msg_1007, &bit);

  return 0;
}

/** Decode an RTCMv3 message type 1008 (Antenna Descriptor & Serial Number)
 *
 * \param buff The input data buffer
 * \param RTCM message struct
 * \return If valid then return 0.
 *         Returns a negative number if the message is invalid:
 *          - `-1` : Message type mismatch
 */
int8_t rtcm3_decode_1008(const uint8_t *buff, rtcm_msg_1008 *msg_1008)
{
  uint16_t bit = 0;
  uint16_t msg_num = getbitu(buff, bit, 12);
  bit += 12;

  if (msg_num != 1008)
    /* Unexpected message type. */
    return -1;

  rtcm3_decode_1007_base(buff, &msg_1008->msg_1007, &bit);
  msg_1008->serial_count = getbitu(buff, bit, 8);
  bit += 8;
  for (uint8_t i = 0; i < msg_1008->serial_count; ++i) {
    msg_1008->serial_num[i] = getbitu(buff, bit, 8);
    bit += 8;
  }
  return 0;
}

/** Decode an RTCMv3 message type 1010 (Extended L1-Only GLO RTK Observables)
 *
 * \param buff The input data buffer
 * \param RTCM message struct
 * \return If valid then return 0.
 *         Returns a negative number if the message is invalid:
 *          - `-1` : Message type mismatch
 */
int8_t rtcm3_decode_1010(const uint8_t *buff, rtcm_obs_message *msg_1010)
{
  uint16_t bit = 0;
  bit += rtcm3_read_glo_header(buff, &msg_1010->header);

  if (msg_1010->header.msg_num != 1010)
    /* Unexpected message type. */
    return -1;

  for (uint8_t i = 0; i < msg_1010->header.n_sat; i++) {
    init_data(&msg_1010->sats[i]);

    msg_1010->sats[i].svId = getbitu(buff, bit, 6);
    bit += 6;

    rtcm_freq_data *l1_freq_data = &msg_1010->sats[i].obs[L1_FREQ];

    uint32_t l1_pr;
    int32_t phr_pr_diff;
    decode_basic_glo_l1_freq_data(buff, &bit, l1_freq_data, &l1_pr,
                                  &phr_pr_diff, &msg_1010->sats[i].fcn);

    uint8_t amb = getbitu(buff, bit, 7);
    bit += 7;
    l1_freq_data->cnr = 0.25 * getbitu(buff, bit, 8);
    bit += 8;
    l1_freq_data->flags.valid_cnr = 1;

    l1_freq_data->pseudorange = 0.02 * l1_pr + PRUNIT_GLO * amb;
    l1_freq_data->flags.valid_pr = 1;
    int8_t glo_fcn = msg_1010->sats[i].fcn - 7;
    l1_freq_data->carrier_phase =
      (l1_freq_data->pseudorange + 0.0005 * phr_pr_diff) /
      (CLIGHT / (GLO_L1_FREQ + glo_fcn * GLO_L1_CH_OFFSET) );
    l1_freq_data->flags.valid_cp = 1;
  }

  return 0;
}

/** Decode an RTCMv3 message type 1012 (Extended L1/L2 GLO RTK Observables)
 *
 * \param buff The input data buffer
 * \param RTCM message struct
 * \return If valid then return 0.
 *         Returns a negative number if the message is invalid:
 *          - `-1` : Message type mismatch
 */
int8_t rtcm3_decode_1012(const uint8_t *buff, rtcm_obs_message *msg_1012)
{
  uint16_t bit = 0;
  bit += rtcm3_read_glo_header(buff, &msg_1012->header);

  if (msg_1012->header.msg_num != 1012)
    /* Unexpected message type. */
    return -1;

  for (uint8_t i = 0; i < msg_1012->header.n_sat; i++) {
    init_data(&msg_1012->sats[i]);

    /* TODO: Handle SBAS prns properly, numbered differently in RTCM? */
    msg_1012->sats[i].svId = getbitu(buff, bit, 6);
    bit += 6;

    rtcm_freq_data *l1_freq_data = &msg_1012->sats[i].obs[L1_FREQ];

    uint32_t l1_pr;
    int32_t l2_pr;
    int32_t phr_pr_diff;
    decode_basic_glo_l1_freq_data(buff, &bit, l1_freq_data, &l1_pr,
                                  &phr_pr_diff, &msg_1012->sats[i].fcn);

    uint8_t amb = getbitu(buff, bit, 8);
    bit += 8;
    l1_freq_data->cnr = 0.25 * getbitu(buff, bit, 8);
    bit += 8;
    l1_freq_data->flags.valid_cnr = 1;

    l1_freq_data->pseudorange = 0.02 * l1_pr + PRUNIT_GLO * amb;
    l1_freq_data->flags.valid_pr = 1;
    int8_t glo_fcn = msg_1012->sats[i].fcn - 7;
    l1_freq_data->carrier_phase =
      (l1_freq_data->pseudorange + 0.0005 * phr_pr_diff) /
      (CLIGHT / (GLO_L1_FREQ + glo_fcn * GLO_L1_CH_OFFSET));
    l1_freq_data->flags.valid_cp = 1;

    rtcm_freq_data *l2_freq_data = &msg_1012->sats[i].obs[L2_FREQ];

    decode_basic_l2_freq_data(buff, &bit, l2_freq_data, &l2_pr,
                                    &phr_pr_diff);

    l2_freq_data->cnr = 0.25 * getbitu(buff, bit, 8);
    bit += 8;
    l2_freq_data->flags.valid_cnr = 1;

    l2_freq_data->pseudorange = 0.02 * l2_pr + l1_freq_data->pseudorange;
    l2_freq_data->flags.valid_pr = 1;
    l2_freq_data->carrier_phase =
      (l1_freq_data->pseudorange + 0.0005 * phr_pr_diff) /
      (CLIGHT / (GLO_L2_FREQ + glo_fcn * GLO_L2_CH_OFFSET));
    l2_freq_data->flags.valid_cp = 1;
  }

  return 0;
}

