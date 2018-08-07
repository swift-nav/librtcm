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

#include "rtcm3_decode.h"
#include "rtcm3_eph_decode.h"
#include <math.h>
#include <stdio.h>
#include "bits.h"

void init_sat_data(rtcm_sat_data *sat_data) {
  for (uint8_t freq = 0; freq < NUM_FREQS; ++freq) {
    sat_data->obs[freq].flags.data = 0;
  }
}

static uint32_t from_lock_ind(uint8_t lock) {
  if (lock < 24) return lock;
  if (lock < 48) return 2 * lock - 24;
  if (lock < 72) return 4 * lock - 120;
  if (lock < 96) return 8 * lock - 408;
  if (lock < 120) return 16 * lock - 1176;
  if (lock < 127) return 32 * lock - 3096;
  return 937;
}

/* Returns the lock time in milliseconds */
/* RTCM 10403.3 Table 3.5-74 */
static uint32_t from_msm_lock_ind(uint8_t lock) {
  if (lock == 0) {
    return 0;
  } else {
    return 32 << lock;
  }
}

/* Returns the lock time in milliseconds */
/* RTCM 10403.3 Table 3.5-75 */
static uint32_t from_msm_lock_ind_ext(uint16_t lock) {
  if (lock < 64) return lock;
  if (lock < 96) return 2 * lock - 64;
  if (lock < 128) return 4 * lock - 256;
  if (lock < 160) return 8 * lock - 768;
  if (lock < 192) return 16 * lock - 2048;
  if (lock < 224) return 32 * lock - 5120;
  if (lock < 256) return 64 * lock - 12288;
  if (lock < 288) return 128 * lock - 28672;
  if (lock < 320) return 256 * lock - 65536;
  if (lock < 352) return 512 * lock - 147456;
  if (lock < 384) return 1024 * lock - 327680;
  if (lock < 416) return 2048 * lock - 720896;
  if (lock < 448) return 4096 * lock - 1572864;
  if (lock < 480) return 8192 * lock - 3407872;
  if (lock < 512) return 16384 * lock - 7340032;
  if (lock < 544) return 32768 * lock - 15728640;
  if (lock < 576) return 65536 * lock - 33554432;
  if (lock < 608) return 131072 * lock - 71303168;
  if (lock < 640) return 262144 * lock - 150994944;
  if (lock < 672) return 524288 * lock - 318767104;
  if (lock < 704) return 1048576 * lock - 671088640;
  return 67108864;
}

void decode_basic_gps_l1_freq_data(const uint8_t buff[],
                                   uint16_t *bit,
                                   rtcm_freq_data *freq_data,
                                   uint32_t *pr,
                                   int32_t *phr_pr_diff) {
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

void decode_basic_glo_l1_freq_data(const uint8_t buff[],
                                   uint16_t *bit,
                                   rtcm_freq_data *freq_data,
                                   uint32_t *pr,
                                   int32_t *phr_pr_diff,
                                   uint8_t *fcn) {
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

void decode_basic_l2_freq_data(const uint8_t buff[],
                               uint16_t *bit,
                               rtcm_freq_data *freq_data,
                               int32_t *pr,
                               int32_t *phr_pr_diff) {
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

uint16_t rtcm3_read_header(const uint8_t buff[], rtcm_obs_header *header) {
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

uint16_t rtcm3_read_glo_header(const uint8_t buff[], rtcm_obs_header *header) {
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

/* unwrap underflowed uint30 value to a wrapped tow_ms value */
static uint32_t normalize_bds2_tow(const uint32_t tow_ms) {
  if (tow_ms >= C_2P30 - BDS_SECOND_TO_GPS_SECOND * 1000) {
    uint32_t negative_tow_ms = C_2P30 - tow_ms;
    return RTCM_MAX_TOW_MS + 1 - negative_tow_ms;
  }
  return tow_ms;
}

static uint16_t rtcm3_read_msm_header(const uint8_t buff[],
                                      const constellation_t cons,
                                      rtcm_msm_header *header) {
  uint16_t bit = 0;
  header->msg_num = getbitu(buff, bit, 12);
  bit += 12;
  header->stn_id = getbitu(buff, bit, 12);
  bit += 12;
  if (CONSTELLATION_GLO == cons) {
    /* skip the day of week, it is handled in gnss_converters */
    bit += 3;
    /* for GLONASS, the epoch time is the time of day in ms */
    header->tow_ms = getbitu(buff, bit, 27);
    bit += 27;
  } else if (CONSTELLATION_BDS2) {
    /* Beidou time can be negative (at least for some Septentrio base stations),
     * so normalize it first */
    header->tow_ms = normalize_bds2_tow(getbitu(buff, bit, 30));
    bit += 30;
  } else {
    /* for other systems, epoch time is the time of week in ms */
    header->tow_ms = getbitu(buff, bit, 30);
    bit += 30;
  }
  header->multiple = getbitu(buff, bit, 1);
  bit += 1;
  header->iods = getbitu(buff, bit, 3);
  bit += 3;
  header->reserved = getbitu(buff, bit, 7);
  bit += 7;
  header->steering = getbitu(buff, bit, 2);
  bit += 2;
  header->ext_clock = getbitu(buff, bit, 2);
  bit += 2;
  header->div_free = getbitu(buff, bit, 1);
  bit += 1;
  header->smooth = getbitu(buff, bit, 3);
  bit += 3;

  for (uint8_t i = 0; i < MSM_SATELLITE_MASK_SIZE; i++) {
    header->satellite_mask[i] = getbitu(buff, bit, 1);
    bit++;
  }
  for (uint8_t i = 0; i < MSM_SIGNAL_MASK_SIZE; i++) {
    header->signal_mask[i] = getbitu(buff, bit, 1);
    bit++;
  }
  uint8_t num_sats =
      count_mask_values(MSM_SATELLITE_MASK_SIZE, header->satellite_mask);
  uint8_t num_sigs =
      count_mask_values(MSM_SIGNAL_MASK_SIZE, header->signal_mask);
  uint8_t cell_mask_size = num_sats * num_sigs;

  for (uint8_t i = 0; i < cell_mask_size; i++) {
    header->cell_mask[i] = getbitu(buff, bit, 1);
    bit++;
  }
  return bit;
}

uint8_t construct_L1_code(rtcm_freq_data *l1_freq_data,
                          int32_t pr,
                          double amb_correction) {
  l1_freq_data->pseudorange = 0.02 * pr + amb_correction;
  if (pr != (int)PR_L1_INVALID) {
    return 1;
  }
  return 0;
}

uint8_t construct_L1_phase(rtcm_freq_data *l1_freq_data,
                           int32_t phr_pr_diff,
                           double freq) {
  l1_freq_data->carrier_phase =
      (l1_freq_data->pseudorange + 0.0005 * phr_pr_diff) / (GPS_C / freq);
  if (phr_pr_diff != (int)CP_INVALID) {
    return 1;
  }
  return 0;
}

uint8_t construct_L2_code(rtcm_freq_data *l2_freq_data,
                          const rtcm_freq_data *l1_freq_data,
                          int32_t pr) {
  l2_freq_data->pseudorange = 0.02 * pr + l1_freq_data->pseudorange;
  if (pr != (int)PR_L2_INVALID) {
    return 1;
  }
  return 0;
}

uint8_t construct_L2_phase(rtcm_freq_data *l2_freq_data,
                           const rtcm_freq_data *l1_freq_data,
                           int32_t phr_pr_diff,
                           double freq) {
  l2_freq_data->carrier_phase =
      (l1_freq_data->pseudorange + 0.0005 * phr_pr_diff) / (GPS_C / freq);
  if (phr_pr_diff != (int)CP_INVALID) {
    return 1;
  }
  return 0;
}

uint8_t get_cnr(rtcm_freq_data *freq_data,
                const uint8_t buff[],
                uint16_t *bit) {
  uint8_t cnr = getbitu(buff, *bit, 8);
  *bit += 8;
  if (cnr == 0) {
    return 0;
  }
  freq_data->cnr = 0.25 * cnr;
  return 1;
}

/** Decode an RTCMv3 message type 1001 (L1-Only GPS RTK Observables)
 *
 * \param buff The input data buffer
 * \param RTCM message struct
 * \return  - RC_OK : Success
 *          - RC_MESSAGE_TYPE_MISMATCH : Message type mismatch
 *          - RC_INVALID_MESSAGE : TOW sanity check fail
 */
rtcm3_rc rtcm3_decode_1001(const uint8_t buff[], rtcm_obs_message *msg_1001) {
  uint16_t bit = 0;
  bit += rtcm3_read_header(buff, &msg_1001->header);

  if (msg_1001->header.msg_num != 1001) /* Unexpected message type. */
    return RC_MESSAGE_TYPE_MISMATCH;

  if (msg_1001->header.tow_ms > RTCM_MAX_TOW_MS) {
    return RC_INVALID_MESSAGE;
  }

  for (uint8_t i = 0; i < msg_1001->header.n_sat; i++) {
    init_sat_data(&msg_1001->sats[i]);

    msg_1001->sats[i].svId = getbitu(buff, bit, 6);
    bit += 6;

    rtcm_freq_data *l1_freq_data = &msg_1001->sats[i].obs[L1_FREQ];

    uint32_t l1_pr;
    int32_t phr_pr_diff;
    decode_basic_gps_l1_freq_data(
        buff, &bit, l1_freq_data, &l1_pr, &phr_pr_diff);

    l1_freq_data->flags.valid_pr = construct_L1_code(l1_freq_data, l1_pr, 0);
    l1_freq_data->flags.valid_cp =
        construct_L1_phase(l1_freq_data, phr_pr_diff, GPS_L1_HZ);
  }

  return RC_OK;
}

/** Decode an RTCMv3 message type 1002 (Extended L1-Only GPS RTK Observables)
 *
 * \param buff The input data buffer
 * \param RTCM message struct
 * \return  - RC_OK : Success
 *          - RC_MESSAGE_TYPE_MISMATCH : Message type mismatch
 *          - RC_INVALID_MESSAGE : TOW sanity check fail
 */
rtcm3_rc rtcm3_decode_1002(const uint8_t buff[], rtcm_obs_message *msg_1002) {
  uint16_t bit = 0;
  bit += rtcm3_read_header(buff, &msg_1002->header);

  if (msg_1002->header.msg_num != 1002) /* Unexpected message type. */
    return RC_MESSAGE_TYPE_MISMATCH;

  if (msg_1002->header.tow_ms > RTCM_MAX_TOW_MS) {
    return RC_INVALID_MESSAGE;
  }

  for (uint8_t i = 0; i < msg_1002->header.n_sat; i++) {
    init_sat_data(&msg_1002->sats[i]);

    msg_1002->sats[i].svId = getbitu(buff, bit, 6);
    bit += 6;

    rtcm_freq_data *l1_freq_data = &msg_1002->sats[i].obs[L1_FREQ];

    uint32_t l1_pr;
    int32_t phr_pr_diff;
    decode_basic_gps_l1_freq_data(
        buff, &bit, l1_freq_data, &l1_pr, &phr_pr_diff);

    uint8_t amb = getbitu(buff, bit, 8);
    bit += 8;
    l1_freq_data->flags.valid_cnr = get_cnr(l1_freq_data, buff, &bit);
    l1_freq_data->flags.valid_pr =
        construct_L1_code(l1_freq_data, l1_pr, amb * PRUNIT_GPS);
    l1_freq_data->flags.valid_cp =
        construct_L1_phase(l1_freq_data, phr_pr_diff, GPS_L1_HZ);
  }

  return RC_OK;
}

/** Decode an RTCMv3 message type 1003 (L1/L2 GPS RTK Observables)
 *
 * \param buff The input data buffer
 * \param RTCM message struct
 * \return  - RC_OK : Success
 *          - RC_MESSAGE_TYPE_MISMATCH : Message type mismatch
 *          - RC_INVALID_MESSAGE : TOW sanity check fail
 */
rtcm3_rc rtcm3_decode_1003(const uint8_t buff[], rtcm_obs_message *msg_1003) {
  uint16_t bit = 0;
  bit += rtcm3_read_header(buff, &msg_1003->header);

  if (msg_1003->header.msg_num != 1003) /* Unexpected message type. */
    return RC_MESSAGE_TYPE_MISMATCH;

  if (msg_1003->header.tow_ms > RTCM_MAX_TOW_MS) {
    return RC_INVALID_MESSAGE;
  }

  for (uint8_t i = 0; i < msg_1003->header.n_sat; i++) {
    init_sat_data(&msg_1003->sats[i]);

    msg_1003->sats[i].svId = getbitu(buff, bit, 6);
    bit += 6;

    rtcm_freq_data *l1_freq_data = &msg_1003->sats[i].obs[L1_FREQ];

    uint32_t l1_pr;
    int32_t l2_pr;
    int32_t phr_pr_diff;
    decode_basic_gps_l1_freq_data(
        buff, &bit, l1_freq_data, &l1_pr, &phr_pr_diff);

    l1_freq_data->flags.valid_pr = construct_L1_code(l1_freq_data, l1_pr, 0);
    l1_freq_data->flags.valid_cp =
        construct_L1_phase(l1_freq_data, phr_pr_diff, GPS_L1_HZ);

    rtcm_freq_data *l2_freq_data = &msg_1003->sats[i].obs[L2_FREQ];

    decode_basic_l2_freq_data(buff, &bit, l2_freq_data, &l2_pr, &phr_pr_diff);

    l2_freq_data->flags.valid_pr =
        construct_L2_code(l2_freq_data, l1_freq_data, l2_pr);
    l2_freq_data->flags.valid_cp =
        construct_L2_phase(l2_freq_data, l1_freq_data, phr_pr_diff, GPS_L2_HZ);
  }

  return RC_OK;
}

/** Decode an RTCMv3 message type 1004 (Extended L1/L2 GPS RTK Observables)
 *
 * \param buff The input data buffer
 * \param RTCM message struct
 * \return  - RC_OK : Success
 *          - RC_MESSAGE_TYPE_MISMATCH : Message type mismatch
 *          - RC_INVALID_MESSAGE : TOW sanity check fail
 */
rtcm3_rc rtcm3_decode_1004(const uint8_t buff[], rtcm_obs_message *msg_1004) {
  uint16_t bit = 0;
  bit += rtcm3_read_header(buff, &msg_1004->header);

  if (msg_1004->header.msg_num != 1004) /* Unexpected message type. */
    return RC_MESSAGE_TYPE_MISMATCH;

  if (msg_1004->header.tow_ms > RTCM_MAX_TOW_MS) {
    return RC_INVALID_MESSAGE;
  }

  for (uint8_t i = 0; i < msg_1004->header.n_sat; i++) {
    init_sat_data(&msg_1004->sats[i]);

    msg_1004->sats[i].svId = getbitu(buff, bit, 6);
    bit += 6;

    rtcm_freq_data *l1_freq_data = &msg_1004->sats[i].obs[L1_FREQ];

    uint32_t l1_pr;
    int32_t l2_pr;
    int32_t phr_pr_diff;
    decode_basic_gps_l1_freq_data(
        buff, &bit, l1_freq_data, &l1_pr, &phr_pr_diff);

    uint8_t amb = getbitu(buff, bit, 8);
    bit += 8;

    l1_freq_data->flags.valid_cnr = get_cnr(l1_freq_data, buff, &bit);
    l1_freq_data->flags.valid_pr =
        construct_L1_code(l1_freq_data, l1_pr, amb * PRUNIT_GPS);
    l1_freq_data->flags.valid_cp =
        construct_L1_phase(l1_freq_data, phr_pr_diff, GPS_L1_HZ);

    rtcm_freq_data *l2_freq_data = &msg_1004->sats[i].obs[L2_FREQ];

    decode_basic_l2_freq_data(buff, &bit, l2_freq_data, &l2_pr, &phr_pr_diff);

    l2_freq_data->flags.valid_cnr = get_cnr(l2_freq_data, buff, &bit);
    l2_freq_data->flags.valid_pr =
        construct_L2_code(l2_freq_data, l1_freq_data, l2_pr);
    l2_freq_data->flags.valid_cp =
        construct_L2_phase(l2_freq_data, l1_freq_data, phr_pr_diff, GPS_L2_HZ);
  }

  return RC_OK;
}

rtcm3_rc rtcm3_decode_1005_base(const uint8_t buff[],
                                rtcm_msg_1005 *msg_1005,
                                uint16_t *bit) {
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

  return RC_OK;
}

/** Decode an RTCMv3 message type 1005 (Stationary RTK Reference Station ARP)
 *
 * \param buff The input data buffer
 * \param RTCM message struct
 * \return  - RC_OK : Success
 *          - RC_MESSAGE_TYPE_MISMATCH : Message type mismatch
 */
rtcm3_rc rtcm3_decode_1005(const uint8_t buff[], rtcm_msg_1005 *msg_1005) {
  uint16_t bit = 0;
  uint16_t msg_num = getbitu(buff, bit, 12);
  bit += 12;

  if (msg_num != 1005) /* Unexpected message type. */
    return RC_MESSAGE_TYPE_MISMATCH;

  return rtcm3_decode_1005_base(buff, msg_1005, &bit);
}

/** Decode an RTCMv3 message type 1005 (Stationary RTK Reference Station ARP
 * with antenna height)
 *
 * \param buff The input data buffer
 * \param RTCM message struct
 * \return  - RC_OK : Success
 *          - RC_MESSAGE_TYPE_MISMATCH : Message type mismatch
 */
rtcm3_rc rtcm3_decode_1006(const uint8_t buff[], rtcm_msg_1006 *msg_1006) {
  uint16_t bit = 0;
  uint16_t msg_num = getbitu(buff, bit, 12);
  bit += 12;

  if (msg_num != 1006) /* Unexpected message type. */
    return RC_MESSAGE_TYPE_MISMATCH;

  rtcm3_decode_1005_base(buff, &msg_1006->msg_1005, &bit);
  msg_1006->ant_height = (double)(getbitu(buff, bit, 16)) / 10000.0;
  bit += 16;
  return RC_OK;
}

rtcm3_rc rtcm3_decode_1007_base(const uint8_t buff[],
                                rtcm_msg_1007 *msg_1007,
                                uint16_t *bit) {
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

  return RC_OK;
}

/** Decode an RTCMv3 message type 1007 (Antenna Descriptor)
 *
 * \param buff The input data buffer
 * \param RTCM message struct
 * \return  - RC_OK : Success
 *          - RC_MESSAGE_TYPE_MISMATCH : Message type mismatch
 */
rtcm3_rc rtcm3_decode_1007(const uint8_t buff[], rtcm_msg_1007 *msg_1007) {
  uint16_t bit = 0;
  uint16_t msg_num = getbitu(buff, bit, 12);
  bit += 12;

  if (msg_num != 1007) /* Unexpected message type. */
    return RC_MESSAGE_TYPE_MISMATCH;

  rtcm3_decode_1007_base(buff, msg_1007, &bit);

  return RC_OK;
}

/** Decode an RTCMv3 message type 1008 (Antenna Descriptor & Serial Number)
 *
 * \param buff The input data buffer
 * \param RTCM message struct
 * \return  - RC_OK : Success
 *          - RC_MESSAGE_TYPE_MISMATCH : Message type mismatch
 */
rtcm3_rc rtcm3_decode_1008(const uint8_t buff[], rtcm_msg_1008 *msg_1008) {
  uint16_t bit = 0;
  uint16_t msg_num = getbitu(buff, bit, 12);
  bit += 12;

  if (msg_num != 1008) /* Unexpected message type. */
    return RC_MESSAGE_TYPE_MISMATCH;

  rtcm3_decode_1007_base(buff, &msg_1008->msg_1007, &bit);
  msg_1008->serial_count = getbitu(buff, bit, 8);
  bit += 8;
  for (uint8_t i = 0; i < msg_1008->serial_count; ++i) {
    msg_1008->serial_num[i] = getbitu(buff, bit, 8);
    bit += 8;
  }
  return RC_OK;
}

/** Decode an RTCMv3 message type 1010 (Extended L1-Only GLO RTK Observables)
 *
 * \param buff The input data buffer
 * \param RTCM message struct
 * \return  - RC_OK : Success
 *          - RC_MESSAGE_TYPE_MISMATCH : Message type mismatch
 *          - RC_INVALID_MESSAGE : TOW sanity check fail
 */
rtcm3_rc rtcm3_decode_1010(const uint8_t buff[], rtcm_obs_message *msg_1010) {
  uint16_t bit = 0;
  bit += rtcm3_read_glo_header(buff, &msg_1010->header);

  if (msg_1010->header.msg_num != 1010) /* Unexpected message type. */
    return RC_MESSAGE_TYPE_MISMATCH;

  if (msg_1010->header.tow_ms > RTCM_GLO_MAX_TOW_MS) {
    return RC_INVALID_MESSAGE;
  }

  for (uint8_t i = 0; i < msg_1010->header.n_sat; i++) {
    init_sat_data(&msg_1010->sats[i]);

    msg_1010->sats[i].svId = getbitu(buff, bit, 6);
    bit += 6;

    rtcm_freq_data *l1_freq_data = &msg_1010->sats[i].obs[L1_FREQ];

    uint32_t l1_pr;
    int32_t phr_pr_diff;
    decode_basic_glo_l1_freq_data(
        buff, &bit, l1_freq_data, &l1_pr, &phr_pr_diff, &msg_1010->sats[i].fcn);

    uint8_t amb = getbitu(buff, bit, 7);
    bit += 7;

    l1_freq_data->flags.valid_cnr = get_cnr(l1_freq_data, buff, &bit);

    int8_t glo_fcn = msg_1010->sats[i].fcn - MT1012_GLO_FCN_OFFSET;
    l1_freq_data->flags.valid_pr =
        construct_L1_code(l1_freq_data, l1_pr, PRUNIT_GLO * amb);
    l1_freq_data->flags.valid_cp =
        (msg_1010->sats[i].fcn <= MT1012_GLO_MAX_FCN) &&
        construct_L1_phase(
            l1_freq_data, phr_pr_diff, GLO_L1_HZ + glo_fcn * GLO_L1_DELTA_HZ);
  }

  return RC_OK;
}

/** Decode an RTCMv3 message type 1012 (Extended L1/L2 GLO RTK Observables)
 *
 * \param buff The input data buffer
 * \param RTCM message struct
 * \return  - RC_OK : Success
 *          - RC_MESSAGE_TYPE_MISMATCH : Message type mismatch
 *          - RC_INVALID_MESSAGE : TOW sanity check fail
 */
rtcm3_rc rtcm3_decode_1012(const uint8_t buff[], rtcm_obs_message *msg_1012) {
  uint16_t bit = 0;
  bit += rtcm3_read_glo_header(buff, &msg_1012->header);

  if (msg_1012->header.msg_num != 1012) /* Unexpected message type. */
    return RC_MESSAGE_TYPE_MISMATCH;

  if (msg_1012->header.tow_ms > RTCM_GLO_MAX_TOW_MS) {
    return RC_INVALID_MESSAGE;
  }

  for (uint8_t i = 0; i < msg_1012->header.n_sat; i++) {
    init_sat_data(&msg_1012->sats[i]);

    msg_1012->sats[i].svId = getbitu(buff, bit, 6);
    bit += 6;

    rtcm_freq_data *l1_freq_data = &msg_1012->sats[i].obs[L1_FREQ];

    uint32_t l1_pr;
    int32_t l2_pr;
    int32_t phr_pr_diff;
    decode_basic_glo_l1_freq_data(
        buff, &bit, l1_freq_data, &l1_pr, &phr_pr_diff, &msg_1012->sats[i].fcn);

    uint8_t amb = getbitu(buff, bit, 7);
    bit += 7;

    int8_t glo_fcn = msg_1012->sats[i].fcn - MT1012_GLO_FCN_OFFSET;
    l1_freq_data->flags.valid_cnr = get_cnr(l1_freq_data, buff, &bit);
    l1_freq_data->flags.valid_pr =
        construct_L1_code(l1_freq_data, l1_pr, amb * PRUNIT_GLO);
    l1_freq_data->flags.valid_cp =
        (msg_1012->sats[i].fcn <= MT1012_GLO_MAX_FCN) &&
        construct_L1_phase(
            l1_freq_data, phr_pr_diff, GLO_L1_HZ + glo_fcn * GLO_L1_DELTA_HZ);

    rtcm_freq_data *l2_freq_data = &msg_1012->sats[i].obs[L2_FREQ];

    decode_basic_l2_freq_data(buff, &bit, l2_freq_data, &l2_pr, &phr_pr_diff);

    l2_freq_data->flags.valid_cnr = get_cnr(l2_freq_data, buff, &bit);
    l2_freq_data->flags.valid_pr =
        construct_L2_code(l2_freq_data, l1_freq_data, l2_pr);
    l2_freq_data->flags.valid_cp =
        construct_L2_phase(l2_freq_data,
                           l1_freq_data,
                           phr_pr_diff,
                           GLO_L2_HZ + glo_fcn * GLO_L2_DELTA_HZ);
  }

  return RC_OK;
}

/** Decode an RTCMv3 message type 1029 (Unicode Text String Message)
 *
 * \param buff The input data buffer
 * \param RTCM message struct
 * \return  - RC_OK : Success
 *          - RC_MESSAGE_TYPE_MISMATCH : Message type mismatch
 */
rtcm3_rc rtcm3_decode_1029(const uint8_t buff[], rtcm_msg_1029 *msg_1029) {
  uint16_t bit = 0;
  uint16_t msg_num = getbitu(buff, bit, 12);
  bit += 12;

  if (msg_num != 1029) /* Unexpected message type. */
    return RC_MESSAGE_TYPE_MISMATCH;

  msg_1029->stn_id = getbitu(buff, bit, 12);
  bit += 12;

  msg_1029->mjd_num = getbitu(buff, bit, 16);
  bit += 16;

  msg_1029->utc_sec_of_day = getbitu(buff, bit, 17);
  bit += 17;

  msg_1029->unicode_chars = getbitu(buff, bit, 7);
  bit += 7;

  msg_1029->utf8_code_units_n = getbitu(buff, bit, 8);
  bit += 8;
  for (uint8_t i = 0; i < msg_1029->utf8_code_units_n; ++i) {
    msg_1029->utf8_code_units[i] = getbitu(buff, bit, 8);
    bit += 8;
  }

  return RC_OK;
}

/** Decode an RTCMv3 message type 1033 (Rcv and Ant descriptor)
 *
 * \param buff The input data buffer
 * \param RTCM message struct
 * \return  - RC_OK : Success
 *          - RC_MESSAGE_TYPE_MISMATCH : Message type mismatch
 */
rtcm3_rc rtcm3_decode_1033(const uint8_t buff[], rtcm_msg_1033 *msg_1033) {
  uint16_t bit = 0;
  uint16_t msg_num = getbitu(buff, bit, 12);
  bit += 12;

  if (msg_num != 1033) /* Unexpected message type. */
    return RC_MESSAGE_TYPE_MISMATCH;

  msg_1033->stn_id = getbitu(buff, bit, 12);
  bit += 12;

  msg_1033->antenna_desc_counter = getbitu(buff, bit, 8);
  bit += 8;
  for (uint8_t i = 0; i < msg_1033->antenna_desc_counter; ++i) {
    msg_1033->antenna_descriptor[i] = getbitu(buff, bit, 8);
    bit += 8;
  }

  msg_1033->antenna_setup_ID = getbitu(buff, bit, 8);
  bit += 8;

  msg_1033->antenna_serial_num_counter = getbitu(buff, bit, 8);
  bit += 8;
  for (uint8_t i = 0; i < msg_1033->antenna_serial_num_counter; ++i) {
    msg_1033->antenna_serial_num[i] = getbitu(buff, bit, 8);
    bit += 8;
  }

  msg_1033->rcv_descriptor_counter = getbitu(buff, bit, 8);
  bit += 8;
  for (uint8_t i = 0; i < msg_1033->rcv_descriptor_counter; ++i) {
    msg_1033->rcv_descriptor[i] = getbitu(buff, bit, 8);
    bit += 8;
  }

  msg_1033->rcv_fw_counter = getbitu(buff, bit, 8);
  bit += 8;
  for (uint8_t i = 0; i < msg_1033->rcv_fw_counter; ++i) {
    msg_1033->rcv_fw_version[i] = getbitu(buff, bit, 8);
    bit += 8;
  }

  msg_1033->rcv_serial_num_counter = getbitu(buff, bit, 8);
  bit += 8;
  for (uint8_t i = 0; i < msg_1033->rcv_serial_num_counter; ++i) {
    msg_1033->rcv_serial_num[i] = getbitu(buff, bit, 8);
    bit += 8;
  }

  return RC_OK;
}

/** Decode an RTCMv3 message type 1230 (Code-Phase Bias Message)
 *
 * \param buff The input data buffer
 * \param RTCM message struct
 * \return  - RC_OK : Success
 *          - RC_MESSAGE_TYPE_MISMATCH : Message type mismatch
 */
rtcm3_rc rtcm3_decode_1230(const uint8_t buff[], rtcm_msg_1230 *msg_1230) {
  uint16_t bit = 0;
  uint16_t msg_num = getbitu(buff, bit, 12);
  bit += 12;

  if (msg_num != 1230) /* Unexpected message type. */
    return RC_MESSAGE_TYPE_MISMATCH;

  msg_1230->stn_id = getbitu(buff, bit, 12);
  bit += 12;
  msg_1230->bias_indicator = getbitu(buff, bit, 1);
  bit += 1;
  /* 3 Reserved bits */
  bit += 3;
  msg_1230->fdma_signal_mask = getbitu(buff, bit, 4);
  bit += 4;
  if (msg_1230->fdma_signal_mask & 0x08) {
    msg_1230->L1_CA_cpb_meter = getbits(buff, bit, 16) * 0.02;
    bit += 16;
  }
  if (msg_1230->fdma_signal_mask & 0x04) {
    msg_1230->L1_P_cpb_meter = getbits(buff, bit, 16) * 0.02;
    bit += 16;
  }
  if (msg_1230->fdma_signal_mask & 0x02) {
    msg_1230->L2_CA_cpb_meter = getbits(buff, bit, 16) * 0.02;
    bit += 16;
  }
  if (msg_1230->fdma_signal_mask & 0x01) {
    msg_1230->L2_P_cpb_meter = getbits(buff, bit, 16) * 0.02;
    bit += 16;
  }

  return RC_OK;
}

static void decode_msm_sat_data(const uint8_t buff[],
                                const uint8_t num_sats,
                                const msm_enum msm_type,
                                double rough_range[],
                                bool rough_range_valid[],
                                uint8_t sat_info[],
                                bool sat_info_valid[],
                                double rough_rate[],
                                bool rough_rate_valid[],
                                uint16_t *bit) {
  /* number of integer milliseconds, DF397 */
  for (uint8_t i = 0; i < num_sats; i++) {
    uint32_t range_ms = getbitu(buff, *bit, 8);
    *bit += 8;
    rough_range[i] = (double)range_ms * PRUNIT_GPS;
    rough_range_valid[i] = (MSM_ROUGH_RANGE_INVALID != range_ms);
  }

  /* satellite info (constellation-dependent)*/
  for (uint8_t i = 0; i < num_sats; i++) {
    if (MSM5 == msm_type || MSM7 == msm_type) {
      sat_info[i] = getbitu(buff, *bit, 4);
      *bit += 4;
      sat_info_valid[i] = true;
    } else {
      sat_info[i] = 0;
      sat_info_valid[i] = false;
    }
  }

  /* rough range modulo 1 ms, DF398 */
  for (uint8_t i = 0; i < num_sats; i++) {
    uint32_t rough_pr = getbitu(buff, *bit, 10);
    *bit += 10;
    if (rough_range_valid[i]) {
      rough_range[i] += (double)rough_pr / 1024 * PRUNIT_GPS;
    }
  }

  /* range rate, m/s, DF399*/
  for (uint8_t i = 0; i < num_sats; i++) {
    if (MSM5 == msm_type || MSM7 == msm_type) {
      int16_t rate = getbits(buff, *bit, 14);
      *bit += 14;
      rough_rate[i] = (double)rate;
      rough_rate_valid[i] = (MSM_ROUGH_RANGE_INVALID != rate);
    } else {
      rough_rate[i] = 0;
      rough_rate_valid[i] = false;
    }
  }
}

static void decode_msm_fine_pseudoranges(const uint8_t buff[],
                                         const uint8_t num_cells,
                                         double fine_pr[],
                                         flag_bf flags[],
                                         uint16_t *bit) {
  /* DF400 */
  for (uint16_t i = 0; i < num_cells; i++) {
    int16_t decoded = (int16_t)getbits(buff, *bit, 15);
    *bit += 15;
    flags[i].valid_pr = (decoded != MSM_PR_INVALID);
    fine_pr[i] = (double)decoded * C_1_2P24 * PRUNIT_GPS;
  }
}

static void decode_msm_fine_pseudoranges_extended(const uint8_t buff[],
                                                  const uint8_t num_cells,
                                                  double fine_pr[],
                                                  flag_bf flags[],
                                                  uint16_t *bit) {
  /* DF405 */
  for (uint16_t i = 0; i < num_cells; i++) {
    int32_t decoded = (int32_t)getbitsl(buff, *bit, 20);
    *bit += 20;
    flags[i].valid_pr = (decoded != MSM_PR_EXT_INVALID);
    fine_pr[i] = (double)decoded * C_1_2P29 * PRUNIT_GPS;
  }
}

static void decode_msm_fine_phaseranges(const uint8_t buff[],
                                        const uint8_t num_cells,
                                        double fine_cp[],
                                        flag_bf flags[],
                                        uint16_t *bit) {
  /* DF401 */
  for (uint16_t i = 0; i < num_cells; i++) {
    int32_t decoded = getbits(buff, *bit, 22);
    *bit += 22;
    flags[i].valid_cp = (decoded != MSM_CP_INVALID);
    fine_cp[i] = (double)decoded * C_1_2P29 * PRUNIT_GPS;
  }
}

static void decode_msm_fine_phaseranges_extended(const uint8_t buff[],
                                                 const uint8_t num_cells,
                                                 double fine_cp[],
                                                 flag_bf flags[],
                                                 uint16_t *bit) {
  /* DF406 */
  for (uint16_t i = 0; i < num_cells; i++) {
    int32_t decoded = getbits(buff, *bit, 24);
    *bit += 24;
    flags[i].valid_cp = (decoded != MSM_CP_EXT_INVALID);
    fine_cp[i] = (double)decoded * C_1_2P31 * PRUNIT_GPS;
  }
}

static void decode_msm_lock_times(const uint8_t buff[],
                                  const uint8_t num_cells,
                                  double lock_time[],
                                  flag_bf flags[],
                                  uint16_t *bit) {
  /* DF402 */
  for (uint16_t i = 0; i < num_cells; i++) {
    uint32_t lock_ind = getbitu(buff, *bit, 4);
    *bit += 4;
    lock_time[i] = (double)from_msm_lock_ind(lock_ind) / 1000;
    flags[i].valid_lock = 1;
  }
}

static void decode_msm_lock_times_extended(const uint8_t buff[],
                                           const uint8_t num_cells,
                                           double lock_time[],
                                           flag_bf flags[],
                                           uint16_t *bit) {
  /* DF407 */
  for (uint16_t i = 0; i < num_cells; i++) {
    uint16_t lock_ind = getbitu(buff, *bit, 10);
    *bit += 10;
    lock_time[i] = (double)from_msm_lock_ind_ext(lock_ind) / 1000;
    flags[i].valid_lock = 1;
  }
}

static void decode_msm_hca_indicators(const uint8_t buff[],
                                      const uint8_t num_cells,
                                      bool hca_indicator[],
                                      uint16_t *bit) {
  /* DF420 */
  for (uint16_t i = 0; i < num_cells; i++) {
    hca_indicator[i] = (bool)getbitu(buff, *bit, 1);
    *bit += 1;
  }
}

static void decode_msm_cnrs(const uint8_t buff[],
                            const uint8_t num_cells,
                            double cnr[],
                            flag_bf flags[],
                            uint16_t *bit) {
  /* DF403 */
  for (uint16_t i = 0; i < num_cells; i++) {
    uint32_t decoded = getbitu(buff, *bit, 6);
    *bit += 6;
    flags[i].valid_cnr = (decoded != 0);
    cnr[i] = (double)decoded;
  }
}

static void decode_msm_cnrs_extended(const uint8_t buff[],
                                     const uint8_t num_cells,
                                     double cnr[],
                                     flag_bf flags[],
                                     uint16_t *bit) {
  /* DF408 */
  for (uint16_t i = 0; i < num_cells; i++) {
    uint32_t decoded = getbitu(buff, *bit, 10);
    *bit += 10;
    flags[i].valid_cnr = (decoded != 0);
    cnr[i] = (double)decoded * C_1_2P4;
  }
}

static void decode_msm_fine_phaserangerates(const uint8_t buff[],
                                            const uint8_t num_cells,
                                            double *fine_dop,
                                            flag_bf *flags,
                                            uint16_t *bit) {
  /* DF404 */
  for (uint16_t i = 0; i < num_cells; i++) {
    int32_t decoded = getbits(buff, *bit, 15);
    *bit += 15;
    fine_dop[i] = (double)decoded * 0.0001;
    flags[i].valid_dop = (decoded != MSM_DOP_INVALID);
  }
}

/** Decode an RTCMv3 Multi System Messages 4-7
 *
 * \param buff The input data buffer
 * \param msm_type MSM4, MSM5, MSM6 or MSM7
 * \param glo_sv_id_fcn_map Optional GLO FCN table (size MAX_GLO_PRN + 1)
 * \param msg The parsed RTCM message struct
 * \return  - RC_OK : Success
 *          - RC_MESSAGE_TYPE_MISMATCH : Message type mismatch
 *          - RC_INVALID_MESSAGE : Cell mask too large or invalid TOW
 */
static rtcm3_rc rtcm3_decode_msm_internal(const uint8_t buff[],
                                          const uint16_t msm_type,
                                          const uint8_t glo_sv_id_fcn_map[],
                                          rtcm_msm_message *msg) {
  if (MSM4 != msm_type && MSM5 != msm_type && MSM6 != msm_type &&
      MSM7 != msm_type) {
    /* Invalid message type requested */
    return RC_MESSAGE_TYPE_MISMATCH;
  }

  msg->header.msg_num = getbitu(buff, 0, 12);

  if (msm_type != to_msm_type(msg->header.msg_num)) {
    /* Message number does not match the requested message type */
    return RC_MESSAGE_TYPE_MISMATCH;
  }

  constellation_t cons = to_constellation(msg->header.msg_num);
  if (CONSTELLATION_INVALID == cons) {
    /* Unexpected message type. */
    return RC_MESSAGE_TYPE_MISMATCH;
  }

  uint16_t bit = 0;
  bit += rtcm3_read_msm_header(buff, cons, &msg->header);

  if (CONSTELLATION_GLO != cons) {
    if (msg->header.tow_ms > RTCM_MAX_TOW_MS) {
      return RC_INVALID_MESSAGE;
    }
  } else if (msg->header.tow_ms > RTCM_GLO_MAX_TOW_MS) { /* GLO */
    return RC_INVALID_MESSAGE;
  }

  uint8_t num_sats =
      count_mask_values(MSM_SATELLITE_MASK_SIZE, msg->header.satellite_mask);
  uint8_t num_sigs =
      count_mask_values(MSM_SIGNAL_MASK_SIZE, msg->header.signal_mask);

  if (num_sats * num_sigs > MSM_MAX_CELLS) {
    /* Too large cell mask, most probably a parsing error */
    return RC_INVALID_MESSAGE;
  }

  uint8_t cell_mask_size = num_sats * num_sigs;
  uint8_t num_cells = count_mask_values(cell_mask_size, msg->header.cell_mask);

  /* Satellite Data */

  double rough_range[num_sats];
  double rough_rate[num_sats];
  uint8_t sat_info[num_sats];
  bool rough_range_valid[num_sats];
  bool rough_rate_valid[num_sats];
  bool sat_info_valid[num_sats];

  decode_msm_sat_data(buff,
                      num_sats,
                      msm_type,
                      rough_range,
                      rough_range_valid,
                      sat_info,
                      sat_info_valid,
                      rough_rate,
                      rough_rate_valid,
                      &bit);

  /* Signal Data */

  double fine_pr[num_cells];
  double fine_cp[num_cells];
  double lock_time[num_cells];
  bool hca_indicator[num_cells];
  double cnr[num_cells];
  double fine_dop[num_cells];
  flag_bf flags[num_cells];

  for (uint8_t i = 0; i < num_cells; i++) {
    flags[i].data = 0;
  }

  if (MSM4 == msm_type || MSM5 == msm_type) {
    decode_msm_fine_pseudoranges(buff, num_cells, fine_pr, flags, &bit);
    decode_msm_fine_phaseranges(buff, num_cells, fine_cp, flags, &bit);
    decode_msm_lock_times(buff, num_cells, lock_time, flags, &bit);
  } else if (MSM6 == msm_type || MSM7 == msm_type) {
    decode_msm_fine_pseudoranges_extended(
        buff, num_cells, fine_pr, flags, &bit);
    decode_msm_fine_phaseranges_extended(buff, num_cells, fine_cp, flags, &bit);
    decode_msm_lock_times_extended(buff, num_cells, lock_time, flags, &bit);
  } else {
    return RC_MESSAGE_TYPE_MISMATCH;
  }

  decode_msm_hca_indicators(buff, num_cells, hca_indicator, &bit);

  if (MSM4 == msm_type || MSM5 == msm_type) {
    decode_msm_cnrs(buff, num_cells, cnr, flags, &bit);
  } else if (MSM6 == msm_type || MSM7 == msm_type) {
    decode_msm_cnrs_extended(buff, num_cells, cnr, flags, &bit);
  } else {
    return RC_MESSAGE_TYPE_MISMATCH;
  }

  if (MSM5 == msm_type || MSM7 == msm_type) {
    decode_msm_fine_phaserangerates(buff, num_cells, fine_dop, flags, &bit);
  }

  uint8_t i = 0;
  for (uint8_t sat = 0; sat < num_sats; sat++) {
    msg->sats[sat].rough_range_m = rough_range[sat];
    msg->sats[sat].sat_info = sat_info[sat];
    msg->sats[sat].rough_range_rate_m_s = rough_rate[sat];

    for (uint8_t sig = 0; sig < num_sigs; sig++) {
      if (msg->header.cell_mask[sat * num_sigs + sig]) {
        /* get GLO FCN */
        uint8_t glo_fcn = MSM_GLO_FCN_UNKNOWN;
        bool glo_fcn_valid = get_glo_fcn(&msg->header,
                                         sat,
                                         sat_info,
                                         sat_info_valid,
                                         glo_sv_id_fcn_map,
                                         &glo_fcn);

        double freq;
        bool freq_valid = msm_signal_frequency(
            &msg->header, sig, glo_fcn, glo_fcn_valid, &freq);

        if (rough_range_valid[sat] && flags[i].valid_pr) {
          msg->signals[i].pseudorange_m = rough_range[sat] + fine_pr[i];
        } else {
          msg->signals[i].pseudorange_m = 0;
          flags[i].valid_pr = false;
        }
        if (rough_range_valid[sat] && flags[i].valid_cp && freq_valid) {
          /* convert carrier phase into cycles */
          msg->signals[i].carrier_phase_cyc =
              (rough_range[sat] + fine_cp[i]) * (freq / GPS_C);
        } else {
          msg->signals[i].carrier_phase_cyc = 0;
          flags[i].valid_cp = false;
        }
        msg->signals[i].lock_time_s = lock_time[i];
        msg->signals[i].hca_indicator = hca_indicator[i];
        if (flags[i].valid_cnr) {
          msg->signals[i].cnr = cnr[i];
        } else {
          msg->signals[i].cnr = 0;
        }
        if (rough_rate_valid[sat] && flags[i].valid_dop && freq_valid) {
          /* convert Doppler into Hz */
          msg->signals[i].range_rate_Hz =
              (rough_rate[sat] + fine_dop[i]) * (freq / GPS_C);
        } else {
          msg->signals[i].range_rate_Hz = 0;
          flags[i].valid_dop = 0;
        }
        msg->signals[i].flags = flags[i];
        i++;
      }
    }
  }

  return RC_OK;
}

/** Decode an RTCMv3 Multi System Message 4
 *
 * \param buff The input data buffer
 * \param glo_sv_id_fcn_map Optional GLO FCN table (size MAX_GLO_PRN + 1)
 * \param RTCM message struct
 * \return  - RC_OK : Success
 *          - RC_MESSAGE_TYPE_MISMATCH : Message type mismatch
 *          - RC_INVALID_MESSAGE : Cell mask too large or invalid TOW
 */
rtcm3_rc rtcm3_decode_msm4(const uint8_t buff[],
                           const uint8_t glo_sv_id_fcn_map[],
                           rtcm_msm_message *msg) {
  return rtcm3_decode_msm_internal(buff, MSM4, glo_sv_id_fcn_map, msg);
}

/** Decode an RTCMv3 Multi System Message 5
 *
 * \param buff The input data buffer
 * \param RTCM message struct
 * \return  - RC_OK : Success
 *          - RC_MESSAGE_TYPE_MISMATCH : Message type mismatch
 *          - RC_INVALID_MESSAGE : Cell mask too large or invalid TOW
 */
rtcm3_rc rtcm3_decode_msm5(const uint8_t buff[], rtcm_msm_message *msg) {
  return rtcm3_decode_msm_internal(buff, MSM5, NULL, msg);
}

/** Decode an RTCMv3 Multi System Message 6
 *
 * \param buff The input data buffer
 * \param glo_sv_id_fcn_map Optional GLO FCN table (size MAX_GLO_PRN + 1)
 * \param RTCM message struct
 * \return  - RC_OK : Success
 *          - RC_MESSAGE_TYPE_MISMATCH : Message type mismatch
 *          - RC_INVALID_MESSAGE : Cell mask too large or invalid TOW
 */
rtcm3_rc rtcm3_decode_msm6(const uint8_t buff[],
                           const uint8_t glo_sv_id_fcn_map[],
                           rtcm_msm_message *msg) {
  return rtcm3_decode_msm_internal(buff, MSM6, glo_sv_id_fcn_map, msg);
}

/** Decode an RTCMv3 Multi System Message 7
 *
 * \param buff The input data buffer
 * \param RTCM message struct
 * \return  - RC_OK : Success
 *          - RC_MESSAGE_TYPE_MISMATCH : Message type mismatch
 *          - RC_INVALID_MESSAGE : Cell mask too large or invalid TOW
 */
rtcm3_rc rtcm3_decode_msm7(const uint8_t buff[], rtcm_msm_message *msg) {
  return rtcm3_decode_msm_internal(buff, MSM7, NULL, msg);
}
