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

#ifndef PIKSI_BUILDROOT_RTCM3_MESSAGES_H_H
#define PIKSI_BUILDROOT_RTCM3_MESSAGES_H_H

#include <stdint.h>
#include <stdbool.h>
#include <constants.h>

typedef enum { L1_FREQ, L2_FREQ, NUM_FREQS } freq_enum;

typedef struct {
  uint16_t msg_num;      /* Msg Num DF002 uint16 12*/
  uint16_t stn_id;       /* Station Id DF003 uint16 12*/
  uint32_t tow_ms;       /* GPS/GLO time of week DF004/DF034 uint32 30/27 */
  uint8_t sync;          /* Syncronous flag DF005 bit(1) 1 */
  uint8_t n_sat;         /* Number of satellites DF006 uint8 5 */
  uint8_t div_free;      /* Divergance free flag DF007 bit(1) 1 */
  uint8_t smooth;        /* GPS Smoothing Interval DF008 bit(3) 3 */
} rtcm_obs_header;

typedef union {
  struct {
    uint8_t valid_pr : 1;
    uint8_t valid_cp : 1;
    uint8_t valid_cnr : 1;
    uint8_t valid_lock : 1;
  };
  uint8_t data;
} flag_bf;

typedef struct {
  uint8_t code;
  double pseudorange;
  double carrier_phase;
  uint32_t lock;
  double cnr;
  flag_bf flags;

} rtcm_freq_data;

typedef struct {
  uint8_t svId;
  uint8_t fcn;
  rtcm_freq_data obs[NUM_FREQS];
} rtcm_sat_data;

typedef struct {
  rtcm_obs_header header;
  rtcm_sat_data sats[RTCM_MAX_SATS];
} rtcm_obs_message;

typedef struct {
  uint16_t stn_id;
  uint8_t ITRF;            /* Reserved for ITRF Realization Year DF021 uint6 6 */
  uint8_t GPS_ind;         /* GPS Indicator DF022 bit(1) 1 */
  uint8_t GLO_ind;         /* GLONASS Indicator DF023 bit(1) 1 */
  uint8_t GAL_ind;         /* Reserved for Galileo Indicator DF024 bit(1) 1 */
  uint8_t ref_stn_ind;     /* Reference-Station Indicator DF141 bit(1) 1 */
  double arp_x;       /* Antenna Reference Point ECEF-X DF025 int38 38 */
  uint8_t osc_ind;         /* Single Receiver Oscillator Indicator DF142 bit(1) 1 */
  uint8_t reserved;        /* Reserved DF001 bit(1) 1 */
  double arp_y;       /* Antenna Reference Point ECEF-Y DF026 int38 38 */
  uint8_t quart_cycle_ind; /* Quarter Cycle Indicator DF364 bit(2) 2 */
  double arp_z;       /* Antenna Reference Point ECEF-Z DF027 int38 38 */
} rtcm_msg_1005;

typedef struct {
  rtcm_msg_1005 msg_1005;
  double ant_height; /* Antenna Height DF028 uint16 16 */
} rtcm_msg_1006;

typedef struct {
  uint16_t stn_id;    /* Reference Station ID DF003 uint12 12 */
  uint8_t desc_count; /* Descriptor Counter N DF029 uint8 8 N <= 31 */
  char desc[32]; /* Antenna Descriptor DF030 char8(N) 8*N */
  uint8_t ant_id;     /* Antenna Setup ID DF031 uint8 8 */
} rtcm_msg_1007;

typedef struct {
  rtcm_msg_1007 msg_1007;
  uint8_t serial_count;     /* Serial Number Counter M DF032 uint8 8 M <= 31 */
  char serial_num[32]; /* Antenna Serial Number DF033 char8(M) 8*M */
} rtcm_msg_1008;

#endif /* PIKSI_BUILDROOT_RTCM3_MESSAGES_H_H */
