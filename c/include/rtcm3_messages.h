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

#include <common.h>
#include <constants.h>

typedef enum { L1_FREQ, L2_FREQ, NUM_FREQS } freq_enum;

typedef struct {
  u16 msg_num;      /* Msg Num DF002 uint16 12*/
  u16 stn_id;       /* Station Id DF003 uint16 12*/
  u32 tow;          /* GPS time of week DF004 uint32 30 */
  u8 sync;          /* Syncronous flag DF005 bit(1) 1 */
  u8 n_sat;         /* Number of satellites DF006 uint8 5 */
  u8 div_free;      /* Divergance free flag DF007 bit(1) 1 */
  u8 smooth;        /* GPS Smoothing Interval DF008 bit(3) 3 */
} rtcm_obs_header;

typedef union {
  struct {
    u8 valid_pr : 1;
    u8 valid_cp : 1;
    u8 valid_cnr : 1;
    u8 valid_lock : 1;
  };
  u8 data;
} flag_bf;

typedef struct {
  u8 code;
  u8 fcn;
  double pseudorange;
  double carrier_phase;
  u32 lock;
  double cnr;
  flag_bf flags;

} rtcm_freq_data;

typedef struct {
  u8 svId;
  rtcm_freq_data obs[NUM_FREQS];
} rtcm_sat_data;

typedef struct {
  rtcm_obs_header header;
  rtcm_sat_data sats[RTCM_MAX_SATS];
} rtcm_obs_message;

typedef struct {
  u16 stn_id;
  u8 ITRF;            /* Reserved for ITRF Realization Year DF021 uint6 6 */
  u8 GPS_ind;         /* GPS Indicator DF022 bit(1) 1 */
  u8 GLO_ind;         /* GLONASS Indicator DF023 bit(1) 1 */
  u8 GAL_ind;         /* Reserved for Galileo Indicator DF024 bit(1) 1 */
  u8 ref_stn_ind;     /* Reference-Station Indicator DF141 bit(1) 1 */
  double arp_x;       /* Antenna Reference Point ECEF-X DF025 int38 38 */
  u8 osc_ind;         /* Single Receiver Oscillator Indicator DF142 bit(1) 1 */
  u8 reserved;        /* Reserved DF001 bit(1) 1 */
  double arp_y;       /* Antenna Reference Point ECEF-Y DF026 int38 38 */
  u8 quart_cycle_ind; /* Quarter Cycle Indicator DF364 bit(2) 2 */
  double arp_z;       /* Antenna Reference Point ECEF-Z DF027 int38 38 */
} rtcm_msg_1005;

typedef struct {
  rtcm_msg_1005 msg_1005;
  double ant_height; /* Antenna Height DF028 uint16 16 */
} rtcm_msg_1006;

typedef struct {
  u16 stn_id;    /* Reference Station ID DF003 uint12 12 */
  u8 desc_count; /* Descriptor Counter N DF029 uint8 8 N <= 31 */
  char desc[32]; /* Antenna Descriptor DF030 char8(N) 8*N */
  u8 ant_id;     /* Antenna Setup ID DF031 uint8 8 */
} rtcm_msg_1007;

typedef struct {
  rtcm_msg_1007 msg_1007;
  u8 serial_count;     /* Serial Number Counter M DF032 uint8 8 M <= 31 */
  char serial_num[32]; /* Antenna Serial Number DF033 char8(M) 8*M */
} rtcm_msg_1008;

#endif /* PIKSI_BUILDROOT_RTCM3_MESSAGES_H_H */
