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

/* These encoder functions are provided to allow for easier unit testing however they are not robust to be used
 * in production. Before using with real data, we would need to handle ambiguity rollover and code carrier
 * divergance at least
 */

#ifndef LIBRTCM_RTCM_ENCODER_H
#define LIBRTCM_RTCM_ENCODER_H

#include <rtcm3_messages.h>

void setbitu(uint8_t *buff, uint32_t pos, uint32_t len, uint32_t data);
void setbitul(uint8_t *buff, uint32_t pos, uint32_t len, uint64_t data);
void setbits(uint8_t *buff, uint32_t pos, uint32_t len, int32_t data);
void setbitsl(uint8_t *buff, uint32_t pos, uint32_t len, int64_t data);

uint16_t rtcm3_write_header(const rtcm_obs_header *header, uint8_t num_sats, uint8_t *buff);

uint16_t rtcm3_encode_1001(const rtcm_obs_message *msg_1001, uint8_t *buff);
uint16_t rtcm3_encode_1002(const rtcm_obs_message *msg_1002, uint8_t *buff);
uint16_t rtcm3_encode_1003(const rtcm_obs_message *msg_1003, uint8_t *buff);
uint16_t rtcm3_encode_1004(const rtcm_obs_message *msg_1004, uint8_t *buff);
uint16_t rtcm3_encode_1005(const rtcm_msg_1005 *msg_1005, uint8_t *buff);
uint16_t rtcm3_encode_1006(const rtcm_msg_1006 *msg_1006, uint8_t *buff);
uint16_t rtcm3_encode_1007(const rtcm_msg_1007 *msg_1007, uint8_t *buff);
uint16_t rtcm3_encode_1008(const rtcm_msg_1008 *msg_1008, uint8_t *buff);
uint16_t rtcm3_encode_1010(const rtcm_obs_message *msg_1010, uint8_t *buff);
uint16_t rtcm3_encode_1012(const rtcm_obs_message *msg_1012, uint8_t *buff);

#endif //LIBRTCM_RTCM_ENCODER_H
