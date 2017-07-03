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

void setbitu(u8 *buff, u32 pos, u32 len, u32 data);
void setbitul(u8 *buff, u32 pos, u32 len, u64 data);
void setbits(u8 *buff, u32 pos, u32 len, s32 data);
void setbitsl(u8 *buff, u32 pos, u32 len, s64 data);

u16 rtcm3_write_header(const rtcm_obs_header *header, u8 num_sats, u8 *buff);

u16 rtcm3_encode_1001(const rtcm_obs_message *msg_1001, u8 *buff);
u16 rtcm3_encode_1002(const rtcm_obs_message *msg_1002, u8 *buff);
u16 rtcm3_encode_1003(const rtcm_obs_message *msg_1003, u8 *buff);
u16 rtcm3_encode_1004(const rtcm_obs_message *msg_1004, u8 *buff);
u16 rtcm3_encode_1005(const rtcm_msg_1005 *msg_1005, u8 *buff);
u16 rtcm3_encode_1006(const rtcm_msg_1006 *msg_1006, u8 *buff);
u16 rtcm3_encode_1007(const rtcm_msg_1007 *msg_1007, u8 *buff);
u16 rtcm3_encode_1008(const rtcm_msg_1008 *msg_1008, u8 *buff);
u16 rtcm3_encode_1010(const rtcm_obs_message *msg_1010, u8 *buff);
u16 rtcm3_encode_1012(const rtcm_obs_message *msg_1012, u8 *buff);

#endif //LIBRTCM_RTCM_ENCODER_H
