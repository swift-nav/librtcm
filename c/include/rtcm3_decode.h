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

#ifndef SWIFTNAV_RTCM3_DECODE_H
#define SWIFTNAV_RTCM3_DECODE_H

#include <rtcm3_messages.h>

u32 getbitu(const u8 *buff, u32 pos, u8 len);
u64 getbitul(const u8 *buff, u32 pos, u8 len);
s32 getbits(const u8 *buff, u32 pos, u8 len);
s64 getbitsl(const u8 *buff, u32 pos, u8 len);

u16 rtcm3_read_header(const u8 *buff, rtcm_obs_header *header);

s8 rtcm3_decode_1001(const u8 *buff, rtcm_obs_message *msg_1001);
s8 rtcm3_decode_1002(const u8 *buff, rtcm_obs_message *msg_1002);
s8 rtcm3_decode_1003(const u8 *buff, rtcm_obs_message *msg_1003);
s8 rtcm3_decode_1004(const u8 *buff, rtcm_obs_message *msg_1004);
s8 rtcm3_decode_1005(const u8 *buff, rtcm_msg_1005 *msg_1005);
s8 rtcm3_decode_1006(const u8 *buff, rtcm_msg_1006 *msg_1006);
s8 rtcm3_decode_1007(const u8 *buff, rtcm_msg_1007 *msg_1007);
s8 rtcm3_decode_1008(const u8 *buff, rtcm_msg_1008 *msg_1008);
s8 rtcm3_decode_1010(const u8 *buff, rtcm_obs_message *msg_1010);
s8 rtcm3_decode_1012(const u8 *buff, rtcm_obs_message *msg_1012);

#endif /* SWIFTNAV_RTCM3_DECODE_H */
