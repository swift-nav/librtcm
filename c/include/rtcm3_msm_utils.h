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

#ifndef SWIFTNAV_RTCM3_MSM_UTILS_H
#define SWIFTNAV_RTCM3_MSM_UTILS_H

#include "rtcm3_messages.h"

double msm_signal_frequency(const rtcm_msm_header *header,
                            const uint8_t signal_index,
                            const uint8_t sat_info);
msm_enum to_msm_type(uint16_t msg_num);
constellation_t to_constellation(uint16_t msg_num);
uint8_t count_mask_values(uint8_t mask_size, const bool mask[]);
uint8_t find_nth_mask_value(const uint8_t mask_size,
                            const bool mask[],
                            const uint8_t n);

code_t msm_signal_to_code(const rtcm_msm_header *header, uint8_t signal_index);
uint8_t msm_sat_to_prn(const rtcm_msm_header *header, uint8_t satellite_index);

#endif /* SWIFTNAV_RTCM3_MSM_UTILS_H */
