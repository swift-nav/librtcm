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

#ifndef SWIFTNAV_DECODE_MACROS_H
#define SWIFTNAV_DECODE_MACROS_H

#ifdef __cplusplus
extern "C" {
#endif

/* macros for reading rcv/ant descriptor strings */
#define GET_STR_LEN(TheBuff, TheIdx, TheOutput)         \
  do {                                                  \
    (TheOutput) = rtcm_getbitu((TheBuff), (TheIdx), 8); \
    if (RTCM_MAX_STRING_LEN <= (TheOutput)) {           \
      return RC_INVALID_MESSAGE;                        \
    }                                                   \
    (TheIdx) += 8;                                      \
  } while (false);

#define GET_STR(TheBuff, TheIdx, TheLen, TheOutput)          \
  do {                                                       \
    for (uint8_t i = 0; i < (TheLen); ++i) {                 \
      (TheOutput)[i] = rtcm_getbitu((TheBuff), (TheIdx), 8); \
      (TheIdx) += 8;                                         \
    }                                                        \
  } while (false);

#ifdef __cplusplus
}
#endif

#endif /* SWIFTNAV_DECODE_MACROS_H */
