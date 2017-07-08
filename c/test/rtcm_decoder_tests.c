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

#include "rtcm_decoder_tests.h"
#include <assert.h>
#include <math.h>
#include <rtcm3_decode.h>
#include <string.h>
#include "rtcm_encoder.h"

int main(void)
{
  test_rtcm_1001();
  test_rtcm_1002();
  test_rtcm_1003();
  test_rtcm_1004();
  test_rtcm_1005();
  test_rtcm_1006();
  test_rtcm_1007();
  test_rtcm_1008();
  test_rtcm_1010();
  test_rtcm_1012();
}

void test_rtcm_1001(void)
{
  rtcm_obs_header header;
  header.msg_num = 1001;
  header.div_free = 0;
  header.n_sat = 3;
  header.smooth = 0;
  header.stn_id = 7;
  header.sync = 1;
  header.tow_ms = 309000000;

  rtcm_obs_message msg1001;
  memset((void *) &msg1001, 0, sizeof(msg1001));
  msg1001.header = header;
  msg1001.sats[0].svId = 4;
  msg1001.sats[0].obs[0].code = 0;
  msg1001.sats[0].obs[0].pseudorange = 20000004.4;
  msg1001.sats[0].obs[0].carrier_phase = 105100794.4;
  msg1001.sats[0].obs[0].lock = 900;
  msg1001.sats[0].obs[0].flags.valid_pr = 1;
  msg1001.sats[0].obs[0].flags.valid_cp = 1;
  msg1001.sats[0].obs[0].flags.valid_lock = 1;

  msg1001.sats[1].svId = 6;
  msg1001.sats[1].obs[0].code = 0;
  msg1001.sats[1].obs[0].pseudorange = 22000004.4;
  msg1001.sats[1].obs[0].carrier_phase = 115610703.4;
  msg1001.sats[1].obs[0].lock = 254;
  msg1001.sats[1].obs[0].flags.valid_pr = 1;
  msg1001.sats[1].obs[0].flags.valid_cp = 1;
  msg1001.sats[1].obs[0].flags.valid_lock = 1;

  msg1001.sats[2].svId = 6;
  msg1001.sats[2].obs[0].code = 0;
  msg1001.sats[2].obs[0].pseudorange = 22000004.4;
  msg1001.sats[2].obs[0].carrier_phase = 115610553.4;
  msg1001.sats[2].obs[0].lock = 254;
  msg1001.sats[2].obs[0].flags.valid_pr = 1;
  msg1001.sats[2].obs[0].flags.valid_cp = 0;
  msg1001.sats[2].obs[0].flags.valid_lock = 0;

  uint8_t buff[1024];
  memset(buff,0,1024);
  rtcm3_encode_1001(&msg1001, buff);

  rtcm_obs_message msg1001_out;
  int8_t ret = rtcm3_decode_1001(buff, &msg1001_out);

  assert(ret == 0 && msgobs_equals(&msg1001, &msg1001_out));
}

void test_rtcm_1002(void)
{
  rtcm_obs_header header;
  header.msg_num = 1002;
  header.div_free = 0;
  header.n_sat = 3;
  header.smooth = 0;
  header.stn_id = 7;
  header.sync = 1;
  header.tow_ms = 309000000;

  rtcm_obs_message msg1002;
  memset((void *) &msg1002, 0, sizeof(msg1002));
  msg1002.header = header;
  msg1002.sats[0].svId = 4;
  msg1002.sats[0].obs[0].code = 0;
  msg1002.sats[0].obs[0].pseudorange = 20000004.4;
  msg1002.sats[0].obs[0].carrier_phase = 105100794.4;
  msg1002.sats[0].obs[0].lock = 900;
  msg1002.sats[0].obs[0].flags.valid_pr = 1;
  msg1002.sats[0].obs[0].flags.valid_cp = 1;
  msg1002.sats[0].obs[0].flags.valid_lock = 1;
  msg1002.sats[0].obs[0].cnr = 3.4;
  msg1002.sats[0].obs[0].flags.valid_cnr = 1;

  msg1002.sats[1].svId = 6;
  msg1002.sats[1].obs[0].code = 0;
  msg1002.sats[1].obs[0].pseudorange = 22000004.4;
  msg1002.sats[1].obs[0].carrier_phase = 115610703.4;
  msg1002.sats[1].obs[0].lock = 254;
  msg1002.sats[1].obs[0].flags.valid_pr = 1;
  msg1002.sats[1].obs[0].flags.valid_cp = 1;
  msg1002.sats[1].obs[0].flags.valid_lock = 1;
  msg1002.sats[1].obs[0].cnr = 50.2;
  msg1002.sats[1].obs[0].flags.valid_cnr = 1;

  msg1002.sats[2].svId = 6;
  msg1002.sats[2].obs[0].code = 0;
  msg1002.sats[2].obs[0].pseudorange = 22000004.4;
  msg1002.sats[2].obs[0].carrier_phase = 115610553.4;
  msg1002.sats[2].obs[0].lock = 254;
  msg1002.sats[2].obs[0].flags.valid_pr = 1;
  msg1002.sats[2].obs[0].flags.valid_cp = 0;
  msg1002.sats[2].obs[0].flags.valid_lock = 0;
  msg1002.sats[2].obs[0].cnr = 50.2;
  msg1002.sats[2].obs[0].flags.valid_cnr = 0;

  uint8_t buff[1024];
  memset(buff,0,1024);
  rtcm3_encode_1002(&msg1002, buff);

  rtcm_obs_message msg1002_out;
  int8_t ret = rtcm3_decode_1002(buff, &msg1002_out);

  assert(ret == 0 && msgobs_equals(&msg1002, &msg1002_out));
}

void test_rtcm_1003(void)
{
  rtcm_obs_header header;
  header.msg_num = 1003;
  header.div_free = 0;
  header.n_sat = 3;
  header.smooth = 0;
  header.stn_id = 7;
  header.sync = 1;
  header.tow_ms = 309000000;

  rtcm_obs_message msg1003;
  memset((void *) &msg1003, 0, sizeof(msg1003));
  msg1003.header = header;
  msg1003.sats[0].svId = 4;
  msg1003.sats[0].obs[0].code = 0;
  msg1003.sats[0].obs[0].pseudorange = 20000004.4;
  msg1003.sats[0].obs[0].carrier_phase = 105100794.4;
  msg1003.sats[0].obs[0].lock = 900;
  msg1003.sats[0].obs[0].flags.valid_pr = 1;
  msg1003.sats[0].obs[0].flags.valid_cp = 1;
  msg1003.sats[0].obs[0].flags.valid_lock = 1;
  msg1003.sats[0].obs[0].flags.valid_cnr = 0;
  msg1003.sats[0].obs[1] = msg1003.sats[0].obs[0];
  msg1003.sats[0].obs[1].pseudorange = 20000124.4;
  msg1003.sats[0].obs[1].carrier_phase = 81897184.4;

  msg1003.sats[1].svId = 6;
  msg1003.sats[1].obs[0].code = 0;
  msg1003.sats[1].obs[0].pseudorange = 22000004.4;
  msg1003.sats[1].obs[0].carrier_phase = 115610703.4;
  msg1003.sats[1].obs[0].lock = 254;
  msg1003.sats[1].obs[0].flags.valid_pr = 1;
  msg1003.sats[1].obs[0].flags.valid_cp = 1;
  msg1003.sats[1].obs[0].flags.valid_lock = 1;
  msg1003.sats[1].obs[0].flags.valid_cnr = 0;
  msg1003.sats[1].obs[1] = msg1003.sats[1].obs[0];
  msg1003.sats[1].obs[1].pseudorange = 22000024.4;
  msg1003.sats[1].obs[1].carrier_phase = 90086422.236;


  msg1003.sats[2].svId = 6;
  msg1003.sats[2].obs[0].code = 0;
  msg1003.sats[2].obs[0].pseudorange = 22000004.4;
  msg1003.sats[2].obs[0].carrier_phase = 115610553.4;
  msg1003.sats[2].obs[0].lock = 254;
  msg1003.sats[2].obs[0].flags.valid_pr = 1;
  msg1003.sats[2].obs[0].flags.valid_cp = 0;
  msg1003.sats[2].obs[0].flags.valid_lock = 0;
  msg1003.sats[2].obs[0].cnr = 50.2;
  msg1003.sats[2].obs[0].flags.valid_cnr = 0;

  uint8_t buff[1024];
  memset(buff,0,1024);
  rtcm3_encode_1003(&msg1003, buff);

  rtcm_obs_message msg1003_out;
  int8_t ret = rtcm3_decode_1003(buff, &msg1003_out);

  assert(ret == 0 && msgobs_equals(&msg1003, &msg1003_out));
}

void test_rtcm_1004(void)
{
  rtcm_obs_header header;
  header.msg_num = 1004;
  header.div_free = 0;
  header.n_sat = 3;
  header.smooth = 0;
  header.stn_id = 7;
  header.sync = 1;
  header.tow_ms = 309000000;

  rtcm_obs_message msg1004;
  memset((void *) &msg1004, 0, sizeof(msg1004));
  msg1004.header = header;
  msg1004.sats[0].svId = 4;
  msg1004.sats[0].obs[0].code = 0;
  msg1004.sats[0].obs[0].pseudorange = 20000004.4;
  msg1004.sats[0].obs[0].carrier_phase = 105100794.4;
  msg1004.sats[0].obs[0].lock = 900;
  msg1004.sats[0].obs[0].flags.valid_pr = 1;
  msg1004.sats[0].obs[0].flags.valid_cp = 1;
  msg1004.sats[0].obs[0].flags.valid_lock = 1;
  msg1004.sats[0].obs[0].cnr = 3.4;
  msg1004.sats[0].obs[0].flags.valid_cnr = 1;
  msg1004.sats[0].obs[1] = msg1004.sats[0].obs[0];
  msg1004.sats[0].obs[1].pseudorange = 20000124.4;
  msg1004.sats[0].obs[1].carrier_phase = 81897184.4;
  msg1004.sats[0].obs[0].cnr = 3.4;
  msg1004.sats[0].obs[1].cnr = 1.4;

  msg1004.sats[1].svId = 6;
  msg1004.sats[1].obs[0].code = 0;
  msg1004.sats[1].obs[0].pseudorange = 22000004.4;
  msg1004.sats[1].obs[0].carrier_phase = 115610703.4;
  msg1004.sats[1].obs[0].lock = 254;
  msg1004.sats[1].obs[0].flags.valid_pr = 1;
  msg1004.sats[1].obs[0].flags.valid_cp = 1;
  msg1004.sats[1].obs[0].flags.valid_lock = 1;
  msg1004.sats[1].obs[0].cnr = 50.2;
  msg1004.sats[1].obs[0].flags.valid_cnr = 1;
  msg1004.sats[1].obs[0].cnr = 50.2;
  msg1004.sats[1].obs[0].flags.valid_cnr = 1;
  msg1004.sats[1].obs[1] = msg1004.sats[1].obs[0];
  msg1004.sats[1].obs[1].pseudorange = 22000024.4;
  msg1004.sats[1].obs[1].carrier_phase = 90086422.236;


  msg1004.sats[2].svId = 6;
  msg1004.sats[2].obs[0].code = 0;
  msg1004.sats[2].obs[0].pseudorange = 22000004.4;
  msg1004.sats[2].obs[0].carrier_phase = 115610553.4;
  msg1004.sats[2].obs[0].lock = 254;
  msg1004.sats[2].obs[0].flags.valid_pr = 1;
  msg1004.sats[2].obs[0].flags.valid_cp = 0;
  msg1004.sats[2].obs[0].flags.valid_lock = 0;
  msg1004.sats[2].obs[0].cnr = 50.2;
  msg1004.sats[2].obs[0].flags.valid_cnr = 0;
  msg1004.sats[2].obs[0].cnr = 50.2;
  msg1004.sats[2].obs[0].flags.valid_cnr = 0;
  msg1004.sats[2].obs[1].cnr = 54.2;
  msg1004.sats[2].obs[1].flags.valid_cnr = 1;

  uint8_t buff[1024];
  memset(buff,0,1024);
  rtcm3_encode_1004(&msg1004, buff);

  rtcm_obs_message msg1004_out;
  int8_t ret = rtcm3_decode_1004(buff, &msg1004_out);

  assert(ret == 0 && msgobs_equals(&msg1004, &msg1004_out));
}

void test_rtcm_1005(void)
{
  rtcm_msg_1005 msg1005;

  msg1005.stn_id = 5;
  msg1005.ref_stn_ind = 1;
  msg1005.quart_cycle_ind = 1;
  msg1005.osc_ind = 0;
  msg1005.ITRF = 1;
  msg1005.GPS_ind = 1;
  msg1005.GLO_ind = 1;
  msg1005.GAL_ind = 0;
  msg1005.arp_x = 3578346.5475;
  msg1005.arp_y = -5578346.5578;
  msg1005.arp_z = 2578346.6757;

  uint8_t buff[1024];
  memset(buff,0,1024);
  rtcm3_encode_1005(&msg1005, buff);

  rtcm_msg_1005 msg1005_out;
  int8_t ret = rtcm3_decode_1005(buff, &msg1005_out);

  assert(ret == 0 && msg1005_equals(&msg1005, &msg1005_out));
}

void test_rtcm_1006(void)
{
  rtcm_msg_1006 msg1006;

  msg1006.msg_1005.stn_id = 5;
  msg1006.msg_1005.ref_stn_ind = 0;
  msg1006.msg_1005.quart_cycle_ind = 0;
  msg1006.msg_1005.osc_ind = 1;
  msg1006.msg_1005.ITRF = 0;
  msg1006.msg_1005.GPS_ind = 0;
  msg1006.msg_1005.GLO_ind = 0;
  msg1006.msg_1005.GAL_ind = 1;
  msg1006.msg_1005.arp_x = 3573346.5475;
  msg1006.msg_1005.arp_y = -5576346.5578;
  msg1006.msg_1005.arp_z = 2578376.6757;
  msg1006.ant_height = 1.567;

  uint8_t buff[1024];
  memset(buff,0,1024);
  rtcm3_encode_1006(&msg1006, buff);

  rtcm_msg_1006 msg1006_out;
  int8_t ret = rtcm3_decode_1006(buff, &msg1006_out);

  assert(ret == 0 && msg1006_equals(&msg1006, &msg1006_out));
}

void test_rtcm_1007(void)
{
  rtcm_msg_1007 msg1007;

  msg1007.stn_id = 1022;
  msg1007.desc_count = 29;
  strcpy(msg1007.desc, "Something with 29 characters.");
  msg1007.ant_id = 254;

  uint8_t buff[1024];
  memset(buff,0,1024);
  rtcm3_encode_1007(&msg1007, buff);

  rtcm_msg_1007 msg1007_out;
  int8_t ret = rtcm3_decode_1007(buff, &msg1007_out);

  assert(ret == 0 && msg1007_equals(&msg1007, &msg1007_out));
}

void test_rtcm_1008(void)
{
  rtcm_msg_1008 msg1008;

  msg1008.msg_1007.stn_id = 22;
  msg1008.msg_1007.desc_count = 27;
  strcpy(msg1008.msg_1007.desc, "Something without 30 chars.");
  msg1008.msg_1007.ant_id = 1;
  msg1008.serial_count = 9;
  strcpy(msg1008.serial_num, "123456789");

  uint8_t buff[1024];
  memset(buff,0,1024);
  rtcm3_encode_1008(&msg1008, buff);

  rtcm_msg_1008 msg1008_out;
  int8_t ret = rtcm3_decode_1008(buff, &msg1008_out);

  assert(ret == 0 && msg1008_equals(&msg1008, &msg1008_out));
}

void test_rtcm_1010(void)
{
  rtcm_obs_header header;
  header.msg_num = 1010;
  header.div_free = 0;
  header.n_sat = 3;
  header.smooth = 0;
  header.stn_id = 7;
  header.sync = 1;
  header.tow_ms = 86399000;

  rtcm_obs_message msg1010;
  memset((void *) &msg1010, 0, sizeof(msg1010));
  msg1010.header = header;
  msg1010.sats[0].svId = 4;
  msg1010.sats[0].fcn = 9;
  msg1010.sats[0].obs[0].code = 0;
  msg1010.sats[0].obs[0].pseudorange = 20000004.4;
  msg1010.sats[0].obs[0].carrier_phase = 106949010.6;
  msg1010.sats[0].obs[0].lock = 900;
  msg1010.sats[0].obs[0].flags.valid_pr = 1;
  msg1010.sats[0].obs[0].flags.valid_cp = 1;
  msg1010.sats[0].obs[0].flags.valid_lock = 1;
  msg1010.sats[0].obs[0].cnr = 3.4;
  msg1010.sats[0].obs[0].flags.valid_cnr = 1;

  msg1010.sats[1].svId = 6;
  msg1010.sats[1].fcn = 0x03;
  msg1010.sats[1].obs[0].code = 0;
  msg1010.sats[1].obs[0].pseudorange = 22000004.4;
  msg1010.sats[1].obs[0].carrier_phase = 117396240.5;
  msg1010.sats[1].obs[0].lock = 254;
  msg1010.sats[1].obs[0].flags.valid_pr = 1;
  msg1010.sats[1].obs[0].flags.valid_cp = 1;
  msg1010.sats[1].obs[0].flags.valid_lock = 1;
  msg1010.sats[1].obs[0].cnr = 50.2;
  msg1010.sats[1].obs[0].flags.valid_cnr = 1;

  msg1010.sats[2].svId = 6;
  msg1010.sats[2].fcn = 5;
  msg1010.sats[2].obs[0].code = 0;
  msg1010.sats[2].obs[0].pseudorange = 22000004.4;
  msg1010.sats[2].obs[0].carrier_phase = 117396248.1;
  msg1010.sats[2].obs[0].lock = 254;
  msg1010.sats[2].obs[0].flags.valid_pr = 1;
  msg1010.sats[2].obs[0].flags.valid_cp = 0;
  msg1010.sats[2].obs[0].flags.valid_lock = 0;
  msg1010.sats[2].obs[0].cnr = 50.2;
  msg1010.sats[2].obs[0].flags.valid_cnr = 0;

  uint8_t buff[1024];
  memset(buff,0,1024);
    rtcm3_encode_1010(&msg1010, buff);

  rtcm_obs_message msg1010_out;
  int8_t ret = rtcm3_decode_1010(buff, &msg1010_out);

  assert(ret == 0 && msgobs_glo_equals(&msg1010, &msg1010_out));
}

void test_rtcm_1012(void)
{

  rtcm_obs_header header;
  header.msg_num = 1012;
  header.div_free = 0;
  header.n_sat = 3;
  header.smooth = 0;
  header.stn_id = 7;
  header.sync = 1;
  header.tow_ms = 34700000;

  rtcm_obs_message msg1012;
  memset((void *) &msg1012, 0, sizeof(msg1012));
  msg1012.header = header;
  msg1012.sats[0].svId = 4;
  msg1012.sats[0].fcn = 7;
  msg1012.sats[0].obs[0].code = 0;
  msg1012.sats[0].obs[0].pseudorange = 20000004.4;
  msg1012.sats[0].obs[0].carrier_phase =  106874009.6;
  msg1012.sats[0].obs[0].lock = 900;
  msg1012.sats[0].obs[0].flags.valid_pr = 1;
  msg1012.sats[0].obs[0].flags.valid_cp = 1;
  msg1012.sats[0].obs[0].flags.valid_lock = 1;
  msg1012.sats[0].obs[0].cnr = 3.4;
  msg1012.sats[0].obs[0].flags.valid_cnr = 1;
  msg1012.sats[0].obs[1] = msg1012.sats[0].obs[0];
  msg1012.sats[0].obs[1].pseudorange = 20000124.4;
  msg1012.sats[0].obs[1].carrier_phase = 83124100.9;
  msg1012.sats[0].obs[0].cnr = 3.4;
  msg1012.sats[0].obs[1].cnr = 1.4;

  msg1012.sats[1].svId = 6;
  msg1012.sats[1].fcn = 13;
  msg1012.sats[1].obs[0].code = 0;
  msg1012.sats[1].obs[0].pseudorange = 22000004.4;
  msg1012.sats[1].obs[0].carrier_phase = 117809044.6;
  msg1012.sats[1].obs[0].lock = 254;
  msg1012.sats[1].obs[0].flags.valid_pr = 1;
  msg1012.sats[1].obs[0].flags.valid_cp = 1;
  msg1012.sats[1].obs[0].flags.valid_lock = 1;
  msg1012.sats[1].obs[0].cnr = 50.2;
  msg1012.sats[1].obs[0].flags.valid_cnr = 1;
  msg1012.sats[1].obs[0].cnr = 50.2;
  msg1012.sats[1].obs[0].flags.valid_cnr = 1;
  msg1012.sats[1].obs[1] = msg1012.sats[1].obs[0];
  msg1012.sats[1].obs[1].pseudorange = 22000024.4;
  msg1012.sats[1].obs[1].carrier_phase = 91629341.2;


  msg1012.sats[2].svId = 6;
  msg1012.sats[2].fcn = 1;
  msg1012.sats[2].obs[0].code = 0;
  msg1012.sats[2].obs[0].pseudorange = 22000004.4;
  msg1012.sats[2].obs[0].carrier_phase = 117396239.7;
  msg1012.sats[2].obs[0].lock = 254;
  msg1012.sats[2].obs[0].flags.valid_pr = 1;
  msg1012.sats[2].obs[0].flags.valid_cp = 0;
  msg1012.sats[2].obs[0].flags.valid_lock = 0;
  msg1012.sats[2].obs[0].cnr = 50.2;
  msg1012.sats[2].obs[0].flags.valid_cnr = 0;
  msg1012.sats[2].obs[0].cnr = 50.2;
  msg1012.sats[2].obs[0].flags.valid_cnr = 0;
  msg1012.sats[2].obs[1].cnr = 54.2;
  msg1012.sats[2].obs[1].flags.valid_cnr = 1;

  uint8_t buff[1024];
  memset(buff,0,1024);
  rtcm3_encode_1012(&msg1012, buff);

  rtcm_obs_message msg1012_out;
  int8_t ret = rtcm3_decode_1012(buff, &msg1012_out);

  assert(ret == 0 && msgobs_glo_equals(&msg1012, &msg1012_out));
}

bool msgobs_equals(const rtcm_obs_message *msg_in,
                   const rtcm_obs_message *msg_out)
{

  if (msg_in->header.msg_num != msg_out->header.msg_num) {
    return false;
  }
  bool L1_only = false;
  if (msg_in->header.msg_num == 1001 || msg_in->header.msg_num == 1002) {
    L1_only = true;
  }
  if (msg_in->header.stn_id != msg_out->header.stn_id) {
    return false;
  }
  if (msg_in->header.tow_ms != msg_out->header.tow_ms) {
    return false;
  }
  if (msg_in->header.sync != msg_out->header.sync) {
    return false;
  }

  uint8_t num_sats = 0;
  for (uint8_t i = 0; i < msg_in->header.n_sat; i++) {
    flag_bf l1_flags = msg_in->sats[i].obs[L1_FREQ].flags;
    flag_bf l2_flags = msg_in->sats[i].obs[L2_FREQ].flags;
    if (l1_flags.valid_pr && l1_flags.valid_cp &&
        (L1_only || (l2_flags.valid_pr && l2_flags.valid_cp))) {
      ++num_sats;
    }
  }
  if (num_sats != msg_out->header.n_sat) {
    return false;
  }
  if (msg_in->header.div_free != msg_out->header.div_free) {
    return false;
  }
  if (msg_in->header.smooth != msg_out->header.smooth) {
    return false;
  }

  uint8_t out_sat_idx = 0;
  for (uint8_t in_sat_idx = 0; in_sat_idx < msg_in->header.n_sat; ++in_sat_idx) {
    flag_bf l1_flags = msg_in->sats[in_sat_idx].obs[L1_FREQ].flags;
    flag_bf l2_flags = msg_in->sats[in_sat_idx].obs[L2_FREQ].flags;
    if (!l1_flags.valid_pr || !l1_flags.valid_cp ||
        (!L1_only && (!l2_flags.valid_pr || !l2_flags.valid_cp))) {
      continue;
    }

    if (msg_in->sats[in_sat_idx].svId != msg_out->sats[out_sat_idx].svId) {
      return false;
    }

    uint8_t amb = 0;
    if (msg_in->header.msg_num == 1001 || msg_in->header.msg_num == 1003) {
      amb = (uint8_t)roundl((msg_in->sats[in_sat_idx].obs[0].pseudorange -
                        msg_out->sats[out_sat_idx].obs[0].pseudorange) /
                       PRUNIT_GPS);
    }

    for (uint8_t freq = 0; freq < NUM_FREQS; ++freq) {
      const rtcm_freq_data *in_freq = &msg_in->sats[in_sat_idx].obs[freq];
      const rtcm_freq_data *out_freq = &msg_out->sats[out_sat_idx].obs[freq];

      if (in_freq->flags.valid_pr != out_freq->flags.valid_pr) {
        return false;
      }

      if (in_freq->flags.valid_cp != out_freq->flags.valid_cp) {
        return false;
      }

      if ((msg_in->header.msg_num == 1002 || msg_in->header.msg_num == 1004) &&
          in_freq->flags.valid_cnr != out_freq->flags.valid_cnr) {
        return false;
      }

      if (in_freq->flags.valid_lock != out_freq->flags.valid_lock) {
        return false;
      }

      if (in_freq->flags.valid_pr) {
        if (in_freq->code != out_freq->code ||
            fabs(in_freq->pseudorange - out_freq->pseudorange -
                 amb * PRUNIT_GPS) > 0.01) {
          return false;
        }
      }
      if (in_freq->flags.valid_cp) {
        double frequency = freq == L1_FREQ ? GPS_L1_FREQ : GPS_L2_FREQ;
        if (fabs(in_freq->carrier_phase - out_freq->carrier_phase ) -
                 ((double)amb * PRUNIT_GPS / (CLIGHT / frequency)) >
            0.0005 / (CLIGHT / frequency)) {
          return false;
        }
      }
      if (in_freq->flags.valid_cnr) {
        if (fabs(in_freq->cnr - out_freq->cnr) > 0.125) {
          return false;
        }
      }
      if (in_freq->flags.valid_lock) {
        if( in_freq->lock < 24 ) {
            if( out_freq->lock >= 24 ) {
                return false;
            }
        }
        else if( in_freq->lock < 72 ) {
            if( out_freq->lock < 24 || out_freq->lock >= 72 )
            {
                return false;
            }
        }
        else if( in_freq->lock < 168 ) {
            if (out_freq->lock < 72 || out_freq->lock >= 168)
            {
                return false;
            }
        }
        else if( in_freq->lock < 360 ) {
            if( out_freq->lock < 168 || out_freq->lock >= 360
            ) {
                return false;
            }
        }
        else if( in_freq->lock < 744 ) {
            if( out_freq->lock < 360 || out_freq->lock >= 744
            ) {
                return false;
            }
        }
        else if( in_freq->lock < 937 ) {
            if( out_freq->lock < 744 || out_freq->lock >= 937
            ) {
                return false;
            }
        }
        else {
            if( out_freq->lock < 937 ) {
                return false;
            }
        }
      }
    }
    ++out_sat_idx;
  }

  return true;
}

bool msgobs_glo_equals(const rtcm_obs_message *msg_in,
                       const rtcm_obs_message *msg_out)
{

  if (msg_in->header.msg_num != msg_out->header.msg_num) {
    return false;
  }
  bool L1_only = false;
  if (msg_in->header.msg_num == 1009 || msg_in->header.msg_num == 1010) {
    L1_only = true;
  }
  if (msg_in->header.stn_id != msg_out->header.stn_id) {
    return false;
  }
  if (msg_in->header.tow_ms != msg_out->header.tow_ms) {
    return false;
  }
  if (msg_in->header.sync != msg_out->header.sync) {
    return false;
  }

  uint8_t num_sats = 0;
  for (uint8_t i = 0; i < msg_in->header.n_sat; i++) {
    flag_bf l1_flags = msg_in->sats[i].obs[L1_FREQ].flags;
    flag_bf l2_flags = msg_in->sats[i].obs[L2_FREQ].flags;
    if (l1_flags.valid_pr && l1_flags.valid_cp &&
        (L1_only || (l2_flags.valid_pr && l2_flags.valid_cp))) {
      ++num_sats;
    }
  }
  if (num_sats != msg_out->header.n_sat) {
    return false;
  }
  if (msg_in->header.div_free != msg_out->header.div_free) {
    return false;
  }
  if (msg_in->header.smooth != msg_out->header.smooth) {
    return false;
  }

  uint8_t out_sat_idx = 0;
  for (uint8_t in_sat_idx = 0; in_sat_idx < msg_in->header.n_sat; ++in_sat_idx) {
    flag_bf l1_flags = msg_in->sats[in_sat_idx].obs[L1_FREQ].flags;
    flag_bf l2_flags = msg_in->sats[in_sat_idx].obs[L2_FREQ].flags;
    if (!l1_flags.valid_pr || !l1_flags.valid_cp ||
        (!L1_only && (!l2_flags.valid_pr || !l2_flags.valid_cp))) {
      continue;
    }

    if (msg_in->sats[in_sat_idx].svId != msg_out->sats[out_sat_idx].svId) {
      return false;
    }

    if (msg_in->sats[in_sat_idx].fcn != msg_out->sats[out_sat_idx].fcn) {
      return false;
    }

    uint8_t amb = 0;
    if (msg_in->header.msg_num == 1009 || msg_in->header.msg_num == 1011) {
      amb = (uint8_t)roundl((msg_in->sats[in_sat_idx].obs[0].pseudorange -
                        msg_out->sats[out_sat_idx].obs[0].pseudorange) /
                       PRUNIT_GLO);
    }

    for (uint8_t freq = 0; freq < NUM_FREQS; ++freq) {
      const rtcm_freq_data *in_freq = &msg_in->sats[in_sat_idx].obs[freq];
      const rtcm_freq_data *out_freq = &msg_out->sats[out_sat_idx].obs[freq];

      if (in_freq->flags.valid_pr != out_freq->flags.valid_pr) {
        return false;
      }

      if (in_freq->flags.valid_cp != out_freq->flags.valid_cp) {
        return false;
      }

      if ((msg_in->header.msg_num == 1010 || msg_in->header.msg_num == 1012) &&
          in_freq->flags.valid_cnr != out_freq->flags.valid_cnr) {
        return false;
      }

      if (in_freq->flags.valid_lock != out_freq->flags.valid_lock) {
        return false;
      }

      if (in_freq->flags.valid_pr) {
        if (in_freq->code != out_freq->code ||
            fabs(in_freq->pseudorange - out_freq->pseudorange -
                 amb * PRUNIT_GLO) > 0.01) {
          return false;
        }
      }
      if (in_freq->flags.valid_cp) {
        int fcn = msg_in->sats[in_sat_idx].fcn - 7;
        double frequency = freq == L1_FREQ ? GLO_L1_FREQ + fcn * GLO_L1_CH_OFFSET : GLO_L1_FREQ + fcn * GLO_L2_CH_OFFSET;
        if (fabs(in_freq->carrier_phase - out_freq->carrier_phase ) -
            ((double)amb * PRUNIT_GLO / (CLIGHT / frequency)) >
            0.0005 / (CLIGHT / frequency)) {
          return false;
        }
      }
      if (in_freq->flags.valid_cnr) {
        if (fabs(in_freq->cnr - out_freq->cnr) > 0.125) {
          return false;
        }
      }
      if (in_freq->flags.valid_lock) {
        if( in_freq->lock < 24 ) {
          if( out_freq->lock >= 24 ) {
            return false;
          }
        }
        else if( in_freq->lock < 72 ) {
          if( out_freq->lock < 24 || out_freq->lock >= 72 )
          {
            return false;
          }
        }
        else if( in_freq->lock < 168 ) {
          if (out_freq->lock < 72 || out_freq->lock >= 168)
          {
            return false;
          }
        }
        else if( in_freq->lock < 360 ) {
          if( out_freq->lock < 168 || out_freq->lock >= 360
            ) {
            return false;
          }
        }
        else if( in_freq->lock < 744 ) {
          if( out_freq->lock < 360 || out_freq->lock >= 744
            ) {
            return false;
          }
        }
        else if( in_freq->lock < 937 ) {
          if( out_freq->lock < 744 || out_freq->lock >= 937
            ) {
            return false;
          }
        }
        else {
          if( out_freq->lock < 937 ) {
            return false;
          }
        }
      }
    }
    ++out_sat_idx;
  }

  return true;
}

bool msg1005_equals(const rtcm_msg_1005 *lhs, const rtcm_msg_1005 *rhs)
{
  if (lhs->stn_id != rhs->stn_id) {
    return false;
  }
  if (lhs->ITRF != rhs->ITRF) {
    return false;
  }
  if (lhs->GPS_ind != rhs->GPS_ind) {
    return false;
  }
  if (lhs->GLO_ind != rhs->GLO_ind) {
    return false;
  }
  if (lhs->GAL_ind != rhs->GAL_ind) {
    return false;
  }
  if (lhs->ref_stn_ind != rhs->ref_stn_ind) {
    return false;
  }
  if (lhs->osc_ind != rhs->osc_ind) {
    return false;
  }
  if (lhs->quart_cycle_ind != rhs->quart_cycle_ind) {
    return false;
  }
  if (fabs(lhs->arp_x - rhs->arp_x) > 0.00005) {
    return false;
  }
  if (fabs(lhs->arp_y - rhs->arp_y) > 0.00005) {
    return false;
  }
  if (fabs(lhs->arp_z - rhs->arp_z) > 0.00005) {
    return false;
  }

  return true;
}

bool msg1006_equals(const rtcm_msg_1006 *lhs, const rtcm_msg_1006 *rhs)
{
  if (fabs(lhs->ant_height - rhs->ant_height) > 0.00005) {
    return false;
  }

  return msg1005_equals(&lhs->msg_1005, &rhs->msg_1005);
}

bool msg1007_equals(const rtcm_msg_1007 *lhs, const rtcm_msg_1007 *rhs)
{
  if (lhs->stn_id != rhs->stn_id) {
    return false;
  }
  if (lhs->desc_count != rhs->desc_count) {
    return false;
  }
  for (uint8_t ch = 0; ch < lhs->desc_count; ++ch) {
    if (lhs->desc[ch] != rhs->desc[ch]) {
      return false;
    }
  }
  if (lhs->ant_id != rhs->ant_id) {
    return false;
  }

  return true;
}

bool msg1008_equals(const rtcm_msg_1008 *lhs, const rtcm_msg_1008 *rhs)
{
  for (uint8_t ch = 0; ch < lhs->serial_count; ++ch) {
    if (lhs->serial_num[ch] != rhs->serial_num[ch]) {
      return false;
    }
  }

  return msg1007_equals(&lhs->msg_1007, &rhs->msg_1007);
}
