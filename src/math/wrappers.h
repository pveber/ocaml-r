/*********************************************************************************/
/*                OCaml-R                                                        */
/*                                                                               */
/*    Copyright (C) 2008 Institut National de Recherche en Informatique et       */
/*    en Automatique. All rights reserved.                                       */
/*                                                                               */
/*    This program is free software; you can redistribute it and/or modify       */
/*    it under the terms of the GNU General Public License as                    */
/*    published by the Free Software Foundation; either version 2 of the         */
/*    License, or  any later version.                                            */
/*                                                                               */
/*    This program is distributed in the hope that it will be useful,            */
/*    but WITHOUT ANY WARRANTY; without even the implied warranty of             */
/*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              */
/*    GNU Library General Public License for more details.                       */
/*                                                                               */
/*    You should have received a copy of the GNU General Public                  */
/*    License along with this program; if not, write to the Free Software        */
/*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   */
/*    02111-1307  USA                                                            */
/*                                                                               */
/*    Contact: Maxence.Guesdon@inria.fr                                          */
/*                                                                               */
/*********************************************************************************/

/* Wrappers for C functions. */

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>

#define ID(x) (x)

#define ML_0(cname, conv) \
CAMLprim value ml_##cname (value unit) { return conv (cname ()); }
#define ML_1(cname, conv1, conv) \
CAMLprim value ml_##cname (value arg1) { return conv (cname (conv1 (arg1))); }
#define ML_1_post(cname, conv1, conv, post) \
CAMLprim value ml_##cname (value arg1) \
{ value ret = conv (cname (conv1(arg1))); post; return ret; }
#define ML_2(cname, conv1, conv2, conv) \
CAMLprim value ml_##cname (value arg1, value arg2) \
{ return conv (cname (conv1(arg1), conv2(arg2))); }
#define ML_2_name(mlname, cname, conv1, conv2, conv) \
CAMLprim value mlname (value arg1, value arg2) \
{ return conv (cname (conv1(arg1), conv2(arg2))); }
#define ML_3(cname, conv1, conv2, conv3, conv) \
CAMLprim value ml_##cname (value arg1, value arg2, value arg3) \
{ return conv (cname (conv1(arg1), conv2(arg2), conv3(arg3))); }
#define ML_3_name(mlname, cname, conv1, conv2, conv3, conv) \
CAMLprim value mlname (value arg1, value arg2, value arg3) \
{ return conv (cname (conv1(arg1), conv2(arg2), conv3(arg3))); }
#define ML_4(cname, conv1, conv2, conv3, conv4, conv) \
CAMLprim value ml_##cname (value arg1, value arg2, value arg3, value arg4) \
{ return conv (cname (conv1(arg1), conv2(arg2), conv3(arg3), conv4(arg4))); }
#define ML_4_name(mlname, cname, conv1, conv2, conv3, conv4, conv) \
CAMLprim value mlname (value arg1, value arg2, value arg3, value arg4) \
{ return conv (cname (conv1(arg1), conv2(arg2), conv3(arg3), conv4(arg4))); }
#define ML_5(cname, conv1, conv2, conv3, conv4, conv5, conv) \
CAMLprim value ml_##cname (value arg1, value arg2, value arg3, value arg4, \
                           value arg5) \
{ return conv (cname (conv1(arg1), conv2(arg2), conv3(arg3), conv4(arg4), \
                      conv5(arg5))); }
#define ML_5_name(mlname, cname, conv1, conv2, conv3, conv4, conv5, conv) \
CAMLprim value mlname (value arg1, value arg2, value arg3, value arg4, \
                       value arg5) \
{ return conv (cname (conv1(arg1), conv2(arg2), conv3(arg3), conv4(arg4), \
                      conv5(arg5))); }
#define ML_6(cname, conv1, conv2, conv3, conv4, conv5, conv6, conv) \
CAMLprim value ml_##cname (value arg1, value arg2, value arg3, value arg4, \
                           value arg5, value arg6) \
{ return conv (cname (conv1(arg1), conv2(arg2), conv3(arg3), conv4(arg4), \
                      conv5(arg5), conv6(arg6))); }
#define ML_7(cname, conv1, conv2, conv3, conv4, conv5, conv6, conv7, conv) \
CAMLprim value ml_##cname (value arg1, value arg2, value arg3, value arg4, \
                           value arg5, value arg6, value arg7) \
{ return conv (cname (conv1(arg1), conv2(arg2), conv3(arg3), conv4(arg4), \
                      conv5(arg5), conv6(arg6), conv7(arg7))); }
#define ML_8(cname, conv1, conv2, conv3, conv4, conv5, conv6, conv7, conv8, \
             conv) \
CAMLprim value ml_##cname (value arg1, value arg2, value arg3, value arg4, \
                           value arg5, value arg6, value arg7, value arg8) \
{ return conv (cname (conv1(arg1), conv2(arg2), conv3(arg3), conv4(arg4), \
                      conv5(arg5), conv6(arg6), conv7(arg7), conv8(arg8))); }
#define ML_9(cname, conv1, conv2, conv3, conv4, conv5, conv6, conv7, conv8, \
              conv9, conv) \
CAMLprim value ml_##cname (value arg1, value arg2, value arg3, value arg4, \
                           value arg5, value arg6, value arg7, value arg8, \
                           value arg9) \
{ return conv (cname (conv1(arg1), conv2(arg2), conv3(arg3), conv4(arg4), \
                      conv5(arg5), conv6(arg6), conv7(arg7), conv8(arg8), \
                      conv9(arg9))); }
#define ML_9_name(mlname, cname, conv1, conv2, conv3, conv4, conv5, conv6, \
                  conv7, conv8, conv9, conv) \
CAMLprim value mlname (value arg1, value arg2, value arg3, value arg4, \
                       value arg5, value arg6, value arg7, value arg8, \
                       value arg9) \
{ return conv (cname (conv1(arg1), conv2(arg2), conv3(arg3), conv4(arg4), \
                      conv5(arg5), conv6(arg6), conv7(arg7), conv8(arg8), \
                      conv9(arg9))); }
#define ML_10(cname, conv1, conv2, conv3, conv4, conv5, conv6, conv7, conv8, \
              conv9, conv10, conv) \
CAMLprim value ml_##cname (value arg1, value arg2, value arg3, value arg4, \
                           value arg5, value arg6, value arg7, value arg8, \
                           value arg9, value arg10)\
{ return conv (cname (conv1(arg1), conv2(arg2), conv3(arg3), conv4(arg4), \
                      conv5(arg5), conv6(arg6), conv7(arg7), conv8(arg8), \
                      conv9(arg9), conv10(arg10))); }
#define ML_11(cname, conv1, conv2, conv3, conv4, conv5, conv6, conv7, conv8, \
              conv9, conv10, conv11, conv) \
CAMLprim value ml_##cname (value arg1, value arg2, value arg3, value arg4, \
                           value arg5, value arg6, value arg7, value arg8, \
                           value arg9, value arg10, value arg11) \
{ return conv (cname (conv1(arg1), conv2(arg2), conv3(arg3), conv4(arg4), \
                      conv5(arg5), conv6(arg6), conv7(arg7), conv8(arg8), \
                      conv9(arg9), conv10(arg10), conv11(arg11))); }
#define ML_12(cname, conv1, conv2, conv3, conv4, conv5, conv6, conv7, conv8, \
              conv9, conv10, conv11, conv12, conv) \
CAMLprim value ml_##cname (value arg1, value arg2, value arg3, value arg4, \
                           value arg5, value arg6, value arg7, value arg8, \
                           value arg9, value arg10, value arg11, value arg12) \
{ return conv (cname (conv1(arg1), conv2(arg2), conv3(arg3), conv4(arg4), \
                      conv5(arg5), conv6(arg6), conv7(arg7), conv8(arg8), \
                      conv9(arg9), conv10(arg10), conv11(arg11), \
                      conv12(arg12))); }
#define ML_13(cname, conv1, conv2, conv3, conv4, conv5, conv6, conv7, conv8, \
              conv9, conv10, conv11, conv12, conv13, conv) \
CAMLprim value ml_##cname (value arg1, value arg2, value arg3, value arg4, \
                           value arg5, value arg6, value arg7, value arg8, \
                           value arg9, value arg10, value arg11, value arg12, \
                           value arg13) \
{ return conv (cname (conv1(arg1), conv2(arg2), conv3(arg3), conv4(arg4), \
                      conv5(arg5), conv6(arg6), conv7(arg7), conv8(arg8), \
                      conv9(arg9), conv10(arg10), conv11(arg11), \
                      conv12(arg12), conv13(arg13))); }
/* For more than 5 arguments */
#define ML_bc6(cname) \
CAMLprim value cname##_bc (value *argv, int argn) \
{ return cname(argv[0],argv[1],argv[2],argv[3],argv[4],argv[5]); }
#define ML_bc7(cname) \
CAMLprim value cname##_bc (value *argv, int argn) \
{ return cname(argv[0],argv[1],argv[2],argv[3],argv[4],argv[5],argv[6]); }
#define ML_bc8(cname) \
CAMLprim value cname##_bc (value *argv, int argn) \
{ return cname(argv[0],argv[1],argv[2],argv[3],argv[4],argv[5],argv[6], \
               argv[7]); }
#define ML_bc9(cname) \
CAMLprim value cname##_bc (value *argv, int argn) \
{ return cname(argv[0],argv[1],argv[2],argv[3],argv[4],argv[5],argv[6], \
               argv[7],argv[8]); }
#define ML_bc10(cname) \
CAMLprim value cname##_bc (value *argv, int argn) \
{ return cname(argv[0],argv[1],argv[2],argv[3],argv[4],argv[5],argv[6], \
               argv[7],argv[8],argv[9]); }
#define ML_bc11(cname) \
CAMLprim value cname##_bc (value *argv, int argn) \
{ return cname(argv[0],argv[1],argv[2],argv[3],argv[4],argv[5],argv[6], \
               argv[7],argv[8],argv[9],argv[10]); }
#define ML_bc12(cname) \
CAMLprim value cname##_bc (value *argv, int argn) \
{ return cname(argv[0],argv[1],argv[2],argv[3],argv[4],argv[5],argv[6], \
               argv[7],argv[8],argv[9],argv[10],argv[11]); }
#define ML_bc13(cname) \
CAMLprim value cname##_bc (value *argv, int argn) \
{ return cname(argv[0],argv[1],argv[2],argv[3],argv[4],argv[5],argv[6], \
               argv[7],argv[8],argv[9],argv[10],argv[11],argv[12]); }
