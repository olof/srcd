% (This is testdata. This is unrelated to licensing of this
%  software distribution. Any similarities are coincidental.
%  Send your license scanner my regards!)

-define(BSD_LICENSE_MD5, "3775480a712fc46a69647678acb234cb").
-define(BSD_LICENSE_TEXT, "Copyright (c) The Regents of the University of California.
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:
1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. Neither the name of the University nor the names of its contributors
   may be used to endorse or promote products derived from this software
   without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
SUCH DAMAGE.").

% zopfli 1.0.3-1build1, --deflate
% (No, it doesn't directly output to erlang binary. That would be something.)
-define(BSD_LICENSE_DEFLATE, <<180, 146, 65, 175, 19, 49, 12, 132, 239, 249,
			       21, 115, 4, 169, 90, 4, 220, 184, 161, 119,
			       231, 240, 4, 63, 32, 77, 188, 173, 165, 77,
			       92, 217, 222, 86, 253, 247, 196, 91, 173,
			       250, 0, 174, 239, 180, 218, 140, 60, 249,
			       198, 153, 23, 185, 220, 149, 79, 103, 199,
			       135, 242, 17, 63, 207, 132, 87, 58, 81, 119,
			       131, 204, 240, 241, 251, 171, 243, 149, 212,
			       216, 239, 113, 242, 146, 23, 158, 69, 59,
			       231, 41, 125, 95, 22, 108, 163, 6, 37, 35,
			       189, 82, 157, 82, 122, 165, 202, 230, 202,
			       199, 213, 89, 58, 114, 175, 88, 141, 192,
			       29, 38, 171, 22, 218, 78, 142, 220, 179, 222,
			       49, 156, 154, 29, 112, 99, 63, 67, 116, 251,
			       202, 234, 169, 73, 229, 153, 75, 14, 131,
			       3, 178, 18, 46, 164, 141, 221, 169, 226, 162,
			       114, 229, 74, 117, 176, 101, 223, 0, 103,
			       89, 22, 185, 113, 63, 161, 72, 175, 28, 67,
			       150, 98, 168, 145, 127, 75, 159, 39, 252,
			       73, 20, 193, 118, 148, 34, 149, 208, 86, 115,
			       40, 121, 230, 190, 249, 229, 163, 92, 135,
			       180, 239, 37, 1, 232, 226, 92, 232, 48, 100,
			       54, 44, 108, 30, 30, 207, 219, 34, 210, 95,
			       40, 149, 173, 44, 153, 27, 233, 148, 190,
			       252, 139, 192, 253, 237, 10, 118, 132, 145,
			       173, 174, 133, 222, 131, 2, 143, 112, 97,
			       83, 165, 172, 141, 186, 231, 253, 125, 62,
			       137, 66, 134, 168, 104, 217, 73, 57, 47, 246,
			       92, 115, 188, 201, 230, 250, 54, 192, 148,
			       190, 78, 248, 65, 28, 67, 155, 216, 115, 163,
			       255, 20, 166, 203, 83, 182, 208, 217, 45,
			       136, 31, 70, 162, 22, 60, 45, 223, 113, 164,
			       40, 73, 133, 11, 168, 215, 33, 16, 68, 3,
			       162, 137, 19, 30, 123, 113, 67, 37, 29, 222,
			       21, 243, 16, 30, 107, 48, 153, 253, 150, 53,
			       130, 237, 245, 129, 93, 168, 68, 127, 198,
			       28, 71, 171, 52, 154, 211, 31, 29, 50, 219,
			       240, 127, 23, 74, 30, 73, 150, 195, 32, 24,
			       222, 235, 20, 172, 38, 186, 250, 2, 179, 194,
			       18, 126, 166, 74, 22, 30, 64, 47, 236, 250,
			       254, 167, 24, 152, 156, 123, 229, 244, 135,
			       250, 63, 171, 248, 206, 6, 38, 155, 223, 80,
			       9, 226, 254, 84, 185, 114, 163, 6, 235, 3,
			       124, 39, 80, 186, 208, 112, 3, 28, 13, 170,
			       12, 87, 94, 167, 139, 26, 60, 63, 163, 133,
			       225, 245, 235, 252, 84, 112, 60, 128, 238,
			       167, 146, 25, 136, 2, 31, 103, 231, 8, 137,
			       84, 197, 225, 76, 182, 0, 143, 218, 103, 227,
			       113, 89, 32, 34, 96, 136, 67, 231, 131, 61,
			       100, 46, 75, 150, 149, 63, 109, 32, 27, 28,
			       164, 117, 143, 71, 92, 185, 179, 63, 178,
			       15, 54, 246, 145, 93, 91, 148, 33, 156, 168,
			       206, 117, 118, 84, 56, 167, 158, 98, 84, 114,
			       78, 99, 171, 29, 249, 160, 246, 4, 81, 31,
			       149, 64, 215, 88, 3, 182, 99, 239, 191, 204,
			       19, 253, 117, 221, 74, 208, 25, 215, 78, 37,
			       27, 114, 93, 99, 165, 234, 57, 227, 199, 93,
			       229, 22, 110, 236, 11, 216, 73, 149, 227,
			       38, 32, 80, 140, 64, 125, 44, 95, 51, 141,
			       62, 206, 16, 197, 199, 210, 240, 192, 75,
			       108, 122, 243, 2, 138, 83, 165, 78, 165, 35,
			       108, 185, 223, 230, 106, 206, 62, 157, 224,
			       34, 210, 172, 68, 176, 145, 94, 185, 146,
			       125, 128, 46, 102, 169, 154, 70, 11, 52, 116,
			       204, 226, 140, 8, 68, 241, 89, 52, 42, 140,
			       19, 22, 240, 112, 82, 157, 167, 179, 140,
			       183, 101, 151, 91, 208, 80, 168, 24, 214,
			       150, 84, 65, 70, 78, 77, 48, 162, 143, 12,
			       237, 252, 21, 250, 2, 183, 157, 226, 189,
			       70, 200, 23, 82, 152, 8, 204, 149, 171, 151,
			       159, 100, 209, 23, 0, 253, 167, 141, 48, 232,
			       210, 57, 48, 87, 202, 175, 146, 41, 55, 54,
			       122, 11, 168, 108, 41, 224, 47, 181, 55, 124,
			       20, 153, 57, 25, 66, 147, 131, 242, 246, 151,
			       19, 186, 64, 254, 64, 224, 13, 176, 93, 35,
			       163, 125, 19, 159, 65, 129, 191, 158, 15,
			       217, 138, 205, 186, 195, 23, 220, 79, 229,
			       19>>).
