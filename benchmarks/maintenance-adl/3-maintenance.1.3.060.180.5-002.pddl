(define (problem maintenance-scheduling-1-3-60-180-5-2)
 (:domain maintenance-scheduling-domain)
 (:objects d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 d11 d12 d13 d14 d15 d16 d17 d18 d19 d20 d21 d22 d23 d24 d25 d26 d27 d28 d29 d30 d31 d32 d33 d34 d35 d36 d37 d38 d39 d40 d41 d42 d43 d44 d45 d46 d47 d48 d49 d50 d51 d52 d53 d54 d55 d56 d57 d58 d59 d60 d61 - day
   FRA BER HAM - airport
   ap1 ap2 ap3 ap4 ap5 ap6 ap7 ap8 ap9 ap10 ap11 ap12 ap13 ap14 ap15 ap16 ap17 ap18 ap19 ap20 ap21 ap22 ap23 ap24 ap25 ap26 ap27 ap28 ap29 ap30 ap31 ap32 ap33 ap34 ap35 ap36 ap37 ap38 ap39 ap40 ap41 ap42 ap43 ap44 ap45 ap46 ap47 ap48 ap49 ap50 ap51 ap52 ap53 ap54 ap55 ap56 ap57 ap58 ap59 ap60 ap61 ap62 ap63 ap64 ap65 ap66 ap67 ap68 ap69 ap70 ap71 ap72 ap73 ap74 ap75 ap76 ap77 ap78 ap79 ap80 ap81 ap82 ap83 ap84 ap85 ap86 ap87 ap88 ap89 ap90 ap91 ap92 ap93 ap94 ap95 ap96 ap97 ap98 ap99 ap100 ap101 ap102 ap103 ap104 ap105 ap106 ap107 ap108 ap109 ap110 ap111 ap112 ap113 ap114 ap115 ap116 ap117 ap118 ap119 ap120 ap121 ap122 ap123 ap124 ap125 ap126 ap127 ap128 ap129 ap130 ap131 ap132 ap133 ap134 ap135 ap136 ap137 ap138 ap139 ap140 ap141 ap142 ap143 ap144 ap145 ap146 ap147 ap148 ap149 ap150 ap151 ap152 ap153 ap154 ap155 ap156 ap157 ap158 ap159 ap160 ap161 ap162 ap163 ap164 ap165 ap166 ap167 ap168 ap169 ap170 ap171 ap172 ap173 ap174 ap175 ap176 ap177 ap178 ap179 ap180 - plane)
 (:init
  (today d1)  (today d2)  (today d3)  (today d4)  (today d5)  (today d6)  (today d7)  (today d8)  (today d9)  (today d10)  (today d11)  (today d12)  (today d13)  (today d14)  (today d15)  (today d16)  (today d17)  (today d18)  (today d19)  (today d20)  (today d21)  (today d22)  (today d23)  (today d24)  (today d25)  (today d26)  (today d27)  (today d28)  (today d29)  (today d30)  (today d31)  (today d32)  (today d33)  (today d34)  (today d35)  (today d36)  (today d37)  (today d38)  (today d39)  (today d40)  (today d41)  (today d42)  (today d43)  (today d44)  (today d45)  (today d46)  (today d47)  (today d48)  (today d49)  (today d50)  (today d51)  (today d52)  (today d53)  (today d54)  (today d55)  (today d56)  (today d57)  (today d58)  (today d59)  (today d60)  (at ap1 d25 FRA)
  (at ap1 d8 BER)
  (at ap1 d36 BER)
  (at ap1 d21 HAM)
  (at ap1 d29 HAM)
  (at ap2 d53 FRA)
  (at ap2 d11 BER)
  (at ap2 d24 BER)
  (at ap2 d6 HAM)
  (at ap2 d37 HAM)
  (at ap3 d4 BER)
  (at ap3 d24 BER)
  (at ap3 d4 HAM)
  (at ap3 d41 HAM)
  (at ap3 d48 HAM)
  (at ap4 d29 FRA)
  (at ap4 d37 FRA)
  (at ap4 d50 FRA)
  (at ap4 d38 BER)
  (at ap4 d60 HAM)
  (at ap5 d41 FRA)
  (at ap5 d49 FRA)
  (at ap5 d38 BER)
  (at ap5 d13 HAM)
  (at ap5 d29 HAM)
  (at ap6 d21 FRA)
  (at ap6 d36 FRA)
  (at ap6 d34 BER)
  (at ap6 d6 HAM)
  (at ap6 d20 HAM)
  (at ap7 d17 FRA)
  (at ap7 d26 FRA)
  (at ap7 d8 BER)
  (at ap7 d47 HAM)
  (at ap7 d53 HAM)
  (at ap8 d18 FRA)
  (at ap8 d35 BER)
  (at ap8 d3 HAM)
  (at ap8 d20 HAM)
  (at ap8 d27 HAM)
  (at ap9 d20 FRA)
  (at ap9 d37 FRA)
  (at ap9 d38 BER)
  (at ap9 d47 BER)
  (at ap9 d53 HAM)
  (at ap10 d13 FRA)
  (at ap10 d24 FRA)
  (at ap10 d24 BER)
  (at ap10 d42 BER)
  (at ap10 d36 HAM)
  (at ap11 d23 BER)
  (at ap11 d24 BER)
  (at ap11 d36 BER)
  (at ap11 d30 HAM)
  (at ap11 d44 HAM)
  (at ap12 d3 FRA)
  (at ap12 d39 FRA)
  (at ap12 d38 BER)
  (at ap12 d4 HAM)
  (at ap12 d15 HAM)
  (at ap13 d7 BER)
  (at ap13 d8 BER)
  (at ap13 d10 BER)
  (at ap13 d14 BER)
  (at ap13 d24 BER)
  (at ap14 d33 FRA)
  (at ap14 d42 FRA)
  (at ap14 d42 FRA)
  (at ap14 d59 FRA)
  (at ap14 d13 BER)
  (at ap15 d55 FRA)
  (at ap15 d57 FRA)
  (at ap15 d35 BER)
  (at ap15 d38 BER)
  (at ap15 d53 BER)
  (at ap16 d12 FRA)
  (at ap16 d17 FRA)
  (at ap16 d7 BER)
  (at ap16 d14 HAM)
  (at ap16 d56 HAM)
  (at ap17 d22 FRA)
  (at ap17 d41 FRA)
  (at ap17 d4 BER)
  (at ap17 d47 HAM)
  (at ap17 d55 HAM)
  (at ap18 d28 FRA)
  (at ap18 d42 FRA)
  (at ap18 d46 FRA)
  (at ap18 d22 BER)
  (at ap18 d51 HAM)
  (at ap19 d22 FRA)
  (at ap19 d1 BER)
  (at ap19 d13 BER)
  (at ap19 d2 HAM)
  (at ap19 d4 HAM)
  (at ap20 d45 FRA)
  (at ap20 d47 FRA)
  (at ap20 d58 FRA)
  (at ap20 d16 HAM)
  (at ap20 d44 HAM)
  (at ap21 d20 FRA)
  (at ap21 d27 FRA)
  (at ap21 d54 FRA)
  (at ap21 d4 BER)
  (at ap21 d48 BER)
  (at ap22 d38 FRA)
  (at ap22 d55 FRA)
  (at ap22 d57 FRA)
  (at ap22 d28 BER)
  (at ap22 d34 HAM)
  (at ap23 d25 FRA)
  (at ap23 d44 FRA)
  (at ap23 d13 BER)
  (at ap23 d21 HAM)
  (at ap23 d25 HAM)
  (at ap24 d2 FRA)
  (at ap24 d20 FRA)
  (at ap24 d36 FRA)
  (at ap24 d10 BER)
  (at ap24 d41 BER)
  (at ap25 d31 FRA)
  (at ap25 d38 FRA)
  (at ap25 d43 FRA)
  (at ap25 d50 BER)
  (at ap25 d56 HAM)
  (at ap26 d3 BER)
  (at ap26 d11 BER)
  (at ap26 d15 BER)
  (at ap26 d27 BER)
  (at ap26 d33 BER)
  (at ap27 d50 FRA)
  (at ap27 d23 BER)
  (at ap27 d24 BER)
  (at ap27 d16 HAM)
  (at ap27 d37 HAM)
  (at ap28 d6 FRA)
  (at ap28 d13 BER)
  (at ap28 d50 BER)
  (at ap28 d8 HAM)
  (at ap28 d29 HAM)
  (at ap29 d50 FRA)
  (at ap29 d9 BER)
  (at ap29 d32 BER)
  (at ap29 d25 HAM)
  (at ap29 d37 HAM)
  (at ap30 d57 FRA)
  (at ap30 d5 BER)
  (at ap30 d46 BER)
  (at ap30 d58 BER)
  (at ap30 d51 HAM)
  (at ap31 d35 BER)
  (at ap31 d49 BER)
  (at ap31 d14 HAM)
  (at ap31 d31 HAM)
  (at ap31 d31 HAM)
  (at ap32 d1 HAM)
  (at ap32 d2 HAM)
  (at ap32 d8 HAM)
  (at ap32 d22 HAM)
  (at ap32 d60 HAM)
  (at ap33 d36 FRA)
  (at ap33 d59 FRA)
  (at ap33 d51 BER)
  (at ap33 d54 BER)
  (at ap33 d21 HAM)
  (at ap34 d28 FRA)
  (at ap34 d40 BER)
  (at ap34 d56 BER)
  (at ap34 d12 HAM)
  (at ap34 d55 HAM)
  (at ap35 d59 FRA)
  (at ap35 d26 BER)
  (at ap35 d57 BER)
  (at ap35 d10 HAM)
  (at ap35 d22 HAM)
  (at ap36 d16 FRA)
  (at ap36 d44 FRA)
  (at ap36 d54 FRA)
  (at ap36 d27 BER)
  (at ap36 d47 HAM)
  (at ap37 d5 BER)
  (at ap37 d43 BER)
  (at ap37 d13 HAM)
  (at ap37 d29 HAM)
  (at ap37 d53 HAM)
  (at ap38 d2 FRA)
  (at ap38 d2 HAM)
  (at ap38 d10 HAM)
  (at ap38 d17 HAM)
  (at ap38 d53 HAM)
  (at ap39 d60 FRA)
  (at ap39 d43 BER)
  (at ap39 d4 HAM)
  (at ap39 d8 HAM)
  (at ap39 d17 HAM)
  (at ap40 d8 FRA)
  (at ap40 d1 BER)
  (at ap40 d23 BER)
  (at ap40 d58 BER)
  (at ap40 d22 HAM)
  (at ap41 d11 FRA)
  (at ap41 d1 BER)
  (at ap41 d7 HAM)
  (at ap41 d8 HAM)
  (at ap41 d58 HAM)
  (at ap42 d58 FRA)
  (at ap42 d47 BER)
  (at ap42 d1 HAM)
  (at ap42 d5 HAM)
  (at ap42 d57 HAM)
  (at ap43 d16 FRA)
  (at ap43 d12 HAM)
  (at ap43 d48 HAM)
  (at ap43 d55 HAM)
  (at ap43 d58 HAM)
  (at ap44 d31 FRA)
  (at ap44 d46 FRA)
  (at ap44 d50 FRA)
  (at ap44 d46 BER)
  (at ap44 d53 HAM)
  (at ap45 d21 FRA)
  (at ap45 d24 BER)
  (at ap45 d52 BER)
  (at ap45 d7 HAM)
  (at ap45 d9 HAM)
  (at ap46 d17 FRA)
  (at ap46 d18 BER)
  (at ap46 d36 BER)
  (at ap46 d37 BER)
  (at ap46 d38 HAM)
  (at ap47 d33 BER)
  (at ap47 d46 BER)
  (at ap47 d18 HAM)
  (at ap47 d33 HAM)
  (at ap47 d58 HAM)
  (at ap48 d43 FRA)
  (at ap48 d42 BER)
  (at ap48 d57 HAM)
  (at ap48 d57 HAM)
  (at ap48 d59 HAM)
  (at ap49 d60 FRA)
  (at ap49 d24 BER)
  (at ap49 d52 BER)
  (at ap49 d21 HAM)
  (at ap49 d43 HAM)
  (at ap50 d41 FRA)
  (at ap50 d43 FRA)
  (at ap50 d10 BER)
  (at ap50 d12 BER)
  (at ap50 d12 HAM)
  (at ap51 d9 FRA)
  (at ap51 d12 BER)
  (at ap51 d40 BER)
  (at ap51 d25 HAM)
  (at ap51 d44 HAM)
  (at ap52 d41 FRA)
  (at ap52 d25 BER)
  (at ap52 d46 BER)
  (at ap52 d27 HAM)
  (at ap52 d48 HAM)
  (at ap53 d12 FRA)
  (at ap53 d28 HAM)
  (at ap53 d32 HAM)
  (at ap53 d44 HAM)
  (at ap53 d56 HAM)
  (at ap54 d25 FRA)
  (at ap54 d58 FRA)
  (at ap54 d45 BER)
  (at ap54 d52 BER)
  (at ap54 d6 HAM)
  (at ap55 d1 FRA)
  (at ap55 d13 FRA)
  (at ap55 d5 BER)
  (at ap55 d33 BER)
  (at ap55 d48 HAM)
  (at ap56 d7 FRA)
  (at ap56 d10 FRA)
  (at ap56 d32 FRA)
  (at ap56 d52 BER)
  (at ap56 d55 BER)
  (at ap57 d20 FRA)
  (at ap57 d22 FRA)
  (at ap57 d41 BER)
  (at ap57 d50 BER)
  (at ap57 d1 HAM)
  (at ap58 d9 FRA)
  (at ap58 d4 BER)
  (at ap58 d43 BER)
  (at ap58 d47 BER)
  (at ap58 d60 HAM)
  (at ap59 d5 FRA)
  (at ap59 d14 FRA)
  (at ap59 d44 BER)
  (at ap59 d60 BER)
  (at ap59 d38 HAM)
  (at ap60 d10 BER)
  (at ap60 d12 BER)
  (at ap60 d49 BER)
  (at ap60 d14 HAM)
  (at ap60 d52 HAM)
  (at ap61 d21 FRA)
  (at ap61 d36 FRA)
  (at ap61 d4 BER)
  (at ap61 d59 BER)
  (at ap61 d32 HAM)
  (at ap62 d8 FRA)
  (at ap62 d32 FRA)
  (at ap62 d57 BER)
  (at ap62 d41 HAM)
  (at ap62 d53 HAM)
  (at ap63 d31 FRA)
  (at ap63 d41 FRA)
  (at ap63 d31 BER)
  (at ap63 d51 BER)
  (at ap63 d48 HAM)
  (at ap64 d5 FRA)
  (at ap64 d43 FRA)
  (at ap64 d11 BER)
  (at ap64 d18 BER)
  (at ap64 d35 HAM)
  (at ap65 d3 FRA)
  (at ap65 d1 BER)
  (at ap65 d41 BER)
  (at ap65 d15 HAM)
  (at ap65 d22 HAM)
  (at ap66 d11 FRA)
  (at ap66 d38 BER)
  (at ap66 d10 HAM)
  (at ap66 d10 HAM)
  (at ap66 d32 HAM)
  (at ap67 d20 BER)
  (at ap67 d41 BER)
  (at ap67 d56 BER)
  (at ap67 d8 HAM)
  (at ap67 d56 HAM)
  (at ap68 d19 FRA)
  (at ap68 d18 BER)
  (at ap68 d20 BER)
  (at ap68 d28 HAM)
  (at ap68 d33 HAM)
  (at ap69 d53 FRA)
  (at ap69 d59 FRA)
  (at ap69 d8 BER)
  (at ap69 d23 BER)
  (at ap69 d4 HAM)
  (at ap70 d28 BER)
  (at ap70 d49 BER)
  (at ap70 d4 HAM)
  (at ap70 d21 HAM)
  (at ap70 d24 HAM)
  (at ap71 d12 FRA)
  (at ap71 d40 FRA)
  (at ap71 d9 BER)
  (at ap71 d18 BER)
  (at ap71 d59 HAM)
  (at ap72 d5 FRA)
  (at ap72 d35 BER)
  (at ap72 d43 BER)
  (at ap72 d14 HAM)
  (at ap72 d17 HAM)
  (at ap73 d1 FRA)
  (at ap73 d7 HAM)
  (at ap73 d16 HAM)
  (at ap73 d34 HAM)
  (at ap73 d38 HAM)
  (at ap74 d10 FRA)
  (at ap74 d11 FRA)
  (at ap74 d51 BER)
  (at ap74 d11 HAM)
  (at ap74 d18 HAM)
  (at ap75 d4 FRA)
  (at ap75 d12 FRA)
  (at ap75 d7 BER)
  (at ap75 d32 BER)
  (at ap75 d56 BER)
  (at ap76 d41 FRA)
  (at ap76 d42 FRA)
  (at ap76 d54 FRA)
  (at ap76 d48 BER)
  (at ap76 d44 HAM)
  (at ap77 d16 FRA)
  (at ap77 d49 FRA)
  (at ap77 d55 FRA)
  (at ap77 d55 FRA)
  (at ap77 d53 HAM)
  (at ap78 d13 FRA)
  (at ap78 d59 FRA)
  (at ap78 d8 HAM)
  (at ap78 d48 HAM)
  (at ap78 d60 HAM)
  (at ap79 d31 FRA)
  (at ap79 d28 BER)
  (at ap79 d54 BER)
  (at ap79 d14 HAM)
  (at ap79 d33 HAM)
  (at ap80 d60 FRA)
  (at ap80 d54 BER)
  (at ap80 d16 HAM)
  (at ap80 d42 HAM)
  (at ap80 d44 HAM)
  (at ap81 d26 FRA)
  (at ap81 d36 FRA)
  (at ap81 d45 FRA)
  (at ap81 d55 FRA)
  (at ap81 d3 HAM)
  (at ap82 d54 FRA)
  (at ap82 d1 BER)
  (at ap82 d9 BER)
  (at ap82 d58 BER)
  (at ap82 d8 HAM)
  (at ap83 d14 FRA)
  (at ap83 d55 FRA)
  (at ap83 d37 BER)
  (at ap83 d58 BER)
  (at ap83 d37 HAM)
  (at ap84 d57 FRA)
  (at ap84 d37 BER)
  (at ap84 d25 HAM)
  (at ap84 d28 HAM)
  (at ap84 d50 HAM)
  (at ap85 d5 BER)
  (at ap85 d43 BER)
  (at ap85 d57 BER)
  (at ap85 d23 HAM)
  (at ap85 d49 HAM)
  (at ap86 d30 FRA)
  (at ap86 d51 FRA)
  (at ap86 d2 BER)
  (at ap86 d40 BER)
  (at ap86 d33 HAM)
  (at ap87 d23 FRA)
  (at ap87 d26 BER)
  (at ap87 d43 BER)
  (at ap87 d22 HAM)
  (at ap87 d39 HAM)
  (at ap88 d11 FRA)
  (at ap88 d42 FRA)
  (at ap88 d13 BER)
  (at ap88 d32 BER)
  (at ap88 d37 HAM)
  (at ap89 d9 FRA)
  (at ap89 d50 BER)
  (at ap89 d51 BER)
  (at ap89 d10 HAM)
  (at ap89 d18 HAM)
  (at ap90 d8 BER)
  (at ap90 d46 HAM)
  (at ap90 d49 HAM)
  (at ap90 d52 HAM)
  (at ap90 d60 HAM)
  (at ap91 d47 BER)
  (at ap91 d30 HAM)
  (at ap91 d48 HAM)
  (at ap91 d51 HAM)
  (at ap91 d56 HAM)
  (at ap92 d15 FRA)
  (at ap92 d17 FRA)
  (at ap92 d51 FRA)
  (at ap92 d30 BER)
  (at ap92 d42 HAM)
  (at ap93 d55 FRA)
  (at ap93 d1 BER)
  (at ap93 d47 BER)
  (at ap93 d54 BER)
  (at ap93 d9 HAM)
  (at ap94 d16 FRA)
  (at ap94 d20 FRA)
  (at ap94 d50 FRA)
  (at ap94 d20 BER)
  (at ap94 d52 HAM)
  (at ap95 d24 FRA)
  (at ap95 d46 FRA)
  (at ap95 d47 BER)
  (at ap95 d19 HAM)
  (at ap95 d60 HAM)
  (at ap96 d32 FRA)
  (at ap96 d26 HAM)
  (at ap96 d28 HAM)
  (at ap96 d41 HAM)
  (at ap96 d55 HAM)
  (at ap97 d60 FRA)
  (at ap97 d12 BER)
  (at ap97 d31 BER)
  (at ap97 d45 BER)
  (at ap97 d54 HAM)
  (at ap98 d15 BER)
  (at ap98 d39 BER)
  (at ap98 d51 BER)
  (at ap98 d59 BER)
  (at ap98 d41 HAM)
  (at ap99 d16 FRA)
  (at ap99 d50 FRA)
  (at ap99 d39 HAM)
  (at ap99 d41 HAM)
  (at ap99 d44 HAM)
  (at ap100 d26 FRA)
  (at ap100 d53 FRA)
  (at ap100 d10 BER)
  (at ap100 d18 BER)
  (at ap100 d21 BER)
  (at ap101 d11 HAM)
  (at ap101 d30 HAM)
  (at ap101 d31 HAM)
  (at ap101 d46 HAM)
  (at ap101 d59 HAM)
  (at ap102 d17 FRA)
  (at ap102 d45 FRA)
  (at ap102 d23 HAM)
  (at ap102 d34 HAM)
  (at ap102 d60 HAM)
  (at ap103 d54 FRA)
  (at ap103 d18 HAM)
  (at ap103 d28 HAM)
  (at ap103 d30 HAM)
  (at ap103 d43 HAM)
  (at ap104 d12 BER)
  (at ap104 d27 BER)
  (at ap104 d2 HAM)
  (at ap104 d40 HAM)
  (at ap104 d60 HAM)
  (at ap105 d12 FRA)
  (at ap105 d38 BER)
  (at ap105 d47 BER)
  (at ap105 d56 BER)
  (at ap105 d58 BER)
  (at ap106 d18 FRA)
  (at ap106 d20 FRA)
  (at ap106 d38 FRA)
  (at ap106 d11 BER)
  (at ap106 d40 BER)
  (at ap107 d55 FRA)
  (at ap107 d13 BER)
  (at ap107 d59 BER)
  (at ap107 d60 BER)
  (at ap107 d58 HAM)
  (at ap108 d10 FRA)
  (at ap108 d30 FRA)
  (at ap108 d16 BER)
  (at ap108 d45 BER)
  (at ap108 d57 HAM)
  (at ap109 d12 FRA)
  (at ap109 d14 FRA)
  (at ap109 d42 FRA)
  (at ap109 d2 HAM)
  (at ap109 d43 HAM)
  (at ap110 d17 FRA)
  (at ap110 d52 FRA)
  (at ap110 d24 BER)
  (at ap110 d49 BER)
  (at ap110 d39 HAM)
  (at ap111 d41 FRA)
  (at ap111 d21 HAM)
  (at ap111 d37 HAM)
  (at ap111 d58 HAM)
  (at ap111 d59 HAM)
  (at ap112 d15 BER)
  (at ap112 d16 HAM)
  (at ap112 d33 HAM)
  (at ap112 d35 HAM)
  (at ap112 d44 HAM)
  (at ap113 d4 FRA)
  (at ap113 d8 BER)
  (at ap113 d33 HAM)
  (at ap113 d51 HAM)
  (at ap113 d57 HAM)
  (at ap114 d54 FRA)
  (at ap114 d6 BER)
  (at ap114 d38 BER)
  (at ap114 d21 HAM)
  (at ap114 d39 HAM)
  (at ap115 d43 FRA)
  (at ap115 d52 FRA)
  (at ap115 d47 BER)
  (at ap115 d12 HAM)
  (at ap115 d39 HAM)
  (at ap116 d11 BER)
  (at ap116 d25 BER)
  (at ap116 d23 HAM)
  (at ap116 d45 HAM)
  (at ap116 d46 HAM)
  (at ap117 d21 FRA)
  (at ap117 d32 FRA)
  (at ap117 d58 FRA)
  (at ap117 d43 BER)
  (at ap117 d31 HAM)
  (at ap118 d10 FRA)
  (at ap118 d13 FRA)
  (at ap118 d19 FRA)
  (at ap118 d49 FRA)
  (at ap118 d2 HAM)
  (at ap119 d2 FRA)
  (at ap119 d4 FRA)
  (at ap119 d12 FRA)
  (at ap119 d16 HAM)
  (at ap119 d55 HAM)
  (at ap120 d47 FRA)
  (at ap120 d53 FRA)
  (at ap120 d33 BER)
  (at ap120 d50 BER)
  (at ap120 d12 HAM)
  (at ap121 d2 FRA)
  (at ap121 d4 FRA)
  (at ap121 d1 HAM)
  (at ap121 d9 HAM)
  (at ap121 d34 HAM)
  (at ap122 d47 FRA)
  (at ap122 d16 BER)
  (at ap122 d55 BER)
  (at ap122 d56 BER)
  (at ap122 d22 HAM)
  (at ap123 d21 FRA)
  (at ap123 d52 FRA)
  (at ap123 d57 FRA)
  (at ap123 d12 BER)
  (at ap123 d24 HAM)
  (at ap124 d11 FRA)
  (at ap124 d24 BER)
  (at ap124 d47 BER)
  (at ap124 d50 BER)
  (at ap124 d18 HAM)
  (at ap125 d16 FRA)
  (at ap125 d20 FRA)
  (at ap125 d28 FRA)
  (at ap125 d13 BER)
  (at ap125 d45 BER)
  (at ap126 d3 FRA)
  (at ap126 d6 FRA)
  (at ap126 d38 FRA)
  (at ap126 d15 BER)
  (at ap126 d30 BER)
  (at ap127 d22 FRA)
  (at ap127 d32 FRA)
  (at ap127 d24 HAM)
  (at ap127 d34 HAM)
  (at ap127 d57 HAM)
  (at ap128 d12 FRA)
  (at ap128 d20 FRA)
  (at ap128 d40 FRA)
  (at ap128 d25 BER)
  (at ap128 d30 HAM)
  (at ap129 d6 FRA)
  (at ap129 d49 FRA)
  (at ap129 d14 BER)
  (at ap129 d16 BER)
  (at ap129 d28 HAM)
  (at ap130 d16 BER)
  (at ap130 d51 BER)
  (at ap130 d51 BER)
  (at ap130 d4 HAM)
  (at ap130 d16 HAM)
  (at ap131 d59 FRA)
  (at ap131 d4 BER)
  (at ap131 d15 BER)
  (at ap131 d23 BER)
  (at ap131 d51 HAM)
  (at ap132 d25 FRA)
  (at ap132 d27 FRA)
  (at ap132 d46 FRA)
  (at ap132 d58 BER)
  (at ap132 d38 HAM)
  (at ap133 d51 FRA)
  (at ap133 d54 FRA)
  (at ap133 d17 HAM)
  (at ap133 d28 HAM)
  (at ap133 d44 HAM)
  (at ap134 d8 FRA)
  (at ap134 d35 FRA)
  (at ap134 d14 BER)
  (at ap134 d46 BER)
  (at ap134 d19 HAM)
  (at ap135 d15 FRA)
  (at ap135 d45 FRA)
  (at ap135 d19 BER)
  (at ap135 d24 HAM)
  (at ap135 d60 HAM)
  (at ap136 d16 FRA)
  (at ap136 d15 BER)
  (at ap136 d4 HAM)
  (at ap136 d10 HAM)
  (at ap136 d41 HAM)
  (at ap137 d37 FRA)
  (at ap137 d19 BER)
  (at ap137 d21 HAM)
  (at ap137 d27 HAM)
  (at ap137 d48 HAM)
  (at ap138 d40 FRA)
  (at ap138 d4 BER)
  (at ap138 d7 BER)
  (at ap138 d40 HAM)
  (at ap138 d58 HAM)
  (at ap139 d39 FRA)
  (at ap139 d1 BER)
  (at ap139 d2 BER)
  (at ap139 d33 BER)
  (at ap139 d43 BER)
  (at ap140 d8 FRA)
  (at ap140 d15 BER)
  (at ap140 d30 BER)
  (at ap140 d42 BER)
  (at ap140 d35 HAM)
  (at ap141 d2 FRA)
  (at ap141 d2 BER)
  (at ap141 d19 BER)
  (at ap141 d13 HAM)
  (at ap141 d49 HAM)
  (at ap142 d25 FRA)
  (at ap142 d30 FRA)
  (at ap142 d36 FRA)
  (at ap142 d5 HAM)
  (at ap142 d47 HAM)
  (at ap143 d10 FRA)
  (at ap143 d12 FRA)
  (at ap143 d35 FRA)
  (at ap143 d34 BER)
  (at ap143 d35 BER)
  (at ap144 d12 BER)
  (at ap144 d57 BER)
  (at ap144 d57 BER)
  (at ap144 d49 HAM)
  (at ap144 d58 HAM)
  (at ap145 d50 FRA)
  (at ap145 d6 BER)
  (at ap145 d44 BER)
  (at ap145 d8 HAM)
  (at ap145 d16 HAM)
  (at ap146 d17 FRA)
  (at ap146 d20 FRA)
  (at ap146 d34 FRA)
  (at ap146 d35 BER)
  (at ap146 d40 BER)
  (at ap147 d18 FRA)
  (at ap147 d20 FRA)
  (at ap147 d10 HAM)
  (at ap147 d13 HAM)
  (at ap147 d16 HAM)
  (at ap148 d20 FRA)
  (at ap148 d29 FRA)
  (at ap148 d29 BER)
  (at ap148 d42 BER)
  (at ap148 d44 BER)
  (at ap149 d8 FRA)
  (at ap149 d43 FRA)
  (at ap149 d58 FRA)
  (at ap149 d16 BER)
  (at ap149 d44 HAM)
  (at ap150 d41 FRA)
  (at ap150 d50 FRA)
  (at ap150 d10 HAM)
  (at ap150 d50 HAM)
  (at ap150 d51 HAM)
  (at ap151 d4 BER)
  (at ap151 d5 BER)
  (at ap151 d10 BER)
  (at ap151 d36 HAM)
  (at ap151 d59 HAM)
  (at ap152 d9 FRA)
  (at ap152 d59 FRA)
  (at ap152 d1 BER)
  (at ap152 d15 BER)
  (at ap152 d15 BER)
  (at ap153 d16 FRA)
  (at ap153 d48 FRA)
  (at ap153 d54 FRA)
  (at ap153 d36 BER)
  (at ap153 d37 BER)
  (at ap154 d6 FRA)
  (at ap154 d29 FRA)
  (at ap154 d41 FRA)
  (at ap154 d54 FRA)
  (at ap154 d55 HAM)
  (at ap155 d25 FRA)
  (at ap155 d31 FRA)
  (at ap155 d31 FRA)
  (at ap155 d24 BER)
  (at ap155 d51 HAM)
  (at ap156 d12 BER)
  (at ap156 d43 BER)
  (at ap156 d52 BER)
  (at ap156 d25 HAM)
  (at ap156 d32 HAM)
  (at ap157 d42 FRA)
  (at ap157 d27 BER)
  (at ap157 d12 HAM)
  (at ap157 d29 HAM)
  (at ap157 d46 HAM)
  (at ap158 d9 FRA)
  (at ap158 d29 FRA)
  (at ap158 d45 FRA)
  (at ap158 d23 HAM)
  (at ap158 d52 HAM)
  (at ap159 d43 FRA)
  (at ap159 d55 FRA)
  (at ap159 d27 BER)
  (at ap159 d47 BER)
  (at ap159 d50 HAM)
  (at ap160 d25 FRA)
  (at ap160 d34 BER)
  (at ap160 d35 BER)
  (at ap160 d54 BER)
  (at ap160 d25 HAM)
  (at ap161 d20 FRA)
  (at ap161 d58 FRA)
  (at ap161 d16 BER)
  (at ap161 d3 HAM)
  (at ap161 d3 HAM)
  (at ap162 d35 FRA)
  (at ap162 d20 BER)
  (at ap162 d39 BER)
  (at ap162 d43 HAM)
  (at ap162 d54 HAM)
  (at ap163 d5 FRA)
  (at ap163 d53 FRA)
  (at ap163 d43 BER)
  (at ap163 d45 BER)
  (at ap163 d48 BER)
  (at ap164 d10 FRA)
  (at ap164 d18 FRA)
  (at ap164 d28 BER)
  (at ap164 d8 HAM)
  (at ap164 d42 HAM)
  (at ap165 d34 FRA)
  (at ap165 d30 BER)
  (at ap165 d18 HAM)
  (at ap165 d21 HAM)
  (at ap165 d37 HAM)
  (at ap166 d56 FRA)
  (at ap166 d29 BER)
  (at ap166 d41 BER)
  (at ap166 d4 HAM)
  (at ap166 d55 HAM)
  (at ap167 d19 FRA)
  (at ap167 d38 FRA)
  (at ap167 d42 BER)
  (at ap167 d50 BER)
  (at ap167 d52 HAM)
  (at ap168 d17 FRA)
  (at ap168 d30 FRA)
  (at ap168 d60 FRA)
  (at ap168 d9 BER)
  (at ap168 d29 HAM)
  (at ap169 d2 FRA)
  (at ap169 d17 FRA)
  (at ap169 d22 FRA)
  (at ap169 d2 BER)
  (at ap169 d29 HAM)
  (at ap170 d52 FRA)
  (at ap170 d31 HAM)
  (at ap170 d50 HAM)
  (at ap170 d52 HAM)
  (at ap170 d53 HAM)
  (at ap171 d1 FRA)
  (at ap171 d10 FRA)
  (at ap171 d42 FRA)
  (at ap171 d49 FRA)
  (at ap171 d60 HAM)
  (at ap172 d54 FRA)
  (at ap172 d9 BER)
  (at ap172 d25 BER)
  (at ap172 d34 BER)
  (at ap172 d50 BER)
  (at ap173 d9 FRA)
  (at ap173 d11 FRA)
  (at ap173 d52 FRA)
  (at ap173 d15 HAM)
  (at ap173 d18 HAM)
  (at ap174 d45 FRA)
  (at ap174 d7 BER)
  (at ap174 d38 BER)
  (at ap174 d55 BER)
  (at ap174 d36 HAM)
  (at ap175 d15 FRA)
  (at ap175 d14 BER)
  (at ap175 d25 BER)
  (at ap175 d53 BER)
  (at ap175 d43 HAM)
  (at ap176 d23 BER)
  (at ap176 d48 BER)
  (at ap176 d52 BER)
  (at ap176 d20 HAM)
  (at ap176 d57 HAM)
  (at ap177 d39 BER)
  (at ap177 d41 BER)
  (at ap177 d45 BER)
  (at ap177 d32 HAM)
  (at ap177 d56 HAM)
  (at ap178 d22 FRA)
  (at ap178 d37 FRA)
  (at ap178 d37 FRA)
  (at ap178 d14 BER)
  (at ap178 d33 HAM)
  (at ap179 d40 FRA)
  (at ap179 d13 BER)
  (at ap179 d35 BER)
  (at ap179 d52 BER)
  (at ap179 d30 HAM)
  (at ap180 d11 FRA)
  (at ap180 d22 FRA)
  (at ap180 d28 FRA)
  (at ap180 d39 FRA)
  (at ap180 d30 BER)
)
  (:goal (and
 (done ap1)
 (done ap2)
 (done ap3)
 (done ap4)
 (done ap5)
 (done ap6)
 (done ap7)
 (done ap8)
 (done ap9)
 (done ap10)
 (done ap11)
 (done ap12)
 (done ap13)
 (done ap14)
 (done ap15)
 (done ap16)
 (done ap17)
 (done ap18)
 (done ap19)
 (done ap20)
 (done ap21)
 (done ap22)
 (done ap23)
 (done ap24)
 (done ap25)
 (done ap26)
 (done ap27)
 (done ap28)
 (done ap29)
 (done ap30)
 (done ap31)
 (done ap32)
 (done ap33)
 (done ap34)
 (done ap35)
 (done ap36)
 (done ap37)
 (done ap38)
 (done ap39)
 (done ap40)
 (done ap41)
 (done ap42)
 (done ap43)
 (done ap44)
 (done ap45)
 (done ap46)
 (done ap47)
 (done ap48)
 (done ap49)
 (done ap50)
 (done ap51)
 (done ap52)
 (done ap53)
 (done ap54)
 (done ap55)
 (done ap56)
 (done ap57)
 (done ap58)
 (done ap59)
 (done ap60)
 (done ap61)
 (done ap62)
 (done ap63)
 (done ap64)
 (done ap65)
 (done ap66)
 (done ap67)
 (done ap68)
 (done ap69)
 (done ap70)
 (done ap71)
 (done ap72)
 (done ap73)
 (done ap74)
 (done ap75)
 (done ap76)
 (done ap77)
 (done ap78)
 (done ap79)
 (done ap80)
 (done ap81)
 (done ap82)
 (done ap83)
 (done ap84)
 (done ap85)
 (done ap86)
 (done ap87)
 (done ap88)
 (done ap89)
 (done ap90)
 (done ap91)
 (done ap92)
 (done ap93)
 (done ap94)
 (done ap95)
 (done ap96)
 (done ap97)
 (done ap98)
 (done ap99)
 (done ap100)
 (done ap101)
 (done ap102)
 (done ap103)
 (done ap104)
 (done ap105)
 (done ap106)
 (done ap107)
 (done ap108)
 (done ap109)
 (done ap110)
 (done ap111)
 (done ap112)
 (done ap113)
 (done ap114)
 (done ap115)
 (done ap116)
 (done ap117)
 (done ap118)
 (done ap119)
 (done ap120)
 (done ap121)
 (done ap122)
 (done ap123)
 (done ap124)
 (done ap125)
 (done ap126)
 (done ap127)
 (done ap128)
 (done ap129)
 (done ap130)
 (done ap131)
 (done ap132)
 (done ap133)
 (done ap134)
 (done ap135)
 (done ap136)
 (done ap137)
 (done ap138)
 (done ap139)
 (done ap140)
 (done ap141)
 (done ap142)
 (done ap143)
 (done ap144)
 (done ap145)
 (done ap146)
 (done ap147)
 (done ap148)
 (done ap149)
 (done ap150)
 (done ap151)
 (done ap152)
 (done ap153)
 (done ap154)
 (done ap155)
 (done ap156)
 (done ap157)
 (done ap158)
 (done ap159)
 (done ap160)
 (done ap161)
 (done ap162)
 (done ap163)
 (done ap164)
 (done ap165)
 (done ap166)
 (done ap167)
 (done ap168)
 (done ap169)
 (done ap170)
 (done ap171)
 (done ap172)
 (done ap173)
 (done ap174)
 (done ap175)
 (done ap176)
 (done ap177)
 (done ap178)
 (done ap179)
 (done ap180)
  ))
  )
