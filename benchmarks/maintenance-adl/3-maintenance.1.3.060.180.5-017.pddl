(define (problem maintenance-scheduling-1-3-60-180-5-17)
 (:domain maintenance-scheduling-domain)
 (:objects d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 d11 d12 d13 d14 d15 d16 d17 d18 d19 d20 d21 d22 d23 d24 d25 d26 d27 d28 d29 d30 d31 d32 d33 d34 d35 d36 d37 d38 d39 d40 d41 d42 d43 d44 d45 d46 d47 d48 d49 d50 d51 d52 d53 d54 d55 d56 d57 d58 d59 d60 d61 - day
   FRA BER HAM - airport
   ap1 ap2 ap3 ap4 ap5 ap6 ap7 ap8 ap9 ap10 ap11 ap12 ap13 ap14 ap15 ap16 ap17 ap18 ap19 ap20 ap21 ap22 ap23 ap24 ap25 ap26 ap27 ap28 ap29 ap30 ap31 ap32 ap33 ap34 ap35 ap36 ap37 ap38 ap39 ap40 ap41 ap42 ap43 ap44 ap45 ap46 ap47 ap48 ap49 ap50 ap51 ap52 ap53 ap54 ap55 ap56 ap57 ap58 ap59 ap60 ap61 ap62 ap63 ap64 ap65 ap66 ap67 ap68 ap69 ap70 ap71 ap72 ap73 ap74 ap75 ap76 ap77 ap78 ap79 ap80 ap81 ap82 ap83 ap84 ap85 ap86 ap87 ap88 ap89 ap90 ap91 ap92 ap93 ap94 ap95 ap96 ap97 ap98 ap99 ap100 ap101 ap102 ap103 ap104 ap105 ap106 ap107 ap108 ap109 ap110 ap111 ap112 ap113 ap114 ap115 ap116 ap117 ap118 ap119 ap120 ap121 ap122 ap123 ap124 ap125 ap126 ap127 ap128 ap129 ap130 ap131 ap132 ap133 ap134 ap135 ap136 ap137 ap138 ap139 ap140 ap141 ap142 ap143 ap144 ap145 ap146 ap147 ap148 ap149 ap150 ap151 ap152 ap153 ap154 ap155 ap156 ap157 ap158 ap159 ap160 ap161 ap162 ap163 ap164 ap165 ap166 ap167 ap168 ap169 ap170 ap171 ap172 ap173 ap174 ap175 ap176 ap177 ap178 ap179 ap180 - plane)
 (:init
  (today d1)  (today d2)  (today d3)  (today d4)  (today d5)  (today d6)  (today d7)  (today d8)  (today d9)  (today d10)  (today d11)  (today d12)  (today d13)  (today d14)  (today d15)  (today d16)  (today d17)  (today d18)  (today d19)  (today d20)  (today d21)  (today d22)  (today d23)  (today d24)  (today d25)  (today d26)  (today d27)  (today d28)  (today d29)  (today d30)  (today d31)  (today d32)  (today d33)  (today d34)  (today d35)  (today d36)  (today d37)  (today d38)  (today d39)  (today d40)  (today d41)  (today d42)  (today d43)  (today d44)  (today d45)  (today d46)  (today d47)  (today d48)  (today d49)  (today d50)  (today d51)  (today d52)  (today d53)  (today d54)  (today d55)  (today d56)  (today d57)  (today d58)  (today d59)  (today d60)  (at ap1 d17 FRA)
  (at ap1 d21 BER)
  (at ap1 d36 BER)
  (at ap1 d56 BER)
  (at ap1 d42 HAM)
  (at ap2 d6 FRA)
  (at ap2 d22 FRA)
  (at ap2 d8 BER)
  (at ap2 d52 BER)
  (at ap2 d28 HAM)
  (at ap3 d53 FRA)
  (at ap3 d54 FRA)
  (at ap3 d4 BER)
  (at ap3 d12 BER)
  (at ap3 d21 BER)
  (at ap4 d29 FRA)
  (at ap4 d53 FRA)
  (at ap4 d24 BER)
  (at ap4 d5 HAM)
  (at ap4 d42 HAM)
  (at ap5 d20 FRA)
  (at ap5 d21 FRA)
  (at ap5 d27 FRA)
  (at ap5 d39 BER)
  (at ap5 d40 BER)
  (at ap6 d40 FRA)
  (at ap6 d51 BER)
  (at ap6 d57 BER)
  (at ap6 d30 HAM)
  (at ap6 d58 HAM)
  (at ap7 d3 FRA)
  (at ap7 d8 FRA)
  (at ap7 d14 BER)
  (at ap7 d59 BER)
  (at ap7 d60 HAM)
  (at ap8 d6 FRA)
  (at ap8 d17 FRA)
  (at ap8 d23 FRA)
  (at ap8 d38 FRA)
  (at ap8 d49 BER)
  (at ap9 d11 BER)
  (at ap9 d3 HAM)
  (at ap9 d47 HAM)
  (at ap9 d56 HAM)
  (at ap9 d56 HAM)
  (at ap10 d6 FRA)
  (at ap10 d19 FRA)
  (at ap10 d45 FRA)
  (at ap10 d37 BER)
  (at ap10 d48 HAM)
  (at ap11 d14 FRA)
  (at ap11 d38 FRA)
  (at ap11 d19 BER)
  (at ap11 d31 BER)
  (at ap11 d40 BER)
  (at ap12 d13 HAM)
  (at ap12 d19 HAM)
  (at ap12 d31 HAM)
  (at ap12 d38 HAM)
  (at ap12 d50 HAM)
  (at ap13 d59 FRA)
  (at ap13 d56 BER)
  (at ap13 d59 BER)
  (at ap13 d6 HAM)
  (at ap13 d59 HAM)
  (at ap14 d7 FRA)
  (at ap14 d24 FRA)
  (at ap14 d60 FRA)
  (at ap14 d33 BER)
  (at ap14 d20 HAM)
  (at ap15 d44 FRA)
  (at ap15 d5 BER)
  (at ap15 d24 BER)
  (at ap15 d45 BER)
  (at ap15 d45 BER)
  (at ap16 d28 FRA)
  (at ap16 d40 FRA)
  (at ap16 d40 FRA)
  (at ap16 d49 FRA)
  (at ap16 d45 HAM)
  (at ap17 d6 FRA)
  (at ap17 d12 FRA)
  (at ap17 d7 BER)
  (at ap17 d35 BER)
  (at ap17 d38 HAM)
  (at ap18 d39 BER)
  (at ap18 d39 BER)
  (at ap18 d49 BER)
  (at ap18 d42 HAM)
  (at ap18 d49 HAM)
  (at ap19 d19 FRA)
  (at ap19 d33 FRA)
  (at ap19 d46 BER)
  (at ap19 d49 BER)
  (at ap19 d3 HAM)
  (at ap20 d14 FRA)
  (at ap20 d37 FRA)
  (at ap20 d5 HAM)
  (at ap20 d22 HAM)
  (at ap20 d56 HAM)
  (at ap21 d4 FRA)
  (at ap21 d12 FRA)
  (at ap21 d59 FRA)
  (at ap21 d3 HAM)
  (at ap21 d32 HAM)
  (at ap22 d23 FRA)
  (at ap22 d47 FRA)
  (at ap22 d54 FRA)
  (at ap22 d14 BER)
  (at ap22 d39 BER)
  (at ap23 d3 FRA)
  (at ap23 d13 FRA)
  (at ap23 d49 FRA)
  (at ap23 d60 BER)
  (at ap23 d44 HAM)
  (at ap24 d2 FRA)
  (at ap24 d3 BER)
  (at ap24 d39 BER)
  (at ap24 d17 HAM)
  (at ap24 d22 HAM)
  (at ap25 d10 BER)
  (at ap25 d8 HAM)
  (at ap25 d37 HAM)
  (at ap25 d39 HAM)
  (at ap25 d60 HAM)
  (at ap26 d25 FRA)
  (at ap26 d45 FRA)
  (at ap26 d53 FRA)
  (at ap26 d29 HAM)
  (at ap26 d43 HAM)
  (at ap27 d39 FRA)
  (at ap27 d55 BER)
  (at ap27 d4 HAM)
  (at ap27 d38 HAM)
  (at ap27 d46 HAM)
  (at ap28 d36 FRA)
  (at ap28 d42 FRA)
  (at ap28 d22 BER)
  (at ap28 d17 HAM)
  (at ap28 d36 HAM)
  (at ap29 d37 FRA)
  (at ap29 d48 FRA)
  (at ap29 d27 HAM)
  (at ap29 d28 HAM)
  (at ap29 d31 HAM)
  (at ap30 d45 FRA)
  (at ap30 d11 BER)
  (at ap30 d15 BER)
  (at ap30 d59 BER)
  (at ap30 d58 HAM)
  (at ap31 d1 BER)
  (at ap31 d40 BER)
  (at ap31 d60 BER)
  (at ap31 d14 HAM)
  (at ap31 d43 HAM)
  (at ap32 d2 FRA)
  (at ap32 d27 FRA)
  (at ap32 d37 FRA)
  (at ap32 d55 BER)
  (at ap32 d4 HAM)
  (at ap33 d12 FRA)
  (at ap33 d20 FRA)
  (at ap33 d41 BER)
  (at ap33 d12 HAM)
  (at ap33 d14 HAM)
  (at ap34 d45 FRA)
  (at ap34 d58 FRA)
  (at ap34 d57 BER)
  (at ap34 d5 HAM)
  (at ap34 d5 HAM)
  (at ap35 d19 FRA)
  (at ap35 d31 BER)
  (at ap35 d6 HAM)
  (at ap35 d24 HAM)
  (at ap35 d55 HAM)
  (at ap36 d19 FRA)
  (at ap36 d22 FRA)
  (at ap36 d39 BER)
  (at ap36 d10 HAM)
  (at ap36 d52 HAM)
  (at ap37 d10 FRA)
  (at ap37 d24 FRA)
  (at ap37 d32 FRA)
  (at ap37 d44 FRA)
  (at ap37 d33 BER)
  (at ap38 d4 FRA)
  (at ap38 d55 FRA)
  (at ap38 d34 BER)
  (at ap38 d28 HAM)
  (at ap38 d47 HAM)
  (at ap39 d51 FRA)
  (at ap39 d10 BER)
  (at ap39 d44 BER)
  (at ap39 d4 HAM)
  (at ap39 d32 HAM)
  (at ap40 d34 FRA)
  (at ap40 d55 FRA)
  (at ap40 d55 FRA)
  (at ap40 d14 BER)
  (at ap40 d17 BER)
  (at ap41 d51 FRA)
  (at ap41 d11 BER)
  (at ap41 d3 HAM)
  (at ap41 d6 HAM)
  (at ap41 d26 HAM)
  (at ap42 d13 FRA)
  (at ap42 d53 FRA)
  (at ap42 d50 BER)
  (at ap42 d51 BER)
  (at ap42 d9 HAM)
  (at ap43 d11 FRA)
  (at ap43 d18 FRA)
  (at ap43 d44 BER)
  (at ap43 d27 HAM)
  (at ap43 d41 HAM)
  (at ap44 d47 FRA)
  (at ap44 d21 BER)
  (at ap44 d56 BER)
  (at ap44 d28 HAM)
  (at ap44 d46 HAM)
  (at ap45 d31 FRA)
  (at ap45 d8 BER)
  (at ap45 d4 HAM)
  (at ap45 d40 HAM)
  (at ap45 d60 HAM)
  (at ap46 d41 FRA)
  (at ap46 d46 BER)
  (at ap46 d11 HAM)
  (at ap46 d43 HAM)
  (at ap46 d55 HAM)
  (at ap47 d41 FRA)
  (at ap47 d23 HAM)
  (at ap47 d53 HAM)
  (at ap47 d55 HAM)
  (at ap47 d60 HAM)
  (at ap48 d27 FRA)
  (at ap48 d30 FRA)
  (at ap48 d45 BER)
  (at ap48 d26 HAM)
  (at ap48 d40 HAM)
  (at ap49 d15 FRA)
  (at ap49 d39 FRA)
  (at ap49 d42 FRA)
  (at ap49 d60 BER)
  (at ap49 d33 HAM)
  (at ap50 d4 FRA)
  (at ap50 d56 BER)
  (at ap50 d57 BER)
  (at ap50 d1 HAM)
  (at ap50 d40 HAM)
  (at ap51 d35 FRA)
  (at ap51 d19 BER)
  (at ap51 d25 BER)
  (at ap51 d48 BER)
  (at ap51 d53 BER)
  (at ap52 d35 FRA)
  (at ap52 d36 FRA)
  (at ap52 d20 HAM)
  (at ap52 d26 HAM)
  (at ap52 d57 HAM)
  (at ap53 d6 FRA)
  (at ap53 d20 BER)
  (at ap53 d54 BER)
  (at ap53 d27 HAM)
  (at ap53 d54 HAM)
  (at ap54 d31 FRA)
  (at ap54 d36 FRA)
  (at ap54 d11 HAM)
  (at ap54 d38 HAM)
  (at ap54 d60 HAM)
  (at ap55 d31 FRA)
  (at ap55 d58 FRA)
  (at ap55 d24 HAM)
  (at ap55 d27 HAM)
  (at ap55 d34 HAM)
  (at ap56 d3 FRA)
  (at ap56 d32 FRA)
  (at ap56 d2 BER)
  (at ap56 d1 HAM)
  (at ap56 d32 HAM)
  (at ap57 d30 FRA)
  (at ap57 d31 FRA)
  (at ap57 d38 BER)
  (at ap57 d26 HAM)
  (at ap57 d40 HAM)
  (at ap58 d21 FRA)
  (at ap58 d52 FRA)
  (at ap58 d33 HAM)
  (at ap58 d36 HAM)
  (at ap58 d46 HAM)
  (at ap59 d3 FRA)
  (at ap59 d24 FRA)
  (at ap59 d10 BER)
  (at ap59 d36 HAM)
  (at ap59 d51 HAM)
  (at ap60 d14 FRA)
  (at ap60 d41 FRA)
  (at ap60 d37 BER)
  (at ap60 d43 BER)
  (at ap60 d39 HAM)
  (at ap61 d1 FRA)
  (at ap61 d1 BER)
  (at ap61 d43 BER)
  (at ap61 d4 HAM)
  (at ap61 d16 HAM)
  (at ap62 d8 FRA)
  (at ap62 d15 FRA)
  (at ap62 d35 BER)
  (at ap62 d2 HAM)
  (at ap62 d55 HAM)
  (at ap63 d24 FRA)
  (at ap63 d31 FRA)
  (at ap63 d37 FRA)
  (at ap63 d49 BER)
  (at ap63 d57 HAM)
  (at ap64 d39 FRA)
  (at ap64 d25 BER)
  (at ap64 d25 BER)
  (at ap64 d7 HAM)
  (at ap64 d36 HAM)
  (at ap65 d30 FRA)
  (at ap65 d17 BER)
  (at ap65 d47 BER)
  (at ap65 d3 HAM)
  (at ap65 d14 HAM)
  (at ap66 d26 FRA)
  (at ap66 d22 BER)
  (at ap66 d27 BER)
  (at ap66 d38 BER)
  (at ap66 d38 HAM)
  (at ap67 d35 BER)
  (at ap67 d48 BER)
  (at ap67 d20 HAM)
  (at ap67 d28 HAM)
  (at ap67 d40 HAM)
  (at ap68 d5 BER)
  (at ap68 d28 BER)
  (at ap68 d10 HAM)
  (at ap68 d14 HAM)
  (at ap68 d29 HAM)
  (at ap69 d33 FRA)
  (at ap69 d51 FRA)
  (at ap69 d55 BER)
  (at ap69 d52 HAM)
  (at ap69 d52 HAM)
  (at ap70 d2 FRA)
  (at ap70 d17 FRA)
  (at ap70 d50 FRA)
  (at ap70 d3 BER)
  (at ap70 d8 HAM)
  (at ap71 d9 FRA)
  (at ap71 d12 FRA)
  (at ap71 d28 FRA)
  (at ap71 d48 BER)
  (at ap71 d51 BER)
  (at ap72 d4 FRA)
  (at ap72 d11 FRA)
  (at ap72 d52 FRA)
  (at ap72 d20 HAM)
  (at ap72 d37 HAM)
  (at ap73 d5 FRA)
  (at ap73 d9 HAM)
  (at ap73 d18 HAM)
  (at ap73 d25 HAM)
  (at ap73 d59 HAM)
  (at ap74 d2 BER)
  (at ap74 d34 BER)
  (at ap74 d46 BER)
  (at ap74 d46 HAM)
  (at ap74 d50 HAM)
  (at ap75 d13 BER)
  (at ap75 d15 BER)
  (at ap75 d27 BER)
  (at ap75 d36 BER)
  (at ap75 d22 HAM)
  (at ap76 d6 BER)
  (at ap76 d14 BER)
  (at ap76 d32 BER)
  (at ap76 d45 BER)
  (at ap76 d44 HAM)
  (at ap77 d27 FRA)
  (at ap77 d31 FRA)
  (at ap77 d50 FRA)
  (at ap77 d1 BER)
  (at ap77 d8 BER)
  (at ap78 d43 FRA)
  (at ap78 d51 FRA)
  (at ap78 d20 BER)
  (at ap78 d25 BER)
  (at ap78 d56 BER)
  (at ap79 d11 BER)
  (at ap79 d16 BER)
  (at ap79 d54 BER)
  (at ap79 d57 BER)
  (at ap79 d38 HAM)
  (at ap80 d27 FRA)
  (at ap80 d39 FRA)
  (at ap80 d53 FRA)
  (at ap80 d51 BER)
  (at ap80 d1 HAM)
  (at ap81 d45 FRA)
  (at ap81 d12 BER)
  (at ap81 d20 BER)
  (at ap81 d58 BER)
  (at ap81 d48 HAM)
  (at ap82 d18 BER)
  (at ap82 d21 BER)
  (at ap82 d25 BER)
  (at ap82 d48 BER)
  (at ap82 d3 HAM)
  (at ap83 d17 FRA)
  (at ap83 d34 BER)
  (at ap83 d1 HAM)
  (at ap83 d26 HAM)
  (at ap83 d53 HAM)
  (at ap84 d8 FRA)
  (at ap84 d23 FRA)
  (at ap84 d42 FRA)
  (at ap84 d1 BER)
  (at ap84 d40 HAM)
  (at ap85 d26 FRA)
  (at ap85 d32 BER)
  (at ap85 d39 BER)
  (at ap85 d9 HAM)
  (at ap85 d42 HAM)
  (at ap86 d21 FRA)
  (at ap86 d29 FRA)
  (at ap86 d16 BER)
  (at ap86 d40 HAM)
  (at ap86 d60 HAM)
  (at ap87 d23 FRA)
  (at ap87 d4 BER)
  (at ap87 d43 BER)
  (at ap87 d54 BER)
  (at ap87 d45 HAM)
  (at ap88 d10 FRA)
  (at ap88 d13 FRA)
  (at ap88 d2 BER)
  (at ap88 d8 BER)
  (at ap88 d22 HAM)
  (at ap89 d41 BER)
  (at ap89 d9 HAM)
  (at ap89 d13 HAM)
  (at ap89 d30 HAM)
  (at ap89 d37 HAM)
  (at ap90 d2 FRA)
  (at ap90 d21 FRA)
  (at ap90 d8 BER)
  (at ap90 d35 BER)
  (at ap90 d37 HAM)
  (at ap91 d44 FRA)
  (at ap91 d20 BER)
  (at ap91 d47 BER)
  (at ap91 d18 HAM)
  (at ap91 d22 HAM)
  (at ap92 d3 BER)
  (at ap92 d42 BER)
  (at ap92 d47 BER)
  (at ap92 d55 BER)
  (at ap92 d55 HAM)
  (at ap93 d25 BER)
  (at ap93 d40 BER)
  (at ap93 d44 BER)
  (at ap93 d5 HAM)
  (at ap93 d54 HAM)
  (at ap94 d25 FRA)
  (at ap94 d36 FRA)
  (at ap94 d60 FRA)
  (at ap94 d42 BER)
  (at ap94 d6 HAM)
  (at ap95 d20 FRA)
  (at ap95 d39 BER)
  (at ap95 d46 BER)
  (at ap95 d46 BER)
  (at ap95 d41 HAM)
  (at ap96 d45 FRA)
  (at ap96 d46 FRA)
  (at ap96 d9 BER)
  (at ap96 d27 BER)
  (at ap96 d58 HAM)
  (at ap97 d18 BER)
  (at ap97 d43 BER)
  (at ap97 d50 BER)
  (at ap97 d20 HAM)
  (at ap97 d32 HAM)
  (at ap98 d29 FRA)
  (at ap98 d31 FRA)
  (at ap98 d54 FRA)
  (at ap98 d9 BER)
  (at ap98 d21 BER)
  (at ap99 d46 FRA)
  (at ap99 d35 BER)
  (at ap99 d36 BER)
  (at ap99 d60 BER)
  (at ap99 d37 HAM)
  (at ap100 d48 FRA)
  (at ap100 d7 HAM)
  (at ap100 d38 HAM)
  (at ap100 d55 HAM)
  (at ap100 d57 HAM)
  (at ap101 d10 FRA)
  (at ap101 d15 FRA)
  (at ap101 d13 HAM)
  (at ap101 d31 HAM)
  (at ap101 d40 HAM)
  (at ap102 d39 BER)
  (at ap102 d49 BER)
  (at ap102 d50 BER)
  (at ap102 d57 BER)
  (at ap102 d54 HAM)
  (at ap103 d49 BER)
  (at ap103 d28 HAM)
  (at ap103 d33 HAM)
  (at ap103 d47 HAM)
  (at ap103 d55 HAM)
  (at ap104 d40 BER)
  (at ap104 d43 BER)
  (at ap104 d3 HAM)
  (at ap104 d28 HAM)
  (at ap104 d54 HAM)
  (at ap105 d15 FRA)
  (at ap105 d33 FRA)
  (at ap105 d35 BER)
  (at ap105 d51 BER)
  (at ap105 d51 HAM)
  (at ap106 d21 BER)
  (at ap106 d16 HAM)
  (at ap106 d19 HAM)
  (at ap106 d41 HAM)
  (at ap106 d44 HAM)
  (at ap107 d9 FRA)
  (at ap107 d21 FRA)
  (at ap107 d32 BER)
  (at ap107 d6 HAM)
  (at ap107 d8 HAM)
  (at ap108 d28 FRA)
  (at ap108 d44 FRA)
  (at ap108 d31 BER)
  (at ap108 d36 BER)
  (at ap108 d4 HAM)
  (at ap109 d28 FRA)
  (at ap109 d29 FRA)
  (at ap109 d9 BER)
  (at ap109 d4 HAM)
  (at ap109 d39 HAM)
  (at ap110 d3 BER)
  (at ap110 d19 BER)
  (at ap110 d47 HAM)
  (at ap110 d54 HAM)
  (at ap110 d58 HAM)
  (at ap111 d26 FRA)
  (at ap111 d51 FRA)
  (at ap111 d11 BER)
  (at ap111 d34 BER)
  (at ap111 d31 HAM)
  (at ap112 d22 FRA)
  (at ap112 d25 FRA)
  (at ap112 d59 BER)
  (at ap112 d44 HAM)
  (at ap112 d54 HAM)
  (at ap113 d30 FRA)
  (at ap113 d50 BER)
  (at ap113 d60 BER)
  (at ap113 d23 HAM)
  (at ap113 d39 HAM)
  (at ap114 d19 FRA)
  (at ap114 d41 FRA)
  (at ap114 d9 HAM)
  (at ap114 d20 HAM)
  (at ap114 d46 HAM)
  (at ap115 d1 FRA)
  (at ap115 d9 FRA)
  (at ap115 d19 FRA)
  (at ap115 d34 FRA)
  (at ap115 d35 HAM)
  (at ap116 d41 FRA)
  (at ap116 d33 BER)
  (at ap116 d41 BER)
  (at ap116 d58 BER)
  (at ap116 d35 HAM)
  (at ap117 d21 FRA)
  (at ap117 d38 FRA)
  (at ap117 d57 FRA)
  (at ap117 d10 BER)
  (at ap117 d33 BER)
  (at ap118 d33 FRA)
  (at ap118 d58 FRA)
  (at ap118 d9 BER)
  (at ap118 d25 BER)
  (at ap118 d55 HAM)
  (at ap119 d1 FRA)
  (at ap119 d50 BER)
  (at ap119 d3 HAM)
  (at ap119 d33 HAM)
  (at ap119 d55 HAM)
  (at ap120 d16 FRA)
  (at ap120 d11 BER)
  (at ap120 d35 BER)
  (at ap120 d42 BER)
  (at ap120 d24 HAM)
  (at ap121 d42 FRA)
  (at ap121 d31 BER)
  (at ap121 d33 BER)
  (at ap121 d59 BER)
  (at ap121 d21 HAM)
  (at ap122 d51 FRA)
  (at ap122 d50 BER)
  (at ap122 d50 BER)
  (at ap122 d20 HAM)
  (at ap122 d48 HAM)
  (at ap123 d53 FRA)
  (at ap123 d56 BER)
  (at ap123 d16 HAM)
  (at ap123 d46 HAM)
  (at ap123 d49 HAM)
  (at ap124 d19 BER)
  (at ap124 d44 BER)
  (at ap124 d12 HAM)
  (at ap124 d18 HAM)
  (at ap124 d27 HAM)
  (at ap125 d18 FRA)
  (at ap125 d24 BER)
  (at ap125 d50 BER)
  (at ap125 d2 HAM)
  (at ap125 d53 HAM)
  (at ap126 d51 BER)
  (at ap126 d14 HAM)
  (at ap126 d21 HAM)
  (at ap126 d24 HAM)
  (at ap126 d58 HAM)
  (at ap127 d31 FRA)
  (at ap127 d20 BER)
  (at ap127 d48 BER)
  (at ap127 d35 HAM)
  (at ap127 d55 HAM)
  (at ap128 d6 FRA)
  (at ap128 d57 FRA)
  (at ap128 d8 HAM)
  (at ap128 d55 HAM)
  (at ap128 d58 HAM)
  (at ap129 d29 FRA)
  (at ap129 d47 FRA)
  (at ap129 d2 BER)
  (at ap129 d18 HAM)
  (at ap129 d44 HAM)
  (at ap130 d18 FRA)
  (at ap130 d22 FRA)
  (at ap130 d1 BER)
  (at ap130 d9 BER)
  (at ap130 d37 HAM)
  (at ap131 d5 BER)
  (at ap131 d5 BER)
  (at ap131 d57 BER)
  (at ap131 d4 HAM)
  (at ap131 d51 HAM)
  (at ap132 d8 FRA)
  (at ap132 d49 FRA)
  (at ap132 d47 BER)
  (at ap132 d59 BER)
  (at ap132 d30 HAM)
  (at ap133 d48 FRA)
  (at ap133 d24 BER)
  (at ap133 d27 BER)
  (at ap133 d33 BER)
  (at ap133 d9 HAM)
  (at ap134 d28 FRA)
  (at ap134 d9 BER)
  (at ap134 d17 BER)
  (at ap134 d28 BER)
  (at ap134 d57 HAM)
  (at ap135 d7 FRA)
  (at ap135 d16 FRA)
  (at ap135 d3 BER)
  (at ap135 d20 HAM)
  (at ap135 d41 HAM)
  (at ap136 d25 FRA)
  (at ap136 d42 FRA)
  (at ap136 d10 HAM)
  (at ap136 d42 HAM)
  (at ap136 d57 HAM)
  (at ap137 d23 FRA)
  (at ap137 d39 FRA)
  (at ap137 d3 BER)
  (at ap137 d25 BER)
  (at ap137 d20 HAM)
  (at ap138 d5 FRA)
  (at ap138 d38 FRA)
  (at ap138 d60 FRA)
  (at ap138 d40 BER)
  (at ap138 d44 HAM)
  (at ap139 d5 FRA)
  (at ap139 d11 FRA)
  (at ap139 d14 BER)
  (at ap139 d15 BER)
  (at ap139 d40 BER)
  (at ap140 d6 FRA)
  (at ap140 d27 FRA)
  (at ap140 d46 BER)
  (at ap140 d11 HAM)
  (at ap140 d49 HAM)
  (at ap141 d22 FRA)
  (at ap141 d28 BER)
  (at ap141 d47 BER)
  (at ap141 d10 HAM)
  (at ap141 d53 HAM)
  (at ap142 d28 FRA)
  (at ap142 d34 FRA)
  (at ap142 d17 BER)
  (at ap142 d48 BER)
  (at ap142 d33 HAM)
  (at ap143 d11 FRA)
  (at ap143 d22 BER)
  (at ap143 d56 BER)
  (at ap143 d40 HAM)
  (at ap143 d47 HAM)
  (at ap144 d5 FRA)
  (at ap144 d24 FRA)
  (at ap144 d7 BER)
  (at ap144 d56 BER)
  (at ap144 d21 HAM)
  (at ap145 d2 FRA)
  (at ap145 d12 FRA)
  (at ap145 d24 FRA)
  (at ap145 d9 HAM)
  (at ap145 d58 HAM)
  (at ap146 d12 FRA)
  (at ap146 d31 FRA)
  (at ap146 d26 BER)
  (at ap146 d34 BER)
  (at ap146 d49 BER)
  (at ap147 d3 FRA)
  (at ap147 d8 BER)
  (at ap147 d44 BER)
  (at ap147 d50 BER)
  (at ap147 d7 HAM)
  (at ap148 d8 FRA)
  (at ap148 d5 BER)
  (at ap148 d49 BER)
  (at ap148 d49 HAM)
  (at ap148 d55 HAM)
  (at ap149 d8 FRA)
  (at ap149 d38 BER)
  (at ap149 d42 BER)
  (at ap149 d48 BER)
  (at ap149 d38 HAM)
  (at ap150 d5 FRA)
  (at ap150 d55 BER)
  (at ap150 d29 HAM)
  (at ap150 d38 HAM)
  (at ap150 d44 HAM)
  (at ap151 d31 FRA)
  (at ap151 d9 BER)
  (at ap151 d42 BER)
  (at ap151 d6 HAM)
  (at ap151 d11 HAM)
  (at ap152 d24 FRA)
  (at ap152 d10 HAM)
  (at ap152 d30 HAM)
  (at ap152 d41 HAM)
  (at ap152 d57 HAM)
  (at ap153 d51 FRA)
  (at ap153 d8 BER)
  (at ap153 d56 BER)
  (at ap153 d3 HAM)
  (at ap153 d24 HAM)
  (at ap154 d8 FRA)
  (at ap154 d24 FRA)
  (at ap154 d26 BER)
  (at ap154 d43 BER)
  (at ap154 d5 HAM)
  (at ap155 d45 FRA)
  (at ap155 d4 BER)
  (at ap155 d28 BER)
  (at ap155 d13 HAM)
  (at ap155 d46 HAM)
  (at ap156 d5 BER)
  (at ap156 d41 BER)
  (at ap156 d57 BER)
  (at ap156 d30 HAM)
  (at ap156 d44 HAM)
  (at ap157 d1 FRA)
  (at ap157 d18 BER)
  (at ap157 d28 BER)
  (at ap157 d23 HAM)
  (at ap157 d25 HAM)
  (at ap158 d21 FRA)
  (at ap158 d27 BER)
  (at ap158 d51 BER)
  (at ap158 d54 BER)
  (at ap158 d37 HAM)
  (at ap159 d1 FRA)
  (at ap159 d27 BER)
  (at ap159 d53 BER)
  (at ap159 d53 BER)
  (at ap159 d48 HAM)
  (at ap160 d19 FRA)
  (at ap160 d14 BER)
  (at ap160 d59 BER)
  (at ap160 d33 HAM)
  (at ap160 d39 HAM)
  (at ap161 d23 FRA)
  (at ap161 d16 BER)
  (at ap161 d45 BER)
  (at ap161 d34 HAM)
  (at ap161 d47 HAM)
  (at ap162 d46 FRA)
  (at ap162 d50 FRA)
  (at ap162 d53 FRA)
  (at ap162 d31 BER)
  (at ap162 d51 HAM)
  (at ap163 d24 BER)
  (at ap163 d6 HAM)
  (at ap163 d17 HAM)
  (at ap163 d33 HAM)
  (at ap163 d34 HAM)
  (at ap164 d14 FRA)
  (at ap164 d20 FRA)
  (at ap164 d51 FRA)
  (at ap164 d53 FRA)
  (at ap164 d46 BER)
  (at ap165 d34 FRA)
  (at ap165 d9 BER)
  (at ap165 d13 BER)
  (at ap165 d45 BER)
  (at ap165 d48 HAM)
  (at ap166 d24 BER)
  (at ap166 d51 BER)
  (at ap166 d52 BER)
  (at ap166 d43 HAM)
  (at ap166 d50 HAM)
  (at ap167 d35 FRA)
  (at ap167 d3 HAM)
  (at ap167 d28 HAM)
  (at ap167 d41 HAM)
  (at ap167 d43 HAM)
  (at ap168 d19 FRA)
  (at ap168 d54 FRA)
  (at ap168 d13 BER)
  (at ap168 d43 HAM)
  (at ap168 d53 HAM)
  (at ap169 d13 FRA)
  (at ap169 d43 FRA)
  (at ap169 d1 BER)
  (at ap169 d28 BER)
  (at ap169 d54 HAM)
  (at ap170 d11 FRA)
  (at ap170 d28 FRA)
  (at ap170 d57 FRA)
  (at ap170 d29 BER)
  (at ap170 d22 HAM)
  (at ap171 d2 FRA)
  (at ap171 d3 BER)
  (at ap171 d12 BER)
  (at ap171 d37 HAM)
  (at ap171 d38 HAM)
  (at ap172 d3 BER)
  (at ap172 d53 BER)
  (at ap172 d12 HAM)
  (at ap172 d27 HAM)
  (at ap172 d59 HAM)
  (at ap173 d16 FRA)
  (at ap173 d46 FRA)
  (at ap173 d42 BER)
  (at ap173 d51 BER)
  (at ap173 d15 HAM)
  (at ap174 d3 FRA)
  (at ap174 d13 FRA)
  (at ap174 d40 BER)
  (at ap174 d58 BER)
  (at ap174 d27 HAM)
  (at ap175 d35 BER)
  (at ap175 d5 HAM)
  (at ap175 d24 HAM)
  (at ap175 d48 HAM)
  (at ap175 d58 HAM)
  (at ap176 d3 FRA)
  (at ap176 d16 FRA)
  (at ap176 d9 BER)
  (at ap176 d18 HAM)
  (at ap176 d27 HAM)
  (at ap177 d3 FRA)
  (at ap177 d6 FRA)
  (at ap177 d28 FRA)
  (at ap177 d7 BER)
  (at ap177 d46 BER)
  (at ap178 d30 FRA)
  (at ap178 d41 BER)
  (at ap178 d56 BER)
  (at ap178 d8 HAM)
  (at ap178 d42 HAM)
  (at ap179 d24 FRA)
  (at ap179 d31 FRA)
  (at ap179 d47 BER)
  (at ap179 d26 HAM)
  (at ap179 d34 HAM)
  (at ap180 d23 FRA)
  (at ap180 d44 FRA)
  (at ap180 d38 BER)
  (at ap180 d57 BER)
  (at ap180 d57 BER)
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
