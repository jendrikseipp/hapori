(define (problem maintenance-scheduling-1-3-40-50-8-1)
 (:domain maintenance-scheduling-domain)
 (:objects d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 d11 d12 d13 d14 d15 d16 d17 d18 d19 d20 d21 d22 d23 d24 d25 d26 d27 d28 d29 d30 d31 d32 d33 d34 d35 d36 d37 d38 d39 d40 d41 - day
   FRA BER HAM - airport
   ap1 ap2 ap3 ap4 ap5 ap6 ap7 ap8 ap9 ap10 ap11 ap12 ap13 ap14 ap15 ap16 ap17 ap18 ap19 ap20 ap21 ap22 ap23 ap24 ap25 ap26 ap27 ap28 ap29 ap30 ap31 ap32 ap33 ap34 ap35 ap36 ap37 ap38 ap39 ap40 ap41 ap42 ap43 ap44 ap45 ap46 ap47 ap48 ap49 ap50 - plane)
 (:init
  (today d1)  (today d2)  (today d3)  (today d4)  (today d5)  (today d6)  (today d7)  (today d8)  (today d9)  (today d10)  (today d11)  (today d12)  (today d13)  (today d14)  (today d15)  (today d16)  (today d17)  (today d18)  (today d19)  (today d20)  (today d21)  (today d22)  (today d23)  (today d24)  (today d25)  (today d26)  (today d27)  (today d28)  (today d29)  (today d30)  (today d31)  (today d32)  (today d33)  (today d34)  (today d35)  (today d36)  (today d37)  (today d38)  (today d39)  (today d40)  (at ap1 d10 FRA)
  (at ap1 d16 FRA)
  (at ap1 d21 FRA)
  (at ap1 d24 FRA)
  (at ap1 d23 BER)
  (at ap1 d24 BER)
  (at ap1 d5 HAM)
  (at ap1 d36 HAM)
  (at ap2 d4 FRA)
  (at ap2 d11 FRA)
  (at ap2 d22 FRA)
  (at ap2 d13 BER)
  (at ap2 d21 BER)
  (at ap2 d22 BER)
  (at ap2 d37 BER)
  (at ap2 d37 HAM)
  (at ap3 d9 FRA)
  (at ap3 d10 FRA)
  (at ap3 d14 FRA)
  (at ap3 d21 FRA)
  (at ap3 d40 FRA)
  (at ap3 d11 BER)
  (at ap3 d9 HAM)
  (at ap3 d40 HAM)
  (at ap4 d5 FRA)
  (at ap4 d24 FRA)
  (at ap4 d34 FRA)
  (at ap4 d37 FRA)
  (at ap4 d11 BER)
  (at ap4 d19 BER)
  (at ap4 d21 HAM)
  (at ap4 d34 HAM)
  (at ap5 d18 FRA)
  (at ap5 d19 FRA)
  (at ap5 d21 FRA)
  (at ap5 d25 FRA)
  (at ap5 d10 BER)
  (at ap5 d30 BER)
  (at ap5 d6 HAM)
  (at ap5 d13 HAM)
  (at ap6 d11 FRA)
  (at ap6 d12 FRA)
  (at ap6 d24 FRA)
  (at ap6 d38 FRA)
  (at ap6 d26 BER)
  (at ap6 d36 BER)
  (at ap6 d24 HAM)
  (at ap6 d32 HAM)
  (at ap7 d3 BER)
  (at ap7 d7 BER)
  (at ap7 d18 BER)
  (at ap7 d23 BER)
  (at ap7 d1 HAM)
  (at ap7 d19 HAM)
  (at ap7 d21 HAM)
  (at ap7 d39 HAM)
  (at ap8 d10 FRA)
  (at ap8 d11 FRA)
  (at ap8 d19 FRA)
  (at ap8 d34 FRA)
  (at ap8 d35 FRA)
  (at ap8 d27 BER)
  (at ap8 d17 HAM)
  (at ap8 d24 HAM)
  (at ap9 d17 FRA)
  (at ap9 d25 FRA)
  (at ap9 d13 BER)
  (at ap9 d20 BER)
  (at ap9 d32 BER)
  (at ap9 d35 BER)
  (at ap9 d24 HAM)
  (at ap9 d25 HAM)
  (at ap10 d9 FRA)
  (at ap10 d15 BER)
  (at ap10 d22 BER)
  (at ap10 d29 BER)
  (at ap10 d20 HAM)
  (at ap10 d22 HAM)
  (at ap10 d34 HAM)
  (at ap10 d40 HAM)
  (at ap11 d1 FRA)
  (at ap11 d4 BER)
  (at ap11 d8 BER)
  (at ap11 d20 BER)
  (at ap11 d37 BER)
  (at ap11 d12 HAM)
  (at ap11 d27 HAM)
  (at ap11 d31 HAM)
  (at ap12 d10 FRA)
  (at ap12 d28 FRA)
  (at ap12 d8 BER)
  (at ap12 d13 BER)
  (at ap12 d17 BER)
  (at ap12 d36 BER)
  (at ap12 d22 HAM)
  (at ap12 d39 HAM)
  (at ap13 d1 FRA)
  (at ap13 d2 FRA)
  (at ap13 d9 FRA)
  (at ap13 d11 BER)
  (at ap13 d32 BER)
  (at ap13 d33 BER)
  (at ap13 d35 BER)
  (at ap13 d13 HAM)
  (at ap14 d33 FRA)
  (at ap14 d24 BER)
  (at ap14 d35 BER)
  (at ap14 d2 HAM)
  (at ap14 d18 HAM)
  (at ap14 d19 HAM)
  (at ap14 d33 HAM)
  (at ap14 d34 HAM)
  (at ap15 d13 FRA)
  (at ap15 d19 FRA)
  (at ap15 d21 FRA)
  (at ap15 d40 FRA)
  (at ap15 d1 BER)
  (at ap15 d31 BER)
  (at ap15 d40 BER)
  (at ap15 d25 HAM)
  (at ap16 d1 FRA)
  (at ap16 d1 FRA)
  (at ap16 d2 FRA)
  (at ap16 d9 FRA)
  (at ap16 d26 BER)
  (at ap16 d36 BER)
  (at ap16 d1 HAM)
  (at ap16 d39 HAM)
  (at ap17 d6 FRA)
  (at ap17 d11 FRA)
  (at ap17 d12 FRA)
  (at ap17 d11 BER)
  (at ap17 d30 BER)
  (at ap17 d4 HAM)
  (at ap17 d13 HAM)
  (at ap17 d30 HAM)
  (at ap18 d5 FRA)
  (at ap18 d6 FRA)
  (at ap18 d30 FRA)
  (at ap18 d35 FRA)
  (at ap18 d40 FRA)
  (at ap18 d30 BER)
  (at ap18 d3 HAM)
  (at ap18 d33 HAM)
  (at ap19 d3 FRA)
  (at ap19 d3 FRA)
  (at ap19 d19 FRA)
  (at ap19 d14 BER)
  (at ap19 d34 BER)
  (at ap19 d40 BER)
  (at ap19 d7 HAM)
  (at ap19 d24 HAM)
  (at ap20 d12 FRA)
  (at ap20 d28 FRA)
  (at ap20 d33 FRA)
  (at ap20 d1 BER)
  (at ap20 d12 BER)
  (at ap20 d36 BER)
  (at ap20 d10 HAM)
  (at ap20 d12 HAM)
  (at ap21 d1 FRA)
  (at ap21 d13 FRA)
  (at ap21 d16 FRA)
  (at ap21 d18 FRA)
  (at ap21 d27 FRA)
  (at ap21 d12 BER)
  (at ap21 d8 HAM)
  (at ap21 d17 HAM)
  (at ap22 d15 FRA)
  (at ap22 d5 BER)
  (at ap22 d37 BER)
  (at ap22 d3 HAM)
  (at ap22 d13 HAM)
  (at ap22 d19 HAM)
  (at ap22 d34 HAM)
  (at ap22 d36 HAM)
  (at ap23 d13 FRA)
  (at ap23 d23 FRA)
  (at ap23 d37 FRA)
  (at ap23 d39 FRA)
  (at ap23 d1 BER)
  (at ap23 d26 BER)
  (at ap23 d40 BER)
  (at ap23 d9 HAM)
  (at ap24 d2 FRA)
  (at ap24 d12 FRA)
  (at ap24 d24 FRA)
  (at ap24 d20 BER)
  (at ap24 d21 BER)
  (at ap24 d22 HAM)
  (at ap24 d33 HAM)
  (at ap24 d34 HAM)
  (at ap25 d1 FRA)
  (at ap25 d15 BER)
  (at ap25 d15 BER)
  (at ap25 d17 BER)
  (at ap25 d18 BER)
  (at ap25 d18 BER)
  (at ap25 d32 BER)
  (at ap25 d15 HAM)
  (at ap26 d5 FRA)
  (at ap26 d15 FRA)
  (at ap26 d25 FRA)
  (at ap26 d38 FRA)
  (at ap26 d40 FRA)
  (at ap26 d31 BER)
  (at ap26 d10 HAM)
  (at ap26 d13 HAM)
  (at ap27 d12 FRA)
  (at ap27 d17 FRA)
  (at ap27 d22 BER)
  (at ap27 d24 BER)
  (at ap27 d34 BER)
  (at ap27 d2 HAM)
  (at ap27 d14 HAM)
  (at ap27 d20 HAM)
  (at ap28 d1 FRA)
  (at ap28 d6 FRA)
  (at ap28 d17 FRA)
  (at ap28 d35 FRA)
  (at ap28 d8 BER)
  (at ap28 d2 HAM)
  (at ap28 d32 HAM)
  (at ap28 d33 HAM)
  (at ap29 d10 FRA)
  (at ap29 d18 FRA)
  (at ap29 d10 BER)
  (at ap29 d20 BER)
  (at ap29 d25 BER)
  (at ap29 d27 BER)
  (at ap29 d9 HAM)
  (at ap29 d39 HAM)
  (at ap30 d30 FRA)
  (at ap30 d39 FRA)
  (at ap30 d40 FRA)
  (at ap30 d4 BER)
  (at ap30 d20 BER)
  (at ap30 d9 HAM)
  (at ap30 d9 HAM)
  (at ap30 d18 HAM)
  (at ap31 d19 FRA)
  (at ap31 d1 BER)
  (at ap31 d9 BER)
  (at ap31 d24 BER)
  (at ap31 d27 BER)
  (at ap31 d39 BER)
  (at ap31 d2 HAM)
  (at ap31 d27 HAM)
  (at ap32 d32 FRA)
  (at ap32 d38 FRA)
  (at ap32 d5 BER)
  (at ap32 d14 BER)
  (at ap32 d33 BER)
  (at ap32 d34 BER)
  (at ap32 d40 BER)
  (at ap32 d12 HAM)
  (at ap33 d11 FRA)
  (at ap33 d26 FRA)
  (at ap33 d29 FRA)
  (at ap33 d31 FRA)
  (at ap33 d40 FRA)
  (at ap33 d4 BER)
  (at ap33 d7 BER)
  (at ap33 d8 BER)
  (at ap34 d5 FRA)
  (at ap34 d8 FRA)
  (at ap34 d37 FRA)
  (at ap34 d4 BER)
  (at ap34 d25 BER)
  (at ap34 d25 BER)
  (at ap34 d7 HAM)
  (at ap34 d35 HAM)
  (at ap35 d1 FRA)
  (at ap35 d6 FRA)
  (at ap35 d26 FRA)
  (at ap35 d4 BER)
  (at ap35 d7 BER)
  (at ap35 d13 BER)
  (at ap35 d31 BER)
  (at ap35 d6 HAM)
  (at ap36 d3 FRA)
  (at ap36 d21 FRA)
  (at ap36 d38 FRA)
  (at ap36 d1 BER)
  (at ap36 d26 BER)
  (at ap36 d29 BER)
  (at ap36 d40 BER)
  (at ap36 d5 HAM)
  (at ap37 d36 FRA)
  (at ap37 d19 BER)
  (at ap37 d27 BER)
  (at ap37 d27 BER)
  (at ap37 d35 BER)
  (at ap37 d40 BER)
  (at ap37 d2 HAM)
  (at ap37 d30 HAM)
  (at ap38 d29 FRA)
  (at ap38 d1 BER)
  (at ap38 d5 BER)
  (at ap38 d25 BER)
  (at ap38 d38 BER)
  (at ap38 d2 HAM)
  (at ap38 d12 HAM)
  (at ap38 d19 HAM)
  (at ap39 d14 FRA)
  (at ap39 d40 FRA)
  (at ap39 d14 BER)
  (at ap39 d35 BER)
  (at ap39 d17 HAM)
  (at ap39 d20 HAM)
  (at ap39 d22 HAM)
  (at ap39 d23 HAM)
  (at ap40 d14 FRA)
  (at ap40 d12 BER)
  (at ap40 d13 BER)
  (at ap40 d29 BER)
  (at ap40 d37 BER)
  (at ap40 d1 HAM)
  (at ap40 d16 HAM)
  (at ap40 d39 HAM)
  (at ap41 d15 FRA)
  (at ap41 d9 BER)
  (at ap41 d22 BER)
  (at ap41 d33 BER)
  (at ap41 d40 BER)
  (at ap41 d18 HAM)
  (at ap41 d28 HAM)
  (at ap41 d35 HAM)
  (at ap42 d17 FRA)
  (at ap42 d4 BER)
  (at ap42 d17 BER)
  (at ap42 d3 HAM)
  (at ap42 d5 HAM)
  (at ap42 d13 HAM)
  (at ap42 d19 HAM)
  (at ap42 d24 HAM)
  (at ap43 d16 FRA)
  (at ap43 d17 FRA)
  (at ap43 d18 FRA)
  (at ap43 d33 FRA)
  (at ap43 d6 BER)
  (at ap43 d9 BER)
  (at ap43 d22 HAM)
  (at ap43 d32 HAM)
  (at ap44 d21 FRA)
  (at ap44 d40 FRA)
  (at ap44 d40 FRA)
  (at ap44 d40 FRA)
  (at ap44 d3 BER)
  (at ap44 d6 HAM)
  (at ap44 d29 HAM)
  (at ap44 d36 HAM)
  (at ap45 d2 FRA)
  (at ap45 d3 FRA)
  (at ap45 d35 FRA)
  (at ap45 d6 BER)
  (at ap45 d19 BER)
  (at ap45 d35 BER)
  (at ap45 d1 HAM)
  (at ap45 d33 HAM)
  (at ap46 d23 FRA)
  (at ap46 d33 FRA)
  (at ap46 d3 BER)
  (at ap46 d12 BER)
  (at ap46 d36 BER)
  (at ap46 d1 HAM)
  (at ap46 d2 HAM)
  (at ap46 d20 HAM)
  (at ap47 d2 FRA)
  (at ap47 d5 FRA)
  (at ap47 d9 FRA)
  (at ap47 d17 FRA)
  (at ap47 d34 FRA)
  (at ap47 d16 BER)
  (at ap47 d36 BER)
  (at ap47 d25 HAM)
  (at ap48 d40 FRA)
  (at ap48 d3 BER)
  (at ap48 d24 BER)
  (at ap48 d25 BER)
  (at ap48 d29 BER)
  (at ap48 d20 HAM)
  (at ap48 d24 HAM)
  (at ap48 d39 HAM)
  (at ap49 d23 FRA)
  (at ap49 d31 FRA)
  (at ap49 d31 FRA)
  (at ap49 d38 FRA)
  (at ap49 d16 BER)
  (at ap49 d20 BER)
  (at ap49 d23 BER)
  (at ap49 d11 HAM)
  (at ap50 d23 FRA)
  (at ap50 d30 FRA)
  (at ap50 d32 FRA)
  (at ap50 d14 BER)
  (at ap50 d20 BER)
  (at ap50 d34 BER)
  (at ap50 d9 HAM)
  (at ap50 d37 HAM)
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
  ))
  )
