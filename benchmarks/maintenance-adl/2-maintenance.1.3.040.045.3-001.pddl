(define (problem maintenance-scheduling-1-3-40-45-3-1)
 (:domain maintenance-scheduling-domain)
 (:objects d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 d11 d12 d13 d14 d15 d16 d17 d18 d19 d20 d21 d22 d23 d24 d25 d26 d27 d28 d29 d30 d31 d32 d33 d34 d35 d36 d37 d38 d39 d40 d41 - day
   FRA BER HAM - airport
   ap1 ap2 ap3 ap4 ap5 ap6 ap7 ap8 ap9 ap10 ap11 ap12 ap13 ap14 ap15 ap16 ap17 ap18 ap19 ap20 ap21 ap22 ap23 ap24 ap25 ap26 ap27 ap28 ap29 ap30 ap31 ap32 ap33 ap34 ap35 ap36 ap37 ap38 ap39 ap40 ap41 ap42 ap43 ap44 ap45 - plane)
 (:init
  (today d1)  (today d2)  (today d3)  (today d4)  (today d5)  (today d6)  (today d7)  (today d8)  (today d9)  (today d10)  (today d11)  (today d12)  (today d13)  (today d14)  (today d15)  (today d16)  (today d17)  (today d18)  (today d19)  (today d20)  (today d21)  (today d22)  (today d23)  (today d24)  (today d25)  (today d26)  (today d27)  (today d28)  (today d29)  (today d30)  (today d31)  (today d32)  (today d33)  (today d34)  (today d35)  (today d36)  (today d37)  (today d38)  (today d39)  (today d40)  (at ap1 d5 FRA)
  (at ap1 d38 FRA)
  (at ap1 d1 HAM)
  (at ap2 d9 FRA)
  (at ap2 d31 FRA)
  (at ap2 d17 HAM)
  (at ap3 d29 FRA)
  (at ap3 d35 FRA)
  (at ap3 d25 HAM)
  (at ap4 d10 BER)
  (at ap4 d34 BER)
  (at ap4 d19 HAM)
  (at ap5 d8 FRA)
  (at ap5 d26 FRA)
  (at ap5 d14 HAM)
  (at ap6 d1 FRA)
  (at ap6 d14 BER)
  (at ap6 d31 HAM)
  (at ap7 d11 FRA)
  (at ap7 d36 FRA)
  (at ap7 d17 HAM)
  (at ap8 d19 FRA)
  (at ap8 d38 BER)
  (at ap8 d38 HAM)
  (at ap9 d20 FRA)
  (at ap9 d39 FRA)
  (at ap9 d14 HAM)
  (at ap10 d1 BER)
  (at ap10 d32 HAM)
  (at ap10 d34 HAM)
  (at ap11 d30 FRA)
  (at ap11 d39 FRA)
  (at ap11 d38 BER)
  (at ap12 d36 FRA)
  (at ap12 d1 BER)
  (at ap12 d15 HAM)
  (at ap13 d36 FRA)
  (at ap13 d1 BER)
  (at ap13 d28 HAM)
  (at ap14 d36 FRA)
  (at ap14 d34 HAM)
  (at ap14 d34 HAM)
  (at ap15 d38 FRA)
  (at ap15 d10 BER)
  (at ap15 d35 BER)
  (at ap16 d4 FRA)
  (at ap16 d20 FRA)
  (at ap16 d38 BER)
  (at ap17 d34 FRA)
  (at ap17 d36 FRA)
  (at ap17 d28 HAM)
  (at ap18 d19 BER)
  (at ap18 d29 BER)
  (at ap18 d7 HAM)
  (at ap19 d25 FRA)
  (at ap19 d14 BER)
  (at ap19 d18 BER)
  (at ap20 d31 FRA)
  (at ap20 d1 BER)
  (at ap20 d21 BER)
  (at ap21 d31 BER)
  (at ap21 d4 HAM)
  (at ap21 d14 HAM)
  (at ap22 d21 BER)
  (at ap22 d30 BER)
  (at ap22 d33 BER)
  (at ap23 d24 BER)
  (at ap23 d31 BER)
  (at ap23 d10 HAM)
  (at ap24 d1 BER)
  (at ap24 d11 BER)
  (at ap24 d30 HAM)
  (at ap25 d17 FRA)
  (at ap25 d31 BER)
  (at ap25 d27 HAM)
  (at ap26 d17 FRA)
  (at ap26 d5 HAM)
  (at ap26 d25 HAM)
  (at ap27 d32 FRA)
  (at ap27 d39 FRA)
  (at ap27 d21 HAM)
  (at ap28 d6 FRA)
  (at ap28 d10 FRA)
  (at ap28 d19 HAM)
  (at ap29 d13 FRA)
  (at ap29 d1 HAM)
  (at ap29 d29 HAM)
  (at ap30 d5 FRA)
  (at ap30 d9 FRA)
  (at ap30 d18 BER)
  (at ap31 d11 FRA)
  (at ap31 d11 HAM)
  (at ap31 d34 HAM)
  (at ap32 d28 FRA)
  (at ap32 d1 HAM)
  (at ap32 d4 HAM)
  (at ap33 d12 FRA)
  (at ap33 d40 FRA)
  (at ap33 d31 HAM)
  (at ap34 d2 FRA)
  (at ap34 d15 HAM)
  (at ap34 d29 HAM)
  (at ap35 d21 BER)
  (at ap35 d36 BER)
  (at ap35 d6 HAM)
  (at ap36 d26 BER)
  (at ap36 d40 BER)
  (at ap36 d14 HAM)
  (at ap37 d19 BER)
  (at ap37 d7 HAM)
  (at ap37 d25 HAM)
  (at ap38 d1 FRA)
  (at ap38 d33 BER)
  (at ap38 d21 HAM)
  (at ap39 d19 FRA)
  (at ap39 d12 BER)
  (at ap39 d12 HAM)
  (at ap40 d9 FRA)
  (at ap40 d22 FRA)
  (at ap40 d1 BER)
  (at ap41 d39 FRA)
  (at ap41 d28 BER)
  (at ap41 d32 HAM)
  (at ap42 d8 BER)
  (at ap42 d35 BER)
  (at ap42 d36 HAM)
  (at ap43 d7 BER)
  (at ap43 d3 HAM)
  (at ap43 d5 HAM)
  (at ap44 d24 FRA)
  (at ap44 d18 BER)
  (at ap44 d29 BER)
  (at ap45 d26 FRA)
  (at ap45 d31 BER)
  (at ap45 d2 HAM)
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
  ))
  )