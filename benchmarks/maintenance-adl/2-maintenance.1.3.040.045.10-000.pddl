(define (problem maintenance-scheduling-1-3-40-45-10-0)
 (:domain maintenance-scheduling-domain)
 (:objects d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 d11 d12 d13 d14 d15 d16 d17 d18 d19 d20 d21 d22 d23 d24 d25 d26 d27 d28 d29 d30 d31 d32 d33 d34 d35 d36 d37 d38 d39 d40 d41 - day
   FRA BER HAM - airport
   ap1 ap2 ap3 ap4 ap5 ap6 ap7 ap8 ap9 ap10 ap11 ap12 ap13 ap14 ap15 ap16 ap17 ap18 ap19 ap20 ap21 ap22 ap23 ap24 ap25 ap26 ap27 ap28 ap29 ap30 ap31 ap32 ap33 ap34 ap35 ap36 ap37 ap38 ap39 ap40 ap41 ap42 ap43 ap44 ap45 - plane)
 (:init
  (today d1)  (today d2)  (today d3)  (today d4)  (today d5)  (today d6)  (today d7)  (today d8)  (today d9)  (today d10)  (today d11)  (today d12)  (today d13)  (today d14)  (today d15)  (today d16)  (today d17)  (today d18)  (today d19)  (today d20)  (today d21)  (today d22)  (today d23)  (today d24)  (today d25)  (today d26)  (today d27)  (today d28)  (today d29)  (today d30)  (today d31)  (today d32)  (today d33)  (today d34)  (today d35)  (today d36)  (today d37)  (today d38)  (today d39)  (today d40)  (at ap1 d1 BER)
  (at ap1 d4 BER)
  (at ap1 d8 BER)
  (at ap1 d8 BER)
  (at ap1 d1 HAM)
  (at ap1 d11 HAM)
  (at ap1 d13 HAM)
  (at ap1 d25 HAM)
  (at ap1 d28 HAM)
  (at ap1 d34 HAM)
  (at ap2 d1 FRA)
  (at ap2 d11 FRA)
  (at ap2 d20 FRA)
  (at ap2 d20 FRA)
  (at ap2 d17 BER)
  (at ap2 d20 BER)
  (at ap2 d24 BER)
  (at ap2 d33 BER)
  (at ap2 d18 HAM)
  (at ap2 d30 HAM)
  (at ap3 d34 FRA)
  (at ap3 d11 BER)
  (at ap3 d29 BER)
  (at ap3 d29 BER)
  (at ap3 d35 BER)
  (at ap3 d2 HAM)
  (at ap3 d5 HAM)
  (at ap3 d6 HAM)
  (at ap3 d8 HAM)
  (at ap3 d29 HAM)
  (at ap4 d18 FRA)
  (at ap4 d24 FRA)
  (at ap4 d2 BER)
  (at ap4 d3 BER)
  (at ap4 d19 BER)
  (at ap4 d26 BER)
  (at ap4 d29 BER)
  (at ap4 d36 BER)
  (at ap4 d26 HAM)
  (at ap4 d30 HAM)
  (at ap5 d12 FRA)
  (at ap5 d37 FRA)
  (at ap5 d39 FRA)
  (at ap5 d26 BER)
  (at ap5 d30 BER)
  (at ap5 d36 BER)
  (at ap5 d26 HAM)
  (at ap5 d29 HAM)
  (at ap5 d33 HAM)
  (at ap5 d38 HAM)
  (at ap6 d15 FRA)
  (at ap6 d18 FRA)
  (at ap6 d22 FRA)
  (at ap6 d26 FRA)
  (at ap6 d23 BER)
  (at ap6 d9 HAM)
  (at ap6 d10 HAM)
  (at ap6 d18 HAM)
  (at ap6 d32 HAM)
  (at ap6 d36 HAM)
  (at ap7 d7 FRA)
  (at ap7 d8 FRA)
  (at ap7 d36 FRA)
  (at ap7 d3 BER)
  (at ap7 d16 BER)
  (at ap7 d21 BER)
  (at ap7 d23 BER)
  (at ap7 d34 BER)
  (at ap7 d35 BER)
  (at ap7 d32 HAM)
  (at ap8 d16 FRA)
  (at ap8 d19 FRA)
  (at ap8 d26 FRA)
  (at ap8 d31 FRA)
  (at ap8 d1 BER)
  (at ap8 d13 BER)
  (at ap8 d16 BER)
  (at ap8 d10 HAM)
  (at ap8 d15 HAM)
  (at ap8 d17 HAM)
  (at ap9 d4 FRA)
  (at ap9 d24 FRA)
  (at ap9 d27 FRA)
  (at ap9 d31 FRA)
  (at ap9 d5 BER)
  (at ap9 d7 BER)
  (at ap9 d39 BER)
  (at ap9 d4 HAM)
  (at ap9 d14 HAM)
  (at ap9 d21 HAM)
  (at ap10 d8 FRA)
  (at ap10 d16 FRA)
  (at ap10 d33 FRA)
  (at ap10 d40 FRA)
  (at ap10 d17 BER)
  (at ap10 d24 BER)
  (at ap10 d26 BER)
  (at ap10 d3 HAM)
  (at ap10 d25 HAM)
  (at ap10 d26 HAM)
  (at ap11 d2 FRA)
  (at ap11 d13 FRA)
  (at ap11 d23 FRA)
  (at ap11 d5 BER)
  (at ap11 d9 BER)
  (at ap11 d30 BER)
  (at ap11 d34 BER)
  (at ap11 d19 HAM)
  (at ap11 d26 HAM)
  (at ap11 d32 HAM)
  (at ap12 d8 FRA)
  (at ap12 d35 FRA)
  (at ap12 d36 FRA)
  (at ap12 d36 FRA)
  (at ap12 d8 BER)
  (at ap12 d15 BER)
  (at ap12 d37 BER)
  (at ap12 d16 HAM)
  (at ap12 d33 HAM)
  (at ap12 d34 HAM)
  (at ap13 d10 FRA)
  (at ap13 d12 FRA)
  (at ap13 d16 FRA)
  (at ap13 d1 BER)
  (at ap13 d4 BER)
  (at ap13 d12 BER)
  (at ap13 d37 BER)
  (at ap13 d4 HAM)
  (at ap13 d5 HAM)
  (at ap13 d9 HAM)
  (at ap14 d5 FRA)
  (at ap14 d9 FRA)
  (at ap14 d10 FRA)
  (at ap14 d18 FRA)
  (at ap14 d31 FRA)
  (at ap14 d38 FRA)
  (at ap14 d10 BER)
  (at ap14 d15 BER)
  (at ap14 d20 BER)
  (at ap14 d1 HAM)
  (at ap15 d8 FRA)
  (at ap15 d26 FRA)
  (at ap15 d29 FRA)
  (at ap15 d35 FRA)
  (at ap15 d10 BER)
  (at ap15 d34 BER)
  (at ap15 d14 HAM)
  (at ap15 d17 HAM)
  (at ap15 d19 HAM)
  (at ap15 d25 HAM)
  (at ap16 d1 FRA)
  (at ap16 d11 FRA)
  (at ap16 d19 FRA)
  (at ap16 d36 FRA)
  (at ap16 d39 FRA)
  (at ap16 d14 BER)
  (at ap16 d38 BER)
  (at ap16 d17 HAM)
  (at ap16 d31 HAM)
  (at ap16 d38 HAM)
  (at ap17 d20 FRA)
  (at ap17 d30 FRA)
  (at ap17 d36 FRA)
  (at ap17 d39 FRA)
  (at ap17 d1 BER)
  (at ap17 d38 BER)
  (at ap17 d14 HAM)
  (at ap17 d15 HAM)
  (at ap17 d32 HAM)
  (at ap17 d34 HAM)
  (at ap18 d36 FRA)
  (at ap18 d36 FRA)
  (at ap18 d38 FRA)
  (at ap18 d1 BER)
  (at ap18 d1 BER)
  (at ap18 d10 BER)
  (at ap18 d35 BER)
  (at ap18 d28 HAM)
  (at ap18 d34 HAM)
  (at ap18 d34 HAM)
  (at ap19 d4 FRA)
  (at ap19 d20 FRA)
  (at ap19 d25 FRA)
  (at ap19 d34 FRA)
  (at ap19 d36 FRA)
  (at ap19 d19 BER)
  (at ap19 d29 BER)
  (at ap19 d38 BER)
  (at ap19 d7 HAM)
  (at ap19 d28 HAM)
  (at ap20 d31 FRA)
  (at ap20 d1 BER)
  (at ap20 d14 BER)
  (at ap20 d18 BER)
  (at ap20 d21 BER)
  (at ap20 d21 BER)
  (at ap20 d31 BER)
  (at ap20 d33 BER)
  (at ap20 d4 HAM)
  (at ap20 d14 HAM)
  (at ap21 d17 FRA)
  (at ap21 d1 BER)
  (at ap21 d11 BER)
  (at ap21 d24 BER)
  (at ap21 d30 BER)
  (at ap21 d31 BER)
  (at ap21 d31 BER)
  (at ap21 d10 HAM)
  (at ap21 d27 HAM)
  (at ap21 d30 HAM)
  (at ap22 d6 FRA)
  (at ap22 d10 FRA)
  (at ap22 d17 FRA)
  (at ap22 d32 FRA)
  (at ap22 d39 FRA)
  (at ap22 d1 HAM)
  (at ap22 d5 HAM)
  (at ap22 d19 HAM)
  (at ap22 d21 HAM)
  (at ap22 d25 HAM)
  (at ap23 d5 FRA)
  (at ap23 d9 FRA)
  (at ap23 d11 FRA)
  (at ap23 d13 FRA)
  (at ap23 d18 BER)
  (at ap23 d1 HAM)
  (at ap23 d4 HAM)
  (at ap23 d11 HAM)
  (at ap23 d29 HAM)
  (at ap23 d34 HAM)
  (at ap24 d2 FRA)
  (at ap24 d12 FRA)
  (at ap24 d28 FRA)
  (at ap24 d40 FRA)
  (at ap24 d21 BER)
  (at ap24 d36 BER)
  (at ap24 d6 HAM)
  (at ap24 d15 HAM)
  (at ap24 d29 HAM)
  (at ap24 d31 HAM)
  (at ap25 d1 FRA)
  (at ap25 d19 FRA)
  (at ap25 d19 BER)
  (at ap25 d26 BER)
  (at ap25 d33 BER)
  (at ap25 d40 BER)
  (at ap25 d7 HAM)
  (at ap25 d14 HAM)
  (at ap25 d21 HAM)
  (at ap25 d25 HAM)
  (at ap26 d9 FRA)
  (at ap26 d22 FRA)
  (at ap26 d39 FRA)
  (at ap26 d1 BER)
  (at ap26 d8 BER)
  (at ap26 d12 BER)
  (at ap26 d28 BER)
  (at ap26 d35 BER)
  (at ap26 d12 HAM)
  (at ap26 d32 HAM)
  (at ap27 d24 FRA)
  (at ap27 d26 FRA)
  (at ap27 d7 BER)
  (at ap27 d18 BER)
  (at ap27 d29 BER)
  (at ap27 d31 BER)
  (at ap27 d2 HAM)
  (at ap27 d3 HAM)
  (at ap27 d5 HAM)
  (at ap27 d36 HAM)
  (at ap28 d8 FRA)
  (at ap28 d22 FRA)
  (at ap28 d25 FRA)
  (at ap28 d26 FRA)
  (at ap28 d6 BER)
  (at ap28 d17 BER)
  (at ap28 d32 BER)
  (at ap28 d37 BER)
  (at ap28 d17 HAM)
  (at ap28 d28 HAM)
  (at ap29 d8 FRA)
  (at ap29 d26 FRA)
  (at ap29 d1 BER)
  (at ap29 d15 BER)
  (at ap29 d19 BER)
  (at ap29 d39 BER)
  (at ap29 d11 HAM)
  (at ap29 d12 HAM)
  (at ap29 d29 HAM)
  (at ap29 d35 HAM)
  (at ap30 d6 FRA)
  (at ap30 d9 FRA)
  (at ap30 d12 FRA)
  (at ap30 d16 FRA)
  (at ap30 d31 FRA)
  (at ap30 d38 FRA)
  (at ap30 d25 BER)
  (at ap30 d38 BER)
  (at ap30 d16 HAM)
  (at ap30 d21 HAM)
  (at ap31 d1 FRA)
  (at ap31 d8 FRA)
  (at ap31 d20 FRA)
  (at ap31 d10 BER)
  (at ap31 d14 BER)
  (at ap31 d31 BER)
  (at ap31 d6 HAM)
  (at ap31 d7 HAM)
  (at ap31 d24 HAM)
  (at ap31 d39 HAM)
  (at ap32 d6 FRA)
  (at ap32 d15 FRA)
  (at ap32 d36 FRA)
  (at ap32 d40 FRA)
  (at ap32 d8 BER)
  (at ap32 d17 BER)
  (at ap32 d40 BER)
  (at ap32 d30 HAM)
  (at ap32 d33 HAM)
  (at ap32 d40 HAM)
  (at ap33 d5 FRA)
  (at ap33 d2 BER)
  (at ap33 d5 BER)
  (at ap33 d22 BER)
  (at ap33 d24 BER)
  (at ap33 d40 BER)
  (at ap33 d20 HAM)
  (at ap33 d27 HAM)
  (at ap33 d32 HAM)
  (at ap33 d39 HAM)
  (at ap34 d28 FRA)
  (at ap34 d40 FRA)
  (at ap34 d10 BER)
  (at ap34 d18 BER)
  (at ap34 d33 BER)
  (at ap34 d3 HAM)
  (at ap34 d15 HAM)
  (at ap34 d24 HAM)
  (at ap34 d37 HAM)
  (at ap34 d40 HAM)
  (at ap35 d22 FRA)
  (at ap35 d27 FRA)
  (at ap35 d15 BER)
  (at ap35 d33 BER)
  (at ap35 d36 BER)
  (at ap35 d2 HAM)
  (at ap35 d14 HAM)
  (at ap35 d24 HAM)
  (at ap35 d32 HAM)
  (at ap35 d38 HAM)
  (at ap36 d4 FRA)
  (at ap36 d6 FRA)
  (at ap36 d7 FRA)
  (at ap36 d31 BER)
  (at ap36 d33 BER)
  (at ap36 d36 BER)
  (at ap36 d40 BER)
  (at ap36 d11 HAM)
  (at ap36 d24 HAM)
  (at ap36 d40 HAM)
  (at ap37 d9 FRA)
  (at ap37 d18 FRA)
  (at ap37 d20 BER)
  (at ap37 d32 BER)
  (at ap37 d11 HAM)
  (at ap37 d12 HAM)
  (at ap37 d17 HAM)
  (at ap37 d33 HAM)
  (at ap37 d37 HAM)
  (at ap37 d40 HAM)
  (at ap38 d5 FRA)
  (at ap38 d2 BER)
  (at ap38 d2 BER)
  (at ap38 d18 BER)
  (at ap38 d27 BER)
  (at ap38 d31 BER)
  (at ap38 d32 BER)
  (at ap38 d5 HAM)
  (at ap38 d6 HAM)
  (at ap38 d20 HAM)
  (at ap39 d8 FRA)
  (at ap39 d13 FRA)
  (at ap39 d20 FRA)
  (at ap39 d37 FRA)
  (at ap39 d15 BER)
  (at ap39 d31 BER)
  (at ap39 d18 HAM)
  (at ap39 d20 HAM)
  (at ap39 d23 HAM)
  (at ap39 d40 HAM)
  (at ap40 d5 FRA)
  (at ap40 d9 FRA)
  (at ap40 d16 FRA)
  (at ap40 d9 BER)
  (at ap40 d14 BER)
  (at ap40 d6 HAM)
  (at ap40 d12 HAM)
  (at ap40 d18 HAM)
  (at ap40 d30 HAM)
  (at ap40 d31 HAM)
  (at ap41 d4 FRA)
  (at ap41 d10 FRA)
  (at ap41 d16 FRA)
  (at ap41 d21 FRA)
  (at ap41 d24 FRA)
  (at ap41 d13 BER)
  (at ap41 d23 BER)
  (at ap41 d24 BER)
  (at ap41 d5 HAM)
  (at ap41 d36 HAM)
  (at ap42 d9 FRA)
  (at ap42 d10 FRA)
  (at ap42 d11 FRA)
  (at ap42 d14 FRA)
  (at ap42 d22 FRA)
  (at ap42 d21 BER)
  (at ap42 d22 BER)
  (at ap42 d37 BER)
  (at ap42 d9 HAM)
  (at ap42 d37 HAM)
  (at ap43 d5 FRA)
  (at ap43 d21 FRA)
  (at ap43 d34 FRA)
  (at ap43 d37 FRA)
  (at ap43 d40 FRA)
  (at ap43 d11 BER)
  (at ap43 d19 BER)
  (at ap43 d21 HAM)
  (at ap43 d34 HAM)
  (at ap43 d40 HAM)
  (at ap44 d18 FRA)
  (at ap44 d19 FRA)
  (at ap44 d21 FRA)
  (at ap44 d24 FRA)
  (at ap44 d25 FRA)
  (at ap44 d10 BER)
  (at ap44 d11 BER)
  (at ap44 d30 BER)
  (at ap44 d6 HAM)
  (at ap44 d13 HAM)
  (at ap45 d11 FRA)
  (at ap45 d12 FRA)
  (at ap45 d24 FRA)
  (at ap45 d38 FRA)
  (at ap45 d18 BER)
  (at ap45 d26 BER)
  (at ap45 d36 BER)
  (at ap45 d19 HAM)
  (at ap45 d24 HAM)
  (at ap45 d32 HAM)
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
