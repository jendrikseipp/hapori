(define (problem maintenance-scheduling-1-3-50-50-10-1)
 (:domain maintenance-scheduling-domain)
 (:objects d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 d11 d12 d13 d14 d15 d16 d17 d18 d19 d20 d21 d22 d23 d24 d25 d26 d27 d28 d29 d30 d31 d32 d33 d34 d35 d36 d37 d38 d39 d40 d41 d42 d43 d44 d45 d46 d47 d48 d49 d50 d51 - day
   FRA BER HAM - airport
   ap1 ap2 ap3 ap4 ap5 ap6 ap7 ap8 ap9 ap10 ap11 ap12 ap13 ap14 ap15 ap16 ap17 ap18 ap19 ap20 ap21 ap22 ap23 ap24 ap25 ap26 ap27 ap28 ap29 ap30 ap31 ap32 ap33 ap34 ap35 ap36 ap37 ap38 ap39 ap40 ap41 ap42 ap43 ap44 ap45 ap46 ap47 ap48 ap49 ap50 - plane)
 (:init
  (today d1)  (today d2)  (today d3)  (today d4)  (today d5)  (today d6)  (today d7)  (today d8)  (today d9)  (today d10)  (today d11)  (today d12)  (today d13)  (today d14)  (today d15)  (today d16)  (today d17)  (today d18)  (today d19)  (today d20)  (today d21)  (today d22)  (today d23)  (today d24)  (today d25)  (today d26)  (today d27)  (today d28)  (today d29)  (today d30)  (today d31)  (today d32)  (today d33)  (today d34)  (today d35)  (today d36)  (today d37)  (today d38)  (today d39)  (today d40)  (today d41)  (today d42)  (today d43)  (today d44)  (today d45)  (today d46)  (today d47)  (today d48)  (today d49)  (today d50)  (at ap1 d1 FRA)
  (at ap1 d22 FRA)
  (at ap1 d29 FRA)
  (at ap1 d33 FRA)
  (at ap1 d5 BER)
  (at ap1 d34 BER)
  (at ap1 d45 BER)
  (at ap1 d4 HAM)
  (at ap1 d28 HAM)
  (at ap1 d33 HAM)
  (at ap2 d1 FRA)
  (at ap2 d3 FRA)
  (at ap2 d19 FRA)
  (at ap2 d50 FRA)
  (at ap2 d21 BER)
  (at ap2 d21 BER)
  (at ap2 d50 BER)
  (at ap2 d19 HAM)
  (at ap2 d22 HAM)
  (at ap2 d45 HAM)
  (at ap3 d2 FRA)
  (at ap3 d12 FRA)
  (at ap3 d41 FRA)
  (at ap3 d41 FRA)
  (at ap3 d49 FRA)
  (at ap3 d16 BER)
  (at ap3 d21 BER)
  (at ap3 d36 BER)
  (at ap3 d1 HAM)
  (at ap3 d29 HAM)
  (at ap4 d1 FRA)
  (at ap4 d25 FRA)
  (at ap4 d26 FRA)
  (at ap4 d45 FRA)
  (at ap4 d20 BER)
  (at ap4 d20 BER)
  (at ap4 d20 HAM)
  (at ap4 d23 HAM)
  (at ap4 d43 HAM)
  (at ap4 d44 HAM)
  (at ap5 d20 FRA)
  (at ap5 d23 FRA)
  (at ap5 d23 FRA)
  (at ap5 d26 FRA)
  (at ap5 d50 FRA)
  (at ap5 d40 BER)
  (at ap5 d44 BER)
  (at ap5 d13 HAM)
  (at ap5 d17 HAM)
  (at ap5 d24 HAM)
  (at ap6 d2 FRA)
  (at ap6 d13 FRA)
  (at ap6 d29 FRA)
  (at ap6 d48 FRA)
  (at ap6 d6 BER)
  (at ap6 d21 BER)
  (at ap6 d22 BER)
  (at ap6 d44 BER)
  (at ap6 d12 HAM)
  (at ap6 d40 HAM)
  (at ap7 d8 FRA)
  (at ap7 d13 FRA)
  (at ap7 d17 FRA)
  (at ap7 d41 FRA)
  (at ap7 d45 FRA)
  (at ap7 d46 FRA)
  (at ap7 d2 BER)
  (at ap7 d8 HAM)
  (at ap7 d37 HAM)
  (at ap7 d39 HAM)
  (at ap8 d13 FRA)
  (at ap8 d17 FRA)
  (at ap8 d20 BER)
  (at ap8 d35 BER)
  (at ap8 d37 BER)
  (at ap8 d13 HAM)
  (at ap8 d24 HAM)
  (at ap8 d26 HAM)
  (at ap8 d29 HAM)
  (at ap8 d43 HAM)
  (at ap9 d2 FRA)
  (at ap9 d23 FRA)
  (at ap9 d34 FRA)
  (at ap9 d49 FRA)
  (at ap9 d1 BER)
  (at ap9 d1 BER)
  (at ap9 d46 BER)
  (at ap9 d14 HAM)
  (at ap9 d22 HAM)
  (at ap9 d43 HAM)
  (at ap10 d11 FRA)
  (at ap10 d32 FRA)
  (at ap10 d5 BER)
  (at ap10 d7 BER)
  (at ap10 d15 BER)
  (at ap10 d20 BER)
  (at ap10 d38 BER)
  (at ap10 d42 BER)
  (at ap10 d48 BER)
  (at ap10 d25 HAM)
  (at ap11 d15 FRA)
  (at ap11 d28 FRA)
  (at ap11 d35 FRA)
  (at ap11 d35 FRA)
  (at ap11 d50 FRA)
  (at ap11 d1 BER)
  (at ap11 d34 BER)
  (at ap11 d10 HAM)
  (at ap11 d10 HAM)
  (at ap11 d13 HAM)
  (at ap12 d21 FRA)
  (at ap12 d42 FRA)
  (at ap12 d47 FRA)
  (at ap12 d4 BER)
  (at ap12 d12 BER)
  (at ap12 d38 BER)
  (at ap12 d4 HAM)
  (at ap12 d22 HAM)
  (at ap12 d23 HAM)
  (at ap12 d42 HAM)
  (at ap13 d18 FRA)
  (at ap13 d25 FRA)
  (at ap13 d27 FRA)
  (at ap13 d46 FRA)
  (at ap13 d27 BER)
  (at ap13 d30 BER)
  (at ap13 d30 BER)
  (at ap13 d45 BER)
  (at ap13 d2 HAM)
  (at ap13 d49 HAM)
  (at ap14 d19 FRA)
  (at ap14 d30 FRA)
  (at ap14 d30 FRA)
  (at ap14 d50 FRA)
  (at ap14 d20 BER)
  (at ap14 d34 BER)
  (at ap14 d19 HAM)
  (at ap14 d38 HAM)
  (at ap14 d39 HAM)
  (at ap14 d39 HAM)
  (at ap15 d19 FRA)
  (at ap15 d28 FRA)
  (at ap15 d14 BER)
  (at ap15 d19 BER)
  (at ap15 d20 BER)
  (at ap15 d21 BER)
  (at ap15 d29 BER)
  (at ap15 d47 BER)
  (at ap15 d7 HAM)
  (at ap15 d12 HAM)
  (at ap16 d11 FRA)
  (at ap16 d16 FRA)
  (at ap16 d19 FRA)
  (at ap16 d21 FRA)
  (at ap16 d22 FRA)
  (at ap16 d4 BER)
  (at ap16 d4 BER)
  (at ap16 d5 BER)
  (at ap16 d33 BER)
  (at ap16 d32 HAM)
  (at ap17 d7 FRA)
  (at ap17 d25 FRA)
  (at ap17 d30 FRA)
  (at ap17 d14 BER)
  (at ap17 d15 BER)
  (at ap17 d17 BER)
  (at ap17 d28 BER)
  (at ap17 d34 BER)
  (at ap17 d35 BER)
  (at ap17 d37 HAM)
  (at ap18 d6 FRA)
  (at ap18 d11 FRA)
  (at ap18 d18 FRA)
  (at ap18 d36 FRA)
  (at ap18 d3 BER)
  (at ap18 d4 BER)
  (at ap18 d7 BER)
  (at ap18 d11 BER)
  (at ap18 d36 HAM)
  (at ap18 d45 HAM)
  (at ap19 d1 FRA)
  (at ap19 d3 FRA)
  (at ap19 d18 FRA)
  (at ap19 d1 BER)
  (at ap19 d29 BER)
  (at ap19 d29 BER)
  (at ap19 d36 BER)
  (at ap19 d50 BER)
  (at ap19 d35 HAM)
  (at ap19 d40 HAM)
  (at ap20 d46 FRA)
  (at ap20 d7 BER)
  (at ap20 d15 BER)
  (at ap20 d20 BER)
  (at ap20 d27 BER)
  (at ap20 d45 BER)
  (at ap20 d2 HAM)
  (at ap20 d9 HAM)
  (at ap20 d22 HAM)
  (at ap20 d32 HAM)
  (at ap21 d9 FRA)
  (at ap21 d34 FRA)
  (at ap21 d50 FRA)
  (at ap21 d15 BER)
  (at ap21 d21 BER)
  (at ap21 d25 BER)
  (at ap21 d48 BER)
  (at ap21 d22 HAM)
  (at ap21 d37 HAM)
  (at ap21 d40 HAM)
  (at ap22 d14 FRA)
  (at ap22 d37 BER)
  (at ap22 d39 BER)
  (at ap22 d42 BER)
  (at ap22 d43 BER)
  (at ap22 d44 BER)
  (at ap22 d11 HAM)
  (at ap22 d13 HAM)
  (at ap22 d26 HAM)
  (at ap22 d39 HAM)
  (at ap23 d5 FRA)
  (at ap23 d17 BER)
  (at ap23 d22 BER)
  (at ap23 d29 BER)
  (at ap23 d30 BER)
  (at ap23 d33 BER)
  (at ap23 d35 HAM)
  (at ap23 d38 HAM)
  (at ap23 d39 HAM)
  (at ap23 d48 HAM)
  (at ap24 d37 FRA)
  (at ap24 d46 FRA)
  (at ap24 d47 FRA)
  (at ap24 d9 BER)
  (at ap24 d14 BER)
  (at ap24 d3 HAM)
  (at ap24 d13 HAM)
  (at ap24 d25 HAM)
  (at ap24 d32 HAM)
  (at ap24 d34 HAM)
  (at ap25 d13 FRA)
  (at ap25 d20 FRA)
  (at ap25 d38 FRA)
  (at ap25 d40 FRA)
  (at ap25 d50 FRA)
  (at ap25 d23 BER)
  (at ap25 d36 BER)
  (at ap25 d9 HAM)
  (at ap25 d16 HAM)
  (at ap25 d32 HAM)
  (at ap26 d3 FRA)
  (at ap26 d31 FRA)
  (at ap26 d32 FRA)
  (at ap26 d35 FRA)
  (at ap26 d9 BER)
  (at ap26 d26 BER)
  (at ap26 d45 BER)
  (at ap26 d31 HAM)
  (at ap26 d36 HAM)
  (at ap26 d43 HAM)
  (at ap27 d15 FRA)
  (at ap27 d23 FRA)
  (at ap27 d43 FRA)
  (at ap27 d2 BER)
  (at ap27 d6 BER)
  (at ap27 d23 BER)
  (at ap27 d36 BER)
  (at ap27 d11 HAM)
  (at ap27 d30 HAM)
  (at ap27 d42 HAM)
  (at ap28 d14 FRA)
  (at ap28 d17 FRA)
  (at ap28 d29 FRA)
  (at ap28 d32 FRA)
  (at ap28 d6 BER)
  (at ap28 d15 BER)
  (at ap28 d23 BER)
  (at ap28 d44 BER)
  (at ap28 d5 HAM)
  (at ap28 d19 HAM)
  (at ap29 d18 FRA)
  (at ap29 d40 FRA)
  (at ap29 d41 FRA)
  (at ap29 d10 BER)
  (at ap29 d16 BER)
  (at ap29 d33 BER)
  (at ap29 d39 BER)
  (at ap29 d11 HAM)
  (at ap29 d20 HAM)
  (at ap29 d44 HAM)
  (at ap30 d2 FRA)
  (at ap30 d3 FRA)
  (at ap30 d20 FRA)
  (at ap30 d21 FRA)
  (at ap30 d43 FRA)
  (at ap30 d4 BER)
  (at ap30 d14 BER)
  (at ap30 d50 BER)
  (at ap30 d9 HAM)
  (at ap30 d17 HAM)
  (at ap31 d27 FRA)
  (at ap31 d33 FRA)
  (at ap31 d3 BER)
  (at ap31 d3 BER)
  (at ap31 d4 BER)
  (at ap31 d23 BER)
  (at ap31 d30 BER)
  (at ap31 d6 HAM)
  (at ap31 d30 HAM)
  (at ap31 d37 HAM)
  (at ap32 d14 BER)
  (at ap32 d37 BER)
  (at ap32 d42 BER)
  (at ap32 d49 BER)
  (at ap32 d18 HAM)
  (at ap32 d23 HAM)
  (at ap32 d27 HAM)
  (at ap32 d28 HAM)
  (at ap32 d34 HAM)
  (at ap32 d41 HAM)
  (at ap33 d5 FRA)
  (at ap33 d13 FRA)
  (at ap33 d34 FRA)
  (at ap33 d4 BER)
  (at ap33 d10 BER)
  (at ap33 d16 BER)
  (at ap33 d43 BER)
  (at ap33 d9 HAM)
  (at ap33 d11 HAM)
  (at ap33 d31 HAM)
  (at ap34 d9 FRA)
  (at ap34 d13 FRA)
  (at ap34 d21 FRA)
  (at ap34 d28 FRA)
  (at ap34 d8 BER)
  (at ap34 d28 BER)
  (at ap34 d46 BER)
  (at ap34 d8 HAM)
  (at ap34 d27 HAM)
  (at ap34 d38 HAM)
  (at ap35 d9 FRA)
  (at ap35 d10 FRA)
  (at ap35 d23 FRA)
  (at ap35 d32 FRA)
  (at ap35 d18 BER)
  (at ap35 d2 HAM)
  (at ap35 d8 HAM)
  (at ap35 d17 HAM)
  (at ap35 d25 HAM)
  (at ap35 d44 HAM)
  (at ap36 d16 FRA)
  (at ap36 d32 FRA)
  (at ap36 d38 FRA)
  (at ap36 d45 FRA)
  (at ap36 d10 BER)
  (at ap36 d16 BER)
  (at ap36 d24 BER)
  (at ap36 d39 BER)
  (at ap36 d12 HAM)
  (at ap36 d49 HAM)
  (at ap37 d5 FRA)
  (at ap37 d12 FRA)
  (at ap37 d37 FRA)
  (at ap37 d46 FRA)
  (at ap37 d25 BER)
  (at ap37 d26 BER)
  (at ap37 d32 BER)
  (at ap37 d8 HAM)
  (at ap37 d40 HAM)
  (at ap37 d44 HAM)
  (at ap38 d14 FRA)
  (at ap38 d15 FRA)
  (at ap38 d18 FRA)
  (at ap38 d30 FRA)
  (at ap38 d40 FRA)
  (at ap38 d26 BER)
  (at ap38 d31 BER)
  (at ap38 d6 HAM)
  (at ap38 d15 HAM)
  (at ap38 d49 HAM)
  (at ap39 d10 FRA)
  (at ap39 d30 FRA)
  (at ap39 d19 BER)
  (at ap39 d25 BER)
  (at ap39 d30 BER)
  (at ap39 d50 BER)
  (at ap39 d9 HAM)
  (at ap39 d17 HAM)
  (at ap39 d34 HAM)
  (at ap39 d36 HAM)
  (at ap40 d14 FRA)
  (at ap40 d37 FRA)
  (at ap40 d15 BER)
  (at ap40 d16 BER)
  (at ap40 d26 BER)
  (at ap40 d43 BER)
  (at ap40 d7 HAM)
  (at ap40 d7 HAM)
  (at ap40 d34 HAM)
  (at ap40 d48 HAM)
  (at ap41 d7 FRA)
  (at ap41 d14 FRA)
  (at ap41 d18 FRA)
  (at ap41 d24 FRA)
  (at ap41 d21 BER)
  (at ap41 d5 HAM)
  (at ap41 d5 HAM)
  (at ap41 d13 HAM)
  (at ap41 d31 HAM)
  (at ap41 d49 HAM)
  (at ap42 d21 FRA)
  (at ap42 d32 FRA)
  (at ap42 d48 FRA)
  (at ap42 d19 BER)
  (at ap42 d22 BER)
  (at ap42 d34 BER)
  (at ap42 d18 HAM)
  (at ap42 d20 HAM)
  (at ap42 d21 HAM)
  (at ap42 d30 HAM)
  (at ap43 d14 BER)
  (at ap43 d18 BER)
  (at ap43 d21 BER)
  (at ap43 d45 BER)
  (at ap43 d50 BER)
  (at ap43 d2 HAM)
  (at ap43 d5 HAM)
  (at ap43 d7 HAM)
  (at ap43 d26 HAM)
  (at ap43 d42 HAM)
  (at ap44 d16 FRA)
  (at ap44 d21 FRA)
  (at ap44 d22 FRA)
  (at ap44 d11 BER)
  (at ap44 d26 BER)
  (at ap44 d43 BER)
  (at ap44 d15 HAM)
  (at ap44 d35 HAM)
  (at ap44 d38 HAM)
  (at ap44 d45 HAM)
  (at ap45 d7 FRA)
  (at ap45 d40 FRA)
  (at ap45 d13 BER)
  (at ap45 d24 BER)
  (at ap45 d25 BER)
  (at ap45 d28 BER)
  (at ap45 d28 HAM)
  (at ap45 d31 HAM)
  (at ap45 d34 HAM)
  (at ap45 d39 HAM)
  (at ap46 d11 FRA)
  (at ap46 d22 FRA)
  (at ap46 d32 FRA)
  (at ap46 d38 FRA)
  (at ap46 d45 FRA)
  (at ap46 d46 FRA)
  (at ap46 d44 BER)
  (at ap46 d7 HAM)
  (at ap46 d20 HAM)
  (at ap46 d28 HAM)
  (at ap47 d2 FRA)
  (at ap47 d3 FRA)
  (at ap47 d23 FRA)
  (at ap47 d28 FRA)
  (at ap47 d27 BER)
  (at ap47 d34 BER)
  (at ap47 d12 HAM)
  (at ap47 d31 HAM)
  (at ap47 d44 HAM)
  (at ap47 d47 HAM)
  (at ap48 d8 FRA)
  (at ap48 d26 FRA)
  (at ap48 d26 FRA)
  (at ap48 d28 FRA)
  (at ap48 d16 BER)
  (at ap48 d40 BER)
  (at ap48 d48 BER)
  (at ap48 d18 HAM)
  (at ap48 d24 HAM)
  (at ap48 d31 HAM)
  (at ap49 d19 FRA)
  (at ap49 d25 FRA)
  (at ap49 d28 FRA)
  (at ap49 d1 BER)
  (at ap49 d4 BER)
  (at ap49 d28 BER)
  (at ap49 d49 BER)
  (at ap49 d50 BER)
  (at ap49 d2 HAM)
  (at ap49 d23 HAM)
  (at ap50 d10 FRA)
  (at ap50 d15 FRA)
  (at ap50 d19 FRA)
  (at ap50 d24 FRA)
  (at ap50 d5 BER)
  (at ap50 d21 BER)
  (at ap50 d21 BER)
  (at ap50 d20 HAM)
  (at ap50 d25 HAM)
  (at ap50 d46 HAM)
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
