(define (domain slitherlink)
(:requirements
    :strips
    :typing
    :negative-preconditions
    :equality
    :conditional-effects
)
(:types
    node - object
    cell - object
    cell-capacity-level - object
)
(:constants
    cap-0 - cell-capacity-level
    cap-1 - cell-capacity-level
    cap-2 - cell-capacity-level
    cap-3 - cell-capacity-level
    cap-4 - cell-capacity-level
    n-0 - node
    n-1 - node
    n-100 - node
    n-1009 - node
    n-101 - node
    n-1010 - node
    n-1012 - node
    n-1013 - node
    n-1016 - node
    n-1017 - node
    n-1033 - node
    n-1034 - node
    n-1036 - node
    n-1037 - node
    n-104 - node
    n-1040 - node
    n-1041 - node
    n-1044 - node
    n-1045 - node
    n-105 - node
    n-1057 - node
    n-1058 - node
    n-1060 - node
    n-1061 - node
    n-1064 - node
    n-1065 - node
    n-1081 - node
    n-1082 - node
    n-1084 - node
    n-1085 - node
    n-1088 - node
    n-1089 - node
    n-1105 - node
    n-1106 - node
    n-1108 - node
    n-1109 - node
    n-1112 - node
    n-1113 - node
    n-1129 - node
    n-113 - node
    n-1130 - node
    n-1132 - node
    n-1133 - node
    n-1136 - node
    n-1137 - node
    n-1153 - node
    n-1154 - node
    n-1156 - node
    n-1157 - node
    n-116 - node
    n-1160 - node
    n-1161 - node
    n-1177 - node
    n-1178 - node
    n-1180 - node
    n-1181 - node
    n-1184 - node
    n-1185 - node
    n-12 - node
    n-1201 - node
    n-1202 - node
    n-1204 - node
    n-1205 - node
    n-1208 - node
    n-1209 - node
    n-121 - node
    n-1212 - node
    n-1213 - node
    n-122 - node
    n-1225 - node
    n-1226 - node
    n-1228 - node
    n-1229 - node
    n-1232 - node
    n-1233 - node
    n-124 - node
    n-1249 - node
    n-125 - node
    n-1250 - node
    n-1252 - node
    n-1253 - node
    n-1256 - node
    n-1257 - node
    n-1273 - node
    n-1274 - node
    n-1276 - node
    n-1277 - node
    n-128 - node
    n-1280 - node
    n-1281 - node
    n-129 - node
    n-1297 - node
    n-1298 - node
    n-13 - node
    n-1300 - node
    n-1301 - node
    n-1304 - node
    n-1305 - node
    n-132 - node
    n-1321 - node
    n-1322 - node
    n-1324 - node
    n-1325 - node
    n-1328 - node
    n-1329 - node
    n-133 - node
    n-1345 - node
    n-1346 - node
    n-1348 - node
    n-1349 - node
    n-1352 - node
    n-1353 - node
    n-1356 - node
    n-1357 - node
    n-136 - node
    n-1369 - node
    n-137 - node
    n-1370 - node
    n-1372 - node
    n-1373 - node
    n-1376 - node
    n-1377 - node
    n-1393 - node
    n-1394 - node
    n-1396 - node
    n-1397 - node
    n-1400 - node
    n-1401 - node
    n-1417 - node
    n-1418 - node
    n-1420 - node
    n-1421 - node
    n-1424 - node
    n-1425 - node
    n-1441 - node
    n-1442 - node
    n-1444 - node
    n-1445 - node
    n-1448 - node
    n-1449 - node
    n-145 - node
    n-146 - node
    n-148 - node
    n-149 - node
    n-152 - node
    n-153 - node
    n-16 - node
    n-169 - node
    n-17 - node
    n-170 - node
    n-172 - node
    n-173 - node
    n-176 - node
    n-177 - node
    n-193 - node
    n-194 - node
    n-196 - node
    n-197 - node
    n-2 - node
    n-20 - node
    n-200 - node
    n-201 - node
    n-217 - node
    n-218 - node
    n-220 - node
    n-221 - node
    n-224 - node
    n-225 - node
    n-24 - node
    n-240 - node
    n-241 - node
    n-242 - node
    n-243 - node
    n-244 - node
    n-245 - node
    n-248 - node
    n-249 - node
    n-25 - node
    n-26 - node
    n-265 - node
    n-266 - node
    n-268 - node
    n-269 - node
    n-27 - node
    n-272 - node
    n-273 - node
    n-276 - node
    n-277 - node
    n-28 - node
    n-280 - node
    n-281 - node
    n-289 - node
    n-29 - node
    n-290 - node
    n-292 - node
    n-293 - node
    n-296 - node
    n-297 - node
    n-3 - node
    n-313 - node
    n-314 - node
    n-316 - node
    n-317 - node
    n-32 - node
    n-320 - node
    n-321 - node
    n-33 - node
    n-337 - node
    n-338 - node
    n-340 - node
    n-341 - node
    n-344 - node
    n-345 - node
    n-361 - node
    n-362 - node
    n-364 - node
    n-365 - node
    n-368 - node
    n-369 - node
    n-385 - node
    n-386 - node
    n-388 - node
    n-389 - node
    n-392 - node
    n-393 - node
    n-4 - node
    n-408 - node
    n-409 - node
    n-41 - node
    n-410 - node
    n-411 - node
    n-412 - node
    n-413 - node
    n-416 - node
    n-417 - node
    n-433 - node
    n-434 - node
    n-436 - node
    n-437 - node
    n-44 - node
    n-440 - node
    n-441 - node
    n-444 - node
    n-445 - node
    n-448 - node
    n-449 - node
    n-457 - node
    n-458 - node
    n-460 - node
    n-461 - node
    n-464 - node
    n-465 - node
    n-48 - node
    n-481 - node
    n-482 - node
    n-484 - node
    n-485 - node
    n-488 - node
    n-489 - node
    n-49 - node
    n-5 - node
    n-50 - node
    n-505 - node
    n-506 - node
    n-508 - node
    n-509 - node
    n-51 - node
    n-512 - node
    n-513 - node
    n-52 - node
    n-529 - node
    n-53 - node
    n-530 - node
    n-532 - node
    n-533 - node
    n-536 - node
    n-537 - node
    n-553 - node
    n-554 - node
    n-556 - node
    n-557 - node
    n-56 - node
    n-560 - node
    n-561 - node
    n-57 - node
    n-577 - node
    n-578 - node
    n-580 - node
    n-581 - node
    n-584 - node
    n-585 - node
    n-600 - node
    n-601 - node
    n-602 - node
    n-603 - node
    n-604 - node
    n-605 - node
    n-608 - node
    n-609 - node
    n-625 - node
    n-626 - node
    n-628 - node
    n-629 - node
    n-632 - node
    n-633 - node
    n-636 - node
    n-637 - node
    n-640 - node
    n-641 - node
    n-649 - node
    n-65 - node
    n-650 - node
    n-652 - node
    n-653 - node
    n-656 - node
    n-657 - node
    n-673 - node
    n-674 - node
    n-676 - node
    n-677 - node
    n-68 - node
    n-680 - node
    n-681 - node
    n-697 - node
    n-698 - node
    n-700 - node
    n-701 - node
    n-704 - node
    n-705 - node
    n-72 - node
    n-721 - node
    n-722 - node
    n-724 - node
    n-725 - node
    n-728 - node
    n-729 - node
    n-73 - node
    n-74 - node
    n-745 - node
    n-746 - node
    n-748 - node
    n-749 - node
    n-75 - node
    n-752 - node
    n-753 - node
    n-76 - node
    n-769 - node
    n-77 - node
    n-770 - node
    n-772 - node
    n-773 - node
    n-776 - node
    n-777 - node
    n-793 - node
    n-794 - node
    n-796 - node
    n-797 - node
    n-8 - node
    n-80 - node
    n-800 - node
    n-801 - node
    n-81 - node
    n-816 - node
    n-817 - node
    n-818 - node
    n-819 - node
    n-820 - node
    n-821 - node
    n-824 - node
    n-825 - node
    n-841 - node
    n-842 - node
    n-844 - node
    n-845 - node
    n-848 - node
    n-849 - node
    n-852 - node
    n-853 - node
    n-865 - node
    n-866 - node
    n-868 - node
    n-869 - node
    n-872 - node
    n-873 - node
    n-889 - node
    n-89 - node
    n-890 - node
    n-892 - node
    n-893 - node
    n-896 - node
    n-897 - node
    n-9 - node
    n-913 - node
    n-914 - node
    n-916 - node
    n-917 - node
    n-92 - node
    n-920 - node
    n-921 - node
    n-937 - node
    n-938 - node
    n-940 - node
    n-941 - node
    n-944 - node
    n-945 - node
    n-96 - node
    n-961 - node
    n-962 - node
    n-964 - node
    n-965 - node
    n-968 - node
    n-969 - node
    n-97 - node
    n-98 - node
    n-985 - node
    n-986 - node
    n-988 - node
    n-989 - node
    n-99 - node
    n-992 - node
    n-993 - node
    c-c0 - cell
    c-c1 - cell
    c-c10 - cell
    c-c100 - cell
    c-c101 - cell
    c-c102 - cell
    c-c103 - cell
    c-c104 - cell
    c-c105 - cell
    c-c106 - cell
    c-c107 - cell
    c-c108 - cell
    c-c109 - cell
    c-c11 - cell
    c-c110 - cell
    c-c111 - cell
    c-c112 - cell
    c-c113 - cell
    c-c114 - cell
    c-c115 - cell
    c-c116 - cell
    c-c117 - cell
    c-c118 - cell
    c-c119 - cell
    c-c12 - cell
    c-c120 - cell
    c-c121 - cell
    c-c122 - cell
    c-c123 - cell
    c-c124 - cell
    c-c125 - cell
    c-c126 - cell
    c-c127 - cell
    c-c128 - cell
    c-c129 - cell
    c-c13 - cell
    c-c130 - cell
    c-c131 - cell
    c-c132 - cell
    c-c133 - cell
    c-c134 - cell
    c-c135 - cell
    c-c136 - cell
    c-c137 - cell
    c-c138 - cell
    c-c139 - cell
    c-c14 - cell
    c-c140 - cell
    c-c141 - cell
    c-c142 - cell
    c-c143 - cell
    c-c144 - cell
    c-c145 - cell
    c-c146 - cell
    c-c147 - cell
    c-c148 - cell
    c-c149 - cell
    c-c15 - cell
    c-c150 - cell
    c-c151 - cell
    c-c152 - cell
    c-c153 - cell
    c-c154 - cell
    c-c155 - cell
    c-c156 - cell
    c-c157 - cell
    c-c158 - cell
    c-c159 - cell
    c-c16 - cell
    c-c160 - cell
    c-c161 - cell
    c-c162 - cell
    c-c163 - cell
    c-c164 - cell
    c-c165 - cell
    c-c166 - cell
    c-c167 - cell
    c-c168 - cell
    c-c169 - cell
    c-c17 - cell
    c-c170 - cell
    c-c171 - cell
    c-c172 - cell
    c-c173 - cell
    c-c174 - cell
    c-c175 - cell
    c-c176 - cell
    c-c177 - cell
    c-c178 - cell
    c-c179 - cell
    c-c18 - cell
    c-c180 - cell
    c-c181 - cell
    c-c182 - cell
    c-c183 - cell
    c-c184 - cell
    c-c185 - cell
    c-c186 - cell
    c-c187 - cell
    c-c188 - cell
    c-c189 - cell
    c-c19 - cell
    c-c190 - cell
    c-c191 - cell
    c-c192 - cell
    c-c193 - cell
    c-c194 - cell
    c-c195 - cell
    c-c196 - cell
    c-c197 - cell
    c-c198 - cell
    c-c199 - cell
    c-c2 - cell
    c-c20 - cell
    c-c200 - cell
    c-c201 - cell
    c-c202 - cell
    c-c203 - cell
    c-c204 - cell
    c-c205 - cell
    c-c206 - cell
    c-c207 - cell
    c-c208 - cell
    c-c209 - cell
    c-c21 - cell
    c-c210 - cell
    c-c211 - cell
    c-c212 - cell
    c-c213 - cell
    c-c214 - cell
    c-c215 - cell
    c-c216 - cell
    c-c217 - cell
    c-c218 - cell
    c-c219 - cell
    c-c22 - cell
    c-c220 - cell
    c-c221 - cell
    c-c222 - cell
    c-c223 - cell
    c-c224 - cell
    c-c225 - cell
    c-c226 - cell
    c-c227 - cell
    c-c228 - cell
    c-c229 - cell
    c-c23 - cell
    c-c230 - cell
    c-c231 - cell
    c-c232 - cell
    c-c233 - cell
    c-c234 - cell
    c-c235 - cell
    c-c236 - cell
    c-c237 - cell
    c-c238 - cell
    c-c239 - cell
    c-c24 - cell
    c-c240 - cell
    c-c241 - cell
    c-c242 - cell
    c-c243 - cell
    c-c244 - cell
    c-c245 - cell
    c-c246 - cell
    c-c247 - cell
    c-c248 - cell
    c-c249 - cell
    c-c25 - cell
    c-c250 - cell
    c-c251 - cell
    c-c252 - cell
    c-c253 - cell
    c-c254 - cell
    c-c255 - cell
    c-c256 - cell
    c-c257 - cell
    c-c258 - cell
    c-c259 - cell
    c-c26 - cell
    c-c260 - cell
    c-c261 - cell
    c-c262 - cell
    c-c263 - cell
    c-c264 - cell
    c-c265 - cell
    c-c266 - cell
    c-c267 - cell
    c-c268 - cell
    c-c269 - cell
    c-c27 - cell
    c-c270 - cell
    c-c271 - cell
    c-c272 - cell
    c-c273 - cell
    c-c274 - cell
    c-c275 - cell
    c-c276 - cell
    c-c277 - cell
    c-c278 - cell
    c-c279 - cell
    c-c28 - cell
    c-c280 - cell
    c-c281 - cell
    c-c282 - cell
    c-c283 - cell
    c-c284 - cell
    c-c285 - cell
    c-c286 - cell
    c-c287 - cell
    c-c288 - cell
    c-c289 - cell
    c-c29 - cell
    c-c290 - cell
    c-c291 - cell
    c-c292 - cell
    c-c293 - cell
    c-c294 - cell
    c-c295 - cell
    c-c296 - cell
    c-c297 - cell
    c-c298 - cell
    c-c299 - cell
    c-c3 - cell
    c-c30 - cell
    c-c300 - cell
    c-c301 - cell
    c-c302 - cell
    c-c303 - cell
    c-c304 - cell
    c-c305 - cell
    c-c306 - cell
    c-c307 - cell
    c-c308 - cell
    c-c309 - cell
    c-c31 - cell
    c-c310 - cell
    c-c311 - cell
    c-c312 - cell
    c-c313 - cell
    c-c314 - cell
    c-c315 - cell
    c-c316 - cell
    c-c317 - cell
    c-c318 - cell
    c-c319 - cell
    c-c32 - cell
    c-c320 - cell
    c-c321 - cell
    c-c322 - cell
    c-c323 - cell
    c-c324 - cell
    c-c325 - cell
    c-c326 - cell
    c-c327 - cell
    c-c328 - cell
    c-c329 - cell
    c-c33 - cell
    c-c330 - cell
    c-c331 - cell
    c-c332 - cell
    c-c333 - cell
    c-c334 - cell
    c-c335 - cell
    c-c336 - cell
    c-c337 - cell
    c-c338 - cell
    c-c339 - cell
    c-c34 - cell
    c-c340 - cell
    c-c341 - cell
    c-c342 - cell
    c-c343 - cell
    c-c344 - cell
    c-c345 - cell
    c-c346 - cell
    c-c347 - cell
    c-c348 - cell
    c-c349 - cell
    c-c35 - cell
    c-c350 - cell
    c-c351 - cell
    c-c352 - cell
    c-c353 - cell
    c-c354 - cell
    c-c355 - cell
    c-c356 - cell
    c-c357 - cell
    c-c358 - cell
    c-c359 - cell
    c-c36 - cell
    c-c360 - cell
    c-c361 - cell
    c-c362 - cell
    c-c363 - cell
    c-c364 - cell
    c-c365 - cell
    c-c37 - cell
    c-c38 - cell
    c-c39 - cell
    c-c4 - cell
    c-c40 - cell
    c-c41 - cell
    c-c42 - cell
    c-c43 - cell
    c-c44 - cell
    c-c45 - cell
    c-c46 - cell
    c-c47 - cell
    c-c48 - cell
    c-c49 - cell
    c-c5 - cell
    c-c50 - cell
    c-c51 - cell
    c-c52 - cell
    c-c53 - cell
    c-c54 - cell
    c-c55 - cell
    c-c56 - cell
    c-c57 - cell
    c-c58 - cell
    c-c59 - cell
    c-c6 - cell
    c-c60 - cell
    c-c61 - cell
    c-c62 - cell
    c-c63 - cell
    c-c64 - cell
    c-c65 - cell
    c-c66 - cell
    c-c67 - cell
    c-c68 - cell
    c-c69 - cell
    c-c7 - cell
    c-c70 - cell
    c-c71 - cell
    c-c72 - cell
    c-c73 - cell
    c-c74 - cell
    c-c75 - cell
    c-c76 - cell
    c-c77 - cell
    c-c78 - cell
    c-c79 - cell
    c-c8 - cell
    c-c80 - cell
    c-c81 - cell
    c-c82 - cell
    c-c83 - cell
    c-c84 - cell
    c-c85 - cell
    c-c86 - cell
    c-c87 - cell
    c-c88 - cell
    c-c89 - cell
    c-c9 - cell
    c-c90 - cell
    c-c91 - cell
    c-c92 - cell
    c-c93 - cell
    c-c94 - cell
    c-c95 - cell
    c-c96 - cell
    c-c97 - cell
    c-c98 - cell
    c-c99 - cell
    c-outside-cell-0-41 - cell
    c-outside-cell-100-243 - cell
    c-outside-cell-1009-1012 - cell
    c-outside-cell-1012-1013 - cell
    c-outside-cell-1013-1016 - cell
    c-outside-cell-1016-1177 - cell
    c-outside-cell-1040-1041 - cell
    c-outside-cell-1041-1044 - cell
    c-outside-cell-1044-1045 - cell
    c-outside-cell-1045-848 - cell
    c-outside-cell-113-116 - cell
    c-outside-cell-116-99 - cell
    c-outside-cell-1177-1180 - cell
    c-outside-cell-1180-1181 - cell
    c-outside-cell-1181-1184 - cell
    c-outside-cell-1184-1321 - cell
    c-outside-cell-12-13 - cell
    c-outside-cell-1208-1209 - cell
    c-outside-cell-1209-1212 - cell
    c-outside-cell-1212-1213 - cell
    c-outside-cell-1213-1040 - cell
    c-outside-cell-13-16 - cell
    c-outside-cell-132-133 - cell
    c-outside-cell-1321-1324 - cell
    c-outside-cell-1324-1325 - cell
    c-outside-cell-1325-1328 - cell
    c-outside-cell-1328-1441 - cell
    c-outside-cell-133-136 - cell
    c-outside-cell-1348-1349 - cell
    c-outside-cell-1349-1352 - cell
    c-outside-cell-1352-1353 - cell
    c-outside-cell-1353-1356 - cell
    c-outside-cell-1356-1357 - cell
    c-outside-cell-1357-1208 - cell
    c-outside-cell-136-137 - cell
    c-outside-cell-137-12 - cell
    c-outside-cell-1372-1373 - cell
    c-outside-cell-1373-1376 - cell
    c-outside-cell-1376-1377 - cell
    c-outside-cell-1377-1348 - cell
    c-outside-cell-1396-1397 - cell
    c-outside-cell-1397-1400 - cell
    c-outside-cell-1400-1401 - cell
    c-outside-cell-1401-1372 - cell
    c-outside-cell-1420-1421 - cell
    c-outside-cell-1421-1424 - cell
    c-outside-cell-1424-1425 - cell
    c-outside-cell-1425-1396 - cell
    c-outside-cell-1441-1444 - cell
    c-outside-cell-1444-1445 - cell
    c-outside-cell-1445-1448 - cell
    c-outside-cell-1448-1449 - cell
    c-outside-cell-1449-1420 - cell
    c-outside-cell-16-17 - cell
    c-outside-cell-17-20 - cell
    c-outside-cell-20-3 - cell
    c-outside-cell-24-65 - cell
    c-outside-cell-240-241 - cell
    c-outside-cell-241-244 - cell
    c-outside-cell-243-240 - cell
    c-outside-cell-244-411 - cell
    c-outside-cell-27-24 - cell
    c-outside-cell-276-277 - cell
    c-outside-cell-277-280 - cell
    c-outside-cell-280-281 - cell
    c-outside-cell-281-132 - cell
    c-outside-cell-3-0 - cell
    c-outside-cell-408-409 - cell
    c-outside-cell-409-412 - cell
    c-outside-cell-41-44 - cell
    c-outside-cell-411-408 - cell
    c-outside-cell-412-603 - cell
    c-outside-cell-44-27 - cell
    c-outside-cell-444-445 - cell
    c-outside-cell-445-448 - cell
    c-outside-cell-448-449 - cell
    c-outside-cell-449-276 - cell
    c-outside-cell-48-89 - cell
    c-outside-cell-51-48 - cell
    c-outside-cell-600-601 - cell
    c-outside-cell-601-604 - cell
    c-outside-cell-603-600 - cell
    c-outside-cell-604-819 - cell
    c-outside-cell-632-633 - cell
    c-outside-cell-633-636 - cell
    c-outside-cell-636-637 - cell
    c-outside-cell-637-640 - cell
    c-outside-cell-640-641 - cell
    c-outside-cell-641-444 - cell
    c-outside-cell-65-68 - cell
    c-outside-cell-68-51 - cell
    c-outside-cell-72-113 - cell
    c-outside-cell-75-72 - cell
    c-outside-cell-816-817 - cell
    c-outside-cell-817-820 - cell
    c-outside-cell-819-816 - cell
    c-outside-cell-820-821 - cell
    c-outside-cell-821-824 - cell
    c-outside-cell-824-1009 - cell
    c-outside-cell-848-849 - cell
    c-outside-cell-849-852 - cell
    c-outside-cell-852-853 - cell
    c-outside-cell-853-632 - cell
    c-outside-cell-89-92 - cell
    c-outside-cell-92-75 - cell
    c-outside-cell-96-97 - cell
    c-outside-cell-97-100 - cell
    c-outside-cell-99-96 - cell
)
(:predicates
    (cell-capacity-inc ?x0 - cell-capacity-level ?x1 - cell-capacity-level)
    (cell-capacity ?x0 - cell ?x1 - cell-capacity-level)
    (cell-edge ?x0 - cell ?x1 - cell ?x2 - node ?x3 - node)
    (node-degree0 ?x0 - node)
    (node-degree1 ?x0 - node)
    (node-degree2 ?x0 - node)
    (linked ?x0 - node ?x1 - node)
    (disable-link-0-0)
    (NOT-node-degree1 ?x0 - node)
)
(:action link-0-0
    :parameters (?n1 - node ?n2 - node ?c1 - cell ?c1capfrom - cell-capacity-level ?c1capto - cell-capacity-level ?c2 - cell ?c2capfrom - cell-capacity-level ?c2capto - cell-capacity-level)
    :precondition (and (not (linked ?n1 ?n2)) (node-degree0 ?n1) (node-degree0 ?n2) (cell-edge ?c1 ?c2 ?n1 ?n2) (cell-capacity ?c1 ?c1capfrom) (cell-capacity ?c2 ?c2capfrom) (cell-capacity-inc ?c1capto ?c1capfrom) (cell-capacity-inc ?c2capto ?c2capfrom) (not (disable-link-0-0)))
    :effect (and (linked ?n1 ?n2) (not (node-degree0 ?n1)) (not (node-degree0 ?n2)) (not (cell-capacity ?c1 ?c1capfrom)) (cell-capacity ?c1 ?c1capto) (not (cell-capacity ?c2 ?c2capfrom)) (cell-capacity ?c2 ?c2capto) (disable-link-0-0) (node-degree1 ?n1) (not (NOT-node-degree1 ?n1)) (node-degree1 ?n2) (not (NOT-node-degree1 ?n2)))
)

(:action link-0-1
    :parameters (?n1 - node ?n2 - node ?c1 - cell ?c1capfrom - cell-capacity-level ?c1capto - cell-capacity-level ?c2 - cell ?c2capfrom - cell-capacity-level ?c2capto - cell-capacity-level)
    :precondition (and (not (linked ?n1 ?n2)) (node-degree0 ?n1) (node-degree1 ?n2) (cell-edge ?c1 ?c2 ?n1 ?n2) (cell-capacity ?c1 ?c1capfrom) (cell-capacity ?c2 ?c2capfrom) (cell-capacity-inc ?c1capto ?c1capfrom) (cell-capacity-inc ?c2capto ?c2capfrom))
    :effect (and (linked ?n1 ?n2) (not (node-degree0 ?n1)) (node-degree2 ?n2) (not (cell-capacity ?c1 ?c1capfrom)) (cell-capacity ?c1 ?c1capto) (not (cell-capacity ?c2 ?c2capfrom)) (cell-capacity ?c2 ?c2capto) (node-degree1 ?n1) (not (NOT-node-degree1 ?n1)) (not (node-degree1 ?n2)) (NOT-node-degree1 ?n2))
)

(:action link-1-0
    :parameters (?n1 - node ?n2 - node ?c1 - cell ?c1capfrom - cell-capacity-level ?c1capto - cell-capacity-level ?c2 - cell ?c2capfrom - cell-capacity-level ?c2capto - cell-capacity-level)
    :precondition (and (not (linked ?n1 ?n2)) (node-degree1 ?n1) (node-degree0 ?n2) (cell-edge ?c1 ?c2 ?n1 ?n2) (cell-capacity ?c1 ?c1capfrom) (cell-capacity ?c2 ?c2capfrom) (cell-capacity-inc ?c1capto ?c1capfrom) (cell-capacity-inc ?c2capto ?c2capfrom))
    :effect (and (linked ?n1 ?n2) (node-degree2 ?n1) (not (node-degree0 ?n2)) (not (cell-capacity ?c1 ?c1capfrom)) (cell-capacity ?c1 ?c1capto) (not (cell-capacity ?c2 ?c2capfrom)) (cell-capacity ?c2 ?c2capto) (not (node-degree1 ?n1)) (NOT-node-degree1 ?n1) (node-degree1 ?n2) (not (NOT-node-degree1 ?n2)))
)

(:action link-1-1
    :parameters (?n1 - node ?n2 - node ?c1 - cell ?c1capfrom - cell-capacity-level ?c1capto - cell-capacity-level ?c2 - cell ?c2capfrom - cell-capacity-level ?c2capto - cell-capacity-level)
    :precondition (and (not (linked ?n1 ?n2)) (node-degree1 ?n1) (node-degree1 ?n2) (cell-edge ?c1 ?c2 ?n1 ?n2) (cell-capacity ?c1 ?c1capfrom) (cell-capacity ?c2 ?c2capfrom) (cell-capacity-inc ?c1capto ?c1capfrom) (cell-capacity-inc ?c2capto ?c2capfrom))
    :effect (and (linked ?n1 ?n2) (node-degree2 ?n1) (node-degree2 ?n2) (not (cell-capacity ?c1 ?c1capfrom)) (cell-capacity ?c1 ?c1capto) (not (cell-capacity ?c2 ?c2capfrom)) (cell-capacity ?c2 ?c2capto) (not (node-degree1 ?n1)) (NOT-node-degree1 ?n1) (not (node-degree1 ?n2)) (NOT-node-degree1 ?n2))
)

)
