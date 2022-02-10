package bench

import RegexPackage._
import annotation.experimental
import org.openjdk.jmh.annotations._

@experimental
class RegexBench3 {

@Benchmark
def run(): Unit = {

val r1 = Regex3("((((((((((a))))))))))"); "a" match { case r1(g0, g1, g2, g3, g4, g5, g6, g7, g8, g9) => assert((g0, g1, g2, g3, g4, g5, g6, g7, g8, g9) == ("a", "a", "a", "a", "a", "a", "a", "a", "a", "a")) }
val r2 = Regex3("((((((((((a))))))))))\\10"); "aa" match { case r2(g0, g1, g2, g3, g4, g5, g6, g7, g8, g9) => assert((g0, g1, g2, g3, g4, g5, g6, g7, g8, g9) == ("a", "a", "a", "a", "a", "a", "a", "a", "a", "a")) }
val r3 = Regex3("(((((((((a)))))))))"); "a" match { case r3(g0, g1, g2, g3, g4, g5, g6, g7, g8) => assert((g0, g1, g2, g3, g4, g5, g6, g7, g8) == ("a", "a", "a", "a", "a", "a", "a", "a", "a")) }
val r4 = Regex3("((?:aaaa|bbbb)cccc)?"); "aaaacccc" match { case r4(g0) => assert((g0) == ("aaaacccc")) }
val r5 = Regex3("((?:aaaa|bbbb)cccc)?"); "bbbbcccc" match { case r5(g0) => assert((g0) == ("bbbbcccc")) }
val r6 = Regex3("((?i)a)b"); "ab" match { case r6(g0) => assert((g0) == ("a")) }
val r7 = Regex3("((?i)a)b"); "Ab" match { case r7(g0) => assert((g0) == ("A")) }
val r8 = Regex3("((?i:a))b"); "ab" match { case r8(g0) => assert((g0) == ("a")) }
val r9 = Regex3("((?i:a))b"); "Ab" match { case r9(g0) => assert((g0) == ("A")) }
val r10 = Regex3("(([a-c])b*?\\2)*"); "ababbbcbc" match { case r10(g0, g1) => assert((g0, g1) == ("cbc", "c")) }
val r11 = Regex3("(([a-c])b*?\\2){3}"); "ababbbcbc" match { case r11(g0, g1) => assert((g0, g1) == ("cbc", "c")) }
val r12 = Regex3("((a)(b)c)(d)"); "abcd" match { case r12(g0, g1, g2, g3) => assert((g0, g1, g2, g3) == ("abc", "a", "b", "d")) }
val r13 = Regex3("((foo)|(bar))*"); "foobar" match { case r13(g0, g1, g2) => assert((g0, g1, g2) == ("bar", "foo", "bar")) }
val r14 = Regex3("(.*)c(.*)"); "abcde" match { case r14(g0, g1) => assert((g0, g1) == ("ab", "de")) }
val r15 = Regex3("(?:(f)(o)(o)|(b)(a)(r))*"); "foobar" match { case r15(g0, g1, g2, g3, g4, g5) => assert((g0, g1, g2, g3, g4, g5) == ("f", "o", "o", "b", "a", "r")) }
val r16 = Regex3("([[:digit:]-[:alpha:]]+)"); "-" match { case r16(g0) => assert((g0) == ("-")) }
val r17 = Regex3("([[:digit:]-z]+)"); "-" match { case r17(g0) => assert((g0) == ("-")) }
val r18 = Regex3("([\\d-\\s]+)"); "-" match { case r18(g0) => assert((g0) == ("-")) }
val r19 = Regex3("([\\d-z]+)"); "-" match { case r19(g0) => assert((g0) == ("-")) }
val r20 = Regex3("([\\w:]+::)?(\\w+)$"); "abcd" match { case r20(null, g1) => assert((null, g1) == (null, "abcd")) }
val r21 = Regex3("([\\w:]+::)?(\\w+)$"); "xy:z:::abcd" match { case r21(g0, g1) => assert((g0, g1) == ("xy:z:::", "abcd")) }
val r22 = Regex3("([a-c]*)\\1"); "abcabc" match { case r22(g0) => assert((g0) == ("abc")) }
val r23 = Regex3("([abc])*bcd"); "abcd" match { case r23(g0) => assert((g0) == ("a")) }
val r24 = Regex3("([abc])*d"); "abbbcd" match { case r24(g0) => assert((g0) == ("c")) }
val r25 = Regex3("([yX].|WORDS|[yX].|WORD)+S"); "WORDS" match { case r25(g0) => assert((g0) == ("WORD")) }
val r26 = Regex3("([yX].|WORDS|[yX].|WORD)S"); "WORDS" match { case r26(g0) => assert((g0) == ("WORD")) }
val r27 = Regex3("([yX].|WORDS|WORD|[xY].)+S"); "WORDS" match { case r27(g0) => assert((g0) == ("WORD")) }
val r28 = Regex3("([yX].|WORDS|WORD|[xY].)S"); "WORDS" match { case r28(g0) => assert((g0) == ("WORD")) }
val r29 = Regex3("([zx].|foo|fool|[zq].|money|parted|[yx].)$"); "fool" match { case r29(g0) => assert((g0) == ("fool")) }
val r30 = Regex3("([zx].|foo|fool|[zq].|money|parted|[yx].)+$"); "fool" match { case r30(g0) => assert((g0) == ("fool")) }
val r31 = Regex3("(\\d+\\.\\d+)"); "3.1415926" match { case r31(g0) => assert((g0) == ("3.1415926")) }
val r32 = Regex3("(\\w+:)+"); "one:" match { case r32(g0) => assert((g0) == ("one:")) }
val r33 = Regex3("(^|a)b"); "ab" match { case r33(g0) => assert((g0) == ("a")) }
val r34 = Regex3("(a)?(a)+"); "a" match { case r34(null, g1) => assert((null, g1) == (null, "a")) }
val r35 = Regex3("(a)b(c)"); "abc" match { case r35(g0, g1) => assert((g0, g1) == ("a", "c")) }
val r36 = Regex3("(a)|(b)"); "b" match { case r36(null, g1) => assert((null, g1) == (null, "b")) }
val r37 = Regex3("(a)|\\1"); "a" match { case r37(g0) => assert((g0) == ("a")) }
val r38 = Regex3("(a+|b)*"); "ab" match { case r38(g0) => assert((g0) == ("b")) }
val r39 = Regex3("(a+|b)+"); "ab" match { case r39(g0) => assert((g0) == ("b")) }
val r40 = Regex3("(a+|b){0,}"); "ab" match { case r40(g0) => assert((g0) == ("b")) }
val r41 = Regex3("(a+|b){1,}"); "ab" match { case r41(g0) => assert((g0) == ("b")) }
val r42 = Regex3("(aA)*+b"); "aAaAaAaAaAb" match { case r42(g0) => assert((g0) == ("aA")) }
val r43 = Regex3("(aA)++b"); "aAaAaAaAaAb" match { case r43(g0) => assert((g0) == ("aA")) }
val r44 = Regex3("(aA)?+b"); "aAb" match { case r44(g0) => assert((g0) == ("aA")) }
val r45 = Regex3("(aA){1,5}+b"); "aAaAaAaAaAb" match { case r45(g0) => assert((g0) == ("aA")) }
val r46 = Regex3("(aA|bB)*+b"); "bBbBbBbBbBb" match { case r46(g0) => assert((g0) == ("bB")) }
val r47 = Regex3("(aA|bB)++b"); "aAbBaAaAbBb" match { case r47(g0) => assert((g0) == ("bB")) }
val r48 = Regex3("(aA|bB)?+b"); "bBb" match { case r48(g0) => assert((g0) == ("bB")) }
val r49 = Regex3("(aA|bB){1,5}+b"); "bBaAbBaAbBb" match { case r49(g0) => assert((g0) == ("bB")) }
val r50 = Regex3("(ab)?(ab)+"); "ab" match { case r50(null, g1) => assert((null, g1) == (null, "ab")) }
val r51 = Regex3("(abc)?(abc)+"); "abc" match { case r51(null, g1) => assert((null, g1) == (null, "abc")) }
val r52 = Regex3("(abc)\\1"); "abcabc" match { case r52(g0) => assert((g0) == ("abc")) }
val r53 = Regex3("(ab|a)b*c"); "abc" match { case r53(g0) => assert((g0) == ("ab")) }
val r54 = Regex3("(ab|ab*)bc"); "abc" match { case r54(g0) => assert((g0) == ("a")) }
val r55 = Regex3("(a|(bc)){0,0}+xyz"); "xyz" match { case r55(null, null) => assert((null, null) == (null, null)) }
val r56 = Regex3("(a|(bc)){0,0}?xyz"); "xyz" match { case r56(null, null) => assert((null, null) == (null, null)) }
val r57 = Regex3("(a|b|c|d|e)f"); "ef" match { case r57(g0) => assert((g0) == ("e")) }
val r58 = Regex3("(bc+d$|ef*g.|h?i(j|k))"); "effgz" match { case r58(g0, null) => assert((g0, null) == ("effgz", null)) }
val r59 = Regex3("(bc+d$|ef*g.|h?i(j|k))"); "ij" match { case r59(g0, g1) => assert((g0, g1) == ("ij", "j")) }
val r60 = Regex3("(foo[1x]|bar[2x]|baz[3x])*y"); "foo1bar2baz3y" match { case r60(g0) => assert((g0) == ("baz3")) }
val r61 = Regex3("(foo[1x]|bar[2x]|baz[3x])+y"); "foo1bar2baz3y" match { case r61(g0) => assert((g0) == ("baz3")) }
val r62 = Regex3("(foo|fool|[zx].|money|parted)$"); "fool" match { case r62(g0) => assert((g0) == ("fool")) }
val r63 = Regex3("(foo|fool|[zx].|money|parted)+$"); "fool" match { case r63(g0) => assert((g0) == ("fool")) }
val r64 = Regex3("(foo|fool|money|parted)$"); "fool" match { case r64(g0) => assert((g0) == ("fool")) }
val r65 = Regex3("(foo|fool|x.|money|parted)$"); "fool" match { case r65(g0) => assert((g0) == ("fool")) }
val r66 = Regex3("(q1|.)*(q2|.)*(x(a|bc)*y){2,3}"); "xayxay" match { case r66(null, null, g2, g3) => assert((null, null, g2, g3) == (null, null, "xay", "a")) }
val r67 = Regex3("(q1|.)*(q2|.)*(x(a|bc)*y){2,}"); "xayxay" match { case r67(null, null, g2, g3) => assert((null, null, g2, g3) == (null, null, "xay", "a")) }
val r68 = Regex3("(q1|z)*(q2|z)*z{15}-.*?(x(a|bc)*y){2,3}Z"); "zzzzzzzzzzzzzzzz-xayxayxayxayZ" match { case r68(g0, null, g2, g3) => assert((g0, null, g2, g3) == ("z", null, "xay", "a")) }
val r69 = Regex3("(WORDS|WORD)S"); "WORDS" match { case r69(g0) => assert((g0) == ("WORD")) }
val r70 = Regex3("(WORDS|WORLD|WORD)+S"); "WORDS" match { case r70(g0) => assert((g0) == ("WORD")) }
val r71 = Regex3("(WORDS|WORLD|WORD)S"); "WORDS" match { case r71(g0) => assert((g0) == ("WORD")) }
val r72 = Regex3("(x.|foo|fool|x.|money|parted|y.)$"); "fool" match { case r72(g0) => assert((g0) == ("fool")) }
val r73 = Regex3("(X.|WORDS|WORD|Y.)S"); "WORDS" match { case r73(g0) => assert((g0) == ("WORD")) }
val r74 = Regex3("(X.|WORDS|X.|WORD)S"); "WORDS" match { case r74(g0) => assert((g0) == ("WORD")) }
val r75 = Regex3("(x|y|z[QW])*(longish|loquatious|excessive|overblown[QW])*"); "xyzQzWlongishoverblownW" match { case r75(g0, g1) => assert((g0, g1) == ("zW", "overblownW")) }
val r76 = Regex3("(x|y|z[QW])*+(longish|loquatious|excessive|overblown[QW])*+"); "xyzQzWlongishoverblownW" match { case r76(g0, g1) => assert((g0, g1) == ("zW", "overblownW")) }
val r77 = Regex3("(x|y|z[QW])+(longish|loquatious|excessive|overblown[QW])+"); "xyzQzWlongishoverblownW" match { case r77(g0, g1) => assert((g0, g1) == ("zW", "overblownW")) }
val r78 = Regex3("(x|y|z[QW])++(longish|loquatious|excessive|overblown[QW])++"); "xyzQzWlongishoverblownW" match { case r78(g0, g1) => assert((g0, g1) == ("zW", "overblownW")) }
val r79 = Regex3("(x|y|z[QW]){1,5}(longish|loquatious|excessive|overblown[QW]){1,5}"); "xyzQzWlongishoverblownW" match { case r79(g0, g1) => assert((g0, g1) == ("zW", "overblownW")) }
val r80 = Regex3("(x|y|z[QW]){1,5}+(longish|loquatious|excessive|overblown[QW]){1,5}+"); "xyzQzWlongishoverblownW" match { case r80(g0, g1) => assert((g0, g1) == ("zW", "overblownW")) }
val r81 = Regex3(".*?(?:(\\w)|(\\w))x"); "abx" match { case r81(g0, null) => assert((g0, null) == ("b", null)) }
val r82 = Regex3("2(]*)?$\\1"); "2" match { case r82(g0) => assert((g0) == ("")) }
val r83 = Regex3("\\((.*), (.*)\\)"); "(a, b)" match { case r83(g0, g1) => assert((g0, g1) == ("a", "b")) }
val r84 = Regex3("^((?:aa)*)(?:X+((?:\\d+|-)(?:X+(.+))?))?$"); "aaaaX5" match { case r84(g0, g1, null) => assert((g0, g1, null) == ("aaaa", "5", null)) }
val r85 = Regex3("^((a|b)+)*ax"); "aax" match { case r85(g0, g1) => assert((g0, g1) == ("a", "a")) }
val r86 = Regex3("^((a|bc)+)*ax"); "aax" match { case r86(g0, g1) => assert((g0, g1) == ("a", "a")) }
val r87 = Regex3("^(.*?)\\s*\\|\\s*(?:\\/\\s*|)\'(.+)\'$"); "text|\'sec\'" match { case r87(g0, g1) => assert((g0, g1) == ("text", "sec")) }
val r88 = Regex3("^(.+)?B"); "AB" match { case r88(g0) => assert((g0) == ("A")) }
val r89 = Regex3("^(.,){2}c"); "a,b,c" match { case r89(g0) => assert((g0) == ("b,")) }
val r90 = Regex3("^(0+)?(?:x(1))?"); "x1" match { case r90(null, g1) => assert((null, g1) == (null, "1")) }
val r91 = Regex3("^(?:(\\d)x)?\\d$"); "1" match { case r91(null) => assert((null) == (null)) }
val r92 = Regex3("^(?:(X)?(\\d)|(X)?(\\d\\d))$"); "X12" match { case r92(null, null, g2, g3) => assert((null, null, g2, g3) == (null, null, "X", "12")) }
val r93 = Regex3("^(?:(XX)?(\\d)|(XX)?(\\d\\d))$"); "XX12" match { case r93(null, null, g2, g3) => assert((null, null, g2, g3) == (null, null, "XX", "12")) }
val r94 = Regex3("^(?:f|o|b){2,3}?((?:b|a|r)+)\\1$"); "foobarbar" match { case r94(g0) => assert((g0) == ("bar")) }
val r95 = Regex3("^(?:f|o|b){2,3}?((?:b|a|r)+?)\\1$"); "foobarbar" match { case r95(g0) => assert((g0) == ("bar")) }
val r96 = Regex3("^(?:f|o|b){2,3}?(.+)\\1$"); "foobarbar" match { case r96(g0) => assert((g0) == ("bar")) }
val r97 = Regex3("^(?:f|o|b){2,3}?(.+?)\\1$"); "foobarbar" match { case r97(g0) => assert((g0) == ("bar")) }
val r98 = Regex3("^(?:f|o|b){3,4}((?:b|a|r)+)\\1$"); "foobarbar" match { case r98(g0) => assert((g0) == ("bar")) }
val r99 = Regex3("^(?:f|o|b){3,4}((?:b|a|r)+?)\\1$"); "foobarbar" match { case r99(g0) => assert((g0) == ("bar")) }
val r100 = Regex3("^(?:f|o|b){3,4}(.+)\\1$"); "foobarbar" match { case r100(g0) => assert((g0) == ("bar")) }
val r101 = Regex3("^(?:f|o|b){3,4}(.+?)\\1$"); "foobarbar" match { case r101(g0) => assert((g0) == ("bar")) }
val r102 = Regex3("^([0-9a-fA-F]+)(?:x([0-9a-fA-F]+)?)(?:x([0-9a-fA-F]+))?"); "012cxx0190" match { case r102(g0, null, g2) => assert((g0, null, g2) == ("012c", null, "0190")) }
val r103 = Regex3("^([^,]*,){0,3}d"); "aaa,b,c,d" match { case r103(g0) => assert((g0) == ("c,")) }
val r104 = Regex3("^([^,]*,){2}c"); "a,b,c" match { case r104(g0) => assert((g0) == ("b,")) }
val r105 = Regex3("^([^,]*,){3,}d"); "aaa,b,c,d" match { case r105(g0) => assert((g0) == ("c,")) }
val r106 = Regex3("^([^,]*,){3}d"); "aaa,b,c,d" match { case r106(g0) => assert((g0) == ("c,")) }
val r107 = Regex3("^([^,]{0,3},){0,3}d"); "aaa,b,c,d" match { case r107(g0) => assert((g0) == ("c,")) }
val r108 = Regex3("^([^,]{0,3},){3,}d"); "aaa,b,c,d" match { case r108(g0) => assert((g0) == ("c,")) }
val r109 = Regex3("^([^,]{0,3},){3}d"); "aaa,b,c,d" match { case r109(g0) => assert((g0) == ("c,")) }
val r110 = Regex3("^([^,]{1,3},){0,3}d"); "aaa,b,c,d" match { case r110(g0) => assert((g0) == ("c,")) }
val r111 = Regex3("^([^,]{1,3},){3,}d"); "aaa,b,c,d" match { case r111(g0) => assert((g0) == ("c,")) }
val r112 = Regex3("^([^,]{1,3},){3}d"); "aaa,b,c,d" match { case r112(g0) => assert((g0) == ("c,")) }
val r113 = Regex3("^([^,]{1,},){0,3}d"); "aaa,b,c,d" match { case r113(g0) => assert((g0) == ("c,")) }
val r114 = Regex3("^([^,]{1,},){3,}d"); "aaa,b,c,d" match { case r114(g0) => assert((g0) == ("c,")) }
val r115 = Regex3("^([^,]{1,},){3}d"); "aaa,b,c,d" match { case r115(g0) => assert((g0) == ("c,")) }
val r116 = Regex3("^([^a-z])|(\\^)$"); "." match { case r116(g0, null) => assert((g0, null) == (".", null)) }
val r117 = Regex3("^([a]{1})*$"); "aa" match { case r117(g0) => assert((g0) == ("a")) }
val r118 = Regex3("^([ab]*?)(b)?(c)$"); "abac" match { case r118(g0, null, g2) => assert((g0, null, g2) == ("aba", null, "c")) }
val r119 = Regex3("^([TUV]+|XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P):"); "ZEQQQQQQQQQQQQQQQQQQP:" match { case r119(g0) => assert((g0) == ("ZEQQQQQQQQQQQQQQQQQQP")) }
val r120 = Regex3("^([TUV]+|XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P):"); "ZEQQQX:" match { case r120(g0) => assert((g0) == ("ZEQQQX")) }
val r121 = Regex3("^([TUV]+|XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P|[MKJ]):"); "ZEQQQQQQQQQQQQQQQQQQP:" match { case r121(g0) => assert((g0) == ("ZEQQQQQQQQQQQQQQQQQQP")) }
val r122 = Regex3("^([TUV]+|XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P|[MKJ]):"); "ZEQQQX:" match { case r122(g0) => assert((g0) == ("ZEQQQX")) }
val r123 = Regex3("^([TUV]+|XXX|YYY|Z.Q*X|Z[TE]Q*P):"); "ZEQQQQQQQQQQQQQQQQQQP:" match { case r123(g0) => assert((g0) == ("ZEQQQQQQQQQQQQQQQQQQP")) }
val r124 = Regex3("^([TUV]+|XXX|YYY|Z.Q*X|Z[TE]Q*P):"); "ZEQQQX:" match { case r124(g0) => assert((g0) == ("ZEQQQX")) }
val r125 = Regex3("^([TUV]+|XXX|YYY|Z.Q*X|Z[TE]Q*P|[MKJ]):"); "ZEQQQQQQQQQQQQQQQQQQP:" match { case r125(g0) => assert((g0) == ("ZEQQQQQQQQQQQQQQQQQQP")) }
val r126 = Regex3("^([TUV]+|XXX|YYY|Z.Q*X|Z[TE]Q*P|[MKJ]):"); "ZEQQQX:" match { case r126(g0) => assert((g0) == ("ZEQQQX")) }
val r127 = Regex3("^(a(b)?)+$"); "aba" match { case r127(g0, g1) => assert((g0, g1) == ("a", "b")) }
val r128 = Regex3("^(a)?a$"); "a" match { case r128(null) => assert((null) == (null)) }
val r129 = Regex3("^(a+)*ax"); "aax" match { case r129(g0) => assert((g0) == ("a")) }
val r130 = Regex3("^(a\\1?)(a\\1?)(a\\2?)(a\\3?)$"); "aaaaaa" match { case r130(g0, g1, g2, g3) => assert((g0, g1, g2, g3) == ("a", "aa", "a", "aa")) }
val r131 = Regex3("^(a\\1?){4}$"); "aaaaaa" match { case r131(g0) => assert((g0) == ("aa")) }
val r132 = Regex3("^(a\\1?){4}$"); "aaaaaaaaaa" match { case r132(g0) => assert((g0) == ("aaaa")) }
val r133 = Regex3("^(aa(bb)?)+$"); "aabbaa" match { case r133(g0, g1) => assert((g0, g1) == ("aa", "bb")) }
val r134 = Regex3("^(b+?|a){1,2}c"); "bbbac" match { case r134(g0) => assert((g0) == ("a")) }
val r135 = Regex3("^(b+?|a){1,2}c"); "bbbbac" match { case r135(g0) => assert((g0) == ("a")) }
val r136 = Regex3("^(foo|)bar$"); "bar" match { case r136(g0) => assert((g0) == ("")) }
val r137 = Regex3("^(foo||baz)bar$"); "bar" match { case r137(g0) => assert((g0) == ("")) }
val r138 = Regex3("^(foo||baz)bar$"); "bazbar" match { case r138(g0) => assert((g0) == ("baz")) }
val r139 = Regex3("^(foo||baz)bar$"); "foobar" match { case r139(g0) => assert((g0) == ("foo")) }
val r140 = Regex3("^(XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P):"); "ZEQQQQQQQQQQQQQQQQQQP:" match { case r140(g0) => assert((g0) == ("ZEQQQQQQQQQQQQQQQQQQP")) }
val r141 = Regex3("^(XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P):"); "ZEQQQX:" match { case r141(g0) => assert((g0) == ("ZEQQQX")) }
val r142 = Regex3("^(XXX|YYY|Z.Q*X|Z[TE]Q*P):"); "ZEQQQQQQQQQQQQQQQQQQP:" match { case r142(g0) => assert((g0) == ("ZEQQQQQQQQQQQQQQQQQQP")) }
val r143 = Regex3("^(XXX|YYY|Z.Q*X|Z[TE]Q*P):"); "ZEQQQX:" match { case r143(g0) => assert((g0) == ("ZEQQQX")) }
val r144 = Regex3("^.{2,3}?((?:b|a|r)+)\\1$"); "foobarbar" match { case r144(g0) => assert((g0) == ("bar")) }
val r145 = Regex3("^.{2,3}?((?:b|a|r)+?)\\1$"); "foobarbar" match { case r145(g0) => assert((g0) == ("bar")) }
val r146 = Regex3("^.{2,3}?(.+)\\1$"); "foobarbar" match { case r146(g0) => assert((g0) == ("bar")) }
val r147 = Regex3("^.{2,3}?(.+?)\\1$"); "foobarbar" match { case r147(g0) => assert((g0) == ("bar")) }
val r148 = Regex3("^.{3,4}((?:b|a|r)+)\\1$"); "foobarbar" match { case r148(g0) => assert((g0) == ("bar")) }
val r149 = Regex3("^.{3,4}((?:b|a|r)+?)\\1$"); "foobarbar" match { case r149(g0) => assert((g0) == ("bar")) }
val r150 = Regex3("^.{3,4}(.+)\\1$"); "foobarbar" match { case r150(g0) => assert((g0) == ("bar")) }
val r151 = Regex3("^.{3,4}(.+?)\\1$"); "foobarbar" match { case r151(g0) => assert((g0) == ("bar")) }
val r152 = Regex3("^m?(\\d)(.*)\\1$"); "5b5" match { case r152(g0, g1) => assert((g0, g1) == ("5", "b")) }
val r153 = Regex3("^m?(\\D)(.*)\\1$"); "aba" match { case r153(g0, g1) => assert((g0, g1) == ("a", "b")) }
val r154 = Regex3("^m?(\\S)(.*)\\1$"); "aba" match { case r154(g0, g1) => assert((g0, g1) == ("a", "b")) }
val r155 = Regex3("^m?(\\W)(.*)\\1$"); ":b:" match { case r155(g0, g1) => assert((g0, g1) == (":", "b")) }
val r156 = Regex3("^m?(\\w)(.*)\\1$"); "aba" match { case r156(g0, g1) => assert((g0, g1) == ("a", "b")) }
val r157 = Regex3("a(?:b|(c|e){1,2}?|d)+?(.)"); "ace" match { case r157(g0, g1) => assert((g0, g1) == ("c", "e")) }
val r158 = Regex3("a(?:b|c|d)(.)"); "ace" match { case r158(g0) => assert((g0) == ("e")) }
val r159 = Regex3("a(?:b|c|d)*(.)"); "ace" match { case r159(g0) => assert((g0) == ("e")) }
val r160 = Regex3("a(?:b|c|d)+(.)"); "acdbcdbe" match { case r160(g0) => assert((g0) == ("e")) }
val r161 = Regex3("a(?:b|c|d)+?(.)"); "acdbcdbe" match { case r161(g0) => assert((g0) == ("e")) }
val r162 = Regex3("a(?:b|c|d)+?(.)"); "ace" match { case r162(g0) => assert((g0) == ("e")) }
val r163 = Regex3("a(?:b|c|d){5,6}(.)"); "acdbcdbe" match { case r163(g0) => assert((g0) == ("e")) }
val r164 = Regex3("a(?:b|c|d){5,6}?(.)"); "acdbcdbe" match { case r164(g0) => assert((g0) == ("e")) }
val r165 = Regex3("a(?:b|c|d){5,7}(.)"); "acdbcdbe" match { case r165(g0) => assert((g0) == ("e")) }
val r166 = Regex3("a(?:b|c|d){5,7}?(.)"); "acdbcdbe" match { case r166(g0) => assert((g0) == ("e")) }
val r167 = Regex3("a(?:b|c|d){6,7}(.)"); "acdbcdbe" match { case r167(g0) => assert((g0) == ("e")) }
val r168 = Regex3("a(?:b|c|d){6,7}?(.)"); "acdbcdbe" match { case r168(g0) => assert((g0) == ("e")) }
val r169 = Regex3("a([bc]*)(c*d)"); "abcd" match { case r169(g0, g1) => assert((g0, g1) == ("bc", "d")) }
val r170 = Regex3("a([bc]*)(c+d)"); "abcd" match { case r170(g0, g1) => assert((g0, g1) == ("b", "cd")) }
val r171 = Regex3("a([bc]*)c*"); "abc" match { case r171(g0) => assert((g0) == ("bc")) }
val r172 = Regex3("a([bc]+)(c*d)"); "abcd" match { case r172(g0, g1) => assert((g0, g1) == ("bc", "d")) }
val r173 = Regex3("a(bc)d"); "abcd" match { case r173(g0) => assert((g0) == ("bc")) }
val r174 = Regex3("foo(aA)*+b"); "fooaAaAaAaAaAb" match { case r174(g0) => assert((g0) == ("aA")) }
val r175 = Regex3("foo(aA)++b"); "fooaAaAaAaAaAb" match { case r175(g0) => assert((g0) == ("aA")) }
val r176 = Regex3("foo(aA)?+b"); "fooaAb" match { case r176(g0) => assert((g0) == ("aA")) }
val r177 = Regex3("foo(aA){1,5}+b"); "fooaAaAaAaAaAb" match { case r177(g0) => assert((g0) == ("aA")) }
val r178 = Regex3("foo(aA|bB)*+b"); "foobBbBaAaAaAb" match { case r178(g0) => assert((g0) == ("aA")) }
val r179 = Regex3("foo(aA|bB)++b"); "foobBaAbBaAbBb" match { case r179(g0) => assert((g0) == ("bB")) }
val r180 = Regex3("foo(aA|bB)?+b"); "foobBb" match { case r180(g0) => assert((g0) == ("bB")) }
val r181 = Regex3("foo(aA|bB){1,5}+b"); "foobBaAaAaAaAb" match { case r181(g0) => assert((g0) == ("aA")) }
val r182 = Regex3("X(\\w+)(?=\\s)|X(\\w+)"); "Xab" match { case r182(null, g1) => assert((null, g1) == (null, "ab")) }
val r183 = Regex3("x(~~)*(?:(?:F)?)?"); "x~~" match { case r183(g0) => assert((g0) == ("~~")) }

}}
