package bench

import RegexPackage._
import annotation.experimental
import org.openjdk.jmh.annotations._

@experimental
object RegexBench2 {

// @Benchmark
def main(args: Array[String]): Unit = {

val r1 = Regex2("((((((((((a))))))))))"); "a" match { case r1(g0, g1, g2, g3, g4, g5, g6, g7, g8, g9) => assert((g0, g1, g2, g3, g4, g5, g6, g7, g8, g9) == ("a", "a", "a", "a", "a", "a", "a", "a", "a", "a")) }
val r2 = Regex2("((((((((((a))))))))))\\10"); "aa" match { case r2(g0, g1, g2, g3, g4, g5, g6, g7, g8, g9) => assert((g0, g1, g2, g3, g4, g5, g6, g7, g8, g9) == ("a", "a", "a", "a", "a", "a", "a", "a", "a", "a")) }
val r3 = Regex2("(((((((((a)))))))))"); "a" match { case r3(g0, g1, g2, g3, g4, g5, g6, g7, g8) => assert((g0, g1, g2, g3, g4, g5, g6, g7, g8) == ("a", "a", "a", "a", "a", "a", "a", "a", "a")) }
val r4 = Regex2("((?:aaaa|bbbb)cccc)?"); "aaaacccc" match { case r4(Some(g0)) => assert((g0) == ("aaaacccc")) }
val r5 = Regex2("((?:aaaa|bbbb)cccc)?"); "bbbbcccc" match { case r5(Some(g0)) => assert((g0) == ("bbbbcccc")) }
val r6 = Regex2("((?i)a)b"); "ab" match { case r6(g0) => assert((g0) == ("a")) }
val r7 = Regex2("((?i)a)b"); "Ab" match { case r7(g0) => assert((g0) == ("A")) }
val r8 = Regex2("((?i:a))b"); "ab" match { case r8(g0) => assert((g0) == ("a")) }
val r9 = Regex2("((?i:a))b"); "Ab" match { case r9(g0) => assert((g0) == ("A")) }
val r10 = Regex2("(([a-c])b*?\\2)*"); "ababbbcbc" match { case r10(Some(g0), Some(g1)) => assert((g0, g1) == ("cbc", "c")) }
val r11 = Regex2("(([a-c])b*?\\2){3}"); "ababbbcbc" match { case r11(g0, g1) => assert((g0, g1) == ("cbc", "c")) }
val r12 = Regex2("((a)(b)c)(d)"); "abcd" match { case r12(g0, g1, g2, g3) => assert((g0, g1, g2, g3) == ("abc", "a", "b", "d")) }
val r13 = Regex2("((foo)|(bar))*"); "foobar" match { case r13(Some(g0), Some(g1), Some(g2)) => assert((g0, g1, g2) == ("bar", "foo", "bar")) }
val r14 = Regex2("(.*)c(.*)"); "abcde" match { case r14(g0, g1) => assert((g0, g1) == ("ab", "de")) }
val r15 = Regex2("(?:(f)(o)(o)|(b)(a)(r))*"); "foobar" match { case r15(Some(g0), Some(g1), Some(g2), Some(g3), Some(g4), Some(g5)) => assert((g0, g1, g2, g3, g4, g5) == ("f", "o", "o", "b", "a", "r")) }
val r16 = Regex2("([[:digit:]-[:alpha:]]+)"); "-" match { case r16(g0) => assert((g0) == ("-")) }
val r17 = Regex2("([[:digit:]-z]+)"); "-" match { case r17(g0) => assert((g0) == ("-")) }
val r18 = Regex2("([\\d-\\s]+)"); "-" match { case r18(g0) => assert((g0) == ("-")) }
val r19 = Regex2("([\\d-z]+)"); "-" match { case r19(g0) => assert((g0) == ("-")) }
val r20 = Regex2("([\\w:]+::)?(\\w+)$"); "abcd" match { case r20(None, g1) => assert((null, g1) == (null, "abcd")) }
val r21 = Regex2("([\\w:]+::)?(\\w+)$"); "xy:z:::abcd" match { case r21(Some(g0), g1) => assert((g0, g1) == ("xy:z:::", "abcd")) }
val r22 = Regex2("([a-c]*)\\1"); "abcabc" match { case r22(g0) => assert((g0) == ("abc")) }
val r23 = Regex2("([abc])*bcd"); "abcd" match { case r23(Some(g0)) => assert((g0) == ("a")) }
val r24 = Regex2("([abc])*d"); "abbbcd" match { case r24(Some(g0)) => assert((g0) == ("c")) }
val r25 = Regex2("([yX].|WORDS|[yX].|WORD)+S"); "WORDS" match { case r25(g0) => assert((g0) == ("WORD")) }
val r26 = Regex2("([yX].|WORDS|[yX].|WORD)S"); "WORDS" match { case r26(g0) => assert((g0) == ("WORD")) }
val r27 = Regex2("([yX].|WORDS|WORD|[xY].)+S"); "WORDS" match { case r27(g0) => assert((g0) == ("WORD")) }
val r28 = Regex2("([yX].|WORDS|WORD|[xY].)S"); "WORDS" match { case r28(g0) => assert((g0) == ("WORD")) }
val r29 = Regex2("([zx].|foo|fool|[zq].|money|parted|[yx].)$"); "fool" match { case r29(g0) => assert((g0) == ("fool")) }
val r30 = Regex2("([zx].|foo|fool|[zq].|money|parted|[yx].)+$"); "fool" match { case r30(g0) => assert((g0) == ("fool")) }
val r31 = Regex2("(\\d+\\.\\d+)"); "3.1415926" match { case r31(g0) => assert((g0) == ("3.1415926")) }
val r32 = Regex2("(\\w+:)+"); "one:" match { case r32(g0) => assert((g0) == ("one:")) }
val r33 = Regex2("(^|a)b"); "ab" match { case r33(g0) => assert((g0) == ("a")) }
val r34 = Regex2("(a)?(a)+"); "a" match { case r34(None, g1) => assert((null, g1) == (null, "a")) }
val r35 = Regex2("(a)b(c)"); "abc" match { case r35(g0, g1) => assert((g0, g1) == ("a", "c")) }
val r36 = Regex2("(a)|(b)"); "b" match { case r36(None, Some(g1)) => assert((null, g1) == (null, "b")) }
val r37 = Regex2("(a)|\\1"); "a" match { case r37(Some(g0)) => assert((g0) == ("a")) }
val r38 = Regex2("(a+|b)*"); "ab" match { case r38(Some(g0)) => assert((g0) == ("b")) }
val r39 = Regex2("(a+|b)+"); "ab" match { case r39(g0) => assert((g0) == ("b")) }
val r40 = Regex2("(a+|b){0,}"); "ab" match { case r40(Some(g0)) => assert((g0) == ("b")) }
val r41 = Regex2("(a+|b){1,}"); "ab" match { case r41(g0) => assert((g0) == ("b")) }
val r42 = Regex2("(aA)*+b"); "aAaAaAaAaAb" match { case r42(Some(g0)) => assert((g0) == ("aA")) }
val r43 = Regex2("(aA)++b"); "aAaAaAaAaAb" match { case r43(g0) => assert((g0) == ("aA")) }
val r44 = Regex2("(aA)?+b"); "aAb" match { case r44(Some(g0)) => assert((g0) == ("aA")) }
val r45 = Regex2("(aA){1,5}+b"); "aAaAaAaAaAb" match { case r45(g0) => assert((g0) == ("aA")) }
val r46 = Regex2("(aA|bB)*+b"); "bBbBbBbBbBb" match { case r46(Some(g0)) => assert((g0) == ("bB")) }
val r47 = Regex2("(aA|bB)++b"); "aAbBaAaAbBb" match { case r47(g0) => assert((g0) == ("bB")) }
val r48 = Regex2("(aA|bB)?+b"); "bBb" match { case r48(Some(g0)) => assert((g0) == ("bB")) }
val r49 = Regex2("(aA|bB){1,5}+b"); "bBaAbBaAbBb" match { case r49(g0) => assert((g0) == ("bB")) }
val r50 = Regex2("(ab)?(ab)+"); "ab" match { case r50(None, g1) => assert((null, g1) == (null, "ab")) }
val r51 = Regex2("(abc)?(abc)+"); "abc" match { case r51(None, g1) => assert((null, g1) == (null, "abc")) }
val r52 = Regex2("(abc)\\1"); "abcabc" match { case r52(g0) => assert((g0) == ("abc")) }
val r53 = Regex2("(ab|a)b*c"); "abc" match { case r53(g0) => assert((g0) == ("ab")) }
val r54 = Regex2("(ab|ab*)bc"); "abc" match { case r54(g0) => assert((g0) == ("a")) }
val r55 = Regex2("(a|(bc)){0,0}+xyz"); "xyz" match { case r55(None, None) => assert((null, null) == (null, null)) }
val r56 = Regex2("(a|(bc)){0,0}?xyz"); "xyz" match { case r56(None, None) => assert((null, null) == (null, null)) }
val r57 = Regex2("(a|b|c|d|e)f"); "ef" match { case r57(g0) => assert((g0) == ("e")) }
val r58 = Regex2("(bc+d$|ef*g.|h?i(j|k))"); "effgz" match { case r58(g0, None) => assert((g0, null) == ("effgz", null)) }
val r59 = Regex2("(bc+d$|ef*g.|h?i(j|k))"); "ij" match { case r59(g0, Some(g1)) => assert((g0, g1) == ("ij", "j")) }
val r60 = Regex2("(foo[1x]|bar[2x]|baz[3x])*y"); "foo1bar2baz3y" match { case r60(Some(g0)) => assert((g0) == ("baz3")) }
val r61 = Regex2("(foo[1x]|bar[2x]|baz[3x])+y"); "foo1bar2baz3y" match { case r61(g0) => assert((g0) == ("baz3")) }
val r62 = Regex2("(foo|fool|[zx].|money|parted)$"); "fool" match { case r62(g0) => assert((g0) == ("fool")) }
val r63 = Regex2("(foo|fool|[zx].|money|parted)+$"); "fool" match { case r63(g0) => assert((g0) == ("fool")) }
val r64 = Regex2("(foo|fool|money|parted)$"); "fool" match { case r64(g0) => assert((g0) == ("fool")) }
val r65 = Regex2("(foo|fool|x.|money|parted)$"); "fool" match { case r65(g0) => assert((g0) == ("fool")) }
val r66 = Regex2("(q1|.)*(q2|.)*(x(a|bc)*y){2,3}"); "xayxay" match { case r66(None, None, g2, Some(g3)) => assert((null, null, g2, g3) == (null, null, "xay", "a")) }
val r67 = Regex2("(q1|.)*(q2|.)*(x(a|bc)*y){2,}"); "xayxay" match { case r67(None, None, g2, Some(g3)) => assert((null, null, g2, g3) == (null, null, "xay", "a")) }
val r68 = Regex2("(q1|z)*(q2|z)*z{15}-.*?(x(a|bc)*y){2,3}Z"); "zzzzzzzzzzzzzzzz-xayxayxayxayZ" match { case r68(Some(g0), None, g2, Some(g3)) => assert((g0, null, g2, g3) == ("z", null, "xay", "a")) }
val r69 = Regex2("(WORDS|WORD)S"); "WORDS" match { case r69(g0) => assert((g0) == ("WORD")) }
val r70 = Regex2("(WORDS|WORLD|WORD)+S"); "WORDS" match { case r70(g0) => assert((g0) == ("WORD")) }
val r71 = Regex2("(WORDS|WORLD|WORD)S"); "WORDS" match { case r71(g0) => assert((g0) == ("WORD")) }
val r72 = Regex2("(x.|foo|fool|x.|money|parted|y.)$"); "fool" match { case r72(g0) => assert((g0) == ("fool")) }
val r73 = Regex2("(X.|WORDS|WORD|Y.)S"); "WORDS" match { case r73(g0) => assert((g0) == ("WORD")) }
val r74 = Regex2("(X.|WORDS|X.|WORD)S"); "WORDS" match { case r74(g0) => assert((g0) == ("WORD")) }
val r75 = Regex2("(x|y|z[QW])*(longish|loquatious|excessive|overblown[QW])*"); "xyzQzWlongishoverblownW" match { case r75(Some(g0), Some(g1)) => assert((g0, g1) == ("zW", "overblownW")) }
val r76 = Regex2("(x|y|z[QW])*+(longish|loquatious|excessive|overblown[QW])*+"); "xyzQzWlongishoverblownW" match { case r76(Some(g0), Some(g1)) => assert((g0, g1) == ("zW", "overblownW")) }
val r77 = Regex2("(x|y|z[QW])+(longish|loquatious|excessive|overblown[QW])+"); "xyzQzWlongishoverblownW" match { case r77(g0, g1) => assert((g0, g1) == ("zW", "overblownW")) }
val r78 = Regex2("(x|y|z[QW])++(longish|loquatious|excessive|overblown[QW])++"); "xyzQzWlongishoverblownW" match { case r78(g0, g1) => assert((g0, g1) == ("zW", "overblownW")) }
val r79 = Regex2("(x|y|z[QW]){1,5}(longish|loquatious|excessive|overblown[QW]){1,5}"); "xyzQzWlongishoverblownW" match { case r79(g0, g1) => assert((g0, g1) == ("zW", "overblownW")) }
val r80 = Regex2("(x|y|z[QW]){1,5}+(longish|loquatious|excessive|overblown[QW]){1,5}+"); "xyzQzWlongishoverblownW" match { case r80(g0, g1) => assert((g0, g1) == ("zW", "overblownW")) }
val r81 = Regex2(".*?(?:(\\w)|(\\w))x"); "abx" match { case r81(Some(g0), None) => assert((g0, null) == ("b", null)) }
val r82 = Regex2("2(]*)?$\\1"); "2" match { case r82(Some(g0)) => assert((g0) == ("")) }
val r83 = Regex2("\\((.*), (.*)\\)"); "(a, b)" match { case r83(g0, g1) => assert((g0, g1) == ("a", "b")) }
val r84 = Regex2("^((?:aa)*)(?:X+((?:\\d+|-)(?:X+(.+))?))?$"); "aaaaX5" match { case r84(g0, Some(g1), None) => assert((g0, g1, null) == ("aaaa", "5", null)) }
val r85 = Regex2("^((a|b)+)*ax"); "aax" match { case r85(Some(g0), Some(g1)) => assert((g0, g1) == ("a", "a")) }
val r86 = Regex2("^((a|bc)+)*ax"); "aax" match { case r86(Some(g0), Some(g1)) => assert((g0, g1) == ("a", "a")) }
val r87 = Regex2("^(.*?)\\s*\\|\\s*(?:\\/\\s*|)\'(.+)\'$"); "text|\'sec\'" match { case r87(g0, g1) => assert((g0, g1) == ("text", "sec")) }
val r88 = Regex2("^(.+)?B"); "AB" match { case r88(Some(g0)) => assert((g0) == ("A")) }
val r89 = Regex2("^(.,){2}c"); "a,b,c" match { case r89(g0) => assert((g0) == ("b,")) }
val r90 = Regex2("^(0+)?(?:x(1))?"); "x1" match { case r90(None, Some(g1)) => assert((null, g1) == (null, "1")) }
val r91 = Regex2("^(?:(\\d)x)?\\d$"); "1" match { case r91(None) => assert((null) == (null)) }
val r92 = Regex2("^(?:(X)?(\\d)|(X)?(\\d\\d))$"); "X12" match { case r92(None, None, Some(g2), Some(g3)) => assert((null, null, g2, g3) == (null, null, "X", "12")) }
val r93 = Regex2("^(?:(XX)?(\\d)|(XX)?(\\d\\d))$"); "XX12" match { case r93(None, None, Some(g2), Some(g3)) => assert((null, null, g2, g3) == (null, null, "XX", "12")) }
val r94 = Regex2("^(?:f|o|b){2,3}?((?:b|a|r)+)\\1$"); "foobarbar" match { case r94(g0) => assert((g0) == ("bar")) }
val r95 = Regex2("^(?:f|o|b){2,3}?((?:b|a|r)+?)\\1$"); "foobarbar" match { case r95(g0) => assert((g0) == ("bar")) }
val r96 = Regex2("^(?:f|o|b){2,3}?(.+)\\1$"); "foobarbar" match { case r96(g0) => assert((g0) == ("bar")) }
val r97 = Regex2("^(?:f|o|b){2,3}?(.+?)\\1$"); "foobarbar" match { case r97(g0) => assert((g0) == ("bar")) }
val r98 = Regex2("^(?:f|o|b){3,4}((?:b|a|r)+)\\1$"); "foobarbar" match { case r98(g0) => assert((g0) == ("bar")) }
val r99 = Regex2("^(?:f|o|b){3,4}((?:b|a|r)+?)\\1$"); "foobarbar" match { case r99(g0) => assert((g0) == ("bar")) }
val r100 = Regex2("^(?:f|o|b){3,4}(.+)\\1$"); "foobarbar" match { case r100(g0) => assert((g0) == ("bar")) }
val r101 = Regex2("^(?:f|o|b){3,4}(.+?)\\1$"); "foobarbar" match { case r101(g0) => assert((g0) == ("bar")) }
val r102 = Regex2("^([0-9a-fA-F]+)(?:x([0-9a-fA-F]+)?)(?:x([0-9a-fA-F]+))?"); "012cxx0190" match { case r102(g0, None, Some(g2)) => assert((g0, null, g2) == ("012c", null, "0190")) }
val r103 = Regex2("^([^,]*,){0,3}d"); "aaa,b,c,d" match { case r103(Some(g0)) => assert((g0) == ("c,")) }
val r104 = Regex2("^([^,]*,){2}c"); "a,b,c" match { case r104(g0) => assert((g0) == ("b,")) }
val r105 = Regex2("^([^,]*,){3,}d"); "aaa,b,c,d" match { case r105(g0) => assert((g0) == ("c,")) }
val r106 = Regex2("^([^,]*,){3}d"); "aaa,b,c,d" match { case r106(g0) => assert((g0) == ("c,")) }
val r107 = Regex2("^([^,]{0,3},){0,3}d"); "aaa,b,c,d" match { case r107(Some(g0)) => assert((g0) == ("c,")) }
val r108 = Regex2("^([^,]{0,3},){3,}d"); "aaa,b,c,d" match { case r108(g0) => assert((g0) == ("c,")) }
val r109 = Regex2("^([^,]{0,3},){3}d"); "aaa,b,c,d" match { case r109(g0) => assert((g0) == ("c,")) }
val r110 = Regex2("^([^,]{1,3},){0,3}d"); "aaa,b,c,d" match { case r110(Some(g0)) => assert((g0) == ("c,")) }
val r111 = Regex2("^([^,]{1,3},){3,}d"); "aaa,b,c,d" match { case r111(g0) => assert((g0) == ("c,")) }
val r112 = Regex2("^([^,]{1,3},){3}d"); "aaa,b,c,d" match { case r112(g0) => assert((g0) == ("c,")) }
val r113 = Regex2("^([^,]{1,},){0,3}d"); "aaa,b,c,d" match { case r113(Some(g0)) => assert((g0) == ("c,")) }
val r114 = Regex2("^([^,]{1,},){3,}d"); "aaa,b,c,d" match { case r114(g0) => assert((g0) == ("c,")) }
val r115 = Regex2("^([^,]{1,},){3}d"); "aaa,b,c,d" match { case r115(g0) => assert((g0) == ("c,")) }
val r116 = Regex2("^([^a-z])|(\\^)$"); "." match { case r116(Some(g0), None) => assert((g0, null) == (".", null)) }
val r117 = Regex2("^([a]{1})*$"); "aa" match { case r117(Some(g0)) => assert((g0) == ("a")) }
val r118 = Regex2("^([ab]*?)(b)?(c)$"); "abac" match { case r118(g0, None, g2) => assert((g0, null, g2) == ("aba", null, "c")) }
val r119 = Regex2("^([TUV]+|XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P):"); "ZEQQQQQQQQQQQQQQQQQQP:" match { case r119(g0) => assert((g0) == ("ZEQQQQQQQQQQQQQQQQQQP")) }
val r120 = Regex2("^([TUV]+|XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P):"); "ZEQQQX:" match { case r120(g0) => assert((g0) == ("ZEQQQX")) }
val r121 = Regex2("^([TUV]+|XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P|[MKJ]):"); "ZEQQQQQQQQQQQQQQQQQQP:" match { case r121(g0) => assert((g0) == ("ZEQQQQQQQQQQQQQQQQQQP")) }
val r122 = Regex2("^([TUV]+|XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P|[MKJ]):"); "ZEQQQX:" match { case r122(g0) => assert((g0) == ("ZEQQQX")) }
val r123 = Regex2("^([TUV]+|XXX|YYY|Z.Q*X|Z[TE]Q*P):"); "ZEQQQQQQQQQQQQQQQQQQP:" match { case r123(g0) => assert((g0) == ("ZEQQQQQQQQQQQQQQQQQQP")) }
val r124 = Regex2("^([TUV]+|XXX|YYY|Z.Q*X|Z[TE]Q*P):"); "ZEQQQX:" match { case r124(g0) => assert((g0) == ("ZEQQQX")) }
val r125 = Regex2("^([TUV]+|XXX|YYY|Z.Q*X|Z[TE]Q*P|[MKJ]):"); "ZEQQQQQQQQQQQQQQQQQQP:" match { case r125(g0) => assert((g0) == ("ZEQQQQQQQQQQQQQQQQQQP")) }
val r126 = Regex2("^([TUV]+|XXX|YYY|Z.Q*X|Z[TE]Q*P|[MKJ]):"); "ZEQQQX:" match { case r126(g0) => assert((g0) == ("ZEQQQX")) }
val r127 = Regex2("^(a(b)?)+$"); "aba" match { case r127(g0, Some(g1)) => assert((g0, g1) == ("a", "b")) }
val r128 = Regex2("^(a)?a$"); "a" match { case r128(None) => assert((null) == (null)) }
val r129 = Regex2("^(a+)*ax"); "aax" match { case r129(Some(g0)) => assert((g0) == ("a")) }
val r130 = Regex2("^(a\\1?)(a\\1?)(a\\2?)(a\\3?)$"); "aaaaaa" match { case r130(g0, g1, g2, g3) => assert((g0, g1, g2, g3) == ("a", "aa", "a", "aa")) }
val r131 = Regex2("^(a\\1?){4}$"); "aaaaaa" match { case r131(g0) => assert((g0) == ("aa")) }
val r132 = Regex2("^(a\\1?){4}$"); "aaaaaaaaaa" match { case r132(g0) => assert((g0) == ("aaaa")) }
val r133 = Regex2("^(aa(bb)?)+$"); "aabbaa" match { case r133(g0, Some(g1)) => assert((g0, g1) == ("aa", "bb")) }
val r134 = Regex2("^(b+?|a){1,2}c"); "bbbac" match { case r134(g0) => assert((g0) == ("a")) }
val r135 = Regex2("^(b+?|a){1,2}c"); "bbbbac" match { case r135(g0) => assert((g0) == ("a")) }
val r136 = Regex2("^(foo|)bar$"); "bar" match { case r136(g0) => assert((g0) == ("")) }
val r137 = Regex2("^(foo||baz)bar$"); "bar" match { case r137(g0) => assert((g0) == ("")) }
val r138 = Regex2("^(foo||baz)bar$"); "bazbar" match { case r138(g0) => assert((g0) == ("baz")) }
val r139 = Regex2("^(foo||baz)bar$"); "foobar" match { case r139(g0) => assert((g0) == ("foo")) }
val r140 = Regex2("^(XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P):"); "ZEQQQQQQQQQQQQQQQQQQP:" match { case r140(g0) => assert((g0) == ("ZEQQQQQQQQQQQQQQQQQQP")) }
val r141 = Regex2("^(XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P):"); "ZEQQQX:" match { case r141(g0) => assert((g0) == ("ZEQQQX")) }
val r142 = Regex2("^(XXX|YYY|Z.Q*X|Z[TE]Q*P):"); "ZEQQQQQQQQQQQQQQQQQQP:" match { case r142(g0) => assert((g0) == ("ZEQQQQQQQQQQQQQQQQQQP")) }
val r143 = Regex2("^(XXX|YYY|Z.Q*X|Z[TE]Q*P):"); "ZEQQQX:" match { case r143(g0) => assert((g0) == ("ZEQQQX")) }
val r144 = Regex2("^.{2,3}?((?:b|a|r)+)\\1$"); "foobarbar" match { case r144(g0) => assert((g0) == ("bar")) }
val r145 = Regex2("^.{2,3}?((?:b|a|r)+?)\\1$"); "foobarbar" match { case r145(g0) => assert((g0) == ("bar")) }
val r146 = Regex2("^.{2,3}?(.+)\\1$"); "foobarbar" match { case r146(g0) => assert((g0) == ("bar")) }
val r147 = Regex2("^.{2,3}?(.+?)\\1$"); "foobarbar" match { case r147(g0) => assert((g0) == ("bar")) }
val r148 = Regex2("^.{3,4}((?:b|a|r)+)\\1$"); "foobarbar" match { case r148(g0) => assert((g0) == ("bar")) }
val r149 = Regex2("^.{3,4}((?:b|a|r)+?)\\1$"); "foobarbar" match { case r149(g0) => assert((g0) == ("bar")) }
val r150 = Regex2("^.{3,4}(.+)\\1$"); "foobarbar" match { case r150(g0) => assert((g0) == ("bar")) }
val r151 = Regex2("^.{3,4}(.+?)\\1$"); "foobarbar" match { case r151(g0) => assert((g0) == ("bar")) }
val r152 = Regex2("^m?(\\d)(.*)\\1$"); "5b5" match { case r152(g0, g1) => assert((g0, g1) == ("5", "b")) }
val r153 = Regex2("^m?(\\D)(.*)\\1$"); "aba" match { case r153(g0, g1) => assert((g0, g1) == ("a", "b")) }
val r154 = Regex2("^m?(\\S)(.*)\\1$"); "aba" match { case r154(g0, g1) => assert((g0, g1) == ("a", "b")) }
val r155 = Regex2("^m?(\\W)(.*)\\1$"); ":b:" match { case r155(g0, g1) => assert((g0, g1) == (":", "b")) }
val r156 = Regex2("^m?(\\w)(.*)\\1$"); "aba" match { case r156(g0, g1) => assert((g0, g1) == ("a", "b")) }
val r157 = Regex2("a(?:b|(c|e){1,2}?|d)+?(.)"); "ace" match { case r157(Some(g0), g1) => assert((g0, g1) == ("c", "e")) }
val r158 = Regex2("a(?:b|c|d)(.)"); "ace" match { case r158(g0) => assert((g0) == ("e")) }
val r159 = Regex2("a(?:b|c|d)*(.)"); "ace" match { case r159(g0) => assert((g0) == ("e")) }
val r160 = Regex2("a(?:b|c|d)+(.)"); "acdbcdbe" match { case r160(g0) => assert((g0) == ("e")) }
val r161 = Regex2("a(?:b|c|d)+?(.)"); "acdbcdbe" match { case r161(g0) => assert((g0) == ("e")) }
val r162 = Regex2("a(?:b|c|d)+?(.)"); "ace" match { case r162(g0) => assert((g0) == ("e")) }
val r163 = Regex2("a(?:b|c|d){5,6}(.)"); "acdbcdbe" match { case r163(g0) => assert((g0) == ("e")) }
val r164 = Regex2("a(?:b|c|d){5,6}?(.)"); "acdbcdbe" match { case r164(g0) => assert((g0) == ("e")) }
val r165 = Regex2("a(?:b|c|d){5,7}(.)"); "acdbcdbe" match { case r165(g0) => assert((g0) == ("e")) }
val r166 = Regex2("a(?:b|c|d){5,7}?(.)"); "acdbcdbe" match { case r166(g0) => assert((g0) == ("e")) }
val r167 = Regex2("a(?:b|c|d){6,7}(.)"); "acdbcdbe" match { case r167(g0) => assert((g0) == ("e")) }
val r168 = Regex2("a(?:b|c|d){6,7}?(.)"); "acdbcdbe" match { case r168(g0) => assert((g0) == ("e")) }
val r169 = Regex2("a([bc]*)(c*d)"); "abcd" match { case r169(g0, g1) => assert((g0, g1) == ("bc", "d")) }
val r170 = Regex2("a([bc]*)(c+d)"); "abcd" match { case r170(g0, g1) => assert((g0, g1) == ("b", "cd")) }
val r171 = Regex2("a([bc]*)c*"); "abc" match { case r171(g0) => assert((g0) == ("bc")) }
val r172 = Regex2("a([bc]+)(c*d)"); "abcd" match { case r172(g0, g1) => assert((g0, g1) == ("bc", "d")) }
val r173 = Regex2("a(bc)d"); "abcd" match { case r173(g0) => assert((g0) == ("bc")) }
val r174 = Regex2("foo(aA)*+b"); "fooaAaAaAaAaAb" match { case r174(Some(g0)) => assert((g0) == ("aA")) }
val r175 = Regex2("foo(aA)++b"); "fooaAaAaAaAaAb" match { case r175(g0) => assert((g0) == ("aA")) }
val r176 = Regex2("foo(aA)?+b"); "fooaAb" match { case r176(Some(g0)) => assert((g0) == ("aA")) }
val r177 = Regex2("foo(aA){1,5}+b"); "fooaAaAaAaAaAb" match { case r177(g0) => assert((g0) == ("aA")) }
val r178 = Regex2("foo(aA|bB)*+b"); "foobBbBaAaAaAb" match { case r178(Some(g0)) => assert((g0) == ("aA")) }
val r179 = Regex2("foo(aA|bB)++b"); "foobBaAbBaAbBb" match { case r179(g0) => assert((g0) == ("bB")) }
val r180 = Regex2("foo(aA|bB)?+b"); "foobBb" match { case r180(Some(g0)) => assert((g0) == ("bB")) }
val r181 = Regex2("foo(aA|bB){1,5}+b"); "foobBaAaAaAaAb" match { case r181(g0) => assert((g0) == ("aA")) }
val r182 = Regex2("X(\\w+)(?=\\s)|X(\\w+)"); "Xab" match { case r182(None, Some(g1)) => assert((null, g1) == (null, "ab")) }
val r183 = Regex2("x(~~)*(?:(?:F)?)?"); "x~~" match { case r183(Some(g0)) => assert((g0) == ("~~")) }

val r184 = Regex2("(\\Qxxx\\E)"); "xxx" match { case r184(g0) => assert((g0) == ("xxx")) }
val r185 = Regex2("([(][)])"); "()" match { case r185(g0) => assert((g0) == ("()")) }
// val r186 = Regex2("(\\Q))((())((\\\\...\\E)"); "))((())((\\\\..." match { case r186(g0) => assert((g0) == ("))((())((\\\\...")) }

}}
