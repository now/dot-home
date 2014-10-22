﻿#NoEnv
#SingleInstance force
#MaxHotkeysPerInterval 180

Process, Priority, , H
SendMode Input
SetTitleMatchMode Regex

WM_COMMAND := 0x111

digraphs := ComObjCreate("Scripting.Dictionary")
digraphs.item["Nb"] := "#"
digraphs.item["DO"] := "$"
digraphs.item["At"] := "@"
digraphs.item["<("] := "["
digraphs.item["//"] := "\"
digraphs.item[")>"] := "]"
digraphs.item["'>"] := "^"
digraphs.item["'!"] := "`"
digraphs.item["(!"] := "{"
digraphs.item["!!"] := "|"
digraphs.item["!)"] := "}"
digraphs.item["'?"] := "~"
digraphs.item["DT"] := ""
digraphs.item["PA"] := ""
digraphs.item["HO"] := ""
digraphs.item["BH"] := ""
digraphs.item["NH"] := ""
digraphs.item["IN"] := ""
digraphs.item["NL"] := ""
digraphs.item["SA"] := ""
digraphs.item["ES"] := ""
digraphs.item["HS"] := ""
digraphs.item["HJ"] := ""
digraphs.item["VS"] := ""
digraphs.item["PD"] := ""
digraphs.item["PU"] := ""
digraphs.item["RI"] := ""
digraphs.item["S2"] := ""
digraphs.item["S3"] := ""
digraphs.item["DC"] := ""
digraphs.item["P1"] := ""
digraphs.item["P2"] := ""
digraphs.item["TS"] := ""
digraphs.item["CC"] := ""
digraphs.item["MW"] := ""
digraphs.item["SG"] := ""
digraphs.item["EG"] := ""
digraphs.item["SS"] := ""
digraphs.item["GC"] := ""
digraphs.item["SC"] := ""
digraphs.item["CI"] := ""
digraphs.item["ST"] := ""
digraphs.item["OC"] := ""
digraphs.item["PM"] := ""
digraphs.item["AC"] := ""
digraphs.item["NS"] := " "
digraphs.item["!I"] := "¡"
digraphs.item["Ct"] := "¢"
digraphs.item["Pd"] := "£"
digraphs.item["Cu"] := "¤"
digraphs.item["Ye"] := "¥"
digraphs.item["BB"] := "¦"
digraphs.item["SE"] := "§"
digraphs.item["':"] := "¨"
digraphs.item["Co"] := "©"
digraphs.item["-a"] := "ª"
digraphs.item["<<"] := "«"
digraphs.item["NO"] := "¬"
digraphs.item["--"] := "­"
digraphs.item["Rg"] := "®"
digraphs.item["'m"] := "¯"
digraphs.item["DG"] := "°"
digraphs.item["+-"] := "±"
digraphs.item["2S"] := "²"
digraphs.item["3S"] := "³"
digraphs.item["''"] := "´"
digraphs.item["My"] := "µ"
digraphs.item["PI"] := "¶"
digraphs.item[".M"] := "·"
digraphs.item["',"] := "¸"
digraphs.item["1S"] := "¹"
digraphs.item["-o"] := "º"
digraphs.item[">>"] := "»"
digraphs.item["14"] := "¼"
digraphs.item["12"] := "½"
digraphs.item["34"] := "¾"
digraphs.item["?I"] := "¿"
digraphs.item["A!"] := "À"
digraphs.item["A'"] := "Á"
digraphs.item["A>"] := "Â"
digraphs.item["A?"] := "Ã"
digraphs.item["A:"] := "Ä"
digraphs.item["AA"] := "Å"
digraphs.item["AE"] := "Æ"
digraphs.item["C,"] := "Ç"
digraphs.item["E!"] := "È"
digraphs.item["E'"] := "É"
digraphs.item["E>"] := "Ê"
digraphs.item["E:"] := "Ë"
digraphs.item["I!"] := "Ì"
digraphs.item["I'"] := "Í"
digraphs.item["I>"] := "Î"
digraphs.item["I:"] := "Ï"
digraphs.item["D-"] := "Ð"
digraphs.item["N?"] := "Ñ"
digraphs.item["O!"] := "Ò"
digraphs.item["O'"] := "Ó"
digraphs.item["O>"] := "Ô"
digraphs.item["O?"] := "Õ"
digraphs.item["O:"] := "Ö"
digraphs.item["*X"] := "×"
digraphs.item["O/"] := "Ø"
digraphs.item["U!"] := "Ù"
digraphs.item["U'"] := "Ú"
digraphs.item["U>"] := "Û"
digraphs.item["U:"] := "Ü"
digraphs.item["Y'"] := "Ý"
digraphs.item["TH"] := "Þ"
digraphs.item["ss"] := "ß"
digraphs.item["a!"] := "à"
digraphs.item["a'"] := "á"
digraphs.item["a>"] := "â"
digraphs.item["a?"] := "ã"
digraphs.item["a:"] := "ä"
digraphs.item["aa"] := "å"
digraphs.item["ae"] := "æ"
digraphs.item["c,"] := "ç"
digraphs.item["e!"] := "è"
digraphs.item["e'"] := "é"
digraphs.item["e>"] := "ê"
digraphs.item["e:"] := "ë"
digraphs.item["i!"] := "ì"
digraphs.item["i'"] := "í"
digraphs.item["i>"] := "î"
digraphs.item["i:"] := "ï"
digraphs.item["d-"] := "ð"
digraphs.item["n?"] := "ñ"
digraphs.item["o!"] := "ò"
digraphs.item["o'"] := "ó"
digraphs.item["o>"] := "ô"
digraphs.item["o?"] := "õ"
digraphs.item["o:"] := "ö"
digraphs.item["-:"] := "÷"
digraphs.item["o/"] := "ø"
digraphs.item["u!"] := "ù"
digraphs.item["u'"] := "ú"
digraphs.item["u>"] := "û"
digraphs.item["u:"] := "ü"
digraphs.item["y'"] := "ý"
digraphs.item["th"] := "þ"
digraphs.item["y:"] := "ÿ"
digraphs.item["A-"] := "Ā"
digraphs.item["a-"] := "ā"
digraphs.item["A("] := "Ă"
digraphs.item["a("] := "ă"
digraphs.item["A;"] := "Ą"
digraphs.item["a;"] := "ą"
digraphs.item["C'"] := "Ć"
digraphs.item["c'"] := "ć"
digraphs.item["C>"] := "Ĉ"
digraphs.item["c>"] := "ĉ"
digraphs.item["C."] := "Ċ"
digraphs.item["c."] := "ċ"
digraphs.item["C<"] := "Č"
digraphs.item["c<"] := "č"
digraphs.item["D<"] := "Ď"
digraphs.item["d<"] := "ď"
digraphs.item["D/"] := "Đ"
digraphs.item["d/"] := "đ"
digraphs.item["E-"] := "Ē"
digraphs.item["e-"] := "ē"
digraphs.item["E("] := "Ĕ"
digraphs.item["e("] := "ĕ"
digraphs.item["E."] := "Ė"
digraphs.item["e."] := "ė"
digraphs.item["E;"] := "Ę"
digraphs.item["e;"] := "ę"
digraphs.item["E<"] := "Ě"
digraphs.item["e<"] := "ě"
digraphs.item["G>"] := "Ĝ"
digraphs.item["g>"] := "ĝ"
digraphs.item["G("] := "Ğ"
digraphs.item["g("] := "ğ"
digraphs.item["G."] := "Ġ"
digraphs.item["g."] := "ġ"
digraphs.item["G,"] := "Ģ"
digraphs.item["g,"] := "ģ"
digraphs.item["H>"] := "Ĥ"
digraphs.item["h>"] := "ĥ"
digraphs.item["H/"] := "Ħ"
digraphs.item["h/"] := "ħ"
digraphs.item["I?"] := "Ĩ"
digraphs.item["i?"] := "ĩ"
digraphs.item["I-"] := "Ī"
digraphs.item["i-"] := "ī"
digraphs.item["I("] := "Ĭ"
digraphs.item["i("] := "ĭ"
digraphs.item["I;"] := "Į"
digraphs.item["i;"] := "į"
digraphs.item["I."] := "İ"
digraphs.item["i."] := "ı"
digraphs.item["IJ"] := "Ĳ"
digraphs.item["ij"] := "ĳ"
digraphs.item["J>"] := "Ĵ"
digraphs.item["j>"] := "ĵ"
digraphs.item["K,"] := "Ķ"
digraphs.item["k,"] := "ķ"
digraphs.item["kk"] := "ĸ"
digraphs.item["L'"] := "Ĺ"
digraphs.item["l'"] := "ĺ"
digraphs.item["L,"] := "Ļ"
digraphs.item["l,"] := "ļ"
digraphs.item["L<"] := "Ľ"
digraphs.item["l<"] := "ľ"
digraphs.item["L."] := "Ŀ"
digraphs.item["l."] := "ŀ"
digraphs.item["L/"] := "Ł"
digraphs.item["l/"] := "ł"
digraphs.item["N'"] := "Ń"
digraphs.item["n'"] := "ń"
digraphs.item["N,"] := "Ņ"
digraphs.item["n,"] := "ņ"
digraphs.item["N<"] := "Ň"
digraphs.item["n<"] := "ň"
digraphs.item["'n"] := "ŉ"
digraphs.item["NG"] := "Ŋ"
digraphs.item["ng"] := "ŋ"
digraphs.item["O-"] := "Ō"
digraphs.item["o-"] := "ō"
digraphs.item["O("] := "Ŏ"
digraphs.item["o("] := "ŏ"
digraphs.item["O"""] := "Ő"
digraphs.item["o"""] := "ő"
digraphs.item["OE"] := "Œ"
digraphs.item["oe"] := "œ"
digraphs.item["R'"] := "Ŕ"
digraphs.item["r'"] := "ŕ"
digraphs.item["R,"] := "Ŗ"
digraphs.item["r,"] := "ŗ"
digraphs.item["R<"] := "Ř"
digraphs.item["r<"] := "ř"
digraphs.item["S'"] := "Ś"
digraphs.item["s'"] := "ś"
digraphs.item["S>"] := "Ŝ"
digraphs.item["s>"] := "ŝ"
digraphs.item["S,"] := "Ş"
digraphs.item["s,"] := "ş"
digraphs.item["S<"] := "Š"
digraphs.item["s<"] := "š"
digraphs.item["T,"] := "Ţ"
digraphs.item["t,"] := "ţ"
digraphs.item["T<"] := "Ť"
digraphs.item["t<"] := "ť"
digraphs.item["T/"] := "Ŧ"
digraphs.item["t/"] := "ŧ"
digraphs.item["U?"] := "Ũ"
digraphs.item["u?"] := "ũ"
digraphs.item["U-"] := "Ū"
digraphs.item["u-"] := "ū"
digraphs.item["U("] := "Ŭ"
digraphs.item["u("] := "ŭ"
digraphs.item["U0"] := "Ů"
digraphs.item["u0"] := "ů"
digraphs.item["U"""] := "Ű"
digraphs.item["u"""] := "ű"
digraphs.item["U;"] := "Ų"
digraphs.item["u;"] := "ų"
digraphs.item["W>"] := "Ŵ"
digraphs.item["w>"] := "ŵ"
digraphs.item["Y>"] := "Ŷ"
digraphs.item["y>"] := "ŷ"
digraphs.item["Y:"] := "Ÿ"
digraphs.item["Z'"] := "Ź"
digraphs.item["z'"] := "ź"
digraphs.item["Z."] := "Ż"
digraphs.item["z."] := "ż"
digraphs.item["Z<"] := "Ž"
digraphs.item["z<"] := "ž"
digraphs.item["O9"] := "Ơ"
digraphs.item["o9"] := "ơ"
digraphs.item["OI"] := "Ƣ"
digraphs.item["oi"] := "ƣ"
digraphs.item["yr"] := "Ʀ"
digraphs.item["U9"] := "Ư"
digraphs.item["u9"] := "ư"
digraphs.item["Z/"] := "Ƶ"
digraphs.item["z/"] := "ƶ"
digraphs.item["ED"] := "Ʒ"
digraphs.item["A<"] := "Ǎ"
digraphs.item["a<"] := "ǎ"
digraphs.item["I<"] := "Ǐ"
digraphs.item["i<"] := "ǐ"
digraphs.item["O<"] := "Ǒ"
digraphs.item["o<"] := "ǒ"
digraphs.item["U<"] := "Ǔ"
digraphs.item["u<"] := "ǔ"
digraphs.item["A1"] := "Ǟ"
digraphs.item["a1"] := "ǟ"
digraphs.item["A7"] := "Ǡ"
digraphs.item["a7"] := "ǡ"
digraphs.item["A3"] := "Ǣ"
digraphs.item["a3"] := "ǣ"
digraphs.item["G/"] := "Ǥ"
digraphs.item["g/"] := "ǥ"
digraphs.item["G<"] := "Ǧ"
digraphs.item["g<"] := "ǧ"
digraphs.item["K<"] := "Ǩ"
digraphs.item["k<"] := "ǩ"
digraphs.item["O;"] := "Ǫ"
digraphs.item["o;"] := "ǫ"
digraphs.item["O1"] := "Ǭ"
digraphs.item["o1"] := "ǭ"
digraphs.item["EZ"] := "Ǯ"
digraphs.item["ez"] := "ǯ"
digraphs.item["j<"] := "ǰ"
digraphs.item["G'"] := "Ǵ"
digraphs.item["g'"] := "ǵ"
digraphs.item[";S"] := "ʿ"
digraphs.item["'<"] := "ˇ"
digraphs.item["'("] := "˘"
digraphs.item["'."] := "˙"
digraphs.item["'0"] := "˚"
digraphs.item["';"] := "˛"
digraphs.item["'"""] := "˝"
digraphs.item["A%"] := "Ά"
digraphs.item["E%"] := "Έ"
digraphs.item["Y%"] := "Ή"
digraphs.item["I%"] := "Ί"
digraphs.item["O%"] := "Ό"
digraphs.item["U%"] := "Ύ"
digraphs.item["W%"] := "Ώ"
digraphs.item["i3"] := "ΐ"
digraphs.item["A*"] := "Α"
digraphs.item["B*"] := "Β"
digraphs.item["G*"] := "Γ"
digraphs.item["D*"] := "Δ"
digraphs.item["E*"] := "Ε"
digraphs.item["Z*"] := "Ζ"
digraphs.item["Y*"] := "Η"
digraphs.item["H*"] := "Θ"
digraphs.item["I*"] := "Ι"
digraphs.item["K*"] := "Κ"
digraphs.item["L*"] := "Λ"
digraphs.item["M*"] := "Μ"
digraphs.item["N*"] := "Ν"
digraphs.item["C*"] := "Ξ"
digraphs.item["O*"] := "Ο"
digraphs.item["P*"] := "Π"
digraphs.item["R*"] := "Ρ"
digraphs.item["S*"] := "Σ"
digraphs.item["T*"] := "Τ"
digraphs.item["U*"] := "Υ"
digraphs.item["F*"] := "Φ"
digraphs.item["X*"] := "Χ"
digraphs.item["Q*"] := "Ψ"
digraphs.item["W*"] := "Ω"
digraphs.item["J*"] := "Ϊ"
digraphs.item["V*"] := "Ϋ"
digraphs.item["a%"] := "ά"
digraphs.item["e%"] := "έ"
digraphs.item["y%"] := "ή"
digraphs.item["i%"] := "ί"
digraphs.item["u3"] := "ΰ"
digraphs.item["a*"] := "α"
digraphs.item["b*"] := "β"
digraphs.item["g*"] := "γ"
digraphs.item["d*"] := "δ"
digraphs.item["e*"] := "ε"
digraphs.item["z*"] := "ζ"
digraphs.item["y*"] := "η"
digraphs.item["h*"] := "θ"
digraphs.item["i*"] := "ι"
digraphs.item["k*"] := "κ"
digraphs.item["l*"] := "λ"
digraphs.item["m*"] := "μ"
digraphs.item["n*"] := "ν"
digraphs.item["c*"] := "ξ"
digraphs.item["o*"] := "ο"
digraphs.item["p*"] := "π"
digraphs.item["r*"] := "ρ"
digraphs.item["*s"] := "ς"
digraphs.item["s*"] := "σ"
digraphs.item["t*"] := "τ"
digraphs.item["u*"] := "υ"
digraphs.item["f*"] := "φ"
digraphs.item["x*"] := "χ"
digraphs.item["q*"] := "ψ"
digraphs.item["w*"] := "ω"
digraphs.item["j*"] := "ϊ"
digraphs.item["v*"] := "ϋ"
digraphs.item["o%"] := "ό"
digraphs.item["u%"] := "ύ"
digraphs.item["w%"] := "ώ"
digraphs.item["'G"] := "Ϙ"
digraphs.item[",G"] := "ϙ"
digraphs.item["T3"] := "Ϛ"
digraphs.item["t3"] := "ϛ"
digraphs.item["M3"] := "Ϝ"
digraphs.item["m3"] := "ϝ"
digraphs.item["K3"] := "Ϟ"
digraphs.item["k3"] := "ϟ"
digraphs.item["P3"] := "Ϡ"
digraphs.item["p3"] := "ϡ"
digraphs.item["'%"] := "ϴ"
digraphs.item["j3"] := "ϵ"
digraphs.item["IO"] := "Ё"
digraphs.item["D%"] := "Ђ"
digraphs.item["G%"] := "Ѓ"
digraphs.item["IE"] := "Є"
digraphs.item["DS"] := "Ѕ"
digraphs.item["II"] := "І"
digraphs.item["YI"] := "Ї"
digraphs.item["J%"] := "Ј"
digraphs.item["LJ"] := "Љ"
digraphs.item["NJ"] := "Њ"
digraphs.item["Ts"] := "Ћ"
digraphs.item["KJ"] := "Ќ"
digraphs.item["V%"] := "Ў"
digraphs.item["DZ"] := "Џ"
digraphs.item["A="] := "А"
digraphs.item["B="] := "Б"
digraphs.item["V="] := "В"
digraphs.item["G="] := "Г"
digraphs.item["D="] := "Д"
digraphs.item["E="] := "Е"
digraphs.item["Z%"] := "Ж"
digraphs.item["Z="] := "З"
digraphs.item["I="] := "И"
digraphs.item["J="] := "Й"
digraphs.item["K="] := "К"
digraphs.item["L="] := "Л"
digraphs.item["M="] := "М"
digraphs.item["N="] := "Н"
digraphs.item["O="] := "О"
digraphs.item["P="] := "П"
digraphs.item["R="] := "Р"
digraphs.item["S="] := "С"
digraphs.item["T="] := "Т"
digraphs.item["U="] := "У"
digraphs.item["F="] := "Ф"
digraphs.item["H="] := "Х"
digraphs.item["C="] := "Ц"
digraphs.item["C%"] := "Ч"
digraphs.item["S%"] := "Ш"
digraphs.item["Sc"] := "Щ"
digraphs.item["="""] := "Ъ"
digraphs.item["Y="] := "Ы"
digraphs.item["%"""] := "Ь"
digraphs.item["JE"] := "Э"
digraphs.item["JU"] := "Ю"
digraphs.item["JA"] := "Я"
digraphs.item["a="] := "а"
digraphs.item["b="] := "б"
digraphs.item["v="] := "в"
digraphs.item["g="] := "г"
digraphs.item["d="] := "д"
digraphs.item["e="] := "е"
digraphs.item["z%"] := "ж"
digraphs.item["z="] := "з"
digraphs.item["i="] := "и"
digraphs.item["j="] := "й"
digraphs.item["k="] := "к"
digraphs.item["l="] := "л"
digraphs.item["m="] := "м"
digraphs.item["n="] := "н"
digraphs.item["o="] := "о"
digraphs.item["p="] := "п"
digraphs.item["r="] := "р"
digraphs.item["s="] := "с"
digraphs.item["t="] := "т"
digraphs.item["u="] := "у"
digraphs.item["f="] := "ф"
digraphs.item["h="] := "х"
digraphs.item["c="] := "ц"
digraphs.item["c%"] := "ч"
digraphs.item["s%"] := "ш"
digraphs.item["sc"] := "щ"
digraphs.item["='"] := "ъ"
digraphs.item["y="] := "ы"
digraphs.item["%'"] := "ь"
digraphs.item["je"] := "э"
digraphs.item["ju"] := "ю"
digraphs.item["ja"] := "я"
digraphs.item["io"] := "ё"
digraphs.item["d%"] := "ђ"
digraphs.item["g%"] := "ѓ"
digraphs.item["ie"] := "є"
digraphs.item["ds"] := "ѕ"
digraphs.item["ii"] := "і"
digraphs.item["yi"] := "ї"
digraphs.item["j%"] := "ј"
digraphs.item["lj"] := "љ"
digraphs.item["nj"] := "њ"
digraphs.item["ts"] := "ћ"
digraphs.item["kj"] := "ќ"
digraphs.item["v%"] := "ў"
digraphs.item["dz"] := "џ"
digraphs.item["Y3"] := "Ѣ"
digraphs.item["y3"] := "ѣ"
digraphs.item["O3"] := "Ѫ"
digraphs.item["o3"] := "ѫ"
digraphs.item["F3"] := "Ѳ"
digraphs.item["f3"] := "ѳ"
digraphs.item["V3"] := "Ѵ"
digraphs.item["v3"] := "ѵ"
digraphs.item["C3"] := "Ҁ"
digraphs.item["c3"] := "ҁ"
digraphs.item["G3"] := "Ґ"
digraphs.item["g3"] := "ґ"
digraphs.item["A+"] := "א"
digraphs.item["B+"] := "ב"
digraphs.item["G+"] := "ג"
digraphs.item["D+"] := "ד"
digraphs.item["H+"] := "ה"
digraphs.item["W+"] := "ו"
digraphs.item["Z+"] := "ז"
digraphs.item["X+"] := "ח"
digraphs.item["Tj"] := "ט"
digraphs.item["J+"] := "י"
digraphs.item["K%"] := "ך"
digraphs.item["K+"] := "כ"
digraphs.item["L+"] := "ל"
digraphs.item["M%"] := "ם"
digraphs.item["M+"] := "מ"
digraphs.item["N%"] := "ן"
digraphs.item["N+"] := "נ"
digraphs.item["S+"] := "ס"
digraphs.item["E+"] := "ע"
digraphs.item["P%"] := "ף"
digraphs.item["P+"] := "פ"
digraphs.item["Zj"] := "ץ"
digraphs.item["ZJ"] := "צ"
digraphs.item["Q+"] := "ק"
digraphs.item["R+"] := "ר"
digraphs.item["Sh"] := "ש"
digraphs.item["T+"] := "ת"
digraphs.item[",+"] := "،"
digraphs.item[";+"] := "؛"
digraphs.item["?+"] := "؟"
digraphs.item["H'"] := "ء"
digraphs.item["aM"] := "آ"
digraphs.item["aH"] := "أ"
digraphs.item["wH"] := "ؤ"
digraphs.item["ah"] := "إ"
digraphs.item["yH"] := "ئ"
digraphs.item["a+"] := "ا"
digraphs.item["b+"] := "ب"
digraphs.item["tm"] := "ة"
digraphs.item["t+"] := "ت"
digraphs.item["tk"] := "ث"
digraphs.item["g+"] := "ج"
digraphs.item["hk"] := "ح"
digraphs.item["x+"] := "خ"
digraphs.item["d+"] := "د"
digraphs.item["dk"] := "ذ"
digraphs.item["r+"] := "ر"
digraphs.item["z+"] := "ز"
digraphs.item["s+"] := "س"
digraphs.item["sn"] := "ش"
digraphs.item["c+"] := "ص"
digraphs.item["dd"] := "ض"
digraphs.item["tj"] := "ط"
digraphs.item["zH"] := "ظ"
digraphs.item["e+"] := "ع"
digraphs.item["i+"] := "غ"
digraphs.item["++"] := "ـ"
digraphs.item["f+"] := "ف"
digraphs.item["q+"] := "ق"
digraphs.item["k+"] := "ك"
digraphs.item["l+"] := "ل"
digraphs.item["m+"] := "م"
digraphs.item["n+"] := "ن"
digraphs.item["h+"] := "ه"
digraphs.item["w+"] := "و"
digraphs.item["j+"] := "ى"
digraphs.item["y+"] := "ي"
digraphs.item[":+"] := " ً"
digraphs.item["""+"] := " ٌ"
digraphs.item["=+"] := " ٍ"
digraphs.item["/+"] := " َ"
digraphs.item["'+"] := " ُ"
digraphs.item["1+"] := " ِ"
digraphs.item["3+"] := " ّ"
digraphs.item["0+"] := " ْ"
digraphs.item["aS"] := " ٰ"
digraphs.item["p+"] := "پ"
digraphs.item["v+"] := "ڤ"
digraphs.item["gf"] := "گ"
digraphs.item["0a"] := "۰"
digraphs.item["1a"] := "۱"
digraphs.item["2a"] := "۲"
digraphs.item["3a"] := "۳"
digraphs.item["4a"] := "۴"
digraphs.item["5a"] := "۵"
digraphs.item["6a"] := "۶"
digraphs.item["7a"] := "۷"
digraphs.item["8a"] := "۸"
digraphs.item["9a"] := "۹"
digraphs.item["B."] := "Ḃ"
digraphs.item["b."] := "ḃ"
digraphs.item["B_"] := "Ḇ"
digraphs.item["b_"] := "ḇ"
digraphs.item["D."] := "Ḋ"
digraphs.item["d."] := "ḋ"
digraphs.item["D_"] := "Ḏ"
digraphs.item["d_"] := "ḏ"
digraphs.item["D,"] := "Ḑ"
digraphs.item["d,"] := "ḑ"
digraphs.item["F."] := "Ḟ"
digraphs.item["f."] := "ḟ"
digraphs.item["G-"] := "Ḡ"
digraphs.item["g-"] := "ḡ"
digraphs.item["H."] := "Ḣ"
digraphs.item["h."] := "ḣ"
digraphs.item["H:"] := "Ḧ"
digraphs.item["h:"] := "ḧ"
digraphs.item["H,"] := "Ḩ"
digraphs.item["h,"] := "ḩ"
digraphs.item["K'"] := "Ḱ"
digraphs.item["k'"] := "ḱ"
digraphs.item["K_"] := "Ḵ"
digraphs.item["k_"] := "ḵ"
digraphs.item["L_"] := "Ḻ"
digraphs.item["l_"] := "ḻ"
digraphs.item["M'"] := "Ḿ"
digraphs.item["m'"] := "ḿ"
digraphs.item["M."] := "Ṁ"
digraphs.item["m."] := "ṁ"
digraphs.item["N."] := "Ṅ"
digraphs.item["n."] := "ṅ"
digraphs.item["N_"] := "Ṉ"
digraphs.item["n_"] := "ṉ"
digraphs.item["P'"] := "Ṕ"
digraphs.item["p'"] := "ṕ"
digraphs.item["P."] := "Ṗ"
digraphs.item["p."] := "ṗ"
digraphs.item["R."] := "Ṙ"
digraphs.item["r."] := "ṙ"
digraphs.item["R_"] := "Ṟ"
digraphs.item["r_"] := "ṟ"
digraphs.item["S."] := "Ṡ"
digraphs.item["s."] := "ṡ"
digraphs.item["T."] := "Ṫ"
digraphs.item["t."] := "ṫ"
digraphs.item["T_"] := "Ṯ"
digraphs.item["t_"] := "ṯ"
digraphs.item["V?"] := "Ṽ"
digraphs.item["v?"] := "ṽ"
digraphs.item["W!"] := "Ẁ"
digraphs.item["w!"] := "ẁ"
digraphs.item["W'"] := "Ẃ"
digraphs.item["w'"] := "ẃ"
digraphs.item["W:"] := "Ẅ"
digraphs.item["w:"] := "ẅ"
digraphs.item["W."] := "Ẇ"
digraphs.item["w."] := "ẇ"
digraphs.item["X."] := "Ẋ"
digraphs.item["x."] := "ẋ"
digraphs.item["X:"] := "Ẍ"
digraphs.item["x:"] := "ẍ"
digraphs.item["Y."] := "Ẏ"
digraphs.item["y."] := "ẏ"
digraphs.item["Z>"] := "Ẑ"
digraphs.item["z>"] := "ẑ"
digraphs.item["Z_"] := "Ẕ"
digraphs.item["z_"] := "ẕ"
digraphs.item["h_"] := "ẖ"
digraphs.item["t:"] := "ẗ"
digraphs.item["w0"] := "ẘ"
digraphs.item["y0"] := "ẙ"
digraphs.item["A2"] := "Ả"
digraphs.item["a2"] := "ả"
digraphs.item["E2"] := "Ẻ"
digraphs.item["e2"] := "ẻ"
digraphs.item["E?"] := "Ẽ"
digraphs.item["e?"] := "ẽ"
digraphs.item["I2"] := "Ỉ"
digraphs.item["i2"] := "ỉ"
digraphs.item["O2"] := "Ỏ"
digraphs.item["o2"] := "ỏ"
digraphs.item["U2"] := "Ủ"
digraphs.item["u2"] := "ủ"
digraphs.item["Y!"] := "Ỳ"
digraphs.item["y!"] := "ỳ"
digraphs.item["Y2"] := "Ỷ"
digraphs.item["y2"] := "ỷ"
digraphs.item["Y?"] := "Ỹ"
digraphs.item["y?"] := "ỹ"
digraphs.item[";'"] := "ἀ"
digraphs.item[",'"] := "ἁ"
digraphs.item[";!"] := "ἂ"
digraphs.item[",!"] := "ἃ"
digraphs.item["?;"] := "ἄ"
digraphs.item["?,"] := "ἅ"
digraphs.item["!:"] := "ἆ"
digraphs.item["?:"] := "ἇ"
digraphs.item["1N"] := " "
digraphs.item["1M"] := " "
digraphs.item["3M"] := " "
digraphs.item["4M"] := " "
digraphs.item["6M"] := " "
digraphs.item["1T"] := " "
digraphs.item["1H"] := " "
digraphs.item["-1"] := "‐"
digraphs.item["-N"] := "–"
digraphs.item["-M"] := "—"
digraphs.item["-3"] := "―"
digraphs.item["!2"] := "‖"
digraphs.item["=2"] := "‗"
digraphs.item["'6"] := "‘"
digraphs.item["'9"] := "’"
digraphs.item[".9"] := "‚"
digraphs.item["9'"] := "‛"
digraphs.item["""6"] := "“"
digraphs.item["""9"] := "”"
digraphs.item[":9"] := "„"
digraphs.item["9"""] := "‟"
digraphs.item["/-"] := "†"
digraphs.item["/="] := "‡"
digraphs.item[".."] := "‥"
digraphs.item["%0"] := "‰"
digraphs.item["1'"] := "′"
digraphs.item["2'"] := "″"
digraphs.item["3'"] := "‴"
digraphs.item["1"""] := "‵"
digraphs.item["2"""] := "‶"
digraphs.item["3"""] := "‷"
digraphs.item["Ca"] := "‸"
digraphs.item["<1"] := "‹"
digraphs.item[">1"] := "›"
digraphs.item[":X"] := "※"
digraphs.item["'-"] := "‾"
digraphs.item["/f"] := "⁄"
digraphs.item["0S"] := "⁰"
digraphs.item["4S"] := "⁴"
digraphs.item["5S"] := "⁵"
digraphs.item["6S"] := "⁶"
digraphs.item["7S"] := "⁷"
digraphs.item["8S"] := "⁸"
digraphs.item["9S"] := "⁹"
digraphs.item["+S"] := "⁺"
digraphs.item["-S"] := "⁻"
digraphs.item["=S"] := "⁼"
digraphs.item["(S"] := "⁽"
digraphs.item[")S"] := "⁾"
digraphs.item["nS"] := "ⁿ"
digraphs.item["0s"] := "₀"
digraphs.item["1s"] := "₁"
digraphs.item["2s"] := "₂"
digraphs.item["3s"] := "₃"
digraphs.item["4s"] := "₄"
digraphs.item["5s"] := "₅"
digraphs.item["6s"] := "₆"
digraphs.item["7s"] := "₇"
digraphs.item["8s"] := "₈"
digraphs.item["9s"] := "₉"
digraphs.item["+s"] := "₊"
digraphs.item["-s"] := "₋"
digraphs.item["=s"] := "₌"
digraphs.item["(s"] := "₍"
digraphs.item[")s"] := "₎"
digraphs.item["Li"] := "₤"
digraphs.item["Pt"] := "₧"
digraphs.item["W="] := "₩"
digraphs.item["=e"] := "€"
digraphs.item["Eu"] := "€"
digraphs.item["oC"] := "℃"
digraphs.item["co"] := "℅"
digraphs.item["oF"] := "℉"
digraphs.item["N0"] := "№"
digraphs.item["PO"] := "℗"
digraphs.item["Rx"] := "℞"
digraphs.item["SM"] := "℠"
digraphs.item["TM"] := "™"
digraphs.item["Om"] := "Ω"
digraphs.item["AO"] := "Å"
digraphs.item["13"] := "⅓"
digraphs.item["23"] := "⅔"
digraphs.item["15"] := "⅕"
digraphs.item["25"] := "⅖"
digraphs.item["35"] := "⅗"
digraphs.item["45"] := "⅘"
digraphs.item["16"] := "⅙"
digraphs.item["56"] := "⅚"
digraphs.item["18"] := "⅛"
digraphs.item["38"] := "⅜"
digraphs.item["58"] := "⅝"
digraphs.item["78"] := "⅞"
digraphs.item["1R"] := "Ⅰ"
digraphs.item["2R"] := "Ⅱ"
digraphs.item["3R"] := "Ⅲ"
digraphs.item["4R"] := "Ⅳ"
digraphs.item["5R"] := "Ⅴ"
digraphs.item["6R"] := "Ⅵ"
digraphs.item["7R"] := "Ⅶ"
digraphs.item["8R"] := "Ⅷ"
digraphs.item["9R"] := "Ⅸ"
digraphs.item["aR"] := "Ⅹ"
digraphs.item["bR"] := "Ⅺ"
digraphs.item["cR"] := "Ⅻ"
digraphs.item["1r"] := "ⅰ"
digraphs.item["2r"] := "ⅱ"
digraphs.item["3r"] := "ⅲ"
digraphs.item["4r"] := "ⅳ"
digraphs.item["5r"] := "ⅴ"
digraphs.item["6r"] := "ⅵ"
digraphs.item["7r"] := "ⅶ"
digraphs.item["8r"] := "ⅷ"
digraphs.item["9r"] := "ⅸ"
digraphs.item["ar"] := "ⅹ"
digraphs.item["br"] := "ⅺ"
digraphs.item["cr"] := "ⅻ"
digraphs.item["<-"] := "←"
digraphs.item["-!"] := "↑"
digraphs.item["->"] := "→"
digraphs.item["-v"] := "↓"
digraphs.item["<>"] := "↔"
digraphs.item["UD"] := "↕"
digraphs.item["<="] := "⇐"
digraphs.item["=>"] := "⇒"
digraphs.item["=="] := "⇔"
digraphs.item["FA"] := "∀"
digraphs.item["dP"] := "∂"
digraphs.item["TE"] := "∃"
digraphs.item["/0"] := "∅"
digraphs.item["DE"] := "∆"
digraphs.item["NB"] := "∇"
digraphs.item["(-"] := "∈"
digraphs.item["-)"] := "∋"
digraphs.item["*P"] := "∏"
digraphs.item["+Z"] := "∑"
digraphs.item["-2"] := "−"
digraphs.item["-+"] := "∓"
digraphs.item["*-"] := "∗"
digraphs.item["Ob"] := "∘"
digraphs.item["Sb"] := "∙"
digraphs.item["RT"] := "√"
digraphs.item["0("] := "∝"
digraphs.item["00"] := "∞"
digraphs.item["-L"] := "∟"
digraphs.item["-V"] := "∠"
digraphs.item["PP"] := "∥"
digraphs.item["AN"] := "∧"
digraphs.item["OR"] := "∨"
digraphs.item["(U"] := "∩"
digraphs.item[")U"] := "∪"
digraphs.item["In"] := "∫"
digraphs.item["DI"] := "∬"
digraphs.item["Io"] := "∮"
digraphs.item[".:"] := "∴"
digraphs.item[":."] := "∵"
digraphs.item[":R"] := "∶"
digraphs.item["::"] := "∷"
digraphs.item["?1"] := "∼"
digraphs.item["CG"] := "∾"
digraphs.item["?-"] := "≃"
digraphs.item["?="] := "≅"
digraphs.item["?2"] := "≈"
digraphs.item["=?"] := "≌"
digraphs.item["HI"] := "≓"
digraphs.item["!="] := "≠"
digraphs.item["=3"] := "≡"
digraphs.item["=<"] := "≤"
digraphs.item[">="] := "≥"
digraphs.item["<*"] := "≪"
digraphs.item["*>"] := "≫"
digraphs.item["!<"] := "≮"
digraphs.item["!>"] := "≯"
digraphs.item["(C"] := "⊂"
digraphs.item[")C"] := "⊃"
digraphs.item["(_"] := "⊆"
digraphs.item[")_"] := "⊇"
digraphs.item["0."] := "⊙"
digraphs.item["02"] := "⊚"
digraphs.item["-T"] := "⊥"
digraphs.item[".P"] := "⋅"
digraphs.item[":3"] := "⋮"
digraphs.item[".3"] := "…"
digraphs.item["Eh"] := "⌂"
digraphs.item["<7"] := "⌈"
digraphs.item[">7"] := "⌉"
digraphs.item["7<"] := "⌊"
digraphs.item["7>"] := "⌋"
digraphs.item["NI"] := "⌐"
digraphs.item["(A"] := "⌒"
digraphs.item["TR"] := "⌕"
digraphs.item["Iu"] := "⌠"
digraphs.item["Il"] := "⌡"
digraphs.item["</"] := "〈"
digraphs.item["/>"] := "〉"
digraphs.item["Vs"] := "␣"
digraphs.item["1h"] := "⑀"
digraphs.item["3h"] := "⑁"
digraphs.item["2h"] := "⑂"
digraphs.item["4h"] := "⑃"
digraphs.item["1j"] := "⑆"
digraphs.item["2j"] := "⑇"
digraphs.item["3j"] := "⑈"
digraphs.item["4j"] := "⑉"
digraphs.item["1."] := "⒈"
digraphs.item["2."] := "⒉"
digraphs.item["3."] := "⒊"
digraphs.item["4."] := "⒋"
digraphs.item["5."] := "⒌"
digraphs.item["6."] := "⒍"
digraphs.item["7."] := "⒎"
digraphs.item["8."] := "⒏"
digraphs.item["9."] := "⒐"
digraphs.item["hh"] := "─"
digraphs.item["HH"] := "━"
digraphs.item["vv"] := "│"
digraphs.item["VV"] := "┃"
digraphs.item["3-"] := "┄"
digraphs.item["3_"] := "┅"
digraphs.item["3!"] := "┆"
digraphs.item["3/"] := "┇"
digraphs.item["4-"] := "┈"
digraphs.item["4_"] := "┉"
digraphs.item["4!"] := "┊"
digraphs.item["4/"] := "┋"
digraphs.item["dr"] := "┌"
digraphs.item["dR"] := "┍"
digraphs.item["Dr"] := "┎"
digraphs.item["DR"] := "┏"
digraphs.item["dl"] := "┐"
digraphs.item["dL"] := "┑"
digraphs.item["Dl"] := "┒"
digraphs.item["LD"] := "┓"
digraphs.item["ur"] := "└"
digraphs.item["uR"] := "┕"
digraphs.item["Ur"] := "┖"
digraphs.item["UR"] := "┗"
digraphs.item["ul"] := "┘"
digraphs.item["uL"] := "┙"
digraphs.item["Ul"] := "┚"
digraphs.item["UL"] := "┛"
digraphs.item["vr"] := "├"
digraphs.item["vR"] := "┝"
digraphs.item["Vr"] := "┠"
digraphs.item["VR"] := "┣"
digraphs.item["vl"] := "┤"
digraphs.item["vL"] := "┥"
digraphs.item["Vl"] := "┨"
digraphs.item["VL"] := "┫"
digraphs.item["dh"] := "┬"
digraphs.item["dH"] := "┯"
digraphs.item["Dh"] := "┰"
digraphs.item["DH"] := "┳"
digraphs.item["uh"] := "┴"
digraphs.item["uH"] := "┷"
digraphs.item["Uh"] := "┸"
digraphs.item["UH"] := "┻"
digraphs.item["vh"] := "┼"
digraphs.item["vH"] := "┿"
digraphs.item["Vh"] := "╂"
digraphs.item["VH"] := "╋"
digraphs.item["FD"] := "╱"
digraphs.item["BD"] := "╲"
digraphs.item["TB"] := "▀"
digraphs.item["LB"] := "▄"
digraphs.item["FB"] := "█"
digraphs.item["lB"] := "▌"
digraphs.item["RB"] := "▐"
digraphs.item[".S"] := "░"
digraphs.item[":S"] := "▒"
digraphs.item["?S"] := "▓"
digraphs.item["fS"] := "■"
digraphs.item["OS"] := "□"
digraphs.item["RO"] := "▢"
digraphs.item["Rr"] := "▣"
digraphs.item["RF"] := "▤"
digraphs.item["RY"] := "▥"
digraphs.item["RH"] := "▦"
digraphs.item["RZ"] := "▧"
digraphs.item["RK"] := "▨"
digraphs.item["RX"] := "▩"
digraphs.item["sB"] := "▪"
digraphs.item["SR"] := "▬"
digraphs.item["Or"] := "▭"
digraphs.item["UT"] := "▲"
digraphs.item["uT"] := "△"
digraphs.item["PR"] := "▶"
digraphs.item["Tr"] := "▷"
digraphs.item["Dt"] := "▼"
digraphs.item["dT"] := "▽"
digraphs.item["PL"] := "◀"
digraphs.item["Tl"] := "◁"
digraphs.item["Db"] := "◆"
digraphs.item["Dw"] := "◇"
digraphs.item["LZ"] := "◊"
digraphs.item["0m"] := "○"
digraphs.item["0o"] := "◎"
digraphs.item["0M"] := "●"
digraphs.item["0L"] := "◐"
digraphs.item["0R"] := "◑"
digraphs.item["Sn"] := "◘"
digraphs.item["Ic"] := "◙"
digraphs.item["Fd"] := "◢"
digraphs.item["Bd"] := "◣"
digraphs.item["*2"] := "★"
digraphs.item["*1"] := "☆"
digraphs.item["<H"] := "☜"
digraphs.item[">H"] := "☞"
digraphs.item["0u"] := "☺"
digraphs.item["0U"] := "☻"
digraphs.item["SU"] := "☼"
digraphs.item["Fm"] := "♀"
digraphs.item["Ml"] := "♂"
digraphs.item["cS"] := "♠"
digraphs.item["cH"] := "♡"
digraphs.item["cD"] := "♢"
digraphs.item["cC"] := "♣"
digraphs.item["Md"] := "♩"
digraphs.item["M8"] := "♪"
digraphs.item["M2"] := "♫"
digraphs.item["Mb"] := "♭"
digraphs.item["Mx"] := "♮"
digraphs.item["MX"] := "♯"
digraphs.item["OK"] := "✓"
digraphs.item["XX"] := "✗"
digraphs.item["-X"] := "✠"
digraphs.item["IS"] := "　"
digraphs.item[",_"] := "、"
digraphs.item["._"] := "。"
digraphs.item["+"""] := "〃"
digraphs.item["+_"] := "〄"
digraphs.item["*_"] := "々"
digraphs.item[";_"] := "〆"
digraphs.item["0_"] := "〇"
digraphs.item["<+"] := "《"
digraphs.item[">+"] := "》"
digraphs.item["<'"] := "「"
digraphs.item[">'"] := "」"
digraphs.item["<"""] := "『"
digraphs.item[">"""] := "』"
digraphs.item["("""] := "【"
digraphs.item[")"""] := "】"
digraphs.item["=T"] := "〒"
digraphs.item["=_"] := "〓"
digraphs.item["('"] := "〔"
digraphs.item[")'"] := "〕"
digraphs.item["(I"] := "〖"
digraphs.item[")I"] := "〗"
digraphs.item["-?"] := "〜"
digraphs.item["A5"] := "ぁ"
digraphs.item["a5"] := "あ"
digraphs.item["I5"] := "ぃ"
digraphs.item["i5"] := "い"
digraphs.item["U5"] := "ぅ"
digraphs.item["u5"] := "う"
digraphs.item["E5"] := "ぇ"
digraphs.item["e5"] := "え"
digraphs.item["O5"] := "ぉ"
digraphs.item["o5"] := "お"
digraphs.item["ka"] := "か"
digraphs.item["ga"] := "が"
digraphs.item["ki"] := "き"
digraphs.item["gi"] := "ぎ"
digraphs.item["ku"] := "く"
digraphs.item["gu"] := "ぐ"
digraphs.item["ke"] := "け"
digraphs.item["ge"] := "げ"
digraphs.item["ko"] := "こ"
digraphs.item["go"] := "ご"
digraphs.item["sa"] := "さ"
digraphs.item["za"] := "ざ"
digraphs.item["si"] := "し"
digraphs.item["zi"] := "じ"
digraphs.item["su"] := "す"
digraphs.item["zu"] := "ず"
digraphs.item["se"] := "せ"
digraphs.item["ze"] := "ぜ"
digraphs.item["so"] := "そ"
digraphs.item["zo"] := "ぞ"
digraphs.item["ta"] := "た"
digraphs.item["da"] := "だ"
digraphs.item["ti"] := "ち"
digraphs.item["di"] := "ぢ"
digraphs.item["tU"] := "っ"
digraphs.item["tu"] := "つ"
digraphs.item["du"] := "づ"
digraphs.item["te"] := "て"
digraphs.item["de"] := "で"
digraphs.item["to"] := "と"
digraphs.item["do"] := "ど"
digraphs.item["na"] := "な"
digraphs.item["ni"] := "に"
digraphs.item["nu"] := "ぬ"
digraphs.item["ne"] := "ね"
digraphs.item["no"] := "の"
digraphs.item["ha"] := "は"
digraphs.item["ba"] := "ば"
digraphs.item["pa"] := "ぱ"
digraphs.item["hi"] := "ひ"
digraphs.item["bi"] := "び"
digraphs.item["pi"] := "ぴ"
digraphs.item["hu"] := "ふ"
digraphs.item["bu"] := "ぶ"
digraphs.item["pu"] := "ぷ"
digraphs.item["he"] := "へ"
digraphs.item["be"] := "べ"
digraphs.item["pe"] := "ぺ"
digraphs.item["ho"] := "ほ"
digraphs.item["bo"] := "ぼ"
digraphs.item["po"] := "ぽ"
digraphs.item["ma"] := "ま"
digraphs.item["mi"] := "み"
digraphs.item["mu"] := "む"
digraphs.item["me"] := "め"
digraphs.item["mo"] := "も"
digraphs.item["yA"] := "ゃ"
digraphs.item["ya"] := "や"
digraphs.item["yU"] := "ゅ"
digraphs.item["yu"] := "ゆ"
digraphs.item["yO"] := "ょ"
digraphs.item["yo"] := "よ"
digraphs.item["ra"] := "ら"
digraphs.item["ri"] := "り"
digraphs.item["ru"] := "る"
digraphs.item["re"] := "れ"
digraphs.item["ro"] := "ろ"
digraphs.item["wA"] := "ゎ"
digraphs.item["wa"] := "わ"
digraphs.item["wi"] := "ゐ"
digraphs.item["we"] := "ゑ"
digraphs.item["wo"] := "を"
digraphs.item["n5"] := "ん"
digraphs.item["vu"] := "ゔ"
digraphs.item["""5"] := "゛"
digraphs.item["05"] := "゜"
digraphs.item["*5"] := "ゝ"
digraphs.item["+5"] := "ゞ"
digraphs.item["a6"] := "ァ"
digraphs.item["A6"] := "ア"
digraphs.item["i6"] := "ィ"
digraphs.item["I6"] := "イ"
digraphs.item["u6"] := "ゥ"
digraphs.item["U6"] := "ウ"
digraphs.item["e6"] := "ェ"
digraphs.item["E6"] := "エ"
digraphs.item["o6"] := "ォ"
digraphs.item["O6"] := "オ"
digraphs.item["Ka"] := "カ"
digraphs.item["Ga"] := "ガ"
digraphs.item["Ki"] := "キ"
digraphs.item["Gi"] := "ギ"
digraphs.item["Ku"] := "ク"
digraphs.item["Gu"] := "グ"
digraphs.item["Ke"] := "ケ"
digraphs.item["Ge"] := "ゲ"
digraphs.item["Ko"] := "コ"
digraphs.item["Go"] := "ゴ"
digraphs.item["Sa"] := "サ"
digraphs.item["Za"] := "ザ"
digraphs.item["Si"] := "シ"
digraphs.item["Zi"] := "ジ"
digraphs.item["Su"] := "ス"
digraphs.item["Zu"] := "ズ"
digraphs.item["Se"] := "セ"
digraphs.item["Ze"] := "ゼ"
digraphs.item["So"] := "ソ"
digraphs.item["Zo"] := "ゾ"
digraphs.item["Ta"] := "タ"
digraphs.item["Da"] := "ダ"
digraphs.item["Ti"] := "チ"
digraphs.item["Di"] := "ヂ"
digraphs.item["TU"] := "ッ"
digraphs.item["Tu"] := "ツ"
digraphs.item["Du"] := "ヅ"
digraphs.item["Te"] := "テ"
digraphs.item["De"] := "デ"
digraphs.item["To"] := "ト"
digraphs.item["Do"] := "ド"
digraphs.item["Na"] := "ナ"
digraphs.item["Ni"] := "ニ"
digraphs.item["Nu"] := "ヌ"
digraphs.item["Ne"] := "ネ"
digraphs.item["No"] := "ノ"
digraphs.item["Ha"] := "ハ"
digraphs.item["Ba"] := "バ"
digraphs.item["Pa"] := "パ"
digraphs.item["Hi"] := "ヒ"
digraphs.item["Bi"] := "ビ"
digraphs.item["Pi"] := "ピ"
digraphs.item["Hu"] := "フ"
digraphs.item["Bu"] := "ブ"
digraphs.item["Pu"] := "プ"
digraphs.item["He"] := "ヘ"
digraphs.item["Be"] := "ベ"
digraphs.item["Pe"] := "ペ"
digraphs.item["Ho"] := "ホ"
digraphs.item["Bo"] := "ボ"
digraphs.item["Po"] := "ポ"
digraphs.item["Ma"] := "マ"
digraphs.item["Mi"] := "ミ"
digraphs.item["Mu"] := "ム"
digraphs.item["Me"] := "メ"
digraphs.item["Mo"] := "モ"
digraphs.item["YA"] := "ャ"
digraphs.item["Ya"] := "ヤ"
digraphs.item["YU"] := "ュ"
digraphs.item["Yu"] := "ユ"
digraphs.item["YO"] := "ョ"
digraphs.item["Yo"] := "ヨ"
digraphs.item["Ra"] := "ラ"
digraphs.item["Ri"] := "リ"
digraphs.item["Ru"] := "ル"
digraphs.item["Re"] := "レ"
digraphs.item["Ro"] := "ロ"
digraphs.item["WA"] := "ヮ"
digraphs.item["Wa"] := "ワ"
digraphs.item["Wi"] := "ヰ"
digraphs.item["We"] := "ヱ"
digraphs.item["Wo"] := "ヲ"
digraphs.item["N6"] := "ン"
digraphs.item["Vu"] := "ヴ"
digraphs.item["KA"] := "ヵ"
digraphs.item["KE"] := "ヶ"
digraphs.item["Va"] := "ヷ"
digraphs.item["Vi"] := "ヸ"
digraphs.item["Ve"] := "ヹ"
digraphs.item["Vo"] := "ヺ"
digraphs.item[".6"] := "・"
digraphs.item["-6"] := "ー"
digraphs.item["*6"] := "ヽ"
digraphs.item["+6"] := "ヾ"
digraphs.item["b4"] := "ㄅ"
digraphs.item["p4"] := "ㄆ"
digraphs.item["m4"] := "ㄇ"
digraphs.item["f4"] := "ㄈ"
digraphs.item["d4"] := "ㄉ"
digraphs.item["t4"] := "ㄊ"
digraphs.item["n4"] := "ㄋ"
digraphs.item["l4"] := "ㄌ"
digraphs.item["g4"] := "ㄍ"
digraphs.item["k4"] := "ㄎ"
digraphs.item["h4"] := "ㄏ"
digraphs.item["j4"] := "ㄐ"
digraphs.item["q4"] := "ㄑ"
digraphs.item["x4"] := "ㄒ"
digraphs.item["zh"] := "ㄓ"
digraphs.item["ch"] := "ㄔ"
digraphs.item["sh"] := "ㄕ"
digraphs.item["r4"] := "ㄖ"
digraphs.item["z4"] := "ㄗ"
digraphs.item["c4"] := "ㄘ"
digraphs.item["s4"] := "ㄙ"
digraphs.item["a4"] := "ㄚ"
digraphs.item["o4"] := "ㄛ"
digraphs.item["e4"] := "ㄜ"
digraphs.item["ai"] := "ㄞ"
digraphs.item["ei"] := "ㄟ"
digraphs.item["au"] := "ㄠ"
digraphs.item["ou"] := "ㄡ"
digraphs.item["an"] := "ㄢ"
digraphs.item["en"] := "ㄣ"
digraphs.item["aN"] := "ㄤ"
digraphs.item["eN"] := "ㄥ"
digraphs.item["er"] := "ㄦ"
digraphs.item["i4"] := "ㄧ"
digraphs.item["u4"] := "ㄨ"
digraphs.item["iu"] := "ㄩ"
digraphs.item["v4"] := "ㄪ"
digraphs.item["nG"] := "ㄫ"
digraphs.item["gn"] := "ㄬ"
digraphs.item["1c"] := "㈠"
digraphs.item["2c"] := "㈡"
digraphs.item["3c"] := "㈢"
digraphs.item["4c"] := "㈣"
digraphs.item["5c"] := "㈤"
digraphs.item["6c"] := "㈥"
digraphs.item["7c"] := "㈦"
digraphs.item["8c"] := "㈧"
digraphs.item["9c"] := "㈨"
digraphs.item["  "] := ""
digraphs.item["/c"] := ""
digraphs.item["UA"] := ""
digraphs.item["UB"] := ""
digraphs.item["""3"] := ""
digraphs.item["""1"] := ""
digraphs.item["""!"] := ""
digraphs.item["""'"] := ""
digraphs.item[""">"] := ""
digraphs.item["""?"] := ""
digraphs.item["""-"] := ""
digraphs.item["""("] := ""
digraphs.item["""."] := ""
digraphs.item[""":"] := ""
digraphs.item["""0"] := ""
digraphs.item[""""""] := ""
digraphs.item["""<"] := ""
digraphs.item[""","] := ""
digraphs.item[""";"] := ""
digraphs.item["""_"] := ""
digraphs.item["""="] := ""
digraphs.item["""/"] := ""
digraphs.item["""i"] := ""
digraphs.item["""d"] := ""
digraphs.item["""p"] := ""
digraphs.item[";;"] := ""
digraphs.item[",,"] := ""
digraphs.item["b3"] := ""
digraphs.item["Ci"] := ""
digraphs.item["f("] := ""
digraphs.item["ed"] := ""
digraphs.item["am"] := ""
digraphs.item["pm"] := ""
digraphs.item["Fl"] := ""
digraphs.item["GF"] := ""
digraphs.item[">V"] := ""
digraphs.item["!*"] := ""
digraphs.item["?*"] := ""
digraphs.item["J<"] := ""
digraphs.item["ff"] := "ﬀ"
digraphs.item["fi"] := "ﬁ"
digraphs.item["fl"] := "ﬂ"
digraphs.item["ft"] := "ﬅ"
digraphs.item["st"] := "ﬆ"

digraphs.item["cb"] := "•"
digraphs.item["tb"] := "‣"
digraphs.item[",3"] := "⋯"
digraphs.item["(/"] := "∉"
digraphs.item["<Y"] := "≺"
digraphs.item["</"] := "⟨"
digraphs.item[">/"] := "⟩"

TagEditorVerify()
{
  TagEditor := ComObjActive("TagEditor.Application")
  Document := TagEditor.ActiveDocument
  VisibleBefore := Document.MessageView.Visible
  Document.Verify()
  VisibleAfter := Document.MessageView.Visible
  if !VisibleBefore && VisibleAfter
    WinMenuSelectItem, , , View, Toolbars, Messages
}

TagEditorCloseAll()
{
  TagEditor := ComObjActive("TagEditor.Application")
  Count := TagEditor.Documents.Count
  loop %Count% {
    TagEditor.Documents.Item(Count - (A_Index - 1) - 1).Close()
  }
}

ScrollTimeout := 1500
ScrollBoost := 20
ScrollLimit := 60
scrollDistance := 0
scrollVMax := 1

#IfWinActive ^Nirvana \d+\.\d+\.\d+ - \\\\Remote$ ahk_class Transparent Windows Client
!n::SendInput {F2}+{F10}a
!j::SendInput +{Tab}+{Tab}+{Tab}

#IfWinActive ^Job \d+ Overview\*? - \\\\Remote$ ahk_class Transparent Windows Client
!g::SendInput {Tab}{Tab}{Tab}{Tab}{Tab}{Tab}{Tab}{Tab}H{Tab}+{F10}a0,5+{Tab}+{Tab}+{Tab}+{Tab}+{Tab}+{Tab}{Space}Weibull{Enter}!o
!w::SendInput !sWeibull{Enter}!o
Escape::!c
Enter::!o

#IfWinActive ^Customer|Employee$ ahk_class Transparent Window Client
Escape::WinClose

#IfWinActive ^SDL Trados TagEditor
^w::SendInput ^{F4}

^+w::TagEditorCloseAll()

^n::PostMessage %WM_COMMAND%, 32879

^p::PostMessage %WM_COMMAND%, 32880

^l::PostMessage %WM_COMMAND%, 32875

F3::
SendInput ^f
WinWaitActive Find
SendInput {Enter}{Escape}
return

F8::TagEditorVerify()

#IfWinActive ^TranslatorTool
^u::
InputBox nRows, Rows to Copy, How many rows do you want to copy to memory?
if ErrorLevel
  return
Loop %nRows% {
  SendInput !4
  Sleep 100
  SendInput {Down}
}
return

#IfWinActive ahk_class TTOTAL_CMD
!PgUp::SendInput {Home}
!PgDn::SendInput {End}
+!PgUp::SendInput +{Home}
+!PgDn::SendInput +{End}

#IfWinActive ahk_class TDLGZIP
!h::ControlFocus TEdit1

#IfWinActive ahk_class TDLGUNZIPALL
!h::ControlFocus TEdit1

#IfWinActive ahk_class TDLGUNZIP
!h::ControlFocus TEdit1

#IfWinActive ahk_class TNewConnDlg
!c::ControlFocus Edit1

#IfWinActive ahk_class TInpComboDlg
!f::ControlFocus TEdit1
!r::ControlFocus Edit1

#IfWinActive ahk_class TCheckEditBox
!f::ControlFocus TEdit1

#IfWinActive ahk_class tdb_wndw_cls_edord
!l::ControlFocus SysListView321

#IfWinActive ahk_class IsoDraw5Class
/::
WinMenuSelectItem, , , Macros, Search
WinWait Macro Input:
WinWaitActive ahk_class IsoDraw5Class
WinMenuSelectItem, , , Window, Size, Full page
WinMenuSelectItem, , , Window, Size, 200 `%
return

^/::
WinMenuSelectItem, , , Macros, Search-Exact
WinWait Macro Input:
WinWaitActive ahk_class IsoDraw5Class
WinMenuSelectItem, , , Window, Size, Full page
WinMenuSelectItem, , , Window, Size, 200 `%
return

^r::
WinMenuSelectItem, , , Macros, Initialize-Window
Saved_KeyDelay := A_KeyDelay
SetKeyDelay 100
SendEvent !-x
SetKeyDelay %Saved_KeyDelay%
return

#IfWinActive

#w::PostMessage 0x112, 0xf060, , , A

^k::
if WinActive("ahk_class Emacs") or WinActive("ahk_class mintty") or WinActive("ahk_class Vim") or WinActive("ahk_class PuTTY")
  Dg := "kk"
else
  Input Dg, L2 C
if (Dg = "kk") {
  Hotkey ^k, Off
  SendInput ^k
  Hotkey ^k, On
  return
}
digraph := digraphs.item[Dg]
if (digraph = "") {
  Dg := SubStr(Dg, 2, 1) . SubStr(Dg, 1, 1)
  digraph := digraphs.item[Dg]
  if (digraph = "")
    return
}
SendInput %digraph%
return

WheelUp::Goto Scroll
WheelDown::Goto Scroll
WheelLeft::Goto Scroll
WheelRight::Goto Scroll

Scroll:
  t := A_TimeSincePriorHotkey
  if (A_PriorHotkey = A_ThisHotkey && t < ScrollTimeout) {
    scrollDistance++
    v := (t < 80 && t > 1) ? (250.0 / t) - 1 : 1
    if (ScrollBoost > 1 && scrollDistance > ScrollBoost) {
      if (v > scrollVMax)
        scrollVMax := v
      else
        v := scrollVMax
      v *= scrollDistance / ScrollBoost
    }
    v := (v > 1) ? ((v > ScrollLimit) ? ScrollLimit : Floor(v)) : 1
    MouseClick, %A_ThisHotkey%, , , v
  } else {
    scrollDistance := 0
    scrollVMax := 1
    MouseClick %A_ThisHotkey%
  }
  return
