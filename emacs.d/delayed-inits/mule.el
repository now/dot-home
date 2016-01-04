(quail-define-package
 "swedish-postfix+rfc1345" "UTF-8" "SV<+m" t
 "Swedish (Svenska) input method: AA → Å, AE → Ä, OE → Ö, E' → É.

Doubling the postfix separates the letter and postfix, for
example, aee → ae.

Also includes parts of RFC1345 mnemonics, for example, &a' → á."
 nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("AA" ?Å)
 ("aa" ?å)
 ("AE" ?Ä)
 ("ae" ?ä)
 ("OE" ?Ö)
 ("oe" ?ö)
 ("E'" ?É)
 ("e'" ?é)

 ("AAA" ["AA"])
 ("aaa" ["aa"])
 ("AEE" ["AE"])
 ("aee" ["ae"])
 ("OEE" ["OE"])
 ("oee" ["oe"])
 ("E''" ["E'"])
 ("e''" ["e'"])
 ("e'9" ["e’"])

 ("(C)" ?\©)
 ("^TM" ?\™)

 (":-)" ?\☺)
; (":-D" ?\😃)
; (";-)" ?\😉)
; (":-|" ?\😐)
 (":-(" ?\☹)
 ("&<3" ?♡)

 ("&->" ?\→)
 ("&cb" ?\•)
 ("&SE" ?\§)
 ("&<1" ?\‹)
 ("&>1" ?\›)
 ("\"6" ?\“)
 ("\"9" ?\”)
 ("'6" ?\‘)
 ("'9" ?\’)
 ("&0S" ?\⁰)
 ("&1S" ?\¹)
 ("&2S" ?\²)
 ("&3S" ?\³)
 ("&4S" ?\⁴)
 ("&5S" ?\⁵)
 ("&6S" ?\⁶)
 ("&7S" ?\⁷)
 ("&8S" ?\⁸)
 ("&9S" ?\⁹)
 ("&0s" ?\₀)
 ("&+S" ?\⁺)
 ("&1s" ?\₁)
 ("&2s" ?\₂)
 ("&3s" ?\₃)
 ("&4s" ?\₄)
 ("&5s" ?\₅)
 ("&6s" ?\₆)
 ("&7s" ?\₇)
 ("&8s" ?\₈)
 ("&9s" ?\₉)
 ("&**" ?\×)
; ... &-> …
 )

(setq default-input-method "swedish-postfix+rfc1345")
