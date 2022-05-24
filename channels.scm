(cons* (channel
	(name 'nonguix)
	(url "https://gitlab.com/nonguix/nonguix")
	(introduction
	 (make-channel-introduction
	  "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
	  (openpgp-fingerprint
	   "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
       (channel
	(name 'nonnonguix)
	(url "https://github.com/now/nonnonguix")
	(introduction
	 (make-channel-introduction
	  "ab424cccd448529304724478ed29600a888bb3d8"
	  (openpgp-fingerprint
	   "2600 B3BF 5789 590A F785  ED0E 5332 3FCA D4A1 3EDE"))))
       %default-channels)
