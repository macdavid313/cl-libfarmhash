# cl-libfarmhash - Common Lisp Binding for Google's [Farmhash](https://github.com/google/farmhash).

<a href="http://quickdocs.org/cl-libfarmhash"><img src="http://quickdocs.org/badge/cl-libfarmhash.svg" /></a>

## Usage

**note:** _all results shown below are from a 64bit Mac OS platform._

### farmhash

```common-lisp
FARM> (farmhash "test")
2929758365
```

### farmhash32

```common-lisp
FARM> (farmhash32 "test")
168770635
```

### farmhash32-with-seed

```common-lisp
FARM> (farmhash32-with-seed "test" 12345)
687740529
```

### farmhash64

```common-lisp
FARM> (farmhash64 "test")
656818571139125405
```

### farmhash64-with-seed

```common-lisp
FARM> (farmhash64-with-seed "test" 12345)
1329645378812687902
```

### farmhash64-with-seeds

```common-lisp
FARM> (farmhash64-with-seeds "test" 12345 54321)
2807312541950536949
```

### farmhash128

```common-lisp
FARM> (farmhash128 "test")
278051559519782271719215074689603618262
```

### farmhash128-with-seed

```common-lisp
FARM> (farmhash128-with-seed "test" 1234554321)
8373479877626909324346027983011427843
```

### farmhash-fingerprint32

```common-lisp
FARM> (farmhash-fingerprint32 "test")
1633095781
```

### farmhash-fingerprint64

```common-lisp
FARM> (farmhash-fingerprint64 "test")
8581389452482819506
```

### farmhash-fingerprint128

```common-lisp
FARM> (farmhash-fingerprint128 "test")
334882099032867325754781607143811124132
```

## Installation

Just `(ql:quickload :cl-libfarmhash)`. All exported functions has been simply tested on `SBCL`, `CCL`, `Allegro CL` and `LispWorks`.

## Author

* David Gu (david_guru@gty.org.in)

## Copyright

Copyright (c) 2016 David Gu (david_guru@gty.org.in)
