MyUnitPath=modules/my-units
PB2PAS:
	mkdir -p /tmp/PB2PAS 
	fpc -O3 -g -gl PB2PAS.lpr -Fumodules/* -FuParamManager -Fu. -Fu./*  -FU/tmp/PB2PAS -oPB2PAS

ZioDump: 
	mkdir -p /tmp/PB2PAS /tmp/ZioDump
	fpc -O3 -g -gl ZioDump.lpr -Fumodules/* -FuParamManager -Fu. -Fu./*  -FU/tmp/ZioDump -oZioDump

all: ZioDump PB2PAS


clean:
	rm -Rf /tmp/PB2PAS/
