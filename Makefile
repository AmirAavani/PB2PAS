MyUnitPath=modules/my-units
all: clean
	mkdir -p /tmp/PB2PAS /tmp/ZioDump
	fpc -O3 -g -gl PB2PAS.lpr -Fumodules/* -FuParamManager -Fu. -Fu./*  -FU/tmp/PB2PAS -oPB2PAS
	fpc -O3 -g -gl ZioDump.lpr -Fumodules/* -FuParamManager -Fu. -Fu./*  -FU/tmp/ZioDump -oZioDump

clean:
	rm -Rf /tmp/PB2PAS/
