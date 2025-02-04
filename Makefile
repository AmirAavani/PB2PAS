MyUnitPath=../modules/my-units

all: clean
	mkdir -p /tmp/PB2PAS
	fpc -O3 -g -gl PB2PAS.lpr -Fumodules/* -FuParamManager -Fu. -Fu./*  -FU/tmp/PB2PAS -oPB2PAS

clean:
	rm -Rf /tmp/PB2PAS/
