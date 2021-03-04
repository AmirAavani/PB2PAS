# PB2PAS
PB2PAS is a protocol buffer compiler, that compiles protobufs to Pascal. This project is under development.

I am making my contributions/submissions to this project solely in my personal capacity and am not conveying any rights to any intellectual property of any third parties.

# Usage

1) Obtain a copy of the repo and run 
    git submodule init
    git submodule update

2) Build __PB2PAS__
    > fpc -Sd PB2PAS.lpr  -Fumodules/PB2PAS -Fumodules/General-Purpose-Units -Fumodules/ALogger -Fumodules/Unicode-Functions -Fumodules/Threading

3) Running __PB2PAS__
    > ls Samples/

    > Main.pp  test_dep1.proto  test.proto 

    > ./PB2PAS --InputFile Samples/test.proto

    > ls Samples/

    > Main.pp  test_dep1.proto  TestDep1Unit.pp  test.proto  TestUnit.pp 

    > fpc -Sd Samples/Main.pp -Fumodules/PB2PAS
    > ./Samples/Main
