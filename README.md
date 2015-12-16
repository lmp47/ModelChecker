# ModelChecker

    git clone https://github.com/lmp47/ModelChecker.git
    cd ModelChecker
    git submodule init
    git submodule update
    rm Parser/AigerTools.hsc
    cabal sandbox init
    cabal configure --enable-tests
    cabal build
    cabal test
