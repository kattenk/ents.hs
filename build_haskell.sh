cd haskell

echo 'Make sure to run `cabal update`'
echo $'Building project...\n'
cabal build -O2 -j \
  --with-compiler=wasm32-wasi-ghc \
  --with-hc-pkg=wasm32-wasi-ghc-pkg \
  --with-hsc2hs=wasm32-wasi-hsc2hs \
  --allow-newer="*,base,time" \
  --enable-shared \
  --constraint="h-raylib -detect-platform +platform-web" \
  --flags="web"
if [ $? -ne 0 ]; then
  echo "Error: Project build failed"
  exit 1
fi
echo $'\nBuilt successfully!\n'

echo "Moving project binary..."
mkdir -p ../public
rm -f ../public/haskell.wasm
# cp ./dist-newstyle/build/wasm32-wasi/ghc-*/haskell-0.1.0.0/x/haskell/build/haskell/haskell.wasm ../public/haskell.wasm
cp ./dist-newstyle/build/wasm32-wasi/ghc-*/haskell-0.1.0.0/x/haskell/opt/build/haskell/haskell.wasm ../public/haskell.wasm
echo "Binary moved successfully!"
