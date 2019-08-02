echo "rm -rf src/.liquid"
rm -rf src/.liquid
echo "use newtype"
rm ./src/Repro2.hs
cp ./Repro2-has-newtype.hs ./src/Repro2.hs
sleep .5
cat ./src/Repro2.hs
echo "stack exec liquid -- src/Repro2.hs"
stack exec liquid -- src/Repro2.hs
echo "remove newtype"
rm ./src/Repro2.hs
cp ./Repro2-no-newtype.hs ./src/Repro2.hs
sleep .5
cat ./src/Repro2.hs
echo "stack exec liquid -- src/Repro2.hs"
stack exec liquid -- src/Repro2.hs
echo "stack exec liquid -- src/Repro2.hs --full"
stack exec liquid -- src/Repro2.hs --full
