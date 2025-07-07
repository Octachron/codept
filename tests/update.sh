CODEPT="../_build/default/full/codept.exe -nested -deps -no-alias-deps -expand-deps -k -format simple-json"

for i in cases/*.ml
do
   echo ${i/ml/ref}
  $CODEPT $i &> ${i/ml/ref}
done

cd complex
CODEPT=../$CODEPT
echo "$CODEPT"
for i in *
do
  echo $i
  $CODEPT $i &> $i/reference
done

