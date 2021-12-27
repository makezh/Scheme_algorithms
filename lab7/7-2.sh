countLines(){
  TOTAL=0
  for file in `find "$1" -name "*.c" -o -name "*.h"`
  do
    numLines=$(grep -cve '^\s*$' $file)
    TOTAL=$(($TOTAL + $numLines))
    echo $file $numLines
  done
  echo "Количество непустых строк:" $TOTAL
}

countLines "$1"