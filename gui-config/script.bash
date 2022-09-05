for file in ./*.lisp
do

  str1="$(cat insert.txt)"
  str2="$(cat $file)"


  printf "$str1 \n\n$str2" > file

done
