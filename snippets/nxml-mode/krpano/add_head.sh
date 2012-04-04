
FILES=$(echo *)

for file in $FILES
do
    echo "Editing file : $file"

    sed -i 1d $file

done
