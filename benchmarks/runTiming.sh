lo=$1
hi=$2
k=$3
for i in $(seq $lo 5 $hi); do
    ./genFile.sh $i $k > main.hs;
    echo "sievedn: $i,";
    /usr/bin/time -f "%e" stack ghc  main.hs 2>&1;
#    /usr/bin/time -f "%e" stack ghc -- -fomit-interface-pragmas main.hs 2>&1;

done | grep -v Compiling | grep -v Linking   | awk '/,$/ { printf("%s", $0); next } 1' | sed 's/sievedn: //'
