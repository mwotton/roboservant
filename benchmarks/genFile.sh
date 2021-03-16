n=$1
k=$2

cat prelude
echo -n "type SubApi = \"route1\" :> ReqBody '[JSON] Input :> Post '[JSON] Output"
for i in $(seq 2 $n); do
    echo -n "  :<|> \"route${i}\" :> "
    echo "ReqBody '[JSON] Input :> Post '[JSON] Output"
done

echo -n "server = handler "

for i in $(seq 2 $n); do
    echo -n ":<|> handler "
done
echo
echo

echo -n "type Api = Flatten ("
for i in $(seq 1 $k); do
    echo -n '"foo" :> '
done;

echo "SubApi )"


