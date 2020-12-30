n=$1

cat prelude
echo -n "type Api = \"route1\" :> ReqBody '[JSON] Input :> Post '[JSON] Output"
for i in $(seq 2 $n); do
    echo "  :<|> \"route${i}\" :> ReqBody '[JSON] Input :> Post '[JSON] Output"
done

echo -n "server = handler "

for i in $(seq 2 $n); do
    echo -n ":<|> handler "
done
