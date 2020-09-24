testwatch:
	ghcid -T main -c 'stack repl --test --ghc-options=-fobject-code roboservant --ghc-options="+RTS -N4"' --allow-eval --restart="stack.yaml" --restart="package.yaml"  -W
