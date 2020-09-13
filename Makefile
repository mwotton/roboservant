testwatch:
	ghcid -c 'stack repl --test --ghc-options=-fobject-code roboservant' --allow-eval --restart="stack.yaml" --restart="package.yaml"  -W
