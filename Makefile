testwatch:
	ghcid -c 'stack repl --test --ghc-options=-fobject-code' --allow-eval --restart="stack.yaml" --restart="package.yaml"
