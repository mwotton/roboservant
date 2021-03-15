testwatch:
	ghcid -T main -c 'stack repl roboservant:lib roboservant:test:roboservant-test --test --ghc-options=-fobject-code' --allow-eval --restart="stack.yaml" --restart="package.yaml"  -W
pedanticwatch:
	ghcid -c 'stack repl --test --ghc-options=-fobject-code' --allow-eval --restart="stack.yaml" --restart="package.yaml"  -W
