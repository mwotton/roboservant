# Repository Guidelines

## Project Structure & Module Organization
- Library sources live in `src/` with modules under the `Roboservant` namespace (e.g., `src/Roboservant/Types`).
- Tests reside in `test/`, including regression fixtures such as `test/Records.hs` and the main spec entry point `test/Spec.hs`.
- Metadata and configuration files are in the repository root: `roboservant.cabal`, `package.yaml`, and `stack.yaml` control builds; `Example.lhs` hosts the runnable tutorial.

## Build, Test, and Development Commands
- `stack build` compiles the library and executables against the resolver specified in `stack.yaml`.
- `stack test` runs the full Hspec suite under `test/Spec.hs`; expect long runs on first invocation while Stack builds dependencies.
- `stack exec -- example` executes the literate integration example defined in `Example.lhs`.
- Keep CI pinned to the GHC releases marked “suitable for use” on [the GHC status page](https://gitlab.haskell.org/ghc/ghc/-/wikis/GHC%20Status#all-released-ghc-versions).

## Coding Style & Naming Conventions
- Follow the existing Haskell style: two-space indentation, record fields in camelCase (e.g., `complexRecordServer`), and module names in `Camel.Case`.
- Prefer deriving strategies (`deriving stock`) and explicit language pragmas at the top of each file.
- Keep imports qualified when working with large modules to avoid name clashes (see `test/Spec.hs` for patterns).

## Testing Guidelines
- Tests use Hspec; group related fuzz suites with `describe` blocks and name scenarios with clear expectations ("finds no error…").
- Regression fixtures live as dedicated modules in `test/` and should expose typed Servant APIs plus both good/bad servers when applicable.
- Always run `stack test` before submitting changes; add seeds when fuzz cases require deterministic reproducers.

## Commit & Pull Request Guidelines
- Commit messages typically follow "verb phrase" style (e.g., `notice of hiatus`, `bump version 1.0.3`). Keep them short and present-tense.
- Pull requests should describe the API surface touched, new tests, and any required seeds. **When creating PR descriptions, use a shell heredoc or write to a temporary file**; direct multi-line arguments introduce encoded newline characters in the tooling.
- Link related issues where possible and note any manual verification steps (`stack test`, `stack exec`).

