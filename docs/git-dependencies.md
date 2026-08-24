# Git Dependencies

This fork supports **private packages** fetched directly from git
repositories, referenced by any URL that `git clone` understands
(`git@gitlab.example.com:org/project.git`, `https://...`, `file:///...`).

Authentication is delegated entirely to git, so SSH keys, ssh-agent, and
credential helpers all work as usual.

## Usage

Add a top-level `"git-dependencies"` field to `elm.json`, mapping a package
name to the git URL. The package must **also** appear in the normal
dependency fields, exactly like a registry package:

```json
{
    "type": "application",
    "source-directories": [ "src" ],
    "elm-version": "0.19.2",
    "dependencies": {
        "direct": {
            "elm/core": "1.0.5",
            "webbhuset/elm-promise": "1.2.0"
        },
        "indirect": { "elm/json": "1.1.3" }
    },
    "test-dependencies": { "direct": {}, "indirect": {} },
    "git-dependencies": {
        "webbhuset/elm-promise": "git@gitlab.webbhuset.com:webbhuset/internal/frontend/elm-promise.git"
    }
}
```

Package projects work the same way, with the usual constraint in
`"dependencies"`:

```json
    "dependencies": {
        "elm/core": "1.0.0 <= v < 2.0.0",
        "webbhuset/elm-promise": "1.2.0 <= v < 2.0.0"
    },
    "git-dependencies": {
        "webbhuset/elm-promise": "git@gitlab.webbhuset.com:webbhuset/internal/frontend/elm-promise.git"
    }
```

`elm install webbhuset/elm-promise` also works once the package is listed
in `"git-dependencies"`.

## How versions work

Versions are **git tags** named exactly like Elm versions: `1.0.0`,
`2.1.3`, etc. (no `v` prefix). Tag a release in the private repository and
reference that version in `elm.json`.

- For **applications**, versions are pinned in `"dependencies"`, so no
  network access is needed once the package is in the local cache.
- For **package projects** and `elm install`, available versions are
  discovered with `git ls-remote --tags`.

Sources are cloned once into the shared package cache
(`~/.elm/0.19.2/packages/author/project/version/`) with
`git clone --depth 1 --branch <version>`. A `git-url` file in that
directory records where the sources came from; if a project maps the same
package name and version to a *different* URL, the build stops with an
error instead of silently reusing the wrong sources.

Since the cache key is `(name, version)`, tags should be treated as
immutable. If you move a tag, delete the corresponding directory from
`~/.elm` (and the package's `artifacts.dat`) to force a fresh clone.

## Kernel code in git dependencies

Packages fetched through `"git-dependencies"` are **trusted like the
`elm/*` packages**: they may define `Elm.Kernel.*` modules (JavaScript
files under `src/Elm/Kernel/`), effect modules, and custom infix
operators. This is intended for private packages that need native
JavaScript, e.g. server modules running on Node.js.

Notes:

- Kernel module *short names* are global: `Elm.Kernel.Foo` in two
  different packages generates colliding `_Foo_*` functions. Prefix your
  kernel module names (e.g. `Elm.Kernel.WhServer`) to stay clear of the
  names used by `elm/*` packages.
- Kernel code only takes effect when the package is *consumed from the
  package cache* (the normal case). Running `elm make` inside the
  kernel package itself does not include the JavaScript — develop against
  a test application that depends on the package.

## Compatibility

- The `"git-dependencies"` field is ignored by the official compiler and
  by tools using the official parser (elm-format, editors, elm-test), so
  an `elm.json` using it remains structurally valid everywhere. Builds
  with the official compiler will fail to *resolve* the private packages,
  of course — everyone building the project should use this fork.
- Projects without `"git-dependencies"` behave byte-for-byte like the
  official compiler; the field is never written unless present.
- `elm publish` rejects packages that have `"git-dependencies"`, since
  published packages must be resolvable from the public registry.
