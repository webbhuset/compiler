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

`elm init` generates a project already set up this way: the patched forks
are pinned to their fork versions with `"git-dependencies"` entries —
elm/core from `git@github.com:webbhuset/core.git`, elm/browser from
`git@github.com:webbhuset/elm-browser.git` and elm/virtual-dom from
`git@github.com:webbhuset/virtual-dom.git` — so task ports, comparable
newtypes, `Css.vars`, and `Browser.Worker` work out of the box in new
projects.

Those three repositories are public, but the URLs are written in the SSH
form, and it matters which form a project uses: the cache records the URL
it cloned a version from and rejects a project that maps the same package
and version to a different one, the `https://` form of the same
repository included. Stick to the URLs `elm init` writes.

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

### Numbering a fork of a published package

A fork keeps the upstream *name*, so its versions share one namespace —
and one cache directory — with the versions the upstream author
publishes. Taking the next number up (elm/core `1.0.6` on top of the
published `1.0.5`) collides the day upstream publishes it: the git
dependency then stops with a cache-origin error, and a project that
resolves the same version from the registry silently reuses the fork's
sources instead. So forks are numbered in a range upstream will never
reach:

    major = upstream major
    minor = 100 + upstream minor
    patch = 100 * upstream patch + fork revision

elm/core `1.0.5` patched three times is `1.100.503`; a fourth revision
is `1.100.504`. Rebasing those patches onto an upstream `1.0.6` gives
`1.100.601`, and onto an upstream `1.1.0` gives `1.101.1` — the patch
number is arithmetic, never zero-padded, since a version field may not
have a leading zero. Fork revision 0 means pristine upstream sources.

The major must stay the upstream major, because published packages
constrain their dependencies as `1.0.0 <= v < 2.0.0` and the fork has to
satisfy that. Everything below the major is free: for a package listed in
`"git-dependencies"` the solver replaces the registry's version list
outright, so fork numbers are never compared with published ones. Version
fields are 16-bit, so the scheme holds until upstream reaches patch 655.

The forks this compiler expects (the versions `elm init` pins):

| package | upstream | fork | repository | patches |
| --- | --- | --- | --- | --- |
| elm/core | 1.0.5 | 1.100.503 | `webbhuset/core` | comparable newtypes, task ports, `Task.await`, `widen` |
| elm/browser | 1.0.2 | 1.100.201 | `webbhuset/elm-browser` | `Browser.Worker` |
| elm/virtual-dom | 1.0.5 | 1.100.502 | `webbhuset/virtual-dom` | custom properties, HTML to string |

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
