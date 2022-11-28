## Haskell Comprehension

Haskell Comprehension is a tool to inspect/verify/debug the interaction your haskell source code has with the GHC Core2Core optimization pipeline.
It uses a plugin to make a snapshot of each intermediate AST which can interactively be explored in a browser using the frontend app.
The frontend app is also hosted at http://core.hugopeters.me and contains some example captures to pick from.

## Requirements

* GHC >= 8.4
* zlib (apt: `zlib1g-dev`)
* pygmentize (apt: `python3-pygments`)

## Using the plugin & Creating dumps

The project is currently not on hackage so you need to add this repo as a local repo

- :package: Clone this repository to wherever you want and update the submodules
  ```sh
  git clone --recurse-submodules https://github.com/HugoPeters1024/hs-comprehension 
  ```
- Create or edit a `cabal.project` file for your project to transitively include all upstream dependencies (assuming the clone location is at `..`): 
    ```cabal
    packages: .
            , ../hs-comprehension/plugin
            , ../hs-comprehension/ghc-dump
    ```
- :electric_plug: Register the dependency and enable the plugin in your cabal file, optionally use the `-fplugin-opt` flag to give a name to your capture.
    ```cabal
    build-depends:    base ^>=4.16.0.0
                    , hs-comprehension-plugin
                    , ...

    ...

    ghc-options: -fplugin HsComprehension.Plugin
    ```

- :floppy_disk: Now whenever you build dump files are created in `dist-newstyle/coredump-MyCapture001/`

- To create a zip archive you can run `cabal run hs-comprehension-zip`.

# Inspecting dumps

- :female_detective: To inspect the dumps with the frontend got to http://core.hugopeters.me. Note that dump files are exclusively read locally and are not send over the network.

- :hammer: Alternatively you build the frontend yourself by running the `make_release.sh` script. This script requires a few dependencies.
    1. [Elm](https://guide.elm-lang.org/install/elm.html)
    2. [uglify-js](https://www.npmjs.com/package/uglify-js) (which transitively requires npm)

  This creates the `release` directory with the required files, most notably an index html. This file will not work when opened directoy in a browser but has to be served over http. The fastest way to do this is by running `python3 -m http.server` in the release directory. I don't think this is recommend for production applications that actually face the internet but on localhost it is fine.

## How it works

### Capture

1. The plugin intersperses the core2core pipeline with snapshot phases.
2. Each snapshot phase ensures that all binders are a unique identifier for that given module. This means that the plugin is not entirely noninvasive. Therefore, whilst the result should be exactly the same, I would not recommend using plugin while building for a release.
3. The phases are then converted to a version agnostic representation
3. The phase snapshots are then embellished with a some analysis:
    - Definition Analysis, in which phase has this binder been observed for the first time

### Exploration

The frontend fetches the data from the zip on a per module and per phase basis.

## Current Capabilities

- GHC >= 8.4 support
- Haskell like Core pretty printer
- Easy phase scrolling
- Side by side view of 2 or more captures
- Variable highlighting for easy scope exploration
- Variable renaming (generated names are often not informative)
- Click to jump to phase of first occurrence
- Click to query hoogle
- Inspect which rewrite rules have fired in which order at each phase.
- Extract the changes from two captures


## Possible Future Capabilities/Goals

- <del>Capture the dflags used to configure the compiler</del> Done
- Productively explore the unfolding of a variable (it is currently not clear what that means)
    - We need a use case/example for this feature
    - `HsComprehension/Cvt.hs` has the unfolding currently disabled to save space and improve performance
- <del>For the comparison of captures, hide toplevel definitions that are identical. A feature that could possible utilize an alpha-equivalent hashing scheme: https://simon.peytonjones.org/hashing-modulo-alpha/</del> Done
- <del>Qualify module names to reduce syntactical noise.</del> Done
- Parse the strictness analysis of binders to present it more clearly

## Identified Limitations

- Getting more detailed information about rewrite rule firings (such as which ones are considered etc.) will require changes to GHC.
