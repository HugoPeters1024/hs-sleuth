## Haskell Comprehension

Haskell Comprehension is a tool to inspect/verify/debug the interaction your haskell source code has with the GHC Core2Core optimization pipeline. It uses a plugin to make a snapshot of each intermediate AST which can interactively be explored in a browser using the frontend app.

## Requirements

* GHC >= 8.4
* zlib (`sudo apt install zlib1g-dev`)

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
            , ../hs-comprehension/cborg/cborg
            , ../hs-comprehension/ghc-dump
    ```
- :electric_plug: Register the dependency and enable the plugin in your cabal file, use the `-fplugin-opt` flag to give a name to your capture.
    ```cabal
    build-depends:    base ^>=4.16.0.0
                    , hs-comprehension-plugin
                    , ...

    ...

    ghc-options: -fplugin HsComprehension.Plugin -fplugin-opt HsComprehension.Plugin:MyCapture001
    ```

- :floppy_disk: Now whenever you build dump files are created in `dist-newstyle/coredump-MyCapture001/`


# Inspecting dumps


- :female_detective: To inspect the dumps with the frontend, you need to run the server:
    ```sh
    -- when building the server yourself (GHC 9.2+)
    -- Make sure to install Elm: https://guide.elm-lang.org/install/elm.html
    -- You can also opt to not install elm and extract the html and css files from the release
    cd hs-comprehension/
    ./make_release.sh
    cd release/
    ./server --project-root PATH/TO/YOUR/PROJECT


    -- when using the released build (https://github.com/HugoPeters1024/hs-comprehension/releases)
    ./server --project-root PATH/TO/YOUR/PROJECT

    -- the server will serve at port 8080
    ```

That should be all!

## How it works

### Capture
1. The plugin intersperses the core2core pipeline with snapshot phases.
2. Each snapshot phase ensures that all binders are a unique identifier for that given module. This means that the plugin is not entirely noninvasive. Therefore, whilst the result should be exactly the same, I would not recommend using plugin while building for a release.
3. The phases are then converted to a version agnostic representation
3. The phase snapshots are then embellished with a some analysis:
    - Definition Analysis, in which phase has this binder been observed for the first time

### Exploration
1. The server binary acts as a json api interface to the made captures
2. The frontend fetches the data from the server per module on a need to know basis.

## Current Capabilities

- GHC >= 8.4 support
- Core pretty printer
- Phase scrolling
- Side by side view of 2 or more captures
- Variable highlighting for easy scope exploration
- Variable renaming (generated names are often not informative)
- Click to jump to phase of first occurrence
- Inspect which rewrite rules have fired in which order at each phase.


## Possible Future Capabilities/Goals

- Capture the dflags used to configure the compiler
- Productively explore the unfolding of a variable (it is currently not clear what that means)
    - We need a use case/example for this feature
- For the comparison of captures, hide toplevel definitions that are identical. A feature that could possible utilize an alpha-equivalent hashing scheme: https://simon.peytonjones.org/hashing-modulo-alpha/
- A stress tested frontend with enormous modules along with an answer to the question if it is possible to provide a degraded feature set instead of none.
- Click to go the module (for external names in the same project)
- Qualify module names to reduce syntactical noise. 
- Parse the strictness analysis of binders to present it more clearly
- Trigger recompilation/recapture from the frontend whilst allowing changing compiler flags
    - Can this be done on a per module basis?

## Identified Limitations

- Getting more detailed information about rewrite rule firings (such as which ones are considered etc.) will require changes to GHC.


## For the Final Thesis

- What the contributions (making a tool to support haskell developers), and how can we verify that (ACM Emperical Evaluation Checklist?)
    - Also conversations with the industry, Well Typed, Channable, Chordify?






