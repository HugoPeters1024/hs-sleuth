## Haskell Comprehension

This a newer version that restarted with Ben's ghc-dump as a basis. The previous version is still available under the branch 'old'

## Requirements

* GHC 9.2
* Cabal 3.16

## Quickstart

The project is currently not on hackage so you need to add this repo as a local repo

- Clone this repository to wherever you want (lets assume `..`) and update the submodules
  ```sh
  git clone --recurse-submodules https://github.com/HugoPeters1024/hs-comprehension 
  ```
- Create or edit a `cabal.project` file for your project: 
    ```sh
    packages: .
            , ../hs-comprehension-plugin  #or wherever else you cloned it
    ```
- Register the dependency and enable the plugin in your cabal file
    ```sh
    build-depends:    base ^>=4.16.0.0
                    , hs-comprehension-plugin
                    , ...

    ...

    ghc-options: -fplugin HsComprehension.Plugin
    ```

- Now whenever you build dump files are created in `dist-newstyle/coredump`


- To inspect the dumps with the frontend, you need to run the server:
    ```sh
    cabal run hs-comprehension-server
    ```

- Then open the frontend html in `frontend/index.html`

That should be all!



