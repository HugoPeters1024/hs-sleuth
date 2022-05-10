### Postgen hotfix

Unfortunately, the generator is not perfect, these changes are to be done by hand:

- Add a `localBinder : () -> BinderThunk` field to `ExternalName`
- Append a field `() -> BinderThunk` to `BinderId`
- Add a default `_ -> Untouched` closure as defaults in the decoder for the 2 instances above
- Add `import Generated.Types exposing (..)` to the decoder
