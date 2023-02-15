## Reminders
  * The "offset" in the various `writeXOffAddr` primitives in `GHC.Exts` is an
    *index*, not a byte count.

## Overheads
### Plain
  * 2023-02-15: From a core glance on a manual Put instance: Hardly anything
    dude. Just pattern matching and primitives. Fantastic.

### Generics
  * 2023-02-15: From a core glance on a Generic Put instance: Yes. Sum types in
    particular incur overhead. Don't know how to avoid that. Didn't seem bad
    though. Particularly curious how it scales for big ol' sum types.
