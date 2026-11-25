# Naming and versioning

Name each file for its purpose: a reader should be able to infer what it does
or contains from its path, without opening it. Use specific domain terms,
such as `transaction-finalization.ts` or `deployment-authority-fixture.ts`.
Group directories by concern and follow the language's naming conventions.

Keep names focused on purpose. Ticket, goal and ruling labels go in commit
messages and decision records, never in names, comments, test titles or
messages: state the rule itself. Put versions in wire, storage, or
manifest values. Qualify names when the distinction matters,
such as demo or test variants. Preserve external names and pinned evidence paths.
[review]
