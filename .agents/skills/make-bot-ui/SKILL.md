---
name: make-bot-ui
description: "Design and verify a UI that submits bounded jobs to an available Codex or Claude runner."
disable-model-invocation: true
---

# Make a bot UI

This port uses a user-selected Codex or Claude runner. Neither CLI is a webhook service. Read [the host adapter](../poteto-mode/references/host-adapter.md) for supported execution and authorization.

1. Identify the user action, job schema, observable completion state, and available runner. Prefer an existing Midgard service. If none exists, build a concrete local prototype within the requested scope; publishing and scheduling follow the user's actual authorization. [review]
2. Design a server-side boundary with authenticated requests, a small allowlisted action set, bounded concurrency and runtime, job IDs, and persistent completion/error state where recovery requires it. Treat request text as input data rather than tool instructions. [review]
3. Keep credentials in the runner's existing credential mechanism or server environment. The browser sends only the bounded action and parameters. Keep secrets out of chat, prompts, logs, and source control. [review]
4. Execute provider CLIs using argument arrays and stdin, with the existing permission policy. The bundled dispatcher supports read-only jobs; writable jobs require a separately reviewed execution boundary. [review]
5. Bind a local prototype to loopback. Expose it through the user's configured hosting or network only when requested. Verify authentication, rejected actions, timeout/error reporting, duplicate submission semantics, and a harmless successful job through the UI. [review]
6. Show the verified local result and any deployment work still required. Link actual artifacts and report exact checks. [review]
