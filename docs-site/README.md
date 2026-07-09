# Midgard Documentation Site

Documentation for [Midgard](https://github.com/Anastasia-Labs/midgard), Cardano's
first optimistic rollup protocol. Built with [Fumadocs](https://fumadocs.dev) on
Next.js.

Covers the TypeScript SDK (`lucid-midgard`, `midgard-core`, `midgard-sdk`), the
validation engine, running an operator node, watchers and data availability, fault
proofs, the on-chain Aiken/Plutarch validators, and the protocol specification.

## Develop

```sh
pnpm install
pnpm dev      # http://localhost:3000
```

## Build

```sh
pnpm build    # builds the linked SDK packages, then next build
pnpm start
```

`prebuild` builds `demo/midgard-core` and `demo/lucid-midgard` first. Their `dist/`
directories are gitignored, and the type-checked code samples resolve against them, so
the build cannot succeed without them.

```sh
pnpm types:check   # fumadocs-mdx + next typegen + tsc --noEmit
```

## Structure

- `content/docs/**`: MDX documentation. Each folder has a `meta.json` sidebar.
- `app/`: Next.js app router (docs layout, landing page, search route).
- `lib/`: Fumadocs source loader and shared layout options.
- `components/`: MDX components (Mermaid diagrams, cards, tabs, steps).
- `source.config.ts`: Fumadocs MDX collection, code highlighting, and the twoslash
  transformer.

Deployment is not wired here; the app deploys as a standard Next.js project (e.g.
Vercel with the `docs-site/` root), see the main repo for hosting decisions.

## Writing docs

### Code samples are type-checked

Mark every TypeScript sample that uses a Midgard package with `ts twoslash`:

````md
```ts twoslash
import type { CompleteTx } from "@al-ft/lucid-midgard";
declare const tx: CompleteTx;
// ---cut---
const signed = await tx.sign.withWallet().complete();
```
````

The build fails if a sample names an export or method that does not exist. Use
`declare const` plus `// ---cut---` to set up context without printing it. A sample that
cannot be type-checked is a sample that will be wrong within two releases.

The homepage sample lives in a JSX string literal that twoslash cannot reach, so
`app/(home)/homepage-sample.check.ts` type-checks the same calls. Keep the two in step.

Aiken has no Shiki grammar. Use a ` ```rust ` fence for Aiken source.

### Structural facts are checked too

`pnpm check:facts` (run by `prebuild` and by CI) asserts that the fibers, fault-proof
CLI commands, and transaction statuses named in the docs still match the source they
are drawn from, including the counts written in prose. Add a fiber and the build tells
you which page to update. To cover a new fact, extend
`scripts/check-docs-facts.mjs`.

It checks symbols and counts, not meaning. A page can still mislead while passing.

### Three kinds of claim

Every stale sentence found in the copy audit was a claim nothing could check. Sort a
claim before you write it:

| Kind | Example | Rule |
| --- | --- | --- |
| Derivable from code | "Ten long-running fibers." | Add it to `check:facts`, or do not state it. |
| About a past event | "Boundary plans complete as of 2026-06-19." | Durable. A date on something that happened stays true. |
| About present project state | "No-go for an open public testnet." | Never duplicate. State it once, dated, and link. |

The third kind has no mechanical guard, so the only defence is to keep one copy of it.
`getting-started/status.mdx` is that copy, and it defers to
`public_testnet_readiness.md`. Do not reproduce a readiness matrix, a blocker list, or a
commit hash pinning present state on any other page. Link to the page that owns it.

### Rules

1. **Cite the source, and lead with the reader task where both are possible.** These do
   not compete. Writing "per `src/fibers/`" obliges you to open `src/fibers/`. Reorder a
   page for the reader; never drop the citation to do it.
2. **Where the code does not state a behavior, do not assert one.** If a page describes
   what the software does, name the file, function, or constant that backs the claim.
3. **Do not summarize an unwritten spec section.** A heading is not a rule. Several
   sections of the technical specification are stubs with `% TODO` comments.
4. **Delete self-authenticating adjectives.** "Real", "actual", "grounded",
   "always-current", "genuinely". A page that calls its own content "real" is
   compensating for a fact it is missing. Keep the word only where it draws a genuine
   contrast, as in "`/readyz` is the real signal" against `/healthz`.
5. **Never restate a command name as its description.** "`submit-deposit`: submit a
   deposit" carries nothing. State a precondition, a side effect, an irreversibility, or
   delete the line.
6. **Date every readiness or status claim.** Pair it with a date, a source file, or a
   "last reviewed" note. This targets readiness claims, not the word "currently":
   "operators currently eligible to produce blocks" describes a protocol state.
7. **Do not imply registry availability.** The `@al-ft/*` packages are workspace
   packages. Write "intended package name" until publication is verified.
8. **Label implementation state** on pages that mix shipped behavior with design target:
   Implemented, Design target, Readiness blocker, Historical design context, or Spec
   summary.
9. **Use precise rollup terms.** Committed, queued, matured, merged, challenged. Never
   "settled" for a block under challenge; `settlement` is a distinct protocol object.
10. **"Fraud proof" is the protocol; "fault proof" is the tooling.** The package and its
    binary say fault proof. The concept is a fraud proof. Both survive; say which you
    mean.
11. **No em-dashes.** Use a period, a colon, a comma, or parentheses.

### Two traps

- **"on-chain", not "onchain"** applies to prose only. `onchain/aiken` and
  `onchain/plutarch` are real directories. A find-and-replace corrupts working paths.
- **Do not silently correct a quotation.** Where a page quotes a package README, fix the
  README first, then update the quote. The same holds for identifiers copied from a
  source file: repair the source, not just the page.
