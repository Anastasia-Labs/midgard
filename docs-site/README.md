# Midgard Documentation Site

Fumadocs/Next.js documentation for the SDKs, validation, operator and watcher
services, on-chain contracts, and protocol specification.

## Develop and build

Install the demo workspace dependencies first because the site links its SDK
packages. From the repository root, using each directory's pinned pnpm version
(demo: 9.15.4; docs-site: 10.11.0):

```sh
pnpm --dir demo install --frozen-lockfile
pnpm --dir docs-site install --frozen-lockfile
pnpm --dir docs-site dev
```

Development serves at `http://localhost:3000`. `predev` builds `midgard-core`
and `lucid-midgard` so their linked declarations are available.

From `docs-site`:

```sh
pnpm build
pnpm types:check
```

`prebuild` builds the linked SDKs and runs `check:links`, then Next.js
exports the site to `out/`. The configured static
export is not served by `next start`; use `pnpm dev` for local development or
serve `out/` with a static file server. `types:check` also builds the SDKs,
then runs `fumadocs-mdx`, `next typegen`, and `tsc --noEmit`.

The [Pages workflow](../.github/workflows/pages-deploy.yml) builds the static
site with `NEXT_PUBLIC_BASE_PATH=/midgard`, adds the compiled technical
specification PDF, and deploys the combined artifact on configured `main`
pushes or manual dispatch.

## Structure

- `content/docs/`: MDX pages and folder `meta.json` navigation.
- `app/`: Next.js routes, layouts, homepage, and search export.
- `lib/`: Fumadocs source loader and shared layout options.
- `components/`: Mermaid and other site components.
- `source.config.ts`: content schemas, highlighting, and twoslash.

## Checked examples and references

Mark SDK examples with `ts twoslash` to type-check them during the build:

````md
```ts twoslash
import type { CompleteTx } from "@al-ft/lucid-midgard";
declare const tx: CompleteTx;
// ---cut---
const signed = await tx.sign.withWallet().complete();
```
````

Use declarations before `// ---cut---` for context that should not appear in
the rendered sample. Plain `ts` fences are not checked by twoslash. Type
checking verifies API shape, not balancing, state-dependent validation, or live
network behavior. The homepage's string sample has a separate fixture in
`app/(home)/homepage-sample.check.ts`; keep its calls aligned with the rendered
sample. The site currently uses `rust` fences for Aiken highlighting.

Run the documentation link check from this directory:

```sh
pnpm check:links
```

Link checks validate repository-local targets. They do not establish semantic
correctness: read the relevant implementation and tests before changing a
behavioral claim.

## Writing and maintenance

Follow the [documentation policy](../docs/DOCUMENTATION_POLICY.md). Keep current
readiness in [the readiness assessment](../docs/public_testnet_readiness.md)
and proof installation/evidence in [the fault-proof audit](../docs/fault-proofs/README.md).
Link those authorities instead of copying blocker lists into multiple pages.

- Lead with the reader's task; cite concrete implementation or specification
  sources for behavior.
- Distinguish a design target, implemented behavior, and deployment acceptance.
  An unfinished specification heading does not establish a rule.
- Keep useful rationale in ADRs and remove delivered plans. Historical context
  alone does not justify a second copy of an operating procedure.
- Use precise lifecycle terms: admission, accepted, committed, merged, challenged.
  A queued block remains challengeable; settlement is a separate protocol object.
- Treat `@al-ft/*` packages as workspace packages unless publication is verified.
- Use “fraud proof” for the protocol and “fault proof” for the tooling names.
- Update source consumers and relevant CI path triggers when adding a new
  documentation fact check. Re-run the named checks after edits.
