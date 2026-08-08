# `docs/spec/` — implementation-normative component specifications

This directory holds owner-reviewed, implementation-normative component
specifications: exact types, byte-level encodings, and plain-English security
properties for deployed Midgard components.

- **Status:** Active.
- **Last reviewed:** 2026-08-08 (directory established, Phase 0 of the flat
  reversion program).

## Authority rule

The rule below is a property of this directory: every component
specification added here inherits it.

- Where a component spec exists here, it **wins over `technical-spec/` on
  concrete detail** — types, encodings, constants, byte-level behavior, and
  the security properties stated with them. That scope is stated identically
  in the governing policy at `docs/DOCUMENTATION_POLICY.md` §Source
  hierarchy item 2; the two must be read as one rule.
  `technical-spec/` remains the protocol-level design target; divergences
  from a component spec are fixed as errata on `technical-spec/`'s own
  cadence and are never grounds to reopen a component spec.
- `GOAL_SPEC.md` binds its acceptance criteria to these documents **by
  reference at scheme altitude**: scheme names live in `GOAL_SPEC.md`,
  primitives live here — "primitives" meaning exactly the concrete detail
  enumerated above. Primitive-level corrections to a component spec are
  errata to that document and do not reopen `GOAL_SPEC.md` acceptance
  criteria.
- `docs/midgard/decisions/` records rationale (_why_); the component specs
  here carry the definition (_what_). A decision record must not restate
  format detail, and a component spec must not carry decision rationale
  beyond what its normative content requires.
- Amendments to a component spec are ordinary reviewed commits approved by
  the repository owner. `git log -p -- docs/spec/<file>` is the amendment
  history.

## Documents

- `midgard-tx.md` — the MidgardTx compact-transaction type, canonical
  encoding, the nine flat blake2b-256 field commitments, the enveloped
  preimage grammar, and the three-tier field-preimage carriage convention.
