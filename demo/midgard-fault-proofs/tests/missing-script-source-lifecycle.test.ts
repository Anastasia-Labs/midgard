/**
 * missingScriptSource (`0000002d`) — complete Lucid lifecycle over the six
 * applied reference scripts.
 *
 * Every thread starts from the generic computation-thread `Init` and walks
 * step 01 (purpose binding) → step 02 (trace authentication) → step 03
 * (purpose and transaction-source frontiers) → step 04 (resolved partition,
 * scan opening) → step 05 (the resumable universal-source scan) → step 06
 * (permanent mint) → state-queue target-and-descendant removal. Evidence is
 * reconstructed from the retained DA rows the block commits, never from a
 * fabricated mid-thread datum. The suite covers the §5.3 items the family
 * owns: both directions over every purpose kind and both source locations,
 * honest refusals at the exact on-chain predicate, reason/coordinate and
 * seam substitutions, cancel from every nonterminal physical step, restart
 * from a real scan checkpoint, permanent mint plus removal, the scan's own
 * budget bound, and the maximum consensus-bounded frontier. Every positive
 * transaction is measured against the Van Rossem envelope with the
 * repository's reserves; no oversized route exists here.
 */

import "@al-ft/midgard-core";
import "@lucid-evolution/lucid";
import "vitest";
import "../src/missing-script-source/family.js";
import "../src/missing-script-source/retained-script-universe.js";
import "../src/missing-script-source/schemas.js";
import "../src/missing-script-source/universe-scan.js";
import "./support/emulator/expect-onchain-refusal.js";
import "./support/measured-fit-ledger.js";
import "./support/missing-script-source-emulator.js";
import "./support/missing-script-source-shapes.js";
import "./missing-script-source-lifecycle.missing-script-source-retained-fixtures.js";
import "./missing-script-source-lifecycle.missing-script-source-real-lifecycle.js";
