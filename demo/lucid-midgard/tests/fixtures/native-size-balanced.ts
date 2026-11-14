/**
 * The **declared construction** of the size-balanced conformance fixture.
 *
 * `native-size-balanced-15_5k.json` used to be an opaque checked-in blob: a
 * ~16 kB `fullTxCborHex` with no producer anywhere in the repo (#588). Every
 * derived artifact — the Aiken goldens in
 * `onchain/aiken/lib/midgard/fraud-proofs/native-tx.size-balanced.test.ak`, the
 * retained-DA boundary measurement, the `mixed-size-balanced` row of the
 * Cardano-capability corpus — hung off bytes nothing could regenerate, so a
 * wire-format change could only be absorbed by hand-editing the blob.
 *
 * This module replaces the blob with a construction stated in parameters:
 * `SIZE_BALANCED_PARAMETERS` below says what the transaction *is*, and every
 * byte follows from it through the canonical §5.1 producers in
 * `@al-ft/midgard-core`. The fixture's identity is therefore a consequence of
 * the declaration, not an input to it — when the grammar moves, the parameters
 * stay put and the bytes regenerate.
 *
 * What "size-balanced" declares is the *shape*, not one exact byte count: the
 * point of the fixture is a transaction near the top of the Cardano envelope
 * whose nine fields all carry real cardinality at once, so that decode, reveal
 * and reconstruction costs are measured against a realistic mix rather than
 * against one maximised field. `targetFullTxCborBytes ± fullTxCborToleranceBytes`
 * is the band the construction must land in, and the builder asserts it.
 *
 * Unlike its high-cardinality sibling this transaction is **not** built through
 * `LucidMidgard`: its script witnesses are deliberately synthetic (they are not
 * valid UPLC programs), which is what lets one fixture carry 68 script
 * witnesses at this size, and which the retained-DA harness admits only under
 * the `diagnostic-synthetic-script-witnesses` production-admission label for
 * exactly this corpus row. Building it from declared field preimages rather
 * than from a wallet keeps that property honest and visible.
 *
 * Writer: `pnpm --dir demo/lucid-midgard run fixtures:native-size-balanced:sync`
 * (`tests/native-size-balanced-fixture.test.ts`), then
 * `pnpm --dir demo/lucid-midgard run fixtures:native-compact` to rebind the
 * Aiken goldens derived from it.
 */

import "@al-ft/midgard-core/codec";
import "@lucid-evolution/lucid";
import "./native-tx-fixture-shape.js";
import "./native-size-balanced.size-balanced-parameters.js";
import "./native-size-balanced.build-size-balanced-native-tx-fixture.js";
export { buildSizeBalancedNativeTxFixture } from "./native-size-balanced.build-size-balanced-native-tx-fixture.js";
export {
  SIZE_BALANCED_COUNTS,
  SIZE_BALANCED_FIXTURE_NAME,
  SIZE_BALANCED_PARAMETERS,
  SIZE_BALANCED_PRODUCER,
  type SizeBalancedNativeTxFixture,
} from "./native-size-balanced.size-balanced-parameters.js";
