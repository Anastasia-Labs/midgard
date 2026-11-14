/** One committed block of normal transactions, applied in the given order
 * over `predecessorEntries`: step `i` is `txs[i]`, between the roots
 * `steps[i]` the operator committed for it. The header starts from the first
 * step's pre-state root and commits the last step's post-state root. The
 * predecessor block's ledger source answers with `predecessorEntries`. */
import { computeHash28 } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../../src/evidence/canonical-block-evidence.js";
import type { LedgerEntry } from "../../src/ledger-at.js";
import { admitCompleteCanonicalReplayPredecessor } from "../../src/workflow/complete-replay.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  type FixtureTransaction,
  reencodeFixturePayload,
} from "./canonical-block-evidence-fixture.js";

export const committedL2BlockFixture = async ({
  predecessorEntries,
  txs,
  steps,
}: {
  readonly predecessorEntries: readonly LedgerEntry[];
  readonly txs: readonly FixtureTransaction[];
  readonly steps: readonly Readonly<{ pre: string; post: string }>[];
}) => {
  if (txs.length === 0 || steps.length !== txs.length)
    throw new Error("each transaction of the block commits one step");
  const base = await buildCanonicalBlockFixture({
    transactions: txs,
    prevUtxosRoot: steps[0]!.pre,
    transitionSteps: txs.map((tx, index) => ({
      schema_version: 1n,
      step_index: BigInt(index),
      event_key: { L2TransactionEventKey: { tx_id: tx.txId } },
      phase: "L2Transaction",
      pre_utxos_root: steps[index]!.pre,
      post_utxos_root: steps[index]!.post,
    })),
  });
  const header: SDK.Header = { ...base.header, utxosRoot: steps.at(-1)!.post };
  const headerHash = computeHash28(SDK.encodeHeaderCbor(header)).toString(
    "hex",
  );
  const payload: SDK.DaPayload = {
    ...base.payload,
    block_body: {
      ...base.payload.block_body,
      header,
      header_hash: headerHash,
    },
  };
  const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
    observation: authenticatedHeaderObservation({ header, headerHash }),
    payloadEnvelopeCbor: await reencodeFixturePayload(payload),
    daProvenance: {
      trustClass: "public_or_permissionless_da",
      sourceId: "test/committed-l2-block",
      grade: "security",
    },
  });
  const context = {
    predecessor: await admitCompleteCanonicalReplayPredecessor({
      currentEvidence: evidence,
      ledgerAt: async (hash) =>
        hash === header.prevHeaderHash ? predecessorEntries : undefined,
    }),
  };
  return { evidence, context };
};
