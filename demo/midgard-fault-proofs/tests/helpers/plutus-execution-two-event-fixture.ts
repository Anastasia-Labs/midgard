/** One committed block of two accepted normal transactions, each spending its
 * own deposit-derived script output: the operator commits both and applies
 * both ledger effects in order, and every root is computed from the actual
 * outputs. Each spend runs its own PlutusV3 script, failing or passing. */
import {
  computeHash28,
  encodeMidgardSpendInputItem,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { makeOutput } from "@al-ft/midgard-validation/tests/validation-fixtures";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../../src/evidence/canonical-block-evidence.js";
import { ledgerEntriesRoot, type LedgerEntry } from "../../src/ledger-at.js";
import { admitCompleteCanonicalReplayPredecessor } from "../../src/workflow/complete-replay.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  fixtureTransactionFromCanonicalCbor,
  reencodeFixturePayload,
} from "./canonical-block-evidence-fixture.js";
import {
  depositSeed,
  scenarioTransaction,
  scriptCredential,
  SEED_LOVELACE,
} from "./plutus-execution-block-fixture.js";

export type TwoEventSpend = "failing" | "passing";

const FIRST_SEED_BYTE = 0x61;
const SECOND_SEED_BYTE = 0x64;

/** `first` is the block's first event, `second` its second. */
export const plutusExecutionTwoEventBlockFixture = async (
  first: TwoEventSpend,
  second: TwoEventSpend,
) => {
  const seeds = [
    depositSeed(FIRST_SEED_BYTE, scriptCredential(first)),
    depositSeed(SECOND_SEED_BYTE, scriptCredential(second)),
  ];
  const txs = [first, second].map((scenario, index) => {
    const native = scenarioTransaction(scenario, seeds[index]!);
    return fixtureTransactionFromCanonicalCbor({
      txId: native.txId.toString("hex"),
      canonicalCbor: native.txCbor,
    });
  });
  const predecessorEntries: readonly LedgerEntry[] = seeds;
  // The ledger after each event: its seed spent, its output created.
  const after: LedgerEntry[][] = [];
  let ledger: LedgerEntry[] = [...predecessorEntries];
  for (const [index, tx] of txs.entries()) {
    ledger = [
      ...ledger.filter((entry) => !entry.outRef.equals(seeds[index]!.outRef)),
      {
        outRef: encodeMidgardSpendInputItem({
          txId: Buffer.from(tx.txId, "hex"),
          outputIndex: 0,
        }),
        output: makeOutput(SEED_LOVELACE),
      },
    ];
    after.push(ledger);
  }
  const prevUtxosRoot = await ledgerEntriesRoot(predecessorEntries);
  const roots = [
    prevUtxosRoot,
    ...(await Promise.all(after.map((entries) => ledgerEntriesRoot(entries)))),
  ];
  const base = await buildCanonicalBlockFixture({
    transactions: txs,
    prevUtxosRoot,
    transitionSteps: txs.map((tx, index) => ({
      schema_version: 1n,
      step_index: BigInt(index),
      event_key: { L2TransactionEventKey: { tx_id: tx.txId } },
      phase: "L2Transaction",
      pre_utxos_root: roots[index]!,
      post_utxos_root: roots[index + 1]!,
    })),
  });
  const header: SDK.Header = { ...base.header, utxosRoot: roots[2]! };
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
      sourceId: "test/plutus-execution-two-events",
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
  return { evidence, context, txs };
};
