/** One committed block of accepted normal transactions, each spending its
 * own deposit-derived script output: the operator commits them all and
 * applies their ledger effects in order, and every root is computed from the
 * actual outputs unless a step's post-state root is replaced. Each spend runs
 * its own PlutusV3 script, failing or passing. */
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

export type EventSpend = "failing" | "passing";

/** The deposit seed byte of the block's `index`th spend. */
const seedByte = (index: number): number => 0x61 + 3 * index;

/** A root no ledger of the block opens. */
export const FOREIGN_UTXOS_ROOT = "ab".repeat(32);

/**
 * `spends` in the order the block applies them. `predecessorFiller` joins
 * the previous ledger untouched; `foreignPostRootAt` names a step whose
 * committed post-state root (and so the next step's pre-state root) is
 * `FOREIGN_UTXOS_ROOT` rather than the root of its actual ledger.
 */
export const plutusExecutionEventsBlockFixture = async (
  spends: readonly EventSpend[],
  {
    predecessorFiller = [],
    foreignPostRootAt,
  }: {
    readonly predecessorFiller?: readonly LedgerEntry[];
    readonly foreignPostRootAt?: number;
  } = {},
) => {
  const seeds = spends.map((spend, index) =>
    depositSeed(seedByte(index), scriptCredential(spend)),
  );
  const txs = spends.map((spend, index) => {
    const native = scenarioTransaction(spend, seeds[index]!);
    return fixtureTransactionFromCanonicalCbor({
      txId: native.txId.toString("hex"),
      canonicalCbor: native.txCbor,
    });
  });
  const predecessorEntries: readonly LedgerEntry[] = [
    ...seeds,
    ...predecessorFiller,
  ];
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
  const roots = [prevUtxosRoot];
  for (const [index, entries] of after.entries())
    roots.push(
      index === foreignPostRootAt
        ? FOREIGN_UTXOS_ROOT
        : await ledgerEntriesRoot(entries),
    );
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
  const header: SDK.Header = { ...base.header, utxosRoot: roots.at(-1)! };
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
      sourceId: "test/plutus-execution-events",
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
