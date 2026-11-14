import {
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { expect } from "vitest";

import {
  buildDirectSourceIndexEvidence,
  prepareDirectScriptMaterial,
} from "../../src/direct-script-derivation/index.js";
import {
  checkLedgerAgainstRoot,
  ledgerEntriesRoot,
} from "../../src/ledger-at.js";
import { reconstructDaPayload } from "../../src/transition-trace/reconstruct.js";
import {
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
} from "../helpers/canonical-block-evidence-fixture.js";

/** Exactly 100 deterministic genuine leaves, not opaque sibling/model roots.
 * Follows direct-source-fixture.ts's canonical output -> descriptor -> MPF
 * construction. Script payloads are opaque here; no script program executes.
 * Scope remains an authenticated-bound source prefix, not L2 acceptance. */
export const prepareDirectSourceResourceFixture = async () => {
  const count = 100;
  const entries = Array.from({ length: count }, (_, index) => {
    const txId = Buffer.alloc(32);
    txId.writeUInt32BE(index + 1, 28);
    return {
      outRef: encodeMidgardSpendInputItem({ txId, outputIndex: 0 }),
      output: encodeMidgardTxOutput({
        address: Buffer.from("70" + "22".repeat(28), "hex"),
        value: { lovelace: 5_000_000n, assets: new Map() },
        script_ref: {
          language: "PlutusV3",
          scriptBytes: Buffer.from([1, index]),
        },
      }),
    };
  });
  const root = await ledgerEntriesRoot(entries);
  const predecessor = await checkLedgerAgainstRoot({
    entries,
    utxosRoot: root,
    label: "N6 F2 actual hundred-entry predecessor",
  });
  expect(predecessor.trie.count).toBe(BigInt(count));
  const transaction = buildFixtureTransaction({
    spendInputs: entries.map((entry) => entry.outRef),
    referenceInputs: [],
    scriptWitnesses: [],
    fee: 0n,
  });
  const eventKey: SDK.EventKey = {
    L2TransactionEventKey: { tx_id: transaction.txId },
  };
  const fixture = await buildCanonicalBlockFixture({
    transactions: [transaction],
    prevUtxosRoot: root,
    transitionSteps: [
      {
        schema_version: 1n,
        step_index: 0n,
        event_key: eventKey,
        phase: "L2Transaction",
        pre_utxos_root: root,
        post_utxos_root: root,
      },
    ],
  });
  const material = await prepareDirectScriptMaterial({
    current: await reconstructDaPayload({
      payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
    }),
    eventKey,
    predecessor,
  });
  const evidence = await buildDirectSourceIndexEvidence(material);
  expect(material.bound.event_pre_root).toBe(root);
  expect(material.fields[0]!.map((item) => item.toString("hex"))).toEqual(
    entries.map((entry) => entry.outRef.toString("hex")),
  );
  expect(evidence.inputs).toHaveLength(count);
  expect(evidence.steps).toHaveLength(count);
  const inputs = evidence.inputs.map((entry, index) => {
    if (entry.source === null) throw new Error("F2 fixture script absent");
    expect(entry.fieldIndex).toBe(0);
    expect(entry.itemIndex).toBe(index);
    const step = evidence.steps[index]!;
    expect(step.candidate.descriptor).toEqual(entry.source.descriptor);
    return { ...entry, source: entry.source, step };
  });
  expect(evidence.index.count).toBe(BigInt(count));
  return {
    material,
    inputs,
    evidence,
    root,
    count,
    // Retain the actual source bytes, not only their root or derived counts.
    sourceInputs: {
      ledgerEntries: entries.map((entry) => ({
        outRefCbor: entry.outRef.toString("hex"),
        outputCbor: entry.output.toString("hex"),
      })),
      payloadEnvelopeCbor: fixture.payloadEnvelopeCbor.toString("hex"),
      eventKey,
    },
  };
};

export type DirectSourceResourceFixture = Awaited<
  ReturnType<typeof prepareDirectSourceResourceFixture>
>;
