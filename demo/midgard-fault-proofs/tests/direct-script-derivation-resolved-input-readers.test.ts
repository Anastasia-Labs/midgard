/**
 * The readers of a direct script derivation's resolved inputs take each
 * input's own entry: a transaction with several inputs, in a raw order that
 * is not their canonical order, gets for every input the leaf and membership
 * proof the ledger has for that input, and a spend the ledger does not hold
 * is absent at its own rank only.
 */
import {
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk/core";
import { describe, expect, it } from "vitest";

import { directPurposeSpendDescriptorPresent } from "../src/direct-purpose-family/script-purposes.js";
import { directSpendEvidence } from "../src/direct-script-derivation/claimed-purpose.js";
import { prepareDirectScriptMaterial } from "../src/direct-script-derivation/material.js";
import { directResolvedSources } from "../src/direct-script-derivation/sources.js";
import { ledgerEntriesRoot } from "../src/ledger-at.js";
import type { Ledger } from "../src/ledger-store/index.js";
import { keyValuePhasProof } from "../src/transition-trace/phas.js";
import { reconstructDaPayload } from "../src/transition-trace/reconstruct.reconstruct-da-payload.js";
import {
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
} from "./helpers/canonical-block-evidence-fixture.js";
import { referenceLedgerOf } from "./helpers/event-ledger-reference.js";
import { memoryLedger } from "./support/fixture-ledger.js";

const outRefAt = (byte: number, outputIndex: number) =>
  encodeMidgardSpendInputItem({ txId: Buffer.alloc(32, byte), outputIndex });

const scriptOutput = (hashByte: number) =>
  encodeMidgardTxOutput({
    address: Buffer.from("70" + hashByte.toString(16).repeat(28), "hex"),
    value: { lovelace: 5_000_000n, assets: new Map() },
  });

/** Spends `spent` and references `referenced`, in the raw orders given, at
 * the ledger holding `held`. */
const materialFor = async ({
  held,
  spent,
  referenced,
}: {
  held: readonly Buffer[];
  spent: readonly Buffer[];
  referenced: readonly Buffer[];
}) => {
  const entries = held.map((outRef, index) => ({
    outRef,
    output: scriptOutput(0x44 + index * 0x11),
  }));
  const root = await ledgerEntriesRoot(entries);
  const ledger = await memoryLedger(entries, root);
  const tx = buildFixtureTransaction({
    spendInputs: [...spent],
    referenceInputs: [...referenced],
    fee: 0n,
  });
  const eventKey: SDK.EventKey = { L2TransactionEventKey: { tx_id: tx.txId } };
  const fixture = await buildCanonicalBlockFixture({
    transactions: [tx],
    prevUtxosRoot: root,
    transitionSteps: [
      {
        schema_version: 1n,
        step_index: 0n,
        event_key: eventKey,
        phase: "L2Transaction",
        pre_utxos_root: root,
        post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
      },
    ],
  });
  const material = await prepareDirectScriptMaterial({
    current: await reconstructDaPayload({
      payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
    }),
    eventKey,
    predecessor: ledger,
  });
  return { ledger, material };
};

/** The leaf and membership proof `ledger` has for `key`. */
const ownEntry = async (ledger: Ledger, key: Buffer) => {
  const { trie } = await referenceLedgerOf(ledger);
  const { value } = trie.entries.find((entry) => entry.key.equals(key))!;
  return {
    descriptor_cbor: value.toString("hex"),
    proof: await keyValuePhasProof(trie, key, value),
  };
};

// Distinct outputs at distinct output indexes, so no two inputs share a leaf.
const [low, middle, high, lowReference, highReference] = [
  outRefAt(0x11, 3),
  outRefAt(0x22, 0),
  outRefAt(0x33, 5),
  outRefAt(0x12, 1),
  outRefAt(0x34, 2),
];
const canonicalSpends = [low, middle, high];
const rawSpends = [high, low, middle];
const rawReferences = [highReference, lowReference];

describe("readers of a direct script derivation's resolved inputs", () => {
  it("give each resolved source its own input's leaf and proof", async () => {
    const { ledger, material } = await materialFor({
      held: [...canonicalSpends, lowReference, highReference],
      spent: rawSpends,
      referenced: rawReferences,
    });
    for (const fieldIndex of [0, 1] as const)
      expect(
        (await directResolvedSources(material, fieldIndex)).map(
          ({ witness }) => witness,
        ),
      ).toEqual(
        await Promise.all(
          material.fields[fieldIndex]!.map((key) => ownEntry(ledger, key)),
        ),
      );
  });

  it("give each spend's claimed purpose evidence its own input's leaf and proof", async () => {
    const { ledger, material } = await materialFor({
      held: canonicalSpends,
      spent: rawSpends,
      referenced: [],
    });
    expect(
      await Promise.all(
        canonicalSpends.map(async (_, rank) => {
          const evidence = (await directSpendEvidence(material, BigInt(rank)))!;
          return {
            item: evidence.item,
            rawItemIndex: evidence.rawItemIndex,
            descriptor_cbor: evidence.descriptorCbor,
            proof: evidence.proof,
          };
        }),
      ),
    ).toEqual(
      await Promise.all(
        canonicalSpends.map(async (key) => ({
          item: key,
          rawItemIndex: BigInt(rawSpends.indexOf(key)),
          ...(await ownEntry(ledger, key)),
        })),
      ),
    );
  });

  it("find a spend's descriptor absent at its own rank only", async () => {
    // The spend the ledger does not hold is first in raw order and in
    // canonical order; the other two are held.
    const absent = outRefAt(0x01, 0);
    const { material } = await materialFor({
      held: [middle, high],
      spent: [absent, high, middle],
      referenced: [],
    });
    expect(
      [0n, 1n, 2n].map((rank) =>
        directPurposeSpendDescriptorPresent(material, rank),
      ),
    ).toEqual([false, true, true]);
  });
});
