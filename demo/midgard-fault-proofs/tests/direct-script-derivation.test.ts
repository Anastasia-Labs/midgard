import {
  buildMidgardBoundedItem,
  encodeMidgardRedeemerWitnessItem,
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  encodeMidgardVersionedScript,
} from "@al-ft/midgard-core";
import { encodeCborUnsigned } from "@al-ft/midgard-core/codec/cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { Constr, Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  buildDirectReceiveEvidence,
  buildDirectSourceIndexEvidence,
  directPurposeAt,
  directReceiveCredentials,
  directRedeemerAt,
  directResolvedSources,
  directWitnessSource,
  prepareDirectScriptMaterial,
} from "../src/direct-script-derivation/index.js";
import { checkLedgerAgainstRoot, ledgerEntriesRoot } from "../src/ledger-at.js";
import { reconstructDaPayload } from "../src/transition-trace/reconstruct.js";
import { completeCanonicalReplayPredecessorLedger } from "../src/workflow/complete-replay.js";
import {
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
} from "./helpers/canonical-block-evidence-fixture.js";
import { nativeDecodingFixture } from "./support/native-script-decoding-retained.js";

const a = "22".repeat(28);
const b = "33".repeat(28);
const output = (hash: string, protectedAddress = true) =>
  encodeMidgardTxOutput({
    address: Buffer.from((protectedAddress ? "78" : "70") + hash, "hex"),
    value: { lovelace: 5_000_000n, assets: new Map() },
  });

describe("direct script preparation", () => {
  it("preserves the completed source receipt in every receive wire state", () => {
    const bound: SDK.DirectScriptBound = {
      subject: {
        version: 1n,
        direction: 0n,
        source_kind: 0n,
        transaction_id: "11".repeat(32),
        source_key: "",
        rejection_reason: null,
      },
      compact_cbor: "00",
      network_order_context: { body_network_mismatch: false },
      event_pre_root: "66".repeat(32),
    };
    const source_index = { root: "44".repeat(32), count: 7n };
    const receipt = { bound, source_index };
    const fraud_prover = "aa".repeat(28);
    expect(
      Data.to({ fraud_prover, data: receipt }, SDK.DirectReceiveStartDatum),
    ).toBe(
      Data.to({ fraud_prover, data: receipt }, SDK.DirectSourceBoundIndexDatum),
    );
    const cursor: SDK.DirectFieldCursor = {
      bound,
      field_index: 2n,
      phase: "Certifying",
      checkpoint_hash: "55".repeat(32),
    };
    const wires = [
      Data.to(
        {
          fraud_prover,
          data: {
            cursor,
            source_index,
            credential_root: SDK.EMPTY_MERKLE_TREE_ROOT,
            credential_count: 0n,
            complete: false,
          },
        },
        SDK.DirectReceiveOutputDatum,
      ),
      Data.to(
        {
          fraud_prover,
          data: {
            bound,
            source_index,
            output_root: SDK.EMPTY_MERKLE_TREE_ROOT,
            expected_count: 0n,
            declared_root: SDK.EMPTY_MERKLE_TREE_ROOT,
            declared_count: 0n,
            previous_credential: "",
            target_ordinal: 0n,
            selected: null,
          },
        },
        SDK.DirectReceiveDeclarationDatum,
      ),
      Data.to(
        {
          fraud_prover,
          data: {
            bound,
            source_index,
            coordinate: { kind: 3n, index: 0n },
            purpose: null,
          },
        },
        SDK.DirectPurposeSelectionDatum,
      ),
    ];
    // Absolute field one is the source receipt in each Aiken record, rather
    // than a decoder round-trip which could silently omit the same field.
    for (const wire of wires) {
      const datum = Data.from(wire);
      if (!(datum instanceof Constr)) throw new Error("step datum absent");
      const optional = datum.fields[1];
      if (!(optional instanceof Constr)) throw new Error("step state absent");
      const state = optional.fields[0];
      if (!(state instanceof Constr)) throw new Error("receive state absent");
      expect(state.fields[1]).toEqual(new Constr(0, ["44".repeat(32), 7n]));
    }
  });

  it("binds the accepted source and its exact event pre-root", async () => {
    const tx = buildFixtureTransaction({ spendInputs: [], fee: 0n });
    const eventKey: SDK.EventKey = {
      L2TransactionEventKey: { tx_id: tx.txId },
    };
    const fixture = await buildCanonicalBlockFixture({
      transactions: [tx],
      transitionSteps: [
        {
          schema_version: 1n,
          step_index: 0n,
          event_key: eventKey,
          phase: "L2Transaction",
          pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
          post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
        },
      ],
    });
    const current = await reconstructDaPayload({
      payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
    });
    const prepared = await prepareDirectScriptMaterial({ current, eventKey });
    expect(prepared.bound.subject).toEqual(SDK.acceptedVerdictSubject(tx.txId));
    expect(prepared.sourceBound.compact_cbor).toBe(
      tx.compactCbor.toString("hex"),
    );
    expect(prepared.bound.event_pre_root).toBe(SDK.EMPTY_MERKLE_TREE_ROOT);
    expect(prepared.fields).toHaveLength(9);
  });

  it.each([0, 1] as const)(
    "uses forced direction %s without an operator trace state",
    async (direction) => {
      const fixture = await nativeDecodingFixture({ direction });
      const current = fixture.evidence.reconstruction;
      const eventKey = current.forcedTransactions[0]!;
      const prepared = await prepareDirectScriptMaterial({
        current,
        eventKey: { ForcedTransactionEventKey: { tx_order_id: eventKey.key } },
        predecessor: completeCanonicalReplayPredecessorLedger({
          evidence: fixture.evidence,
          context: fixture.context,
        }),
      });
      expect(prepared.bound.subject.direction).toBe(BigInt(direction));
      expect(prepared.bound.subject.source_kind).toBe(1n);
      expect(prepared.bound.event_pre_root).toBe(fixture.ledger.root);
      const resolved = await directResolvedSources(prepared, 1);
      expect(resolved).toHaveLength(1);
      expect(resolved[0]!.source!.item).toEqual(fixture.item);
      expect(resolved[0]!.source!.descriptor.source_key).toBe(
        fixture.key.toString("hex"),
      );
      expect(
        Data.from(
          Data.to(resolved[0]!.witness, SDK.DirectResolvedSourceWitness),
          SDK.DirectResolvedSourceWitness,
        ),
      ).toEqual(resolved[0]!.witness);
    },
  );

  it.each(["ascending", "shuffled"] as const)(
    "maps canonical spend ordinals from %s raw positions",
    async (order) => {
      // Purpose coordinates are the sorted spend-only rank, independently of
      // the raw field position and the union input-resolution traversal.
      const keys = [
        [0x11, 7],
        [0x22, 2],
        [0x33, 9],
      ].map(([byte, index]) =>
        encodeMidgardSpendInputItem({
          txId: Buffer.alloc(32, byte),
          outputIndex: index!,
        }),
      );
      const hashes = ["44".repeat(28), "55".repeat(28), "66".repeat(28)];
      const entries = keys.map((outRef, index) => ({
        outRef,
        output: output(hashes[index]!),
      }));
      const root = await ledgerEntriesRoot(entries);
      const predecessor = await checkLedgerAgainstRoot({
        entries,
        utxosRoot: root,
        label: "three spend T203 control",
      });
      const raw = order === "ascending" ? keys : [keys[2]!, keys[0]!, keys[1]!];
      const tx = buildFixtureTransaction({ spendInputs: raw, fee: 0n });
      const eventKey: SDK.EventKey = {
        L2TransactionEventKey: { tx_id: tx.txId },
      };
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
      const current = await reconstructDaPayload({
        payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
      });
      const prepared = await prepareDirectScriptMaterial({
        current,
        eventKey,
        predecessor,
      });
      expect(
        [0n, 1n, 2n].map((index) =>
          directPurposeAt(prepared, { kind: 0n, index }),
        ),
      ).toEqual(
        keys.map((key, index) => ({
          coordinate: { kind: 0n, index: BigInt(index) },
          script_hash: hashes[index],
          subject: key.toString("hex"),
        })),
      );
      expect(directPurposeAt(prepared, { kind: 0n, index: 3n })).toBeNull();
      expect(() => directPurposeAt(prepared, { kind: 0n, index: -1n })).toThrow(
        "purpose coordinate differs",
      );
    },
  );

  it("uses canonical raw observer hashes at their exact Observe ordinals", async () => {
    const observers = [Buffer.alloc(28, 0x22), Buffer.alloc(28, 0x58)];
    const tx = buildFixtureTransaction({
      spendInputs: [],
      requiredObservers: observers,
      fee: 0n,
    });
    const eventKey: SDK.EventKey = {
      L2TransactionEventKey: { tx_id: tx.txId },
    };
    const fixture = await buildCanonicalBlockFixture({
      transactions: [tx],
      transitionSteps: [
        {
          schema_version: 1n,
          step_index: 0n,
          event_key: eventKey,
          phase: "L2Transaction",
          pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
          post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
        },
      ],
    });
    const material = await prepareDirectScriptMaterial({
      current: await reconstructDaPayload({
        payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
      }),
      eventKey,
    });
    expect(material.fields[3]).toEqual(observers);
    expect(
      observers.map((_, index) =>
        directPurposeAt(material, { kind: 2n, index: BigInt(index) }),
      ),
    ).toEqual(
      observers.map((hash, index) => ({
        coordinate: { kind: 2n, index: BigInt(index) },
        script_hash: hash.toString("hex"),
        subject: hash.toString("hex"),
      })),
    );
    expect(directPurposeAt(material, { kind: 2n, index: 2n })).toBeNull();
  });

  it("builds one exact source index with reference and spent script priority over duplicate witnesses", async () => {
    const script = {
      language: "PlutusV3",
      scriptBytes: Buffer.from("0102", "hex"),
    } as const;
    const otherScript = {
      language: "PlutusV3",
      scriptBytes: Buffer.from("0103", "hex"),
    } as const;
    const low = encodeMidgardSpendInputItem({
      txId: Buffer.alloc(32, 0x11),
      outputIndex: 7,
    });
    const high = encodeMidgardSpendInputItem({
      txId: Buffer.alloc(32, 0x33),
      outputIndex: 2,
    });
    const entries = [low, high].map((outRef) => ({
      outRef,
      output: encodeMidgardTxOutput({
        address: Buffer.from("70" + a, "hex"),
        value: { lovelace: 5_000_000n, assets: new Map() },
        script_ref: script,
      }),
    }));
    const root = await ledgerEntriesRoot(entries);
    const predecessor = await checkLedgerAgainstRoot({
      entries,
      utxosRoot: root,
      label: "direct source T224 exact set",
    });
    const item = encodeMidgardVersionedScript(script);
    const tx = buildFixtureTransaction({
      spendInputs: [high],
      referenceInputs: [low],
      scriptWitnesses: [item, item, encodeMidgardVersionedScript(otherScript)],
      fee: 0n,
    });
    const eventKey: SDK.EventKey = {
      L2TransactionEventKey: { tx_id: tx.txId },
    };
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
      predecessor,
    });
    const indexed = await buildDirectSourceIndexEvidence(material);
    const hash = directWitnessSource(item, 0).descriptor.script_hash;
    expect(indexed.inputs).toHaveLength(2);
    expect(indexed.steps).toHaveLength(5);
    expect(indexed.index.count).toBe(2n);
    expect(indexed.selected.get(hash)!.descriptor).toMatchObject({
      field_index: 1n,
      item_index: 0n,
      source_key: low.toString("hex"),
    });
    expect(indexed.steps[0]!.after.count).toBe(1n);
    expect(indexed.steps[1]!.after.count).toBe(1n);
    expect(indexed.steps[1]!.after.root).not.toBe(indexed.steps[0]!.after.root);
    expect(indexed.steps[2]!.after).toEqual(indexed.steps[1]!.after);
    expect(indexed.steps[3]!.after).toEqual(indexed.steps[2]!.after);
    expect((await indexed.lookup(hash)).selected!.source_key).toBe(
      low.toString("hex"),
    );
    expect((await indexed.lookup("ff".repeat(28))).selected).toBeNull();
  });

  it("refuses a nonempty predecessor when no checked ledger is available", async () => {
    const fixture = await nativeDecodingFixture();
    const current = fixture.evidence.reconstruction;
    await expect(
      prepareDirectScriptMaterial({
        current,
        eventKey: {
          ForcedTransactionEventKey: {
            tx_order_id: current.forcedTransactions[0]!.key,
          },
        },
      }),
    ).rejects.toThrow("authenticated predecessor ledger unavailable");
  });

  it("keeps sorted receive ordinals distinct from first output locations", async () => {
    const outputs = [output(b), output(a), output(b), output(a, false)];
    expect(directReceiveCredentials(outputs)).toEqual([
      { hash: a, firstOutputIndex: 1 },
      { hash: b, firstOutputIndex: 0 },
    ]);
    const evidence = await buildDirectReceiveEvidence(outputs);
    expect(evidence.entries.map((item) => item.script_hash)).toEqual([a, b]);
    expect(evidence.entries.map((item) => item.first_output_index)).toEqual([
      1n,
      0n,
    ]);
    expect(evidence.witnesses).toHaveLength(outputs.length);
    expect(evidence.witnesses[2]).toMatchObject({
      RepeatedCredential: { first_output_index: 0n },
    });
    expect(evidence.witnesses[3]).toBe("NoCredential");
    expect(evidence.outputRoots.at(-1)).toBe(evidence.declarationRoots.at(-1));
  });

  it("preserves large witness positions and malformed native payload bytes", () => {
    const item = Buffer.from("8200428109", "hex");
    const source = directWitnessSource(item, 30_004);
    expect(source.descriptor.source_key).toBe(
      encodeCborUnsigned(30_004n).toString("hex"),
    );
    expect(source.item).toEqual(item);
    expect(source.descriptor.item_commitment).toBe(
      buildMidgardBoundedItem({
        fieldIndex: 6,
        itemIndex: 30_004,
        bytes: item,
      }).commitment.toString("hex"),
    );
  });

  it.each(["Spend", "Mint", "Reward", "Receive"] as const)(
    "preserves actual %s redeemer Data and declared units",
    (purpose) => {
      const kind = ["Spend", "Mint", "Reward", "Receive"].indexOf(purpose);
      const data = Buffer.concat([
        Buffer.from("594000", "hex"),
        Buffer.alloc(16_384),
      ]);
      const item = encodeMidgardRedeemerWitnessItem({
        purpose,
        index: 7n,
        redeemerCbor: data,
        executionUnits: { memory: 431n, steps: 912_345n },
      });
      const selected = directRedeemerAt([item], {
        kind: BigInt(kind),
        index: 7n,
      });
      expect(selected).toMatchObject({
        data_length: BigInt(data.length),
        execution_memory: 431n,
        execution_steps: 912_345n,
      });
      if (selected === null) throw new Error("matching actual redeemer absent");
      expect(Data.to(selected, SDK.DirectRedeemer).length / 2).toBeLessThan(
        200,
      );
      expect(
        directRedeemerAt([item], { kind: BigInt(kind), index: 8n }),
      ).toBeNull();
      expect(() =>
        directRedeemerAt([item, item], { kind: BigInt(kind), index: 7n }),
      ).toThrow("duplicate redeemer pointer");
    },
  );
});
