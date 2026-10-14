import {
  computeMidgardForcedTxProofCommitment,
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import {
  buildCanonicalMidgardLedgerEntryOutputMaterial,
  placeholderValidationTraceDescriptorV1,
} from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { checkLedgerAgainstRoot } from "../src/ledger-at.js";
import {
  mpfProofFromWitness,
  normalizedMpfRoot,
} from "../src/transition-trace/detect.js";
import { requireDeletionKeepsTwoChildren } from "../src/transition-trace/detect.mpf-proof-from-witness.js";
import { createTransitionTraceLedgerReplay } from "../src/transition-trace/ledger-replay.js";
import { keyValuePhasRootWithCount } from "../src/transition-trace/phas.js";
import { deriveTransitionTraceReplayEvidence } from "../src/transition-trace/replay.js";
import {
  buildPayloadFixture,
  forcedEventKey,
  reconstruct,
} from "./transition-trace-challenger.build-payload-fixture.js";
import {
  eventToStepEntry,
  forcedTx,
  outRef,
} from "./transition-trace-challenger.native-material.js";

const key = (index: number) =>
  encodeMidgardSpendInputItem({
    txId: Buffer.alloc(32, 0x51),
    outputIndex: index,
  });
const output = encodeMidgardTxOutput({
  address: Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 0x22)]),
  value: { lovelace: 2_000_000n, assets: new Map() },
});

const descriptorEntry = (index: number) => ({
  key: key(index),
  value: Buffer.from(
    buildCanonicalMidgardLedgerEntryOutputMaterial({
      outRef: key(index),
      outputCbor: output,
    }).descriptorCbor,
  ),
});

describe("transition trace retained descriptor mutations", () => {
  it("replays a rejected forced transaction without retained machine endpoints", async () => {
    const id = outRef(88);
    const eventKey = forcedEventKey(id);
    const previous = await reconstruct(await buildPayloadFixture({}));
    const predecessor = await checkLedgerAgainstRoot({
      entries: [],
      utxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      label: "predecessor",
    });
    const transaction = forcedTx(88, {
      ForcedTxInvalid: { reason: "EmptyInputs" },
    });
    const submitted = transaction.submitted_source;
    const descriptor = SDK.validationTraceDescriptorDataFromCore(
      placeholderValidationTraceDescriptorV1(
        {
          eventKeyCbor: Buffer.from(Data.to(eventKey, SDK.EventKey), "hex"),
          transactionCommitment: computeMidgardForcedTxProofCommitment({
            compactCbor: Buffer.from(submitted.compact_cbor, "hex"),
            witnessSetCompactCbor: Buffer.from(
              submitted.witness_set_compact_cbor,
              "hex",
            ),
            fieldPreimageLengthsCbor: Buffer.from(
              submitted.field_preimage_lengths_cbor,
              "hex",
            ),
          }),
        },
        "rejected",
        "EmptyInputs",
      ),
    );
    const current = await reconstruct(
      await buildPayloadFixture({
        prevHeaderHash: previous.headerHash,
        forcedTransactions: [
          [
            Data.to(id, SDK.OutputReference),
            Data.to(transaction, SDK.ForcedInclusionTxV1),
          ],
        ],
        steps: [
          {
            schema_version: 1n,
            step_index: 0n,
            event_key: eventKey,
            phase: "ForcedTransaction",
            pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
            post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
          },
        ],
        eventToStep: [
          eventToStepEntry(eventKey, {
            step_index: 0n,
            phase: "ForcedTransaction",
          }),
        ],
        validationDescriptors: [
          [
            Data.to(eventKey, SDK.EventKey),
            Data.to(descriptor, SDK.ValidationTraceDescriptor),
          ],
        ],
        retainedWitnesses: [],
      }),
    );
    const evidence = await deriveTransitionTraceReplayEvidence({
      current,
      predecessor,
      deposits: [],
      network: "Custom",
      depositPolicyId: "34".repeat(28),
    });
    expect(evidence).toEqual({
      l2TransactionTransitions: [],
      withdrawalTransitions: [],
      depositTransitions: [],
    });
  });

  it("sends the terminal-Branch group opening with every deletion that needs one", async () => {
    const entries = Array.from({ length: 256 }, (_, index) =>
      descriptorEntry(index),
    );
    const ledger = await createTransitionTraceLedgerReplay({
      entries,
      expectedRoot: (await keyValuePhasRootWithCount(entries)).root,
    });
    let opened = 0;
    for (const entry of entries) {
      const removed = await ledger.delete(entry.key);
      const deletion = mpfProofFromWitness({
        key: entry.key,
        value: entry.value,
        proof: removed.delete_proof,
        label: "delete",
      });
      expect(normalizedMpfRoot(deletion.verify(false), "after delete")).toBe(
        ledger.root(),
      );
      requireDeletionKeepsTwoChildren({
        proof: removed.delete_proof,
        opening: removed.opening,
        label: "delete",
      });
      if (removed.opening === "") continue;
      opened += 1;
      expect("Branch" in removed.delete_proof.at(-1)!).toBe(true);
      expect(() =>
        requireDeletionKeepsTwoChildren({
          proof: removed.delete_proof,
          opening: "",
          label: "delete",
        }),
      ).toThrow(/does not keep two other children/u);
    }
    expect(opened).toBeGreaterThan(0);
    expect(ledger.root()).toBe(SDK.EMPTY_MERKLE_TREE_ROOT);
  });

  it("produces ordered canonical MPF witnesses against each evolving root", async () => {
    const entries = [0, 1].map((index) => ({
      key: key(index),
      value: Buffer.from(
        buildCanonicalMidgardLedgerEntryOutputMaterial({
          outRef: key(index),
          outputCbor: output,
        }).descriptorCbor,
      ),
    }));
    const initial = await keyValuePhasRootWithCount(entries);
    const ledger = await createTransitionTraceLedgerReplay({
      entries,
      expectedRoot: initial.root,
    });
    const removed = await ledger.delete(key(0));
    const deletion = mpfProofFromWitness({
      key: key(0),
      value: entries[0]!.value,
      proof: removed.delete_proof,
      label: "delete",
    });
    expect(normalizedMpfRoot(deletion.verify(true), "before delete")).toBe(
      initial.root,
    );
    expect(normalizedMpfRoot(deletion.verify(false), "after delete")).toBe(
      ledger.root(),
    );
    const before = ledger.root();
    const inserted = await ledger.insert(key(2), output);
    const insertion = mpfProofFromWitness({
      key: key(2),
      value: Buffer.from(inserted.value, "hex"),
      proof: inserted.insert_proof,
      label: "insert",
    });
    expect(normalizedMpfRoot(insertion.verify(false), "before insert")).toBe(
      before,
    );
    expect(normalizedMpfRoot(insertion.verify(true), "after insert")).toBe(
      ledger.root(),
    );
    expect(ledger.root()).toBe(
      (
        await keyValuePhasRootWithCount([
          entries[1]!,
          { key: key(2), value: Buffer.from(inserted.value, "hex") },
        ])
      ).root,
    );
    await expect(ledger.delete(key(0))).rejects.toThrow(/absent input/u);
    await expect(ledger.insert(key(2), output)).rejects.toThrow(
      /existing output/u,
    );
  });

  it("refuses a raw-output trie or duplicate ledger keys before replay", async () => {
    const descriptor = Buffer.from(
      buildCanonicalMidgardLedgerEntryOutputMaterial({
        outRef: key(0),
        outputCbor: output,
      }).descriptorCbor,
    );
    const correct = await keyValuePhasRootWithCount([
      { key: key(0), value: descriptor },
    ]);
    await expect(
      createTransitionTraceLedgerReplay({
        entries: [{ key: key(0), value: output }],
        expectedRoot: correct.root,
      }),
    ).rejects.toThrow(/descriptor trie/u);
    await expect(
      createTransitionTraceLedgerReplay({
        entries: [
          { key: key(0), value: descriptor },
          { key: key(0), value: descriptor },
        ],
        expectedRoot: correct.root,
      }),
    ).rejects.toThrow(/duplicate/u);
    const empty = await createTransitionTraceLedgerReplay({
      entries: [],
      expectedRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    });
    expect(empty.root()).toBe(SDK.EMPTY_MERKLE_TREE_ROOT);
  });
});
