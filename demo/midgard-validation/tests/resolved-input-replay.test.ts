import { Proof, Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  buildMidgardValidationMerkleFrontier,
  hashMidgardResolvedContextItemLeaf,
  verifyMidgardValidationMerkleMembership,
} from "@al-ft/midgard-core";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  buildDeterministicValidationMachineTrace,
  buildValidationMachineLedgerInsertOp,
  buildValidationMachineLedgerMutationSteps,
} from "../src/index.js";
import { buildCanonicalMidgardLedgerEntryOutputMaterial } from "../src/ledger-output-descriptor.js";
import {
  FUNDED_OUTPUT_LOVELACE,
  makeNativeTx,
  makeOutput,
  outRefFromByte,
  outRefFromTxId,
} from "./validation-fixtures.js";
import { context } from "./validation-machine.validate-boundary-abi-and-collect-auxiliary-kinds.js";

/**
 * TS twin of the replay-value bindings. Each resolved input is replayed twice:
 * ScriptSources stage 3 authenticates its descriptor against the prior ledger
 * root with an MPF membership proof (`mpf_proof_v1.has`), and the ValueAndMint
 * replay opens the resolved-item leaf stage 3 appended at its cursor
 * (`validation_merkle_v1.verify_membership`). The trace's witnesses satisfy
 * both, and the same witnesses with any other value do not.
 */

const spent = outRefFromByte(0x61);
const referenced = [outRefFromByte(0x62), outRefFromByte(0x63)];
const entries = [spent, ...referenced].map((outRef, index) => ({
  outRef,
  output: makeOutput(FUNDED_OUTPUT_LOVELACE + BigInt(index)),
}));
const descriptorOf = (outRef: Buffer, output: Buffer): Buffer =>
  buildCanonicalMidgardLedgerEntryOutputMaterial({ outRef, outputCbor: output })
    .descriptorCbor;

const replayTrace = async () => {
  const output = makeOutput(FUNDED_OUTPUT_LOVELACE);
  const transaction = makeNativeTx({
    spendInputs: [spent],
    referenceInputs: referenced,
    outputs: [output],
  });
  const expectedLedgerOps = [
    { type: "delete" as const, key: spent },
    buildValidationMachineLedgerInsertOp({
      key: outRefFromTxId(transaction.txId),
      outputCbor: output,
    }),
  ];
  const ledgerMutationSteps = await buildValidationMachineLedgerMutationSteps({
    initialEntries: entries,
    operations: expectedLedgerOps,
  });
  return await Effect.runPromise(
    buildDeterministicValidationMachineTrace({
      l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
      ...context,
      transactionId: transaction.txId,
      canonicalTransactionCbor: transaction.txCbor,
      priorUtxosRoot: ledgerMutationSteps[0]!.preRoot.toString("hex"),
      postUtxosRoot: ledgerMutationSteps.at(-1)!.postRoot.toString("hex"),
      ledgerWitnessEntries: entries,
      expectedLedgerOps,
      ledgerMutationSteps,
      expectedVerdict: "accepted",
      expectedRejectionCode: null,
    }),
  );
};

/** A well-formed descriptor of the same input with one more lovelace. */
const substituted = (outRef: Buffer, lovelace: bigint): Buffer =>
  descriptorOf(outRef, makeOutput(lovelace + 1n));

describe("resolved-input replay values (TS twin)", () => {
  it("binds each replayed value to the prior ledger and to its resolved leaf", async () => {
    const trace = await replayTrace();
    expect(trace.verdict).toBe("accepted");
    const replays = (phase: "scriptSources" | "valueAndMint") =>
      trace.witnesses.flatMap((witness) =>
        witness.phase === phase &&
        witness.auxiliary?.kind === "resolvedInputReplay"
          ? [witness.auxiliary]
          : [],
      );
    const scriptSources = replays("scriptSources");
    const valueAndMint = replays("valueAndMint");
    expect(scriptSources).toHaveLength(3);
    expect(valueAndMint).toHaveLength(3);

    // ScriptSources stage 3: the proof opens (key, value) at the prior root.
    const store = new Store(undefined);
    await store.ready();
    const trie = new Trie(store);
    for (const { outRef, output } of entries)
      await trie.insert(outRef, descriptorOf(outRef, output));
    const priorRoot = Buffer.from(trie.hash);
    const lovelaceOf = new Map(
      entries.map(({ outRef }, index) => [
        outRef.toString("hex"),
        FUNDED_OUTPUT_LOVELACE + BigInt(index),
      ]),
    );
    for (const replay of scriptSources) {
      expect(replay.siblings).toStrictEqual([]);
      const proof = await trie.prove(replay.key);
      expect(Buffer.from(proof.toCBOR())).toStrictEqual(replay.proofCbor);
      const steps = proof.toJSON() as object[];
      const opens = (value: Buffer) =>
        Proof.fromJSON(replay.key, value, steps)
          .verify(true)
          ?.equals(priorRoot) === true;
      expect(opens(replay.value)).toBe(true);
      expect(
        opens(
          substituted(replay.key, lovelaceOf.get(replay.key.toString("hex"))!),
        ),
      ).toBe(false);
    }

    // ValueAndMint: the value opens the leaf ScriptSources appended at the
    // same cursor, in the frontier ScriptSources built.
    const leaf = (
      replay: (typeof scriptSources)[number],
      itemIndex: number,
      value: Buffer,
    ) =>
      hashMidgardResolvedContextItemLeaf({
        sourceKind: replay.sourceKind,
        itemIndex,
        key: replay.key,
        outputCbor: value,
      });
    const frontier = buildMidgardValidationMerkleFrontier(
      scriptSources.map((replay, index) => leaf(replay, index, replay.value)),
    );
    valueAndMint.forEach((replay, cursor) => {
      expect(replay.proofCbor).toStrictEqual(Buffer.from([0x80]));
      const opens = (value: Buffer, leafIndex = cursor) =>
        verifyMidgardValidationMerkleMembership({
          frontier,
          leafIndex,
          leafHash: leaf(replay, leafIndex, value),
          siblings: replay.siblings,
        });
      expect(replay.value).toStrictEqual(scriptSources[cursor]!.value);
      expect(opens(replay.value)).toBe(true);
      expect(
        opens(
          substituted(replay.key, lovelaceOf.get(replay.key.toString("hex"))!),
        ),
      ).toBe(false);
      expect(opens(replay.value, (cursor + 1) % 3)).toBe(false);
    });
  }, 120_000);
});
