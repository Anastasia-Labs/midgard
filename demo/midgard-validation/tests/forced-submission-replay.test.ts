import { decodeSingleCbor, encodeCbor } from "@al-ft/midgard-core/codec";
import {
  computeMidgardForcedTxProofCommitment,
  deriveMidgardForcedTxProofSource,
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec/forced";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  RejectCodes,
  replayValidationMachineEvent,
  runPhaseAValidation,
  validationMachineLedgerRoot,
} from "../src/index.js";
import {
  decodeMidgardLedgerTxFromCanonicalCbor,
  encodeMidgardLedgerTxToCanonicalCbor,
  projectMidgardRawEnvelopeForPhaseAV1,
} from "../src/ledger-tx.js";
import {
  deserializePhaseACandidate,
  serializePhaseACandidate,
} from "../src/wire.js";
import {
  encodeRecomputedNativeTx,
  makeNativeTx,
  makeOutput,
  makeQueued,
  outRefFromByte,
} from "./validation-fixtures.js";

const config = {
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  concurrency: 1,
  strictnessProfile: "forced-submission-test",
};
const fixture = (options: Parameters<typeof makeNativeTx>[0] = {}) =>
  makeNativeTx({
    spendInputs: [outRefFromByte(0x31)],
    outputs: [makeOutput(10_000_000n)],
    ...options,
  });
const submittedBytes = (native: ReturnType<typeof makeNativeTx>) =>
  encodeMidgardForcedTxCanonical(
    materializeMidgardForcedTxFromCanonical(native.tx),
  );
const run = (
  native: ReturnType<typeof makeNativeTx>,
  txCbor = submittedBytes(native),
  sourceKind: "normal" | "forced" = "forced",
  feeA = 0n,
) =>
  Effect.runPromise(
    runPhaseAValidation([{ ...makeQueued(native.txId, txCbor), sourceKind }], {
      ...config,
      minFeeA: feeA,
    }),
  );

describe("forced submission validation boundary", () => {
  it("projects the immutable transaction without an independent validity claim", () => {
    const native = fixture();
    const submitted = decodeMidgardLedgerTxFromCanonicalCbor(
      submittedBytes(native),
      "forced",
    );
    expect(submitted.txId).toEqual(native.txId);
    expect(submitted).not.toHaveProperty("validity");
    expect(submitted.spendInputs).toEqual(
      decodeMidgardLedgerTxFromCanonicalCbor(native.txCbor).spendInputs,
    );
  });

  it("prevents a temporary forced ledger view from becoming a normal committed transaction", () => {
    const submitted = decodeMidgardLedgerTxFromCanonicalCbor(
      submittedBytes(fixture()),
      "forced",
    );
    expect(() => encodeMidgardLedgerTxToCanonicalCbor(submitted)).toThrow();
  });

  it("independently accepts a valid forced submission", async () => {
    const native = fixture();
    const result = await run(native);
    expect(result.rejected).toEqual([]);
    expect(result.accepted).toHaveLength(1);
    expect(result.accepted[0]!.ledgerTx.txId).toEqual(native.txId);
    const restored = deserializePhaseACandidate(
      serializePhaseACandidate(result.accepted[0]!),
    );
    expect(restored.submission.sourceKind).toBe("forced");
    expect(restored.submission.txCbor).toEqual(submittedBytes(native));
    expect(restored.ledgerTx.validity).toBeUndefined();
  });

  it("independently rejects an invalid signature without any operator bit", async () => {
    const result = await run(fixture({ invalidVkeyWitness: true }));
    expect(result.accepted).toEqual([]);
    expect(result.rejected[0]!.code).toBe(RejectCodes.InvalidSignature);
  });

  it("refuses a normal four-element envelope at the forced entry point", async () => {
    const native = fixture();
    const result = await run(native, native.txCbor);
    expect(result.accepted).toEqual([]);
    expect(result.rejected).toHaveLength(1);
    expect(() =>
      decodeMidgardLedgerTxFromCanonicalCbor(native.txCbor, "forced"),
    ).toThrow();
  });

  it("refuses a forced envelope at the normal admission entry point", async () => {
    const native = fixture();
    const result = await run(native, submittedBytes(native), "normal");
    expect(result.accepted).toEqual([]);
    expect(result.rejected).toHaveLength(1);
  });

  it("preserves the same logical fee boundary as the normal envelope", async () => {
    // The forced envelope is one byte shorter. Charging that shorter size
    // would incorrectly admit this one-lovelace-underfunded transaction.
    let native = fixture({ fee: 10_000n });
    native = fixture({ fee: BigInt(native.txCbor.length - 1) });
    expect(submittedBytes(native).length + 1).toBe(native.txCbor.length);
    const [normal, forced] = await Promise.all([
      run(native, native.txCbor, "normal", 1n),
      run(native, submittedBytes(native), "forced", 1n),
    ]);
    expect(normal.rejected).toHaveLength(1);
    expect(forced.rejected).toHaveLength(1);
    expect(forced.rejected[0]!.code).toBe(normal.rejected[0]!.code);
    expect(forced.rejected[0]!.code).toBe(RejectCodes.MinFee);
  });

  it("retains malformed witness evidence byte-for-byte in the forced raw projection", () => {
    const native = fixture();
    const item = Buffer.from("820043820700", "hex");
    const malformed = encodeRecomputedNativeTx({
      ...native.tx,
      witnessSet: {
        ...native.tx.witnessSet,
        scriptTxWitsPreimageCbor: encodeCbor([item]),
      },
    });
    const normalEnvelope = decodeSingleCbor(malformed.txCbor);
    if (!Array.isArray(normalEnvelope) || normalEnvelope.length !== 4)
      throw new Error("native fixture shape");
    const forcedBytes = encodeCbor(normalEnvelope.slice(0, 3));
    const projected = projectMidgardRawEnvelopeForPhaseAV1(
      forcedBytes,
      "forced",
    );
    expect(projected.canonicalSubmittedTx).toBeNull();
    expect(projected.transactionId).toEqual(native.txId);
    expect(projected.scriptWitnesses[0]!.versionedItemBytes).toEqual(item);
    expect(() =>
      decodeMidgardLedgerTxFromCanonicalCbor(forcedBytes, "forced"),
    ).toThrow();
  });
  it.each([
    { valid: true, claimed: "ForcedTxValid" as const },
    {
      valid: true,
      claimed: {
        ForcedTxInvalid: { reason: "FeeBelowMinimum" },
      } satisfies SDK.OperatorVerdict,
    },
    { valid: false, claimed: "ForcedTxValid" as const },
    {
      valid: false,
      claimed: {
        ForcedTxInvalid: {
          reason: { AddressWitnessSignatureInvalid: { witness_index: 0n } },
        },
      } satisfies SDK.OperatorVerdict,
    },
  ])(
    "replays actual validity=$valid independently of claim=$claimed",
    async ({ valid, claimed }) => {
      const native = fixture(valid ? {} : { invalidVkeyWitness: true });
      const submitted = materializeMidgardForcedTxFromCanonical(native.tx);
      const proof = deriveMidgardForcedTxProofSource(submitted);
      const leaf: SDK.ForcedInclusionTxV1 = {
        tx_id: native.txId.toString("hex"),
        submitted_source: {
          compact_cbor: proof.compactCbor.toString("hex"),
          witness_set_compact_cbor: proof.witnessSetCompactCbor.toString("hex"),
          field_preimage_lengths_cbor:
            proof.fieldPreimageLengthsCbor.toString("hex"),
        },
        verdict: claimed,
      };
      const bytesBefore = Data.to(leaf, SDK.ForcedInclusionTxV1);
      const entries = [
        { outRef: outRefFromByte(0x31), output: makeOutput(10_000_000n) },
      ];
      const priorRoot = (await validationMachineLedgerRoot(entries)).toString(
        "hex",
      );
      const replay = await Effect.runPromise(
        replayValidationMachineEvent({
          consensusProfile: MIDGARD_CONSENSUS_PROFILE,
          sourceKind: "forced",
          canonicalTransactionCbor: submittedBytes(native),
          eventKeyCbor: Buffer.from(
            Data.to(
              {
                ForcedTransactionEventKey: {
                  tx_order_id: {
                    transactionId: "73".repeat(32),
                    outputIndex: 0n,
                  },
                },
              },
              SDK.EventKey,
            ),
            "hex",
          ),
          ledgerWitnessEntries: entries,
          priorUtxosRoot: priorRoot,
          blockEndTimeMs: 1_750_000_000_000,
          expectedNetworkId: 0n,
          minFeeA: 0n,
          minFeeB: 0n,
          blockSlot: 100n,
        }),
      );
      expect(replay.trace.verdict).toBe(valid ? "accepted" : "rejected");
      expect(replay.trace.states[0]!.transactionCommitment).toEqual(
        computeMidgardForcedTxProofCommitment(proof),
      );
      expect(replay.trace.states.at(-1)!.transactionCommitment).toEqual(
        computeMidgardForcedTxProofCommitment(proof),
      );
      expect(replay.replayInput.expectedLedgerOps).toHaveLength(valid ? 2 : 0);
      if (!valid) expect(replay.replayInput.postUtxosRoot).toBe(priorRoot);
      expect(Data.to(leaf, SDK.ForcedInclusionTxV1)).toBe(bytesBefore);
    },
  );

  it("keeps the existing missing-signer-before-signature multi-fault order", async () => {
    const result = await run(
      fixture({
        invalidVkeyWitness: true,
        requiredSignerItems: [Buffer.alloc(28, 0xa7)],
      }),
    );
    expect(result.accepted).toEqual([]);
    expect(result.rejected[0]!.code).toBe(RejectCodes.MissingRequiredWitness);
  });
});
