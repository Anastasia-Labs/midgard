import { encodeCbor, MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core";
import { blake2b } from "@noble/hashes/blake2.js";
import { Effect, Either } from "effect";
import { describe, expect, it, vi } from "vitest";

import {
  buildDeterministicValidationMachineTrace,
  DirectValidationTraceUnavailable,
  RejectCodes,
} from "../src/index.js";
import { phaseARejection } from "./reject-subject.support.js";
import {
  encodeRecomputedNativeTx,
  FUNDED_OUTPUT_LOVELACE,
  makeMintPreimageCbor,
  makeNativeTx,
  makeOutput,
  outRefFromByte,
} from "./validation-fixtures.js";

/**
 * A malformed field-6 native script is the one decode failure a validation
 * trace commits, and its forced rejection must name the script the
 * witness-script decoding proof finds malformed. When the shared classifier
 * names no script, the rejection has no subject and the writer has no exact
 * reason to commit, so the trace must refuse the transaction rather than
 * commit it. The classifier is stubbed here to name no script for a genuinely
 * malformed native item, which the transaction's one mint policy runs, so the
 * trace would otherwise reach the native-script execution and commit.
 */
vi.mock(
  "../src/ledger-tx/codec.project-midgard-raw-envelope-for-phase-av1.js",
  async (importOriginal) => {
    const original =
      await importOriginal<
        typeof import("../src/ledger-tx/codec.project-midgard-raw-envelope-for-phase-av1.js")
      >();
    return {
      ...original,
      projectMidgardMalformedNativeWitnessEnvelopeV1: (
        ...args: Parameters<
          typeof original.projectMidgardMalformedNativeWitnessEnvelopeV1
        >
      ) => {
        const found = original.projectMidgardMalformedNativeWitnessEnvelopeV1(
          ...args,
        );
        return found === null ? null : { ...found, malformedScriptIndex: null };
      },
    };
  },
);

describe("malformed field-6 native script the classifier does not name", () => {
  it("has no rejection subject, and its validation trace is refused", async () => {
    const spent = outRefFromByte(0x7a);
    const spentOutput = makeOutput(FUNDED_OUTPUT_LOVELACE);
    // `[0, h'820700']`: a native payload whose constructor tag does not exist.
    const payload = Buffer.from("820700", "hex");
    const policyId = Buffer.from(
      blake2b(Buffer.concat([Buffer.from([0]), payload]), { dkLen: 28 }),
    );
    const assetName = Buffer.from("31", "hex");
    const baseline = makeNativeTx({
      spendInputs: [spent],
      outputs: [
        makeOutput(
          FUNDED_OUTPUT_LOVELACE,
          undefined,
          new Map([
            [
              policyId.toString("hex"),
              new Map([[assetName.toString("hex"), 1n]]),
            ],
          ]),
        ),
      ],
      mintPreimageCbor: makeMintPreimageCbor(
        new Map([[policyId, new Map([[assetName, 1n]])]]),
      ),
    });
    const malformed = encodeRecomputedNativeTx({
      ...baseline.tx,
      witnessSet: {
        ...baseline.tx.witnessSet,
        scriptTxWitsPreimageCbor: encodeCbor([
          Buffer.from("820043820700", "hex"),
        ]),
      },
    });
    const rejection = await phaseARejection(
      malformed,
      RejectCodes.InvalidFieldType,
    );
    expect(rejection.consensusPhase).toBe("canonicalDecode");
    expect(rejection.subject).toBeUndefined();
    const root = "00".repeat(32);
    const trace = await Effect.runPromise(
      Effect.either(
        buildDeterministicValidationMachineTrace({
          consensusProfile: MIDGARD_CONSENSUS_PROFILE,
          eventKeyCbor: encodeCbor([2n, malformed.txId]),
          sourceKind: "normal",
          blockEndTimeMs: 1_750_000_000_000,
          expectedNetworkId: 0n,
          minFeeA: 0n,
          minFeeB: 0n,
          blockSlot: 100n,
          transactionId: malformed.txId,
          canonicalTransactionCbor: malformed.txCbor,
          priorUtxosRoot: root,
          postUtxosRoot: root,
          ledgerWitnessEntries: [{ outRef: spent, output: spentOutput }],
          expectedLedgerOps: [],
          ledgerMutationSteps: [],
          expectedVerdict: "rejected",
          expectedRejectionCode: "E_INVALID_FIELD_TYPE",
        }),
      ),
    );
    if (Either.isRight(trace))
      throw new Error("the trace committed a rejection that names no script");
    expect(trace.left).toBeInstanceOf(DirectValidationTraceUnavailable);
  });
});
