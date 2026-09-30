import { CML } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterAll, describe, expect, it } from "vitest";

import {
  RejectCodes,
  runPhaseAValidation,
  runPhaseBValidationWithPatch,
} from "../src/index.js";
import { MidgardRedeemerTag } from "../src/midgard-redeemers.js";
import type {
  PhaseAValidatedTx,
  RejectCode,
  RejectedTx,
} from "../src/types.js";
import {
  FUNDED_OUTPUT_LOVELACE,
  hashScriptWitness,
  makeNativeTx,
  makeOutput,
  makePhaseBCandidate,
  makeProtectedScriptOutput,
  makeQueued,
  makeRedeemersCbor,
  nativeScriptWitness,
  outRefFromByte,
  plutusV3ScriptWitness,
  TEST_SIGNER_HASH,
} from "./validation-fixtures.js";

/**
 * The subject a rejection records is the arm and coordinate a forced verdict
 * commits, and the fault proofs reopen exactly that coordinate. Each case
 * puts the fault at a non-zero ordinal so a writer that ignored the subject
 * (and wrote ordinal zero) is caught.
 */

const phaseAConfig = {
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  concurrency: 1,
  strictnessProfile: "phase-a-unit",
};

const phaseARejection = async (
  fixture: ReturnType<typeof makeNativeTx>,
  code: RejectCode,
): Promise<RejectedTx> => {
  const result = await Effect.runPromise(
    runPhaseAValidation(
      [makeQueued(fixture.txId, fixture.txCbor)],
      phaseAConfig,
    ),
  );
  expect(result.accepted).toHaveLength(0);
  expect(result.rejected).toHaveLength(1);
  expect(result.rejected[0]!.code).toBe(code);
  return result.rejected[0]!;
};

const phaseBRejection = async (
  candidate: PhaseAValidatedTx,
  state: readonly (readonly [Buffer, Buffer])[],
  code: RejectCode,
): Promise<RejectedTx> => {
  const result = await Effect.runPromise(
    runPhaseBValidationWithPatch(
      [candidate],
      new Map(
        state.map(([outRef, output]) => [outRef.toString("hex"), output]),
      ),
      { nowCardanoSlotNo: 100n, bucketConcurrency: 1 },
    ),
  );
  expect(result.accepted).toHaveLength(0);
  expect(result.rejected).toHaveLength(1);
  expect(result.rejected[0]!.code).toBe(code);
  return result.rejected[0]!;
};

const foreignKey = CML.PrivateKey.generate_ed25519();
const foreignAddress = CML.EnterpriseAddress.new(
  0,
  CML.Credential.new_pub_key(foreignKey.to_public().hash()),
).to_address();
const FOREIGN_ADDRESS_BYTES = Buffer.from(foreignAddress.to_raw_bytes());
afterAll(() => {
  foreignAddress.free();
  foreignKey.free();
});

const funded = makeOutput(FUNDED_OUTPUT_LOVELACE);

describe("phase A rejection subjects", () => {
  it("names both positions of a duplicated spend input", async () => {
    const [a, b] = [outRefFromByte(0x31), outRefFromByte(0x32)];
    const rejection = await phaseARejection(
      makeNativeTx({ spendInputs: [a, b, a] }),
      RejectCodes.DuplicateInputInTx,
    );
    expect(rejection.subject).toStrictEqual({
      arm: "DuplicateInput",
      first: { fieldIndex: 0n, itemIndex: 0n },
      second: { fieldIndex: 0n, itemIndex: 2n },
    });
  });

  it("names both positions of a duplicated reference input", async () => {
    const [a, b, c] = [0x33, 0x34, 0x35].map((byte) => outRefFromByte(byte));
    const rejection = await phaseARejection(
      makeNativeTx({ spendInputs: [c!], referenceInputs: [a!, b!, b!] }),
      RejectCodes.DuplicateInputInTx,
    );
    expect(rejection.subject).toStrictEqual({
      arm: "DuplicateInput",
      first: { fieldIndex: 1n, itemIndex: 1n },
      second: { fieldIndex: 1n, itemIndex: 2n },
    });
  });

  it("names the spend and the reference position of a shared out-ref", async () => {
    const [a, b, c] = [0x36, 0x37, 0x38].map((byte) => outRefFromByte(byte));
    const rejection = await phaseARejection(
      makeNativeTx({ spendInputs: [a!, b!], referenceInputs: [c!, b!] }),
      RejectCodes.DuplicateInputInTx,
    );
    expect(rejection.subject).toStrictEqual({
      arm: "DuplicateInput",
      first: { fieldIndex: 0n, itemIndex: 1n },
      second: { fieldIndex: 1n, itemIndex: 1n },
    });
  });

  it("names the invalid address witness", async () => {
    const rejection = await phaseARejection(
      makeNativeTx({ invalidVkeyWitness: true }),
      RejectCodes.InvalidSignature,
    );
    expect(rejection.subject).toStrictEqual({
      arm: "AddressWitnessSignatureInvalid",
      index: 0n,
    });
  });

  it("names the unsigned required signer", async () => {
    const rejection = await phaseARejection(
      makeNativeTx({
        // Required signers are canonically ordered; the all-0xff hash sorts
        // after the fixture signer's.
        requiredSignerItems: [
          Buffer.from(TEST_SIGNER_HASH, "hex"),
          Buffer.alloc(28, 0xff),
        ],
      }),
      RejectCodes.MissingRequiredWitness,
    );
    expect(rejection.subject).toStrictEqual({
      arm: "RequiredSignerUnsigned",
      index: 1n,
    });
  });

  it("names the native witness script that evaluates false", async () => {
    const rejection = await phaseARejection(
      makeNativeTx({
        scriptWitnesses: [
          nativeScriptWitness({ type: "all", scripts: [] }),
          nativeScriptWitness({ type: "sig", keyHash: Buffer.alloc(28, 0x06) }),
        ],
      }),
      RejectCodes.NativeScriptInvalid,
    );
    expect(rejection.subject).toStrictEqual({
      arm: "WitnessNativeScriptFalse",
      index: 1n,
    });
  });

  it("names the output that fails to decode", async () => {
    const rejection = await phaseARejection(
      makeNativeTx({ outputs: [makeOutput(10n), Buffer.from("ff", "hex")] }),
      RejectCodes.InvalidOutput,
    );
    expect(rejection.subject).toStrictEqual({
      arm: "OutputNonCanonical",
      index: 1n,
    });
  });
});

describe("phase B rejection subjects", () => {
  const [a, b] = [outRefFromByte(0x41), outRefFromByte(0x42)];

  it("names the missing spend input by its field position", async () => {
    const rejection = await phaseBRejection(
      makePhaseBCandidate({ spent: [a, b] }),
      [[a, funded]],
      RejectCodes.InputNotFound,
    );
    expect(rejection.subject).toStrictEqual({
      arm: "InputNotFound",
      sourceKind: 0n,
      index: 1n,
    });
  });

  it("names the spend input whose key did not sign", async () => {
    const rejection = await phaseBRejection(
      makePhaseBCandidate({ spent: [a, b] }),
      [
        [a, funded],
        [b, makeOutput(FUNDED_OUTPUT_LOVELACE, FOREIGN_ADDRESS_BYTES)],
      ],
      RejectCodes.MissingRequiredWitness,
    );
    expect(rejection.subject).toStrictEqual({
      arm: "SpendInputSignerMissing",
      index: 1n,
    });
  });

  it("names the spend input whose resolved output fails to decode", async () => {
    const rejection = await phaseBRejection(
      makePhaseBCandidate({ spent: [a, b] }),
      [
        [a, funded],
        [b, Buffer.from("ff", "hex")],
      ],
      RejectCodes.InvalidOutput,
    );
    expect(rejection.subject).toStrictEqual({
      arm: "InputSpentOutputNonCanonical",
      sourceKind: 0n,
      index: 1n,
    });
  });

  it("names the output below the minimum-Ada floor", async () => {
    const rejection = await phaseBRejection(
      makePhaseBCandidate({ spent: [a], outputs: [funded, makeOutput(1n)] }),
      [[a, funded]],
      RejectCodes.MinAda,
    );
    expect(rejection.subject).toStrictEqual({
      arm: "OutputBelowMinAda",
      index: 1n,
    });
  });

  it("names a plain-path integrity hash mismatch", async () => {
    const candidate = makePhaseBCandidate({ spent: [a] });
    candidate.ledgerTx.scriptIntegrityHash.fill(0xff);
    const rejection = await phaseBRejection(
      candidate,
      [[a, funded]],
      RejectCodes.InvalidFieldType,
    );
    expect(rejection.subject).toStrictEqual({
      arm: "ScriptIntegrityHashMismatch",
    });
  });

  it("names the redeemer no purpose uses", async () => {
    const rejection = await phaseBRejection(
      makePhaseBCandidate({
        spent: [a],
        scriptLanguages: ["PlutusV3"],
        redeemerTxWitsPreimageCbor: makeRedeemersCbor([
          { tag: MidgardRedeemerTag.Mint, index: 0n },
        ]),
      }),
      [[a, funded]],
      RejectCodes.InvalidFieldType,
    );
    expect(rejection.subject).toStrictEqual({
      arm: "UnusedRedeemer",
      index: 0n,
    });
  });

  it("names the PlutusV3 receive execution", async () => {
    const script = plutusV3ScriptWitness(Buffer.from("010203", "hex"));
    const rejection = await phaseBRejection(
      makePhaseBCandidate({
        spent: [a],
        outputs: [
          makeProtectedScriptOutput(
            hashScriptWitness(script),
            FUNDED_OUTPUT_LOVELACE,
          ),
        ],
        scriptWitnesses: [script],
        redeemerTxWitsPreimageCbor: makeRedeemersCbor([
          { tag: MidgardRedeemerTag.Receiving, index: 0n },
        ]),
        scriptLanguages: ["PlutusV3"],
      }),
      [[a, funded]],
      RejectCodes.PlutusScriptInvalid,
    );
    expect(rejection.subject).toStrictEqual({
      arm: "ReceivePurposePlutusV3Forbidden",
      index: 0n,
    });
  });
});
