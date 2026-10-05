import { CML } from "@lucid-evolution/lucid";
import { afterAll, describe, expect, it } from "vitest";

import { RejectCodes } from "../src/index.js";
import { MidgardRedeemerTag } from "../src/midgard-redeemers.js";
import {
  phaseARejection,
  phaseBRejection,
  scriptAddressBytes,
} from "./reject-subject.support.js";
import {
  encodeByteList,
  encodeRecomputedNativeTx,
  FUNDED_OUTPUT_LOVELACE,
  hashScriptWitness,
  makeNativeTx,
  makeOutput,
  makePhaseBCandidate,
  makeProtectedScriptOutput,
  makeRedeemersCbor,
  nativeScriptWitness,
  outRefFromByte,
  plutusV3ScriptWitness,
  TEST_SIGNER_HASH,
} from "./validation-fixtures.js";
import { TEST_PRIVATE_KEY } from "./validation-fixtures.make-min-ada-funded-exact-size-output-item.js";

/**
 * The subject a rejection records is the arm and coordinate a forced verdict
 * commits, and the fault proofs reopen exactly that coordinate. Each case
 * puts the fault at a non-zero ordinal so a writer that ignored the subject
 * (and wrote ordinal zero) is caught.
 */

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
    // Witness 0 signs the transaction id; witness 1 signs other bytes.
    const base = makeNativeTx();
    const witness = (message: Buffer, key: CML.PrivateKey) =>
      Buffer.from(
        CML.make_vkey_witness(
          CML.TransactionHash.from_raw_bytes(message),
          key,
        ).to_cbor_bytes(),
      );
    const fixture = encodeRecomputedNativeTx({
      ...base.tx,
      witnessSet: {
        ...base.tx.witnessSet,
        addrTxWitsPreimageCbor: encodeByteList([
          witness(base.txId, TEST_PRIVATE_KEY),
          witness(Buffer.alloc(32, 0x7f), foreignKey),
        ]),
      },
    });
    expect(fixture.txId).toStrictEqual(base.txId);
    const rejection = await phaseARejection(
      fixture,
      RejectCodes.InvalidSignature,
    );
    expect(rejection.subject).toStrictEqual({
      arm: "AddressWitnessSignatureInvalid",
      index: 1n,
    });
  });

  it("names the first required observer out of order", async () => {
    const rejection = await phaseARejection(
      makeNativeTx({
        requiredObserverItems: [
          Buffer.alloc(28, 0x01),
          Buffer.alloc(28, 0x03),
          Buffer.alloc(28, 0x02),
        ],
      }),
      RejectCodes.InvalidFieldType,
    );
    expect(rejection.subject).toStrictEqual({
      arm: "ObserverOrderInvalid",
      index: 2n,
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
    // Redeemer 0 serves the Plutus spend; redeemer 1 points at no purpose.
    const script = plutusV3ScriptWitness(Buffer.from("01", "hex"));
    const rejection = await phaseBRejection(
      makePhaseBCandidate({
        spent: [a],
        scriptWitnesses: [script],
        scriptLanguages: ["PlutusV3"],
        redeemerTxWitsPreimageCbor: makeRedeemersCbor([
          { tag: MidgardRedeemerTag.Spend, index: 0n },
          { tag: MidgardRedeemerTag.Mint, index: 0n },
        ]),
      }),
      [
        [
          a,
          makeOutput(
            FUNDED_OUTPUT_LOVELACE,
            scriptAddressBytes(hashScriptWitness(script)),
          ),
        ],
      ],
      RejectCodes.InvalidFieldType,
    );
    expect(rejection.subject).toStrictEqual({
      arm: "UnusedRedeemer",
      index: 1n,
    });
  });

  it("names the missing reference input by its field position", async () => {
    const [r0, r1] = [outRefFromByte(0x43), outRefFromByte(0x44)];
    const rejection = await phaseBRejection(
      makePhaseBCandidate({ spent: [a], referenceInputs: [r0, r1] }),
      [
        [a, funded],
        [r0, funded],
      ],
      RejectCodes.InputNotFound,
    );
    expect(rejection.subject).toStrictEqual({
      arm: "InputNotFound",
      sourceKind: 1n,
      index: 1n,
    });
  });

  it("names the reference input whose resolved output fails to decode", async () => {
    const [r0, r1] = [outRefFromByte(0x45), outRefFromByte(0x46)];
    const rejection = await phaseBRejection(
      makePhaseBCandidate({ spent: [a], referenceInputs: [r0, r1] }),
      [
        [a, funded],
        [r0, funded],
        [r1, Buffer.from("ff", "hex")],
      ],
      RejectCodes.InvalidOutput,
    );
    expect(rejection.subject).toStrictEqual({
      arm: "InputSpentOutputNonCanonical",
      sourceKind: 1n,
      index: 1n,
    });
  });

  it("counts a native spend execution before the PlutusV3 receive execution", async () => {
    const native = nativeScriptWitness({ type: "all", scripts: [] });
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
        scriptWitnesses: [native, script],
        redeemerTxWitsPreimageCbor: makeRedeemersCbor([
          { tag: MidgardRedeemerTag.Receiving, index: 0n },
        ]),
        scriptLanguages: ["PlutusV3"],
      }),
      [
        [
          a,
          makeOutput(
            FUNDED_OUTPUT_LOVELACE,
            scriptAddressBytes(hashScriptWitness(native)),
          ),
        ],
      ],
      RejectCodes.PlutusScriptInvalid,
    );
    expect(rejection.subject).toStrictEqual({
      arm: "ReceivePurposePlutusV3Forbidden",
      index: 1n,
    });
  });
});
