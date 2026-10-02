import {
  encodeMidgardTxOutput,
  protectMidgardAddress,
} from "@al-ft/midgard-core/codec";
import { CML } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterAll, describe, expect, it } from "vitest";

import { RejectCodes } from "../src/index.js";
import { MidgardRedeemerTag } from "../src/midgard-redeemers.js";
import {
  phaseBRejection,
  scriptAddressBytes,
} from "./reject-subject.support.js";
import {
  FUNDED_OUTPUT_LOVELACE,
  hashScriptWitness,
  makeMintPreimageCbor,
  makeOutput,
  makePhaseBCandidate,
  makeProtectedScriptOutput,
  makeRedeemersCbor,
  nativeScriptWitness,
  outRefFromByte,
  plutusV3ScriptWitness,
  TEST_ADDRESS_BYTES,
} from "./validation-fixtures.js";

/**
 * Script-source and execution subjects. Every case puts the fault behind a
 * sound subject of the same kind, so a writer that wrote ordinal zero, counted
 * in field order where the namespace is sorted, or skipped native executions
 * names the sound subject instead and fails.
 */

const foreignKey = CML.PrivateKey.generate_ed25519();
const foreignAddress = CML.EnterpriseAddress.new(
  0,
  CML.Credential.new_pub_key(foreignKey.to_public().hash()),
).to_address();
afterAll(() => {
  foreignAddress.free();
  foreignKey.free();
});

const funded = makeOutput(FUNDED_OUTPUT_LOVELACE);
const passing = nativeScriptWitness({ type: "all", scripts: [] });
const PASSING_HASH = hashScriptWitness(passing);
/** Sorts after every real script hash, so it is always the second purpose. */
const ABSENT_HASH = "ff".repeat(28);
const lockedBy = (scriptHash: string): Buffer =>
  makeOutput(FUNDED_OUTPUT_LOVELACE, scriptAddressBytes(scriptHash));
const mintOf = (...policies: readonly string[]): Buffer =>
  makeMintPreimageCbor(
    new Map(
      policies.map((policy) => [
        Buffer.from(policy, "hex"),
        new Map([[Buffer.from("aa", "hex"), 1n]]),
      ]),
    ),
  );

describe("phase B script-source subjects", () => {
  const [a, b] = [outRefFromByte(0x51), outRefFromByte(0x52)];

  it("names the observer without a source by its purpose index", async () => {
    const rejection = await phaseBRejection(
      makePhaseBCandidate({
        spent: [a],
        scriptWitnesses: [passing],
        requiredObserverItems: [
          Buffer.from(PASSING_HASH, "hex"),
          Buffer.from(ABSENT_HASH, "hex"),
        ],
      }),
      [[a, funded]],
      RejectCodes.MissingRequiredWitness,
    );
    expect(rejection.subject).toStrictEqual({
      arm: "ScriptSourceMissing",
      purposeKind: 2n,
      purposeIndex: 1n,
    });
  });

  it("names the mint policy without a source by its purpose index", async () => {
    const rejection = await phaseBRejection(
      makePhaseBCandidate({
        spent: [a],
        scriptWitnesses: [passing],
        mintPreimageCbor: mintOf(PASSING_HASH, ABSENT_HASH),
      }),
      [[a, funded]],
      RejectCodes.MissingRequiredWitness,
    );
    expect(rejection.subject).toStrictEqual({
      arm: "ScriptSourceMissing",
      purposeKind: 1n,
      purposeIndex: 1n,
    });
  });

  it("names the spend without a source by its sorted purpose index", async () => {
    // Field order [b, a]; the spend namespace is sorted, so b is spend 1.
    const rejection = await phaseBRejection(
      makePhaseBCandidate({ spent: [b, a] }),
      [
        [a, funded],
        [b, lockedBy(ABSENT_HASH)],
      ],
      RejectCodes.MissingRequiredWitness,
    );
    expect(rejection.subject).toStrictEqual({
      arm: "ScriptSourceMissing",
      purposeKind: 0n,
      purposeIndex: 1n,
    });
  });

  it("names the receiving script without a source by its purpose index", async () => {
    const rejection = await phaseBRejection(
      makePhaseBCandidate({
        spent: [a],
        scriptWitnesses: [passing],
        outputs: [
          makeProtectedScriptOutput(PASSING_HASH, FUNDED_OUTPUT_LOVELACE),
          makeProtectedScriptOutput(ABSENT_HASH, FUNDED_OUTPUT_LOVELACE),
        ],
      }),
      [[a, funded]],
      RejectCodes.MissingRequiredWitness,
    );
    expect(rejection.subject).toStrictEqual({
      arm: "ScriptSourceMissing",
      purposeKind: 3n,
      purposeIndex: 1n,
    });
  });

  it("names the Plutus mint purpose without a redeemer", async () => {
    const policies = [0x01, 0x02].map((byte) =>
      plutusV3ScriptWitness(Buffer.from([byte])),
    );
    const rejection = await phaseBRejection(
      makePhaseBCandidate({
        spent: [a],
        scriptWitnesses: policies,
        mintPreimageCbor: mintOf(...policies.map(hashScriptWitness)),
        redeemerTxWitsPreimageCbor: makeRedeemersCbor([
          { tag: MidgardRedeemerTag.Mint, index: 0n },
        ]),
        scriptLanguages: ["PlutusV3"],
      }),
      [[a, funded]],
      RejectCodes.MissingRequiredWitness,
    );
    expect(rejection.subject).toStrictEqual({
      arm: "RedeemerMissing",
      purposeKind: 1n,
      purposeIndex: 1n,
    });
  });

  it("names the protected output whose key did not sign", async () => {
    const rejection = await phaseBRejection(
      makePhaseBCandidate({
        spent: [a],
        outputs: [
          funded,
          makeOutput(
            FUNDED_OUTPUT_LOVELACE,
            protectMidgardAddress(Buffer.from(foreignAddress.to_raw_bytes())),
          ),
        ],
      }),
      [[a, funded]],
      RejectCodes.MissingRequiredWitness,
    );
    expect(rejection.subject).toStrictEqual({
      arm: "ProtectedOutputSignerMissing",
      index: 1n,
    });
  });

  it("names the later of two identical script witnesses as unused", async () => {
    const rejection = await phaseBRejection(
      makePhaseBCandidate({
        spent: [a],
        scriptWitnesses: [passing, passing],
      }),
      [[a, lockedBy(PASSING_HASH)]],
      RejectCodes.InvalidFieldType,
    );
    expect(rejection.subject).toStrictEqual({
      arm: "UnusedScriptWitness",
      index: 1n,
    });
  });

  it("names the later of two redeemers with one pointer as unused", async () => {
    const policy = plutusV3ScriptWitness(Buffer.from([0x01]));
    const rejection = await phaseBRejection(
      makePhaseBCandidate({
        spent: [a],
        scriptWitnesses: [policy],
        mintPreimageCbor: mintOf(hashScriptWitness(policy)),
        redeemerTxWitsPreimageCbor: makeRedeemersCbor([
          { tag: MidgardRedeemerTag.Mint, index: 0n },
          { tag: MidgardRedeemerTag.Mint, index: 0n },
        ]),
        scriptLanguages: ["PlutusV3"],
      }),
      [[a, funded]],
      RejectCodes.InvalidFieldType,
    );
    expect(rejection.subject).toStrictEqual({
      arm: "UnusedRedeemer",
      index: 1n,
    });
  });
});

describe("phase B execution subjects", () => {
  const [a, r] = [outRefFromByte(0x61), outRefFromByte(0x62)];

  it("names the failing native execution after a passing one", async () => {
    // Inline natives are all checked in Phase A, so the failing one is a
    // reference script. Spend (execution 0) passes; mint (execution 1) fails.
    const failing = nativeScriptWitness({
      type: "sig",
      keyHash: Buffer.alloc(28, 0x06),
    });
    const rejection = await phaseBRejection(
      makePhaseBCandidate({
        spent: [a],
        referenceInputs: [r],
        scriptWitnesses: [passing],
        mintPreimageCbor: mintOf(hashScriptWitness(failing)),
      }),
      [
        [a, lockedBy(PASSING_HASH)],
        [
          r,
          encodeMidgardTxOutput({
            address: TEST_ADDRESS_BYTES,
            value: { lovelace: FUNDED_OUTPUT_LOVELACE, assets: new Map() },
            script_ref: failing,
          }),
        ],
      ],
      RejectCodes.NativeScriptInvalid,
    );
    expect(rejection.subject).toStrictEqual({
      arm: "ExecutionNativeScriptFalse",
      index: 1n,
    });
  });

  it("counts a native spend execution before the failing Plutus mint", async () => {
    const policy = plutusV3ScriptWitness(Buffer.from([0x01]));
    const rejection = await phaseBRejection(
      makePhaseBCandidate({
        spent: [a],
        scriptWitnesses: [passing, policy],
        mintPreimageCbor: mintOf(hashScriptWitness(policy)),
        redeemerTxWitsPreimageCbor: makeRedeemersCbor([
          { tag: MidgardRedeemerTag.Mint, index: 0n },
        ]),
        scriptLanguages: ["PlutusV3"],
      }),
      [[a, lockedBy(PASSING_HASH)]],
      RejectCodes.PlutusScriptInvalid,
      {
        evaluateProofScript: () =>
          Effect.succeed({ kind: "script_invalid", detail: "fixture" }),
      },
    );
    expect(rejection.subject).toStrictEqual({
      arm: "PlutusExecutionFailed",
      index: 1n,
    });
  });
});
