import {
  type RedeemerContext,
  type TxOutput,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  applyDaAttestationSignatureWitnesses,
  type DaAttestationBuildError,
  type DaAttestationBuildFailureReason,
  type DaAttestationUtxo,
  EMPTY_ATTESTED_SIGNER_BITMAP,
  encodeDaAttestationSignatureWitnesses,
  signerIndexIsDaAttested,
} from "../src/index.js";
import {
  makeFixture,
  type Recording,
  signature,
} from "./da-attestation.make-fixture.js";

export const outRefKey = (utxo: UTxO): string =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

/**
 * The contract is *which* UTxOs the builder puts in the reference-input set,
 * not the order of the `readFrom` calls that got them there: the ledger sorts
 * reference inputs canonically before any validator sees them.
 */
export const referenceSet = (record: Recording): readonly string[] =>
  [...new Set(record.reads.flat().map(outRefKey))].sort();

export const collectedSet = (record: Recording): readonly string[] =>
  [
    ...new Set(record.collects.flatMap(({ inputs }) => inputs).map(outRefKey)),
  ].sort();

export const expectedSet = (utxos: readonly UTxO[]): readonly string[] =>
  [...new Set(utxos.map(outRefKey))].sort();

export const run = <A>(
  program: Effect.Effect<A, DaAttestationBuildError>,
): Promise<A> => Effect.runPromise(program);

/**
 * A refusal is only evidence when it is *this* refusal: every negative below
 * names the precondition it means to trip, so a fixture that happens to be
 * malformed for some unrelated reason no longer satisfies the test.
 */
export const expectBuildRefusal = async <A>(
  program: Effect.Effect<A, DaAttestationBuildError>,
  reason: DaAttestationBuildFailureReason,
): Promise<void> => {
  const result = await Effect.runPromise(Effect.either(program));
  expect(result._tag).toBe("Left");
  if (result._tag === "Left") {
    expect(result.left._tag).toBe("DaAttestationBuildError");
    expect(result.left.reason, result.left.message).toBe(reason);
  }
};

describe("DA attestation witness helpers", () => {
  it("sorts and packs indexed witnesses deterministically", async () => {
    await expect(
      run(
        encodeDaAttestationSignatureWitnesses([
          { signerIndex: 2, signatureHex: signature("bb") },
          { signerIndex: 0, signatureHex: signature("aa") },
        ]),
      ),
    ).resolves.toBe(`00${signature("aa")}02${signature("bb")}`);
  });

  it("applies witnesses to the MSB-first bitmap", async () => {
    const result = await run(
      applyDaAttestationSignatureWitnesses({
        attestedSignersHex: EMPTY_ATTESTED_SIGNER_BITMAP,
        witnesses: [
          { signerIndex: 1, signatureHex: signature("bb") },
          { signerIndex: 0, signatureHex: signature("aa") },
        ],
        committeeSize: 2,
      }),
    );

    expect(result.attestedSigners).toBe(`c0${"00".repeat(31)}`);
    expect(result.attestationCount).toBe(2n);
    expect(result.packedWitnesses).toBe(
      `00${signature("aa")}01${signature("bb")}`,
    );
    expect(signerIndexIsDaAttested(result.attestedSigners, 0)).toBe(true);
    expect(signerIndexIsDaAttested(result.attestedSigners, 1)).toBe(true);
    expect(signerIndexIsDaAttested(result.attestedSigners, 2)).toBe(false);
  });

  it("rejects malformed, duplicate, already-attested, and out-of-committee witnesses", async () => {
    await expectBuildRefusal(
      encodeDaAttestationSignatureWitnesses([
        { signerIndex: 0, signatureHex: "aa" },
      ]),
      "invalid_signature_hex",
    );
    await expectBuildRefusal(
      encodeDaAttestationSignatureWitnesses([
        { signerIndex: 0, signatureHex: signature("aa") },
        { signerIndex: 0, signatureHex: signature("bb") },
      ]),
      "duplicate_signature_witness",
    );
    await expectBuildRefusal(
      applyDaAttestationSignatureWitnesses({
        attestedSignersHex: `80${"00".repeat(31)}`,
        witnesses: [{ signerIndex: 0, signatureHex: signature("aa") }],
      }),
      "signer_already_attested",
    );
    await expectBuildRefusal(
      applyDaAttestationSignatureWitnesses({
        attestedSignersHex: EMPTY_ATTESTED_SIGNER_BITMAP,
        witnesses: [{ signerIndex: 2, signatureHex: signature("aa") }],
        committeeSize: 2,
      }),
      "signer_outside_committee",
    );
  });
});

export const thresholdAttestation = (
  fixture: ReturnType<typeof makeFixture>,
): DaAttestationUtxo => ({
  ...fixture.attestation,
  datum: {
    ...fixture.attestation.datum,
    attested_signers: `c0${"00".repeat(31)}`,
    attestation_count: 2n,
  },
});

export const applyConfig = (fixture: ReturnType<typeof makeFixture>) => ({
  daParamsUtxo: fixture.daParamsUtxo,
  daParamsDatum: fixture.daParamsDatum,
  target: fixture.target,
  attestation: thresholdAttestation(fixture),
  referenceScripts: fixture.referenceScripts,
  validityRange: fixture.applyValidityRange,
  availabilityParameters: fixture.availabilityParameters,
});

export const recordedOutputs = (record: Recording): TxOutput[] =>
  record.payments.map((payment) => ({
    address: payment.address,
    assets: payment.assets,
    datum: payment.datum?.value ?? null,
  }));

export const applyMintRedeemer = (
  record: Recording,
): ((ctx: RedeemerContext) => string) => {
  const redeemer = record.mints[0]?.redeemer;
  if (typeof redeemer !== "function") {
    throw new Error("apply mint redeemer is not a context builder");
  }
  return redeemer as (ctx: RedeemerContext) => string;
};

/**
 * The script-context projection the ledger would hand the apply mint: spent
 * inputs in canonical (tx hash, index) order, reference inputs exactly as the
 * builder read them (the helper under test must sort them itself), and the
 * given final outputs.
 */
export const applyMintContext = (
  fixture: ReturnType<typeof makeFixture>,
  record: Recording,
  outputs: readonly TxOutput[],
): RedeemerContext => {
  const inputs = record.collects
    .flatMap(({ inputs: collected }) => collected)
    .sort((left, right) => {
      const l = outRefKey(left),
        r = outRefKey(right);
      return l < r ? -1 : l > r ? 1 : 0;
    });
  const ownPurpose = {
    tag: "mint",
    index: 0n,
    policyId: fixture.contracts.daAttestation.policyId,
    redeemerListIndex: 0n,
  } as const;
  return {
    inputs,
    referenceInputs: record.reads.flat(),
    outputs,
    redeemers: [ownPurpose],
    ownPurpose,
    inputIndex: (input: Pick<UTxO, "txHash" | "outputIndex">) => {
      const index = inputs.findIndex(
        (candidate) =>
          candidate.txHash === input.txHash &&
          candidate.outputIndex === input.outputIndex,
      );
      return index < 0 ? undefined : BigInt(index);
    },
  } as unknown as RedeemerContext;
};
