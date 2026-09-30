import { h28 } from "@al-ft/midgard-test-support/hex";
import { credentialToAddress, Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  DaAttestationDatum,
  incompleteRescueStrandedDaAttestationTxProgram,
} from "../src/index.js";
import {
  expectBuildFailure,
  GOVERNED_COMMITTEE_HASH,
  makeFixture,
  makeRecordingLucid,
  ROTATED_COMMITTEE_HASH,
  run,
} from "./da-attestation-rotation.make-fixture.js";
import { fixtureDatum } from "./da-attestation-rotation.q62-da-redeemer-constructor-abi.js";

describe("Q63c rescue builder", () => {
  it("rescue 1 — assembles the burn and the full-value refund", async () => {
    const fixture = makeFixture();
    const { lucid, record } = makeRecordingLucid();

    await run(
      incompleteRescueStrandedDaAttestationTxProgram(lucid, fixture.contracts, {
        daParamsUtxo: fixture.daParamsUtxo,
        daParamsDatum: fixture.daParamsDatum,
        attestation: fixture.attestation,
        refundAddress: fixture.refundAddress,
        referenceScripts: fixture.referenceScripts,
      }),
    );

    // The governed params must be a reference input: the validator's whole
    // authorization is a comparison against them.
    expect(record.reads).toEqual([
      [
        fixture.daParamsUtxo,
        fixture.referenceScripts.daAttestationMinting,
        fixture.referenceScripts.daAttestationSpending,
      ],
    ]);
    expect(record.collects.map((entry) => entry.inputs)).toEqual([
      [fixture.attestation.utxo],
    ]);
    expect(record.mints[0]?.assets).toEqual({
      [fixture.attestationUnit]: -1n,
    });
    // The DAAT is burnt, so the refund is the attestation's value less that
    // token — no more, and no less.
    expect(record.payments[0]?.address).toBe(fixture.refundAddress);
    expect(record.payments[0]?.assets).toEqual({ lovelace: 5_000_000n });
  });

  it("rescue 2 — refuses an attestation that is still on the governed committee", async () => {
    const fixture = makeFixture();
    const { lucid } = makeRecordingLucid();

    // The single changed field: the attestation's frozen committee hash is the
    // governed one, so it is still in flight and its value is not the
    // rescuer's to take.
    await expectBuildFailure(
      incompleteRescueStrandedDaAttestationTxProgram(lucid, fixture.contracts, {
        daParamsUtxo: fixture.daParamsUtxo,
        daParamsDatum: fixture.daParamsDatum,
        attestation: {
          ...fixture.attestation,
          datum: {
            ...fixture.attestation.datum,
            committee_signers_hash: GOVERNED_COMMITTEE_HASH,
          },
        },
        refundAddress: fixture.refundAddress,
        referenceScripts: fixture.referenceScripts,
      }),
    );
  });

  it("rescue 3 — refuses a refund back into the attestation script", async () => {
    const fixture = makeFixture();
    const { lucid } = makeRecordingLucid();

    // Such an output would be unspendable forever: every spend path requires
    // the UTxO to carry its DAAT, and the DAAT is being burnt.
    await expectBuildFailure(
      incompleteRescueStrandedDaAttestationTxProgram(lucid, fixture.contracts, {
        daParamsUtxo: fixture.daParamsUtxo,
        daParamsDatum: fixture.daParamsDatum,
        attestation: fixture.attestation,
        refundAddress: fixture.contracts.daAttestation.spendingScriptAddress,
        referenceScripts: fixture.referenceScripts,
      }),
    );
  });

  it("rescue 4 — refuses a redirect away from the frozen beneficiary", async () => {
    const fixture = makeFixture();
    const { lucid } = makeRecordingLucid();

    await expectBuildFailure(
      incompleteRescueStrandedDaAttestationTxProgram(lucid, fixture.contracts, {
        daParamsUtxo: fixture.daParamsUtxo,
        daParamsDatum: fixture.daParamsDatum,
        attestation: fixture.attestation,
        refundAddress: credentialToAddress("Preprod", {
          type: "Key",
          hash: h28(0x67),
        }),
        referenceScripts: fixture.referenceScripts,
      }),
    );
  });

  it("rescue 5 — the full frozen beneficiary survives datum round trips", () => {
    const encoded = Data.to(
      fixtureDatum() as never,
      DaAttestationDatum as never,
    );
    const decoded = Data.from(
      encoded,
      DaAttestationDatum as never,
    ) as ReturnType<typeof fixtureDatum>;
    expect(decoded.committee_signers_hash).toBe(ROTATED_COMMITTEE_HASH);
    expect(decoded.attestation_count).toBe(1n);
  });
});
