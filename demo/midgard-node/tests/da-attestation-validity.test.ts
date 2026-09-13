import * as SDK from "@al-ft/midgard-sdk";
import { CML, Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { daAttestationApplyValidityRangeProgram } from "../src/transactions/da-attestation.js";
import { TEST_AVAILABILITY_PARAMETERS } from "./helpers/availability-challenge.js";
import { createAvailabilityFixture } from "./helpers/availability-challenge-emulator.js";

const thresholdAttestation = async (
  f: Awaited<ReturnType<typeof createAvailabilityFixture>>,
) => {
  const { lucid, contracts } = f;
  const init = await Effect.runPromise(
    SDK.incompleteInitDaAttestationTxProgram(lucid, contracts, {
      daParamsUtxo: f.daParamsUtxo,
      daParamsDatum: f.daParamsDatum,
      target: f.target,
      referenceScripts: f.daReferences,
      attestationOutputLovelace: TEST_AVAILABILITY_PARAMETERS.da_bond_lovelace,
      rescueBeneficiary: await Effect.runPromise(
        SDK.addressDataFromBech32(f.responder.address),
      ),
      availabilityCommitment: f.commitment,
    }),
  );
  await f.submit("attestation init", init, true);
  const unit = SDK.daAttestationUnit(
    contracts.daAttestation,
    f.target.headerHash,
  );
  const current = async (): Promise<SDK.DaAttestationUtxo> => {
    const [utxo] = await lucid.utxosAtWithUnit(
      contracts.daAttestation.spendingScriptAddress,
      unit,
    );
    if (!utxo?.datum) throw new Error("Missing real attestation output");
    return { utxo, datum: Data.from(utxo.datum, SDK.DaAttestationDatum) };
  };
  const add = await Effect.runPromise(
    SDK.incompleteAddDaAttestationSignaturesTxProgram(lucid, contracts, {
      daParamsUtxo: f.daParamsUtxo,
      daParamsDatum: f.daParamsDatum,
      attestation: await current(),
      witnesses: f.committeeKeys.map((key, signerIndex) => ({
        signerIndex,
        signatureHex: Buffer.from(
          key
            .sign(SDK.daAvailabilityAttestationMessage(f.commitment))
            .to_raw_bytes(),
        ).toString("hex"),
      })),
      referenceScripts: f.daReferences,
    }),
  );
  await f.submit("attestation threshold signatures", add, true);
  return current();
};

describe("DA apply validity at production cadence", () => {
  it.each([false, true])(
    "evaluates and submits the actual apply with deadline clipping=%s",
    async (nearDeadline) => {
      const f = await createAvailabilityFixture(1);
      const attestation = await thresholdAttestation(f);
      const headerEndTime = f.target.stateQueueNode.header.endTime;
      const deadline = headerEndTime + SDK.DA_ATTESTATION_TIMEOUT_MS;
      if (nearDeadline)
        f.emulator.awaitSlot(
          Math.floor(Number(deadline - BigInt(f.emulator.now())) / 1000) - 120,
        );
      const currentTime = BigInt(f.emulator.now());
      const validityRange = await Effect.runPromise(
        daAttestationApplyValidityRangeProgram({ currentTime, headerEndTime }),
      );
      expect(validityRange.validFrom).toBe(currentTime - 60_000n);
      expect(validityRange.validTo).toBe(
        nearDeadline ? deadline : currentTime + 420_000n,
      );
      const config = {
        daParamsUtxo: f.daParamsUtxo,
        daParamsDatum: f.daParamsDatum,
        attestation,
        target: f.target,
        referenceScripts: f.daReferences,
        hubOracleRefInput: f.hubOracleRefInput,
        validityRange,
      };
      // The planner does not relax either existing SDK boundary.
      for (const [range, reason] of [
        [
          {
            validFrom: currentTime,
            validTo: currentTime + SDK.MAX_VALIDITY_RANGE_LENGTH_MS + 1n,
          },
          "invalid_validity_range",
        ],
        [
          { validFrom: deadline - 60_000n, validTo: deadline + 1n },
          "validity_range_past_deadline",
        ],
      ] as const) {
        const result = await Effect.runPromise(
          SDK.incompleteApplyDaAttestationToStateQueueTxProgram(
            f.lucid,
            f.contracts,
            {
              ...config,
              validityRange: range,
            },
          ).pipe(Effect.either),
        );
        expect(result).toMatchObject({ _tag: "Left", left: { reason } });
      }
      const builder = await Effect.runPromise(
        SDK.incompleteApplyDaAttestationToStateQueueTxProgram(
          f.lucid,
          f.contracts,
          config,
        ),
      );
      const signed = await (
        await builder.complete({ localUPLCEval: true })
      ).sign
        .withWallet()
        .complete();
      const body = CML.Transaction.from_cbor_hex(signed.toCBOR()).body();
      expect(body.validity_interval_start()).toBe(BigInt(f.emulator.slot - 60));
      expect(body.ttl()).toBe(
        BigInt(f.lucid.unixTimeToSlot(Number(validityRange.validTo))),
      );
      expect(body.ttl()! - body.validity_interval_start()!).toBeLessThanOrEqual(
        480n,
      );
      // Three minutes exceeds the inherited two-minute total range, while the
      // clipped case still gets one minute of real inclusion allowance.
      f.emulator.awaitSlot(nearDeadline ? 60 : 180);
      await signed.submit();
      f.emulator.awaitBlock(1);
      expect(await f.lucid.utxosByOutRef([attestation.utxo])).toEqual([]);
      const [queue] = await f.lucid.utxosAtWithUnit(
        f.contracts.stateQueue.spendingScriptAddress,
        f.queueUnit,
      );
      const view = await Effect.runPromise(
        SDK.getLinkedListNodeViewFromUTxO(queue!),
      );
      expect(
        Data.castFrom(view.data, SDK.StateQueueNode).da_attestation,
      ).toHaveProperty("Attested");
    },
    180_000,
  );

  it.each([0n, 1n])(
    "refuses planning at or after the deadline (+%sms)",
    async (elapsed) => {
      const headerEndTime = 1_000_000n;
      const result = await Effect.runPromise(
        daAttestationApplyValidityRangeProgram({
          headerEndTime,
          currentTime: headerEndTime + SDK.DA_ATTESTATION_TIMEOUT_MS + elapsed,
        }).pipe(Effect.either),
      );
      expect(result).toMatchObject({
        _tag: "Left",
        left: { reason: "validity_range_past_deadline" },
      });
    },
  );
});
