import * as SDK from "@al-ft/midgard-sdk";
import { CML, Data } from "@lucid-evolution/lucid";
import { LucidDaAttestationSubmitter } from "da-committee-node/coordinator/lucid-submitter";
import { buildApplyAttestationTx } from "da-committee-node/coordinator/tx-builders";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

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
        SDK.daAttestationApplyValidityRangeProgram({
          currentTime,
          headerEndTime,
        }),
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
        SDK.daAttestationApplyValidityRangeProgram({
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

// A live L1 tip trails the wall clock by seconds, and the ledger checks the
// validity interval against the tip. The emulator's slot is the tip here, and
// the committee submitter's injected clock runs ahead of it the way a
// preprod submitter's wall clock runs ahead of the chain.
describe("DA committee apply against an L1 tip that trails the submitter's clock", () => {
  const CLOCK_AHEAD_OF_TIP_MS = 8_000n;
  // Fixture setup advances the emulator past the selected profile's attestation
  // timeout, so the header ends later to keep the apply deadline ahead.
  const HEADER_END_TIME_LEAD_MS = 900_000;

  it("refuses an apply opening at the submitter's clock and lands the committee submitter's apply", async () => {
    const f = await createAvailabilityFixture(1, 0, HEADER_END_TIME_LEAD_MS);
    const attestation = await thresholdAttestation(f);
    const deadline =
      f.target.stateQueueNode.header.endTime + SDK.DA_ATTESTATION_TIMEOUT_MS;
    const submitterClock = () =>
      BigInt(f.emulator.now()) + CLOCK_AHEAD_OF_TIP_MS;
    expect(deadline - submitterClock()).toBeGreaterThan(
      SDK.MAX_VALIDITY_RANGE_LENGTH_MS,
    );

    // Negative: a range opening at the submitter's clock is in the tip's
    // future, so the ledger refuses it on the lower bound alone. Completion
    // evaluates the scripts and passes, because apply bounds time only from
    // above.
    const mutantFrom = submitterClock();
    const mutant = await (
      await buildApplyAttestationTx({
        lucid: f.lucid,
        contracts: f.contracts,
        target: f.target,
        attestationUtxo: attestation.utxo,
        attestationDatum: attestation.datum,
        hubOracleRefInput: f.hubOracleRefInput,
        daParamsUtxo: f.daParamsUtxo,
        daParamsDatum: f.daParamsDatum,
        referenceScripts: f.daReferences,
        validityRange: { validFrom: mutantFrom, validTo: mutantFrom + 60_000n },
      })
    ).sign
      .withWallet()
      .complete();
    const tipSlot = f.emulator.slot;
    await expect(mutant.submit()).rejects.toThrow(
      `Lower bound (${(tipSlot + Number(CLOCK_AHEAD_OF_TIP_MS / 1000n)).toString()}) not in slot range (${tipSlot.toString()}).`,
    );
    expect(await f.lucid.utxosByOutRef([attestation.utxo])).toHaveLength(1);

    // Positive: the committee submitter derives its range from the same clock
    // and the same tip accepts its apply.
    const submittedIntervals: {
      readonly start: bigint | undefined;
      readonly ttl: bigint | undefined;
      readonly tipSlot: number;
    }[] = [];
    const submitter = new LucidDaAttestationSubmitter({
      lucid: f.lucid,
      contracts: f.contracts,
      referenceScripts: f.daReferences,
      availabilityParameters: TEST_AVAILABILITY_PARAMETERS,
      currentTime: submitterClock,
      refreshFundingUtxos: async () => {},
      signSubmit: async (tx) => {
        const signed = await tx.sign.withWallet().complete();
        const body = CML.Transaction.from_cbor_hex(signed.toCBOR()).body();
        submittedIntervals.push({
          start: body.validity_interval_start(),
          ttl: body.ttl(),
          tipSlot: f.emulator.slot,
        });
        const txHash = await signed.submit();
        f.emulator.awaitBlock(1);
        return txHash;
      },
      postSubmitVerificationRetryCount: 0,
      postSubmitVerificationDelayMs: 0,
    });
    const clockAtSubmit = submitterClock();
    await expect(
      submitter.applyAttestation({
        record: { headerHash: f.target.headerHash } as never,
        candidate: {
          headerHash: f.target.headerHash,
          outRef: `${attestation.utxo.txHash}#${attestation.utxo.outputIndex.toString()}`,
        } as never,
      }),
    ).resolves.toMatchObject({ status: "submitted" });
    expect(submittedIntervals).toHaveLength(1);
    const [interval] = submittedIntervals;
    const validFrom =
      clockAtSubmit - SDK.DA_ATTESTATION_APPLY_SLOT_LAG_ALLOWANCE_MS;
    expect(interval!.start).toBe(
      BigInt(f.lucid.unixTimeToSlot(Number(validFrom))),
    );
    expect(interval!.start!).toBeLessThanOrEqual(BigInt(interval!.tipSlot));
    expect(interval!.ttl).toBe(
      BigInt(
        f.lucid.unixTimeToSlot(
          Number(validFrom + SDK.MAX_VALIDITY_RANGE_LENGTH_MS),
        ),
      ),
    );
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
  }, 180_000);
});
