import * as SDK from "@al-ft/midgard-sdk";
import { CML } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import { TEST_AVAILABILITY_PARAMETERS as parameters } from "./helpers/availability-challenge.js";
import {
  attestAvailability,
  availabilityDeployment,
  type AvailabilityFixture,
} from "./helpers/availability-challenge-emulator.js";

export const deployment = availabilityDeployment;

export const submit = async (
  f: AvailabilityFixture,
  built: SDK.BuiltDaAvailabilityTransaction,
) => {
  const signed = await built.tx.sign.withWallet().complete();
  expect(signed.toCBOR().length / 2).toBeLessThanOrEqual(15872);
  expect(signed.toHash()).toBe(built.txId);
  const redeemers = CML.Transaction.from_cbor_hex(signed.toCBOR())
    .witness_set()
    .redeemers()
    ?.to_flat_format();
  let memory = 0n,
    steps = 0n;
  for (let i = 0; i < (redeemers?.len() ?? 0); i++) {
    memory += redeemers!.get(i).ex_units().mem();
    steps += redeemers!.get(i).ex_units().steps();
  }
  expect(memory).toBeGreaterThan(0n);
  expect(memory).toBeLessThanOrEqual(13_200_000n);
  expect(steps).toBeLessThanOrEqual(8_000_000_000n);
  // G9/H1: plain-ADA collateral in at most three inputs, covering the ledger
  // collateral percentage of the exact fee.
  expect(built.collateralOutRefs.length).toBeGreaterThanOrEqual(1);
  expect(built.collateralOutRefs.length).toBeLessThanOrEqual(3);
  expect(
    built.collateralOutRefs.reduce((t, u) => t + u.assets.lovelace, 0n),
  ).toBeGreaterThanOrEqual((built.feeLovelace * 150n + 99n) / 100n);
  for (const c of built.collateralOutRefs)
    expect(Object.keys(c.assets)).toEqual(["lovelace"]);

  const id = await signed.submit();
  f.emulator.awaitBlock(1);
  return f.lucid.utxosByOutRef(
    built.expectedOutputs.map((_, outputIndex) => ({
      txHash: id,
      outputIndex,
    })),
  );
};

export const resources = async (
  f: AvailabilityFixture,
  feeLovelace: bigint,
  /** A publication's range must close by the challenge's response deadline. */
  responseDeadline?: bigint,
): Promise<SDK.DaAvailabilityTransactionResources> => {
  const collateralInputs = await f.collateralInputs();
  const validFrom = BigInt(f.emulator.now());
  const validTo =
    responseDeadline !== undefined &&
    responseDeadline + 1n < validFrom + 60_000n
      ? responseDeadline + 1n
      : validFrom + 60_000n;
  return { collateralInputs, feeLovelace, validFrom, validTo };
};

/** The timeout derives its own exact fee: resources without one. */
export const timeoutResources = async (f: AvailabilityFixture) => {
  const { feeLovelace: _fee, ...rest } = await resources(f, 1n);
  return rest;
};

export const OPEN_FUNDING_LOVELACE =
  parameters.challenger_bond_lovelace +
  parameters.challenge_record_lovelace +
  parameters.max_open_fee_lovelace;

export const fundChallenger = async (f: AvailabilityFixture, name: string) => {
  f.lucid.selectWallet.fromPrivateKey(f.challenger.privateKey);
  const funding = await f.submit(
    name,
    f.lucid.newTx().pay.ToAddress(f.challenger.address, {
      lovelace: OPEN_FUNDING_LOVELACE,
    }),
    true,
  );
  return funding.find((u) => u.assets.lovelace === OPEN_FUNDING_LOVELACE)!;
};

export const recordOf = (s: SDK.DaAvailabilityChallengeSnapshot) => {
  if (!s.recordDatum) throw new Error("Expected a challenge record");
  return s.recordDatum;
};

export const open = async (
  f: AvailabilityFixture,
  d: SDK.DaAvailabilityDeployment,
) => {
  const attested = await attestAvailability(f);
  const challengerFunding = await fundChallenger(
    f,
    "prepare challenger resources",
  );
  const p = {
    ...(await resources(f, parameters.max_open_fee_lovelace)),
    commitment: attested.commitment,
    queue: attested.queue,
    challengerFunding,
    challenger: f.challengerKey,
    daChallengeWindowMs: f.timing.daChallengeWindowMs,
  };
  await expect(
    Effect.runPromise(
      SDK.buildOpenDaAvailabilityChallengeTxProgram(f.lucid, d, {
        ...p,
        challengerFunding: {
          ...p.challengerFunding,
          assets: { lovelace: p.challengerFunding.assets.lovelace + 1n },
        },
      }),
    ),
  ).rejects.toThrow(/exact isolated/);
  await expect(
    Effect.runPromise(
      SDK.buildOpenDaAvailabilityChallengeTxProgram(
        f.lucid,
        {
          ...d,
          referenceScripts: {
            ...d.referenceScripts,
            "availability-challenge open withdrawal":
              d.referenceScripts["availability-challenge close withdrawal"]!,
          },
        },
        p,
      ),
    ),
  ).rejects.toThrow(/Unauthentic reference script/);
  const outputs = await submit(
    f,
    await Effect.runPromise(
      SDK.buildOpenDaAvailabilityChallengeTxProgram(f.lucid, d, p),
    ),
  );
  expect(outputs[0]!.assets.lovelace).toBe(
    parameters.challenge_record_lovelace,
  );
  return SDK.fetchDaAvailabilityChallengeSnapshot(
    f.lucid,
    d,
    f.target.headerHash,
  );
};
