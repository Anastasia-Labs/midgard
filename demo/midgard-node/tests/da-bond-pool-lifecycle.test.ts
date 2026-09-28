import { DEPLOYMENT_PROFILES } from "@al-ft/midgard-core/deployment-profile";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  Data,
  generateEmulatorAccountFromPrivateKey,
  getAddressDetails,
  paymentCredentialOf,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { TEST_AVAILABILITY_PARAMETERS } from "./helpers/availability-challenge.js";
import {
  assertAvailabilityRefusal,
  AVAILABILITY_ATTESTATION_OUTPUT_LOVELACE,
  AVAILABILITY_DEFAULT_POOL_LOVELACE,
  type AvailabilityFixture,
  createAvailabilityFixture,
  lastAvailabilityEvaluationFailure,
} from "./helpers/availability-challenge-emulator.js";

/**
 * Pooled DA bond lifecycle on the real pool, DA attestation and state-queue
 * validators (#689 acceptance criteria 1 and 2, pool half). Every negative is
 * paired with its honest control, is refused by the named script under the
 * named purpose and redeemer index, and its SDK pre-build refusal is asserted
 * separately; the test-only skip flags reach the on-chain check.
 */

const LIFECYCLE_TIMEOUT_MS = 600_000;
/**
 * Fixture setup publishes every reference script, one emulator block each, so
 * the header ends this far ahead to keep Apply inside the attestation deadline.
 */
const HEADER_END_TIME_LEAD_MS = 900_000;
/** The withdraw delay the preprod-testing blueprint compiles in. */
const PREPROD_TESTING_WITHDRAW_DELAY_MS = BigInt(
  DEPLOYMENT_PROFILES["preprod-testing"].timing.da_bond_withdraw_delay_ms,
);

const flip = <E>(program: Effect.Effect<unknown, E>): Promise<E> =>
  Effect.runPromise(Effect.flip(program));

const sameOutRef = (a: UTxO, b: UTxO) =>
  a.txHash === b.txHash && a.outputIndex === b.outputIndex;

/** The ledger (sorted) position of `utxo` among the inputs of `txCbor`. */
const sortedInputIndex = (txCbor: string, utxo: UTxO): number => {
  const inputs = CML.Transaction.from_cbor_hex(txCbor).body().inputs();
  const found = Array.from({ length: inputs.len() }, (_, i) => ({
    txHash: inputs.get(i).transaction_id().to_hex(),
    outputIndex: Number(inputs.get(i).index()),
  }))
    .sort((a, b) =>
      a.txHash < b.txHash
        ? -1
        : a.txHash > b.txHash
          ? 1
          : a.outputIndex - b.outputIndex,
    )
    .findIndex(
      (input) =>
        input.txHash === utxo.txHash && input.outputIndex === utxo.outputIndex,
    );
  if (found < 0) throw new Error("The pool is not an input of the refused tx");
  return found;
};

/**
 * Asserts the refusal came from the pool's own spend of `pool`: purpose
 * `spend`, script = the pool policy, index = the pool's sorted input position.
 */
const assertPoolSpendRefusal = async (
  f: AvailabilityFixture,
  attempt: Promise<unknown>,
  pool: UTxO,
) => {
  const refusal = await assertAvailabilityRefusal(
    attempt,
    { purpose: "spend", script: "da-bond-pool" },
    f.scriptNames,
  );
  expect(refusal.scriptHash).toBe(f.contracts.daBondPool.policyId);
  expect(refusal.index).toBe(
    sortedInputIndex(lastAvailabilityEvaluationFailure()!.tx, pool),
  );
  return refusal;
};

/** Pool datum, lovelace and the full asset set. */
const poolView = (pool: UTxO) => ({
  datum: SDK.parseDaBondPoolDatumCbor(pool.datum!),
  lovelace: pool.assets.lovelace,
  units: Object.keys(pool.assets).sort(),
});

/** The owner-quorum spend config the SDK withdraw builders take. */
const quorumConfig = async (f: AvailabilityFixture) => ({
  poolValidator: f.contracts.daBondPool,
  parameters: f.parameters,
  pool: { utxo: await f.getPool() },
  daParamsUtxo: f.daParamsUtxo,
  signerKeyHashes: f.daParamsDatum.owners,
  referenceScripts: {
    daBondPoolSpending: f.poolReferences.daBondPoolSpending,
  },
});

const topUpProgram = async (
  f: AvailabilityFixture,
  amount: bigint,
  options: { skipMinimumPrecheck?: true } = {},
) =>
  SDK.buildTopUpDaBondPoolTxProgram(f.lucid, {
    poolValidator: f.contracts.daBondPool,
    parameters: f.parameters,
    pool: { utxo: await f.getPool() },
    amount,
    referenceScripts: {
      daBondPoolSpending: f.poolReferences.daBondPoolSpending,
    },
    ...options,
  });

/**
 * Lands the attestation init and the committee's threshold signatures, and
 * returns the threshold attestation Apply consumes (the first half of the
 * harness `attestAvailability`, stopping before Apply).
 */
const thresholdAttestation = async (
  f: AvailabilityFixture,
): Promise<SDK.DaAttestationUtxo> => {
  const { lucid, contracts } = f;
  lucid.selectWallet.fromPrivateKey(f.responder.privateKey);
  const init = await Effect.runPromise(
    SDK.incompleteInitDaAttestationTxProgram(lucid, contracts, {
      daParamsUtxo: f.daParamsUtxo,
      daParamsDatum: f.daParamsDatum,
      target: f.target,
      referenceScripts: f.daReferences,
      attestationOutputLovelace: AVAILABILITY_ATTESTATION_OUTPUT_LOVELACE,
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
    if (!utxo?.datum) throw new Error("Missing attestation");
    return { utxo, datum: Data.from(utxo.datum, SDK.DaAttestationDatum) };
  };
  const message = SDK.daAvailabilityAttestationMessage(f.commitment);
  const add = await Effect.runPromise(
    SDK.incompleteAddDaAttestationSignaturesTxProgram(lucid, contracts, {
      daParamsUtxo: f.daParamsUtxo,
      daParamsDatum: f.daParamsDatum,
      attestation: await current(),
      witnesses: f.committeeKeys.map((key, signerIndex) => ({
        signerIndex,
        signatureHex: Buffer.from(key.sign(message).to_raw_bytes()).toString(
          "hex",
        ),
      })),
      referenceScripts: f.daReferences,
    }),
  );
  await f.submit("attestation threshold signatures", add, true);
  return current();
};

/** The SDK Apply against the live pool (re-fetched on every build, H8). */
const applyProgram = (
  f: AvailabilityFixture,
  attestation: SDK.DaAttestationUtxo,
  options: { skipPoolPrecheck?: true } = {},
) =>
  SDK.incompleteApplyDaAttestationToStateQueueTxProgram(f.lucid, f.contracts, {
    daParamsUtxo: f.daParamsUtxo,
    daParamsDatum: f.daParamsDatum,
    attestation,
    target: f.target,
    referenceScripts: f.daReferences,
    availabilityParameters: f.parameters,
    validityRange: {
      validFrom: BigInt(f.emulator.now()),
      validTo: BigInt(f.emulator.now() + 60_000),
    },
    ...options,
  });

/** The DA attestation mint (`ApplyToStateQueue`) is the refusing check. */
const assertApplyRefusedByDaAttestationMint = async (
  f: AvailabilityFixture,
  attestation: SDK.DaAttestationUtxo,
) => {
  const tx = await Effect.runPromise(
    applyProgram(f, attestation, { skipPoolPrecheck: true }),
  );
  const refusal = await assertAvailabilityRefusal(
    tx.complete({ coinSelection: true, localUPLCEval: true }),
    // Apply mints under exactly one policy (the DAAT burn), so Mint[0].
    { purpose: "mint", script: "da-attestation minting", index: 0 },
    f.scriptNames,
  );
  expect(refusal.scriptHash).toBe(f.contracts.daAttestation.policyId);
};

/** Lands Apply and asserts the node is `Attested{commitment_hash}`. */
const applyAndAssertAttested = async (
  f: AvailabilityFixture,
  attestation: SDK.DaAttestationUtxo,
) => {
  const poolBefore = await f.getPool();
  const apply = await Effect.runPromise(applyProgram(f, attestation));
  await f.submit("attestation apply against the pooled bond", apply, true);
  const [queue] = await f.lucid.utxosAtWithUnit(
    f.contracts.stateQueue.spendingScriptAddress,
    f.queueUnit,
  );
  if (!queue?.datum) throw new Error("Apply omitted the queue node");
  const node = Data.castFrom(
    (await Effect.runPromise(SDK.getLinkedListNodeViewFromUTxO(queue))).data,
    SDK.StateQueueNode,
  );
  expect(node.da_attestation).toEqual({
    Attested: {
      commitment_hash: SDK.daAvailabilityCommitmentHash(f.commitment),
    },
  });
  // The pool is a reference input: Apply neither spends nor changes it.
  expect(sameOutRef(await f.getPool(), poolBefore)).toBe(true);
  return queue;
};

describe("pooled DA bond lifecycle on the real validators", () => {
  it(
    "runs the real InitPool, then a non-owner top-up at exactly the minimum; a top-up one lovelace below it is refused by the pool spend",
    async () => {
      const f = await createAvailabilityFixture(1, 0, 0, { seedPool: false });
      const P = f.parameters;
      const floor = P.da_bond_pool_floor_lovelace;
      await expect(f.getPool()).rejects.toThrow();

      // AC1: the real InitPool, funded at exactly the floor (as the atomic
      // protocol init funds it).
      const init = await f.initPoolReal(floor);
      expect(await f.lucid.utxosByOutRef([f.hubOneShot])).toEqual([]);
      expect(init.address).toBe(f.contracts.daBondPool.spendingScriptAddress);
      const address = getAddressDetails(init.address);
      expect(address.paymentCredential).toEqual({
        type: "Script",
        hash: f.contracts.daBondPool.policyId,
      });
      expect(address.stakeCredential).toBeUndefined();
      expect(poolView(init)).toEqual({
        datum: "Bonded",
        lovelace: floor,
        units: ["lovelace", f.poolUnit].sort(),
      });
      expect(init.assets[f.poolUnit]).toBe(1n);
      expect(init.assets.lovelace).toBeGreaterThanOrEqual(floor);
      // Inline datum only, no reference script (G1).
      expect(init.scriptRef ?? null).toBeNull();
      expect(init.datumHash ?? null).toBeNull();
      expect(
        SDK.daBondPoolBacking({
          lovelace: init.assets.lovelace,
          parameters: P,
        }),
      ).toBe(0n);
      expect(sameOutRef(await f.getPool(), init)).toBe(true);

      // TopUp is permissionless: the one-shot holder owns no DA params key.
      const holder = f.oneShotHolder;
      const holderKey = paymentCredentialOf(holder.address).hash;
      expect(f.daParamsDatum.owners).not.toContain(holderKey);
      f.lucid.selectWallet.fromPrivateKey(holder.privateKey);
      try {
        const minimum = P.da_bond_min_top_up_lovelace;
        // SDK pre-build refusal.
        const early = await flip(await topUpProgram(f, minimum - 1n));
        expect(early).toBeInstanceOf(SDK.DaBondPoolBuildError);
        expect(early.reason).toBe("below_min_top_up");
        // On-chain refusal: the pool's TopUp arm.
        const below = await Effect.runPromise(
          await topUpProgram(f, minimum - 1n, { skipMinimumPrecheck: true }),
        );
        await assertPoolSpendRefusal(
          f,
          below.complete({ coinSelection: true, localUPLCEval: true }),
          init,
        );

        // Honest control: exactly the minimum, signed by the non-owner only.
        const control = await Effect.runPromise(await topUpProgram(f, minimum));
        const unsigned = await control.complete({
          coinSelection: true,
          localUPLCEval: true,
        });
        const signed = await unsigned.sign.withWallet().complete();
        const body = CML.Transaction.from_cbor_hex(signed.toCBOR()).body();
        expect(body.required_signers()?.len() ?? 0).toBe(0);
        const witnesses = CML.Transaction.from_cbor_hex(signed.toCBOR())
          .witness_set()
          .vkeywitnesses();
        expect(witnesses?.len()).toBe(1);
        expect(witnesses!.get(0).vkey().hash().to_hex()).toBe(holderKey);
        const hash = await signed.submit();
        f.emulator.awaitBlock(1);
        const topped = await f.getPool();
        expect(topped.txHash).toBe(hash);
        // The increase is exactly the minimum (the boundary the negative
        // above misses by one lovelace).
        expect(topped.assets.lovelace - init.assets.lovelace).toBe(minimum);
        // Datum byte-equal, NFT kept, nothing else added.
        expect(topped.datum).toBe(init.datum);
        expect(topped.address).toBe(init.address);
        expect(poolView(topped).units).toEqual(poolView(init).units);
        expect(topped.assets[f.poolUnit]).toBe(1n);
      } finally {
        f.lucid.selectWallet.fromPrivateKey(f.responder.privateKey);
      }
    },
    LIFECYCLE_TIMEOUT_MS,
  );

  it(
    "refuses an InitPool that does not spend init_ref (the pool mint), before and after the real InitPool",
    async () => {
      const f = await createAvailabilityFixture(1, 0, 0, { seedPool: false });
      const floor = f.parameters.da_bond_pool_floor_lovelace;
      f.lucid.selectWallet.fromPrivateKey(f.responder.privateKey);
      /** An InitPool spending a plain responder coin in place of init_ref. */
      const forgedInit = async () => {
        const [coin] = (await f.lucid.wallet().getUtxos()).filter(
          (u) =>
            Object.keys(u.assets).length === 1 &&
            !u.datum &&
            !u.datumHash &&
            !u.scriptRef &&
            u.assets.lovelace > 10n * floor,
        );
        if (!coin) throw new Error("The responder holds no plain coin");
        expect(
          coin.txHash === f.hubOneShot.txHash &&
            coin.outputIndex === f.hubOneShot.outputIndex,
        ).toBe(false);
        const tx = await Effect.runPromise(
          SDK.buildInitDaBondPoolTxProgram(f.lucid, {
            poolValidator: f.contracts.daBondPool,
            parameters: f.parameters,
            initUtxo: coin,
            lovelace: floor,
            referenceScripts: {
              daBondPoolMinting: f.poolReferences.daBondPoolMinting,
            },
          }),
        );
        // The pool is the only policy minted, so Mint[0].
        const refusal = await assertAvailabilityRefusal(
          tx.complete({ coinSelection: true, localUPLCEval: true }),
          { purpose: "mint", script: "da-bond-pool", index: 0 },
          f.scriptNames,
        );
        expect(refusal.scriptHash).toBe(f.contracts.daBondPool.policyId);
      };

      // Before the one-shot is spent: a pool minted without it is refused.
      await forgedInit();
      await expect(f.getPool()).rejects.toThrow();

      // Honest control: the real InitPool spends init_ref.
      const init = await f.initPoolReal(floor);
      expect(await f.lucid.utxosByOutRef([f.hubOneShot])).toEqual([]);
      expect(poolView(init)).toEqual({
        datum: "Bonded",
        lovelace: floor,
        units: ["lovelace", f.poolUnit].sort(),
      });

      // After it: a second pool NFT cannot be minted, and the pool stands.
      f.lucid.selectWallet.fromPrivateKey(f.responder.privateKey);
      await forgedInit();
      expect(sameOutRef(await f.getPool(), init)).toBe(true);
    },
    LIFECYCLE_TIMEOUT_MS,
  );

  it(
    "refuses BeginWithdraw and CancelWithdraw below the owner quorum (the pool spend), then lands both under the quorum",
    async () => {
      const f = await createAvailabilityFixture(1);
      expect(f.daParamsDatum.update_threshold).toBe(2n);
      expect(f.daParamsDatum.owners).toContain(f.responderKey);
      const outsider = paymentCredentialOf(f.oneShotHolder.address).hash;
      expect(f.daParamsDatum.owners).not.toContain(outsider);
      f.lucid.selectWallet.fromPrivateKey(f.responder.privateKey);
      const beginProgram = async (
        signerKeyHashes: readonly string[],
        options: { skipQuorumPrecheck?: true } = {},
      ) => {
        const now = BigInt(f.emulator.now());
        return SDK.buildBeginDaBondPoolWithdrawTxProgram(f.lucid, {
          ...(await quorumConfig(f)),
          signerKeyHashes,
          withdrawDelayMs: f.timing.daBondWithdrawDelayMs,
          validity: { validFrom: now, validTo: now + 60_000n },
          ...options,
        });
      };
      const cancelProgram = async (
        signerKeyHashes: readonly string[],
        options: { skipQuorumPrecheck?: true } = {},
      ) =>
        SDK.buildCancelDaBondPoolWithdrawTxProgram(f.lucid, {
          ...(await quorumConfig(f)),
          signerKeyHashes,
          ...options,
        });

      // Begin signed by one owner and one non-owner: two signers, one owner.
      const bonded = await f.getPool();
      const halfQuorum = [f.responderKey, outsider];
      const earlyBegin = await flip(await beginProgram(halfQuorum));
      expect(earlyBegin).toBeInstanceOf(SDK.DaBondPoolBuildError);
      expect(earlyBegin.reason).toBe("signer_not_owner");
      expect((await flip(await beginProgram([f.responderKey]))).reason).toBe(
        "insufficient_signers",
      );
      const belowBegin = await Effect.runPromise(
        await beginProgram(halfQuorum, { skipQuorumPrecheck: true }),
      );
      await assertPoolSpendRefusal(
        f,
        belowBegin.complete({ coinSelection: true, localUPLCEval: true }),
        bonded,
      );
      expect(sameOutRef(await f.getPool(), bonded)).toBe(true);

      // Honest control: the full quorum begins.
      const { pool: withdrawing } = await f.beginPoolWithdraw();
      expect(poolView(withdrawing).datum).toMatchObject({ Withdrawing: {} });

      // Cancel signed by one owner of two.
      f.lucid.selectWallet.fromPrivateKey(f.responder.privateKey);
      const earlyCancel = await flip(await cancelProgram([f.responderKey]));
      expect(earlyCancel.reason).toBe("insufficient_signers");
      const belowCancel = await Effect.runPromise(
        await cancelProgram([f.responderKey], { skipQuorumPrecheck: true }),
      );
      await assertPoolSpendRefusal(
        f,
        belowCancel.complete({ coinSelection: true, localUPLCEval: true }),
        withdrawing,
      );
      expect(sameOutRef(await f.getPool(), withdrawing)).toBe(true);

      // Honest control: the full quorum cancels.
      const cancelled = await f.cancelPoolWithdraw();
      expect(poolView(cancelled)).toEqual(poolView(bonded));
    },
    LIFECYCLE_TIMEOUT_MS,
  );

  it(
    "withdraws through Begin, Cancel, Begin and Complete at unlock_at; Complete one slot before unlock_at is refused by the pool spend",
    async () => {
      const f = await createAvailabilityFixture(1);
      const P = f.parameters;
      const delay = f.timing.daBondWithdrawDelayMs;
      // Timing is the generated profile's value, never an SDK constant.
      expect(delay).toBe(PREPROD_TESTING_WITHDRAW_DELAY_MS);
      expect(delay).toBe(2_340_000n);
      const seeded = await f.getPool();
      expect(poolView(seeded).datum).toBe("Bonded");
      expect(seeded.assets.lovelace).toBe(AVAILABILITY_DEFAULT_POOL_LOVELACE);

      // Begin: unlock_at = daBondPoolUnlockAt for the exact validTo used.
      const firstFrom = BigInt(f.emulator.now());
      const firstTo = firstFrom + 60_000n;
      const first = await f.beginPoolWithdraw({
        validFrom: firstFrom,
        validTo: firstTo,
      });
      expect(first.unlockAt).toBe(
        SDK.daBondPoolUnlockAt({ validToMs: firstTo, withdrawDelayMs: delay }),
      );
      expect(first.unlockAt).toBe(firstTo - 1n + delay);
      expect(first.pool.assets).toEqual(seeded.assets);

      // Cancel returns it to Bonded, value unchanged.
      const cancelled = await f.cancelPoolWithdraw();
      expect(poolView(cancelled)).toEqual(poolView(seeded));
      expect(cancelled.datum).toBe(seeded.datum);

      // Begin again, with a different upper bound.
      const secondFrom = BigInt(f.emulator.now());
      const secondTo = secondFrom + 120_000n;
      const second = await f.beginPoolWithdraw({
        validFrom: secondFrom,
        validTo: secondTo,
      });
      const unlockAt = second.unlockAt;
      expect(unlockAt).toBe(
        SDK.daBondPoolUnlockAt({ validToMs: secondTo, withdrawDelayMs: delay }),
      );
      expect(poolView(second.pool).datum).toEqual({
        Withdrawing: { unlock_at: unlockAt },
      });

      const backing = SDK.daBondPoolBacking({
        lovelace: second.pool.assets.lovelace,
        parameters: P,
      });
      expect(backing).toBe(
        AVAILABILITY_DEFAULT_POOL_LOVELACE - P.da_bond_pool_floor_lovelace,
      );
      const destination = generateEmulatorAccountFromPrivateKey({
        lovelace: 0n,
      }).address;
      const completeProgram = async (
        amount: bigint,
        validFrom: bigint,
        options: { skipUnlockPrecheck?: true } = {},
      ) =>
        SDK.buildCompleteDaBondPoolWithdrawTxProgram(f.lucid, {
          ...(await quorumConfig(f)),
          amount,
          destination,
          validity: { validFrom },
          ...options,
        });

      // Right after Begin: SDK pre-build refusal, then the pool refuses.
      const now = BigInt(f.emulator.now());
      expect(now).toBeLessThan(unlockAt);
      const early = await flip(await completeProgram(backing, now));
      expect(early.reason).toBe("before_unlock");
      await assertPoolSpendRefusal(
        f,
        f.completePoolWithdraw(backing, {
          destination,
          validFrom: now,
          skipUnlockPrecheck: true,
        }),
        second.pool,
      );

      // At the boundary. `unlock_at = validTo - 1 + delay` sits one
      // millisecond before a slot boundary, so the last slot-aligned lower
      // bound before it is `unlock_at + 1 - 1000` and the first at or after
      // it is `unlock_at + 1`.
      expect((unlockAt + 1n - BigInt(f.lucid.slotToUnixTime(0))) % 1_000n).toBe(
        0n,
      );
      f.advanceToMs(unlockAt + 1n);
      const lastEarlySlot = unlockAt + 1n - 1_000n;
      const tight = await flip(await completeProgram(backing, lastEarlySlot));
      expect(tight.reason).toBe("before_unlock");
      await assertPoolSpendRefusal(
        f,
        f.completePoolWithdraw(backing, {
          destination,
          validFrom: lastEarlySlot,
          skipUnlockPrecheck: true,
        }),
        second.pool,
      );
      // More than the backing is refused before building.
      const excess = await flip(
        await completeProgram(backing + 1n, unlockAt + 1n),
      );
      expect(excess.reason).toBe("amount_exceeds_backing");

      // Honest control: the whole backing, at the first lower bound >=
      // unlock_at, to a chosen address.
      expect(await f.lucid.utxosAt(destination)).toEqual([]);
      const done = await f.completePoolWithdraw(backing, {
        destination,
        validFrom: unlockAt + 1n,
      });
      expect(poolView(done)).toEqual({
        datum: "Bonded",
        lovelace: second.pool.assets.lovelace - backing,
        units: poolView(seeded).units,
      });
      expect(done.assets.lovelace).toBe(P.da_bond_pool_floor_lovelace);
      const paid = await f.lucid.utxosAt(destination);
      expect(paid).toHaveLength(1);
      expect(paid[0]!.txHash).toBe(done.txHash);
      expect(paid[0]!.assets).toEqual({ lovelace: backing });
    },
    LIFECYCLE_TIMEOUT_MS,
  );

  it(
    "refuses Apply while the pool is short (the DA attestation mint), then accepts the same Apply after a top-up and writes Attested{commitment_hash}",
    async () => {
      const P = TEST_AVAILABILITY_PARAMETERS;
      // One lovelace of backing short of one DA bond.
      const shortLovelace =
        P.da_bond_pool_floor_lovelace + P.da_bond_lovelace - 1n;
      const f = await createAvailabilityFixture(1, 0, HEADER_END_TIME_LEAD_MS, {
        seedPool: { lovelace: shortLovelace },
      });
      const short = await f.getPool();
      expect(poolView(short)).toMatchObject({
        datum: "Bonded",
        lovelace: shortLovelace,
      });
      expect(
        SDK.daBondPoolBacking({ lovelace: shortLovelace, parameters: P }),
      ).toBe(P.da_bond_lovelace - 1n);
      const attestation = await thresholdAttestation(f);

      // SDK pre-build refusal.
      const refused = await flip(applyProgram(f, attestation));
      expect(refused).toBeInstanceOf(SDK.DaAttestationBuildError);
      expect(refused.reason).toBe("pool-under-backed");
      // On-chain refusal: the DA attestation mint's backing check.
      await assertApplyRefusedByDaAttestationMint(f, attestation);

      // Top up by the minimum: backing becomes da_bond + min - 1 >= da_bond.
      const topped = await f.topUpPool(P.da_bond_min_top_up_lovelace);
      expect(topped.datum).toBe(short.datum);
      expect(
        SDK.daBondPoolBacking({
          lovelace: topped.assets.lovelace,
          parameters: P,
        }),
      ).toBeGreaterThanOrEqual(P.da_bond_lovelace);

      // Resume: the same Apply (same attestation, same config) now lands.
      await applyAndAssertAttested(f, attestation);
    },
    LIFECYCLE_TIMEOUT_MS,
  );

  it(
    "refuses Apply while the pool is Withdrawing (the DA attestation mint), then accepts it once the quorum cancels the withdrawal",
    async () => {
      const f = await createAvailabilityFixture(1, 0, HEADER_END_TIME_LEAD_MS);
      const attestation = await thresholdAttestation(f);
      const { pool: withdrawing } = await f.beginPoolWithdraw();
      expect(poolView(withdrawing).datum).toMatchObject({
        Withdrawing: {},
      });
      // Fully backed: only the state refuses.
      expect(
        SDK.daBondPoolBacking({
          lovelace: withdrawing.assets.lovelace,
          parameters: f.parameters,
        }),
      ).toBeGreaterThanOrEqual(f.parameters.da_bond_lovelace);

      // SDK pre-build refusal.
      const refused = await flip(applyProgram(f, attestation));
      expect(refused.reason).toBe("pool-withdrawing");
      // On-chain refusal: the DA attestation mint's `Bonded` check.
      await assertApplyRefusedByDaAttestationMint(f, attestation);

      // Honest control: the quorum cancels, the same Apply lands.
      const bonded = await f.cancelPoolWithdraw();
      expect(poolView(bonded).datum).toBe("Bonded");
      await applyAndAssertAttested(f, attestation);
    },
    LIFECYCLE_TIMEOUT_MS,
  );
});
