/**
 * DA bond pool builders. Refusals are checked before any transaction is
 * assembled. Redeemer indices, `unlock_at` and output values are read back off
 * the transaction Lucid actually serialized and the ledger after submission,
 * on a Lucid emulator. The layout tests carry the shared always-succeeds
 * script in the pool's place; the pool validator's own checks run in the
 * node's emulator lifecycle suites. One test runs the real `InitPool` from the
 * local blueprint and measures what the pool adds to a transaction.
 */
import { readFileSync } from "node:fs";

import {
  applyDoubleCborEncoding,
  CML,
  Constr,
  Data,
  Emulator,
  generateEmulatorAccount,
  getAddressDetails,
  Lucid,
  type LucidEvolution,
  mintingPolicyToId,
  type Network,
  PROTOCOL_PARAMETERS_DEFAULT,
  type Script,
  toUnit,
  type TxSigned,
  type UTxO,
  validatorToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect, Either } from "effect";
import { describe, expect, it } from "vitest";

import {
  availabilityResponseGeometry,
  DA_AVAILABILITY_CHALLENGER_BOND_LOVELACE_MEASUREMENT_CANDIDATE,
  DA_AVAILABILITY_PROFILE_BOND_AMOUNTS,
  DA_AVAILABILITY_RESPONSE_GEOMETRY_MEASUREMENT_CANDIDATE,
  daAvailabilityParameters,
} from "../src/availability-challenge.js";
import type { AuthenticatedValidator } from "../src/common.js";
import { DaParamsDatum } from "../src/da-attestation.js";
import {
  daBondPoolUnit,
  encodeDaBondPoolDatum,
  parseDaBondPoolDatumCbor,
} from "../src/da-bond-pool.js";
import {
  appendDaBondPoolInitialization,
  assertDaBondPoolOwnerQuorum,
  buildBeginDaBondPoolWithdrawTxProgram,
  buildCancelDaBondPoolWithdrawTxProgram,
  buildCompleteDaBondPoolWithdrawTxProgram,
  buildInitDaBondPoolTxProgram,
  buildTopUpDaBondPoolTxProgram,
  DaBondPoolBuildError,
  type DaBondPoolBuildFailureReason,
} from "../src/da-bond-pool-transactions.js";
import { parseFaultProofBlueprint } from "../src/fraud-proof/contracts/blueprint.js";
import * as Sdk from "../src/index.js";
import { buildDaBondPoolValidator } from "../src/protocol-contracts.js";

const ADA = 1_000_000n;
const NETWORK: Network = "Custom";
const PROTOCOL_PARAMETERS = {
  ...PROTOCOL_PARAMETERS_DEFAULT,
  maxCollateralInputs: 3,
} as const;
// Any positive delay: the always-succeeds stand-in does not compile one in.
const WITHDRAW_DELAY_MS = 60_000n;

const parameters = daAvailabilityParameters({
  responseGeometry: availabilityResponseGeometry(
    DA_AVAILABILITY_RESPONSE_GEOMETRY_MEASUREMENT_CANDIDATE,
  ),
  ...DA_AVAILABILITY_PROFILE_BOND_AMOUNTS,
  challengerBondLovelace:
    DA_AVAILABILITY_CHALLENGER_BOND_LOVELACE_MEASUREMENT_CANDIDATE,
  maxOpenFeeLovelace: 500_000n,
  maxPublicationFeeLovelace: 500_000n,
  maxSettlementFeeLovelace: 500_000n,
  maxCloseFeeLovelace: 1_000_000n,
  maxTimeoutFeeLovelace: 1_200_000n,
});
const FLOOR = parameters.da_bond_pool_floor_lovelace;
const MIN_TOP_UP = parameters.da_bond_min_top_up_lovelace;

const alwaysSucceedsBlueprint = JSON.parse(
  readFileSync(
    new URL(
      "../../midgard-node/blueprints/always-succeeds/plutus.json",
      import.meta.url,
    ),
    "utf8",
  ),
) as {
  readonly validators: readonly {
    readonly title: string;
    readonly compiledCode: string;
  }[];
};

const alwaysSucceedsScript = (title: string): Script => {
  const validator = alwaysSucceedsBlueprint.validators.find(
    (entry) => entry.title === title,
  );
  if (validator === undefined) {
    throw new Error(`always-succeeds blueprint has no ${title}`);
  }
  return {
    type: "PlutusV3",
    script: applyDoubleCborEncoding(validator.compiledCode),
  };
};

/** The pool's shape (one script, `Script(policy)`), always succeeding. */
const standInPool = (): AuthenticatedValidator => {
  const script = alwaysSucceedsScript(
    alwaysSucceedsBlueprint.validators[0]!.title,
  );
  return {
    policyId: mintingPolicyToId(script),
    spendingScriptAddress: validatorToAddress(NETWORK, script),
    spendingScriptHash: validatorToScriptHash(script),
    spendingScriptCBOR: script.script,
    mintingScriptCBOR: script.script,
    spendingScript: script,
    mintingScript: script,
  };
};

/** A never-spent address for the DA params and reference-script UTxOs. */
const parkingAddress = (): string =>
  validatorToAddress(NETWORK, alwaysSucceedsScript("midgard.always_fail.else"));

const run = <A>(program: Effect.Effect<A, DaBondPoolBuildError>) =>
  Effect.runPromise(Effect.either(program));

const expectRefusal = async (
  program: Effect.Effect<unknown, DaBondPoolBuildError>,
  reason: DaBondPoolBuildFailureReason,
): Promise<void> => {
  const result = await run(program);
  if (Either.isRight(result)) {
    throw new Error(`expected a ${reason} refusal, the build succeeded`);
  }
  expect(result.left).toBeInstanceOf(DaBondPoolBuildError);
  expect(result.left.reason).toBe(reason);
};

const expectBuilds = async (
  program: Effect.Effect<unknown, DaBondPoolBuildError>,
): Promise<void> => {
  const result = await run(program);
  if (Either.isLeft(result)) {
    throw new Error(
      `expected the build to succeed, refused ${result.left.reason}: ${result.left.message}`,
    );
  }
};

type CmlOutRefs = {
  readonly len: () => number;
  readonly get: (index: number) => {
    readonly transaction_id: () => { readonly to_hex: () => string };
    readonly index: () => bigint | number;
  };
};

const outRefKey = (outRef: {
  readonly txHash: string;
  readonly outputIndex: number;
}): string => `${outRef.txHash}#${outRef.outputIndex.toString()}`;

/**
 * Reference inputs as a script sees them. The body keeps the order Lucid
 * inserted them in; the ledger reads them as a set, sorted by transaction id
 * and then output index, and that sorted list is what redeemer indices count.
 */
const sortedOutRefKeys = (list: CmlOutRefs | undefined): readonly string[] => {
  const refs: { txHash: string; outputIndex: bigint }[] = [];
  for (let index = 0; index < (list?.len() ?? 0); index += 1) {
    const input = list!.get(index);
    refs.push({
      txHash: input.transaction_id().to_hex(),
      outputIndex: BigInt(input.index()),
    });
  }
  return refs
    .sort((left, right) =>
      left.txHash === right.txHash
        ? Number(left.outputIndex - right.outputIndex)
        : left.txHash < right.txHash
          ? -1
          : 1,
    )
    .map((ref) => `${ref.txHash}#${ref.outputIndex.toString()}`);
};

/** What the ledger sees: redeemers, reference inputs, bounds, signers. */
const assembled = (signed: TxSigned) => {
  const tx = CML.Transaction.from_cbor_hex(signed.toCBOR());
  const body = tx.body();
  const redeemers = tx.witness_set().redeemers()?.to_flat_format();
  const flat: { tag: number; index: bigint; data: unknown }[] = [];
  for (let index = 0; index < (redeemers?.len() ?? 0); index += 1) {
    const redeemer = redeemers!.get(index);
    flat.push({
      tag: redeemer.tag(),
      index: redeemer.index(),
      data: Data.from(redeemer.data().to_cbor_hex()),
    });
  }
  const signers: string[] = [];
  const required = body.required_signers();
  for (let index = 0; index < (required?.len() ?? 0); index += 1) {
    signers.push(required!.get(index).to_hex());
  }
  return {
    bytes: signed.toCBOR().length / 2,
    redeemers: flat,
    referenceInputs: sortedOutRefKeys(
      body.reference_inputs() as unknown as CmlOutRefs,
    ),
    validityStartSlot: body.validity_interval_start(),
    ttlSlot: body.ttl(),
    signers,
    inlineScripts: tx.witness_set().plutus_v3_scripts()?.len() ?? 0,
  };
};

const SPEND = CML.RedeemerTag.Spend;
const MINT = CML.RedeemerTag.Mint;

const redeemerOf = (
  view: ReturnType<typeof assembled>,
  tag: number,
): unknown => {
  const matches = view.redeemers.filter((redeemer) => redeemer.tag === tag);
  expect(matches).toHaveLength(1);
  return matches[0]!.data;
};

type Scene = {
  readonly emulator: Emulator;
  readonly lucid: LucidEvolution;
  readonly pool: AuthenticatedValidator;
  readonly unit: string;
  readonly walletKeyHash: string;
  readonly otherOwner: string;
  readonly daParamsUtxo: UTxO;
  readonly spendingReference: UTxO;
};

const submit = async (
  scene: Pick<Scene, "emulator" | "lucid">,
  signed: TxSigned,
): Promise<string> => {
  const txHash = await signed.submit();
  await scene.lucid.awaitTx(txHash);
  scene.emulator.awaitBlock(1);
  return txHash;
};

const daParamsDatum = (
  owners: readonly string[],
  updateThreshold: bigint,
): DaParamsDatum => ({
  committee: "",
  committee_signers_hash: "00".repeat(32),
  da_threshold: 1n,
  owners: [...owners],
  update_threshold: updateThreshold,
});

const setupScene = async (): Promise<Scene> => {
  const account = generateEmulatorAccount({ lovelace: 100_000n * ADA });
  const emulator = new Emulator([account], PROTOCOL_PARAMETERS);
  const lucid = await Lucid(emulator, NETWORK);
  lucid.selectWallet.fromSeed(account.seedPhrase);
  const payment = getAddressDetails(account.address).paymentCredential;
  if (payment?.type !== "Key") {
    throw new Error("expected a key-payment emulator wallet");
  }
  const pool = standInPool();
  const otherOwner = "ab".repeat(28);
  const parking = parkingAddress();
  const paramsCbor = Data.to(
    daParamsDatum([payment.hash, otherOwner], 1n),
    DaParamsDatum,
  );
  const setup = await (
    await lucid
      .newTx()
      // The script reference first, so the params UTxO sorts second among
      // reference inputs while the builder hands it to readFrom first.
      .pay.ToContract(
        parking,
        { kind: "inline", value: Data.void() },
        { lovelace: 30n * ADA },
        pool.spendingScript,
      )
      .pay.ToContract(
        parking,
        { kind: "inline", value: paramsCbor },
        { lovelace: 5n * ADA },
      )
      .complete()
  ).sign
    .withWallet()
    .complete();
  const setupHash = await submit({ emulator, lucid }, setup);
  const [spendingReference, daParamsUtxo] = await lucid.utxosByOutRef([
    { txHash: setupHash, outputIndex: 0 },
    { txHash: setupHash, outputIndex: 1 },
  ]);
  return {
    emulator,
    lucid,
    pool,
    unit: daBondPoolUnit(pool.policyId),
    walletKeyHash: payment.hash,
    otherOwner,
    daParamsUtxo: daParamsUtxo!,
    spendingReference: spendingReference!,
  };
};

const syntheticPool = (
  scene: Pick<Scene, "pool" | "unit">,
  datum: Sdk.DaBondPoolDatum,
  lovelace: bigint,
): UTxO => ({
  txHash: "cd".repeat(32),
  outputIndex: 0,
  address: scene.pool.spendingScriptAddress,
  assets: { lovelace, [scene.unit]: 1n },
  datum: encodeDaBondPoolDatum(datum),
});

const poolAt = async (scene: Scene): Promise<UTxO> => {
  const utxos = await scene.lucid.utxosAtWithUnit(
    scene.pool.spendingScriptAddress,
    scene.unit,
  );
  expect(utxos).toHaveLength(1);
  return utxos[0]!;
};

const initPool = async (scene: Scene, lovelace: bigint): Promise<UTxO> => {
  const [initUtxo] = await scene.lucid.wallet().getUtxos();
  const built = await Effect.runPromise(
    buildInitDaBondPoolTxProgram(scene.lucid, {
      poolValidator: scene.pool,
      parameters,
      initUtxo: initUtxo!,
      lovelace,
    }),
  );
  const signed = await (await built.complete()).sign.withWallet().complete();
  const view = assembled(signed);
  const txHash = await submit(scene, signed);
  const pool = await poolAt(scene);
  expect(pool.txHash).toBe(txHash);
  expect(redeemerOf(view, MINT)).toEqual(
    new Constr(0, [BigInt(pool.outputIndex)]),
  );
  return pool;
};

describe("DA bond pool builders: refusals before assembly", () => {
  it("counts each owner once and refuses a quorum the validator would refuse", () => {
    const owners = ["0a".repeat(28), "0b".repeat(28), "0c".repeat(28)];
    const params = daParamsDatum(owners, 2n);
    expect(() =>
      assertDaBondPoolOwnerQuorum(params, [owners[0]!, owners[2]!]),
    ).not.toThrow();
    const reasonOf = (signers: readonly string[]) => {
      try {
        assertDaBondPoolOwnerQuorum(params, signers);
      } catch (error) {
        expect(error).toBeInstanceOf(DaBondPoolBuildError);
        return (error as DaBondPoolBuildError).reason;
      }
      return "accepted";
    };
    expect(reasonOf([owners[1]!])).toBe("insufficient_signers");
    expect(reasonOf([])).toBe("insufficient_signers");
    expect(reasonOf([owners[1]!, owners[1]!])).toBe("duplicate_signer");
    expect(reasonOf([owners[0]!, "04".repeat(28)])).toBe("signer_not_owner");
    expect(reasonOf([owners[0]!, owners[1]!.toUpperCase()])).toBe(
      "signer_not_owner",
    );
  });

  it("refuses withdrawals signed below update_threshold, by a non-owner, or twice unless the emulator negative asks", async () => {
    const scene = await setupScene();
    const bonded = syntheticPool(scene, "Bonded", FLOOR + 600n * ADA);
    const withdrawing = syntheticPool(
      scene,
      { Withdrawing: { unlock_at: 0n } },
      FLOOR + 600n * ADA,
    );
    const now = BigInt(scene.emulator.now());
    const twoOfTwo: UTxO = {
      ...scene.daParamsUtxo,
      datum: Data.to(
        daParamsDatum([scene.walletKeyHash, scene.otherOwner], 2n),
        DaParamsDatum,
      ),
    };
    const base = {
      poolValidator: scene.pool,
      parameters,
      daParamsUtxo: twoOfTwo,
    } as const;
    const begin = (
      signerKeyHashes: readonly string[],
      skipQuorumPrecheck?: true,
    ) =>
      buildBeginDaBondPoolWithdrawTxProgram(scene.lucid, {
        ...base,
        pool: { utxo: bonded },
        signerKeyHashes,
        withdrawDelayMs: WITHDRAW_DELAY_MS,
        validity: { validFrom: now, validTo: now + 300_000n },
        ...(skipQuorumPrecheck === undefined ? {} : { skipQuorumPrecheck }),
      });
    await expectRefusal(begin([scene.walletKeyHash]), "insufficient_signers");
    await expectRefusal(
      begin([scene.walletKeyHash, scene.walletKeyHash]),
      "duplicate_signer",
    );
    await expectRefusal(
      begin([scene.walletKeyHash, "ef".repeat(28)]),
      "signer_not_owner",
    );
    await expectBuilds(begin([scene.walletKeyHash, scene.otherOwner]));
    // The emulator negatives: below the quorum, for the validator to refuse.
    await expectBuilds(begin([scene.walletKeyHash], true));
    await expectBuilds(begin([scene.walletKeyHash, "ef".repeat(28)], true));
    const cancel = (skipQuorumPrecheck?: true) =>
      buildCancelDaBondPoolWithdrawTxProgram(scene.lucid, {
        ...base,
        pool: { utxo: withdrawing },
        signerKeyHashes: [scene.otherOwner],
        ...(skipQuorumPrecheck === undefined ? {} : { skipQuorumPrecheck }),
      });
    await expectRefusal(cancel(), "insufficient_signers");
    await expectBuilds(cancel(true));
    await expectRefusal(
      buildCompleteDaBondPoolWithdrawTxProgram(scene.lucid, {
        ...base,
        pool: { utxo: withdrawing },
        signerKeyHashes: [scene.walletKeyHash],
        amount: ADA,
        destination: parkingAddress(),
        validity: { validFrom: now },
      }),
      "insufficient_signers",
    );
    await expectRefusal(
      buildBeginDaBondPoolWithdrawTxProgram(scene.lucid, {
        ...base,
        daParamsUtxo: { ...twoOfTwo, datum: Data.void() },
        pool: { utxo: bonded },
        signerKeyHashes: [scene.walletKeyHash, scene.otherOwner],
        withdrawDelayMs: WITHDRAW_DELAY_MS,
        validity: { validFrom: now, validTo: now + 300_000n },
      }),
      "invalid_da_params",
    );
  });

  it("refuses a top-up below da_bond_min_top_up_lovelace unless the emulator negative asks", async () => {
    const scene = await setupScene();
    const topUp = (amount: bigint, skipMinimumPrecheck?: true) =>
      buildTopUpDaBondPoolTxProgram(scene.lucid, {
        poolValidator: scene.pool,
        parameters,
        pool: { utxo: syntheticPool(scene, "Bonded", FLOOR) },
        amount,
        ...(skipMinimumPrecheck === undefined ? {} : { skipMinimumPrecheck }),
      });
    expect(MIN_TOP_UP).toBeGreaterThan(1n);
    await expectRefusal(topUp(MIN_TOP_UP - 1n), "below_min_top_up");
    await expectRefusal(topUp(0n), "below_min_top_up");
    await expectBuilds(topUp(MIN_TOP_UP));
    await expectBuilds(topUp(MIN_TOP_UP - 1n, true));
    await expectRefusal(topUp(-1n, true), "invalid_amount");
  });

  it("bounds CompleteWithdraw to 0 < amount <= backing, at or after unlock_at", async () => {
    const scene = await setupScene();
    const now = BigInt(scene.emulator.now());
    const lovelace = FLOOR + 600n * ADA;
    const backing = lovelace - FLOOR;
    const unlockAt = now + 10_000n;
    const complete = (
      amount: bigint,
      validFrom: bigint,
      skipUnlockPrecheck?: true,
    ) =>
      buildCompleteDaBondPoolWithdrawTxProgram(scene.lucid, {
        poolValidator: scene.pool,
        parameters,
        pool: {
          utxo: syntheticPool(
            scene,
            { Withdrawing: { unlock_at: unlockAt } },
            lovelace,
          ),
        },
        daParamsUtxo: scene.daParamsUtxo,
        signerKeyHashes: [scene.walletKeyHash],
        amount,
        destination: parkingAddress(),
        validity: { validFrom },
        ...(skipUnlockPrecheck === undefined ? {} : { skipUnlockPrecheck }),
      });
    await expectBuilds(complete(backing, unlockAt));
    await expectBuilds(complete(1n, unlockAt));
    await expectRefusal(
      complete(backing + 1n, unlockAt),
      "amount_exceeds_backing",
    );
    await expectRefusal(complete(0n, unlockAt), "invalid_amount");
    await expectRefusal(complete(-1n, unlockAt), "invalid_amount");
    // One slot early: the validator reads the inclusive lower bound.
    await expectRefusal(complete(ADA, unlockAt - 1_000n), "before_unlock");
    await expectBuilds(complete(ADA, unlockAt - 1_000n, true));
    // A pool at or below its floor backs nothing.
    const atFloor = buildCompleteDaBondPoolWithdrawTxProgram(scene.lucid, {
      poolValidator: scene.pool,
      parameters,
      pool: {
        utxo: syntheticPool(
          scene,
          { Withdrawing: { unlock_at: unlockAt } },
          FLOOR,
        ),
      },
      daParamsUtxo: scene.daParamsUtxo,
      signerKeyHashes: [scene.walletKeyHash],
      amount: 1n,
      destination: parkingAddress(),
      validity: { validFrom: unlockAt },
    });
    await expectRefusal(atFloor, "amount_exceeds_backing");
  });

  it("refuses the wrong pool state, a pool below its floor, and a pool that is not one", async () => {
    const scene = await setupScene();
    const now = BigInt(scene.emulator.now());
    const withdrawing = syntheticPool(
      scene,
      { Withdrawing: { unlock_at: now } },
      FLOOR + ADA,
    );
    const bonded = syntheticPool(scene, "Bonded", FLOOR + ADA);
    const quorum = {
      poolValidator: scene.pool,
      parameters,
      daParamsUtxo: scene.daParamsUtxo,
      signerKeyHashes: [scene.walletKeyHash],
    } as const;
    await expectRefusal(
      buildBeginDaBondPoolWithdrawTxProgram(scene.lucid, {
        ...quorum,
        pool: { utxo: withdrawing },
        withdrawDelayMs: WITHDRAW_DELAY_MS,
        validity: { validFrom: now, validTo: now + 300_000n },
      }),
      "pool_not_bonded",
    );
    await expectRefusal(
      buildCancelDaBondPoolWithdrawTxProgram(scene.lucid, {
        ...quorum,
        pool: { utxo: bonded },
      }),
      "pool_not_withdrawing",
    );
    await expectRefusal(
      buildCompleteDaBondPoolWithdrawTxProgram(scene.lucid, {
        ...quorum,
        pool: { utxo: bonded },
        amount: ADA,
        destination: parkingAddress(),
        validity: { validFrom: now },
      }),
      "pool_not_withdrawing",
    );
    await expectRefusal(
      buildBeginDaBondPoolWithdrawTxProgram(scene.lucid, {
        ...quorum,
        pool: { utxo: bonded },
        withdrawDelayMs: WITHDRAW_DELAY_MS,
        validity: {
          validFrom: now,
          validTo: now + Sdk.MAX_VALIDITY_RANGE_LENGTH_MS + 2_000n,
        },
      }),
      "invalid_validity_range",
    );
    const noNft = { ...bonded, assets: { lovelace: FLOOR + ADA } };
    await expectRefusal(
      buildTopUpDaBondPoolTxProgram(scene.lucid, {
        poolValidator: scene.pool,
        parameters,
        pool: { utxo: noNft },
        amount: MIN_TOP_UP,
      }),
      "invalid_pool",
    );
    const [walletUtxo] = await scene.lucid.wallet().getUtxos();
    await expectRefusal(
      buildInitDaBondPoolTxProgram(scene.lucid, {
        poolValidator: scene.pool,
        parameters,
        initUtxo: walletUtxo!,
        lovelace: FLOOR - 1n,
      }),
      "below_floor",
    );
    await expectBuilds(
      buildInitDaBondPoolTxProgram(scene.lucid, {
        poolValidator: scene.pool,
        parameters,
        initUtxo: walletUtxo!,
        lovelace: FLOOR,
      }),
    );
    // The pool address must be Script(own policy) with no stake part.
    const staked: AuthenticatedValidator = {
      ...scene.pool,
      spendingScriptAddress: validatorToAddress(
        NETWORK,
        scene.pool.spendingScript,
        { type: "Key", hash: scene.walletKeyHash },
      ),
    };
    await expectRefusal(
      buildInitDaBondPoolTxProgram(scene.lucid, {
        poolValidator: staked,
        parameters,
        initUtxo: walletUtxo!,
        lovelace: FLOOR,
      }),
      "invalid_pool_address",
    );
    await expectRefusal(
      buildTopUpDaBondPoolTxProgram(scene.lucid, {
        poolValidator: scene.pool,
        parameters,
        pool: { utxo: bonded },
        amount: MIN_TOP_UP,
        referenceScripts: { daBondPoolSpending: scene.daParamsUtxo },
      }),
      "reference_script_mismatch",
    );
  });
});

describe("DA bond pool builders: the transaction Lucid assembles", () => {
  it("InitPool, TopUp, Begin, Cancel, Begin, Complete name the real positions and bounds", async () => {
    const scene = await setupScene();
    const initial = FLOOR + 600n * ADA;
    let pool = await initPool(scene, initial);
    expect(pool.assets).toEqual({ lovelace: initial, [scene.unit]: 1n });
    expect(pool.datum).toBe(encodeDaBondPoolDatum("Bonded"));

    // TopUp, by reference: the datum bytes carry over, value grows by Δ.
    const topUp = await Effect.runPromise(
      buildTopUpDaBondPoolTxProgram(scene.lucid, {
        poolValidator: scene.pool,
        parameters,
        pool: { utxo: pool },
        amount: MIN_TOP_UP,
        referenceScripts: { daBondPoolSpending: scene.spendingReference },
      }),
    );
    let signed = await (await topUp.complete()).sign.withWallet().complete();
    let view = assembled(signed);
    expect(view.inlineScripts).toBe(0);
    expect(view.referenceInputs).toContain(outRefKey(scene.spendingReference));
    await submit(scene, signed);
    const toppedUp = await poolAt(scene);
    expect(redeemerOf(view, SPEND)).toEqual(
      new Constr(0, [BigInt(toppedUp.outputIndex)]),
    );
    expect(toppedUp.datum).toBe(pool.datum);
    expect(toppedUp.assets).toEqual({
      lovelace: initial + MIN_TOP_UP,
      [scene.unit]: 1n,
    });
    pool = toppedUp;

    // BeginWithdraw with a validTo that is not on a slot boundary: unlock_at
    // follows the upper bound the ledger carries, not the one asked for.
    const begin = async (utxo: UTxO) => {
      const now = BigInt(scene.emulator.now());
      const built = await Effect.runPromise(
        buildBeginDaBondPoolWithdrawTxProgram(scene.lucid, {
          poolValidator: scene.pool,
          parameters,
          pool: { utxo },
          daParamsUtxo: scene.daParamsUtxo,
          signerKeyHashes: [scene.walletKeyHash],
          withdrawDelayMs: WITHDRAW_DELAY_MS,
          validity: { validFrom: now, validTo: now + 300_537n },
        }),
      );
      const beginSigned = await (await built.complete()).sign
        .withWallet()
        .complete();
      const beginView = assembled(beginSigned);
      await submit(scene, beginSigned);
      const next = await poolAt(scene);
      const ledgerValidTo = BigInt(
        scene.lucid.slotToUnixTime(Number(beginView.ttlSlot!)),
      );
      expect(ledgerValidTo).toBeLessThan(now + 300_537n);
      const unlockAt = ledgerValidTo - 1n + WITHDRAW_DELAY_MS;
      expect(parseDaBondPoolDatumCbor(next.datum!)).toEqual({
        Withdrawing: { unlock_at: unlockAt },
      });
      expect(next.assets).toEqual(utxo.assets);
      expect(beginView.signers).toEqual([scene.walletKeyHash]);
      expect(redeemerOf(beginView, SPEND)).toEqual(
        new Constr(2, [
          BigInt(
            beginView.referenceInputs.indexOf(outRefKey(scene.daParamsUtxo)),
          ),
          BigInt(next.outputIndex),
        ]),
      );
      expect(beginView.inlineScripts).toBe(1);
      return { next, unlockAt };
    };
    const firstBegin = await begin(pool);
    pool = firstBegin.next;

    const cancel = await Effect.runPromise(
      buildCancelDaBondPoolWithdrawTxProgram(scene.lucid, {
        poolValidator: scene.pool,
        parameters,
        pool: { utxo: pool },
        daParamsUtxo: scene.daParamsUtxo,
        signerKeyHashes: [scene.walletKeyHash],
        referenceScripts: { daBondPoolSpending: scene.spendingReference },
      }),
    );
    signed = await (await cancel.complete()).sign.withWallet().complete();
    view = assembled(signed);
    await submit(scene, signed);
    const cancelled = await poolAt(scene);
    expect(cancelled.datum).toBe(encodeDaBondPoolDatum("Bonded"));
    expect(cancelled.assets).toEqual(pool.assets);
    // The params UTxO and the script reference share a transaction, so the
    // sorted order is fixed by output index: params second, although the
    // builder reads it first and the body lists it first.
    expect(view.referenceInputs).toEqual([
      outRefKey(scene.spendingReference),
      outRefKey(scene.daParamsUtxo),
    ]);
    expect(redeemerOf(view, SPEND)).toEqual(
      new Constr(3, [1n, BigInt(cancelled.outputIndex)]),
    );
    pool = cancelled;

    const secondBegin = await begin(pool);
    pool = secondBegin.next;
    const unlockAt = secondBegin.unlockAt;
    scene.emulator.awaitSlot(Number(WITHDRAW_DELAY_MS / 1_000n) + 400);

    const destination = parkingAddress();
    const amount = 250n * ADA;
    const complete = await Effect.runPromise(
      buildCompleteDaBondPoolWithdrawTxProgram(scene.lucid, {
        poolValidator: scene.pool,
        parameters,
        pool: { utxo: pool },
        daParamsUtxo: scene.daParamsUtxo,
        signerKeyHashes: [scene.walletKeyHash],
        amount,
        destination,
        validity: { validFrom: unlockAt },
      }),
    );
    signed = await (await complete.complete()).sign.withWallet().complete();
    view = assembled(signed);
    const completeHash = await submit(scene, signed);
    const ledgerValidFrom = BigInt(
      scene.lucid.slotToUnixTime(Number(view.validityStartSlot!)),
    );
    // unlock_at sits 1 ms before a slot boundary (a slot-aligned upper bound
    // minus one plus a whole-second delay), and Lucid floors validFrom to its
    // slot, so the builder must round the asked-for unlock_at up to the next
    // boundary: exactly unlock_at + 1.
    expect(ledgerValidFrom).toBe(unlockAt + 1n);
    const completed = await poolAt(scene);
    expect(completed.datum).toBe(encodeDaBondPoolDatum("Bonded"));
    expect(completed.assets).toEqual({
      lovelace: initial + MIN_TOP_UP - amount,
      [scene.unit]: 1n,
    });
    const [paid] = (await scene.lucid.utxosAt(destination)).filter(
      (utxo) => utxo.txHash === completeHash,
    );
    expect(paid?.assets).toEqual({ lovelace: amount });
    expect(redeemerOf(view, SPEND)).toEqual(
      new Constr(4, [
        amount,
        BigInt(view.referenceInputs.indexOf(outRefKey(scene.daParamsUtxo))),
        BigInt(completed.outputIndex),
      ]),
    );
  }, 300_000);

  it("resolves InitPool's output_index from the final position, and refuses a stated one it does not match", async () => {
    const scene = await setupScene();
    const [initUtxo] = await scene.lucid.wallet().getUtxos();
    const parking = parkingAddress();
    const withPriorOutputs = (outputIndex?: bigint) =>
      appendDaBondPoolInitialization(
        scene.lucid
          .newTx()
          .collectFrom([initUtxo!])
          .pay.ToAddress(parking, { lovelace: 2n * ADA })
          .pay.ToAddress(parking, { lovelace: 3n * ADA })
          .pay.ToAddress(parking, { lovelace: 4n * ADA }),
        {
          poolValidator: scene.pool,
          floorLovelace: FLOOR,
          lovelace: FLOOR,
          ...(outputIndex === undefined ? {} : { outputIndex }),
        },
      );
    await expect(withPriorOutputs(0n).complete()).rejects.toThrow(
      /landed at 3, expected 0/u,
    );
    const signed = await (await withPriorOutputs(3n).complete()).sign
      .withWallet()
      .complete();
    const view = assembled(signed);
    const txHash = await submit(scene, signed);
    const pool = await poolAt(scene);
    expect(pool.txHash).toBe(txHash);
    expect(pool.outputIndex).toBe(3);
    expect(redeemerOf(view, MINT)).toEqual(new Constr(0, [3n]));
    expect(() =>
      appendDaBondPoolInitialization(scene.lucid.newTx(), {
        poolValidator: scene.pool,
        floorLovelace: FLOOR,
        lovelace: FLOOR - 1n,
      }),
    ).toThrow(DaBondPoolBuildError);
  }, 300_000);
});

describe("DA bond pool: the real InitPool", () => {
  const blueprint = parseFaultProofBlueprint(
    JSON.parse(
      readFileSync(
        new URL("../../../onchain/aiken/plutus.json", import.meta.url),
        "utf8",
      ),
    ) as unknown,
  );

  it("mints the pool from the local blueprint and measures what it adds to a transaction", async () => {
    const scene = await setupScene();
    const parking = parkingAddress();
    // Split off the nonce first, then publish the pool's minting script from
    // the other wallet UTxO so the publish cannot spend the nonce.
    const walletAddress = await scene.lucid.wallet().address();
    const split = await (
      await scene.lucid
        .newTx()
        .pay.ToAddress(walletAddress, { lovelace: 200n * ADA })
        .complete()
    ).sign
      .withWallet()
      .complete();
    const splitHash = await submit(scene, split);
    const walletUtxos = await scene.lucid.wallet().getUtxos();
    const nonce = walletUtxos.find(
      (utxo) => utxo.txHash === splitHash && utxo.outputIndex === 0,
    );
    const funding = walletUtxos.find((utxo) => utxo !== nonce);
    expect(nonce?.assets).toEqual({ lovelace: 200n * ADA });
    expect(funding).toBeDefined();
    const pool = buildDaBondPoolValidator(
      blueprint,
      NETWORK,
      nonce!,
      "11".repeat(28),
      "22".repeat(28),
      parameters,
    );
    const publish = await (
      await scene.lucid
        .newTx()
        .collectFrom([funding!])
        .pay.ToContract(
          parking,
          { kind: "inline", value: Data.void() },
          { lovelace: 60n * ADA },
          pool.mintingScript,
        )
        .complete({ coinSelection: false })
    ).sign
      .withWallet()
      .complete();
    const publishHash = await submit(scene, publish);
    const [mintingReference] = await scene.lucid.utxosByOutRef([
      { txHash: publishHash, outputIndex: 0 },
    ]);
    expect(await scene.lucid.utxosByOutRef([nonce!])).toHaveLength(1);

    // A baseline that already runs a script, as the atomic init does, so the
    // measured difference is the pool alone and not collateral.
    const standIn = standInPool();
    const baseUnit = toUnit(standIn.policyId, "42415345");
    const base = () =>
      scene.lucid
        .newTx()
        .collectFrom([nonce!])
        .mintAssets({ [baseUnit]: 1n }, Data.void())
        .attach.Script(standIn.mintingScript)
        .pay.ToAddress(parking, { lovelace: 2n * ADA, [baseUnit]: 1n });
    const signedSize = async (
      tx: ReturnType<typeof base>,
    ): Promise<{ bytes: number; signed: TxSigned }> => {
      const signed = await (await tx.complete()).sign.withWallet().complete();
      return { bytes: assembled(signed).bytes, signed };
    };
    const baseline = await signedSize(base());
    const inline = await signedSize(
      appendDaBondPoolInitialization(base(), {
        poolValidator: pool,
        floorLovelace: FLOOR,
        lovelace: FLOOR,
      }),
    );
    const referenced = await signedSize(
      appendDaBondPoolInitialization(base(), {
        poolValidator: pool,
        floorLovelace: FLOOR,
        lovelace: FLOOR,
        referenceScript: mintingReference,
      }),
    );
    const scriptBytes = pool.mintingScript.script.length / 2;
    const inlineDelta = inline.bytes - baseline.bytes;
    const referencedDelta = referenced.bytes - baseline.bytes;
    console.info(
      `DA bond pool init size: script ${scriptBytes.toString()} B; ` +
        `+${inlineDelta.toString()} B inline, +${referencedDelta.toString()} B by reference ` +
        `(baseline ${baseline.bytes.toString()} B)`,
    );
    expect(inlineDelta - referencedDelta).toBeGreaterThanOrEqual(
      scriptBytes - 64,
    );
    expect(referencedDelta).toBeLessThan(400);

    await submit(scene, referenced.signed);
    const minted = await scene.lucid.utxosAtWithUnit(
      pool.spendingScriptAddress,
      daBondPoolUnit(pool.policyId),
    );
    expect(minted).toHaveLength(1);
    expect(minted[0]!.datum).toBe(encodeDaBondPoolDatum("Bonded"));
    expect(minted[0]!.assets).toEqual({
      lovelace: FLOOR,
      [daBondPoolUnit(pool.policyId)]: 1n,
    });
  }, 300_000);
});
