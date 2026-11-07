import "./helpers/follower-emulator-installed.js";
import "./utils.js";
// Loaded before the fixture's first submission, so its capture is complete.
import "./helpers/emulator-chain-capture.js";

import * as SDK from "@al-ft/midgard-sdk";
import {
  type Assets,
  calculateMinLovelaceFromUTxO,
  Emulator,
  generateEmulatorAccount,
  Lucid,
  type LucidEvolution,
  paymentCredentialOf,
  type Script,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect, Either } from "effect";
import { describe, expect, it } from "vitest";

import {
  type OperatorWalletScriptRefStripResult,
  stripOperatorWalletScriptRefsProgram,
} from "../src/transactions/operator-wallet-script-ref-strip.js";
import type { ReferenceScriptSweepLimits } from "../src/transactions/reference-script-sweep.js";
import { deploymentInitializationNonceOutRefKeys } from "../src/transactions/reference-scripts.js";
import {
  drainJournaledWithoutFollower,
  runWithoutFollower,
} from "./helpers/intent-journal.js";
import {
  expectReplayedFamilies,
  walletReplayConfig,
} from "./helpers/intent-journal-replay.expect.js";
import { replayJournaledOnFollower } from "./helpers/intent-journal-replay.js";
import { loadRealMidgardContractsForTest } from "./helpers/real-midgard-contracts.js";
import { plutusScript } from "./reference-script-sweep.plan.js";

/**
 * A reference script of `bytes` payload bytes, unique per seed. The strip
 * never runs it: it only moves it off the wallet's outputs.
 */
const referenceScript = (seed: number, bytes: number): Script =>
  plutusScript(seed, Math.max(4, bytes - (bytes % 4)));

const outRef = (utxo: UTxO): string => `${utxo.txHash}#${utxo.outputIndex}`;
const outRefsOf = (utxos: readonly UTxO[]) => utxos.map(outRef).sort();
const lovelaceIn = (utxos: readonly UTxO[]): bigint =>
  utxos.reduce((total, utxo) => total + (utxo.assets.lovelace ?? 0n), 0n);
const tokensIn = (utxos: readonly UTxO[]): Assets => {
  const totals: Assets = {};
  for (const utxo of utxos)
    for (const [unit, amount] of Object.entries(utxo.assets))
      if (unit !== "lovelace") totals[unit] = (totals[unit] ?? 0n) + amount;
  return totals;
};
const withScriptRef = (utxos: readonly UTxO[]) =>
  utxos.filter((utxo) => utxo.scriptRef != null);

const SCRIPT_BYTES = 9_000;
const AUTH_TOKEN_NAME = SDK.referenceScriptAuthTokenName(
  Object.keys(SDK.REFERENCE_SCRIPT_AUTH_TOKEN_NAMES)[0]!,
);
/** Three ~9 KB scripts per batch. */
const BATCH_REFERENCE_BUDGET = 28_000;

type Wallet = {
  readonly lucid: LucidEvolution;
  readonly address: string;
  readonly seedPhrase: string;
};

type Fixture = {
  readonly emulator: Emulator;
  readonly operator: Wallet;
  readonly reference: Wallet;
  readonly authPolicy: SDK.ReferenceScriptAuthPolicy;
  readonly limits: ReferenceScriptSweepLimits;
};

const walletOn = async (
  emulator: Emulator,
  seedPhrase: string,
): Promise<Wallet> => {
  const lucid = await Lucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(seedPhrase);
  return { lucid, address: await lucid.wallet().address(), seedPhrase };
};

/** Pays `assets` (minted under the auth policy when `tokenName` is set) with `script` to the wallet itself. */
const publishTo = async (
  emulator: Emulator,
  wallet: Wallet,
  {
    script,
    lovelace,
    mint,
  }: {
    readonly script: Script;
    readonly lovelace?: bigint;
    readonly mint?: {
      readonly policy: SDK.ReferenceScriptAuthPolicy;
      readonly tokenName: string;
    };
  },
): Promise<void> => {
  const assets: Assets = {};
  let tx = wallet.lucid.newTx();
  if (mint !== undefined) {
    const unit = toUnit(mint.policy.policyId, mint.tokenName);
    assets[unit] = 1n;
    tx = tx
      .mintAssets({ [unit]: 1n })
      .attach.MintingPolicy(mint.policy.mintingScript)
      .validTo(mint.policy.expiresAtUnixTime - 60_000);
  }
  const params = await emulator.getProtocolParameters();
  assets.lovelace =
    lovelace ??
    calculateMinLovelaceFromUTxO(params.coinsPerUtxoByte, {
      txHash: "00".repeat(32),
      outputIndex: 0,
      address: wallet.address,
      // The largest coin encoding, so the minimum holds for any amount.
      assets: { ...assets, lovelace: 0xffffffffffffffffn },
      scriptRef: script,
    });
  const unsigned = await tx.pay
    .ToAddressWithData(wallet.address, undefined, assets, script)
    .complete();
  await (await unsigned.sign.withWallet().complete()).submit();
  emulator.awaitBlock(1);
};

const setUp = async (): Promise<Fixture> => {
  const operatorAccount = generateEmulatorAccount({
    lovelace: 2_000_000_000n,
  });
  const referenceAccount = generateEmulatorAccount({
    lovelace: 500_000_000n,
  });
  const emulator = new Emulator([operatorAccount, referenceAccount]);
  const operator = await walletOn(emulator, operatorAccount.seedPhrase);
  const reference = await walletOn(emulator, referenceAccount.seedPhrase);
  const authPolicy = await SDK.createReferenceScriptAuthPolicy(
    operator.lucid,
    emulator.now(),
    24 * 60 * 60 * 1_000,
  );
  const params = await emulator.getProtocolParameters();
  const limits: ReferenceScriptSweepLimits = {
    maxTxSize: params.maxTxSize,
    maxValueSize: params.maxValSize,
    maxReferenceScriptBytesPerTx: 204_800,
    minFeeA: BigInt(params.minFeeA),
    minFeeB: BigInt(params.minFeeB),
    coinsPerUtxoByte: params.coinsPerUtxoByte,
    referenceScriptFee: {
      base: {
        numerator: BigInt(params.minFeeRefScriptCostPerByte),
        denominator: 1n,
      },
      range: 25_600,
      multiplier: { numerator: 6n, denominator: 5n },
    },
  };
  // The reference-script wallet's own reference output, which the strip
  // must never spend.
  await publishTo(emulator, reference, {
    script: referenceScript(200, SCRIPT_BYTES),
    lovelace: 70_000_000n,
  });
  return { emulator, operator, reference, authPolicy, limits };
};

/** A deployment whose initialization nonce is in neither wallet. */
let otherDeployment: Promise<SDK.MidgardValidators> | undefined;
const deploymentElsewhere = () =>
  (otherDeployment ??= loadRealMidgardContractsForTest({
    txHash: "00".repeat(32),
    outputIndex: 0,
  }));

const strip = async (
  fixture: Fixture,
  {
    execute,
    wallet = fixture.operator,
    maxReferenceScriptBytesPerBatch,
    contracts,
  }: {
    readonly execute: boolean;
    readonly wallet?: Wallet;
    readonly maxReferenceScriptBytesPerBatch?: number;
    readonly contracts?: SDK.MidgardValidators;
  },
): Promise<Either.Either<OperatorWalletScriptRefStripResult, unknown>> =>
  runWithoutFollower(
    Effect.either(
      stripOperatorWalletScriptRefsProgram({
        lucid: wallet.lucid,
        contracts: contracts ?? (await deploymentElsewhere()),
        referenceScriptsWalletAddress: fixture.reference.address,
        limits: fixture.limits,
        options: {
          execute,
          ...(maxReferenceScriptBytesPerBatch === undefined
            ? {}
            : { maxReferenceScriptBytesPerBatch }),
        },
      }),
    ),
  );

const expectStripped = (
  result: Either.Either<OperatorWalletScriptRefStripResult, unknown>,
): OperatorWalletScriptRefStripResult => {
  if (Either.isLeft(result)) throw result.left;
  return result.right;
};

const utxosOf = (fixture: Fixture, wallet: Wallet) =>
  fixture.operator.lucid.utxosAt(wallet.address);

describe("strip-operator-wallet-script-refs on the emulator", () => {
  it("dry-runs, then strips every script_ref UTxO back to the operator wallet with lovelace and tokens kept", async () => {
    drainJournaledWithoutFollower();
    const fixture = await setUp();
    const { operator } = fixture;
    for (const seed of [1, 2]) {
      await publishTo(fixture.emulator, operator, {
        script: referenceScript(seed, SCRIPT_BYTES),
        lovelace: 70_000_000n,
      });
    }
    await publishTo(fixture.emulator, operator, {
      script: referenceScript(3, SCRIPT_BYTES),
      lovelace: 70_000_000n,
      mint: {
        policy: fixture.authPolicy,
        tokenName: AUTH_TOKEN_NAME,
      },
    });
    const before = await utxosOf(fixture, operator);
    const referenceBefore = outRefsOf(
      await utxosOf(fixture, fixture.reference),
    );
    const scriptRefsBefore = withScriptRef(before);
    const plainBefore = before.filter((utxo) => utxo.scriptRef == null);
    expect(scriptRefsBefore).toHaveLength(3);
    expect(plainBefore.length).toBeGreaterThan(0);
    expect(Object.keys(tokensIn(scriptRefsBefore))).toHaveLength(1);

    // A dry run prints the plan and submits nothing.
    const dryRun = expectStripped(await strip(fixture, { execute: false }));
    expect(dryRun.dryRun).toBe(true);
    expect(dryRun.submitted).toEqual([]);
    expect(dryRun.message).toMatch(/^Dry run: 3 UTxO\(s\).*--execute/);
    expect(dryRun.plan.totals.inputCount).toBe(3);
    // The auth token gets its own minimum-ADA output in the plan.
    expect(dryRun.plan.totals.tokenOutputCount).toBe(1);
    expect(dryRun.plan.retainedUtxoCount).toBe(plainBefore.length);
    expect(outRefsOf(await utxosOf(fixture, operator))).toEqual(
      outRefsOf(before),
    );
    expect(drainJournaledWithoutFollower()).toEqual([]);

    const executed = expectStripped(await strip(fixture, { execute: true }));
    expect(executed.dryRun).toBe(false);
    expect(executed.submitted).toHaveLength(1);
    expect(executed.submitted[0]!).toMatchObject({
      inputCount: 3,
      feeInputCount: 0,
    });
    const after = await utxosOf(fixture, operator);
    expect(withScriptRef(after)).toEqual([]);
    const fees = executed.submitted.reduce((total, b) => total + b.fee, 0n);
    expect(lovelaceIn(after)).toBe(lovelaceIn(before) - fees);
    expect(tokensIn(after)).toEqual(tokensIn(before));
    // The batch inputs paid the fee: the plain UTxOs are untouched.
    const afterRefs = new Set(after.map(outRef));
    for (const plain of plainBefore)
      expect(afterRefs.has(outRef(plain))).toBe(true);
    // The reference-script wallet is never touched.
    expect(outRefsOf(await utxosOf(fixture, fixture.reference))).toEqual(
      referenceBefore,
    );

    // A node following this chain records the batch and judges it wanted at
    // the block before it landed.
    const strips = expectReplayedFamilies(
      await replayJournaledOnFollower({
        emulator: fixture.emulator,
        contracts: await deploymentElsewhere(),
        config: walletReplayConfig({
          operatorSeed: operator.seedPhrase,
          referenceScriptsSeed: fixture.reference.seedPhrase,
          referenceScriptsAddress: fixture.reference.address,
        }),
        slotToPosixMs: (slot) => operator.lucid.slotToUnixTime(slot),
        operatorKeyHash: paymentCredentialOf(operator.address)!.hash,
      }),
      ["reference_sweep"],
    ).reference_sweep!;
    expect(strips.map(({ txHash }) => txHash)).toEqual(
      executed.submitted.map(({ txHash }) => txHash),
    );

    // A re-run is a no-op with a clear message.
    const rerun = expectStripped(await strip(fixture, { execute: true }));
    expect(rerun.submitted).toEqual([]);
    expect(rerun.plan.totals.batchCount).toBe(0);
    expect(rerun.message).toMatch(
      /carries a reference script; nothing to strip/,
    );
  }, 240_000);

  it("is a no-op with a clear message when no operator UTxO carries a reference script", async () => {
    const fixture = await setUp();
    const before = outRefsOf(await utxosOf(fixture, fixture.operator));
    for (const execute of [false, true]) {
      const result = expectStripped(await strip(fixture, { execute }));
      expect(result.submitted).toEqual([]);
      expect(result.plan.batches).toEqual([]);
      expect(result.message).toBe(
        `No operator-wallet UTxO at ${fixture.operator.address} carries a reference script; nothing to strip`,
      );
    }
    expect(outRefsOf(await utxosOf(fixture, fixture.operator))).toEqual(before);
  }, 120_000);

  it("refuses the reference-script wallet and spends nothing", async () => {
    const fixture = await setUp();
    const before = outRefsOf(await utxosOf(fixture, fixture.reference));
    const result = await strip(fixture, {
      execute: true,
      wallet: fixture.reference,
    });
    expect(Either.isLeft(result)).toBe(true);
    expect(String((result as Either.Left<unknown, never>).left)).toMatch(
      /is the reference-script wallet/,
    );
    expect(outRefsOf(await utxosOf(fixture, fixture.reference))).toEqual(
      before,
    );
  }, 120_000);

  it("batches by the reference-script byte budget and confirms each batch", async () => {
    const fixture = await setUp();
    for (const seed of [1, 2, 3, 4, 5, 6, 7])
      await publishTo(fixture.emulator, fixture.operator, {
        script: referenceScript(seed, SCRIPT_BYTES),
        lovelace: 70_000_000n,
      });
    const before = await utxosOf(fixture, fixture.operator);
    const dryRun = expectStripped(
      await strip(fixture, {
        execute: false,
        maxReferenceScriptBytesPerBatch: BATCH_REFERENCE_BUDGET,
      }),
    );
    expect(dryRun.plan.batches.map((batch) => batch.inputCount)).toEqual([
      3, 3, 1,
    ]);
    for (const batch of dryRun.plan.batches)
      expect(batch.referenceScriptBytes).toBeLessThanOrEqual(
        BATCH_REFERENCE_BUDGET,
      );
    const executed = expectStripped(
      await strip(fixture, {
        execute: true,
        maxReferenceScriptBytesPerBatch: BATCH_REFERENCE_BUDGET,
      }),
    );
    expect(executed.submitted.map((batch) => batch.inputCount)).toEqual([
      3, 3, 1,
    ]);
    expect(new Set(executed.submitted.map((b) => b.txHash)).size).toBe(3);
    const after = await utxosOf(fixture, fixture.operator);
    expect(withScriptRef(after)).toEqual([]);
    const fees = executed.submitted.reduce((total, b) => total + b.fee, 0n);
    expect(lovelaceIn(after)).toBe(lovelaceIn(before) - fees);
  }, 240_000);

  it("adds an operator-wallet UTxO for the fee only when the batch cannot pay it", async () => {
    const fixture = await setUp();
    // A small script and an auth token at minimum ADA: once the token output
    // takes its minimum ADA, the batch cannot also fund change and the fee.
    await publishTo(fixture.emulator, fixture.operator, {
      script: referenceScript(9, 0),
      mint: {
        policy: fixture.authPolicy,
        tokenName: AUTH_TOKEN_NAME,
      },
    });
    const before = await utxosOf(fixture, fixture.operator);
    const executed = expectStripped(await strip(fixture, { execute: true }));
    expect(executed.submitted).toHaveLength(1);
    expect(executed.submitted[0]!.inputCount).toBe(1);
    expect(executed.submitted[0]!.feeInputCount).toBeGreaterThan(0);
    const after = await utxosOf(fixture, fixture.operator);
    expect(withScriptRef(after)).toEqual([]);
    expect(lovelaceIn(after)).toBe(
      lovelaceIn(before) - executed.submitted[0]!.fee,
    );
    expect(tokensIn(after)).toEqual(tokensIn(before));
  }, 120_000);

  it("never strips the deployment's reserved initialization nonce, even when it carries a reference script", async () => {
    const fixture = await setUp();
    await publishTo(fixture.emulator, fixture.operator, {
      script: referenceScript(11, SCRIPT_BYTES),
      lovelace: 70_000_000n,
    });
    const [nonce] = withScriptRef(await utxosOf(fixture, fixture.operator));
    await publishTo(fixture.emulator, fixture.operator, {
      script: referenceScript(12, SCRIPT_BYTES),
      lovelace: 70_000_000n,
    });
    expect(
      withScriptRef(await utxosOf(fixture, fixture.operator)),
    ).toHaveLength(2);
    // The strip reads the nonce from the deployment it runs against.
    const contracts = await loadRealMidgardContractsForTest(nonce!);
    expect([...deploymentInitializationNonceOutRefKeys(contracts)]).toEqual([
      outRef(nonce!),
    ]);
    const executed = expectStripped(
      await strip(fixture, { execute: true, contracts }),
    );
    expect(executed.submitted.map((batch) => batch.inputCount)).toEqual([1]);
    expect(
      withScriptRef(await utxosOf(fixture, fixture.operator)).map(outRef),
    ).toEqual([outRef(nonce!)]);
  }, 240_000);

  it("never takes the deployment's reserved initialization nonce for the fee", async () => {
    const fixture = await setUp();
    // As above, the batch needs a fee input; here the nonce is the only
    // other operator-wallet UTxO.
    await publishTo(fixture.emulator, fixture.operator, {
      script: referenceScript(13, 0),
      mint: {
        policy: fixture.authPolicy,
        tokenName: AUTH_TOKEN_NAME,
      },
    });
    const before = await utxosOf(fixture, fixture.operator);
    const plain = before.filter((utxo) => utxo.scriptRef == null);
    expect(plain).toHaveLength(1);
    const contracts = await loadRealMidgardContractsForTest(plain[0]!);
    expect([...deploymentInitializationNonceOutRefKeys(contracts)]).toEqual([
      outRef(plain[0]!),
    ]);
    const result = await strip(fixture, { execute: true, contracts });
    expect(Either.isLeft(result)).toBe(true);
    expect(outRefsOf(await utxosOf(fixture, fixture.operator))).toEqual(
      outRefsOf(before),
    );
  }, 120_000);
});
