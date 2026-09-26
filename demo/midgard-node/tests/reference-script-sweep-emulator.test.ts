import "./utils.js";

import { readFileSync } from "node:fs";

import * as SDK from "@al-ft/midgard-sdk";
import {
  applyDoubleCborEncoding,
  applyParamsToScript,
  type Assets,
  Emulator,
  generateEmulatorAccount,
  Lucid,
  type LucidEvolution,
  type Script,
  toUnit,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect, Either } from "effect";
import { describe, expect, it } from "vitest";

import {
  type LiveReferenceScriptDeployment,
  referenceScriptLedgerBytes,
  type ReferenceScriptSweepLimits,
  ReferenceScriptSweepRefusal,
  type ReferenceScriptSweepResult,
  sweepRetiredReferenceScriptsProgram,
} from "../src/transactions/reference-script-sweep.js";
import { resolveReferenceScriptUtxo } from "../src/transactions/reference-scripts.js";

const alwaysSucceedsCode = (
  JSON.parse(
    readFileSync(
      new URL("../blueprints/always-succeeds/plutus.json", import.meta.url),
      "utf8",
    ),
  ) as { readonly validators: readonly { readonly compiledCode: string }[] }
).validators[0]!.compiledCode;

/**
 * A well-formed Plutus V3 program of roughly `bytes` bytes, unique per seed.
 * The payload is a list of 64-byte chunks, the largest bytestring a Data
 * constant carries in one piece.
 */
const referenceScript = (seed: number, bytes: number): Script => ({
  type: "PlutusV3",
  script: applyDoubleCborEncoding(
    applyParamsToScript(alwaysSucceedsCode, [
      Array.from({ length: Math.ceil(bytes / 66) }, () =>
        Buffer.alloc(64, seed).toString("hex"),
      ),
    ]),
  ),
});

const outRef = (utxo: UTxO): string => `${utxo.txHash}#${utxo.outputIndex}`;

const SCRIPT_BYTES = 9_000;
const RETIRED_COUNT = 7;
const LIVE_COUNT = 3;
/** Three ~9 KB scripts per batch: every full batch crosses the 25_600-byte fee tier. */
const BATCH_REFERENCE_BUDGET = 28_000;

type AuthPolicy = SDK.ReferenceScriptAuthPolicy;

type Fixture = {
  readonly emulator: Emulator;
  readonly lucid: LucidEvolution;
  readonly address: string;
  readonly retiredPolicy: AuthPolicy;
  readonly livePolicy: AuthPolicy;
  readonly retiredScripts: readonly Script[];
  readonly liveScripts: readonly Script[];
  readonly liveTargets: readonly SDK.ReferenceScriptTarget[];
  readonly limits: ReferenceScriptSweepLimits;
};

const publish = async (
  fixture: Pick<Fixture, "emulator" | "lucid" | "address">,
  policy: AuthPolicy,
  tokenName: string,
  script: Script,
): Promise<void> => {
  const unit = toUnit(policy.policyId, tokenName);
  fixture.lucid.clearUTxOOverride();
  const unsigned = await fixture.lucid
    .newTx()
    .mintAssets({ [unit]: 1n })
    .attach.MintingPolicy(policy.mintingScript)
    .pay.ToAddressWithData(
      fixture.address,
      undefined,
      { lovelace: 70_000_000n, [unit]: 1n },
      script,
    )
    .validTo(policy.expiresAtUnixTime - 60_000)
    .complete();
  const signed = await unsigned.sign.withWallet().complete();
  await signed.submit();
  fixture.emulator.awaitBlock(1);
};

const LIVE_ROLE_NAMES = Object.keys(SDK.REFERENCE_SCRIPT_AUTH_TOKEN_NAMES);

const setUp = async ({
  retiredTimelockMs,
  sharedScript = false,
  strandedTarget = false,
}: {
  readonly retiredTimelockMs: number;
  /** The first retired script is also published under the live policy. */
  readonly sharedScript?: boolean;
  /** A live target whose only copy is the first retired script. */
  readonly strandedTarget?: boolean;
}): Promise<Fixture> => {
  const account = generateEmulatorAccount({ lovelace: 5_000_000_000n });
  const emulator = new Emulator([account]);
  const lucid = await Lucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(account.seedPhrase);
  const address = await lucid.wallet().address();
  const retiredPolicy = await SDK.createReferenceScriptAuthPolicy(
    lucid,
    emulator.now(),
    retiredTimelockMs,
  );
  const livePolicy = await SDK.createReferenceScriptAuthPolicy(
    lucid,
    emulator.now(),
    24 * 60 * 60 * 1_000,
  );
  const liveScripts = Array.from({ length: LIVE_COUNT }, (_, index) =>
    referenceScript(100 + index, SCRIPT_BYTES),
  );
  const retiredScripts = Array.from({ length: RETIRED_COUNT }, (_, index) =>
    sharedScript && index === 0
      ? liveScripts[0]!
      : referenceScript(1 + index, SCRIPT_BYTES),
  );
  const fixture = { emulator, lucid, address };
  for (const [index, script] of retiredScripts.entries()) {
    await publish(
      fixture,
      retiredPolicy,
      `726f6c65${index.toString(16).padStart(2, "0")}`,
      script,
    );
  }
  const liveTargets: SDK.ReferenceScriptTarget[] = liveScripts.map(
    (script, index) => ({ name: LIVE_ROLE_NAMES[index]!, script }),
  );
  for (const target of liveTargets) {
    await publish(
      fixture,
      livePolicy,
      SDK.referenceScriptAuthTokenName(target.name),
      target.script,
    );
  }
  if (strandedTarget) {
    liveTargets.push({
      name: LIVE_ROLE_NAMES[LIVE_COUNT]!,
      script: retiredScripts[0]!,
    });
  }
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
  return {
    ...fixture,
    retiredPolicy,
    livePolicy,
    retiredScripts,
    liveScripts,
    liveTargets,
    limits,
  };
};

const liveDeployment = (fixture: Fixture): LiveReferenceScriptDeployment => ({
  authPolicyId: fixture.livePolicy.policyId,
  targets: fixture.liveTargets,
});

const sweep = (
  fixture: Fixture,
  options: {
    readonly retiredAuthPolicyId?: string;
    readonly retiredAuthPolicyScript?: Script;
    readonly execute: boolean;
  },
) =>
  Effect.runPromise(
    Effect.either(
      sweepRetiredReferenceScriptsProgram({
        lucid: fixture.lucid,
        referenceScriptsAddress: fixture.address,
        live: liveDeployment(fixture),
        limits: fixture.limits,
        options: {
          retiredAuthPolicyId:
            options.retiredAuthPolicyId ?? fixture.retiredPolicy.policyId,
          retiredAuthPolicyScript: options.retiredAuthPolicyScript,
          maxReferenceScriptBytesPerBatch: BATCH_REFERENCE_BUDGET,
          execute: options.execute,
          acknowledgeRetirement: options.execute,
        },
      }),
    ),
  );

const expectSwept = (
  result: Either.Either<ReferenceScriptSweepResult, unknown>,
): ReferenceScriptSweepResult => {
  if (Either.isLeft(result)) {
    throw result.left;
  }
  return result.right;
};

const refusalCheckOf = (
  result: Either.Either<ReferenceScriptSweepResult, unknown>,
): string | undefined => {
  if (Either.isRight(result)) {
    return undefined;
  }
  const cause = (result.left as { readonly cause?: unknown }).cause;
  return cause instanceof ReferenceScriptSweepRefusal ? cause.check : undefined;
};

const unitsUnder = (utxos: readonly UTxO[], policyId: string): Assets => {
  const totals: Assets = {};
  for (const utxo of utxos) {
    for (const [unit, amount] of Object.entries(utxo.assets)) {
      if (unit.startsWith(policyId)) {
        totals[unit] = (totals[unit] ?? 0n) + amount;
      }
    }
  }
  return totals;
};

const lovelaceIn = (utxos: readonly UTxO[]): bigint =>
  utxos.reduce((total, utxo) => total + (utxo.assets.lovelace ?? 0n), 0n);

const retiredRefs = (utxos: readonly UTxO[], policyId: string) =>
  utxos.filter(
    (utxo) =>
      utxo.scriptRef != null &&
      Object.keys(utxo.assets).some((unit) => unit.startsWith(policyId)),
  );

/** The outref the live resolution picks for each live target. */
const resolvedLiveOutRefs = (
  fixture: Fixture,
  utxos: readonly UTxO[],
): (string | undefined)[] =>
  fixture.liveTargets.map((target) => {
    const resolved = resolveReferenceScriptUtxo(
      utxos,
      fixture.address,
      target,
      { policyId: fixture.livePolicy.policyId },
    );
    return resolved === undefined ? undefined : outRef(resolved);
  });

describe("retired reference-script sweep on the emulator", () => {
  it("reclaims every retired reference script in several batches and leaves live refs untouched", async () => {
    const fixture = await setUp({ retiredTimelockMs: 20 * 60 * 1_000 });
    fixture.emulator.awaitSlot(30 * 60);
    const before = await fixture.lucid.utxosAt(fixture.address);
    const retiredBefore = retiredRefs(before, fixture.retiredPolicy.policyId);
    const untouchedBefore = before
      .filter((utxo) => !retiredBefore.includes(utxo))
      .map(outRef)
      .sort();
    expect(retiredBefore).toHaveLength(RETIRED_COUNT);
    expect(
      referenceScriptLedgerBytes(retiredBefore[0]!.scriptRef!),
    ).toBeGreaterThan(SCRIPT_BYTES);

    const dryRun = expectSwept(await sweep(fixture, { execute: false }));
    expect(dryRun.dryRun).toBe(true);
    expect(dryRun.submitted).toEqual([]);
    expect(dryRun.plan.tokenDisposition).toBe("quarantine");
    expect(dryRun.plan.batches.map((batch) => batch.inputCount)).toEqual([
      3, 3, 1,
    ]);
    expect(dryRun.plan.batches[0]!.referenceScriptBytes).toBeGreaterThan(
      25_600,
    );
    expect(dryRun.plan.retainedUtxoCount).toBe(untouchedBefore.length);
    expect(
      (await fixture.lucid.utxosAt(fixture.address)).map(outRef).sort(),
    ).toEqual(before.map(outRef).sort());

    const executed = expectSwept(await sweep(fixture, { execute: true }));
    expect(executed.dryRun).toBe(false);
    expect(executed.submitted.map((batch) => batch.inputCount)).toEqual([
      3, 3, 1,
    ]);
    expect(
      executed.submitted.every(
        (batch) => batch.tokenDisposition === "quarantine",
      ),
    ).toBe(true);

    const after = await fixture.lucid.utxosAt(fixture.address);
    expect(retiredRefs(after, fixture.retiredPolicy.policyId)).toEqual([]);
    const afterRefs = new Set(after.map(outRef));
    for (const untouched of untouchedBefore) {
      expect(afterRefs.has(untouched)).toBe(true);
    }
    expect(
      retiredRefs(after, fixture.livePolicy.policyId).map(outRef).sort(),
    ).toEqual(
      retiredRefs(before, fixture.livePolicy.policyId).map(outRef).sort(),
    );
    expect(resolvedLiveOutRefs(fixture, after)).toEqual(
      resolvedLiveOutRefs(fixture, before),
    );
    expect(resolvedLiveOutRefs(fixture, after)).not.toContain(undefined);
    // The retired tokens survive, in script-free quarantine outputs at min ADA.
    const quarantine = after.filter(
      (utxo) =>
        utxo.scriptRef == null &&
        Object.keys(utxo.assets).some((unit) =>
          unit.startsWith(fixture.retiredPolicy.policyId),
        ),
    );
    expect(unitsUnder(quarantine, fixture.retiredPolicy.policyId)).toEqual(
      unitsUnder(retiredBefore, fixture.retiredPolicy.policyId),
    );
    expect(quarantine).toHaveLength(3);
    for (const output of quarantine) {
      expect(output.assets.lovelace).toBeLessThan(2_000_000n);
    }
    // Everything but fees and quarantine min ADA came back to the wallet.
    const fees = executed.submitted.reduce(
      (total, batch) => total + batch.fee,
      0n,
    );
    expect(lovelaceIn(after)).toBe(lovelaceIn(before) - fees);
    expect(
      lovelaceIn(after) -
        lovelaceIn(before.filter((u) => !retiredBefore.includes(u))),
    ).toBe(lovelaceIn(retiredBefore) - fees);

    // A re-run finds nothing: quarantine outputs are never re-selected.
    const rerun = expectSwept(await sweep(fixture, { execute: true }));
    expect(rerun.plan.totals.batchCount).toBe(0);
    expect(rerun.submitted).toEqual([]);
  }, 180_000);

  it("refuses the live auth policy at the live-auth-policy check and spends nothing", async () => {
    const fixture = await setUp({ retiredTimelockMs: 20 * 60 * 1_000 });
    const before = (await fixture.lucid.utxosAt(fixture.address))
      .map(outRef)
      .sort();

    const result = await sweep(fixture, {
      retiredAuthPolicyId: fixture.livePolicy.policyId,
      execute: true,
    });

    expect(refusalCheckOf(result)).toBe("live-auth-policy");
    expect(
      (await fixture.lucid.utxosAt(fixture.address)).map(outRef).sort(),
    ).toEqual(before);
  }, 180_000);

  it("sweeps a retired copy of a live script and leaves the live-policy copy resolving", async () => {
    const fixture = await setUp({
      retiredTimelockMs: 20 * 60 * 1_000,
      sharedScript: true,
    });
    fixture.emulator.awaitSlot(30 * 60);
    const before = await fixture.lucid.utxosAt(fixture.address);
    const resolvedBefore = resolvedLiveOutRefs(fixture, before);
    const sharedHash = validatorToScriptHash(fixture.liveScripts[0]!);
    const retiredShared = retiredRefs(
      before,
      fixture.retiredPolicy.policyId,
    ).filter((utxo) => validatorToScriptHash(utxo.scriptRef!) === sharedHash);
    expect(retiredShared).toHaveLength(1);

    const executed = expectSwept(await sweep(fixture, { execute: true }));

    expect(
      executed.plan.batches.flatMap((batch) => batch.inputOutRefs),
    ).toContain(outRef(retiredShared[0]!));
    const after = await fixture.lucid.utxosAt(fixture.address);
    expect(retiredRefs(after, fixture.retiredPolicy.policyId)).toEqual([]);
    expect(resolvedLiveOutRefs(fixture, after)).toEqual(resolvedBefore);
  }, 180_000);

  it("refuses at live-target-stranded when a live target's only copy is retired, and spends nothing", async () => {
    const fixture = await setUp({
      retiredTimelockMs: 20 * 60 * 1_000,
      strandedTarget: true,
    });
    fixture.emulator.awaitSlot(30 * 60);
    const before = (await fixture.lucid.utxosAt(fixture.address))
      .map(outRef)
      .sort();

    const result = await sweep(fixture, { execute: true });

    expect(refusalCheckOf(result)).toBe("live-target-stranded");
    expect(
      (await fixture.lucid.utxosAt(fixture.address)).map(outRef).sort(),
    ).toEqual(before);
  }, 180_000);

  it("burns the retired tokens while the retired policy is still satisfiable", async () => {
    const fixture = await setUp({ retiredTimelockMs: 60 * 60 * 1_000 });

    const executed = expectSwept(
      await sweep(fixture, {
        retiredAuthPolicyScript: fixture.retiredPolicy.mintingScript,
        execute: true,
      }),
    );

    expect(executed.plan.tokenDisposition).toBe("burn");
    expect(executed.plan.totals.quarantineOutputCount).toBe(0);
    expect(executed.plan.totals.burnedAssetCount).toBe(RETIRED_COUNT);
    expect(executed.submitted.map((batch) => batch.inputCount)).toEqual([
      3, 3, 1,
    ]);
    const after = await fixture.lucid.utxosAt(fixture.address);
    expect(unitsUnder(after, fixture.retiredPolicy.policyId)).toEqual({});
    expect(retiredRefs(after, fixture.livePolicy.policyId)).toHaveLength(
      LIVE_COUNT,
    );
  }, 180_000);
});
