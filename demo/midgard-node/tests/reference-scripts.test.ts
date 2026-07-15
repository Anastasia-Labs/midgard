import "./utils.js";

import { referenceScriptAuthTokenName } from "@al-ft/midgard-sdk";
import {
  type Assets,
  type LucidEvolution,
  toUnit,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it, vi } from "vitest";

import { AlwaysSucceedsContract } from "@/services/always-succeeds.js";
import {
  buildReferenceScriptDeploymentPlan,
  buildReferenceScriptSweepPlan,
  buildReferenceScriptWalletStatus,
  nodeRuntimeReferenceScriptTargets,
  plainAdaOnlyUtxosFromKupoMatches,
  REFERENCE_SCRIPT_COMMAND_NAMES,
  REFERENCE_SCRIPT_CONFIRMATION_TIMEOUT_MS,
  REFERENCE_SCRIPT_SWEEP_DEFAULT_TOKEN_OUTPUT_LOVELACE,
  referenceScriptTargetsByCommand,
  referenceScriptUtxosFromKupoMatches,
  referenceScriptWalletStatusProgram,
  resolveSpendableWalletUtxos,
  verifyNodeRuntimeReferenceScriptsProgram,
} from "@/transactions/reference-scripts.js";

const REFERENCE_SCRIPT_ADDRESS = "addr_test1reference";
const RETURN_ADDRESS = "addr_test1return";
const BURN_ADDRESS = "addr_test1burn";

const txHashFixture = (value: string): string => value.padStart(64, "0");

const mkUtxo = ({
  txHash,
  outputIndex = 0,
  assets,
  scriptRef = false,
  datum,
  datumHash,
}: {
  readonly txHash: string;
  readonly outputIndex?: number;
  readonly assets: Assets;
  readonly scriptRef?: boolean;
  readonly datum?: string;
  readonly datumHash?: string;
}): UTxO => ({
  txHash: txHashFixture(txHash),
  outputIndex,
  address: REFERENCE_SCRIPT_ADDRESS,
  assets,
  ...(datum === undefined ? {} : { datum }),
  ...(datumHash === undefined ? {} : { datumHash }),
  ...(scriptRef
    ? {
        scriptRef: {
          type: "Native" as const,
          script: "8200",
        },
      }
    : {}),
});

describe("node-runtime reference-script registry", () => {
  it("exposes node-runtime as the primary deployment command", () => {
    expect(REFERENCE_SCRIPT_COMMAND_NAMES[0]).toEqual("node-runtime");
    expect(REFERENCE_SCRIPT_COMMAND_NAMES).toContain("node-runtime");
  });

  it("contains the static scripts currently used by node runtime flows", async () => {
    const contracts = await Effect.runPromise(
      Effect.gen(function* () {
        return yield* AlwaysSucceedsContract;
      }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
    );
    const targets = nodeRuntimeReferenceScriptTargets(contracts);
    const names = targets.map(({ name }) => name);

    expect(new Set(names).size).toEqual(names.length);
    expect(names).toContain("reference-script-auth minting");
    expect(names).toContain("hub-oracle minting");
    expect(names).toContain("da-params-governor spending");
    expect(names).toContain("da-params-governor minting");
    expect(names).toContain("da-attestation spending");
    expect(names).toContain("da-attestation minting");
    expect(names).toContain("scheduler spending");
    expect(names).toContain("scheduler minting");
    expect(names).toContain("state-queue spending");
    expect(names).toContain("state-queue minting");
    expect(names).toContain("registered-operators spending");
    expect(names).toContain("registered-operators minting");
    expect(names).toContain("active-operators spending");
    expect(names).toContain("active-operators minting");
    expect(names).toContain("retired-operators spending");
    expect(names).toContain("retired-operators minting");
    expect(names).toContain("fraud-proof-catalogue minting");
    expect(names).toContain("deposit minting");
    expect(names).toContain("deposit spending");
    expect(names).toContain("withdrawal minting");
    expect(names).toContain("withdrawal spending");
    expect(names).toContain("settlement minting");
    expect(names).toContain("membership proof withdrawal");
    expect(names).toContain("reserve spending");
    expect(names).toContain("reserve observer");
    expect(names).toContain("payout spending");
    expect(names).toContain("payout minting");
  });

  it("derives protocol-init as a strict subset of node-runtime", async () => {
    const contracts = await Effect.runPromise(
      Effect.gen(function* () {
        return yield* AlwaysSucceedsContract;
      }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
    );
    const byCommand = referenceScriptTargetsByCommand(contracts);
    const runtimeNames = new Set(
      byCommand["node-runtime"].map(({ name }) => name),
    );

    for (const initTarget of byCommand["protocol-init"]) {
      expect(runtimeNames.has(initTarget.name)).toEqual(true);
    }
  });

  it("exposes reserve and payout script sets as explicit deployment commands", async () => {
    const contracts = await Effect.runPromise(
      Effect.gen(function* () {
        return yield* AlwaysSucceedsContract;
      }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
    );
    const byCommand = referenceScriptTargetsByCommand(contracts);

    expect(REFERENCE_SCRIPT_COMMAND_NAMES).toContain("reserve");
    expect(REFERENCE_SCRIPT_COMMAND_NAMES).toContain("payout");
    expect(REFERENCE_SCRIPT_COMMAND_NAMES).toContain("withdrawal");
    expect(REFERENCE_SCRIPT_COMMAND_NAMES).toContain("phas-membership");
    expect(byCommand.deposit.map(({ name }) => name)).toEqual([
      "deposit minting",
      "deposit spending",
    ]);
    expect(byCommand.withdrawal.map(({ name }) => name)).toEqual([
      "withdrawal minting",
      "withdrawal spending",
    ]);
    expect(byCommand.reserve.map(({ name }) => name)).toEqual([
      "reserve spending",
      "reserve observer",
    ]);
    expect(byCommand["phas-membership"].map(({ name }) => name)).toEqual([
      "membership proof withdrawal",
    ]);
    expect(byCommand.payout.map(({ name }) => name)).toEqual([
      "payout spending",
      "payout minting",
    ]);
  });

  it("accepts a complete published node-runtime reference-script set", async () => {
    const contracts = await Effect.runPromise(
      Effect.gen(function* () {
        return yield* AlwaysSucceedsContract;
      }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
    );
    const targets = nodeRuntimeReferenceScriptTargets(contracts);
    const authPolicy = contracts.referenceScriptAuth;
    const lucidWithReferences = {
      utxosAt: async () =>
        targets.map(
          (target, index): UTxO => ({
            txHash: index.toString(16).padStart(64, "0"),
            outputIndex: index,
            address: REFERENCE_SCRIPT_ADDRESS,
            assets: {
              lovelace: 4_000_000n,
              [toUnit(
                authPolicy.policyId,
                referenceScriptAuthTokenName(target.name),
              )]: 1n,
            },
            scriptRef: target.script,
          }),
        ),
    } as unknown as LucidEvolution;

    const resolved = await Effect.runPromise(
      verifyNodeRuntimeReferenceScriptsProgram(
        lucidWithReferences,
        REFERENCE_SCRIPT_ADDRESS,
        contracts,
        authPolicy,
      ),
    );

    expect(resolved.map(({ name }) => name)).toEqual(
      targets.map(({ name }) => name),
    );
  });

  it("fails startup verification with a complete missing-reference diagnostic", async () => {
    const contracts = await Effect.runPromise(
      Effect.gen(function* () {
        return yield* AlwaysSucceedsContract;
      }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
    );
    const emptyLucid = {
      utxosAt: async () => [] as UTxO[],
    } as unknown as LucidEvolution;

    const result = await Effect.runPromise(
      Effect.either(
        verifyNodeRuntimeReferenceScriptsProgram(
          emptyLucid,
          REFERENCE_SCRIPT_ADDRESS,
          contracts,
          contracts.referenceScriptAuth,
        ),
      ),
    );

    expect(result._tag).toEqual("Left");
    if (result._tag === "Left") {
      expect(result.left.message).toEqual(
        "Missing node-runtime reference scripts",
      );
      expect(String(result.left.cause)).toContain("reserve spending");
      expect(String(result.left.cause)).toContain("payout minting");
    }
  });

  it("rejects a complete script-ref set without auth role tokens", async () => {
    const contracts = await Effect.runPromise(
      Effect.gen(function* () {
        return yield* AlwaysSucceedsContract;
      }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
    );
    const targets = nodeRuntimeReferenceScriptTargets(contracts);
    const lucidWithBareReferences = {
      utxosAt: async () =>
        targets.map(
          (target, index): UTxO => ({
            txHash: index.toString(16).padStart(64, "0"),
            outputIndex: index,
            address: REFERENCE_SCRIPT_ADDRESS,
            assets: { lovelace: 4_000_000n },
            scriptRef: target.script,
          }),
        ),
    } as unknown as LucidEvolution;

    const result = await Effect.runPromise(
      Effect.either(
        verifyNodeRuntimeReferenceScriptsProgram(
          lucidWithBareReferences,
          REFERENCE_SCRIPT_ADDRESS,
          contracts,
          contracts.referenceScriptAuth,
        ),
      ),
    );

    expect(result._tag).toEqual("Left");
    if (result._tag === "Left") {
      expect(result.left.message).toEqual(
        "Missing node-runtime reference scripts",
      );
      expect(String(result.left.cause)).toContain("hub-oracle minting");
    }
  });
});

describe("reference-script wallet sweep planner", () => {
  it("selects script-ref and token-bearing UTxOs while retaining plain ADA-only UTxOs by default", () => {
    const roleUnit = `${"a".repeat(56)}01`;
    const otherUnit = `${"b".repeat(56)}02`;
    const scriptRefUtxo = mkUtxo({
      txHash: "1",
      assets: { lovelace: 4_000_000n, [roleUnit]: 1n },
      scriptRef: true,
    });
    const tokenOnlyUtxo = mkUtxo({
      txHash: "2",
      assets: { lovelace: 2_000_000n, [otherUnit]: 3n },
    });
    const plainUtxo = mkUtxo({
      txHash: "3",
      assets: { lovelace: 10_000_000n },
    });

    const plan = buildReferenceScriptSweepPlan({
      utxos: [plainUtxo, tokenOnlyUtxo, scriptRefUtxo],
      referenceScriptsAddress: REFERENCE_SCRIPT_ADDRESS,
      returnAddress: RETURN_ADDRESS,
      burnAddress: BURN_ADDRESS,
      dryRun: true,
    });

    expect(plan.summary.sweepableUtxoCount).toEqual(2);
    expect(plan.summary.retainedUtxoCount).toEqual(1);
    expect(plan.summary.inputLovelace).toEqual(6_000_000n);
    expect(plan.summary.quarantineLovelace).toEqual(
      REFERENCE_SCRIPT_SWEEP_DEFAULT_TOKEN_OUTPUT_LOVELACE,
    );
    expect(plan.summary.nonLovelaceAssets).toEqual({
      [roleUnit]: 1n,
      [otherUnit]: 3n,
    });
    expect(plan.summary.sweepableOutRefs).toEqual([
      `${scriptRefUtxo.txHash}#0`,
      `${tokenOnlyUtxo.txHash}#0`,
    ]);
    expect(plan.summary.retainedOutRefs).toEqual([`${plainUtxo.txHash}#0`]);
    expect(plan.summary.walletStatus.total.lovelace).toEqual(16_000_000n);
    expect(plan.summary.walletStatus.plainAdaOnly.lovelace).toEqual(
      10_000_000n,
    );
    expect(plan.summary.walletStatus.scriptRefOrTokenBearing.lovelace).toEqual(
      6_000_000n,
    );
    expect(plan.summary.walletStatus.sweepHint?.dryRunCommand).toContain(
      "sweep-reference-script-wallet",
    );
  });

  it("includes plain ADA-only UTxOs when full consolidation is requested", () => {
    const roleUnit = `${"c".repeat(56)}03`;
    const referenceUtxo = mkUtxo({
      txHash: "10",
      assets: { lovelace: 4_000_000n, [roleUnit]: 1n },
      scriptRef: true,
    });
    const plainUtxo = mkUtxo({
      txHash: "11",
      assets: { lovelace: 12_000_000n },
    });

    const plan = buildReferenceScriptSweepPlan({
      utxos: [plainUtxo, referenceUtxo],
      referenceScriptsAddress: REFERENCE_SCRIPT_ADDRESS,
      returnAddress: RETURN_ADDRESS,
      dryRun: true,
      includePlainUtxos: true,
    });

    expect(plan.summary.sweepableUtxoCount).toEqual(2);
    expect(plan.summary.retainedUtxoCount).toEqual(0);
    expect(plan.summary.inputLovelace).toEqual(16_000_000n);
  });

  it("chunks token quarantine outputs by configured non-ADA asset count", () => {
    const units = [
      `${"d".repeat(56)}00`,
      `${"d".repeat(56)}01`,
      `${"d".repeat(56)}02`,
    ];
    const utxo = mkUtxo({
      txHash: "20",
      assets: {
        lovelace: 10_000_000n,
        [units[0]]: 1n,
        [units[1]]: 2n,
        [units[2]]: 3n,
      },
    });

    const plan = buildReferenceScriptSweepPlan({
      utxos: [utxo],
      referenceScriptsAddress: REFERENCE_SCRIPT_ADDRESS,
      returnAddress: RETURN_ADDRESS,
      burnAddress: BURN_ADDRESS,
      dryRun: true,
      tokenOutputLovelace: 2_000_000n,
      maxAssetsPerTokenOutput: 2,
    });

    expect(plan.summary.tokenOutputs.map((output) => output.units)).toEqual([
      [units[0], units[1]],
      [units[2]],
    ]);
    expect(
      plan.summary.tokenOutputs.map((output) => output.nonLovelaceAssetCount),
    ).toEqual([2, 1]);
    expect(plan.summary.quarantineLovelace).toEqual(4_000_000n);
  });

  it("reports datum-bearing ADA separately from plain ADA-only and sweepable trapped ADA", () => {
    const roleUnit = `${"e".repeat(56)}04`;
    const plainUtxo = mkUtxo({
      txHash: "30",
      assets: { lovelace: 9_000_000n },
    });
    const datumUtxo = mkUtxo({
      txHash: "31",
      assets: { lovelace: 5_000_000n },
      datum: "d87980",
    });
    const datumHashUtxo = mkUtxo({
      txHash: "32",
      assets: { lovelace: 6_000_000n },
      datumHash: "ab".repeat(32),
    });
    const scriptRefUtxo = mkUtxo({
      txHash: "33",
      assets: { lovelace: 4_000_000n, [roleUnit]: 1n },
      scriptRef: true,
    });

    const status = buildReferenceScriptWalletStatus({
      utxos: [datumHashUtxo, scriptRefUtxo, plainUtxo, datumUtxo],
      referenceScriptsAddress: REFERENCE_SCRIPT_ADDRESS,
    });

    expect(status.total).toMatchObject({
      utxoCount: 4,
      lovelace: 24_000_000n,
    });
    expect(status.plainAdaOnly).toMatchObject({
      utxoCount: 1,
      lovelace: 9_000_000n,
    });
    expect(status.scriptRefOrTokenBearing).toMatchObject({
      utxoCount: 1,
      lovelace: 4_000_000n,
      nonLovelaceAssetUnitCount: 1,
    });
    expect(status.otherIgnored).toMatchObject({
      utxoCount: 2,
      lovelace: 11_000_000n,
    });
  });
});

describe("reference-script deployment planner", () => {
  it("uses a reference-script-only confirmation window beyond the shared 90-second default", () => {
    expect(REFERENCE_SCRIPT_CONFIRMATION_TIMEOUT_MS).toBe(30 * 60 * 1_000);
    expect(REFERENCE_SCRIPT_CONFIRMATION_TIMEOUT_MS).toBeGreaterThan(90_000);
  });

  it("parses only plain ADA UTxOs from raw Kupo address matches", () => {
    const parsed = plainAdaOnlyUtxosFromKupoMatches(
      [
        {
          transaction_id: txHashFixture("60"),
          output_index: 0,
          address: REFERENCE_SCRIPT_ADDRESS,
          value: { coins: "12000000", assets: {} },
          datum_hash: null,
          datum_type: null,
          script_hash: null,
        },
        {
          transaction_id: txHashFixture("61"),
          output_index: 1,
          address: REFERENCE_SCRIPT_ADDRESS,
          value: { coins: "4000000", assets: {} },
          datum_hash: null,
          datum_type: null,
          script_hash: "ab".repeat(28),
        },
        {
          transaction_id: txHashFixture("62"),
          output_index: 2,
          address: REFERENCE_SCRIPT_ADDRESS,
          value: { coins: "5000000", assets: { [`${"a".repeat(56)}01`]: "1" } },
          datum_hash: null,
          datum_type: null,
          script_hash: null,
        },
        {
          transaction_id: txHashFixture("63"),
          output_index: 3,
          address: REFERENCE_SCRIPT_ADDRESS,
          value: { coins: "6000000", assets: {} },
          datum_hash: "cd".repeat(32),
          datum_type: "hash",
          script_hash: null,
        },
      ],
      REFERENCE_SCRIPT_ADDRESS,
    );

    expect(parsed).toEqual([
      {
        txHash: txHashFixture("60"),
        outputIndex: 0,
        address: REFERENCE_SCRIPT_ADDRESS,
        assets: { lovelace: 12_000_000n },
      },
    ]);
  });

  it("rejects Kupo matches from an unexpected address", () => {
    expect(() =>
      plainAdaOnlyUtxosFromKupoMatches(
        [
          {
            transaction_id: txHashFixture("64"),
            output_index: 0,
            address: "addr_test1unexpected",
            value: { coins: "12000000", assets: {} },
            datum_hash: null,
            datum_type: null,
            script_hash: null,
          },
        ],
        REFERENCE_SCRIPT_ADDRESS,
      ),
    ).toThrow("Kupo match address mismatch");
  });

  it("resolves targeted reference scripts from raw Kupo matches by role token and script hash", async () => {
    const contracts = await Effect.runPromise(
      Effect.gen(function* () {
        return yield* AlwaysSucceedsContract;
      }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
    );
    const targets = nodeRuntimeReferenceScriptTargets(contracts);
    const target = targets[0]!;
    const authPolicy = contracts.referenceScriptAuth;
    const roleUnit = `${authPolicy.policyId}.${referenceScriptAuthTokenName(
      target.name,
    )}`;
    const matches = [
      {
        transaction_id: txHashFixture("65"),
        output_index: 0,
        address: REFERENCE_SCRIPT_ADDRESS,
        value: { coins: "4000000", assets: { [roleUnit]: "1" } },
        datum_hash: null,
        datum_type: null,
        script_hash: validatorToScriptHash(target.script),
      },
      {
        transaction_id: txHashFixture("66"),
        output_index: 1,
        address: REFERENCE_SCRIPT_ADDRESS,
        value: { coins: "4000000", assets: { [roleUnit]: "1" } },
        datum_hash: null,
        datum_type: null,
        script_hash: "ff".repeat(28),
      },
    ];

    const resolved = referenceScriptUtxosFromKupoMatches(
      matches,
      REFERENCE_SCRIPT_ADDRESS,
      [target],
      authPolicy,
    );

    expect(resolved).toHaveLength(1);
    expect(resolved[0]).toMatchObject({
      txHash: txHashFixture("65"),
      outputIndex: 0,
      address: REFERENCE_SCRIPT_ADDRESS,
      assets: {
        lovelace: 4_000_000n,
        [`${authPolicy.policyId}${referenceScriptAuthTokenName(target.name)}`]:
          1n,
      },
      scriptRef: target.script,
    });
  });

  it("resolves Kupo asset quantities for reference scripts", async () => {
    const contracts = await Effect.runPromise(
      Effect.gen(function* () {
        return yield* AlwaysSucceedsContract;
      }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
    );
    const targets = nodeRuntimeReferenceScriptTargets(contracts);
    const target = targets[0]!;
    const authPolicy = contracts.referenceScriptAuth;
    const roleUnit = `${authPolicy.policyId}.${referenceScriptAuthTokenName(
      target.name,
    )}`;
    const matches = [
      {
        transaction_id: txHashFixture("67"),
        output_index: 0,
        address: REFERENCE_SCRIPT_ADDRESS,
        value: { coins: "4000000", assets: { [roleUnit]: "1" } },
        datum_hash: null,
        datum_type: null,
        script_hash: validatorToScriptHash(target.script),
      },
    ];

    const resolved = referenceScriptUtxosFromKupoMatches(
      matches,
      REFERENCE_SCRIPT_ADDRESS,
      [target],
      authPolicy,
    );

    expect(resolved).toHaveLength(1);
    expect(resolved[0]?.assets).toMatchObject({
      lovelace: 4_000_000n,
      [`${authPolicy.policyId}${referenceScriptAuthTokenName(target.name)}`]:
        1n,
    });
  });

  it("builds wallet status from raw Kupo matches without hydrating script bodies", async () => {
    const fetchSpy = vi.spyOn(globalThis, "fetch").mockResolvedValue(
      new Response(
        JSON.stringify([
          {
            transaction_id: txHashFixture("73"),
            output_index: 0,
            address: REFERENCE_SCRIPT_ADDRESS,
            value: { coins: "9000000", assets: {} },
            datum_hash: null,
            datum_type: null,
            script_hash: null,
          },
          {
            transaction_id: txHashFixture("74"),
            output_index: 1,
            address: REFERENCE_SCRIPT_ADDRESS,
            value: {
              coins: "4000000",
              assets: { [`${"e".repeat(56)}.04`]: "1" },
            },
            datum_hash: null,
            datum_type: null,
            script_hash: "cd".repeat(28),
          },
          {
            transaction_id: txHashFixture("75"),
            output_index: 2,
            address: REFERENCE_SCRIPT_ADDRESS,
            value: { coins: "5000000", assets: {} },
            datum_hash: "ab".repeat(32),
            datum_type: "hash",
            script_hash: null,
          },
        ]),
        { status: 200, headers: { "content-type": "application/json" } },
      ),
    );
    const utxosAt = vi.fn(() =>
      Promise.reject(new Error("Lucid hydration must not run")),
    );
    const lucid = {
      config: () => ({ provider: { kupoUrl: "http://kupo.test" } }),
      utxosAt,
    } as unknown as LucidEvolution;

    try {
      const status = await Effect.runPromise(
        referenceScriptWalletStatusProgram(lucid, REFERENCE_SCRIPT_ADDRESS),
      );

      expect(status.total).toMatchObject({
        utxoCount: 3,
        lovelace: 18_000_000n,
      });
      expect(status.plainAdaOnly).toMatchObject({
        utxoCount: 1,
        lovelace: 9_000_000n,
      });
      expect(status.scriptRefOrTokenBearing).toMatchObject({
        utxoCount: 1,
        lovelace: 4_000_000n,
        nonLovelaceAssetUnitCount: 1,
      });
      expect(status.otherIgnored).toMatchObject({
        utxoCount: 1,
        lovelace: 5_000_000n,
      });
      expect(utxosAt).not.toHaveBeenCalled();
      expect(fetchSpy).toHaveBeenCalledTimes(1);
      const requestedUrl = String(fetchSpy.mock.calls[0]?.[0]);
      expect(requestedUrl).toEqual(
        `http://kupo.test/matches/${encodeURIComponent(REFERENCE_SCRIPT_ADDRESS)}?unspent`,
      );
      expect(requestedUrl).not.toContain("/scripts/");
    } finally {
      fetchSpy.mockRestore();
    }
  });

  it("excludes reserved outrefs from spendable funding UTxOs", async () => {
    const reserved = mkUtxo({
      txHash: "50",
      assets: { lovelace: 100_000_000n },
    });
    const available = mkUtxo({
      txHash: "51",
      outputIndex: 1,
      assets: { lovelace: 60_000_000n },
    });
    const tokenBearing = mkUtxo({
      txHash: "52",
      outputIndex: 2,
      assets: { lovelace: 70_000_000n, [`${"a".repeat(56)}01`]: 1n },
    });
    const utxos = [reserved, available, tokenBearing];
    const byOutRef = new Map(
      utxos.map((utxo) => [`${utxo.txHash}#${utxo.outputIndex}`, utxo]),
    );
    const lucid = {
      wallet: () => ({
        getUtxos: async () => utxos,
      }),
      utxosByOutRef: async (
        refs: readonly {
          readonly txHash: string;
          readonly outputIndex: number;
        }[],
      ) =>
        refs
          .map((ref) => byOutRef.get(`${ref.txHash}#${ref.outputIndex}`))
          .filter((utxo): utxo is UTxO => utxo !== undefined),
    } as unknown as LucidEvolution;

    const spendable = await Effect.runPromise(
      resolveSpendableWalletUtxos(
        lucid,
        new Set([`${reserved.txHash}#${reserved.outputIndex.toString()}`]),
      ),
    );

    expect(
      spendable.map((utxo) => `${utxo.txHash}#${utxo.outputIndex}`),
    ).toEqual([`${available.txHash}#${available.outputIndex}`]);
  });

  it("precomputes missing targets, conservative batches, and aggregate top-up", () => {
    const targets = Array.from({ length: 9 }, (_, index) => ({
      name: `target-${index.toString()}`,
      script: {
        type: "Native" as const,
        script: "8200",
      },
    }));
    const walletUtxos = [
      mkUtxo({
        txHash: "40",
        assets: { lovelace: 10_000_000n },
      }),
      mkUtxo({
        txHash: "41",
        assets: { lovelace: 4_000_000n, [`${"f".repeat(56)}01`]: 1n },
      }),
    ];

    const plan = buildReferenceScriptDeploymentPlan({
      scopeName: "node-runtime",
      targets,
      existingTargetNames: new Set(["target-0", "target-1"]),
      walletUtxos,
      maxTargetsPerBatch: 3,
    });

    expect(plan.existingTargetNames).toEqual(["target-0", "target-1"]);
    expect(plan.missingTargetNames).toEqual([
      "target-2",
      "target-3",
      "target-4",
      "target-5",
      "target-6",
      "target-7",
      "target-8",
    ]);
    expect(plan.currentPlainBalance).toEqual(10_000_000n);
    expect(plan.requiredPlainBalance).toEqual(50_000_000n);
    expect(plan.topUpLovelace).toEqual(40_000_000n);
    expect(plan.submitCount).toEqual(3);
    expect(plan.batches.map((batch) => batch.targetNames)).toEqual([
      ["target-2", "target-3", "target-4"],
      ["target-5", "target-6", "target-7"],
      ["target-8"],
    ]);
  });
});
