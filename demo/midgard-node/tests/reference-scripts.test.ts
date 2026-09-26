import "./utils.js";

import { DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE } from "@al-ft/midgard-core/deployment-manifest-identity";
import { referenceScriptAuthTokenName } from "@al-ft/midgard-sdk";
import {
  type Assets,
  type LucidEvolution,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it, vi } from "vitest";

import { AlwaysSucceedsContract } from "../src/services/always-succeeds.js";
import {
  buildReferenceScriptDeploymentPlan,
  buildReferenceScriptWalletStatus,
  nodeRuntimeReferenceScriptTargets,
  REFERENCE_SCRIPT_COMMAND_NAMES,
  REFERENCE_SCRIPT_CONFIRMATION_TIMEOUT_MS,
  referenceScriptTargetsByCommand,
  referenceScriptWalletStatusProgram,
  resolveSpendableWalletUtxos,
  verifyNodeRuntimeReferenceScriptsProgram,
} from "../src/transactions/reference-scripts.js";
import { withRealEventHistoryForTest } from "./helpers/event-history.js";
import { loadRealMidgardContractsForTest } from "./helpers/real-midgard-contracts.js";

const REFERENCE_SCRIPT_ADDRESS = "addr_test1reference";

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
  it("publishes exactly every manifest role for the real contract set", async () => {
    const contracts = await loadRealMidgardContractsForTest({
      txHash: txHashFixture("0"),
      outputIndex: 0,
    });
    const targets = nodeRuntimeReferenceScriptTargets(contracts);

    expect(targets.map(({ name }) => name).sort()).toEqual(
      Object.keys(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE).sort(),
    );
    expect(
      targets.find(({ name }) => name === "V1 field-preimage certificate")
        ?.script,
    ).toEqual(contracts.fieldPreimageCertificate.spendingScript);
    expect(
      targets.find(
        ({ name }) => name === "V1 field-preimage certificate minting",
      )?.script,
    ).toEqual(contracts.fieldPreimageCertificate.mintingScript);
  });

  it("exposes node-runtime as the primary deployment command", () => {
    expect(REFERENCE_SCRIPT_COMMAND_NAMES[0]).toEqual("node-runtime");
    expect(REFERENCE_SCRIPT_COMMAND_NAMES).toContain("node-runtime");
  });

  it("contains the static scripts currently used by node runtime flows", async () => {
    const contracts = await Effect.runPromise(
      Effect.gen(function* () {
        return withRealEventHistoryForTest(yield* AlwaysSucceedsContract, {
          txHash: "00".repeat(32),
          outputIndex: 0,
        });
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
    expect(
      names.filter((name) => name.startsWith("V1 validation-trace ")).sort(),
    ).toEqual(
      Object.keys(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE)
        .filter((name) => name.startsWith("V1 validation-trace "))
        .sort(),
    );
    const registeredFraudProofNames = names.filter((name) =>
      name.startsWith("V1 fraud-proof "),
    );
    // Node-runtime publishes EVERY `V1 fraud-proof ` role the canonical
    // manifest declares. This is stated as the set rather than as a count
    // because it is a coverage requirement, not a pin to maintain: a manifest
    // is only valid when `validateReferenceScripts` finds a confirmed
    // reference script for every declared role, so a family the registry does
    // not enumerate is a deployment that cannot pass startup verification. A
    // count let that gap sit as a stale number; the set names it.
    expect([...registeredFraudProofNames].sort()).toEqual(
      Object.keys(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE)
        .filter((role) => role.startsWith("V1 fraud-proof "))
        .sort(),
    );
    expect(registeredFraudProofNames).toContain(
      "V1 fraud-proof missing-signature step-04",
    );
    expect(registeredFraudProofNames).toContain(
      "V1 fraud-proof missing-native-script-tx step-06",
    );
  });

  it("derives protocol-init as a strict subset of node-runtime", async () => {
    const contracts = await Effect.runPromise(
      Effect.gen(function* () {
        return withRealEventHistoryForTest(yield* AlwaysSucceedsContract, {
          txHash: "00".repeat(32),
          outputIndex: 0,
        });
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
        return withRealEventHistoryForTest(yield* AlwaysSucceedsContract, {
          txHash: "00".repeat(32),
          outputIndex: 0,
        });
      }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
    );
    const byCommand = referenceScriptTargetsByCommand(contracts);

    expect(REFERENCE_SCRIPT_COMMAND_NAMES).toContain("reserve");
    expect(REFERENCE_SCRIPT_COMMAND_NAMES).toContain("payout");
    expect(REFERENCE_SCRIPT_COMMAND_NAMES).toContain("withdrawal");
    expect(REFERENCE_SCRIPT_COMMAND_NAMES).toContain("phas-membership");
    expect(byCommand.deposit.map(({ name }) => name)).toEqual([
      "deposit history retention",
      "deposit history retirement",
      "deposit minting",
      "deposit spending",
    ]);
    expect(byCommand.withdrawal.map(({ name }) => name)).toEqual([
      "withdrawal history retention",
      "withdrawal history retirement",
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
        return withRealEventHistoryForTest(yield* AlwaysSucceedsContract, {
          txHash: "00".repeat(32),
          outputIndex: 0,
        });
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
        return withRealEventHistoryForTest(yield* AlwaysSucceedsContract, {
          txHash: "00".repeat(32),
          outputIndex: 0,
        });
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
        return withRealEventHistoryForTest(yield* AlwaysSucceedsContract, {
          txHash: "00".repeat(32),
          outputIndex: 0,
        });
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

describe("reference-script wallet status", () => {
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
    expect(status.sweepHint?.dryRunCommand).toContain(
      "sweep-reference-script-wallet --retired-auth-policy <retired-policy-id>",
    );
    expect(status.sweepHint?.executeCommand).toContain(
      "--execute --i-am-retiring-reference-scripts",
    );
  });
});

describe("reference-script deployment planner", () => {
  it("uses a reference-script-only confirmation window beyond the shared 90-second default", () => {
    expect(REFERENCE_SCRIPT_CONFIRMATION_TIMEOUT_MS).toBe(30 * 60 * 1_000);
    expect(REFERENCE_SCRIPT_CONFIRMATION_TIMEOUT_MS).toBeGreaterThan(90_000);
  });

  it("builds wallet status from Lucid's provider-neutral hydrated UTxOs", async () => {
    const utxos = [
      mkUtxo({ txHash: "73", assets: { lovelace: 9_000_000n } }),
      mkUtxo({
        txHash: "74",
        outputIndex: 1,
        assets: { lovelace: 4_000_000n, [`${"e".repeat(56)}04`]: 1n },
        scriptRef: true,
      }),
      mkUtxo({
        txHash: "75",
        outputIndex: 2,
        assets: { lovelace: 5_000_000n },
        datumHash: "ab".repeat(32),
      }),
    ];
    const utxosAt = vi.fn(async () => utxos);
    const lucid = { utxosAt } as unknown as LucidEvolution;

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
    expect(utxosAt).toHaveBeenCalledWith(REFERENCE_SCRIPT_ADDRESS);
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
