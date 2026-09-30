import "./reference-scripts.node-runtime-reference-script-registry.js";

import { type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it, vi } from "vitest";

import {
  buildReferenceScriptDeploymentPlan,
  buildReferenceScriptWalletStatus,
  REFERENCE_SCRIPT_CONFIRMATION_TIMEOUT_MS,
  referenceScriptWalletStatusProgram,
  resolveSpendableWalletUtxos,
} from "../src/transactions/reference-scripts.js";
import {
  mkUtxo,
  REFERENCE_SCRIPT_ADDRESS,
} from "./reference-scripts.mk-utxo.js";

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
