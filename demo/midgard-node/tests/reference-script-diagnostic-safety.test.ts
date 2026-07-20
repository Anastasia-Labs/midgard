import type { ReferenceScriptAuthMintingPolicy } from "@al-ft/midgard-sdk";
import { CML, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it, vi } from "vitest";

const { completePublicationMock, handleSignSubmitMock, signCaptureMock } =
  vi.hoisted(() => ({
    completePublicationMock: vi.fn(),
    handleSignSubmitMock: vi.fn(),
    signCaptureMock: vi.fn(),
  }));

vi.mock("@al-ft/midgard-sdk", async (importOriginal) => {
  const actual = await importOriginal<typeof import("@al-ft/midgard-sdk")>();
  return {
    ...actual,
    completeReferenceScriptPublicationTxProgram: completePublicationMock,
  };
});

vi.mock("@/transactions/utils.js", async (importOriginal) => {
  const actual =
    await importOriginal<typeof import("@/transactions/utils.js")>();
  return {
    ...actual,
    handleSignSubmit: handleSignSubmitMock,
    signTransactionForPreSubmitCapture: signCaptureMock,
  };
});

import {
  buildReferenceScriptDeploymentPlan,
  ensureReferenceScriptTargetsProgram,
} from "@/transactions/reference-scripts.js";

const REFERENCE_SCRIPT_ADDRESS = "addr_test1reference";
const FUNDING_ADDRESS = "addr_test1funding";

const plainUtxo: UTxO = {
  txHash: "11".repeat(32),
  outputIndex: 0,
  address: REFERENCE_SCRIPT_ADDRESS,
  assets: { lovelace: 1_000_000n },
};

const target = {
  name: "diagnostic target",
  script: {
    type: "Native" as const,
    script: "8200",
  },
};

const authPolicy = {
  mintingScriptCBOR: "8200",
  policyId: "aa".repeat(28),
  mintingScript: {
    type: "Native" as const,
    script: "8200",
  },
  expiresAtUnixTime: Date.now() + 60 * 60 * 1_000,
} satisfies ReferenceScriptAuthMintingPolicy;

describe("reference-script diagnostic submission safety", () => {
  it("refuses a required automatic top-up before any submit-capable funding path runs", async () => {
    handleSignSubmitMock.mockClear();
    const referenceWalletAddress = vi
      .fn<() => Promise<string>>()
      .mockResolvedValue(REFERENCE_SCRIPT_ADDRESS);
    const referenceWalletUtxos = vi
      .fn<() => Promise<UTxO[]>>()
      .mockResolvedValue([plainUtxo]);
    const referenceUtxosAt = vi
      .fn<(address: string) => Promise<UTxO[]>>()
      .mockResolvedValue([plainUtxo]);
    const overrideReferenceUtxos = vi.fn();
    const referenceScriptsLucid = {
      wallet: () => ({
        address: referenceWalletAddress,
        getUtxos: referenceWalletUtxos,
      }),
      utxosAt: referenceUtxosAt,
      overrideUTxOs: overrideReferenceUtxos,
    } as unknown as LucidEvolution;

    const fundingWalletAccess = vi.fn();
    const fundingTransactionBuild = vi.fn();
    const fundingLucid = {
      wallet: () => {
        fundingWalletAccess();
        return {
          address: async () => FUNDING_ADDRESS,
          getUtxos: async () => [],
        };
      },
      newTx: fundingTransactionBuild,
    } as unknown as LucidEvolution;

    const deploymentPlan = buildReferenceScriptDeploymentPlan({
      scopeName: "diagnostic",
      targets: [target],
      existingTargetNames: new Set(),
      walletUtxos: [plainUtxo],
    });
    expect(deploymentPlan.topUpLovelace).toBeGreaterThan(0n);

    await expect(
      Effect.runPromise(
        ensureReferenceScriptTargetsProgram(
          referenceScriptsLucid,
          "diagnostic",
          [target],
          authPolicy,
          fundingLucid,
          undefined,
          0,
          new Set(),
          {
            outputDirectory: "/tmp/reference-script-diagnostic-safety",
            invocation: "phase4-live-pre-submit-capture",
            abortBeforeSubmit: true,
            session: {
              commandName: "diagnostic",
              runStatePath: "/tmp/run-state.json",
              blueprintPath: "/tmp/plutus.json",
              blueprintSha256: "11".repeat(32),
              ledgerProtocolMajor: 11,
              network: "Preprod",
              hubOracleOneShotOutRef: `${"22".repeat(32)}#0`,
              referenceScriptAuthPolicyId: authPolicy.policyId,
            },
          },
        ),
      ),
    ).rejects.toThrow(
      /Pre-submit diagnostic capture refuses automatic reference-script wallet replenishment/,
    );

    expect(referenceWalletAddress).toHaveBeenCalled();
    expect(referenceWalletUtxos).toHaveBeenCalled();
    expect(referenceUtxosAt).toHaveBeenCalled();
    expect(overrideReferenceUtxos).toHaveBeenCalled();
    expect(fundingWalletAccess).not.toHaveBeenCalled();
    expect(fundingTransactionBuild).not.toHaveBeenCalled();
    expect(handleSignSubmitMock).not.toHaveBeenCalled();
  });

  it("captures all 27 targets exactly once across recursive split leaves while preserving synthetic change lineage", async () => {
    completePublicationMock.mockReset();
    handleSignSubmitMock.mockReset();
    signCaptureMock.mockReset();
    const targets = Array.from({ length: 27 }, (_, index) => ({
      name: `target-${index.toString().padStart(2, "0")}`,
      script: {
        type: "Native" as const,
        script: CML.NativeScript.new_script_invalid_hereafter(
          999_999n + BigInt(index),
        ).to_cbor_hex(),
      },
    }));
    let walletUtxos: UTxO[] = [
      {
        txHash: "66".repeat(32),
        outputIndex: 0,
        address: REFERENCE_SCRIPT_ADDRESS,
        assets: { lovelace: 10_000_000_000n },
      },
    ];
    const utxosAt = vi.fn(async () => [...walletUtxos]);
    const utxosByOutRef = vi.fn(async () => [] as UTxO[]);
    const overrideUTxOs = vi.fn((next: UTxO[]) => {
      walletUtxos = [...next];
    });
    const lucid = {
      wallet: () => ({
        address: async () => REFERENCE_SCRIPT_ADDRESS,
        getUtxos: async () => [...walletUtxos],
      }),
      config: () => ({ provider: {} }),
      utxosAt,
      utxosByOutRef,
      overrideUTxOs,
    } as unknown as LucidEvolution;
    const fundingWalletAccess = vi.fn();
    const fundingLucid = {
      wallet: () => {
        fundingWalletAccess();
        return {
          address: async () => FUNDING_ADDRESS,
          getUtxos: async () => [],
        };
      },
    } as unknown as LucidEvolution;

    completePublicationMock.mockImplementation(
      ({
        selectedFundingInputs,
        referenceScriptsAddress,
        missingTargets,
      }: {
        readonly selectedFundingInputs: readonly UTxO[];
        readonly referenceScriptsAddress: string;
        readonly missingTargets: typeof targets;
      }) => {
        if (missingTargets.length > 2) {
          return Effect.fail(
            new Error(
              `Max transaction size of 16384 exceeded. Found: ${(
                20_000 + missingTargets.length
              ).toString()}`,
            ),
          );
        }
        const inputLovelace = selectedFundingInputs.reduce(
          (total, utxo) => total + (utxo.assets.lovelace ?? 0n),
          0n,
        );
        const localReferenceOutputs = new Map(
          missingTargets.map((target, outputIndex) => [
            target.name,
            {
              outputIndex,
              address: referenceScriptsAddress,
              assets: { lovelace: 4_000_000n },
              scriptRef: target.script,
            },
          ]),
        );
        return Effect.succeed({
          tx: { diagnostic: true },
          layout: {
            localReferenceOutputs,
            walletOutputs: [
              // Production publishes the script refs to the reference-script
              // wallet's own address, so the SDK layout includes these in
              // walletOutputs as well as localReferenceOutputs.
              ...localReferenceOutputs.values(),
              {
                outputIndex: missingTargets.length,
                address: referenceScriptsAddress,
                assets: {
                  lovelace:
                    inputLovelace - BigInt(missingTargets.length) * 4_000_000n,
                },
              },
            ],
          },
        });
      },
    );
    signCaptureMock.mockImplementation(
      (
        _lucid: unknown,
        _unsigned: unknown,
        options: {
          readonly batch: {
            readonly ordinal: number;
          };
        },
      ) =>
        Effect.succeed({
          status: "captured_not_submitted" as const,
          txHash: (options.batch.ordinal + 1).toString(16).padStart(64, "0"),
          signedTxCbor: "00",
          walletAddress: REFERENCE_SCRIPT_ADDRESS,
          cborPath: "/tmp/capture.cbor",
          metadataPath: "/tmp/capture.cbor.json",
        }),
    );

    const resolved = await Effect.runPromise(
      ensureReferenceScriptTargetsProgram(
        lucid,
        "node-runtime",
        targets,
        authPolicy,
        fundingLucid,
        REFERENCE_SCRIPT_ADDRESS,
        1,
        new Set(),
        {
          outputDirectory: "/tmp/reference-script-all-batch-capture",
          invocation: "phase4-live-pre-submit-capture",
          abortBeforeSubmit: true,
          session: {
            commandName: "node-runtime",
            runStatePath: "/tmp/run-state.json",
            blueprintPath: "/tmp/plutus.json",
            blueprintSha256: "77".repeat(32),
            ledgerProtocolMajor: 11,
            network: "Preprod",
            hubOracleOneShotOutRef: `${"88".repeat(32)}#0`,
            referenceScriptAuthPolicyId: authPolicy.policyId,
          },
        },
      ),
    );

    expect(resolved.map(({ name }) => name)).toEqual(
      targets.map(({ name }) => name),
    );
    const batches = signCaptureMock.mock.calls.map(
      (call) =>
        (
          call[2] as {
            readonly batch: {
              readonly ordinal: number;
              readonly splitPath: string;
              readonly targets: readonly {
                readonly name: string;
                readonly outputIndex: number;
              }[];
              readonly inputs: readonly {
                readonly lineage: "live_seed" | "synthetic_change";
              }[];
              readonly walletChangeOutputIndexes: readonly number[];
            };
          }
        ).batch,
    );
    expect(batches).toHaveLength(14);
    expect(batches.map(({ ordinal }) => ordinal)).toEqual(
      Array.from({ length: 14 }, (_, index) => index),
    );
    const capturedTargetNames = batches.flatMap(({ targets }) =>
      targets.map(({ name }) => name),
    );
    expect(capturedTargetNames).toHaveLength(27);
    expect(new Set(capturedTargetNames)).toEqual(
      new Set(targets.map(({ name }) => name)),
    );
    expect(batches.some(({ splitPath }) => splitPath.endsWith(".L"))).toBe(
      true,
    );
    expect(batches.some(({ splitPath }) => splitPath.endsWith(".R"))).toBe(
      true,
    );
    expect(batches[0]?.inputs).toEqual([
      expect.objectContaining({ lineage: "live_seed" }),
    ]);
    for (const batch of batches.slice(1)) {
      expect(batch.inputs).toEqual([
        expect.objectContaining({ lineage: "synthetic_change" }),
      ]);
    }
    for (const batch of batches) {
      expect(batch.walletChangeOutputIndexes).toHaveLength(1);
      const targetOutputIndexes = new Set(
        batch.targets.map(({ outputIndex }) => outputIndex),
      );
      expect(
        batch.walletChangeOutputIndexes.some((outputIndex) =>
          targetOutputIndexes.has(outputIndex),
        ),
      ).toBe(false);
    }
    expect(utxosAt).toHaveBeenCalledTimes(1);
    expect(utxosByOutRef).not.toHaveBeenCalled();
    expect(fundingWalletAccess).not.toHaveBeenCalled();
    expect(handleSignSubmitMock).not.toHaveBeenCalled();
  });
});
