import {
  type Script,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  bindResolvedOutputNonCanonicalReferenceScripts,
  createResolvedOutputNonCanonicalRawL1StageResolver,
  createResolvedOutputNonCanonicalWorkflowRunnerSurface,
  RESOLVED_OUTPUT_NON_CANONICAL_MANIFEST_CONTRACTS,
  type ResolvedOutputNonCanonicalReferenceScripts,
} from "../src/resolved-output-non-canonical/index.js";
import type { FraudProofWorkflowDeploymentBinding } from "../src/workflow/deployment-manifest-binding-v1.js";

const script = (byte: string): Script => ({
  type: "PlutusV3",
  script: byte.repeat(8),
});
const utxo = (byte: string, outputIndex: number): UTxO => ({
  txHash: byte.repeat(64),
  outputIndex,
  address: "addr_test1vr0resolvedoutput",
  assets: { lovelace: 2_000_000n },
  scriptRef: script(byte),
});
const references = (): ResolvedOutputNonCanonicalReferenceScripts => ({
  step01: utxo("1", 0),
  step02: utxo("2", 1),
  step03: utxo("3", 2),
  step04: utxo("4", 3),
  step05: utxo("5", 4),
  fieldPreimageCertificateMint: utxo("6", 5),
  witnesses: {
    computationThreadMint: utxo("7", 6),
    fraudProofMint: utxo("8", 7),
    phasMembershipWithdraw: utxo("9", 8),
  },
});

describe("resolvedOutputNonCanonical production workflow", () => {
  it("exposes the standard callback-free runner and complete manifest roles", () => {
    const runner = createResolvedOutputNonCanonicalWorkflowRunnerSurface({
      loadRuntimeConfig: async () => {
        throw new Error("not reached");
      },
    });
    expect(Object.keys(runner).sort()).toEqual([
      "runOrResume",
      "runnerVersion",
    ]);
    expect(
      Object.values(RESOLVED_OUTPUT_NON_CANONICAL_MANIFEST_CONTRACTS),
    ).toEqual([
      "fraudProofResolvedOutputNonCanonical",
      "fraudProofResolvedOutputNonCanonicalStep02",
      "fraudProofResolvedOutputNonCanonicalStep03",
      "fraudProofResolvedOutputNonCanonicalStep04",
      "fraudProofResolvedOutputNonCanonicalStep05",
      "computationThreadMint",
      "fraudProofMint",
      "phasMembershipWithdraw",
      "fieldPreimageCertificateMint",
    ]);
  });

  it("authenticates every reference out-ref and refuses substitution", () => {
    const supplied = references();
    const names = Object.values(
      RESOLVED_OUTPUT_NON_CANONICAL_MANIFEST_CONTRACTS,
    );
    const values = [
      supplied.step01,
      supplied.step02,
      supplied.step03,
      supplied.step04,
      supplied.step05,
      supplied.witnesses.computationThreadMint,
      supplied.witnesses.fraudProofMint,
      supplied.witnesses.phasMembershipWithdraw,
      supplied.fieldPreimageCertificateMint,
    ];
    const binding = {
      referenceScriptsByContract: Object.fromEntries(
        names.map((name, index) => [
          name,
          {
            outRef: `${values[index]!.txHash}#${values[index]!.outputIndex.toString()}`,
            scriptHash: validatorToScriptHash(values[index]!.scriptRef!),
          },
        ]),
      ),
    } as unknown as FraudProofWorkflowDeploymentBinding<never>;
    expect(
      bindResolvedOutputNonCanonicalReferenceScripts({
        binding,
        referenceScripts: supplied,
      }),
    ).toStrictEqual(supplied);
    expect(() =>
      bindResolvedOutputNonCanonicalReferenceScripts({
        binding,
        referenceScripts: {
          ...supplied,
          step05: { ...supplied.step05, outputIndex: 99 },
        },
      }),
    ).toThrow(/differs from finalized manifest identity/u);
  });

  it("derives reconstruction and final stages only from authenticated raw L1", async () => {
    let stage = {
      kind: "step" as const,
      step: 4,
      threadOutRef: `${"a".repeat(64)}#0`,
      stateQueueBlockOutRef: `${"b".repeat(64)}#0`,
    };
    const resolver = createResolvedOutputNonCanonicalRawL1StageResolver({
      config: {
        binding: { definition: { headerHash: "c".repeat(56) } },
      } as never,
      l1: { observe: async () => ({ stage }) } as never,
      source: { nativeTxCompactCbor: "aa", witnessSetCompactCbor: "bb" },
    });
    await expect(
      resolver({ action: "submitReconstruction", evidence: {} as never }),
    ).resolves.toEqual(
      expect.objectContaining({
        threadOutRef: `${"a".repeat(64)}#0`,
        fraudulentBlockOutRef: `${"b".repeat(64)}#0`,
      }),
    );
    stage = { ...stage, step: 5 };
    await expect(
      resolver({ action: "submitStep05", evidence: {} as never }),
    ).resolves.toEqual(
      expect.objectContaining({ threadOutRef: `${"a".repeat(64)}#0` }),
    );
    await expect(
      resolver({ action: "submitStep03", evidence: {} as never }),
    ).rejects.toThrow(/differs from authenticated raw-L1 stage/u);
  });
});
