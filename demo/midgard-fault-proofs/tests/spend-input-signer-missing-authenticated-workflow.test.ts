import {
  type Script,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  bindSpendInputSignerMissingReferenceScripts,
  createSpendInputSignerMissingRawL1StageResolver,
  createSpendInputSignerMissingWorkflowRunnerSurface,
  SPEND_INPUT_SIGNER_MISSING_MANIFEST_CONTRACTS,
  type SpendInputSignerMissingDeploymentBinding,
  type SpendInputSignerMissingReferenceScripts,
} from "../src/spend-input-signer-missing/index.js";
import { WORKFLOW_ADAPTER_RUNNER } from "../src/workflow/adapters.js";

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
const references = (): SpendInputSignerMissingReferenceScripts => ({
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

/**
 * The finalized manifest as a deployment would publish it: contract name to
 * the one out-ref and script hash that name is allowed to resolve to. Written
 * out here rather than zipped against the production role table, so a
 * production change that re-pointed a role at another contract's publication
 * has to disagree with this table instead of moving with it.
 */
const MANIFEST_IDENTITY: Readonly<Record<string, UTxO>> = Object.freeze({
  fraudProofSpendInputSignerMissing: utxo("1", 0),
  fraudProofSpendInputSignerMissingStep02: utxo("2", 1),
  fraudProofSpendInputSignerMissingStep03: utxo("3", 2),
  fraudProofSpendInputSignerMissingStep04: utxo("4", 3),
  fraudProofSpendInputSignerMissingStep05: utxo("5", 4),
  fieldPreimageCertificateMint: utxo("6", 5),
  computationThreadMint: utxo("7", 6),
  fraudProofMint: utxo("8", 7),
  phasMembershipWithdraw: utxo("9", 8),
});

const bindingFrom = (
  identity: Readonly<Record<string, UTxO>>,
): SpendInputSignerMissingDeploymentBinding =>
  ({
    referenceScriptsByContract: Object.fromEntries(
      Object.entries(identity).map(([name, published]) => [
        name,
        {
          outRef: `${published.txHash}#${published.outputIndex.toString()}`,
          scriptHash: validatorToScriptHash(published.scriptRef!),
        },
      ]),
    ),
  }) as unknown as SpendInputSignerMissingDeploymentBinding;

const withoutContract = (name: string): Readonly<Record<string, UTxO>> =>
  Object.fromEntries(
    Object.entries(MANIFEST_IDENTITY).filter(([key]) => key !== name),
  );

describe("spendInputSignerMissing production workflow", () => {
  /**
   * Central admission drives this family through one method and hands it no
   * hooks: no verdict callback, no evidence callback, no journal callback. The
   * claim is stated as a property of the surface — `runOrResume` is its only
   * callable member — rather than as a copy of the member list, so adding any
   * callback fails here without a rename being able to.
   */
  it("exposes no callable member besides the single drive method", () => {
    const runner = createSpendInputSignerMissingWorkflowRunnerSurface({
      loadRuntimeConfig: async () => {
        throw new Error("not reached");
      },
    });

    expect(runner.runnerVersion).toBe(WORKFLOW_ADAPTER_RUNNER);
    expect(
      Object.entries(runner)
        .filter(([, member]) => typeof member === "function")
        .map(([name]) => name),
    ).toEqual(["runOrResume"]);
  });

  it("refuses a foreign category before it reaches the runtime loader", async () => {
    let loads = 0;
    const runner = createSpendInputSignerMissingWorkflowRunnerSurface({
      loadRuntimeConfig: async () => {
        loads += 1;
        throw new Error("not reached");
      },
    });

    await expect(
      runner.runOrResume({ category: "unusedRedeemer" } as never),
    ).rejects.toThrow(/category mismatch: unusedRedeemer/u);
    // The refusal contract also prohibits the side effect: a mismatched
    // category must not cause a manifest/runtime load.
    expect(loads).toBe(0);
  });

  it("accepts the finalized manifest identity for every role", () => {
    expect(
      bindSpendInputSignerMissingReferenceScripts({
        binding: bindingFrom(MANIFEST_IDENTITY),
        referenceScripts: references(),
      }),
    ).toStrictEqual(references());
  });

  it.each(Object.keys(MANIFEST_IDENTITY))(
    "refuses a manifest that publishes no identity for %s",
    (name) => {
      // Same demonstrated-valid scenario as above with exactly one contract
      // withheld: every role must be covered, so no role can be bound from an
      // incomplete manifest.
      expect(() =>
        bindSpendInputSignerMissingReferenceScripts({
          binding: bindingFrom(withoutContract(name)),
          referenceScripts: references(),
        }),
      ).toThrow(
        new RegExp(
          `finalized manifest has no published reference-script identity for ${name}$`,
          "u",
        ),
      );
    },
  );

  it("refuses a reference UTxO published for a different role", () => {
    const swapped = references();
    expect(() =>
      bindSpendInputSignerMissingReferenceScripts({
        binding: bindingFrom(MANIFEST_IDENTITY),
        referenceScripts: {
          ...swapped,
          step02: swapped.step03,
          step03: swapped.step02,
        },
      }),
    ).toThrow(
      /fraudProofSpendInputSignerMissingStep02 reference UTxO differs from finalized manifest identity/u,
    );
  });

  it("refuses a reference UTxO at the wrong output index", () => {
    const supplied = references();
    expect(() =>
      bindSpendInputSignerMissingReferenceScripts({
        binding: bindingFrom(MANIFEST_IDENTITY),
        referenceScripts: {
          ...supplied,
          step05: { ...supplied.step05, outputIndex: 99 },
        },
      }),
    ).toThrow(
      /fraudProofSpendInputSignerMissingStep05 reference UTxO differs from finalized manifest identity/u,
    );
  });

  it("refuses a reference UTxO carrying a substituted script", () => {
    const supplied = references();
    expect(() =>
      bindSpendInputSignerMissingReferenceScripts({
        binding: bindingFrom(MANIFEST_IDENTITY),
        referenceScripts: {
          ...supplied,
          // Right out-ref, wrong payload: only the script-hash check can
          // reject this one.
          step04: { ...supplied.step04, scriptRef: script("a") },
        },
      }),
    ).toThrow(
      /fraudProofSpendInputSignerMissingStep04 reference UTxO script differs from finalized manifest identity/u,
    );
  });

  it("names every role the production table exposes", () => {
    // The refusal cases above are driven from MANIFEST_IDENTITY; this keeps
    // that table honest about the roles the family actually binds, so a new
    // role cannot be added to production without gaining the coverage above.
    expect(
      Object.values(SPEND_INPUT_SIGNER_MISSING_MANIFEST_CONTRACTS).sort(),
    ).toEqual(Object.keys(MANIFEST_IDENTITY).sort());
  });

  it("derives scanning and final stages only from authenticated raw L1", async () => {
    let stage = {
      kind: "step" as const,
      step: 4,
      threadOutRef: `${"a".repeat(64)}#0`,
      stateQueueBlockOutRef: `${"b".repeat(64)}#0`,
    };
    const resolver = createSpendInputSignerMissingRawL1StageResolver({
      config: {
        binding: { definition: { headerHash: "c".repeat(56) } },
      } as never,
      l1: { observe: async () => ({ stage }) } as never,
      source: { nativeTxCompactCbor: "aa", witnessSetCompactCbor: "bb" },
    });
    await expect(
      resolver({ action: "submitScan", evidence: {} as never }),
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
