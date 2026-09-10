import {
  computeMidgardNativeTxId,
  encodeMidgardFieldPreimage,
  encodeMidgardNativeTxCanonical,
  materializeMidgardNativeTxFromCanonical,
} from "@al-ft/midgard-core";
import {
  type Script,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { FRAUD_PROOF_DEPLOYMENT_ENTRIES_BY_CATEGORY } from "../src/runtime.js";
import {
  bindTransactionOutputNonCanonicalReferenceScripts,
  createTransactionOutputNonCanonicalRawL1StageResolver,
  createTransactionOutputNonCanonicalWorkflowRunnerSurface,
  deriveTransactionOutputNonCanonicalEvidenceFromCanonicalBlock,
  detectTransactionOutputNonCanonicalCompleteReplay,
  TRANSACTION_OUTPUT_NON_CANONICAL_MANIFEST_CONTRACTS,
  TRANSACTION_OUTPUT_NON_CANONICAL_VIOLATION_ID,
  type TransactionOutputNonCanonicalReferenceScripts,
} from "../src/transaction-output-non-canonical/index.js";
import type { FraudProofWorkflowDeploymentBinding } from "../src/workflow/deployment-manifest-binding.js";
import { committedFieldShapeScenarioMaterial } from "./support/committed-field-shape-emulator.js";

const script = (byte: string): Script => ({
  type: "PlutusV3",
  script: byte.repeat(8),
});
const utxo = (byte: string, outputIndex: number): UTxO => ({
  txHash: byte.repeat(64),
  outputIndex,
  address: "addr_test1vr0outputnoncanonical",
  assets: { lovelace: 2_000_000n },
  scriptRef: script(byte),
});
const references = (): TransactionOutputNonCanonicalReferenceScripts => ({
  step01: utxo("1", 0),
  step02: utxo("2", 1),
  step03: utxo("3", 2),
  step04: utxo("4", 3),
  fieldPreimageCertificateMint: utxo("5", 4),
  witnesses: {
    computationThreadMint: utxo("6", 5),
    fraudProofMint: utxo("7", 6),
    phasMembershipWithdraw: utxo("8", 7),
  },
});

const MANIFEST_NAMES = Object.values(
  TRANSACTION_OUTPUT_NON_CANONICAL_MANIFEST_CONTRACTS,
);

/** The supplied UTxO each manifest contract name must be bound to. */
const suppliedByName = (
  supplied: TransactionOutputNonCanonicalReferenceScripts,
): ReadonlyMap<string, UTxO> =>
  new Map([
    [
      TRANSACTION_OUTPUT_NON_CANONICAL_MANIFEST_CONTRACTS.step01,
      supplied.step01,
    ],
    [
      TRANSACTION_OUTPUT_NON_CANONICAL_MANIFEST_CONTRACTS.step02,
      supplied.step02,
    ],
    [
      TRANSACTION_OUTPUT_NON_CANONICAL_MANIFEST_CONTRACTS.step03,
      supplied.step03,
    ],
    [
      TRANSACTION_OUTPUT_NON_CANONICAL_MANIFEST_CONTRACTS.step04,
      supplied.step04,
    ],
    [
      TRANSACTION_OUTPUT_NON_CANONICAL_MANIFEST_CONTRACTS.computationThreadMint,
      supplied.witnesses.computationThreadMint,
    ],
    [
      TRANSACTION_OUTPUT_NON_CANONICAL_MANIFEST_CONTRACTS.fraudProofMint,
      supplied.witnesses.fraudProofMint,
    ],
    [
      TRANSACTION_OUTPUT_NON_CANONICAL_MANIFEST_CONTRACTS.phasMembershipWithdraw,
      supplied.witnesses.phasMembershipWithdraw,
    ],
    [
      TRANSACTION_OUTPUT_NON_CANONICAL_MANIFEST_CONTRACTS.fieldPreimageCertificateMint,
      supplied.fieldPreimageCertificateMint,
    ],
  ]);

const manifestBinding = (
  supplied: TransactionOutputNonCanonicalReferenceScripts,
  omit?: string,
): FraudProofWorkflowDeploymentBinding<never> =>
  ({
    referenceScriptsByContract: Object.fromEntries(
      [...suppliedByName(supplied)]
        .filter(([name]) => name !== omit)
        .map(([name, reference]) => [
          name,
          {
            outRef: `${reference.txHash}#${reference.outputIndex.toString()}`,
            scriptHash: validatorToScriptHash(reference.scriptRef!),
          },
        ]),
    ),
  }) as unknown as FraudProofWorkflowDeploymentBinding<never>;

describe("transactionOutputNonCanonical production workflow", () => {
  it("runs only its own catalogue category, and refuses a foreign one before loading any runtime configuration", async () => {
    let runtimeConfigLoads = 0;
    const runner = createTransactionOutputNonCanonicalWorkflowRunnerSurface({
      loadRuntimeConfig: async () => {
        runtimeConfigLoads += 1;
        throw new Error("runtime configuration was loaded");
      },
    });
    const invocation = (category: string) =>
      ({
        category,
        journalDirectory: "/nonexistent/transaction-output-non-canonical",
        headerHash: "aa".repeat(28),
      }) as never;

    await expect(
      runner.runOrResume(invocation("unusedRedeemer")),
    ).rejects.toThrow(
      /transactionOutputNonCanonical production runner category mismatch: unusedRedeemer/u,
    );
    // The refusal must be the category gate itself, not a downstream failure.
    expect(runtimeConfigLoads).toBe(0);

    // Accept side: its own category passes the gate, so an always-refusing
    // runner cannot satisfy the rule above.
    const ownCategory: unknown = await runner
      .runOrResume(invocation("transactionOutputNonCanonical"))
      .then(
        (value: unknown) => value,
        (cause: unknown) => cause,
      );
    expect(String(ownCategory)).not.toMatch(/category mismatch/u);
  });

  it("names the deployment registry's step contracts in step order", () => {
    // The registry that binds the published deployment is declared
    // independently of the workflow's manifest map; the two must agree, or the
    // workflow would look up reference scripts the deployment never published.
    expect([
      TRANSACTION_OUTPUT_NON_CANONICAL_MANIFEST_CONTRACTS.step01,
      TRANSACTION_OUTPUT_NON_CANONICAL_MANIFEST_CONTRACTS.step02,
      TRANSACTION_OUTPUT_NON_CANONICAL_MANIFEST_CONTRACTS.step03,
      TRANSACTION_OUTPUT_NON_CANONICAL_MANIFEST_CONTRACTS.step04,
    ]).toEqual(
      FRAUD_PROOF_DEPLOYMENT_ENTRIES_BY_CATEGORY.transactionOutputNonCanonical,
    );
  });

  it("binds reference out-refs and script identities against the manifest", () => {
    const supplied = references();
    const binding = manifestBinding(supplied);
    expect(
      bindTransactionOutputNonCanonicalReferenceScripts({
        binding,
        referenceScripts: supplied,
      }),
    ).toStrictEqual(supplied);

    // A moved out-ref, a substituted script, and a manifest that never
    // published the entry are each refused on their own.
    expect(() =>
      bindTransactionOutputNonCanonicalReferenceScripts({
        binding,
        referenceScripts: {
          ...supplied,
          step04: { ...supplied.step04, outputIndex: 99 },
        },
      }),
    ).toThrow(/differs from finalized manifest identity/u);
    expect(() =>
      bindTransactionOutputNonCanonicalReferenceScripts({
        binding,
        referenceScripts: {
          ...supplied,
          step02: { ...supplied.step02, scriptRef: script("9") },
        },
      }),
    ).toThrow(
      /fraudProofTransactionOutputNonCanonicalStep02 reference UTxO script differs from finalized manifest identity/u,
    );

    // Every declared manifest name must actually be consulted: dropping any
    // one of them from the published manifest must refuse.
    for (const name of MANIFEST_NAMES) {
      expect(
        () =>
          bindTransactionOutputNonCanonicalReferenceScripts({
            binding: manifestBinding(supplied, name),
            referenceScripts: supplied,
          }),
        name,
      ).toThrow(
        new RegExp(
          `finalized manifest has no published reference-script identity for ${name}`,
          "u",
        ),
      );
    }
  });

  it("scans every retained field-2 output and derives the sole exact violation", () => {
    const material = committedFieldShapeScenarioMaterial("honest");
    if (material.fullTx === null) throw new Error("missing full transaction");
    const canonical = Buffer.from(
      "a200581d601111111111111111111111111111111111111111111111111111111101821a004c4b40a0",
      "hex",
    );
    const malformed = Buffer.from(
      `b802${canonical.subarray(1).toString("hex")}`,
      "hex",
    );
    const blockWithOutputs = (outputs: readonly Buffer[]) => {
      const fullTx = materializeMidgardNativeTxFromCanonical({
        ...material.fullTx!,
        body: {
          ...material.fullTx!.body,
          outputsPreimageCbor: encodeMidgardFieldPreimage([...outputs]),
        },
      });
      const nodeTxId = computeMidgardNativeTxId(fullTx).toString("hex");
      return {
        nodeTxId,
        block: {
          headerHash: "a".repeat(56),
          transactions: [
            {
              nodeTxId,
              txCbor: encodeMidgardNativeTxCanonical(fullTx).toString("hex"),
            },
          ],
          reconstruction: { forcedTransactions: [] },
        },
      };
    };

    const { nodeTxId, block } = blockWithOutputs([canonical, malformed]);
    const detections = detectTransactionOutputNonCanonicalCompleteReplay(
      block as never,
    );
    expect(detections).toHaveLength(1);
    // Position, header and coordinate are all part of the detection contract:
    // transaction 0, field 2, item 1, and the malformed item's own width.
    expect(detections[0]).toEqual({
      detectionId: `${TRANSACTION_OUTPUT_NON_CANONICAL_VIOLATION_ID}:0:${nodeTxId}:2:1:${malformed.length.toString()}`,
      headerHash: "a".repeat(56),
      violationId: TRANSACTION_OUTPUT_NON_CANONICAL_VIOLATION_ID,
      position: 0n,
      diagnostic: `transaction ${nodeTxId} field 2 item 1 has illegal width ${malformed.length.toString()}`,
    });

    const evidence =
      deriveTransactionOutputNonCanonicalEvidenceFromCanonicalBlock(
        block as never,
      );
    expect(evidence.itemIndex).toBe(1);
    expect(evidence.fieldIndex).toBe(2);
    expect(evidence.subject.transaction_id).toBe(nodeTxId);
    expect(evidence.itemHex).toBe(malformed.toString("hex"));
    expect(evidence.canonical).toBe(false);
    expect(evidence.decisiveFaultHolds).toBe(true);

    // The scan must not flag a canonical retained output ...
    const honest = blockWithOutputs([canonical, canonical]);
    expect(
      detectTransactionOutputNonCanonicalCompleteReplay(honest.block as never),
    ).toEqual([]);
    expect(() =>
      deriveTransactionOutputNonCanonicalEvidenceFromCanonicalBlock(
        honest.block as never,
      ),
    ).toThrow(/yielded 0 exact findings/u);

    // ... and "the sole violation" means exactly one: two malformed outputs
    // cannot be silently narrowed to one finding.
    const pair = blockWithOutputs([malformed, malformed]);
    expect(
      detectTransactionOutputNonCanonicalCompleteReplay(pair.block as never),
    ).toHaveLength(2);
    expect(() =>
      deriveTransactionOutputNonCanonicalEvidenceFromCanonicalBlock(
        pair.block as never,
      ),
    ).toThrow(/yielded 2 exact findings/u);
  });

  it("resolves each physical checkpoint against the authenticated raw-L1 stage", async () => {
    const headerHash = "a".repeat(56);
    const observedHeaderHashes: string[] = [];
    const resolverFor = (
      stage: Record<string, unknown>,
    ): ReturnType<
      typeof createTransactionOutputNonCanonicalRawL1StageResolver
    > =>
      createTransactionOutputNonCanonicalRawL1StageResolver({
        config: { binding: { definition: { headerHash } } } as never,
        l1: {
          observe: async (request: { readonly headerHash: string }) => {
            observedHeaderHashes.push(request.headerHash);
            return { stage };
          },
        } as never,
        source: { nativeTxCompactCbor: "aa", witnessSetCompactCbor: "bb" },
      });
    const stepStage = (step: number) => ({
      kind: "step",
      step,
      threadOutRef: `${"b".repeat(64)}#0`,
      stateQueueBlockOutRef: `${"c".repeat(64)}#0`,
    });

    // Accept: the requested step is the one L1 actually stands at, and the
    // resolved stage carries the observed out-refs plus the authenticated
    // source bytes.
    await expect(
      resolverFor(stepStage(4))({
        action: "submitStep04",
        evidence: {} as never,
      }),
    ).resolves.toEqual({
      fraudulentBlockOutRef: `${"c".repeat(64)}#0`,
      threadOutRef: `${"b".repeat(64)}#0`,
      nativeTxCompactCbor: "aa",
      witnessSetCompactCbor: "bb",
    });
    // The stage is read for the workflow's own bound header.
    expect(observedHeaderHashes).toEqual([headerHash]);

    // Reject: the same action against any other authenticated step.
    for (const step of [1, 2, 3]) {
      await expect(
        resolverFor(stepStage(step))({
          action: "submitStep04",
          evidence: {} as never,
        }),
      ).rejects.toThrow(
        /submitStep04 differs from authenticated raw-L1 stage/u,
      );
    }
    // Reject: the right step for a different action.
    await expect(
      resolverFor(stepStage(4))({
        action: "submitStep03",
        evidence: {} as never,
      }),
    ).rejects.toThrow(/submitStep03 differs from authenticated raw-L1 stage/u);

    // Init and removal have their own stage predicates in both polarities.
    await expect(
      resolverFor({
        kind: "not_started",
        stateQueueBlockOutRef: `${"c".repeat(64)}#0`,
      })({ action: "submitInit", evidence: {} as never }),
    ).resolves.toEqual({ fraudulentBlockOutRef: `${"c".repeat(64)}#0` });
    await expect(
      resolverFor(stepStage(4))({
        action: "submitInit",
        evidence: {} as never,
      }),
    ).rejects.toThrow(/init requires raw-L1 not_started/u);
    await expect(
      resolverFor({
        kind: "proof_token",
        stateQueueBlockOutRef: `${"c".repeat(64)}#0`,
      })({ action: "removeDescendants", evidence: {} as never }),
    ).resolves.toEqual({ fraudulentBlockOutRef: `${"c".repeat(64)}#0` });
    await expect(
      resolverFor(stepStage(4))({
        action: "removeDescendants",
        evidence: {} as never,
      }),
    ).rejects.toThrow(/removal requires raw-L1 proof token/u);
  });
});
