import * as SDK from "@al-ft/midgard-sdk";
import type { UTxO } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import { submitCommittedFieldShapeInit } from "../src/committed-field-shape/submit-committed-field-shape-init.js";
import type {
  DistinctAssetAccumulationCoordinate,
  DistinctAssetAccumulationEvidence,
} from "../src/distinct-asset-accumulation-limit/family.js";
import { buildDistinctAssetAuthenticationFromRetainedDa } from "../src/distinct-asset-accumulation-limit/retained-value-and-mint.js";
import { submitDistinctAssetAccumulationFold } from "../src/distinct-asset-accumulation-limit/submit-fold.js";
import { submitDistinctAssetAccumulationStep01Forced } from "../src/distinct-asset-accumulation-limit/submit-step-01.js";
import { submitDistinctAssetAccumulationStep02 } from "../src/distinct-asset-accumulation-limit/submit-step-02.js";
import { submitDistinctAssetAccumulationStep06 } from "../src/distinct-asset-accumulation-limit/submit-step-06.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import { buildCountedRoot } from "../src/transition-trace/phas.js";
import { buildForcedTransactionLeafMembershipProof } from "../src/transition-trace/witnesses.js";
import {
  type CrossingScenario,
  type CrossingTrace,
  ORDER_KEY,
  outputReason,
} from "./forced-reason-coordinate-distinct-asset.setup.js";
import { buildCatalogueDeploymentInfo } from "./support/emulator/catalogue.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import { publishPlainReferenceScriptUtxo } from "./support/emulator/reference-scripts.js";
import { buildRemovalDeploymentInfo } from "./support/emulator/removal-deployment.js";
import { submitSetupTx } from "./support/emulator/setup-tx.js";
import { buildDecodingBlockFixture } from "./support/native-script-decoding-emulator.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  funderPaymentKeyHash,
  publishRemovalReferenceScripts,
  registerChunkedVerifyRewardAccount,
} from "./support/submit-init-emulator-shared.js";

/**
 * The emulator thread of the distinct-asset coordinate suite: a block that
 * commits one forced crossing with a chosen reason and its real validation
 * trace, and the family's steps over it.
 */

const network = "Custom" as const;

/**
 * A block committing the forced leaf with `committed` as its reason, its
 * real validation trace, and a thread ready to dispute it.
 */
export const commitDistinctAssetBlock = async ({
  crossing,
  built,
  committed,
}: {
  readonly crossing: CrossingScenario;
  readonly built: CrossingTrace;
  readonly committed: {
    readonly outputIndex: number;
    readonly assetIndex: number;
  };
}) => {
  const harness = await makeFaultProofEmulatorHarness({
    registerAdditionalRewardAccounts: registerChunkedVerifyRewardAccount,
    contractOptions: {
      realDistinctAssetAccumulationLimit: true,
      alwaysFraudProofCatalogue: true,
    },
  });
  const contracts = harness.contracts.distinctAssetAccumulationLimit!;
  const catalogue = await buildCatalogueDeploymentInfo({
    ...harness.contracts.fraudProofs,
    distinctAssetAccumulationLimit: {
      ...contracts.steps[0],
      spendingScriptCBOR: contracts.steps[0].spendingScript.script,
    },
  });
  const category = catalogue.categories.distinctAssetAccumulationLimit!;
  const reason = outputReason(committed);
  const block = await buildDecodingBlockFixture({
    operatorVkey: await funderPaymentKeyHash(harness.funderLucid),
    startTime: BigInt(
      alignUnixTimeToEmulatorSlotBoundary(
        harness.funderLucid,
        harness.emulator.now() + 120_000,
      ) - 1,
    ),
    priorLedgerRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    subject: {
      kind: "forced",
      nativeTx: crossing.nativeTx,
      orderKey: ORDER_KEY,
      verdict: { ForcedTxInvalid: { reason } },
    },
  });
  expect(block.nativeTxId).toBe(crossing.transactionId.toString("hex"));
  const validationEntries = [
    { key: built.eventKeyCbor, value: built.descriptorCbor },
    ...block.reconstruction.payload.block_body.validation_traces
      .filter(([key]) => key !== built.eventKeyCbor.toString("hex"))
      .map(([key, value]) => ({
        key: Buffer.from(key, "hex"),
        value: Buffer.from(value, "hex"),
      })),
  ];
  const traceRoot = await buildCountedRoot(
    SDK.ROOT_DOMAINS.validationTraces,
    validationEntries,
  );
  const header = {
    ...block.header,
    validationTracesRoot: traceRoot.root,
    validationTraceCount: traceRoot.count,
  };
  const setup = await submitSetupTx({
    lucid: harness.funderLucid,
    contracts: harness.contracts,
    nonceUtxo: harness.nonceUtxo,
    catalogue,
    header,
  });
  const references: UTxO[] = [];
  for (const [index, step] of contracts.steps.entries())
    references.push(
      (
        await publishPlainReferenceScriptUtxo({
          lucid: harness.funderLucid,
          script: step.spendingScript,
          label: `distinct-asset-coordinate-${index.toString()}`,
        })
      ).utxo,
    );
  // The block as a watcher reconstructs it from L1 and DA.
  const canonicalBlock = {
    headerHash: setup.headerHash,
    header,
    reconstruction: {
      ...block.reconstruction,
      payload: {
        ...block.reconstruction.payload,
        block_body: {
          ...block.reconstruction.payload.block_body,
          validation_traces: validationEntries.map(({ key, value }) => [
            key.toString("hex"),
            value.toString("hex"),
          ]),
          validation_trace_witnesses: built.retainedWitnesses.map(
            ([key, value]) => [key.toString("hex"), value.toString("hex")],
          ),
        },
      },
    },
    transactions: block.reconstruction.transactions.map((entry) => ({
      nodeTxId: Buffer.from(entry.keyBytes).toString("hex"),
      txCbor: entry.fullTransactionCbor.toString("hex"),
      l2TransactionSourceCbor: entry.valueBytes.toString("hex"),
    })),
  } as never;
  const forcedSource = {
    header,
    membership: await buildForcedTransactionLeafMembershipProof({
      reconstruction: block.reconstruction,
      eventKey: built.eventKey,
    }),
    direction: 1n,
  };
  const subject = SDK.forcedVerdictSubject({
    transactionId: block.nativeTxId,
    sourceKey: ORDER_KEY,
    rejectionReason: reason,
  });
  const common = {
    lucid: harness.proverLucid,
    contracts,
    categoryId: category.categoryId,
    signer: harness.proverSigner,
  };
  const init = async () =>
    (
      await submitCommittedFieldShapeInit({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        network,
        contracts: contracts as never,
        category: category as never,
        catalogue: {
          policyId: harness.contracts.fraudProofCatalogue.policyId,
          spendingScriptAddress:
            harness.contracts.fraudProofCatalogue.spendingScriptAddress,
          root: catalogue.root,
        },
        signer: harness.proverSigner,
        fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      })
    ).nextThreadOutRef;
  const step01 = async (
    threadOutRef: string,
    coordinate: DistinctAssetAccumulationCoordinate,
    unsafeSkipLocalViolationCheckForTest = false,
  ) =>
    (
      await submitDistinctAssetAccumulationStep01Forced({
        ...common,
        threadOutRef,
        finding: { subject, coordinate },
        forcedSource,
        referenceScriptUtxo: references[0]!,
        unsafeSkipLocalViolationCheckForTest,
      })
    ).nextThreadOutRef;
  /** Steps 02 through 05 over the retained mutation at `coordinate`. */
  const authenticateAndFold = async (
    threadOutRef: string,
    coordinate: DistinctAssetAccumulationCoordinate,
  ) => {
    const retained = await buildDistinctAssetAuthenticationFromRetainedDa({
      eventKey: built.eventKey,
      finding: { subject, coordinate },
      authenticatedValidationTraceEntries: validationEntries,
      retainedValidationWitnessEntries: built.retainedWitnesses.map(
        ([key, value]) => ({ key, value }),
      ),
      expectedValidationTracesRoot: traceRoot.root,
    });
    let outRef = (
      await submitDistinctAssetAccumulationStep02({
        ...common,
        threadOutRef,
        authentication: retained.authentication,
        referenceScriptUtxo: references[1]!,
      })
    ).nextThreadOutRef;
    for (const stepIndex of [2, 3, 4] as const)
      outRef = (
        await submitDistinctAssetAccumulationFold({
          ...common,
          threadOutRef: outRef,
          stepIndex,
          action: retained.folds[stepIndex - 2]!,
          referenceScriptUtxo: references[stepIndex]!,
        })
      ).nextThreadOutRef;
    return outRef;
  };
  const finalize = async (
    threadOutRef: string,
    evidence: DistinctAssetAccumulationEvidence,
  ) =>
    await submitDistinctAssetAccumulationStep06({
      ...common,
      threadOutRef,
      evidence,
      referenceScriptUtxo: references[5]!,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
  /** Remove the block with the thread's permanent proof. */
  const removeBlock = async () => {
    const removal = await publishRemovalReferenceScripts({
      lucid: harness.proverLucid,
      contracts: harness.contracts,
    });
    const baseDeployment = buildRemovalDeploymentInfo(
      harness.contracts,
      catalogue,
      { removalReferenceScripts: removal.published },
    );
    const names = [
      "fraudProofDistinctAssetAccumulationLimit",
      "fraudProofDistinctAssetAccumulationLimitStep02",
      "fraudProofDistinctAssetAccumulationLimitStep03",
      "fraudProofDistinctAssetAccumulationLimitStep04",
      "fraudProofDistinctAssetAccumulationLimitStep05",
      "fraudProofDistinctAssetAccumulationLimitStep06",
    ];
    const now = BigInt(harness.emulator.now());
    await submitRemoveFraudulentBlock({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      deploymentInfo: {
        ...baseDeployment,
        contracts: {
          ...baseDeployment.contracts,
          ...Object.fromEntries(
            contracts.steps.map((step, index) => [
              names[index]!,
              {
                scriptHash: step.spendingScriptHash,
                contract: {
                  type: step.spendingScript.type,
                  cborHex: step.spendingScript.script,
                },
              },
            ]),
          ),
        },
      },
      network,
      signer: harness.proverSigner,
      fraudCategory: "distinctAssetAccumulationLimit" as never,
      fraudulentHeaderHash: setup.headerHash,
      awaitConfirmation: true,
      requireReferenceScripts: true,
      stateQueueMutationLeaseCoordinator: {
        acquire: async () => ({
          token: "distinct-asset-coordinate-emulator",
          source: "emulator",
          renew: async () => {},
          release: async () => {},
          fail: async () => {},
        }),
      },
      validFrom: now > 120_000n ? now - 120_000n : 0n,
      validTo: now + 300_000n,
    });
  };
  return {
    harness,
    contracts,
    catalogue,
    setup,
    canonicalBlock,
    init,
    step01,
    authenticateAndFold,
    finalize,
    removeBlock,
  };
};
