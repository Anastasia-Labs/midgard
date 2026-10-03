import * as SDK from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { submitCommittedFieldShapeInit } from "../src/committed-field-shape/submit-committed-field-shape-init.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import { buildForcedTransactionLeafMembershipProof } from "../src/transition-trace/witnesses.js";
import {
  applyUnusedRedeemerScripts,
  type UnusedRedeemerContracts,
} from "../src/unused-redeemer/contracts.js";
import { submitUnusedRedeemerStep01Forced } from "../src/unused-redeemer/submit-step-01.js";
import { submitUnusedRedeemerStep02 } from "../src/unused-redeemer/submit-step-02.js";
import { submitUnusedRedeemerStep02a } from "../src/unused-redeemer/submit-step-02a.js";
import { submitUnusedRedeemerStep02b } from "../src/unused-redeemer/submit-step-02b.js";
import { submitUnusedRedeemerStep02c } from "../src/unused-redeemer/submit-step-02c.js";
import { submitUnusedRedeemerStep03 } from "../src/unused-redeemer/submit-step-03.js";
import { submitUnusedRedeemerStep04 } from "../src/unused-redeemer/submit-step-04.js";
import { submitUnusedRedeemerStep05 } from "../src/unused-redeemer/submit-step-05.js";
import { submitUnusedRedeemerStep06 } from "../src/unused-redeemer/submit-step-06.js";
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
} from "./support/submit-init-emulator-shared.js";
import type {
  buildMaterial,
  buildRetainedDa,
} from "./unused-redeemer-lifecycle.build-material.js";
import { network } from "./unused-redeemer-lifecycle.maximum-redeemer-field.js";

type RetainedDa = Pick<
  Awaited<ReturnType<typeof buildRetainedDa>>,
  "eventKey" | "transaction" | "traceRoot"
>;
type Material = Awaited<ReturnType<typeof buildMaterial>>["material"];

const linear = [
  submitUnusedRedeemerStep02,
  submitUnusedRedeemerStep02a,
  submitUnusedRedeemerStep02b,
  submitUnusedRedeemerStep02c,
  submitUnusedRedeemerStep03,
  submitUnusedRedeemerStep04,
] as const;

const stepNames = [
  "fraudProofUnusedRedeemer",
  "fraudProofUnusedRedeemerStep02",
  "fraudProofUnusedRedeemerStep02a",
  "fraudProofUnusedRedeemerStep02b",
  "fraudProofUnusedRedeemerStep02c",
  "fraudProofUnusedRedeemerStep03",
  "fraudProofUnusedRedeemerStep04",
  "fraudProofUnusedRedeemerStep05",
  "fraudProofUnusedRedeemerStep06",
] as const;

/**
 * An emulator stage for an unusedRedeemer proof against a block that commits
 * the retained DA's forced transaction under `UnusedRedeemer { redeemer_index }`
 * of `material`.
 */
export const makeStage = async (da: RetainedDa, material: Material) => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: { alwaysFraudProofCatalogue: true },
  });
  const addressData = Data.from(
    Data.to(
      await Effect.runPromise(
        SDK.addressDataFromBech32(
          harness.contracts.fraudProof.spendingScriptAddress,
        ),
      ),
      SDK.AddressData,
    ),
  );
  const applied = applyUnusedRedeemerScripts({
    blueprint: harness.realBlueprint,
    network,
    computationThreadPolicyId: harness.contracts.computationThread.policyId,
    fraudProofPolicyId: harness.contracts.fraudProof.policyId,
    fraudProofTokenAddressData: addressData,
    hubOracleScriptHash: harness.contracts.hubOracle.policyId,
  });
  const contracts: UnusedRedeemerContracts = {
    steps: applied.map((step, index) => ({
      ...step,
      referenceOutRef: `${"00".repeat(32)}#${index.toString()}`,
    })) as unknown as UnusedRedeemerContracts["steps"],
    computationThread: harness.contracts.computationThread,
    fraudProof: harness.contracts.fraudProof,
    hubOraclePolicyId: harness.contracts.hubOracle.policyId,
    stateQueuePolicyId: harness.contracts.stateQueue.policyId,
  };
  const catalogue = await buildCatalogueDeploymentInfo({
    ...harness.contracts.fraudProofs,
    unusedRedeemer: {
      ...contracts.steps[0],
      spendingScriptCBOR: contracts.steps[0].spendingScript.script,
    },
  });
  const category = catalogue.categories.unusedRedeemer!;
  const redeemerIndex = BigInt(material.evidence.finding.redeemerIndex);
  const orderKey = da.eventKey.ForcedTransactionEventKey?.tx_order_id;
  if (orderKey === undefined) throw new Error("forced event key absent");
  const block = await buildDecodingBlockFixture({
    operatorVkey: await funderPaymentKeyHash(harness.funderLucid),
    startTime: BigInt(
      alignUnixTimeToEmulatorSlotBoundary(
        harness.funderLucid,
        harness.emulator.now() + 120_000,
      ) - 1,
    ),
    priorLedgerRoot: "00".repeat(32),
    subject: {
      kind: "forced",
      nativeTx: da.transaction.tx,
      orderKey,
      verdict: {
        ForcedTxInvalid: {
          reason: { UnusedRedeemer: { redeemer_index: redeemerIndex } },
        },
      },
    },
  });
  const header = {
    ...block.header,
    validationTracesRoot: da.traceRoot.root,
    validationTraceCount: da.traceRoot.count,
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
          label: `unused-redeemer-${index.toString()}`,
        })
      ).utxo,
    );
  const common = {
    lucid: harness.proverLucid,
    contracts,
    categoryId: category.categoryId,
    signer: harness.proverSigner,
  };

  /** Init and the forced bind of the committed coordinate. */
  const bind = async () => {
    const init = await submitCommittedFieldShapeInit({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      network,
      contracts: contracts as never,
      category,
      catalogue: {
        policyId: harness.contracts.fraudProofCatalogue.policyId,
        spendingScriptAddress:
          harness.contracts.fraudProofCatalogue.spendingScriptAddress,
        root: catalogue.root,
      },
      signer: harness.proverSigner,
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    return (
      await submitUnusedRedeemerStep01Forced({
        ...common,
        threadOutRef: init.nextThreadOutRef,
        header,
        membership: await buildForcedTransactionLeafMembershipProof({
          reconstruction: block.reconstruction,
          eventKey: da.eventKey,
        }),
        redeemerIndex,
        referenceScriptUtxo: references[0]!,
      })
    ).nextThreadOutRef;
  };

  /** Steps 02 to 04: the linear step at `offset` (0 is step 02). */
  const linearStep = async (offset: number, threadOutRef: string) =>
    (
      await linear[offset]!({
        ...common,
        threadOutRef,
        authentication: material.authentication,
        evidence: material.evidence,
        referenceScriptUtxo: references[offset + 1]!,
      } as never)
    ).nextThreadOutRef;

  /** The reverse scan, the terminal rule and the proof mint. */
  const finalize = async (threadOutRef: string) => {
    let outRef = threadOutRef;
    for (let complete = false; !complete; ) {
      const scan = await submitUnusedRedeemerStep05({
        ...common,
        threadOutRef: outRef,
        evidence: material.evidence,
        referenceScriptUtxo: references[7]!,
      });
      complete = scan.complete;
      outRef = scan.nextThreadOutRef;
    }
    return await submitUnusedRedeemerStep06({
      ...common,
      threadOutRef: outRef,
      evidence: material.evidence,
      referenceScriptUtxo: references[8]!,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
  };

  /** Removal of the committing block under the minted proof. */
  const remove = async () => {
    const removalReferences = await publishRemovalReferenceScripts({
      lucid: harness.proverLucid,
      contracts: harness.contracts,
    });
    const base = buildRemovalDeploymentInfo(harness.contracts, catalogue, {
      removalReferenceScripts: removalReferences.published,
    });
    const now = BigInt(harness.emulator.now());
    return await submitRemoveFraudulentBlock({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      deploymentInfo: {
        ...base,
        contracts: {
          ...base.contracts,
          ...Object.fromEntries(
            contracts.steps.map((step, index) => [
              stepNames[index]!,
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
      fraudCategory: "unusedRedeemer",
      fraudulentHeaderHash: setup.headerHash,
      awaitConfirmation: true,
      requireReferenceScripts: true,
      stateQueueMutationLeaseCoordinator: {
        acquire: async () => ({
          token: "unused-redeemer-coordinate-lease",
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

  return { bind, linearStep, finalize, remove, stepCount: linear.length };
};
