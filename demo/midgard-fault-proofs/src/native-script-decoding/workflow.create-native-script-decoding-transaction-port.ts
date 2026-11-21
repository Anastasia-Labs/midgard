import * as SDK from "@al-ft/midgard-sdk/core";
import * as SDKFraudProof from "@al-ft/midgard-sdk/fraud-proof";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import { resolveFaultProofFieldPreimageCertificate } from "../field-opening.certify-fault-proof-field-carriage.js";
import { planFaultProofFieldOpening } from "../field-opening.plan-fault-proof-field-opening.js";
import { resolveFaultProofFieldCarriagePublications } from "../field-opening.publish-fault-proof-field-carriage.js";
import { resolvePublishedProofChunks } from "../publish-proof-chunks.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import { parseOutRef, type ResolvedProverSigner } from "../runtime.js";
import {
  nativeTxFromCoreCompact,
  parseSubmitStep01TxInclusion,
} from "../step-support.js";
import { buildInit } from "../submit-init.submit-init.js";
import { keyValuePhasProof } from "../transition-trace/phas.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import { type CompleteCanonicalReplayContext } from "../workflow/complete-replay.js";
import {
  CURSOR_FAMILY_TRANSACTION_PORT,
  type CursorFamilyTransactionPort,
} from "../workflow/cursor-family-adapter.admit-reference-scripts.js";
import {
  captureCursorRemoval,
  cursorFamilyActionInput,
  cursorStringField,
} from "../workflow/cursor-family-runtime.js";
import { type FraudProofWorkflowDeploymentBinding } from "../workflow/deployment-manifest-binding.js";
import { workflowFieldCarriageOwnOutputs } from "../workflow/field-carriage-prerequisite.own-carriage-claims.js";
import type { FraudProofL1Source } from "../workflow/l1-source.js";
import { proofMaterialsAccess } from "../workflow/proof-materials-view.js";
import { workflowStepTransaction } from "../workflow/step-transaction.js";
import {
  admitNativeScriptDecodingWorkflowArtifact,
  prepareNativeScriptDecodingWorkflowArtifact,
} from "./artifact.js";
import type { NativeScriptDecodingContracts } from "./contracts.js";
import { buildNativeScriptDecodingStep01BindNormal } from "./submit-native-script-decoding-step-01.submit-native-script-decoding-step01-bind-normal.js";
import { buildNativeScriptDecodingStep01RecordForced } from "./submit-native-script-decoding-step-01.submit-native-script-decoding-step01-record-forced.js";
import { buildNativeScriptDecodingStep02 } from "./submit-native-script-decoding-step-02.js";
import {
  buildNativeScriptDecodingStep03AdvanceOrCloseClose,
  buildNativeScriptDecodingStep03AdvanceOrCloseSegment,
} from "./submit-native-script-decoding-step-03.submit-native-script-decoding-step03-advance-or-close-close.js";
import { buildNativeScriptDecodingStep03BindDescriptor } from "./submit-native-script-decoding-step-03.submit-native-script-decoding-step03-bind-descriptor.js";
import { buildNativeScriptDecodingStep03OpenSubject } from "./submit-native-script-decoding-step-03.submit-native-script-decoding-step03-open-subject.js";
import { buildNativeScriptDecodingStep04 } from "./submit-native-script-decoding-step-04.js";

export type NativeScriptDecodingWorkflowReferenceScripts = Readonly<{
  steps: readonly [UTxO, UTxO, UTxO, UTxO, UTxO, UTxO];
  witnesses: Required<FaultProofWitnessReferenceScripts>;
  fieldPreimageCertificateMint: UTxO;
}>;

type BoundConfig = Readonly<{
  replayContext?: CompleteCanonicalReplayContext;
  binding: FraudProofWorkflowDeploymentBinding<"nativeScriptDecoding">;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  contracts: NativeScriptDecodingContracts;
  references: NativeScriptDecodingWorkflowReferenceScripts;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

type Prepared = Awaited<
  ReturnType<typeof admitNativeScriptDecodingWorkflowArtifact>
>;

export const sourceInclusion = async (a: Prepared) => {
  if (a.coordinate.sourceKind !== 0 || !("validity" in a.material.compact))
    throw new Error("normal inclusion requires a normal source");
  const entry = a.current.transactions[a.coordinate.sourceIndex]!;
  return parseSubmitStep01TxInclusion({
    nativeTxId: a.txId,
    nativeTx: nativeTxFromCoreCompact(a.material.compact),
    nativeTxCompactCbor: a.material.proofSource.compactCbor.toString("hex"),
    l2TransactionSourceCbor: entry.valueBytes.toString("hex"),
    transactionsPhasRoot: a.current.rootData.transactions.phasRoot,
    txMembershipProofCbor: Data.to(
      await keyValuePhasProof(
        {
          ...a.current.rootData.transactions,
          root: a.current.rootData.transactions.phasRoot,
        },
        entry.keyBytes,
        entry.valueBytes,
      ),
      SDK.Proof,
    ),
  });
};

export const subjectPlan = (a: Prepared, owner: string) =>
  planFaultProofFieldOpening({
    anchorSourceKind: a.coordinate.sourceKind === 1 ? 1n : 0n,
    fieldIndex: Number(a.coordinate.outpointSourceKind),
    anchorTxId: a.txId,
    nativeTxCompactCbor: a.material.proofSource.compactCbor.toString("hex"),
    itemCbors: a.subjectFieldInputs.map(
      SDKFraudProof.encodeMidgardTxInputCanonical,
    ),
    owner,
    publish: true,
    label: "native-script-decoding subject",
  });

export const namesField = (a: Prepared) =>
  (a.coordinate.outpointSourceKind === "0" ||
    a.coordinate.outpointSourceKind === "1") &&
  BigInt(a.coordinate.outpointCursor) >= 0n;

export const createNativeScriptDecodingTransactionPort = (
  config: BoundConfig,
): CursorFamilyTransactionPort<"nativeScriptDecoding"> => ({
  portVersion: CURSOR_FAMILY_TRANSACTION_PORT,
  category: "nativeScriptDecoding",
  prepare: async (args) =>
    await prepareNativeScriptDecodingWorkflowArtifact({
      ...args,
      replayContext: config.replayContext,
    }),
  capture: async ({ action, artifact, holder }) => {
    const ownOutputs = workflowFieldCarriageOwnOutputs({
      category: "nativeScriptDecoding",
      materials: proofMaterialsAccess({
        signer: config.signer,
        lucid: config.lucid,
        holder,
      }),
    });
    const a = await admitNativeScriptDecodingWorkflowArtifact(artifact);
    if (a.current.headerHash !== config.binding.definition.headerHash)
      throw new Error("nativeScriptDecoding bound header changed");
    const input = cursorFamilyActionInput({
      category: "nativeScriptDecoding",
      action,
    });
    if (input.stage === "remove")
      return await captureCursorRemoval({
        category: "nativeScriptDecoding",
        lucid: config.lucid,
        blueprint: config.binding.blueprint,
        deploymentInfo: config.binding.deploymentInfo,
        network: config.binding.network,
        signer: config.signer,
        headerHash: a.current.headerHash,
        input,
        stateQueueMutationLeaseCoordinator:
          config.stateQueueMutationLeaseCoordinator,
        fraudProverRewardLovelace: BigInt(
          config.binding.releaseEconomics.policy.fraudProverRewardLovelace,
        ),
      });
    return {
      transaction: (
        await workflowStepTransaction(
          await (async () => {
            if (input.stage === "init") {
              return await buildInit({
                lucid: config.lucid,
                blueprint: config.binding.blueprint,
                deploymentInfo: config.binding.deploymentInfo,
                network: config.binding.network,
                signer: config.signer,
                fraudCategory: "nativeScriptDecoding",
                fraudulentBlockOutRef: cursorStringField(
                  input,
                  "stateQueueBlockOutRef",
                ),
                fraudulentHeaderHash: a.current.headerHash,
                witnessReferenceScripts: config.references.witnesses,
              });
            }
            const threadOutRef = cursorStringField(input, "threadOutRef");
            const common = {
              lucid: config.lucid,
              contracts: config.contracts,
              categoryId: config.binding.resolvedContracts.category.categoryId,
              signer: config.signer,
              threadOutRef,
            };
            switch (input.stage) {
              case "step_01":
                if (a.coordinate.sourceKind === 1)
                  return await buildNativeScriptDecodingStep01RecordForced({
                    ...common,
                    referenceScriptUtxo: config.references.steps[0],
                  });
                else {
                  const inclusion = await sourceInclusion(a);
                  return await buildNativeScriptDecodingStep01BindNormal({
                    ...common,
                    blueprint: config.binding.blueprint,
                    network: config.binding.network,
                    stateQueueBlockOutRef: cursorStringField(
                      input,
                      "stateQueueBlockOutRef",
                    ),
                    txInclusion: inclusion,
                    publishedProofChunks: await resolvePublishedProofChunks({
                      lucid: config.lucid,
                      address: config.signer.proofMaterialsAddress,
                      proofCbor: inclusion.txMembershipProofCbor,
                      view: config.signer.proofMaterials,
                      holder,
                    }),
                    referenceScriptUtxo: config.references.steps[0],
                    witnessReferenceScripts: config.references.witnesses,
                  });
                }
              case "step_02":
                return await buildNativeScriptDecodingStep02({
                  ...common,
                  reconstruction: a.current,
                  forcedOrderKey:
                    "ForcedTransactionEventKey" in a.eventKey
                      ? a.eventKey.ForcedTransactionEventKey.tx_order_id
                      : undefined,
                  chosenOutpoint:
                    a.direction === 0n
                      ? {
                          sourceKind: BigInt(a.coordinate.outpointSourceKind),
                          cursor: BigInt(a.coordinate.outpointCursor),
                        }
                      : undefined,
                  referenceScriptUtxo: config.references.steps[1],
                });
              case "step_03": {
                const planned = namesField(a)
                  ? subjectPlan(a, config.signer.paymentKeyHash)
                  : null;
                const published =
                  planned === null
                    ? []
                    : await resolveFaultProofFieldCarriagePublications({
                        ownOutputs,
                        lucid: config.lucid,
                        planned,
                        publisherAddress: config.signer.proofMaterialsAddress,
                      });
                const certificate =
                  planned === null
                    ? undefined
                    : await resolveFaultProofFieldPreimageCertificate({
                        ownOutputs,
                        lucid: config.lucid,
                        planned,
                        certificatePolicyId:
                          config.contracts.fieldPreimageCertificatePolicyId,
                        network: config.binding.network,
                      });
                return await buildNativeScriptDecodingStep03OpenSubject({
                  ownOutputs,
                  ...common,
                  nativeTxCompactCbor:
                    a.material.proofSource.compactCbor.toString("hex"),
                  subjectFieldInputs: a.subjectFieldInputs,
                  publishCarriage: true,
                  publishedCarriageUtxos: published,
                  certificateUtxo: certificate ?? undefined,
                  referenceScriptUtxo: config.references.steps[2],
                });
              }
              case "step_04": {
                if (a.descriptorCbor === null || a.ledgerTrie === null)
                  throw new Error(
                    "nativeScriptDecoding missing descriptor evidence",
                  );
                const outpoint =
                  a.subjectFieldInputs[Number(a.coordinate.outpointCursor)]!;
                return await buildNativeScriptDecodingStep03BindDescriptor({
                  ...common,
                  outpointKeyCbor: Buffer.from(
                    SDKFraudProof.encodeMidgardTxInputCanonical(outpoint),
                  ).toString("hex"),
                  descriptorCbor: a.descriptorCbor.toString("hex"),
                  ledgerTrie: a.ledgerTrie,
                  plan: a.plan ?? undefined,
                  referenceScriptItemBytes:
                    a.referenceScriptItemBytes ?? undefined,
                  referenceScriptUtxo: config.references.steps[3],
                });
              }
              case "step_05": {
                if (a.plan === null || a.referenceScriptItemBytes === null)
                  throw new Error("nativeScriptDecoding missing scan evidence");
                const utxos = await config.lucid.utxosByOutRef([
                  parseOutRef(threadOutRef, "nativeScriptDecoding thread"),
                ]);
                if (utxos.length !== 1 || !utxos[0]!.datum)
                  throw new Error("nativeScriptDecoding scan thread absent");
                const datum = Data.from(
                  utxos[0]!.datum,
                  SDKFraudProof.NativeScriptDecodingStep03AdvanceOrCloseDatum,
                );
                if (datum.data === null)
                  throw new Error("nativeScriptDecoding absent scan datum");
                const hash = datum.data.machine_state_hash;
                const segment = a.plan.segments.find(
                  (item) => item.controlBefore.hashHex === hash,
                );
                if (segment !== undefined)
                  return await buildNativeScriptDecodingStep03AdvanceOrCloseSegment(
                    {
                      ...common,
                      segment,
                      referenceScriptItemBytes: a.referenceScriptItemBytes,
                      referenceScriptUtxo: config.references.steps[4],
                    },
                  );
                else {
                  if (a.plan.verdict.control?.hashHex !== hash)
                    throw new Error(
                      "nativeScriptDecoding scan checkpoint differs from deterministic plan",
                    );
                  return await buildNativeScriptDecodingStep03AdvanceOrCloseClose(
                    {
                      ...common,
                      verdict: a.plan.verdict,
                      referenceScriptItemBytes: a.referenceScriptItemBytes,
                      referenceScriptUtxo: config.references.steps[4],
                    },
                  );
                }
              }
              case "step_06":
                return await buildNativeScriptDecodingStep04({
                  ...common,
                  referenceScriptUtxo: config.references.steps[5],
                  witnessReferenceScripts: config.references.witnesses,
                });
              default:
                throw new Error(
                  `nativeScriptDecoding unsupported stage ${input.stage}`,
                );
            }
          })(),
          `native-script-decoding ${input.stage}`,
        )
      ).transaction,
    };
  },
});

export type ManifestBoundNativeScriptDecodingWorkflowConfig = Readonly<{
  replayContext?: CompleteCanonicalReplayContext;
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  referenceScripts: NativeScriptDecodingWorkflowReferenceScripts;
  l1Source: FraudProofL1Source;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;
