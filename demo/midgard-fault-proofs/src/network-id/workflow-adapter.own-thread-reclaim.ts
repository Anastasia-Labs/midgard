import {
  FraudProofComputationThreadStepDatum,
  NetworkIdForcedScanDatum,
  NetworkIdForcedStepDatum,
  NetworkIdStep02Datum,
} from "@al-ft/midgard-sdk";
import type { UTxO } from "@lucid-evolution/lucid";

import type { FraudProofFamilyWorkflowAdapter } from "../workflow/orchestrator.js";
import { createLinearCancelReclaimPort } from "../workflow/own-thread-reclaim.linear-cancel-port.js";
import { withOwnThreadReclaim } from "../workflow/own-thread-reclaim.with-own-thread-reclaim.js";
import type { FraudProofThreadPosition } from "../workflow/raw-l1-family-derivation.own-thread-positions.js";
import type { VerifiedFraudProofReleaseFinalityPolicy } from "../workflow/release-finality-policy.js";
import {
  NETWORK_ID_CATEGORY_LABEL,
  type NetworkIdContracts,
} from "./contracts.js";
import {
  type NetworkIdPhysicalStep,
  networkIdPhysicalSteps,
  submitNetworkIdCancel,
} from "./submit-network-id-cancel.js";
import type { NetworkIdWorkflowAdapterConfig } from "./workflow-adapter.create-network-id-raw-l1-observation-port.js";

const DATUM_AT: Readonly<
  Record<NetworkIdPhysicalStep, FraudProofThreadPosition["datumSchema"]>
> = {
  step01: FraudProofComputationThreadStepDatum,
  forcedStep: NetworkIdForcedStepDatum,
  forcedScan: NetworkIdForcedScanDatum,
  step02: NetworkIdStep02Datum,
};

/** Every place a network-id thread can stand, in physical chain order: step
 * 01, the forced door and the forced outputs scan where deployed, step 02. */
export const networkIdOwnThreadPositions = (
  contracts: Pick<NetworkIdContracts, "steps" | "forcedStep" | "forcedScan">,
): readonly FraudProofThreadPosition[] =>
  networkIdPhysicalSteps(contracts).map(([name, step]) => ({
    address: step.spendingScriptAddress,
    datumSchema: DATUM_AT[name],
  }));

/**
 * The reclaim of our own threads in front of the network-id workflow: on a
 * header another record removed, a thread of ours at any physical position
 * comes back to our wallet through the family's own cancel. `contracts` is
 * the deployment's whole chain; a port that cannot list our threads leaves
 * the adapter as it is.
 */
export const withNetworkIdOwnThreadReclaim = ({
  base,
  config,
  contracts,
  releaseFinality,
}: {
  readonly base: FraudProofFamilyWorkflowAdapter;
  readonly config: NetworkIdWorkflowAdapterConfig;
  readonly contracts: NetworkIdContracts;
  readonly releaseFinality: Pick<
    VerifiedFraudProofReleaseFinalityPolicy,
    "policy"
  >;
}): FraudProofFamilyWorkflowAdapter => {
  const l1 = config.rawL1;
  const observeOwnThreads = l1?.observeOwnThreads;
  const observeSignedTransaction = l1?.observeSignedTransaction;
  if (observeOwnThreads === undefined || observeSignedTransaction === undefined)
    return base;
  const referenceAt: Readonly<Record<NetworkIdPhysicalStep, UTxO | undefined>> =
    {
      step01: config.stepReferenceScripts[0],
      forcedStep: config.forcedStepReferenceScript,
      forcedScan: config.forcedScanReferenceScript,
      step02: config.stepReferenceScripts[1],
    };
  const physical = networkIdPhysicalSteps(contracts);
  return withOwnThreadReclaim({
    category: "networkId",
    base,
    releaseFinality,
    reclaim: createLinearCancelReclaimPort({
      family: NETWORK_ID_CATEGORY_LABEL,
      lucid: config.lucid,
      signer: config.signer,
      categoryId: config.category.categoryId,
      contracts: () => ({
        steps: physical.map(([, step]) => step),
        computationThread: contracts.computationThread,
      }),
      stepReferences: physical.map(([name]) => referenceAt[name]),
      witnessReferenceScripts: config.witnessReferenceScripts,
      cancel: async ({
        threadOutRef,
        referenceScriptUtxo,
        preSubmitBoundary,
      }) =>
        await submitNetworkIdCancel({
          lucid: config.lucid,
          contracts,
          categoryId: config.category.categoryId,
          signer: config.signer,
          threadOutRef,
          referenceScriptUtxo,
          witnessReferenceScripts: config.witnessReferenceScripts,
          preSubmitBoundary,
          awaitConfirmation: false,
        }),
      l1: {
        observeOwnThreads: async (input) => await observeOwnThreads(input),
        observeSignedTransaction: async (input) =>
          await observeSignedTransaction(input),
        ...(l1?.rebroadcastSignedTransaction === undefined
          ? {}
          : {
              rebroadcastSignedTransaction: async (input) =>
                await l1.rebroadcastSignedTransaction!(input),
            }),
      },
    }),
  });
};
