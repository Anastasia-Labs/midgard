import {
  type NetworkIdForcedScanState,
  type NetworkIdStep02State,
} from "@al-ft/midgard-sdk";
import {
  type LucidEvolution,
  type Network,
  type UTxO,
} from "@lucid-evolution/lucid";

import { type FaultProofFieldOpeningPlan } from "../field-opening.js";
import { type ResolvedProverSigner } from "../runtime.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import {
  type NetworkIdContracts,
  type NetworkIdStepContract,
} from "./contracts.js";
import {
  networkIdForcedScanExpectedStateHash,
  type NetworkIdForcedScanPlan,
  type NetworkIdForcedScanStep,
  networkIdForcedScanSuccessorStateHash,
} from "./forced-scan-plan.js";
import { networkIdSubmitError } from "./submit-common.js";
import { type PreparedNetworkIdWrongfulRejection } from "./wrongful-rejection.js";

export const STEP_LABEL = "network-id forced scan";

export type SubmitNetworkIdForcedScanParams = {
  readonly lucid: LucidEvolution;
  readonly contracts: NetworkIdContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly prepared: PreparedNetworkIdWrongfulRejection;
  /** The authenticated §2.5 field-2 opening every action re-supplies. */
  readonly outputsOpeningPlan: FaultProofFieldOpeningPlan;
  readonly scan: NetworkIdForcedScanPlan;
  /** Published `fraudProofNetworkIdForcedScan` reference script; mandatory. */
  readonly referenceScriptUtxo: UTxO;
  /** Published tier-2/3 carriage; resolved from the publisher when omitted. */
  readonly carriageUtxos?: readonly UTxO[];
  /** Existing §8.6 certificate UTxO, required only for tier 3. */
  readonly certificateUtxos?: readonly UTxO[];
  /** Required only when the certificate has to be resolved from chain. */
  readonly network?: Network;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
};

export type SubmitNetworkIdForcedScanResult = {
  readonly txHash: string;
  readonly nextThreadOutRef: string;
  readonly step: NetworkIdForcedScanStep;
  /** The successor scan state, absent on the batch that completes the walk. */
  readonly nextScanState: NetworkIdForcedScanState | null;
  /** Step 02's terminal state, present only on the completing batch. */
  readonly step02State: NetworkIdStep02State | null;
};

export const requireForcedScanContract = (
  contracts: NetworkIdContracts,
): NetworkIdStepContract => {
  const forcedScan = contracts.forcedScan;
  if (forcedScan === undefined) {
    throw networkIdSubmitError(
      "forced outputs scan is not deployed; the forced direction requires fraudProofNetworkIdForcedScan",
    );
  }
  return forcedScan;
};

const boundFor = (prepared: PreparedNetworkIdWrongfulRejection) => ({
  bad_tx_id: prepared.badTxId,
  committed_tx_network_id: prepared.evidence.committedNetworkId,
  expected_network_id: prepared.expectedNetworkId,
  forced_source_key: prepared.subject.source_key,
});

/**
 * Refuses, by name, every live-state disagreement the validator would abort
 * on: the wrong constructor for this action, a bound the forced door did not
 * write, or a committed checkpoint hash that is not the one this action
 * resumes.
 */
export const requireScanState = ({
  state,
  step,
  scan,
  prepared,
}: {
  readonly state: NetworkIdForcedScanState;
  readonly step: NetworkIdForcedScanStep;
  readonly scan: NetworkIdForcedScanPlan;
  readonly prepared: PreparedNetworkIdWrongfulRejection;
}): void => {
  const expectedBound = boundFor(prepared);
  const expectedConstructor =
    step.kind === "open" || step.kind === "startGrammar"
      ? "Ready"
      : step.kind === "advance"
        ? "Scanning"
        : "Grammar";
  const live =
    expectedConstructor === "Ready"
      ? "Ready" in state
        ? { bound: state.Ready.bound, checkpointHash: null }
        : undefined
      : expectedConstructor === "Scanning"
        ? "Scanning" in state
          ? {
              bound: state.Scanning.bound,
              checkpointHash: state.Scanning.checkpoint_hash,
            }
          : undefined
        : "Grammar" in state
          ? {
              bound: state.Grammar.bound,
              checkpointHash: state.Grammar.checkpoint_hash,
            }
          : undefined;
  if (live === undefined) {
    throw networkIdSubmitError(
      `forced scan ${step.kind} requires the ${expectedConstructor} state; the thread carries ${Object.keys(state)[0] ?? "an unknown state"}`,
    );
  }
  const bound = live.bound;
  if (
    bound.bad_tx_id !== expectedBound.bad_tx_id ||
    bound.committed_tx_network_id !== expectedBound.committed_tx_network_id ||
    bound.expected_network_id !== expectedBound.expected_network_id ||
    bound.forced_source_key !== expectedBound.forced_source_key
  ) {
    throw networkIdSubmitError(
      "forced scan thread carries a bound the forced door did not write for this authenticated leaf",
    );
  }
  const expectedHash = networkIdForcedScanExpectedStateHash(scan, step);
  if (expectedHash === undefined) return;
  if (live.checkpointHash !== expectedHash) {
    throw networkIdSubmitError(
      `forced scan ${step.kind} resumes checkpoint ${expectedHash}, but the thread committed ${live.checkpointHash ?? "no checkpoint"}`,
    );
  }
};

export const successorScanState = ({
  scan,
  step,
  prepared,
}: {
  readonly scan: NetworkIdForcedScanPlan;
  readonly step: NetworkIdForcedScanStep;
  readonly prepared: PreparedNetworkIdWrongfulRejection;
}): NetworkIdForcedScanState | null => {
  const checkpointHash = networkIdForcedScanSuccessorStateHash(scan, step);
  if (checkpointHash === null) return null;
  const bound = boundFor(prepared);
  return step.kind === "startGrammar" || step.kind === "resumeGrammar"
    ? ({ Grammar: { bound, checkpoint_hash: checkpointHash } } as never)
    : ({ Scanning: { bound, checkpoint_hash: checkpointHash } } as never);
};
