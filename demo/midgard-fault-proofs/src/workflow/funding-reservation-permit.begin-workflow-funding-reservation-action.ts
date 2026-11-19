import { createHash } from "node:crypto";

import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import { type TxSigned } from "@lucid-evolution/lucid";

import { proofMaterialsAddressOfFundingAddress } from "../funding/proof-materials-address.js";
import { workflowActuationPermitIsReconciliationOnly } from "./actuation-permit.js";
import {
  actionKind,
  refresh,
  stateForJournal,
} from "./funding-reservation-permit.create-workflow-funding-reservation-permit.js";
import { workflowFundingExclusiveProtocolInput } from "./funding-reservation-permit.excluded-by-protocol-input.js";
import { parseSnapshot } from "./funding-reservation-permit.parse-workflow-funding-snapshot.js";
import { isValidationDisputeStagingReclaim } from "./funding-reservation-permit.validation-dispute-staging-authority.js";
import {
  admittedPermits,
  journalPermits,
  type PermitState,
  WORKFLOW_FUNDING_RESERVATION_PERMIT,
  type WorkflowFundingReservationPermit,
  WorkflowFundingReservationUnavailableError,
} from "./funding-reservation-permit.workflow-funding-reservation-port.js";
import {
  WorkflowFundingReservationShortfallError,
  WorkflowFundingShortfallError,
} from "./funding-reservation-permit.workflow-funding-shortfall.js";
import type { FraudProofWorkflowAction } from "./orchestrator.js";
import { isProofMaterialsSweepAction } from "./proof-materials-sweep.action.js";

export const bindWorkflowFundingReservationJournal = <Journal extends object>({
  journal,
  permit,
}: {
  readonly journal: Journal;
  readonly permit: WorkflowFundingReservationPermit;
}): Journal => {
  const state = admittedPermits.get(permit);
  if (
    permit.permitVersion !== WORKFLOW_FUNDING_RESERVATION_PERMIT ||
    state === undefined
  ) {
    throw new Error("production funding reservation permit was not admitted");
  }
  if (journalPermits.has(journal)) {
    throw new Error(
      "workflow journal already has funding reservation authority",
    );
  }
  if (state.boundJournal !== undefined) {
    throw new Error(
      "production funding reservation permit is already bound to a workflow journal",
    );
  }
  state.boundJournal = journal;
  journalPermits.set(journal, state);
  return journal;
};

/** Reservations under reconciliation-only authority whose current action is
 * the spend back of a staging output of ours, the one action they sign. */
const reconciliationReclaims = new WeakSet<PermitState>();

/** Whether the reservation's current action is a spend back it signs under
 * reconciliation-only authority. */
export const isReconciliationReclaim = (state: PermitState): boolean =>
  reconciliationReclaims.has(state);

/** Reservations whose current action is the sweep of their workflow's
 * proof materials, which they sign under any authority. */
const materialsSweeps = new WeakSet<PermitState>();

/** Whether the reservation bound to `journal` signs `action` under
 * reconciliation-only authority: only the spend back of a staging output
 * of ours or the sweep of our proof materials, which its runtime bound then
 * admits only as such. */
export const workflowJournalAdmitsReconciliationReclaim = (
  journal: object,
  action: FraudProofWorkflowAction,
): boolean => {
  const state = stateForJournal(journal);
  return (
    state?.policy !== undefined &&
    (isValidationDisputeStagingReclaim(state.category, action) ||
      isProofMaterialsSweepAction(action))
  );
};

export const assertFundingSubmissionAuthority = (state: PermitState): void => {
  if (
    workflowActuationPermitIsReconciliationOnly(state.actuationPermit) &&
    !reconciliationReclaims.has(state) &&
    !materialsSweeps.has(state)
  )
    throw new Error(
      "reconciliation-only funding authority cannot spend or sign",
    );
};

/** Begins the spend back `action` under reconciliation-only authority: it
 * spends its staging output alone, so it reserves no wallet coin, for its
 * body or its collateral. The staging output sits at our proof-materials
 * address, which the reservation tracks: reserved there, it leaves the
 * reservation with the spend back. */
const beginReconciliationReclaim = async (
  state: PermitState,
  action: FraudProofWorkflowAction,
): Promise<void> => {
  await refresh(state);
  if (state.snapshot.state !== "active")
    throw new Error("production funding reservation is not active");
  state.currentActionKind = actionKind(action);
  state.currentActionDigest = computeDeploymentManifestJsonDigest(action);
  const materials = proofMaterialsAddressOfFundingAddress(
    state.snapshot.walletAddress,
  );
  state.currentFundingOutRefs = Object.freeze(
    state.snapshot.activeInputs
      .map(({ outRef }) => outRef)
      .filter(
        (outRef) => state.resolvedInputs.get(outRef)?.address === materials,
      ),
  );
  state.currentRequiredFundingOutRefs = Object.freeze([]);
  reconciliationReclaims.add(state);
};

/** Begins the sweep `action` of the workflow's proof materials. It needs no
 * funding beyond its materials, so no reserved coin is required of it and
 * no idle refresh can hold it back; its runtime bound admits a reserved
 * funding coin only where the materials alone fall under min-ADA, and none
 * under reconciliation-only authority. */
const beginProofMaterialsSweep = async (
  state: PermitState,
  action: FraudProofWorkflowAction,
): Promise<void> => {
  let current = true;
  try {
    await refresh(state);
  } catch (error) {
    if (!(error instanceof WorkflowFundingReservationUnavailableError))
      throw error;
    current = false;
  }
  if (state.snapshot.state !== "active")
    throw new Error("production funding reservation is not active");
  state.currentActionKind = actionKind(action);
  state.currentActionDigest = computeDeploymentManifestJsonDigest(action);
  // Reserved materials the sweep spends leave the reservation with it; with
  // a stale reservation it spends its materials by their lineage alone.
  state.currentFundingOutRefs = Object.freeze(
    current ? state.snapshot.activeInputs.map(({ outRef }) => outRef) : [],
  );
  state.currentRequiredFundingOutRefs = Object.freeze([]);
  materialsSweeps.add(state);
};

export const beginWorkflowFundingReservationAction = async ({
  journal,
  action,
}: {
  readonly journal: object;
  readonly action: FraudProofWorkflowAction;
}): Promise<void> => {
  const state = stateForJournal(journal);
  if (state === undefined) return;
  if (state.policy === undefined)
    throw new Error("test-only funding permit cannot build transactions");
  reconciliationReclaims.delete(state);
  materialsSweeps.delete(state);
  if (isProofMaterialsSweepAction(action)) {
    await beginProofMaterialsSweep(state, action);
    return;
  }
  if (
    workflowActuationPermitIsReconciliationOnly(state.actuationPermit) &&
    isValidationDisputeStagingReclaim(state.category, action)
  ) {
    await beginReconciliationReclaim(state, action);
    return;
  }
  assertFundingSubmissionAuthority(state);
  let staleInputs = false;
  try {
    await refresh(state);
  } catch (error) {
    if (!(error instanceof WorkflowFundingReservationUnavailableError))
      throw error;
    staleInputs = true;
  }
  if (
    (staleInputs ||
      state.snapshot.activeInputs.length === 0 ||
      state.requiresParameterRefresh) &&
    state.port.refreshIdle !== undefined
  ) {
    let refreshed: unknown;
    try {
      refreshed = await state.port.refreshIdle({
        expectedRevision: state.snapshot.revision,
        releaseStaleInputs: state.idleReleaseAuthorized,
      });
    } catch (error) {
      // The wallet cannot fund this action: its shortfall, named for it.
      if (error instanceof WorkflowFundingReservationShortfallError)
        throw new WorkflowFundingShortfallError(action.actionId, error);
      throw error;
    }
    if (refreshed === null)
      throw new WorkflowFundingReservationUnavailableError();
    state.snapshot = parseSnapshot(refreshed);
    await refresh(state);
    state.requiresParameterRefresh = false;
  } else if (staleInputs) {
    throw new WorkflowFundingReservationUnavailableError();
  }
  assertFundingSubmissionAuthority(state);
  if (state.snapshot.state !== "active")
    throw new Error("production funding reservation is not active");
  state.currentActionKind = actionKind(action);
  state.currentActionDigest = computeDeploymentManifestJsonDigest(action);
  // One pool: any reserved coin may fund the action, post its collateral, or
  // both at once.
  const funding = state.snapshot.activeInputs.map(({ outRef }) => outRef);
  // Owner ruling (whichever lands wins): a replacement must be mutually
  // exclusive with each superseded attempt. A shared protocol input already
  // makes it so; one shared funding input per attempt also does, and the rest
  // of the reserved pool still tops up fees and outputs. An attempt with no
  // input left in the pool needs none: it cannot land without a rollback, and
  // whatever lands first wins. Nothing waits for retirement past k. Each set
  // spans the attempt's lineage, so the input most sets share is drawn first:
  // a common ancestor's input excludes its descendants too.
  // An action whose attempts all spend one protocol input is the exception
  // (a validation-trace thread move): none is forced on it.
  const required: string[] = [];
  let open =
    workflowFundingExclusiveProtocolInput({
      category: state.category,
      actionInput: action.input,
    }) !== null
      ? []
      : (
          (await state.port.readSupersededAttemptFundingOutRefs?.()) ?? []
        ).filter((attempt) =>
          attempt.some((outRef) => funding.includes(outRef)),
        );
  while (open.length !== 0) {
    const count = (outRef: string) =>
      open.filter((attempt) => attempt.includes(outRef)).length;
    const shared = [...funding]
      .sort()
      .reduce((best, outRef) => (count(outRef) > count(best) ? outRef : best));
    required.push(shared);
    open = open.filter((attempt) => !attempt.includes(shared));
  }
  // The real builder selects from durable leased candidates; admission below
  // derives the exact consumed subset from its signed transaction.
  state.currentFundingOutRefs = Object.freeze(funding);
  state.currentRequiredFundingOutRefs = Object.freeze(required.sort());
};

export const bodySha256 = (signed: TxSigned): string =>
  createHash("sha256")
    .update(Buffer.from(signed.toTransaction().body().to_cbor_hex(), "hex"))
    .digest("hex");

export const addAssets = (
  totals: Map<string, bigint>,
  assets: Readonly<Record<string, bigint>>,
): void => {
  for (const [unit, quantity] of Object.entries(assets)) {
    totals.set(unit, (totals.get(unit) ?? 0n) + quantity);
  }
};

export const isProtocolFundingContract = (contract: {
  readonly role: string;
}): boolean =>
  contract.role === "protocol_state" || contract.role === "correction_lock";
