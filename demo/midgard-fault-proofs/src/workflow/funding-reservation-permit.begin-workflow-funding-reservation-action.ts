import { createHash } from "node:crypto";

import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import { type TxSigned } from "@lucid-evolution/lucid";

import { workflowActuationPermitIsReconciliationOnly } from "./actuation-permit.js";
import {
  actionKind,
  refresh,
  stateForJournal,
} from "./funding-reservation-permit.create-workflow-funding-reservation-permit.js";
import { parseStateSnapshot } from "./funding-reservation-permit.reconcile-workflow-funding-submission-handoff.js";
import {
  admittedPermits,
  journalPermits,
  type PermitState,
  WORKFLOW_FUNDING_RESERVATION_PERMIT,
  type WorkflowFundingReservationPermit,
  WorkflowFundingReservationUnavailableError,
} from "./funding-reservation-permit.workflow-funding-reservation-port.js";
import type { FraudProofWorkflowAction } from "./orchestrator.js";

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

export const assertFundingSubmissionAuthority = (state: PermitState): void => {
  if (workflowActuationPermitIsReconciliationOnly(state.actuationPermit))
    throw new Error(
      "reconciliation-only funding authority cannot spend or sign",
    );
};

export const assertCurrentFundingCollateralLimit = (
  state: PermitState,
): void => {
  // Recovery reads and confirmation use the original admitted bounds. A new
  // action must use the current limit even if its old inputs still exist.
  if (
    state.snapshot.activeInputs.filter(({ role }) => role === "collateral")
      .length > state.maximumCollateralInputs
  )
    throw new WorkflowFundingReservationUnavailableError();
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
  assertFundingSubmissionAuthority(state);
  await state.port.assertSubmissionAuthority?.();
  if ((await state.port.readAbandonmentHandoff()) !== null)
    throw new Error(
      "funding abandonment outcome awaits journal acknowledgment",
    );
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
      state.requiresParameterRefresh ||
      // Confirmation releases a transaction's collateral lease, so the next
      // action selects collateral again (the collateral floor is positive).
      !state.snapshot.activeInputs.some(({ role }) => role === "collateral") ||
      state.snapshot.activeInputs.filter(({ role }) => role === "collateral")
        .length > state.maximumCollateralInputs) &&
    state.port.refreshIdle !== undefined
  ) {
    const refreshed = await state.port.refreshIdle({
      expectedRevision: state.snapshot.revision,
      releaseStaleInputs: state.idleReleaseAuthorized,
    });
    if (refreshed === null)
      throw new WorkflowFundingReservationUnavailableError();
    state.snapshot = parseStateSnapshot(state, refreshed);
    await refresh(state);
    state.requiresParameterRefresh = false;
  } else if (staleInputs) {
    throw new WorkflowFundingReservationUnavailableError();
  }
  assertFundingSubmissionAuthority(state);
  await state.port.assertSubmissionAuthority?.();
  if (state.snapshot.state !== "active")
    throw new Error("production funding reservation is not active");
  assertCurrentFundingCollateralLimit(state);
  state.currentActionKind = actionKind(action);
  state.currentActionDigest = computeDeploymentManifestJsonDigest(action);
  const funding = state.snapshot.activeInputs
    .filter(({ role }) => role === "funding")
    .map(({ outRef }) => outRef);
  // Owner ruling (whichever lands wins): a replacement must be mutually
  // exclusive with each superseded attempt. A shared protocol input already
  // makes it so; one shared funding input per attempt also does, and the rest
  // of the reserved pool still tops up fees and outputs. An attempt with no
  // input left in the pool needs none: it cannot land without a rollback, and
  // whatever lands first wins. Nothing waits for retirement past k. Each set
  // spans the attempt's lineage, so the input most sets share is drawn first:
  // a common ancestor's input excludes its descendants too.
  const required: string[] = [];
  let open = (
    (await state.port.readSupersededAttemptFundingOutRefs?.()) ?? []
  ).filter((attempt) => attempt.some((outRef) => funding.includes(outRef)));
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
  state.currentCollateralOutRefs = Object.freeze(
    state.snapshot.activeInputs
      .filter(({ role }) => role === "collateral")
      .map(({ outRef }) => outRef),
  );
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
