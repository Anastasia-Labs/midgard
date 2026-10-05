import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";
import { CML, type UTxO, utxoToCore } from "@lucid-evolution/lucid";

import {
  assertWorkflowActuationPermitIdentity,
  type WorkflowActuationPermit,
} from "./actuation-permit.js";
import type { WorkflowAdapterRunner } from "./adapters.js";
import { parseWorkflowFundingCompletionHandoff } from "./funding-reservation-permit.parse-workflow-funding-abandonment-handoff.js";
import {
  assertSnapshotInputBounds,
  exactUtxos,
  parseSnapshot,
  parseStateSnapshot,
} from "./funding-reservation-permit.reconcile-workflow-funding-submission-handoff.js";
import {
  ACTION_KIND,
  admittedPermits,
  DIGEST,
  exact,
  journalPermits,
  OUT_REF,
  type PermitState,
  WORKFLOW_FUNDING_RESERVATION_PERMIT,
  type WorkflowFundingReservationPermit,
  type WorkflowFundingReservationPort,
} from "./funding-reservation-permit.workflow-funding-reservation-port.js";
import type { FraudProofWorkflowAction } from "./orchestrator.js";
import {
  assertWorkflowRuntimeFundingPolicyRunner,
  readWorkflowRuntimeFundingPolicy,
  type WorkflowRuntimeFundingPolicy,
} from "./runtime-funding-policy.js";

export const resolveExactOutRefs = async ({
  port,
  outRefs,
  label,
}: {
  readonly port: WorkflowFundingReservationPort;
  readonly outRefs: readonly string[];
  readonly label: string;
}): Promise<ReadonlyMap<string, UTxO>> => {
  const resolved = new Map<string, UTxO>();
  for (const utxo of await port.resolveInputs(outRefs)) {
    const outRef = `${utxo.txHash}#${utxo.outputIndex.toString()}`;
    if (!OUT_REF.test(outRef) || resolved.has(outRef)) {
      throw new Error(`${label} resolver returned malformed inputs`);
    }
    resolved.set(outRef, utxo);
  }
  if (
    resolved.size !== outRefs.length ||
    outRefs.some((outRef) => !resolved.has(outRef))
  ) {
    throw new Error(`${label} resolver changed the exact input set`);
  }
  return resolved;
};

export const exactResolvedOutputCbor = (utxo: UTxO): string =>
  utxoToCore(utxo).output().to_canonical_cbor_hex();

export const parseConfirmedActionOutput = (
  value: unknown,
): Readonly<{
  sourceActionKind: string;
  sourceOutputIndex: number;
  outRef: string;
  resolvedOutputCborHex: string;
}> => {
  const record = exact(
    value,
    [
      "sourceActionKind",
      "sourceOutputIndex",
      "outRef",
      "resolvedOutputCborHex",
    ],
    "confirmed production funding action output",
  );
  if (
    typeof record.sourceActionKind !== "string" ||
    !ACTION_KIND.test(record.sourceActionKind) ||
    !Number.isSafeInteger(record.sourceOutputIndex) ||
    (record.sourceOutputIndex as number) < 0 ||
    typeof record.outRef !== "string" ||
    !OUT_REF.test(record.outRef) ||
    typeof record.resolvedOutputCborHex !== "string" ||
    !/^(?:[0-9a-f]{2})+$/u.test(record.resolvedOutputCborHex)
  ) {
    throw new Error("confirmed production funding action output is invalid");
  }
  const output = CML.TransactionOutput.from_cbor_hex(
    record.resolvedOutputCborHex,
  );
  if (output.to_canonical_cbor_hex() !== record.resolvedOutputCborHex) {
    throw new Error(
      "confirmed production funding action output is not canonical",
    );
  }
  return Object.freeze({
    sourceActionKind: record.sourceActionKind,
    sourceOutputIndex: record.sourceOutputIndex as number,
    outRef: record.outRef,
    resolvedOutputCborHex: record.resolvedOutputCborHex,
  });
};

export const parseProtocolInputAuthority = (
  value: unknown,
): Readonly<{
  deploymentFingerprint: string;
  outRef: string;
  semanticRole: "protocol_state";
  resolvedOutputCborHex: string;
}> => {
  const record = exact(
    value,
    [
      "deploymentFingerprint",
      "outRef",
      "semanticRole",
      "resolvedOutputCborHex",
    ],
    "production protocol input authority",
  );
  if (
    typeof record.deploymentFingerprint !== "string" ||
    !DIGEST.test(record.deploymentFingerprint) ||
    typeof record.outRef !== "string" ||
    !OUT_REF.test(record.outRef) ||
    record.semanticRole !== "protocol_state" ||
    typeof record.resolvedOutputCborHex !== "string" ||
    !/^(?:[0-9a-f]{2})+$/u.test(record.resolvedOutputCborHex)
  ) {
    throw new Error("production protocol input authority is invalid");
  }
  const output = CML.TransactionOutput.from_cbor_hex(
    record.resolvedOutputCborHex,
  );
  if (output.to_canonical_cbor_hex() !== record.resolvedOutputCborHex) {
    throw new Error("production protocol input authority is not canonical");
  }
  return Object.freeze({
    deploymentFingerprint: record.deploymentFingerprint,
    outRef: record.outRef,
    semanticRole: "protocol_state",
    resolvedOutputCborHex: record.resolvedOutputCborHex,
  });
};

export const refresh = async (state: PermitState): Promise<void> => {
  const snapshot = parseStateSnapshot(state, await state.port.load());
  if (
    snapshot.reservationId !== state.snapshot.reservationId ||
    snapshot.deploymentFingerprint !== state.snapshot.deploymentFingerprint ||
    snapshot.decisionDigest !== state.snapshot.decisionDigest ||
    snapshot.policyDigest !== state.snapshot.policyDigest ||
    snapshot.reservationBasisDigest !== state.snapshot.reservationBasisDigest ||
    snapshot.rollbackGeneration !== state.snapshot.rollbackGeneration ||
    snapshot.walletAddress !== state.snapshot.walletAddress ||
    snapshot.fundingPaymentKeyHash !== state.snapshot.fundingPaymentKeyHash
  ) {
    throw new Error("production funding reservation identity changed");
  }
  const outRefs = snapshot.activeInputs.map(({ outRef }) => outRef);
  state.snapshot = snapshot;
  state.resolvedInputs = exactUtxos({
    snapshot,
    utxos: await state.port.resolveInputs(outRefs),
  });
};

export const actionKind = (action: FraudProofWorkflowAction): string => {
  const value =
    typeof action.input.actionKind === "string"
      ? action.input.actionKind
      : action.input.stage;
  if (typeof value !== "string" || !ACTION_KIND.test(value)) {
    throw new Error(
      "production workflow action omitted its stable action kind",
    );
  }
  return value;
};

export const stateForJournal = (journal: object): PermitState | undefined =>
  journalPermits.get(journal);

export const createWorkflowFundingReservationPermit = async ({
  category,
  runner,
  policy,
  reservationPolicy = policy,
  capacityPolicy = reservationPolicy,
  actuationPermit,
  rollbackGeneration,
  port,
}: {
  readonly category: FraudProofCatalogueCategoryName;
  readonly runner: WorkflowAdapterRunner;
  readonly policy: WorkflowRuntimeFundingPolicy;
  /** Existing leases retain their original identity through live parameter updates and additive roster corrections. */
  readonly reservationPolicy?: WorkflowRuntimeFundingPolicy;
  /** Authenticated historical capacity may admit recovery snapshots, never fresh spending. */
  readonly capacityPolicy?: WorkflowRuntimeFundingPolicy;
  readonly actuationPermit: WorkflowActuationPermit;
  readonly rollbackGeneration: string;
  readonly port: WorkflowFundingReservationPort;
}): Promise<WorkflowFundingReservationPermit> => {
  const actuation = assertWorkflowActuationPermitIdentity({
    permit: actuationPermit,
    category,
    rollbackGeneration,
  });
  assertWorkflowRuntimeFundingPolicyRunner({ policy, runner, category });
  const funding = readWorkflowRuntimeFundingPolicy(policy);
  assertWorkflowRuntimeFundingPolicyRunner({
    policy: reservationPolicy,
    runner,
    category,
  });
  const reservedFunding = readWorkflowRuntimeFundingPolicy(reservationPolicy);
  assertWorkflowRuntimeFundingPolicyRunner({
    policy: capacityPolicy,
    runner,
    category,
  });
  const capacityFunding = readWorkflowRuntimeFundingPolicy(capacityPolicy);
  // Only L1 parameters and their derived bounds may change. The admitted
  // original policy still pins every durable lease and signed journal intent.
  const stableIdentity = (value: typeof funding) =>
    computeDeploymentManifestJsonDigest({
      ...value,
      contracts: [],
      policyDigest: "",
      protocolParameters: null,
      protocolParametersDigest: "",
      maximumFeeLovelace: "",
      maximumCollateralLovelace: "",
      maximumSlashCollateralLovelace: "",
      maximumCollateralInputs: "",
    });
  if (
    [reservedFunding, capacityFunding].some(
      (prior) => stableIdentity(funding) !== stableIdentity(prior),
    ) ||
    [reservedFunding, capacityFunding].some((priorPolicy) =>
      priorPolicy.contracts.some(
        (prior) =>
          !funding.contracts.some(
            (current) =>
              current.address === prior.address &&
              current.scriptHash === prior.scriptHash &&
              current.role === prior.role,
          ),
      ),
    )
  )
    throw new Error(
      "funding reservation policy changed nonparameter authority or removed a contract",
    );
  const snapshot = parseSnapshot(await port.load());
  const maximumCollateralInputs = Number(funding.maximumCollateralInputs);
  if (
    snapshot.deploymentFingerprint !== actuation.deploymentFingerprint ||
    snapshot.deploymentFingerprint !== funding.deploymentFingerprint ||
    snapshot.decisionDigest !== actuation.executionDecisionDigest ||
    snapshot.rollbackGeneration !== rollbackGeneration ||
    snapshot.policyDigest !== reservedFunding.policyDigest ||
    snapshot.fundingPaymentKeyHash !== funding.fundingPaymentKeyHash ||
    (snapshot.state !== "active" && snapshot.state !== "released")
  )
    throw new Error(
      "production funding reservation does not match its runner authority",
    );
  if (snapshot.state === "released") {
    const handoff = parseWorkflowFundingCompletionHandoff(
      await port.readCompletionHandoff(),
    );
    if (
      handoff.identity.deploymentFingerprint !==
        snapshot.deploymentFingerprint ||
      handoff.identity.decisionDigest !== snapshot.decisionDigest ||
      handoff.identity.category !== category ||
      handoff.identity.target.kind !== "state_queue_header" ||
      handoff.identity.target.headerHash !== actuation.headerHash
    )
      throw new Error(
        "released funding reservation has no exact terminal recovery identity",
      );
  }
  const address = CML.Address.from_bech32(
    snapshot.walletAddress,
  ).to_raw_bytes();
  if (
    address.length !== 29 ||
    address[0]! >> 4 !== 6 ||
    Buffer.from(address.subarray(1)).toString("hex") !==
      funding.fundingPaymentKeyHash
  )
    throw new Error(
      "production funding reservation has a foreign wallet credential",
    );
  const reservationMaximumCollateralInputs = Math.max(
    Number(reservedFunding.maximumCollateralInputs),
    maximumCollateralInputs,
    Number(capacityFunding.maximumCollateralInputs),
  );
  assertSnapshotInputBounds({
    snapshot,
    maximumCollateralInputs: reservationMaximumCollateralInputs,
  });
  const permit: WorkflowFundingReservationPermit = Object.freeze({
    permitVersion: WORKFLOW_FUNDING_RESERVATION_PERMIT,
  });
  admittedPermits.set(permit, {
    category,
    policy,
    actuationPermit,
    port,
    maximumCollateralInputs,
    reservationMaximumCollateralInputs,
    requiresParameterRefresh:
      funding.protocolParametersDigest !==
      reservedFunding.protocolParametersDigest,
    snapshot,
    // A restarted workflow must first reconcile its durable pending intent.
    // Its inputs may already be consumed by that exact confirmed transaction.
    // Only begin/recheck, after journal reconciliation, demand live UTxOs.
    resolvedInputs: new Map(),
    boundJournal: undefined,
    currentActionKind: undefined,
    currentActionDigest: undefined,
    currentFundingOutRefs: Object.freeze([]),
    currentRequiredFundingOutRefs: Object.freeze([]),
    currentCollateralOutRefs: Object.freeze([]),
    pendingTransactionHash: undefined,
    idleReleaseAuthorized: false,
    preparedTransaction: undefined,
  });
  return permit;
};
