import { createHash } from "node:crypto";

import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";
import {
  assetsToValue,
  CML,
  coreToTxOutput,
  coreToUtxo,
  type LucidEvolution,
  type TxSigned,
  type UTxO,
  utxoToCore,
  validatorToScriptHash,
  type WalletApi,
} from "@lucid-evolution/lucid";

import type { ResolvedProverSigner } from "../runtime.js";
import {
  assertWorkflowActuationPermitIdentity,
  type WorkflowActuationPermit,
} from "./actuation-permit.js";
import type { WorkflowAdapterRunner } from "./adapters.js";
import {
  type WorkflowFundingRequirements,
  workflowFundingRequirementsForRunner,
} from "./funding-requirements.js";
import type { FraudProofWorkflowAction } from "./orchestrator.js";
import {
  workflowPreflightTransaction,
  workflowTransactionCollateralInputOutRefs,
  workflowTransactionInputOutRefs,
  workflowTransactionReferenceInputOutRefs,
} from "./transaction-boundary.js";

export const WORKFLOW_FUNDING_RESERVATION_PERMIT =
  "midgard-production-workflow-funding-reservation-permit-v1" as const;

const DIGEST = /^[0-9a-f]{64}$/u;
const OUT_REF = /^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u;
const NATURAL = /^(?:0|[1-9][0-9]*)$/u;
const ACTION_KIND = /^[a-z][a-zA-Z0-9_.:-]{0,127}$/u;

export type WorkflowFundingReservedInput = Readonly<{
  outRef: string;
  role: "funding" | "collateral";
  lovelace: string;
  assets: readonly Readonly<{ unit: string; quantity: string }>[];
}>;

export type WorkflowFundingReservationSnapshot = Readonly<{
  reservationId: string;
  deploymentFingerprint: string;
  decisionDigest: string;
  profileDigest: string;
  calculationDigest: string;
  rollbackGeneration: string;
  revision: string;
  walletAddress: string;
  fundingPaymentKeyHash: string;
  state: "active" | "released" | "conflict";
  activeInputs: readonly WorkflowFundingReservedInput[];
}>;

export type WorkflowFundingPreparedTransition = Readonly<{
  actionKind: string;
  signedTransactionCborHex: string;
  transactionHash: string;
  transactionBodySha256: string;
  consumedOutRefs: readonly string[];
  producedInputs: readonly WorkflowFundingReservedInput[];
}>;

/**
 * Durable watcher-owned authority. The production application supplies this
 * port from its authenticated SQLite reservation store and local-node UTxO
 * source; workflow constructors never accept reservation data from config.
 */
export interface WorkflowFundingReservationPort {
  load(): Promise<unknown>;
  resolveInputs(outRefs: readonly string[]): Promise<readonly UTxO[]>;
  resolveConfirmedActionOutput(input: {
    readonly sourceActionKind: string;
    readonly sourceOutputIndex: number;
  }): Promise<unknown>;
  resolveProtocolInputAuthority(input: {
    readonly deploymentFingerprint: string;
    readonly outRef: string;
    readonly semanticRole: "protocol_state";
  }): Promise<unknown>;
  prepare(input: {
    readonly expectedRevision: string;
    readonly transition: WorkflowFundingPreparedTransition;
  }): Promise<unknown>;
  confirm(input: {
    readonly expectedRevision: string;
    readonly transactionHash: string;
  }): Promise<unknown>;
  abandon(input: {
    readonly expectedRevision: string;
    readonly transactionHash: string;
  }): Promise<unknown>;
  markConflict(input: {
    readonly expectedRevision: string;
    readonly code: "unexpected_spend" | "reservation_collision";
  }): Promise<unknown>;
  release(input: { readonly expectedRevision: string }): Promise<unknown>;
}

export interface WorkflowFundingReservationPermit {
  readonly permitVersion: typeof WORKFLOW_FUNDING_RESERVATION_PERMIT;
}

type PermitState = {
  readonly category: FraudProofCatalogueCategoryName;
  readonly requirements: WorkflowFundingRequirements;
  readonly actuationPermit: WorkflowActuationPermit;
  readonly port: WorkflowFundingReservationPort;
  readonly maximumFundingInputs: number;
  readonly maximumCollateralInputs: number;
  snapshot: WorkflowFundingReservationSnapshot;
  resolvedInputs: ReadonlyMap<string, UTxO>;
  boundJournal: object | undefined;
  currentActionKind: string | undefined;
  currentFundingOutRefs: readonly string[];
  currentCollateralOutRefs: readonly string[];
  pendingTransactionHash: string | undefined;
};

const admittedPermits = new WeakMap<object, PermitState>();
const journalPermits = new WeakMap<object, PermitState>();

const isPlainObject = (value: unknown): value is Record<string, unknown> =>
  typeof value === "object" &&
  value !== null &&
  !Array.isArray(value) &&
  Object.getPrototypeOf(value) === Object.prototype &&
  Reflect.ownKeys(value).length === Object.keys(value).length;

const exact = (
  value: unknown,
  keys: readonly string[],
  label: string,
): Readonly<Record<string, unknown>> => {
  if (!isPlainObject(value)) throw new Error(`${label} is not a plain object`);
  const actual = Object.keys(value).sort();
  const expected = [...keys].sort();
  if (
    actual.length !== expected.length ||
    actual.some((key, index) => key !== expected[index])
  ) {
    throw new Error(`${label} has unknown or missing fields`);
  }
  return value;
};

const canonicalOutRefs = (
  values: readonly string[],
  label: string,
): readonly string[] => {
  if (
    values.some((value) => !OUT_REF.test(value)) ||
    values.some(
      (value, index) =>
        index > 0 && values[index - 1]!.localeCompare(value) >= 0,
    )
  ) {
    throw new Error(`${label} must be canonical, ordered, and unique`);
  }
  return Object.freeze([...values]);
};

const reservedInput = (
  value: unknown,
  label: string,
): WorkflowFundingReservedInput => {
  const record = exact(value, ["outRef", "role", "lovelace", "assets"], label);
  if (
    typeof record.outRef !== "string" ||
    !OUT_REF.test(record.outRef) ||
    (record.role !== "funding" && record.role !== "collateral") ||
    typeof record.lovelace !== "string" ||
    !NATURAL.test(record.lovelace) ||
    BigInt(record.lovelace) <= 0n ||
    !Array.isArray(record.assets)
  ) {
    throw new Error(`${label} is invalid`);
  }
  const assets = record.assets.map((entry, index) => {
    const asset = exact(
      entry,
      ["unit", "quantity"],
      `${label}.assets[${index.toString()}]`,
    );
    if (
      typeof asset.unit !== "string" ||
      !/^[0-9a-f]{56}(?:[0-9a-f]{2}){0,32}$/u.test(asset.unit) ||
      typeof asset.quantity !== "string" ||
      !NATURAL.test(asset.quantity) ||
      BigInt(asset.quantity) <= 0n
    ) {
      throw new Error(`${label}.assets[${index.toString()}] is invalid`);
    }
    return Object.freeze({ unit: asset.unit, quantity: asset.quantity });
  });
  if (
    assets.some(
      (asset, index) =>
        index > 0 && assets[index - 1]!.unit.localeCompare(asset.unit) >= 0,
    ) ||
    (record.role === "collateral" && assets.length !== 0)
  ) {
    throw new Error(`${label} asset set is invalid`);
  }
  return Object.freeze({
    outRef: record.outRef,
    role: record.role,
    lovelace: record.lovelace,
    assets: Object.freeze(assets),
  });
};

const parseSnapshot = (value: unknown): WorkflowFundingReservationSnapshot => {
  const record = exact(
    value,
    [
      "reservationId",
      "deploymentFingerprint",
      "decisionDigest",
      "profileDigest",
      "calculationDigest",
      "rollbackGeneration",
      "revision",
      "walletAddress",
      "fundingPaymentKeyHash",
      "state",
      "activeInputs",
    ],
    "production funding reservation snapshot",
  );
  if (
    typeof record.reservationId !== "string" ||
    !DIGEST.test(record.reservationId) ||
    typeof record.deploymentFingerprint !== "string" ||
    !DIGEST.test(record.deploymentFingerprint) ||
    typeof record.decisionDigest !== "string" ||
    !DIGEST.test(record.decisionDigest) ||
    typeof record.profileDigest !== "string" ||
    !DIGEST.test(record.profileDigest) ||
    typeof record.calculationDigest !== "string" ||
    !DIGEST.test(record.calculationDigest) ||
    typeof record.rollbackGeneration !== "string" ||
    !NATURAL.test(record.rollbackGeneration) ||
    typeof record.revision !== "string" ||
    !NATURAL.test(record.revision) ||
    typeof record.walletAddress !== "string" ||
    record.walletAddress.length === 0 ||
    typeof record.fundingPaymentKeyHash !== "string" ||
    !/^[0-9a-f]{56}$/u.test(record.fundingPaymentKeyHash) ||
    (record.state !== "active" &&
      record.state !== "released" &&
      record.state !== "conflict") ||
    !Array.isArray(record.activeInputs)
  ) {
    throw new Error("production funding reservation snapshot is invalid");
  }
  const activeInputs = record.activeInputs.map((entry, index) =>
    reservedInput(
      entry,
      `production funding reservation snapshot.activeInputs[${index.toString()}]`,
    ),
  );
  canonicalOutRefs(
    activeInputs.map(({ outRef }) => outRef),
    "production funding reservation active inputs",
  );
  if (record.state === "active" && activeInputs.length === 0) {
    throw new Error("active production funding reservation has no inputs");
  }
  if (record.state !== "active" && activeInputs.length !== 0) {
    throw new Error("inactive production funding reservation retains inputs");
  }
  return Object.freeze({
    reservationId: record.reservationId,
    deploymentFingerprint: record.deploymentFingerprint,
    decisionDigest: record.decisionDigest,
    profileDigest: record.profileDigest,
    calculationDigest: record.calculationDigest,
    rollbackGeneration: record.rollbackGeneration,
    revision: record.revision,
    walletAddress: record.walletAddress,
    fundingPaymentKeyHash: record.fundingPaymentKeyHash,
    state: record.state,
    activeInputs: Object.freeze(activeInputs),
  });
};

const assertSnapshotInputBounds = ({
  snapshot,
  maximumFundingInputs,
  maximumCollateralInputs,
}: {
  readonly snapshot: WorkflowFundingReservationSnapshot;
  readonly maximumFundingInputs: number;
  readonly maximumCollateralInputs: number;
}): void => {
  if (
    snapshot.activeInputs.filter(({ role }) => role === "funding").length >
      maximumFundingInputs ||
    snapshot.activeInputs.filter(({ role }) => role === "collateral").length >
      maximumCollateralInputs
  ) {
    throw new Error(
      "production funding reservation exceeds its measured input bounds",
    );
  }
};

const parseStateSnapshot = (
  state: PermitState,
  value: unknown,
): WorkflowFundingReservationSnapshot => {
  const snapshot = parseSnapshot(value);
  assertSnapshotInputBounds({
    snapshot,
    maximumFundingInputs: state.maximumFundingInputs,
    maximumCollateralInputs: state.maximumCollateralInputs,
  });
  return snapshot;
};

const exactUtxos = ({
  snapshot,
  utxos,
}: {
  readonly snapshot: WorkflowFundingReservationSnapshot;
  readonly utxos: readonly UTxO[];
}): ReadonlyMap<string, UTxO> => {
  const resolved = new Map<string, UTxO>();
  for (const utxo of utxos) {
    const outRef = `${utxo.txHash}#${utxo.outputIndex.toString()}`;
    if (!OUT_REF.test(outRef) || resolved.has(outRef)) {
      throw new Error("production funding resolver returned malformed inputs");
    }
    resolved.set(outRef, utxo);
  }
  const expected = snapshot.activeInputs.map(({ outRef }) => outRef);
  const actual = [...resolved.keys()].sort();
  if (
    actual.length !== expected.length ||
    actual.some((outRef, index) => outRef !== expected[index])
  ) {
    throw new Error(
      "production funding resolver changed the reserved input set",
    );
  }
  for (const reserved of snapshot.activeInputs) {
    const utxo = resolved.get(reserved.outRef)!;
    if (utxo.address !== snapshot.walletAddress) {
      throw new Error("production funding resolver returned a foreign address");
    }
    const lovelace = utxo.assets.lovelace;
    if (lovelace?.toString() !== reserved.lovelace) {
      throw new Error("production funding resolver changed reserved lovelace");
    }
    const actualAssets = Object.entries(utxo.assets)
      .filter(([unit]) => unit !== "lovelace")
      .sort(([left], [right]) => left.localeCompare(right));
    if (
      actualAssets.length !== reserved.assets.length ||
      actualAssets.some(
        ([unit, quantity], index) =>
          unit !== reserved.assets[index]!.unit ||
          quantity.toString() !== reserved.assets[index]!.quantity,
      )
    ) {
      throw new Error("production funding resolver changed reserved assets");
    }
  }
  return resolved;
};

const resolveExactOutRefs = async ({
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

const exactResolvedOutputCbor = (utxo: UTxO): string =>
  utxoToCore(utxo).output().to_canonical_cbor_hex();

const exactIdentityAssets = (
  utxo: UTxO,
): readonly Readonly<{ unit: string; quantity: string }>[] =>
  Object.freeze(
    Object.entries(utxo.assets)
      .filter(([unit]) => unit !== "lovelace")
      .sort(([left], [right]) => left.localeCompare(right))
      .map(([unit, quantity]) =>
        Object.freeze({ unit, quantity: quantity.toString() }),
      ),
  );

const parseConfirmedActionOutput = (
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

const parseProtocolInputAuthority = (
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

const refresh = async (state: PermitState): Promise<void> => {
  const snapshot = parseStateSnapshot(state, await state.port.load());
  if (
    snapshot.reservationId !== state.snapshot.reservationId ||
    snapshot.deploymentFingerprint !== state.snapshot.deploymentFingerprint ||
    snapshot.decisionDigest !== state.snapshot.decisionDigest ||
    snapshot.profileDigest !== state.snapshot.profileDigest ||
    snapshot.calculationDigest !== state.snapshot.calculationDigest ||
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

const actionKind = (action: FraudProofWorkflowAction): string => {
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

const stateForJournal = (journal: object): PermitState | undefined =>
  journalPermits.get(journal);

const selectActionInputs = ({
  candidates,
  maximumCount,
  requiredLovelace,
  requiredAssets,
  label,
}: {
  readonly candidates: readonly WorkflowFundingReservedInput[];
  readonly maximumCount: number;
  readonly requiredLovelace: bigint;
  readonly requiredAssets: ReadonlyMap<string, bigint>;
  readonly label: string;
}): readonly string[] => {
  if (requiredLovelace === 0n && requiredAssets.size === 0) {
    return Object.freeze([]);
  }
  if (!Number.isSafeInteger(maximumCount) || maximumCount < 1) {
    throw new Error(`${label} measured input bound is invalid`);
  }
  const ordered = [...candidates].sort((left, right) =>
    left.outRef.localeCompare(right.outRef),
  );
  const covers = (
    selected: readonly WorkflowFundingReservedInput[],
  ): boolean => {
    if (
      selected.reduce(
        (total, candidate) => total + BigInt(candidate.lovelace),
        0n,
      ) < requiredLovelace
    ) {
      return false;
    }
    const assets = new Map<string, bigint>();
    for (const candidate of selected) {
      for (const { unit, quantity } of candidate.assets) {
        assets.set(unit, (assets.get(unit) ?? 0n) + BigInt(quantity));
      }
    }
    return [...requiredAssets].every(
      ([unit, quantity]) => (assets.get(unit) ?? 0n) >= quantity,
    );
  };
  const search = (
    size: number,
    start: number,
    selected: WorkflowFundingReservedInput[],
  ): readonly string[] | null => {
    if (selected.length === size) {
      return covers(selected)
        ? Object.freeze(selected.map(({ outRef }) => outRef).sort())
        : null;
    }
    for (
      let index = start;
      index <= ordered.length - (size - selected.length);
      index += 1
    ) {
      selected.push(ordered[index]!);
      const result = search(size, index + 1, selected);
      selected.pop();
      if (result !== null) return result;
    }
    return null;
  };
  for (
    let size = 1;
    size <= Math.min(maximumCount, ordered.length);
    size += 1
  ) {
    const selected = search(size, 0, []);
    if (selected !== null) return selected;
  }
  throw new Error(
    `${label} cannot be satisfied within its measured input bound`,
  );
};

export const createWorkflowFundingReservationPermit = async ({
  category,
  runner,
  actuationPermit,
  rollbackGeneration,
  port,
}: {
  readonly category: FraudProofCatalogueCategoryName;
  readonly runner: WorkflowAdapterRunner;
  readonly actuationPermit: WorkflowActuationPermit;
  readonly rollbackGeneration: string;
  readonly port: WorkflowFundingReservationPort;
}): Promise<WorkflowFundingReservationPermit> => {
  const actuation = assertWorkflowActuationPermitIdentity({
    permit: actuationPermit,
    category,
    rollbackGeneration,
  });
  const requirements = workflowFundingRequirementsForRunner({
    category,
    runner,
  });
  const snapshot = parseSnapshot(await port.load());
  const maximumFundingInputs = requirements.actions.reduce(
    (maximum, action) =>
      Math.max(
        maximum,
        action.fundingControlledInputs.filter(
          ({ role }) => role === "wallet_funding",
        ).length,
      ),
    0,
  );
  const maximumCollateralInputs = requirements.actions.reduce(
    (maximum, action) =>
      Math.max(
        maximum,
        CML.Transaction.from_cbor_hex(action.signedTransactionCborHex)
          .body()
          .collateral_inputs()
          ?.len() ?? 0,
      ),
    0,
  );
  if (
    snapshot.deploymentFingerprint !== actuation.deploymentFingerprint ||
    snapshot.decisionDigest !== actuation.decisionDigest ||
    snapshot.rollbackGeneration !== rollbackGeneration ||
    snapshot.profileDigest !== requirements.profileDigest ||
    snapshot.fundingPaymentKeyHash !== requirements.fundingPaymentKeyHash ||
    snapshot.state !== "active"
  ) {
    throw new Error(
      "production funding reservation does not match its runner authority",
    );
  }
  assertSnapshotInputBounds({
    snapshot,
    maximumFundingInputs,
    maximumCollateralInputs,
  });
  const permit: WorkflowFundingReservationPermit = Object.freeze({
    permitVersion: WORKFLOW_FUNDING_RESERVATION_PERMIT,
  });
  admittedPermits.set(permit, {
    category,
    requirements,
    actuationPermit,
    port,
    maximumFundingInputs,
    maximumCollateralInputs,
    snapshot,
    resolvedInputs: exactUtxos({
      snapshot,
      utxos: await port.resolveInputs(
        snapshot.activeInputs.map(({ outRef }) => outRef),
      ),
    }),
    boundJournal: undefined,
    currentActionKind: undefined,
    currentFundingOutRefs: Object.freeze([]),
    currentCollateralOutRefs: Object.freeze([]),
    pendingTransactionHash: undefined,
  });
  return permit;
};

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

export const beginWorkflowFundingReservationAction = async ({
  journal,
  action,
}: {
  readonly journal: object;
  readonly action: FraudProofWorkflowAction;
}): Promise<void> => {
  const state = stateForJournal(journal);
  if (state === undefined) return;
  await refresh(state);
  if (state.snapshot.state !== "active") {
    throw new Error("production funding reservation is not active");
  }
  const kind = actionKind(action);
  const measured = state.requirements.actions.find(
    (entry) => entry.actionKind === kind,
  );
  if (measured === undefined) {
    throw new Error(`production funding profile omitted action ${kind}`);
  }
  const requiredAssets = new Map<string, bigint>();
  let requiredLovelace = 0n;
  for (const controlled of measured.fundingControlledInputs) {
    if (controlled.role !== "wallet_funding") continue;
    requiredLovelace += BigInt(controlled.fundingLovelace);
    for (const { unit, quantity } of controlled.fundingAssets) {
      requiredAssets.set(
        unit,
        (requiredAssets.get(unit) ?? 0n) + BigInt(quantity),
      );
    }
  }
  const measuredTransaction = CML.Transaction.from_cbor_hex(
    measured.signedTransactionCborHex,
  );
  const measuredCollateralCount =
    measuredTransaction.body().collateral_inputs()?.len() ?? 0;
  const measuredCollateral =
    measuredTransaction.body().total_collateral() ?? 0n;
  state.currentFundingOutRefs = selectActionInputs({
    candidates: state.snapshot.activeInputs.filter(
      ({ role }) => role === "funding",
    ),
    maximumCount: measured.fundingControlledInputs.filter(
      ({ role }) => role === "wallet_funding",
    ).length,
    requiredLovelace,
    requiredAssets,
    label: `${kind} reserved funding`,
  });
  state.currentCollateralOutRefs = selectActionInputs({
    candidates: state.snapshot.activeInputs.filter(
      ({ role }) => role === "collateral",
    ),
    maximumCount: measuredCollateralCount,
    requiredLovelace: measuredCollateral,
    requiredAssets: new Map(),
    label: `${kind} reserved collateral`,
  });
  state.currentActionKind = kind;
};

const bodySha256 = (signed: TxSigned): string =>
  createHash("sha256")
    .update(
      Buffer.from(signed.toTransaction().body().to_canonical_cbor_hex(), "hex"),
    )
    .digest("hex");

const addAssets = (
  totals: Map<string, bigint>,
  assets: Readonly<Record<string, bigint>>,
): void => {
  for (const [unit, quantity] of Object.entries(assets)) {
    totals.set(unit, (totals.get(unit) ?? 0n) + quantity);
  }
};

const assertMeasuredTransactionBound = async ({
  state,
  measured,
  signed,
  bodyInputs,
  fundingOutRefs,
}: {
  readonly state: PermitState;
  readonly measured: WorkflowFundingRequirements["actions"][number];
  readonly signed: TxSigned;
  readonly bodyInputs: readonly string[];
  readonly fundingOutRefs: readonly string[];
}): Promise<void> => {
  const transaction = signed.toTransaction();
  const body = transaction.body();
  const measuredTransaction = CML.Transaction.from_cbor_hex(
    measured.signedTransactionCborHex,
  );
  if (
    transaction.to_canonical_cbor_hex().length / 2 >
      measured.signedTransactionBytes ||
    body.to_canonical_cbor_hex().length / 2 > measured.txBodyBytes ||
    body.inputs().len() > measured.inputOutRefs.length ||
    body.fee() > measuredTransaction.body().fee()
  ) {
    throw new Error(
      `${measured.actionKind} transaction exceeds its admitted measured shape`,
    );
  }
  let memory = 0n;
  let steps = 0n;
  const redeemers = transaction.witness_set().redeemers()?.to_flat_format();
  for (let index = 0; index < (redeemers?.len() ?? 0); index += 1) {
    const units = redeemers!.get(index).ex_units();
    memory += units.mem();
    steps += units.steps();
  }
  if (
    memory > BigInt(measured.executionUnits.memory) ||
    steps > BigInt(measured.executionUnits.steps)
  ) {
    throw new Error(
      `${measured.actionKind} execution units exceed the admitted measurement`,
    );
  }
  const actualCollateral = body.collateral_inputs()?.len() ?? 0;
  const measuredCollateral =
    measuredTransaction.body().collateral_inputs()?.len() ?? 0;
  if (
    (measured.collateralRequired && actualCollateral < 1) ||
    (!measured.collateralRequired && actualCollateral !== 0) ||
    actualCollateral > measuredCollateral ||
    (body.total_collateral() ?? 0n) >
      (measuredTransaction.body().total_collateral() ?? 0n)
  ) {
    throw new Error(
      `${measured.actionKind} collateral exceeds its admitted measured shape`,
    );
  }
  const referenceOutRefs = [
    ...workflowTransactionReferenceInputOutRefs(signed),
  ].sort();
  const measuredReferenceOutRefs = measured.referenceInputs
    .map(({ outRef }) => outRef)
    .sort();
  if (
    referenceOutRefs.length !== measuredReferenceOutRefs.length ||
    referenceOutRefs.some(
      (outRef, referenceIndex) =>
        outRef !== measuredReferenceOutRefs[referenceIndex],
    )
  ) {
    throw new Error(
      `${measured.actionKind} reference-input roles differ from measurement`,
    );
  }
  const references = await resolveExactOutRefs({
    port: state.port,
    outRefs: referenceOutRefs,
    label: `${measured.actionKind} reference inputs`,
  });
  for (const expected of measured.referenceInputs) {
    const scriptRef = references.get(expected.outRef)!.scriptRef;
    const script = scriptRef?.script;
    if (expected.scriptHash === null && expected.scriptBytes === null) {
      if (scriptRef !== undefined && scriptRef !== null) {
        throw new Error(
          `${measured.actionKind} non-script reference input gained a script`,
        );
      }
      continue;
    }
    if (
      expected.scriptHash === null ||
      expected.scriptBytes === null ||
      scriptRef === undefined ||
      scriptRef === null ||
      script === undefined ||
      !/^(?:[0-9a-f]{2})+$/u.test(script)
    ) {
      throw new Error(
        `${measured.actionKind} reference input omitted exact script bytes`,
      );
    }
    if (
      script.length / 2 !== expected.scriptBytes ||
      validatorToScriptHash(scriptRef) !== expected.scriptHash
    ) {
      throw new Error(
        `${measured.actionKind} reference-script identity differs from measurement`,
      );
    }
  }
  const allInputs = await resolveExactOutRefs({
    port: state.port,
    outRefs: bodyInputs,
    label: `${measured.actionKind} ordinary inputs`,
  });
  for (const [outRef, utxo] of allInputs) {
    if (
      utxo.address === state.snapshot.walletAddress &&
      !fundingOutRefs.includes(outRef)
    ) {
      throw new Error(
        `${measured.actionKind} consumed an unreserved funding-wallet input`,
      );
    }
  }
  const unmatchedNonWalletOutRefs = new Set(
    bodyInputs.filter((outRef) => !fundingOutRefs.includes(outRef)),
  );
  const releasedFundingAssets = new Map<string, bigint>();
  for (const controlled of measured.fundingControlledInputs) {
    if (controlled.role === "wallet_funding") continue;
    if (controlled.role === "released_locked") {
      const confirmed = parseConfirmedActionOutput(
        await state.port.resolveConfirmedActionOutput({
          sourceActionKind: controlled.sourceActionKind!,
          sourceOutputIndex: controlled.sourceOutputIndex!,
        }),
      );
      const sourceAction = state.requirements.actions.find(
        ({ actionKind }) => actionKind === controlled.sourceActionKind,
      );
      const sourceOutput = sourceAction?.fundingControlledOutputs.find(
        ({ outputIndex }) => outputIndex === controlled.sourceOutputIndex,
      );
      const actual = allInputs.get(confirmed.outRef);
      if (
        confirmed.sourceActionKind !== controlled.sourceActionKind ||
        confirmed.sourceOutputIndex !== controlled.sourceOutputIndex ||
        sourceOutput?.role !== "locked_reusable" ||
        sourceOutput.semanticRole !== controlled.semanticRole ||
        sourceOutput.contractAddress !== controlled.contractAddress ||
        actual === undefined ||
        !unmatchedNonWalletOutRefs.delete(confirmed.outRef) ||
        exactResolvedOutputCbor(actual) !== confirmed.resolvedOutputCborHex ||
        actual.address !== controlled.contractAddress ||
        JSON.stringify(exactIdentityAssets(actual)) !==
          JSON.stringify(controlled.identityAssets)
      ) {
        throw new Error(
          `${measured.actionKind} released-lock input lacks exact confirmed lineage`,
        );
      }
      releasedFundingAssets.set(
        "lovelace",
        (releasedFundingAssets.get("lovelace") ?? 0n) +
          BigInt(controlled.fundingLovelace),
      );
      for (const { unit, quantity } of controlled.fundingAssets) {
        releasedFundingAssets.set(
          unit,
          (releasedFundingAssets.get(unit) ?? 0n) + BigInt(quantity),
        );
      }
      continue;
    }
    const candidates = [...unmatchedNonWalletOutRefs]
      .map((outRef) => ({ outRef, utxo: allInputs.get(outRef)! }))
      .filter(
        ({ utxo }) =>
          utxo.address === controlled.contractAddress &&
          JSON.stringify(exactIdentityAssets(utxo)) ===
            JSON.stringify(controlled.identityAssets),
      )
      .sort((left, right) => left.outRef.localeCompare(right.outRef));
    if (candidates.length !== 1) {
      throw new Error(
        `${measured.actionKind} protocol input semantic role is ambiguous`,
      );
    }
    const candidate = candidates[0]!;
    const authority = parseProtocolInputAuthority(
      await state.port.resolveProtocolInputAuthority({
        deploymentFingerprint: state.snapshot.deploymentFingerprint,
        outRef: candidate.outRef,
        semanticRole: "protocol_state",
      }),
    );
    if (
      authority.deploymentFingerprint !==
        state.snapshot.deploymentFingerprint ||
      authority.outRef !== candidate.outRef ||
      authority.semanticRole !== controlled.semanticRole ||
      authority.resolvedOutputCborHex !==
        exactResolvedOutputCbor(candidate.utxo)
    ) {
      throw new Error(
        `${measured.actionKind} protocol input lacks deployment-bound authority`,
      );
    }
    unmatchedNonWalletOutRefs.delete(candidate.outRef);
  }
  if (unmatchedNonWalletOutRefs.size !== 0) {
    throw new Error(
      `${measured.actionKind} ordinary input topology differs from measurement`,
    );
  }
  const outputs = body.outputs();
  if (outputs.len() !== measured.fundingControlledOutputs.length) {
    throw new Error(
      `${measured.actionKind} output topology differs from the admitted measurement`,
    );
  }
  const fundingAssets = new Map<string, bigint>();
  for (const outRef of fundingOutRefs) {
    addAssets(fundingAssets, allInputs.get(outRef)!.assets);
  }
  for (const [unit, quantity] of releasedFundingAssets) {
    fundingAssets.set(unit, (fundingAssets.get(unit) ?? 0n) + quantity);
  }
  const outputFundingAssets = new Map<string, bigint>();
  outputFundingAssets.set("lovelace", body.fee());
  for (const controlled of measured.fundingControlledOutputs) {
    const output = coreToTxOutput(outputs.get(controlled.outputIndex));
    if (output.address !== controlled.contractAddress) {
      throw new Error(
        `${measured.actionKind} output semantic address differs from measurement`,
      );
    }
    if (controlled.role === "protocol") continue;
    if (controlled.role === "wallet_change") {
      addAssets(outputFundingAssets, output.assets);
      continue;
    }
    if (
      (output.assets.lovelace ?? 0n) < BigInt(controlled.fundingLovelace) ||
      controlled.fundingAssets.some(
        ({ unit, quantity }) => (output.assets[unit] ?? 0n) < BigInt(quantity),
      )
    ) {
      throw new Error(
        `${measured.actionKind} locked custody is below its admitted funding role`,
      );
    }
    outputFundingAssets.set(
      "lovelace",
      (outputFundingAssets.get("lovelace") ?? 0n) +
        BigInt(controlled.fundingLovelace),
    );
    for (const { unit, quantity } of controlled.fundingAssets) {
      outputFundingAssets.set(
        unit,
        (outputFundingAssets.get(unit) ?? 0n) + BigInt(quantity),
      );
    }
  }
  const units = new Set([
    ...fundingAssets.keys(),
    ...outputFundingAssets.keys(),
  ]);
  if (
    [...units].some(
      (unit) =>
        (fundingAssets.get(unit) ?? 0n) !==
        (outputFundingAssets.get(unit) ?? 0n),
    )
  ) {
    throw new Error(
      `${measured.actionKind} signed body changes its reserved funding flow`,
    );
  }
};

const producedFundingInputs = ({
  signed,
  walletAddress,
}: {
  readonly signed: TxSigned;
  readonly walletAddress: string;
}): readonly WorkflowFundingReservedInput[] => {
  const body = signed.toTransaction().body();
  const outputs = body.outputs();
  const transactionHash = signed.toHash().toLowerCase();
  const produced: WorkflowFundingReservedInput[] = [];
  for (let index = 0; index < outputs.len(); index += 1) {
    const output = outputs.get(index);
    if (output.address().to_bech32() !== walletAddress) continue;
    const utxo = CML.TransactionUnspentOutput.new(
      CML.TransactionInput.new(
        CML.TransactionHash.from_hex(transactionHash),
        BigInt(index),
      ),
      output,
    );
    const decoded = coreToUtxo(utxo);
    const assets = Object.entries(decoded.assets)
      .filter(([unit]) => unit !== "lovelace")
      .sort(([left], [right]) => left.localeCompare(right))
      .map(([unit, quantity]) =>
        Object.freeze({ unit, quantity: quantity.toString() }),
      );
    produced.push(
      Object.freeze({
        outRef: `${transactionHash}#${index.toString()}`,
        role: "funding" as const,
        lovelace: decoded.assets.lovelace!.toString(),
        assets: Object.freeze(assets),
      }),
    );
  }
  if (produced.length === 0) {
    throw new Error("production transaction omitted reserved-wallet change");
  }
  return Object.freeze(produced);
};

export const prepareWorkflowFundingReservationTransaction = async ({
  journal,
  action,
  preflight,
}: {
  readonly journal: object;
  readonly action: FraudProofWorkflowAction;
  readonly preflight: object;
}): Promise<void> => {
  const state = stateForJournal(journal);
  if (state === undefined) return;
  const signed = workflowPreflightTransaction(preflight);
  if (signed === undefined) {
    throw new Error(
      "production preflight omitted its captured signed transaction",
    );
  }
  const kind = actionKind(action);
  if (state.currentActionKind !== kind) {
    throw new Error(
      "production funding action changed after reservation admission",
    );
  }
  const measured = state.requirements.actions.find(
    (entry) => entry.actionKind === kind,
  );
  if (measured === undefined) {
    throw new Error(`production funding profile omitted action ${kind}`);
  }
  const fundingOutRefs = canonicalOutRefs(
    state.currentFundingOutRefs,
    "reserved funding inputs",
  );
  const collateralOutRefs = canonicalOutRefs(
    state.currentCollateralOutRefs,
    "reserved collateral inputs",
  );
  const bodyInputs = [...workflowTransactionInputOutRefs(signed)].sort();
  const bodyCollateral = [
    ...workflowTransactionCollateralInputOutRefs(signed),
  ].sort();
  if (
    fundingOutRefs.some((outRef) => !bodyInputs.includes(outRef)) ||
    bodyInputs.some((outRef) => collateralOutRefs.includes(outRef))
  ) {
    throw new Error("signed transaction changed reserved ordinary inputs");
  }
  if (
    bodyCollateral.length !== collateralOutRefs.length ||
    bodyCollateral.some((outRef, index) => outRef !== collateralOutRefs[index])
  ) {
    throw new Error("signed transaction changed reserved collateral inputs");
  }
  await assertMeasuredTransactionBound({
    state,
    measured,
    signed,
    bodyInputs,
    fundingOutRefs,
  });
  const transactionHash = signed.toHash().toLowerCase();
  const transition = Object.freeze({
    actionKind: kind,
    signedTransactionCborHex: signed.toTransaction().to_canonical_cbor_hex(),
    transactionHash,
    transactionBodySha256: bodySha256(signed),
    consumedOutRefs: fundingOutRefs,
    producedInputs: producedFundingInputs({
      signed,
      walletAddress: state.snapshot.walletAddress,
    }),
  });
  state.snapshot = parseStateSnapshot(
    state,
    await state.port.prepare({
      expectedRevision: state.snapshot.revision,
      transition,
    }),
  );
  state.pendingTransactionHash = transactionHash;
};

export const assertWorkflowFundingReservationReadyToSubmit = async ({
  journal,
  transactionHash,
}: {
  readonly journal: object;
  readonly transactionHash: string;
}): Promise<void> => {
  const state = stateForJournal(journal);
  if (state === undefined) return;
  const expectedRevision = state.snapshot.revision;
  const expectedPending = state.pendingTransactionHash;
  await refresh(state);
  if (
    state.snapshot.state !== "active" ||
    state.snapshot.revision !== expectedRevision ||
    expectedPending !== transactionHash ||
    state.pendingTransactionHash !== transactionHash
  ) {
    throw new Error("production funding reservation changed before submission");
  }
};

const applyTransition = async ({
  journal,
  outcome,
  transactionHash,
}: {
  readonly journal: object;
  readonly outcome: "confirmed" | "not_found" | "conflict";
  readonly transactionHash: string;
}): Promise<void> => {
  const state = stateForJournal(journal);
  if (state === undefined) return;
  if (
    state.pendingTransactionHash !== undefined &&
    state.pendingTransactionHash !== transactionHash
  ) {
    throw new Error(
      "funding reservation reconciliation changed transaction hash",
    );
  }
  const next =
    outcome === "confirmed"
      ? await state.port.confirm({
          expectedRevision: state.snapshot.revision,
          transactionHash,
        })
      : outcome === "not_found"
        ? await state.port.abandon({
            expectedRevision: state.snapshot.revision,
            transactionHash,
          })
        : await state.port.markConflict({
            expectedRevision: state.snapshot.revision,
            code: "unexpected_spend",
          });
  state.snapshot = parseStateSnapshot(state, next);
  state.pendingTransactionHash = undefined;
  state.currentActionKind = undefined;
  state.currentFundingOutRefs = Object.freeze([]);
  state.currentCollateralOutRefs = Object.freeze([]);
};

export const confirmWorkflowFundingReservationTransaction = async (input: {
  readonly journal: object;
  readonly transactionHash: string;
}): Promise<void> => await applyTransition({ ...input, outcome: "confirmed" });

export const abandonWorkflowFundingReservationTransaction = async (input: {
  readonly journal: object;
  readonly transactionHash: string;
}): Promise<void> => await applyTransition({ ...input, outcome: "not_found" });

export const conflictWorkflowFundingReservationTransaction = async (input: {
  readonly journal: object;
  readonly transactionHash: string;
}): Promise<void> => await applyTransition({ ...input, outcome: "conflict" });

export const releaseWorkflowFundingReservation = async ({
  journal,
}: {
  readonly journal: object;
}): Promise<void> => {
  const state = stateForJournal(journal);
  if (state === undefined) return;
  state.snapshot = parseStateSnapshot(
    state,
    await state.port.release({ expectedRevision: state.snapshot.revision }),
  );
  state.currentActionKind = undefined;
  state.currentFundingOutRefs = Object.freeze([]);
  state.currentCollateralOutRefs = Object.freeze([]);
  state.pendingTransactionHash = undefined;
};

const balanceCbor = (utxos: readonly UTxO[]): string => {
  const assets: Record<string, bigint> = {};
  for (const utxo of utxos) {
    for (const [unit, quantity] of Object.entries(utxo.assets)) {
      assets[unit] = (assets[unit] ?? 0n) + quantity;
    }
  }
  return assetsToValue(assets).to_cbor_hex();
};

/**
 * Returns a signer whose wallet API exposes exactly the currently reserved
 * ordinary and collateral UTxOs. Signing and submission still delegate to the
 * original enterprise-key wallet.
 */
export const restrictWorkflowFundingSigner = ({
  signer,
  permit,
}: {
  readonly signer: ResolvedProverSigner;
  readonly permit: WorkflowFundingReservationPermit;
}): ResolvedProverSigner => {
  const state = admittedPermits.get(permit);
  if (state === undefined) {
    throw new Error("production funding reservation permit was not admitted");
  }
  if (
    signer.address !== state.snapshot.walletAddress ||
    signer.paymentKeyHash !== state.snapshot.fundingPaymentKeyHash
  ) {
    throw new Error("production signer differs from funding reservation");
  }
  return Object.freeze({
    ...signer,
    selectWallet: (lucid: LucidEvolution): void => {
      if (state.currentActionKind === undefined) {
        throw new Error(
          "production signer used before a reserved action began",
        );
      }
      signer.selectWallet(lucid);
      const original = lucid.wallet();
      const funding = state.currentFundingOutRefs.map(
        (outRef) => state.resolvedInputs.get(outRef)!,
      );
      const collateral = state.currentCollateralOutRefs.map(
        (outRef) => state.resolvedInputs.get(outRef)!,
      );
      const addressHex = CML.Address.from_bech32(signer.address).to_hex();
      const api: WalletApi = Object.freeze({
        getNetworkId: async () =>
          CML.Address.from_bech32(signer.address).to_raw_bytes()[0]! & 0x0f,
        getUtxos: async () =>
          funding.map((utxo) => utxoToCore(utxo).to_cbor_hex()),
        getBalance: async () => balanceCbor(funding),
        getUsedAddresses: async () => [addressHex],
        getUnusedAddresses: async () => [],
        getChangeAddress: async () => addressHex,
        getRewardAddresses: async () => [],
        signTx: async (tx) =>
          (
            await original.signTx(CML.Transaction.from_cbor_hex(tx))
          ).to_cbor_hex(),
        signData: async (address, payload) =>
          await original.signMessage(
            CML.Address.from_hex(address).to_bech32(),
            payload,
          ),
        submitTx: async (tx) => await original.submitTx(tx),
        getCollateral: async () =>
          collateral.map((utxo) => utxoToCore(utxo).to_cbor_hex()),
        experimental: Object.freeze({
          getCollateral: async () =>
            collateral.map((utxo) => utxoToCore(utxo).to_cbor_hex()),
          on: () => undefined,
          off: () => undefined,
        }),
      });
      lucid.selectWallet.fromAPI(api);
    },
  });
};

/** Test-only identity seam for runtime lifecycle tests that never build a tx. */
export const unsafeCreateWorkflowFundingReservationPermitForTest = ({
  category,
  actuationPermit,
  deploymentFingerprint,
  decisionDigest,
  rollbackGeneration,
}: {
  readonly category: FraudProofCatalogueCategoryName;
  readonly actuationPermit: WorkflowActuationPermit;
  readonly deploymentFingerprint: string;
  readonly decisionDigest: string;
  readonly rollbackGeneration: string;
}): WorkflowFundingReservationPermit => {
  if (process.env.NODE_ENV !== "test") {
    throw new Error("unsafe funding reservation permit is test-only");
  }
  const identity = assertWorkflowActuationPermitIdentity({
    permit: actuationPermit,
    category,
    rollbackGeneration,
  });
  if (
    identity.deploymentFingerprint !== deploymentFingerprint ||
    identity.decisionDigest !== decisionDigest
  ) {
    throw new Error("test funding reservation identity mismatch");
  }
  const permit: WorkflowFundingReservationPermit = Object.freeze({
    permitVersion: WORKFLOW_FUNDING_RESERVATION_PERMIT,
  });
  const snapshot: WorkflowFundingReservationSnapshot = Object.freeze({
    reservationId: "01".repeat(32),
    deploymentFingerprint,
    decisionDigest,
    profileDigest: "02".repeat(32),
    calculationDigest: "03".repeat(32),
    rollbackGeneration,
    revision: "0",
    walletAddress: "test-only-no-wallet",
    fundingPaymentKeyHash: "04".repeat(28),
    state: "active",
    activeInputs: Object.freeze([]),
  });
  const port: WorkflowFundingReservationPort = Object.freeze({
    load: async () => snapshot,
    resolveInputs: async () => [],
    resolveConfirmedActionOutput: async () => {
      throw new Error("unsafe test permit has no confirmed action lineage");
    },
    resolveProtocolInputAuthority: async () => {
      throw new Error("unsafe test permit has no protocol input authority");
    },
    prepare: async () => snapshot,
    confirm: async () => snapshot,
    abandon: async () => snapshot,
    markConflict: async () => snapshot,
    release: async () => snapshot,
  });
  admittedPermits.set(permit, {
    category,
    requirements: Object.freeze({
      schemaVersion: "midgard-production-workflow-funding-requirements-v1",
      scope: Object.freeze({ kind: "fraud_proof_category", category }),
      deploymentFingerprint,
      blueprintSha256: "05".repeat(32),
      protocolParametersDigest: "06".repeat(32),
      economicsPolicyDigest: "07".repeat(32),
      fundingPaymentKeyHash: "04".repeat(28),
      measurementToolVersion: "test-v1",
      measurementArtifactSha256: "08".repeat(32),
      actions: Object.freeze([]),
      profileDigest: "02".repeat(32),
    }),
    actuationPermit,
    port,
    maximumFundingInputs: 0,
    maximumCollateralInputs: 0,
    snapshot,
    resolvedInputs: new Map(),
    boundJournal: undefined,
    currentActionKind: undefined,
    currentFundingOutRefs: Object.freeze([]),
    currentCollateralOutRefs: Object.freeze([]),
    pendingTransactionHash: undefined,
  });
  return permit;
};

export const unsafeWorkflowFundingReservationSelectedOutRefsForTest = (
  permit: WorkflowFundingReservationPermit,
): Readonly<{
  fundingOutRefs: readonly string[];
  collateralOutRefs: readonly string[];
}> => {
  if (process.env.NODE_ENV !== "test") {
    throw new Error("unsafe funding reservation inspection is test-only");
  }
  const state = admittedPermits.get(permit);
  if (state === undefined) {
    throw new Error("production funding reservation permit was not admitted");
  }
  return Object.freeze({
    fundingOutRefs: Object.freeze([...state.currentFundingOutRefs]),
    collateralOutRefs: Object.freeze([...state.currentCollateralOutRefs]),
  });
};
