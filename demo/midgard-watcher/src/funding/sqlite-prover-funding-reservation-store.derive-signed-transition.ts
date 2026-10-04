import { createHash } from "node:crypto";
import { isAbsolute, normalize } from "node:path";

import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import { CML, coreToTxOutput } from "@lucid-evolution/lucid";

import { watcherCanonicalJson } from "../storage/durable-store.js";
import type { WatcherProtocolParameterHistory } from "./prover-funding.js";
import {
  parseWatcherProverFundingReservationRecord,
  type WatcherProverFundingReservationInput,
  type WatcherProverFundingReservationPlan,
  type WatcherProverFundingReservationRecord,
  type WatcherProverFundingReservationStore,
  type WatcherProverFundingReservationTransition,
} from "./prover-funding-reservation.js";

export const WATCHER_SQLITE_PROVER_FUNDING_RESERVATION_STORE =
  "midgard-watcher-sqlite-prover-funding-reservation-store-v1" as const;

export const HEX_32 = /^[0-9a-f]{64}$/u;

export type WatcherProverFundingReservationConflict = Readonly<{
  code: "reservation_collision";
  outRef: string;
}>;

const admittedConflicts = new WeakSet<object>();

export class ReservationConflictError extends Error {
  readonly conflict: WatcherProverFundingReservationConflict;

  constructor(outRef: string) {
    super("prover funding output is already reserved");
    this.name = "WatcherProductionProverFundingReservationConflictV1";
    this.conflict = Object.freeze({ code: "reservation_collision", outRef });
    admittedConflicts.add(this);
  }
}

export const isWatcherProverFundingReservationConflict = (
  value: unknown,
): value is Error &
  Readonly<{
    conflict: WatcherProverFundingReservationConflict;
  }> => value instanceof Error && admittedConflicts.has(value);

export type WatcherSqliteProverFundingReservationStoreRuntime = Readonly<{
  schemaVersion: typeof WATCHER_SQLITE_PROVER_FUNDING_RESERVATION_STORE;
  store: WatcherProverFundingReservationStore;
  protocolParameterHistory?: WatcherProtocolParameterHistory;
  close(): void;
}>;

export const canonicalDatabasePath = (value: unknown): string => {
  if (
    typeof value !== "string" ||
    value !== value.trim() ||
    !isAbsolute(value) ||
    normalize(value) !== value ||
    value === "/" ||
    value === "/tmp" ||
    value.startsWith("/tmp/")
  ) {
    throw new Error(
      "prover funding reservation store requires a canonical durable path",
    );
  }
  return value;
};

export const identicalInputs = (
  left: readonly WatcherProverFundingReservationInput[],
  right: readonly WatcherProverFundingReservationInput[],
): boolean => watcherCanonicalJson(left) === watcherCanonicalJson(right);

export const assertPlanMatchesRecord = (
  plan: WatcherProverFundingReservationPlan,
  record: WatcherProverFundingReservationRecord,
): void => {
  if (
    record.reservationId !== plan.reservationId ||
    record.deploymentFingerprint !== plan.deploymentFingerprint ||
    record.decisionDigest !== plan.decisionDigest ||
    record.policyDigest !== plan.policyDigest ||
    record.reservationBasisDigest !== plan.reservationBasisDigest
  ) {
    throw new Error("prover funding reservation identity mismatch");
  }
};

export const nextRecord = (input: {
  readonly current: WatcherProverFundingReservationRecord;
  readonly state?: WatcherProverFundingReservationRecord["state"];
  readonly activeInputs?: readonly WatcherProverFundingReservationInput[];
  readonly pendingTransition?: WatcherProverFundingReservationTransition | null;
  readonly lastConfirmedTransitionDigest?: string | null;
  readonly conflictCode?: WatcherProverFundingReservationRecord["conflictCode"];
}): WatcherProverFundingReservationRecord => {
  const recordInput = Object.freeze({
    reservationId: input.current.reservationId,
    deploymentFingerprint: input.current.deploymentFingerprint,
    decisionDigest: input.current.decisionDigest,
    policyDigest: input.current.policyDigest,
    reservationBasisDigest: input.current.reservationBasisDigest,
    revision: (BigInt(input.current.revision) + 1n).toString(),
    state: input.state ?? input.current.state,
    activeInputs: Object.freeze(
      [...(input.activeInputs ?? input.current.activeInputs)].sort(
        (left, right) => left.outRef.localeCompare(right.outRef),
      ),
    ),
    pendingTransition:
      input.pendingTransition === undefined
        ? input.current.pendingTransition
        : input.pendingTransition,
    lastConfirmedTransitionDigest:
      input.lastConfirmedTransitionDigest === undefined
        ? input.current.lastConfirmedTransitionDigest
        : input.lastConfirmedTransitionDigest,
    conflictCode:
      input.conflictCode === undefined
        ? input.current.conflictCode
        : input.conflictCode,
  });
  return parseWatcherProverFundingReservationRecord({
    ...recordInput,
    recordDigest: computeDeploymentManifestJsonDigest(recordInput),
  });
};

export const initialRecord = (
  plan: WatcherProverFundingReservationPlan,
): WatcherProverFundingReservationRecord => {
  const recordInput = Object.freeze({
    reservationId: plan.reservationId,
    deploymentFingerprint: plan.deploymentFingerprint,
    decisionDigest: plan.decisionDigest,
    policyDigest: plan.policyDigest,
    reservationBasisDigest: plan.reservationBasisDigest,
    revision: "0",
    state: "active" as const,
    activeInputs: plan.inputs,
    pendingTransition: null,
    lastConfirmedTransitionDigest: null,
    conflictCode: null,
  });
  return parseWatcherProverFundingReservationRecord({
    ...recordInput,
    recordDigest: computeDeploymentManifestJsonDigest(recordInput),
  });
};

export const makeTransition = (input: {
  readonly actionKind: string;
  readonly transactionHash: string;
  readonly transactionBodySha256: string;
  readonly signedTransactionCborHex: string;
  readonly consumedOutRefs: readonly string[];
  readonly producedInputs: readonly WatcherProverFundingReservationInput[];
}): WatcherProverFundingReservationTransition => {
  const transitionInput = Object.freeze({
    actionKind: input.actionKind,
    transactionHash: input.transactionHash,
    transactionBodySha256: input.transactionBodySha256,
    signedTransactionCborHex: input.signedTransactionCborHex,
    consumedOutRefs: Object.freeze([...input.consumedOutRefs].sort()),
    producedInputs: Object.freeze(
      [...input.producedInputs].sort((left, right) =>
        left.outRef.localeCompare(right.outRef),
      ),
    ),
  });
  const transition = Object.freeze({
    ...transitionInput,
    transitionDigest: computeDeploymentManifestJsonDigest(transitionInput),
  });
  const provisional = Object.freeze({
    reservationId: "00".repeat(32),
    deploymentFingerprint: "00".repeat(32),
    decisionDigest: "00".repeat(32),
    policyDigest: "00".repeat(32),
    reservationBasisDigest: "00".repeat(32),
    revision: "0",
    state: "active" as const,
    activeInputs: transition.producedInputs,
    pendingTransition: transition,
    lastConfirmedTransitionDigest: null,
    conflictCode: null,
  });
  parseWatcherProverFundingReservationRecord({
    ...provisional,
    recordDigest: computeDeploymentManifestJsonDigest(provisional),
  });
  return transition;
};

const transactionInputOutRefs = (inputs: {
  readonly len: () => number;
  readonly get: (index: number) => {
    readonly transaction_id: () => { readonly to_hex: () => string };
    readonly index: () => bigint | number;
  };
}): readonly string[] => {
  const outRefs: string[] = [];
  for (let index = 0; index < inputs.len(); index += 1) {
    const input = inputs.get(index);
    outRefs.push(
      `${input.transaction_id().to_hex()}#${input.index().toString()}`,
    );
  }
  return Object.freeze(outRefs.sort());
};

export const deriveSignedTransition = ({
  plan,
  activeInputs,
  input,
}: {
  readonly plan: WatcherProverFundingReservationPlan;
  readonly activeInputs: readonly WatcherProverFundingReservationInput[];
  readonly input: Parameters<
    WatcherProverFundingReservationStore["prepareTransition"]
  >[0];
}): Parameters<typeof makeTransition>[0] => {
  if (!/^(?:[0-9a-f]{2})+$/u.test(input.signedTransactionCborHex)) {
    throw new Error("prover transition signed transaction is malformed");
  }
  let transaction: CML.Transaction;
  try {
    transaction = CML.Transaction.from_cbor_hex(input.signedTransactionCborHex);
  } catch {
    throw new Error("prover transition signed transaction is malformed");
  }
  if (transaction.to_cbor_hex() !== input.signedTransactionCborHex) {
    throw new Error(
      "prover transition signed transaction is not a lossless Cardano encoding",
    );
  }
  const body = transaction.body();
  const bodyHash = CML.hash_transaction(body).to_raw_bytes();
  const vkeys = transaction.witness_set().vkeywitnesses();
  let fundingWitness = false;
  for (let index = 0; index < (vkeys?.len() ?? 0); index += 1) {
    const witness = vkeys!.get(index);
    if (
      witness.vkey().hash().to_hex() === plan.fundingPaymentKeyHash &&
      witness.vkey().verify(bodyHash, witness.ed25519_signature())
    ) {
      fundingWitness = true;
    }
  }
  if (!fundingWitness) {
    throw new Error("prover transition lacks its reserved funding witness");
  }
  const transactionHash = CML.hash_transaction(body).to_hex();
  const transactionBodySha256 = createHash("sha256")
    .update(Buffer.from(body.to_cbor_hex(), "hex"))
    .digest("hex");
  const planFunding = new Set(
    activeInputs
      .filter(({ role }) => role === "funding")
      .map(({ outRef }) => outRef),
  );
  const consumedOutRefs = transactionInputOutRefs(body.inputs()).filter(
    (outRef) => planFunding.has(outRef),
  );
  const outputs = body.outputs();
  const producedInputs: WatcherProverFundingReservationInput[] = [];
  for (let index = 0; index < outputs.len(); index += 1) {
    const output = coreToTxOutput(outputs.get(index));
    if (output.address !== plan.walletAddress) continue;
    const lovelace = output.assets.lovelace;
    if (lovelace === undefined || lovelace <= 0n) {
      throw new Error("prover transition wallet output omitted lovelace");
    }
    producedInputs.push(
      Object.freeze({
        outRef: `${transactionHash}#${index.toString()}`,
        role: "funding" as const,
        lovelace: lovelace.toString(),
        assets: Object.freeze(
          Object.entries(output.assets)
            .filter(([unit]) => unit !== "lovelace")
            .sort(([left], [right]) => left.localeCompare(right))
            .map(([unit, quantity]) =>
              Object.freeze({ unit, quantity: quantity.toString() }),
            ),
        ),
      }),
    );
  }
  const derived = Object.freeze({
    actionKind: input.actionKind,
    transactionHash,
    transactionBodySha256,
    signedTransactionCborHex: input.signedTransactionCborHex,
    consumedOutRefs,
    producedInputs: Object.freeze(producedInputs),
  });
  if (
    input.transactionHash !== derived.transactionHash ||
    input.transactionBodySha256 !== derived.transactionBodySha256 ||
    watcherCanonicalJson(input.consumedOutRefs) !==
      watcherCanonicalJson(derived.consumedOutRefs) ||
    watcherCanonicalJson(input.producedInputs) !==
      watcherCanonicalJson(derived.producedInputs)
  ) {
    throw new Error(
      "prover transition output differs from the signed transaction",
    );
  }
  return derived;
};
