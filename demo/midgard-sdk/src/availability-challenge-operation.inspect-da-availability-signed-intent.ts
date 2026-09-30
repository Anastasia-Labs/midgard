import { createHash } from "node:crypto";

import type {
  AvailabilityOperationIntent,
  AvailabilityOperationJournal,
} from "@al-ft/midgard-core/availability-operation-journal";
import {
  calculateMinLovelaceFromUTxO,
  CML,
  coreToTxOutput,
  type LucidEvolution,
} from "@lucid-evolution/lucid";

import { type DaAvailabilityParameters } from "./availability-challenge.js";

export type DaAvailabilityOperationObservation =
  | Readonly<{
      status: "included";
      txHash: string;
      inclusionPoint: string;
      confirmationDepth: number;
    }>
  | Readonly<{ status: "unspent"; currentSlot: number }>
  | Readonly<{
      status: "inputs_missing";
      currentSlot: number;
      missingOutRefs: readonly string[];
      /**
       * Missing refs whose consuming transaction the observer verified from
       * its raw bytes (exact hash, phase-2 valid, lists the ref). Absent when
       * the observer gathers no such evidence.
       */
      foreignSpends?: readonly DaAvailabilityForeignSpend[];
    }>
  | Readonly<{ status: "unknown"; reason: string }>
  | Readonly<{ status: "conflicting_spend"; reason: string }>;

/** A canonical, verified spend of one of an intent's missing refs. */
export type DaAvailabilityForeignSpend = Readonly<{
  outRef: string;
  spendingTxHash: string;
  spendPoint: string;
  /** Blocks on top of the spending block at the observation point. */
  confirmationDepth: number;
}>;

export type DaAvailabilityOperationResult = Readonly<{
  status:
    | "submitted"
    | "included"
    | "confirmed"
    | "waiting"
    | "expired"
    | "conflict";
  txHash: string;
  expectedOutRefs: readonly string[];
}>;

export type DaAvailabilityOperationContext = Readonly<{
  deploymentIdentity: string;
  actor: string;
  stateQueuePolicyId: string;
  journal: AvailabilityOperationJournal;
  minimumConfirmationDepth: number;
  transactionLimits: DaAvailabilityOperationLimits;
  /** Must revoke before awaiting rollback recovery; rechecked after signing. */
  assertActuationCurrent: () => void | Promise<void>;
  /** Exact canonical tx inclusion, or positive evidence ALL normal inputs survive. */
  observe: (
    intent: AvailabilityOperationIntent,
  ) => Promise<DaAvailabilityOperationObservation>;
  /** Broadcast these exact persisted bytes. An ambiguous error retains reservations. */
  submit: (signedCbor: string) => Promise<string>;
  nowMs?: () => number;
  leaseDurationMs?: number;
}>;

export type DaAvailabilityOperationLimits = Readonly<{
  maxTxSize: number;
  maxTxExMem: bigint;
  maxTxExSteps: bigint;
  coinsPerUtxoByte: bigint;
  /**
   * Per-action fee ceilings. For `timeout` the ceiling caps only the
   * challenger's contribution `c = fee - feePart`; the slashed penalty share
   * `feePart = min(penalty, taken)` is fee by protocol and outside every cap.
   */
  feeCeilings: Readonly<Record<string, bigint>>;
  /**
   * The largest slashed fee part a timeout can carry, `da_slash_penalty`.
   * Bounds a journaled timeout whose exact fee part is no longer known (the
   * rebroadcast path): `fee <= timeoutFeePartCeiling + feeCeilings.timeout`.
   * Absent means `0`, which admits only timeouts whose whole fee fits the cap.
   */
  timeoutFeePartCeiling?: bigint;
}>;

export const daAvailabilityOperationLimits = (
  lucid: LucidEvolution,
  parameters: DaAvailabilityParameters,
): DaAvailabilityOperationLimits => {
  const protocol = lucid.config().protocolParameters;
  if (!protocol)
    throw new Error("Availability operations require live ledger parameters");
  return {
    maxTxSize: protocol.maxTxSize,
    maxTxExMem: protocol.maxTxExMem,
    maxTxExSteps: protocol.maxTxExSteps,
    coinsPerUtxoByte: protocol.coinsPerUtxoByte,
    feeCeilings: {
      prepare: parameters.max_open_fee_lovelace,
      open: parameters.max_open_fee_lovelace,
      publish: parameters.max_publication_fee_lovelace,
      settle: parameters.max_settlement_fee_lovelace,
      close: parameters.max_close_fee_lovelace,
      timeout: parameters.max_timeout_fee_lovelace,
      prune: parameters.max_timeout_fee_lovelace,
      remove: parameters.max_timeout_fee_lovelace,
    },
    // feePart = min(penalty, taken) and taken <= da_bond > penalty.
    timeoutFeePartCeiling: parameters.da_slash_penalty_lovelace,
  };
};

/**
 * `timeoutFeePart` is the builder's exact slashed fee part when known (at sign
 * time); a timeout without it is held to the aggregate bound.
 */
export const assertDaAvailabilitySignedLimits = (
  intent: Pick<AvailabilityOperationIntent, "action" | "signedCbor" | "txHash">,
  limits: DaAvailabilityOperationLimits,
  timeoutFeePart?: bigint,
): void => {
  const transaction = CML.Transaction.from_cbor_hex(intent.signedCbor);
  const body = transaction.body();
  const redeemers = transaction.witness_set().redeemers()?.to_flat_format();
  let memory = 0n;
  let steps = 0n;
  for (let index = 0; index < (redeemers?.len() ?? 0); index += 1) {
    const units = redeemers!.get(index).ex_units();
    memory += units.mem();
    steps += units.steps();
  }
  const ceiling = limits.feeCeilings[intent.action];
  const fee = body.fee();
  const partCeiling = limits.timeoutFeePartCeiling ?? 0n;
  // Timeout: the ceiling caps c = fee - feePart only.
  const capped =
    intent.action !== "timeout"
      ? ceiling !== undefined && fee <= ceiling
      : ceiling !== undefined &&
        (timeoutFeePart === undefined
          ? partCeiling >= 0n && fee <= partCeiling + ceiling
          : timeoutFeePart >= 0n &&
            timeoutFeePart <= partCeiling &&
            fee >= timeoutFeePart &&
            fee - timeoutFeePart <= ceiling);
  if (
    !Number.isSafeInteger(limits.maxTxSize) ||
    limits.maxTxSize <= 0 ||
    limits.maxTxExMem <= 0n ||
    limits.maxTxExSteps <= 0n ||
    intent.signedCbor.length / 2 > limits.maxTxSize ||
    memory * 5n > limits.maxTxExMem * 4n ||
    steps * 5n > limits.maxTxExSteps * 4n ||
    !capped ||
    fee <= 0n ||
    (intent.action !== "prepare" && (memory === 0n || steps === 0n))
  ) {
    throw new Error(
      "Signed availability transaction exceeds deployment fee, size or execution reserve",
    );
  }
  for (let index = 0; index < body.outputs().len(); index += 1) {
    const output = coreToTxOutput(body.outputs().get(index));
    if (
      limits.coinsPerUtxoByte <= 0n ||
      (output.assets.lovelace ?? 0n) <
        calculateMinLovelaceFromUTxO(limits.coinsPerUtxoByte, {
          ...output,
          txHash: intent.txHash,
          outputIndex: index,
        })
    )
      throw new Error(
        "Signed availability transaction output is below live minimum ADA",
      );
  }
};

const hash = (value: string): string =>
  createHash("sha256").update(value).digest("hex");

const outRefs = (
  inputs: CML.TransactionInputList | undefined,
): readonly string[] =>
  inputs
    ? Array.from({ length: inputs.len() }, (_, index) => {
        const input = inputs.get(index);
        return `${input.transaction_id().to_hex()}#${input.index().toString()}`;
      })
    : [];

export const inspectDaAvailabilitySignedIntent = (input: {
  deploymentIdentity: string;
  actor: string;
  headerHash: string;
  action: string;
  signedCbor: string;
  completesWorkflow?: boolean;
}): AvailabilityOperationIntent => {
  if (
    !/^[0-9a-f]{64}$/u.test(input.deploymentIdentity) ||
    !/^[0-9a-f]{56}$/u.test(input.actor) ||
    !/^[0-9a-f]{56}$/u.test(input.headerHash) ||
    !/^(prepare|open|publish|settle|close|timeout|prune|remove)$/u.test(
      input.action,
    )
  ) {
    throw new Error("Invalid availability operation identity");
  }
  const transaction = CML.Transaction.from_cbor_hex(input.signedCbor);
  const completesWorkflow =
    input.completesWorkflow ??
    (input.action === "close" || input.action === "remove");
  if (
    completesWorkflow &&
    !["close", "timeout", "remove"].includes(input.action)
  ) {
    throw new Error(
      "Only a terminal challenge operation can release future wallet capital",
    );
  }
  const body = transaction.body();
  const txHash = CML.hash_transaction(body).to_hex();
  const ttl = body.ttl();
  const validUntilSlot = ttl === undefined ? NaN : Number(ttl);
  const lower = body.validity_interval_start();
  const validFromSlot = lower === undefined ? NaN : Number(lower);
  const spentOutRefs = outRefs(body.inputs());
  const collateralOutRefs = outRefs(body.collateral_inputs());
  if (
    !transaction.is_valid() ||
    !Number.isSafeInteger(validUntilSlot) ||
    validUntilSlot <= 0 ||
    !Number.isSafeInteger(validFromSlot) ||
    validFromSlot < 0 ||
    validFromSlot >= validUntilSlot ||
    spentOutRefs.length === 0 ||
    (input.action !== "prepare" && collateralOutRefs.length === 0) ||
    new Set([...spentOutRefs, ...collateralOutRefs]).size !==
      spentOutRefs.length + collateralOutRefs.length
  ) {
    throw new Error(
      "Availability intent requires valid signed transaction, finite expiry and disjoint collateral",
    );
  }
  const witnesses = transaction.witness_set().vkeywitnesses();
  if (!witnesses || witnesses.len() === 0)
    throw new Error("Availability operation has no signing witnesses");
  let actorSigned = false;
  for (let index = 0; index < witnesses.len(); index += 1) {
    const witness = witnesses.get(index);
    if (
      !witness
        .vkey()
        .verify(Buffer.from(txHash, "hex"), witness.ed25519_signature())
    ) {
      throw new Error(
        "Availability operation contains an invalid key signature",
      );
    }
    actorSigned ||= witness.vkey().hash().to_hex() === input.actor;
  }
  if (!actorSigned)
    throw new Error(
      "Availability operation was not signed by its reserved actor",
    );
  return Object.freeze({
    deploymentIdentity: input.deploymentIdentity,
    actor: input.actor,
    headerHash: input.headerHash,
    action: input.action,
    signedCbor: input.signedCbor,
    id: hash(
      `${input.deploymentIdentity}:${input.actor}:${input.headerHash}:${input.action}:${txHash}:${completesWorkflow}`,
    ),
    txHash,
    spentOutRefs,
    collateralOutRefs,
    expectedOutRefs: Array.from(
      { length: body.outputs().len() },
      (_, index) => `${txHash}#${index}`,
    ),
    validUntilSlot,
    completesWorkflow,
  });
};
