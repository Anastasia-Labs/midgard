import { createHash, randomUUID } from "node:crypto";

import type {
  AvailabilityOperationIntent,
  AvailabilityOperationJournal,
  AvailabilityOperationLease,
  AvailabilityOperationRecord,
} from "@al-ft/midgard-core/availability-operation-journal";
import {
  calculateMinLovelaceFromUTxO,
  CML,
  coreToTxOutput,
  type LucidEvolution,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  type DaAvailabilityChallengeRecord,
  type DaAvailabilityParameters,
  parseDaAvailabilityChallengeRecordCbor,
} from "./availability-challenge.js";
import { STATE_QUEUE_NODE_ASSET_NAME_PREFIX } from "./linked-list.js";

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

/**
 * The deployed Open predicate requires an exact
 * `challenger_bond_lovelace + challenge_record_lovelace + fee` input.
 */
export const buildDaAvailabilityFundingPreparationTx = async (
  lucid: LucidEvolution,
  input: Readonly<{
    fundingInput: UTxO;
    outputLovelace: bigint;
    feeLovelace: bigint;
    validFrom: bigint;
    validTo: bigint;
  }>,
): Promise<TxSignBuilder> => {
  const walletAddress = await lucid.wallet().address();
  const funding = input.fundingInput;
  if (
    funding.address !== walletAddress ||
    funding.datum !== undefined ||
    funding.datumHash !== undefined ||
    funding.scriptRef !== undefined ||
    Object.keys(funding.assets).length !== 1 ||
    input.outputLovelace <= 0n ||
    input.feeLovelace <= 0n ||
    input.validFrom < 0n ||
    input.validTo <= input.validFrom ||
    input.validTo - input.validFrom > 120_000n ||
    input.validTo > BigInt(Number.MAX_SAFE_INTEGER)
  ) {
    throw new Error("Invalid availability funding preparation resources");
  }
  const change =
    (funding.assets.lovelace ?? 0n) - input.outputLovelace - input.feeLovelace;
  const coinsPerByte = lucid.config().protocolParameters?.coinsPerUtxoByte;
  if (!coinsPerByte)
    throw new Error(
      "Availability funding preparation needs live protocol parameters",
    );
  for (const lovelace of [input.outputLovelace, change]) {
    if (
      lovelace !== 0n &&
      lovelace <
        calculateMinLovelaceFromUTxO(coinsPerByte, {
          address: walletAddress,
          assets: { lovelace },
          txHash: "00".repeat(32),
          outputIndex: 0,
        })
    )
      throw new Error(
        "Availability funding preparation has insufficient working capital or min-ADA change",
      );
  }
  const live = await lucid.utxosByOutRef([funding]);
  if (
    live.length !== 1 ||
    live[0]!.address !== funding.address ||
    live[0]!.assets.lovelace !== funding.assets.lovelace ||
    Object.keys(live[0]!.assets).length !== 1
  ) {
    throw new Error("Availability funding preparation input is stale");
  }
  let tx = lucid
    .newTx()
    .collectFrom([funding])
    .pay.ToAddress(walletAddress, { lovelace: input.outputLovelace });
  if (change > 0n) tx = tx.pay.ToAddress(walletAddress, { lovelace: change });
  const built = await tx
    .setMinFee(input.feeLovelace)
    .validFrom(Number(input.validFrom))
    .validTo(Number(input.validTo))
    .complete({ localUPLCEval: true, coinSelection: false });
  const body = built.toTransaction().body();
  if (
    body.fee() !== input.feeLovelace ||
    body.inputs().len() !== 1 ||
    body.outputs().len() !== (change > 0n ? 2 : 1)
  ) {
    throw new Error(
      "Availability funding preparation changed the reserved layout",
    );
  }
  return built;
};

const assertContext = (context: DaAvailabilityOperationContext): void => {
  if (
    !/^[0-9a-f]{64}$/u.test(context.deploymentIdentity) ||
    !/^[0-9a-f]{56}$/u.test(context.actor) ||
    !/^[0-9a-f]{56}$/u.test(context.stateQueuePolicyId) ||
    !Number.isSafeInteger(context.minimumConfirmationDepth) ||
    context.minimumConfirmationDepth <= 0
  ) {
    throw new Error(
      "Invalid availability operation deployment/finality authority",
    );
  }
};

const assertTerminalIntent = (
  context: DaAvailabilityOperationContext,
  intent: AvailabilityOperationIntent,
): void => {
  if (intent.action !== "timeout" && intent.action !== "remove") return;
  const mint = CML.Transaction.from_cbor_hex(intent.signedCbor).body().mint();
  const removesTarget =
    mint?.get(
      CML.ScriptHash.from_hex(context.stateQueuePolicyId),
      CML.AssetName.from_hex(
        STATE_QUEUE_NODE_ASSET_NAME_PREFIX + intent.headerHash,
      ),
    ) === -1n;
  if (intent.completesWorkflow !== removesTarget) {
    throw new Error(
      "Availability terminal metadata does not match the signed target-header burn",
    );
  }
};

const withLease = async <T>(
  context: DaAvailabilityOperationContext,
  run: (
    lease: AvailabilityOperationLease,
    assertCurrent: () => Promise<void>,
  ) => Promise<T>,
): Promise<T> => {
  assertContext(context);
  const now = context.nowMs ?? Date.now;
  const lease = context.journal.acquire(
    context.actor,
    randomUUID(),
    now(),
    context.leaseDurationMs ?? 300_000,
  );
  const assertCurrent = async (): Promise<void> => {
    context.journal.assertLease(lease, now());
    await context.assertActuationCurrent();
    context.journal.assertLease(lease, now());
  };
  try {
    await assertCurrent();
    return await run(lease, assertCurrent);
  } finally {
    context.journal.release(lease);
  }
};

const reconcile = async (
  context: DaAvailabilityOperationContext,
  lease: AvailabilityOperationLease,
  intent: AvailabilityOperationIntent,
  assertCurrent: () => Promise<void>,
): Promise<DaAvailabilityOperationResult> => {
  const checked = inspectDaAvailabilitySignedIntent(intent);
  if (
    JSON.stringify(checked) !== JSON.stringify(intent) ||
    intent.deploymentIdentity !== context.deploymentIdentity ||
    intent.actor !== context.actor
  ) {
    throw new Error(
      "Persisted availability operation metadata does not match signed transaction",
    );
  }
  assertTerminalIntent(context, intent);
  const result = (
    status: DaAvailabilityOperationResult["status"],
  ): DaAvailabilityOperationResult => ({
    status,
    txHash: intent.txHash,
    expectedOutRefs: intent.expectedOutRefs,
  });
  await assertCurrent();
  const observation = await context.observe(intent);
  await assertCurrent();
  const now = context.nowMs ?? Date.now;
  if (context.journal.get(intent.id)?.state === "confirmed") {
    if (
      observation.status === "unknown" ||
      observation.status === "inputs_missing"
    )
      return result("waiting");
    if (
      observation.status !== "included" ||
      observation.txHash !== intent.txHash ||
      !observation.inclusionPoint ||
      !Number.isSafeInteger(observation.confirmationDepth) ||
      observation.confirmationDepth < context.minimumConfirmationDepth
    ) {
      context.journal.halt(
        "A finalized availability transaction lost canonical finality; authenticated recovery required",
      );
      throw new Error("Finalized availability transaction rolled back");
    }
    return result("confirmed");
  }
  switch (observation.status) {
    case "included":
      if (
        observation.txHash !== intent.txHash ||
        !observation.inclusionPoint ||
        !Number.isSafeInteger(observation.confirmationDepth) ||
        observation.confirmationDepth < 0
      ) {
        throw new Error(
          "Availability operation inclusion does not authenticate the signed intent",
        );
      }
      if (observation.confirmationDepth < context.minimumConfirmationDepth) {
        context.journal.transition(
          lease,
          intent.id,
          "included",
          observation.inclusionPoint,
          null,
          now(),
        );
        return result("included");
      }
      context.journal.transition(
        lease,
        intent.id,
        "confirmed",
        observation.inclusionPoint,
        null,
        now(),
      );
      return result("confirmed");
    case "unknown":
      if (context.journal.get(intent.id)?.state === "included") {
        context.journal.transition(
          lease,
          intent.id,
          "pending",
          null,
          observation.reason,
          now(),
        );
      }
      return result("waiting");
    case "conflicting_spend":
      context.journal.transition(
        lease,
        intent.id,
        "conflict",
        null,
        observation.reason,
        now(),
      );
      return result("conflict");
    case "inputs_missing": {
      const foreignSpends = observation.foreignSpends ?? [];
      if (
        !Number.isSafeInteger(observation.currentSlot) ||
        observation.currentSlot < 0 ||
        observation.missingOutRefs.length === 0 ||
        observation.missingOutRefs.some(
          (ref) =>
            !intent.spentOutRefs.includes(ref) &&
            !intent.collateralOutRefs.includes(ref),
        ) ||
        !Array.isArray(foreignSpends) ||
        foreignSpends.some(
          (spend) =>
            !observation.missingOutRefs.includes(spend.outRef) ||
            typeof spend.spendingTxHash !== "string" ||
            !/^[0-9a-f]{64}$/u.test(spend.spendingTxHash) ||
            typeof spend.spendPoint !== "string" ||
            spend.spendPoint.length === 0 ||
            !Number.isSafeInteger(spend.confirmationDepth) ||
            spend.confirmationDepth < 0 ||
            // Our own transaction spending a normal input is inclusion the
            // source failed to report: it contradicts itself.
            (intent.spentOutRefs.includes(spend.outRef) &&
              spend.spendingTxHash === intent.txHash),
        )
      ) {
        throw new Error("Invalid canonical missing-input observation");
      }
      const orphaned =
        observation.currentSlot >= intent.validUntilSlot &&
        observation.missingOutRefs.every((ref) => {
          const parent = context.journal.findTransaction(ref.split("#")[0]!);
          return (
            parent?.state === "expired" &&
            parent.intent.expectedOutRefs.includes(ref) &&
            parent.intent.validUntilSlot <= observation.currentSlot
          );
        });
      if (orphaned) {
        context.journal.transition(
          lease,
          intent.id,
          "expired",
          null,
          "Expired child of canonically expired parent",
          now(),
        );
        return result("expired");
      }
      // A transaction spends all of its normal inputs or none of them, so one
      // missing while another is still unspent at the same point proves the
      // intent is not included there; past its validity it never can be. This
      // is how a shared input spent by someone else (a pool TopUp, another
      // header's Timeout) releases the intent instead of waiting forever.
      const missingNormal = intent.spentOutRefs.filter((ref) =>
        observation.missingOutRefs.includes(ref),
      );
      if (
        observation.currentSlot >= intent.validUntilSlot &&
        missingNormal.length > 0 &&
        missingNormal.length < intent.spentOutRefs.length
      ) {
        context.journal.transition(
          lease,
          intent.id,
          "expired",
          null,
          "Expired with a normal input spent elsewhere and another still unspent",
          now(),
        );
        return result("expired");
      }
      // Every normal input may be gone, as when another watcher's Timeout on
      // the same header landed first. Absence never proves anything, but one
      // normal input consumed by another valid canonical transaction at
      // finality does: a ledger input is spent once, so ours can never land.
      if (
        observation.currentSlot >= intent.validUntilSlot &&
        foreignSpends.some(
          (spend) =>
            intent.spentOutRefs.includes(spend.outRef) &&
            spend.spendingTxHash !== intent.txHash &&
            spend.confirmationDepth >= context.minimumConfirmationDepth,
        )
      ) {
        context.journal.transition(
          lease,
          intent.id,
          "expired",
          null,
          "Expired with a normal input finally spent by another transaction",
          now(),
        );
        return result("expired");
      }
      if (context.journal.get(intent.id)?.state === "included") {
        context.journal.transition(
          lease,
          intent.id,
          "pending",
          null,
          "Canonical input/inclusion evidence is unresolved",
          now(),
        );
      }
      return result("waiting");
    }
    case "unspent": {
      if (
        !Number.isSafeInteger(observation.currentSlot) ||
        observation.currentSlot < 0
      ) {
        throw new Error(
          "Availability operation reconciliation requires an authoritative slot",
        );
      }
      if (observation.currentSlot >= intent.validUntilSlot) {
        context.journal.transition(
          lease,
          intent.id,
          "expired",
          null,
          "Expired with every normal input canonically unspent",
          now(),
        );
        return result("expired");
      }
      context.journal.transition(
        lease,
        intent.id,
        "pending",
        null,
        "Rebroadcasting canonically unspent intent",
        now(),
      );
      assertDaAvailabilitySignedLimits(intent, context.transactionLimits);
      await assertCurrent();
      // A crash or timeout here is recovered by observing or re-broadcasting the
      // exact same transaction. Never replace signed bytes after an error.
      const txHash = await context.submit(intent.signedCbor);
      if (txHash !== intent.txHash)
        throw new Error(
          "Provider returned a different availability transaction hash",
        );
      return result("submitted");
    }
  }
};

export const reconcileDaAvailabilityOperations = (
  context: DaAvailabilityOperationContext,
): Promise<readonly DaAvailabilityOperationResult[]> =>
  withLease(context, async (lease, assertCurrent) => {
    const results: DaAvailabilityOperationResult[] = [];
    const records = [
      ...context.journal.pending(context.deploymentIdentity, context.actor),
      ...context.journal.unfinalized(context.deploymentIdentity, context.actor),
      ...context.journal.finalizedAnchors(
        context.deploymentIdentity,
        context.actor,
      ),
    ];
    const byTransaction = new Map(
      records.map((record) => [record.intent.txHash, record]),
    );
    const ordered: typeof records = [];
    const visiting = new Set<string>();
    const visited = new Set<string>();
    const visit = (record: (typeof records)[number]): void => {
      if (visited.has(record.intent.id)) return;
      if (visiting.has(record.intent.id))
        throw new Error(
          "Availability journal contains cyclic transaction dependencies",
        );
      visiting.add(record.intent.id);
      for (const ref of record.intent.spentOutRefs) {
        const parent = byTransaction.get(ref.split("#")[0]!);
        if (parent) visit(parent);
      }
      visiting.delete(record.intent.id);
      visited.add(record.intent.id);
      ordered.push(record);
    };
    records.forEach(visit);
    const covered = new Set<string>();
    for (const record of [...ordered].reverse()) {
      if (covered.has(record.intent.id)) continue;
      const outcome = await reconcile(
        context,
        lease,
        record.intent,
        assertCurrent,
      );
      results.push(outcome);
      if (outcome.status !== "included" && outcome.status !== "confirmed")
        continue;
      const ancestors = [...record.intent.spentOutRefs];
      while (ancestors.length > 0) {
        const ref = ancestors.pop()!;
        const parent = byTransaction.get(ref.split("#")[0]!);
        if (!parent || covered.has(parent.intent.id)) continue;
        // An unfinalized child proves ancestry, but does not replace the audit
        // of a previously finalized parent after a possible deep rollback.
        if (parent.state === "confirmed" && outcome.status !== "confirmed")
          continue;
        covered.add(parent.intent.id);
        if (parent.state !== "confirmed") {
          const verified = inspectDaAvailabilitySignedIntent(parent.intent);
          if (JSON.stringify(verified) !== JSON.stringify(parent.intent))
            throw new Error("Corrupt availability ancestor intent");
          context.journal.transition(
            lease,
            parent.intent.id,
            outcome.status,
            `ancestor-of:${record.intent.txHash}`,
            null,
            (context.nowMs ?? Date.now)(),
          );
        }
        ancestors.push(...parent.intent.spentOutRefs);
      }
    }
    // A rolled-back child may be visited before its ancestor's expiry is proved.
    // Revisit only those children, now in dependency order, so an arbitrarily
    // long expired chain releases in one recovery pass rather than one per tick.
    for (const record of ordered) {
      const index = results.findIndex(
        (result) =>
          result.txHash === record.intent.txHash && result.status === "waiting",
      );
      if (
        index < 0 ||
        context.journal.get(record.intent.id)?.state === "confirmed"
      )
        continue;
      if (
        !record.intent.spentOutRefs.some(
          (ref) =>
            context.journal.findTransaction(ref.split("#")[0]!)?.state ===
            "expired",
        )
      )
        continue;
      results[index] = await reconcile(
        context,
        lease,
        record.intent,
        assertCurrent,
      );
    }
    return results;
  });

export type DaAvailabilityCanonicalBoundary = Readonly<{
  pointId: string;
  slot: number;
}>;

/**
 * Positive evidence that `outRef` was consumed by the canonical transaction
 * `transactionId`, read from its raw bytes: they hash to that id, the
 * transaction is phase-2 valid (it spent its inputs, not its collateral), and
 * its inputs list the outRef. A Kupo `spent_at` claim is trusted only through
 * this check.
 */
export const transactionConsumesOutRef = ({
  transactionCbor,
  transactionId,
  outRef,
}: {
  readonly transactionCbor: string;
  readonly transactionId: string;
  readonly outRef: string;
}): boolean => {
  let transaction: CML.Transaction;
  try {
    transaction = CML.Transaction.from_cbor_hex(transactionCbor);
  } catch {
    return false;
  }
  const body = transaction.body();
  if (
    CML.hash_transaction(body).to_hex() !== transactionId ||
    !transaction.is_valid()
  )
    return false;
  const inputs = body.inputs();
  return Array.from({ length: inputs.len() }, (_, index) =>
    inputs.get(index),
  ).some(
    (entry) =>
      `${entry.transaction_id().to_hex()}#${entry.index().toString()}` ===
      outRef,
  );
};

/** A chain point as the foreign-spend readers name it. */
export type DaAvailabilityChainPoint = Readonly<{
  slot: number;
  blockHash: string;
}>;

/**
 * The L1 readers the verified foreign-spend check runs on. Each caller
 * injects its own Kupo and Ogmios transport; none has a default.
 */
export type DaAvailabilityForeignSpendReaders = Readonly<{
  /** The canonical boundary, with the block height of its point. */
  readBoundary: () => Promise<Readonly<{ pointId: string; blockNo: number }>>;
  /** Kupo's exact-match `spent_at`, or undefined when it reports no spend. */
  fetchSpend: (
    outRef: Readonly<{ txHash: string; outputIndex: number }>,
  ) => Promise<
    | Readonly<{ transactionId: string; point: DaAvailabilityChainPoint }>
    | undefined
  >;
  /** A Kupo checkpoint strictly before `slot`, to intersect chain-sync at. */
  fetchAncestor: (slot: number) => Promise<DaAvailabilityChainPoint>;
  /**
   * The transaction `txHash`, read by chain-sync from `ancestor` forward to
   * the exact block `point`, with that block's height and, when Ogmios serves
   * it, the raw transaction. Undefined when the block does not carry it.
   */
  readTransaction: (
    input: Readonly<{
      ancestor: DaAvailabilityChainPoint;
      point: DaAvailabilityChainPoint;
      txHash: string;
    }>,
  ) => Promise<
    | Readonly<{
        txHash: string;
        point: DaAvailabilityChainPoint & Readonly<{ blockNo: number }>;
        cbor?: string;
      }>
    | undefined
  >;
}>;

/** A verified spend, with the consuming transaction's checked bytes. */
export type DaAvailabilityVerifiedForeignSpend = DaAvailabilityForeignSpend &
  Readonly<{
    /** The raw transaction {@link transactionConsumesOutRef} accepted. */
    spendingTransactionCbor: string;
  }>;

/**
 * The canonical spend of `outRef`, verified from the consuming transaction's
 * own bytes through {@link transactionConsumesOutRef}. Kupo's `spent_at`
 * alone is never trusted, so a spend that fails verification reads as none.
 * The whole read sits inside one canonical boundary; a moved boundary, a
 * spend above it, or a spend Ogmios serves without its raw bytes throws.
 */
export const resolveDaAvailabilityForeignSpend = async (
  input: DaAvailabilityForeignSpendReaders & Readonly<{ outRef: string }>,
): Promise<DaAvailabilityVerifiedForeignSpend | undefined> => {
  const [txHash, outputIndex] = input.outRef.split("#");
  const before = await input.readBoundary();
  const spend = await input.fetchSpend({
    txHash: txHash!,
    outputIndex: Number(outputIndex),
  });
  if (spend === undefined) return undefined;
  const ancestor = await input.fetchAncestor(spend.point.slot);
  const transaction = await input.readTransaction({
    ancestor,
    point: spend.point,
    txHash: spend.transactionId,
  });
  const after = await input.readBoundary();
  if (before.pointId !== after.pointId)
    throw new Error(
      "Availability input spend changed during its canonical read",
    );
  if (transaction !== undefined && transaction.point.blockNo > after.blockNo)
    throw new Error(
      "Availability input spend lies above the canonical boundary",
    );
  if (transaction === undefined || transaction.txHash !== spend.transactionId)
    return undefined;
  if (transaction.cbor === undefined)
    throw new Error(
      "Ogmios must run with --include-transaction-cbor to verify a rival spend",
    );
  if (
    !transactionConsumesOutRef({
      transactionCbor: transaction.cbor,
      transactionId: spend.transactionId,
      outRef: input.outRef,
    })
  )
    return undefined;
  return {
    outRef: input.outRef,
    spendingTxHash: spend.transactionId,
    spendPoint: `${transaction.point.slot}:${transaction.point.blockHash}`,
    confirmationDepth: after.blockNo - transaction.point.blockNo,
    spendingTransactionCbor: transaction.cbor,
  };
};

/** Why a stranded challenge workflow row may be released (P20). */
export type DaAvailabilityWorkflowRelease = Readonly<{
  /**
   * `header-node-burned`: a transaction burned the header's queue node.
   * `challenge-closed`: a transaction spent the challenge record and minted
   * nothing under the queue policy, which is a Close by anyone.
   */
  reason: "header-node-burned" | "challenge-closed";
  txHash: string;
  spendPoint: string;
  confirmationDepth: number;
}>;

/** The most node-chain hops one release check walks. */
export const DA_AVAILABILITY_WORKFLOW_RELEASE_MAX_HOPS = 256;

/**
 * The header's node chain is longer than one release check walks. The row is
 * kept and the check runs again on the next reconciliation.
 */
export class DaAvailabilityWorkflowReleaseHopCapError extends Error {
  constructor(headerHash: string) {
    super(
      `Availability workflow release walk for header ${headerHash} reached ${DA_AVAILABILITY_WORKFLOW_RELEASE_MAX_HOPS.toString()} hops without a terminal transaction`,
    );
    this.name = "DaAvailabilityWorkflowReleaseHopCapError";
  }
}

const transactionOutputs = (
  body: CML.TransactionBody,
): ReturnType<typeof coreToTxOutput>[] =>
  Array.from({ length: body.outputs().len() }, (_, index) =>
    coreToTxOutput(body.outputs().get(index)),
  );

const transactionInputRefs = (body: CML.TransactionBody): string[] => {
  const inputs = body.inputs();
  return Array.from({ length: inputs.len() }, (_, index) => {
    const entry = inputs.get(index);
    return `${entry.transaction_id().to_hex()}#${entry.index().toString()}`;
  });
};

/** Indices of the outputs holding any quantity of `unit`. */
const outputsCarrying = (
  outputs: readonly ReturnType<typeof coreToTxOutput>[],
  unit: string,
): number[] =>
  outputs.flatMap((output, index) =>
    (output.assets[unit] ?? 0n) === 0n ? [] : [index],
  );

const mintOf = (
  mint: CML.Mint | undefined,
  policy: string,
  assetName: string,
): bigint | undefined =>
  mint?.get(CML.ScriptHash.from_hex(policy), CML.AssetName.from_hex(assetName));

const mintsUnderPolicy = (
  mint: CML.Mint | undefined,
  policy: string,
): boolean => {
  const assets = mint?.get_assets(CML.ScriptHash.from_hex(policy));
  return assets !== undefined && assets.len() > 0;
};

/**
 * Evidence that this actor's challenge workflow for `headerHash` ended in a
 * terminal step someone else landed, or undefined when there is none yet
 * (P20). From the confirmed Open's signed bytes it derives, by asset and never
 * by index, the queue policy (the one policy under which an Open output holds
 * the header's node NFT), that node output Q0 and the challenge record output
 * R0 (the output holding the challenge asset the Open minted, whose datum
 * decodes as the header's record). It then walks the header's node chain from
 * Q0, one verified spend per hop, until a transaction either burns the node
 * (`header-node-burned`) or spends R0 while minting nothing under the queue
 * policy (`challenge-closed`). Any other hop continues from the one output
 * holding the node NFT.
 *
 * Only positive, verified, finalized evidence counts: every hop's spend comes
 * from {@link resolveDaAvailabilityForeignSpend} and must be at least
 * `minimumConfirmationDepth` deep. A missing or ambiguous derivation, a
 * missing spend, a shallow spend or a failed verification returns undefined;
 * a reader error or a spend above the boundary throws. Consuming the record
 * alone never releases: a Timeout with a descendant spends R0 and burns the
 * descendant's node, while the header's node continues.
 */
export const resolveDaAvailabilityWorkflowRelease = async (
  readers: DaAvailabilityForeignSpendReaders,
  openIntent: Pick<AvailabilityOperationRecord, "intent" | "state">,
  headerHash: string,
  minimumConfirmationDepth: number,
): Promise<DaAvailabilityWorkflowRelease | undefined> => {
  if (
    !Number.isSafeInteger(minimumConfirmationDepth) ||
    minimumConfirmationDepth <= 0
  )
    throw new Error("Invalid availability workflow release finality depth");
  const { intent } = openIntent;
  if (
    openIntent.state !== "confirmed" ||
    intent.action !== "open" ||
    intent.headerHash !== headerHash
  )
    return undefined;
  let open: CML.Transaction;
  try {
    open = CML.Transaction.from_cbor_hex(intent.signedCbor);
  } catch {
    return undefined;
  }
  const openBody = open.body();
  if (CML.hash_transaction(openBody).to_hex() !== intent.txHash)
    return undefined;
  const nodeAssetName = STATE_QUEUE_NODE_ASSET_NAME_PREFIX + headerHash;
  const openOutputs = transactionOutputs(openBody);
  const nodeHolders = openOutputs.flatMap((output, index) =>
    Object.entries(output.assets).flatMap(([unit, quantity]) =>
      unit.length === 56 + nodeAssetName.length &&
      unit.slice(56) === nodeAssetName &&
      quantity !== 0n
        ? [{ policy: unit.slice(0, 56), index, quantity }]
        : [],
    ),
  );
  if (nodeHolders.length !== 1 || nodeHolders[0]!.quantity !== 1n)
    return undefined;
  const queuePolicy = nodeHolders[0]!.policy;
  const nodeUnit = queuePolicy + nodeAssetName;
  const openMint = openBody.mint();
  const records = openOutputs.flatMap((output, index) => {
    if (typeof output.datum !== "string") return [];
    let record: DaAvailabilityChallengeRecord;
    try {
      record = parseDaAvailabilityChallengeRecordCbor(output.datum);
    } catch {
      return [];
    }
    if (record.commitment.header_hash !== headerHash) return [];
    const minted = Object.entries(output.assets).filter(
      ([unit, quantity]) =>
        unit.length === 56 + record.challenge_asset_name.length &&
        unit.slice(56) === record.challenge_asset_name &&
        quantity === 1n &&
        mintOf(openMint, unit.slice(0, 56), record.challenge_asset_name) === 1n,
    );
    return minted.length === 1 ? [index] : [];
  });
  if (records.length !== 1) return undefined;
  const recordOutRef = `${intent.txHash}#${records[0]!.toString()}`;
  let anchor = `${intent.txHash}#${nodeHolders[0]!.index.toString()}`;
  for (let hop = 0; hop < DA_AVAILABILITY_WORKFLOW_RELEASE_MAX_HOPS; hop++) {
    const spend = await resolveDaAvailabilityForeignSpend({
      ...readers,
      outRef: anchor,
    });
    if (
      spend === undefined ||
      spend.confirmationDepth < minimumConfirmationDepth
    )
      return undefined;
    const body = CML.Transaction.from_cbor_hex(
      spend.spendingTransactionCbor,
    ).body();
    const mint = body.mint();
    const evidence = {
      txHash: spend.spendingTxHash,
      spendPoint: spend.spendPoint,
      confirmationDepth: spend.confirmationDepth,
    };
    if (mintOf(mint, queuePolicy, nodeAssetName) === -1n)
      return { reason: "header-node-burned", ...evidence };
    if (
      transactionInputRefs(body).includes(recordOutRef) &&
      !mintsUnderPolicy(mint, queuePolicy)
    )
      return { reason: "challenge-closed", ...evidence };
    const next = outputsCarrying(transactionOutputs(body), nodeUnit);
    if (next.length !== 1) return undefined;
    anchor = `${spend.spendingTxHash}#${next[0]!.toString()}`;
  }
  throw new DaAvailabilityWorkflowReleaseHopCapError(headerHash);
};

/**
 * Provider adapter for a configured local canonical source. Boundary reads must
 * prove that the UTxO index and node share the same chain point. Inclusion depth
 * counts blocks, never elapsed slots. Source failures produce no mutation.
 */
export const createDaAvailabilityOperationObserver =
  (
    input: Readonly<{
      lucid: LucidEvolution;
      readBoundary: () => Promise<DaAvailabilityCanonicalBoundary>;
      /** Needed for providers whose transaction status omits block depth. */
      resolveInclusion?: (
        output: UTxO,
      ) => Promise<
        Readonly<{ slot?: number; blockHash?: string; depth?: number }>
      >;
      /**
       * The verified canonical spend of a missing normal input, or undefined
       * when there is none or it fails verification. Without it the observer
       * reports no `foreignSpends`.
       */
      resolveForeignSpend?: (
        outRef: string,
      ) => Promise<Omit<DaAvailabilityForeignSpend, "outRef"> | undefined>;
    }>,
  ): DaAvailabilityOperationContext["observe"] =>
  async (intent) => {
    const before = await input.readBoundary();
    if (
      !before.pointId ||
      !Number.isSafeInteger(before.slot) ||
      before.slot < 0
    ) {
      throw new Error(
        "Availability observer requires an aligned canonical boundary",
      );
    }
    const status = await input.lucid.transactionStatus(intent.txHash);
    let observation: DaAvailabilityOperationObservation;
    if (status.txHash !== intent.txHash)
      throw new Error(
        "Availability provider returned a foreign transaction status",
      );
    if (status.status === "confirmed") {
      const confirmation = status.confirmation;
      let point = {
        slot: confirmation.slot,
        blockHash: confirmation.blockHash,
        depth:
          confirmation.confirmations === undefined
            ? undefined
            : confirmation.confirmations - 1,
      };
      if (point.depth === undefined && input.resolveInclusion) {
        const body = CML.Transaction.from_cbor_hex(intent.signedCbor).body();
        if (body.outputs().len() === 0)
          throw new Error("Availability operation has no outputs");
        point = {
          ...point,
          ...(await input.resolveInclusion({
            ...coreToTxOutput(body.outputs().get(0)),
            txHash: intent.txHash,
            outputIndex: 0,
          })),
        };
      }
      observation =
        confirmation.txHash === intent.txHash &&
        typeof point.blockHash === "string" &&
        /^[0-9a-f]{64}$/u.test(point.blockHash) &&
        point.slot !== undefined &&
        Number.isSafeInteger(point.slot) &&
        point.slot >= 0 &&
        point.slot <= before.slot &&
        point.depth !== undefined &&
        Number.isSafeInteger(point.depth) &&
        point.depth >= 0
          ? {
              status: "included",
              txHash: intent.txHash,
              inclusionPoint: `${point.slot}:${point.blockHash}`,
              confirmationDepth: point.depth,
            }
          : {
              status: "unknown",
              reason: "Canonical inclusion depth is not available",
            };
    } else if (status.status === "pending") {
      observation = {
        status: "unknown",
        reason: "Signed availability transaction is pending",
      };
    } else {
      const refs = [...intent.spentOutRefs, ...intent.collateralOutRefs];
      const available = await input.lucid.utxosByOutRef(
        refs.map((ref) => {
          const [txHash, outputIndex] = ref.split("#");
          return { txHash: txHash!, outputIndex: Number(outputIndex) };
        }),
      );
      const keys = new Set(
        available.map((utxo) => `${utxo.txHash}#${utxo.outputIndex}`),
      );
      const missingOutRefs = refs.filter((ref) => !keys.has(ref));
      const foreignSpends: DaAvailabilityForeignSpend[] = [];
      if (input.resolveForeignSpend)
        for (const ref of intent.spentOutRefs) {
          if (keys.has(ref)) continue;
          const spend = await input.resolveForeignSpend(ref);
          if (spend)
            foreignSpends.push({
              outRef: ref,
              spendingTxHash: spend.spendingTxHash,
              spendPoint: spend.spendPoint,
              confirmationDepth: spend.confirmationDepth,
            });
        }
      observation =
        missingOutRefs.length === 0
          ? { status: "unspent", currentSlot: before.slot }
          : {
              status: "inputs_missing",
              currentSlot: before.slot,
              missingOutRefs,
              ...(foreignSpends.length === 0 ? {} : { foreignSpends }),
            };
    }
    const after = await input.readBoundary();
    if (before.pointId !== after.pointId || before.slot !== after.slot) {
      return {
        status: "unknown",
        reason: "Canonical source changed during availability reconciliation",
      };
    }
    return observation;
  };

export type DaAvailabilityOperationBuild = Readonly<{
  tx: TxSignBuilder;
  /** Timeout only: `min(penalty, taken)`, the slashed share of the fee. */
  timeoutFeePartLovelace?: bigint;
}>;

/** Reconciles existing actor intent before constructing any new transaction. */
export const runDaAvailabilityOperation = (
  context: DaAvailabilityOperationContext,
  operation: Readonly<{
    headerHash: string;
    action: AvailabilityOperationIntent["action"];
    /** Timeout is terminal only when it removes the challenged head itself. */
    completesWorkflow?: boolean;
    /**
     * The unsigned transaction, or a built one carrying its slashed fee part
     * (a `BuiltDaAvailabilityTransaction` qualifies). A timeout must return
     * its fee part: its fee ceiling caps only `fee - feePart`.
     */
    build: () => Promise<TxSignBuilder | DaAvailabilityOperationBuild>;
  }>,
): Promise<DaAvailabilityOperationResult> =>
  withLease(context, async (lease, assertCurrent) => {
    const pending = context.journal.pending(
      context.deploymentIdentity,
      context.actor,
    );
    if (pending.length > 0)
      return reconcile(context, lease, pending[0]!.intent, assertCurrent);
    for (const anchor of context.journal.finalizedAnchors(
      context.deploymentIdentity,
      context.actor,
    )) {
      const audit = await reconcile(
        context,
        lease,
        anchor.intent,
        assertCurrent,
      );
      if (audit.status !== "confirmed") return audit;
    }
    context.journal.assertWorkflow(
      lease,
      context.deploymentIdentity,
      operation.headerHash,
      operation.action,
      (context.nowMs ?? Date.now)(),
    );
    const built = await operation.build();
    const { tx, timeoutFeePartLovelace } =
      "toTransaction" in built
        ? { tx: built, timeoutFeePartLovelace: undefined }
        : built;
    if (operation.action === "timeout" && timeoutFeePartLovelace === undefined)
      throw new Error(
        "A timeout operation must report its slashed fee part to be signed",
      );
    await assertCurrent();
    const unsignedHash = CML.hash_transaction(
      tx.toTransaction().body(),
    ).to_hex();
    const signed = await tx.sign.withWallet().complete();
    await assertCurrent();
    const intent = inspectDaAvailabilitySignedIntent({
      deploymentIdentity: context.deploymentIdentity,
      actor: context.actor,
      headerHash: operation.headerHash,
      action: operation.action,
      signedCbor: signed.toCBOR(),
      completesWorkflow: operation.completesWorkflow,
    });
    if (intent.txHash !== unsignedHash)
      throw new Error("Signing changed the availability transaction body");
    assertTerminalIntent(context, intent);
    assertDaAvailabilitySignedLimits(
      intent,
      context.transactionLimits,
      timeoutFeePartLovelace,
    );
    const existing = context.journal.get(intent.id);
    if (existing?.state === "included" || existing?.state === "confirmed")
      return reconcile(context, lease, intent, assertCurrent);
    context.journal.persist(lease, intent, (context.nowMs ?? Date.now)());
    return reconcile(context, lease, intent, assertCurrent);
  });
