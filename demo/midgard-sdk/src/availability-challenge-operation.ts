import { createHash, randomUUID } from "node:crypto";

import type {
  AvailabilityOperationIntent,
  AvailabilityOperationJournal,
  AvailabilityOperationLease,
} from "@al-ft/midgard-core/availability-operation-journal";
import {
  calculateMinLovelaceFromUTxO,
  CML,
  coreToTxOutput,
  type LucidEvolution,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";

import type { DaAvailabilityParameters } from "./availability-challenge.js";
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
    }>
  | Readonly<{ status: "unknown"; reason: string }>
  | Readonly<{ status: "conflicting_spend"; reason: string }>;

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
  feeCeilings: Readonly<Record<string, bigint>>;
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
  };
};

const assertSignedLimits = (
  intent: AvailabilityOperationIntent,
  limits: DaAvailabilityOperationLimits,
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
  if (
    !Number.isSafeInteger(limits.maxTxSize) ||
    limits.maxTxSize <= 0 ||
    limits.maxTxExMem <= 0n ||
    limits.maxTxExSteps <= 0n ||
    intent.signedCbor.length / 2 > limits.maxTxSize ||
    memory * 5n > limits.maxTxExMem * 4n ||
    steps * 5n > limits.maxTxExSteps * 4n ||
    ceiling === undefined ||
    body.fee() > ceiling ||
    body.fee() <= 0n ||
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

/** The deployed Open predicate requires an exact bond-plus-fee input. */
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
      if (
        !Number.isSafeInteger(observation.currentSlot) ||
        observation.currentSlot < 0 ||
        observation.missingOutRefs.length === 0 ||
        observation.missingOutRefs.some(
          (ref) =>
            !intent.spentOutRefs.includes(ref) &&
            !intent.collateralOutRefs.includes(ref),
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
      assertSignedLimits(intent, context.transactionLimits);
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
      observation = refs.every((ref) => keys.has(ref))
        ? { status: "unspent", currentSlot: before.slot }
        : {
            status: "inputs_missing",
            currentSlot: before.slot,
            missingOutRefs: refs.filter((ref) => !keys.has(ref)),
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

/** Reconciles existing actor intent before constructing any new transaction. */
export const runDaAvailabilityOperation = (
  context: DaAvailabilityOperationContext,
  operation: Readonly<{
    headerHash: string;
    action: AvailabilityOperationIntent["action"];
    /** Timeout is terminal only when it removes the challenged head itself. */
    completesWorkflow?: boolean;
    build: () => Promise<TxSignBuilder>;
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
    const tx = await operation.build();
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
    assertSignedLimits(intent, context.transactionLimits);
    const existing = context.journal.get(intent.id);
    if (existing?.state === "included" || existing?.state === "confirmed")
      return reconcile(context, lease, intent, assertCurrent);
    context.journal.persist(lease, intent, (context.nowMs ?? Date.now)());
    return reconcile(context, lease, intent, assertCurrent);
  });
