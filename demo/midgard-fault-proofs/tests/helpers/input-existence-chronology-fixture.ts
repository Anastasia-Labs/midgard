/** Small canonical source/event chronology. Roots are computed from actual
 * full outputs; no validation-machine acceptance is claimed. A withdrawal
 * scenario commits the caller's withdrawal event, root entry and ledger
 * effect exactly as given; it builds none of them itself. */
import {
  computeHash28,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  encodeMidgardSpendInputItem,
  hashMidgardValidationRejectionReasonV1,
} from "@al-ft/midgard-core";
import {
  deriveMidgardForcedTxProofSource,
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec/forced";
import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  plutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import type { CanonicalTransitionEffect } from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../../src/evidence/canonical-block-evidence.js";
import {
  checkLedgerAgainstRoot,
  ledgerEntriesRoot,
  type LedgerEntry,
} from "../../src/ledger-at.js";
import { buildCountedRoot } from "../../src/transition-trace/phas.js";
import { admitCompleteCanonicalReplayPredecessor } from "../../src/workflow/complete-replay.js";
import { fixtureBlockSlot } from "../support/emulator/header-fixtures.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  type FixtureTransaction,
  outRefCbor,
  reencodeFixturePayload,
} from "./canonical-block-evidence-fixture.js";
import {
  CHRONOLOGY_SEED_VALUE,
  chronologyDeposit,
  chronologyDepositEntry,
  chronologySeedEntries,
  signedChronologyTransaction,
} from "./input-existence-chronology-seed.js";

export const inputChronologies = [
  "forced_creator",
  "normal_creator",
  "later_creator",
  "forced_spend",
  "normal_spend",
  "withdrawal_consumed",
  "withdrawal_untouched",
  "rejected_creator",
  "forced_target",
  "prior_ledger_present_rejection",
  // A deposit of this block creates the input; deposits apply after every
  // transaction event.
  "same_block_deposit",
  // The input is a prior block's deposit output, still unspent.
  "prior_deposit",
  "forced_prior_deposit",
  // A forced rejection for an input that is absent at its pre-root.
  "absent_input_rejection",
] as const;
export type InputChronology = (typeof inputChronologies)[number];
/** A committed withdrawal: its event key, withdrawals-root entry and ledger
 * effect, each as block production derives it. */
export type CommittedChronologyWithdrawal = Readonly<{
  eventKey: SDK.EventKey;
  rootKey: Buffer;
  rootValue: Buffer;
  effect: CanonicalTransitionEffect;
}>;
/** The L1 transaction-id byte of the same-block deposit. The seed deposits
 * use 81-83, so the deposit's output is absent from the predecessor ledger. */
const SAME_BLOCK_DEPOSIT_BYTE = 84;
const PROVENANCE = {
  trustClass: "public_or_permissionless_da",
  sourceId: "test/input-event-chronology",
  grade: "security",
} as const;

export const inputExistenceChronologyFixture = async (
  scenario: InputChronology,
  field: 0 | 1,
  executionHeader?: Readonly<{
    operatorVkey: string;
    startTime: bigint;
    predecessorHeaderHash: string;
  }>,
  withdrawal?: CommittedChronologyWithdrawal,
) => {
  const seed = outRefCbor(81, 0n);
  const feeSeed = outRefCbor(82, 0n);
  const untouchedSeed = outRefCbor(83, 0n);
  const predecessorEntries = chronologySeedEntries();
  const seedEntry = predecessorEntries.find((entry) =>
    entry.outRef.equals(seed),
  );
  if (seedEntry === undefined)
    throw new Error("chronology seed deposit absent");
  const output = seedEntry.output;
  const predecessorFixture = await buildCanonicalBlockFixture({
    transactions: [],
    utxos: predecessorEntries.map(({ outRef, output }) => ({
      key: outRef,
      value: output,
    })),
  });
  const creator = signedChronologyTransaction({
    spendInputs: [seed],
    outputs: [output],
    fee: 0n,
  });
  const created = encodeMidgardSpendInputItem({
    txId: Buffer.from(creator.txId, "hex"),
    outputIndex: 0,
  });
  const usesCreated = scenario.endsWith("creator");
  const deposit = chronologyDeposit(SAME_BLOCK_DEPOSIT_BYTE);
  const selected = usesCreated
    ? created
    : scenario === "same_block_deposit"
      ? outRefCbor(SAME_BLOCK_DEPOSIT_BYTE, 0n)
      : scenario === "forced_target" || scenario === "absent_input_rejection"
        ? outRefCbor(82, 1n)
        : scenario === "withdrawal_untouched"
          ? untouchedSeed
          : seed;
  const target = signedChronologyTransaction({
    spendInputs: field === 0 ? [selected] : [feeSeed],
    referenceInputs: field === 1 ? [selected] : [],
    fee: CHRONOLOGY_SEED_VALUE,
  });
  const consumer = signedChronologyTransaction({
    spendInputs: [seed],
    fee: CHRONOLOGY_SEED_VALUE,
    validityIntervalStart: 0n,
  });
  const normals: FixtureTransaction[] = [];
  const forced: SDK.DaPayloadEntry[] = [];
  const forcedPreimages: SDK.DaPayloadEntry[] = [];
  const withdrawals: SDK.DaPayloadEntry[] = [];
  const deposits: SDK.DaPayloadEntry[] = [];
  const trace: SDK.DaPayloadEntry[] = [];
  const events: SDK.DaPayloadEntry[] = [];
  const validations: SDK.DaPayloadEntry[] = [];
  const entries = new Map(
    predecessorEntries.map((entry) => [entry.outRef.toString("hex"), entry]),
  );
  let targetEvent: SDK.EventKey | undefined;
  let targetStep = -1n;
  let targetPreRoot = "";
  let targetPostRoot = "";
  let targetPostEntries: readonly LedgerEntry[] = [];
  const append = async (
    event: SDK.EventKey,
    phase: SDK.TransitionPhase,
    effect: () => void,
    rejected = false,
    isTarget = false,
    rejectionReason: SDK.RejectionReason = "EmptyInputs",
  ) => {
    const index = BigInt(trace.length);
    const before = await ledgerEntriesRoot([...entries.values()]);
    effect();
    const after = await ledgerEntriesRoot([...entries.values()]);
    trace.push([
      Data.to(index),
      Data.to(
        {
          schema_version: 1n,
          step_index: index,
          event_key: event,
          phase,
          pre_utxos_root: before,
          post_utxos_root: after,
        },
        SDK.TransitionStep,
      ),
    ]);
    events.push([
      Data.to(event, SDK.EventKey),
      Data.to({ step_index: index, phase }, SDK.EventToStepValue),
    ]);
    if (phase === "L2Transaction" || phase === "ForcedTransaction")
      validations.push([
        Data.to(event, SDK.EventKey),
        Data.to(
          {
            schema_version: 1n,
            machine_version: 1n,
            trace_root: "91".repeat(32),
            step_count: 1n,
            initial_state_hash: "92".repeat(32),
            terminal_state_hash: "93".repeat(32),
            verdict: rejected ? "Rejected" : "Accepted",
            rejection_code_hash: rejected
              ? hashMidgardValidationRejectionReasonV1(
                  rejectionReason,
                ).toString("hex")
              : "00".repeat(32),
          },
          SDK.ValidationTraceDescriptor,
        ),
      ]);
    if (isTarget) {
      targetEvent = event;
      targetStep = index;
      targetPreRoot = before;
      targetPostRoot = after;
      targetPostEntries = [...entries.values()];
    }
  };
  const apply = (
    tx: FixtureTransaction,
    spend: readonly Buffer[],
    outputs: readonly Buffer[],
  ) => {
    for (const key of spend) entries.delete(key.toString("hex"));
    for (const [index, output] of outputs.entries()) {
      const key = encodeMidgardSpendInputItem({
        txId: Buffer.from(tx.txId, "hex"),
        outputIndex: index,
      });
      entries.set(key.toString("hex"), { outRef: key, output });
    }
  };
  const normal = async (
    tx: FixtureTransaction,
    spend: readonly Buffer[],
    outputs: readonly Buffer[],
    isTarget = false,
  ) => {
    normals.push(tx);
    await append(
      { L2TransactionEventKey: { tx_id: tx.txId } },
      "L2Transaction",
      () => apply(tx, spend, outputs),
      false,
      isTarget,
    );
  };
  const forcedTx = async (
    tx: FixtureTransaction,
    spend: readonly Buffer[],
    outputs: readonly Buffer[],
    rejected = false,
    isTarget = false,
    rejectionReason: SDK.RejectionReason = "EmptyInputs",
  ) => {
    const full = materializeMidgardForcedTxFromCanonical(
      decodeMidgardNativeTxFullFromCanonicalCbor(tx.canonicalCbor),
    );
    const source = deriveMidgardForcedTxProofSource(full);
    const key: SDK.OutputReference = {
      transactionId: "a1".repeat(32),
      outputIndex: 0n,
    };
    const leaf: SDK.ForcedInclusionTxV1 = {
      tx_id: tx.txId,
      submitted_source: {
        compact_cbor: source.compactCbor.toString("hex"),
        witness_set_compact_cbor: source.witnessSetCompactCbor.toString("hex"),
        field_preimage_lengths_cbor:
          source.fieldPreimageLengthsCbor.toString("hex"),
      },
      verdict: rejected
        ? { ForcedTxInvalid: { reason: rejectionReason } }
        : "ForcedTxValid",
    };
    const keyCbor = Data.to(key, SDK.OutputReference);
    forced.push([keyCbor, Data.to(leaf, SDK.ForcedInclusionTxV1)]);
    forcedPreimages.push([
      keyCbor,
      encodeMidgardForcedTxCanonical(full).toString("hex"),
    ]);
    await append(
      { ForcedTransactionEventKey: { tx_order_id: key } },
      "ForcedTransaction",
      () => {
        if (!rejected) apply(tx, spend, outputs);
      },
      rejected,
      isTarget,
      rejectionReason,
    );
  };
  if (scenario === "forced_creator" || scenario === "rejected_creator")
    await forcedTx(creator, [seed], [output], scenario === "rejected_creator");
  if (scenario === "forced_spend") await forcedTx(consumer, [seed], []);
  if (scenario === "normal_creator") await normal(creator, [seed], [output]);
  if (scenario === "normal_spend") await normal(consumer, [seed], []);
  if (
    scenario === "withdrawal_consumed" ||
    scenario === "withdrawal_untouched"
  ) {
    if (withdrawal === undefined)
      throw new Error("withdrawal chronology needs a committed withdrawal");
    withdrawals.push([
      withdrawal.rootKey.toString("hex"),
      withdrawal.rootValue.toString("hex"),
    ]);
    await append(withdrawal.eventKey, "Withdrawal", () => {
      for (const operation of withdrawal.effect.operations) {
        const key = operation.outRefCbor.toString("hex");
        if (operation.type === "delete") entries.delete(key);
        else
          entries.set(key, {
            outRef: Buffer.from(operation.outRefCbor),
            output: Buffer.from(operation.outputCbor),
          });
      }
    });
  }
  if (
    scenario === "prior_ledger_present_rejection" ||
    scenario === "absent_input_rejection"
  )
    await forcedTx(target, [], [], true, true, {
      InputNotFound: { source_kind: BigInt(field), input_index: 0n },
    });
  else if (scenario === "forced_target" || scenario === "forced_prior_deposit")
    await forcedTx(
      target,
      field === 0 ? [selected] : [feeSeed],
      [],
      false,
      true,
    );
  else await normal(target, field === 0 ? [selected] : [feeSeed], [], true);
  if (scenario === "later_creator") await normal(creator, [seed], [output]);
  if (scenario === "same_block_deposit") {
    const { id } = deposit.event;
    const eventCbor = aikenSerialisedPlutusDataCborPreservingMapOrder(
      Data.to(deposit.event, SDK.DepositEvent),
    );
    deposits.push([
      SDK.committedDepositKeyBytes(id),
      plutusConstrFieldCbor(eventCbor, [1]),
    ]);
    const entry = chronologyDepositEntry(deposit);
    await append({ DepositEventKey: { deposit_id: id } }, "Deposit", () => {
      entries.set(entry.outRef.toString("hex"), entry);
    });
  }
  // DA root entries are serialized by canonical key, independently of the
  // transition step_index that authenticates application order.
  for (const rows of [
    deposits,
    forced,
    forcedPreimages,
    withdrawals,
    trace,
    events,
    validations,
  ]) {
    rows.sort(([left], [right]) => (left < right ? -1 : left > right ? 1 : 0));
  }
  const base = await buildCanonicalBlockFixture({
    transactions: normals,
    prevHeaderHash:
      executionHeader?.predecessorHeaderHash ?? predecessorFixture.headerHash,
    prevUtxosRoot: predecessorFixture.header.utxosRoot,
  });
  const root = async (domain: SDK.RootDomain, rows: SDK.DaPayloadEntry[]) =>
    (
      await buildCountedRoot(
        domain,
        rows.map(([key, value]) => ({
          key: Buffer.from(key, "hex"),
          value: Buffer.from(value, "hex"),
        })),
      )
    ).root;
  const counts = {
    depositCount: BigInt(deposits.length),
    withdrawalCount: BigInt(withdrawals.length),
    forcedTransactionCount: BigInt(forced.length),
    totalEventCount: BigInt(trace.length),
    transitionStepCount: BigInt(trace.length),
    validationTraceCount: BigInt(validations.length),
  };
  const header: SDK.Header = {
    ...base.header,
    ...(executionHeader === undefined
      ? {}
      : {
          operatorVkey: executionHeader.operatorVkey,
          startTime: executionHeader.startTime,
          endTime: executionHeader.startTime + 30_000n,
          blockSlot: fixtureBlockSlot(executionHeader.startTime + 30_000n),
        }),
    depositCount: counts.depositCount,
    withdrawalCount: counts.withdrawalCount,
    forcedTransactionCount: counts.forcedTransactionCount,
    totalEventCount: counts.totalEventCount,
    transitionStepCount: counts.transitionStepCount,
    // The header pins the validation-trace pair to the empty tree; the body
    // keeps the built members and their count.
    validationTraceCount: 0n,
    utxosRoot: await ledgerEntriesRoot([...entries.values()]),
    depositsRoot: await root(SDK.ROOT_DOMAINS.deposits, deposits),
    withdrawalsRoot: await root(SDK.ROOT_DOMAINS.withdrawals, withdrawals),
    forcedTransactionsRoot: await root(
      SDK.ROOT_DOMAINS.forcedTransactionsV1,
      forced,
    ),
    transitionTraceRoot: await root(SDK.ROOT_DOMAINS.transitionTrace, trace),
    eventToStepRoot: await root(SDK.ROOT_DOMAINS.eventToStep, events),
    validationTracesRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  };
  const headerHash = computeHash28(SDK.encodeHeaderCbor(header)).toString(
    "hex",
  );
  const body = base.payload.block_body;
  const payload: SDK.DaPayload = {
    ...base.payload,
    block_body: {
      ...body,
      header,
      header_hash: headerHash,
      deposits,
      withdrawals,
      forced_transactions: forced,
      forced_transaction_preimages: forcedPreimages,
      transition_trace: trace,
      event_to_step: events,
      validation_traces: validations,
      counts: {
        ...body.counts,
        depositCount: header.depositCount,
        withdrawalCount: header.withdrawalCount,
        forcedTransactionCount: header.forcedTransactionCount,
        totalEventCount: header.totalEventCount,
        transitionStepCount: header.transitionStepCount,
        validationTraceCount: counts.validationTraceCount,
      },
    },
  };
  const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
    observation: authenticatedHeaderObservation({ header, headerHash }),
    payloadEnvelopeCbor: await reencodeFixturePayload(payload),
    daProvenance: PROVENANCE,
  });
  const predecessor = await checkLedgerAgainstRoot({
    entries: predecessorEntries,
    utxosRoot: header.prevUtxosRoot,
    label: "chronology predecessor",
  });
  const context = {
    predecessor: await admitCompleteCanonicalReplayPredecessor({
      currentEvidence: evidence,
      ledgerAt: async (hash) =>
        hash === header.prevHeaderHash ? predecessorEntries : undefined,
    }),
  };
  if (targetEvent === undefined)
    throw new Error("chronology target event absent");
  return {
    evidence,
    context,
    predecessor,
    targetEvent,
    targetStep,
    targetPreRoot,
    targetPostRoot,
    targetPostEntries,
    target,
    selected,
    creator,
    predecessorEntries,
  };
};
