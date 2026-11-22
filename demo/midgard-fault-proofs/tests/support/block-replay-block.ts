/**
 * Small blocks for the block replay: withdrawals and deposits (user events
 * whose L1 material the fixture supplies), forced and L2 transactions signed
 * by one fixed key, so every transaction id, output and root is the same on
 * every run. The committed roots are computed with the validation machine's
 * own ledger root over the whole ledger, and each transaction's committed
 * verdict and state patch come from the validation machine replaying it over
 * the whole ledger, as the replay did before it read only what a transaction
 * names.
 */
import {
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import * as SDK from "@al-ft/midgard-sdk/core";
import * as SDKDa from "@al-ft/midgard-sdk/da";
import {
  FUNDED_OUTPUT_LOVELACE,
  makeNativeTx,
  makeOutput,
  outRefFromByte,
  outRefFromTxId,
} from "@al-ft/midgard-validation/tests/validation-fixtures";
import {
  placeholderValidationTraceDescriptorV1,
  replayValidationVerdictEvent,
  validationMachineLedgerRoot,
  validationTransactionCommitmentV1,
} from "@al-ft/midgard-validation/validation-machine";
import { CML, Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  encodeEventToStepValueCbor,
  encodeTransactionRootValue,
  encodeTransitionIntegerCbor,
  encodeTransitionStepCbor,
} from "../../src/block-replay.js";
import { forcedVerdictForRejection } from "../../src/workflow/forced-rejection-reason.js";

export type LedgerEntry = Readonly<{ outRef: Buffer; output: Buffer }>;

const KEY = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 0x42));

/** The address the fixture key signs for. */
export const FIXTURE_ADDRESS = Buffer.from(
  CML.EnterpriseAddress.new(
    0,
    CML.Credential.new_pub_key(KEY.to_public().hash()),
  )
    .to_address()
    .to_raw_bytes(),
);

/** An output of `lovelace` at the fixture address. */
export const fixtureOutput = (lovelace = FUNDED_OUTPUT_LOVELACE): Buffer =>
  makeOutput(lovelace, FIXTURE_ADDRESS);

/** A parent entry at the fixture address, keyed by `byte`. */
export const fundedEntry = (byte: number): LedgerEntry => ({
  outRef: outRefFromByte(byte),
  output: fixtureOutput(),
});

/** One event of a fixture block, in the block's canonical phase order. */
export type FixtureEvent =
  | { readonly phase: "Withdrawal"; readonly outRef: Buffer }
  | {
      readonly phase: "ForcedTransaction" | "L2Transaction";
      readonly spend: readonly Buffer[];
      readonly references?: readonly Buffer[];
      /** Lovelace of the one output (default: the spent inputs' total). */
      readonly lovelace?: bigint;
    }
  | {
      readonly phase: "Deposit";
      readonly outRef: Buffer;
      readonly output: Buffer;
    };

const transactionOf = (
  event: Extract<FixtureEvent, { spend: readonly Buffer[] }>,
) => {
  const tx = makeNativeTx({
    // Input sets are canonical in ascending key order.
    spendInputs: [...event.spend].sort(Buffer.compare),
    referenceInputs: [...(event.references ?? [])].sort(Buffer.compare),
    outputs: [
      fixtureOutput(
        event.lovelace ?? FUNDED_OUTPUT_LOVELACE * BigInt(event.spend.length),
      ),
    ],
    privateKey: KEY,
  });
  if (event.phase === "L2Transaction")
    return { txId: tx.txId, cbor: tx.txCbor };
  const cbor = encodeMidgardForcedTxCanonical(
    materializeMidgardForcedTxFromCanonical({
      version: tx.tx.version,
      body: tx.tx.body,
      witnessSet: tx.tx.witnessSet,
    }),
  );
  return { txId: tx.txId, cbor };
};

/** The out-ref of the one output a fixture transaction produces. */
export const producedOutRef = (
  event: Extract<FixtureEvent, { spend: readonly Buffer[] }>,
): Buffer => outRefFromTxId(transactionOf(event).txId, 0n);

const ledgerRoot = async (ledger: ReadonlyMap<string, Buffer>) =>
  (
    await validationMachineLedgerRoot(
      [...ledger].map(([key, output]) => ({
        outRef: Buffer.from(key, "hex"),
        output,
      })),
    )
  ).toString("hex");

const eventId = (index: number) => ({
  transactionId: (0xa0 + index).toString(16).padStart(2, "0").repeat(32),
  outputIndex: BigInt(index),
});

/**
 * A block applying `events` in order on `parent`. A withdrawal deletes its
 * out-ref and a deposit inserts its entry when their L1 material is
 * replayed; a withdrawal of an absent out-ref and a deposit over a present
 * one are carried as given (the replay refuses them). Returns the payload,
 * the root before the block and after each event, each transaction's
 * verdict, the ledger after the block, every out-ref an event names (its
 * inputs, reference inputs and outputs, or its user event's out-ref), and
 * the block's L1 material.
 */
export const fixtureBlock = async (
  parent: readonly LedgerEntry[],
  events: readonly FixtureEvent[],
) => {
  const ledger = new Map(
    parent.map((entry) => [entry.outRef.toString("hex"), entry.output]),
  );
  const parentRoot = await ledgerRoot(ledger);
  const roots = [parentRoot];
  const verdicts: ("accepted" | "rejected" | undefined)[] = [];
  const named = new Set<string>();
  const userEvents = new Map<string, { key: string; output: Buffer | null }>();
  const body = {
    transactions: [] as [string, string][],
    transaction_preimages: [] as [string, string][],
    withdrawals: [] as [string, string][],
    forced_transactions: [] as [string, string][],
    forced_transaction_preimages: [] as [string, string][],
    deposits: [] as [string, string][],
    transition_trace: [] as [string, string][],
    event_to_step: [] as [string, string][],
    validation_traces: [] as [string, string][],
  };
  for (const [index, event] of events.entries()) {
    const id = eventId(index);
    const idCbor = Data.to(id, SDK.OutputReference);
    let eventKey: SDK.EventKey;
    if (event.phase === "Withdrawal" || event.phase === "Deposit") {
      const key = event.outRef.toString("hex");
      named.add(key);
      verdicts.push(undefined);
      if (event.phase === "Withdrawal") {
        eventKey = { WithdrawalEventKey: { withdrawal_id: id } };
        body.withdrawals.push([idCbor, "00"]);
        userEvents.set(idCbor, { key, output: null });
        ledger.delete(key);
      } else {
        eventKey = { DepositEventKey: { deposit_id: id } };
        body.deposits.push([idCbor, "00"]);
        userEvents.set(idCbor, { key, output: event.output });
        ledger.set(key, event.output);
      }
    } else {
      const { txId, cbor } = transactionOf(event);
      for (const outRef of [...event.spend, ...(event.references ?? [])])
        named.add(outRef.toString("hex"));
      named.add(outRefFromTxId(txId, 0n).toString("hex"));
      const sourceKind = event.phase === "L2Transaction" ? "normal" : "forced";
      const replay = await Effect.runPromise(
        replayValidationVerdictEvent({
          l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
          consensusProfile: MIDGARD_CONSENSUS_PROFILE,
          canonicalTransactionCbor: cbor,
          ledgerWitnessEntries: [...ledger].map(([key, output]) => ({
            outRef: Buffer.from(key, "hex"),
            output,
          })),
          blockTimeMs: 2,
          expectedNetworkId: 0n,
          minFeeA: 0n,
          minFeeB: 0n,
          blockSlot: 0n,
          sourceKind,
        }),
      );
      verdicts.push(replay.verdict);
      for (const key of replay.statePatch.deletedOutRefs) ledger.delete(key);
      for (const [key, output] of replay.statePatch.upsertedOutRefs)
        ledger.set(key, output);
      if (event.phase === "L2Transaction") {
        eventKey = { L2TransactionEventKey: { tx_id: txId.toString("hex") } };
        body.transactions.push([
          txId.toString("hex"),
          encodeTransactionRootValue(cbor).toString("hex"),
        ]);
        body.transaction_preimages.push([
          txId.toString("hex"),
          cbor.toString("hex"),
        ]);
      } else {
        eventKey = { ForcedTransactionEventKey: { tx_order_id: id } };
        body.forced_transactions.push([idCbor, "00"]);
        body.forced_transaction_preimages.push([idCbor, cbor.toString("hex")]);
      }
      const eventKeyCbor = Data.to(eventKey, SDK.EventKey);
      const reason =
        replay.rejection === undefined
          ? null
          : (
              forcedVerdictForRejection(replay.rejection) as {
                ForcedTxInvalid: { reason: unknown };
              }
            ).ForcedTxInvalid.reason;
      body.validation_traces.push([
        eventKeyCbor,
        Data.to(
          SDKDa.validationTraceDescriptorDataFromCore(
            placeholderValidationTraceDescriptorV1(
              {
                eventKeyCbor: Buffer.from(eventKeyCbor, "hex"),
                transactionCommitment: validationTransactionCommitmentV1({
                  canonicalTransactionCbor: cbor,
                  sourceKind,
                }),
              },
              replay.verdict,
              reason as Parameters<
                typeof placeholderValidationTraceDescriptorV1
              >[2],
            ),
          ),
          SDK.ValidationTraceDescriptor,
        ),
      ]);
    }
    const post = await ledgerRoot(ledger);
    body.transition_trace.push([
      encodeTransitionIntegerCbor(BigInt(index)).toString("hex"),
      encodeTransitionStepCbor({
        schema_version: 1n,
        step_index: BigInt(index),
        event_key: eventKey,
        phase: event.phase,
        pre_utxos_root: roots[roots.length - 1]!,
        post_utxos_root: post,
      }).toString("hex"),
    ]);
    body.event_to_step.push([
      Data.to(eventKey, SDK.EventKey),
      encodeEventToStepValueCbor({
        step_index: BigInt(index),
        phase: event.phase,
      }).toString("hex"),
    ]);
    roots.push(post);
  }
  const header = {
    prevUtxosRoot: parentRoot,
    utxosRoot: roots[roots.length - 1]!,
  } as SDK.Header;
  const payload = {
    version: 1n,
    block_body: {
      header,
      header_hash: "00".repeat(28),
      ...body,
      validation_trace_witnesses: [],
    },
  } as unknown as SDKDa.DaPayload;
  return {
    payload,
    roots,
    verdicts,
    ledger: [...ledger].map(([key, output]) => ({
      outRef: Buffer.from(key, "hex"),
      output,
    })),
    named,
    material: {
      replayUserEvent: ({
        source,
      }: {
        readonly source: SDKDa.DaPayloadEntry;
      }) =>
        Effect.sync(() => {
          const mutation = userEvents.get(source[0]);
          if (mutation === undefined)
            throw new Error("the fixture has no such user event");
          return [mutation];
        }),
      verifyForcedSource: () => Effect.void,
    },
  };
};

/** The replay settings every fixture block is built for. */
export const FIXTURE_REPLAY_SETTINGS = {
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  blockSlot: 0n,
  blockTimeMs: 2,
  l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
} as const;
