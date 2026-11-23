/** The ledger before one event of a block, rebuilt from scratch:
 * the predecessor's entries, each earlier step's effects applied to a plain
 * map, and every intermediate ledger root-checked with a fresh PHAS trie of
 * its entries, independent of the ledger store. A
 * rejected normal transaction changes nothing. It refuses in the order, and
 * at the step, an event-ledger walk must, so a walk's result for any
 * request, in any request order, can be compared with it. */
import { Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  decodeMidgardFieldPreimage,
  decodeMidgardForcedTxFullFromCanonicalCbor,
  encodeMidgardSpendInputItem,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk/core";
import { Data } from "@lucid-evolution/lucid";

import {
  EventLedgerOutputNotCanonical,
  EventLedgerReplayRefusal,
} from "../../src/event-ledger.steps.js";
import type { CanonicalBlockEvidence } from "../../src/evidence/canonical-block-evidence.js";
import { type LedgerEntry, ledgerEntryLeaf } from "../../src/ledger-at.js";
import type { Ledger } from "../../src/ledger-store/index.js";
import {
  type KeyValuePhasRoot,
  keyValuePhasRootWithCount,
} from "../../src/transition-trace/phas.js";
import {
  eventKeyFingerprint,
  eventKeyPhase,
} from "../../src/transition-trace/reconstruct.transition-trace-reconstruction.js";
import { requireReplayPredecessorLedger } from "../../src/workflow/replay-predecessor-ledger.js";

const rank = (phase: SDK.TransitionPhase): number =>
  ["Withdrawal", "ForcedTransaction", "L2Transaction", "Deposit"].indexOf(
    phase,
  );

const refuse = (
  reason: EventLedgerReplayRefusal["reason"],
  step: bigint,
): never => {
  throw new EventLedgerReplayRefusal(reason, step);
};

/** A ledger as the reference rebuilds it: its root, its entries, its
 * outputs by out-ref hex and a PHAS trie of them. */
export type ReferenceLedger = Readonly<{
  utxosRoot: string;
  entries: readonly LedgerEntry[];
  outputs: ReadonlyMap<string, Buffer>;
  trie: KeyValuePhasRoot;
}>;

/** `entries` with a fresh trie of their descriptors; a non-canonical output
 * throws. */
const referenceLedger = async (
  entries: readonly LedgerEntry[],
): Promise<ReferenceLedger> => {
  const trie = await keyValuePhasRootWithCount(
    entries.map((entry) => ({
      key: entry.outRef,
      value: ledgerEntryLeaf(entry, "reference ledger").leaf,
    })),
  );
  return {
    utxosRoot: trie.root,
    entries,
    outputs: new Map(
      entries.map(({ outRef, output }) => [outRef.toString("hex"), output]),
    ),
    trie,
  };
};

const listed = async (ledger: Ledger): Promise<LedgerEntry[]> => {
  const entries: LedgerEntry[] = [];
  for await (const entry of ledger.entries()) entries.push(entry);
  return entries;
};

/** Every entry of `ledger`, rebuilt as the reference holds a ledger. */
export const referenceLedgerOf = async (
  ledger: Ledger,
): Promise<ReferenceLedger> => await referenceLedger(await listed(ledger));

/** Each key's MPF proof CBOR against `ledger`'s trie, built by the MPF
 * library from the ledger's entries: membership for a key it holds, absence
 * for one it does not. */
export const referenceProofs = async (
  ledger: ReferenceLedger,
  keys: readonly Buffer[],
): Promise<Buffer[]> => {
  const proofs: Buffer[] = [];
  for (const key of keys) {
    const trie = await Trie.fromList(
      ledger.trie.entries.map(({ key: entryKey, value }) => ({
        key: Buffer.from(entryKey),
        value: Buffer.from(value),
      })),
    );
    const held = ledger.outputs.has(key.toString("hex"));
    proofs.push(
      Buffer.from((await trie.prove(Buffer.from(key), !held)).toCBOR()),
    );
  }
  return proofs;
};

export const referenceLedgerBefore = async ({
  block,
  predecessor,
  eventKey,
  stepIndex,
}: {
  readonly block: CanonicalBlockEvidence;
  readonly predecessor: Ledger | undefined;
  readonly eventKey: SDK.EventKey;
  readonly stepIndex: bigint;
}): Promise<ReferenceLedger> => {
  if ("DepositEventKey" in eventKey)
    throw new Error(
      "event ledger: subject is not a withdrawal or transaction event",
    );
  if (stepIndex < 0n || stepIndex >= block.header.transitionStepCount)
    refuse("step_out_of_range", stepIndex);
  let ledger = await referenceLedger(
    await listed(
      requireReplayPredecessorLedger({
        block,
        predecessor,
        label: "input-existence",
      }),
    ),
  );
  const entries = new Map<string, LedgerEntry>(
    ledger.entries.map((entry) => [entry.outRef.toString("hex"), entry]),
  );
  const targetRank = rank(eventKeyPhase(eventKey));
  const ranks: number[] = [];
  for (let index = 0n; index < stepIndex; index += 1n) {
    const opening = block.reconstruction.traceByStepIndex.get(index);
    const step = opening?.value;
    if (
      opening === undefined ||
      step === undefined ||
      opening.key !== index ||
      step.step_index !== index ||
      step.schema_version !== 1n ||
      step.pre_utxos_root !== ledger.utxosRoot
    )
      refuse("transition_link_changed", index);
    const event = block.reconstruction.sourceEventsByFingerprint.get(
      eventKeyFingerprint(step!.event_key),
    );
    if (
      event === undefined ||
      event.phase !== step!.phase ||
      Data.to(event.eventKey, SDK.EventKey) !==
        Data.to(step!.event_key, SDK.EventKey)
    )
      refuse("source_event_changed", index);
    // Out of phase order: the first step of a later phase than this one.
    const stepRank = rank(event!.phase);
    if (event!.phase === "Deposit") refuse("phase_order_changed", index);
    const misplaced = ranks.findIndex((prior) => prior > stepRank);
    if (misplaced >= 0) refuse("phase_order_changed", BigInt(misplaced));
    if (stepRank > targetRank) refuse("phase_order_changed", index);
    ranks.push(stepRank);
    let effects:
      | { txId: string; inputs: readonly Buffer[]; outputs: readonly Buffer[] }
      | undefined;
    if (event!.phase === "Withdrawal") {
      if (event!.entry.value.validity === "WithdrawalIsValid") {
        const ref = event!.entry.value.body.l2_outref;
        const key = encodeMidgardSpendInputItem({
          txId: Buffer.from(ref.transactionId, "hex"),
          outputIndex: Number(ref.outputIndex),
        }).toString("hex");
        if (!entries.delete(key)) refuse("withdrawal_input_absent", index);
      }
    } else if (event!.phase === "ForcedTransaction") {
      if (event!.entry.value.verdict === "ForcedTxValid") {
        const tx = decodeMidgardForcedTxFullFromCanonicalCbor(
          event!.entry.fullTransactionCbor,
        );
        effects = {
          txId: event!.entry.value.tx_id,
          inputs: decodeMidgardFieldPreimage(tx.body.spendInputsPreimageCbor),
          outputs: decodeMidgardFieldPreimage(tx.body.outputsPreimageCbor),
        };
      }
    } else if (
      event!.phase === "L2Transaction" &&
      event!.entry.validity === "TxIsValid"
    ) {
      effects = {
        txId: event!.entry.txId,
        inputs: decodeMidgardFieldPreimage(event!.entry.spendInputsPreimage),
        outputs: decodeMidgardFieldPreimage(event!.entry.outputsPreimage),
      };
    }
    if (effects !== undefined) {
      for (const input of effects.inputs)
        if (!entries.delete(input.toString("hex")))
          refuse("transaction_input_absent", index);
      for (const [outputIndex, output] of effects.outputs.entries()) {
        const outRef = encodeMidgardSpendInputItem({
          txId: Buffer.from(effects.txId, "hex"),
          outputIndex,
        });
        if (entries.has(outRef.toString("hex")))
          refuse("transaction_output_already_present", index);
        entries.set(outRef.toString("hex"), {
          outRef,
          output: Buffer.from(output),
        });
      }
    }
    try {
      ledger = await referenceLedger([...entries.values()]);
    } catch (cause) {
      // Every entry the steps did not create was checked before them.
      throw new EventLedgerOutputNotCanonical(index, cause as Error);
    }
    if (ledger.utxosRoot !== step!.post_utxos_root)
      refuse("post_root_changed", index);
  }
  const target = block.reconstruction.traceByStepIndex.get(stepIndex);
  if (
    target === undefined ||
    target.key !== stepIndex ||
    target.value.step_index !== stepIndex ||
    target.value.schema_version !== 1n ||
    target.value.phase !== eventKeyPhase(eventKey) ||
    Data.to(target.value.event_key, SDK.EventKey) !==
      Data.to(eventKey, SDK.EventKey) ||
    target.value.pre_utxos_root !== ledger.utxosRoot
  )
    refuse("selected_pre_root_changed", stepIndex);
  return ledger;
};
