import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  computeMidgardNativeTxId,
  deriveMidgardNativeTxWitnessSetCompact,
  encodeMidgardNativeTxCanonical,
  encodeMidgardNativeTxCompact,
  encodeMidgardNativeTxWitnessSetCompact,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core";
import { encodeMidgardForcedTxCanonical } from "@al-ft/midgard-core/codec/forced";
import { deriveMidgardForcedTxProofSource } from "@al-ft/midgard-core/codec/forced";
import { materializeMidgardForcedTxFromCanonical } from "@al-ft/midgard-core/codec/forced";
import {
  EventKeySchema,
  EventToStepValueSchema,
  type ForcedInclusionTxV1,
  ForcedInclusionTxV1Schema,
  type Header,
  OutputReference,
  Proof,
  type RejectionReason,
  ROOT_DOMAINS,
  type RootMembershipProof,
  TransitionStepSchema,
  ValidationTraceDescriptorSchema,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { submitResolvedOutputNonCanonicalStep01Accepted } from "../../src/resolved-output-non-canonical/index.js";
import { nativeTxFromCoreCompact } from "../../src/step-support.js";
import { buildCountedRoot } from "../../src/transition-trace/phas.js";
import { l2TransactionSourceCbor } from "./emulator/native-tx.js";
import { type ResolvedOutputContext } from "./resolved-output-non-canonical-emulator.build-prior-ledger.js";
import {
  countedTransactionsRoot,
  EMULATOR_HEADER_CLOCK_HEADROOM_MS,
  emulatorSuccessorHeaderStart,
  setupFraudulentBlock,
  submitSuccessorBlockTx,
} from "./submit-init-emulator-fixtures.js";
import {
  h32,
  makeHeader,
  transitionTraceDaEntry,
  transitionTraceOutRef,
} from "./submit-init-emulator-shared.js";

export type CommittedBlock = Readonly<{
  fraudulentBlockOutRef: string;
  headerHash: string;
  header: Header;
  nativeTx: MidgardNativeTxFull;
  nativeTxId: string;
  canonicalCbor: Buffer;
  /** Exact source-kind compact bytes step 02 anchors on. */
  compactCborHex: string;
  witnessSetCompactCborHex: string;
  accepted?: {
    readonly txInclusion: Parameters<
      typeof submitResolvedOutputNonCanonicalStep01Accepted
    >[0]["txInclusion"];
    readonly transactionsRoot: string;
  };
  forced?: {
    readonly leaf: ForcedInclusionTxV1;
    readonly membership: RootMembershipProof<
      OutputReference,
      ForcedInclusionTxV1
    >;
    readonly reason: RejectionReason;
  };
}>;

/**
 * Commits a predecessor whose ledger root is the prior ledger, then the
 * challenged successor: an accepted block carrying `nativeTx` under its
 * transactions root, or a forced block whose counted forced-transactions
 * root carries the leaf rejected for `reason`.
 */
export const commitBlock = async ({
  context,
  nativeTx,
  priorRoot,
  reason,
}: {
  readonly context: ResolvedOutputContext;
  readonly nativeTx: MidgardNativeTxFull;
  readonly priorRoot: string;
  /** Present for a forced (rejected) leaf, absent for an accepted transaction. */
  readonly reason?: RejectionReason;
}): Promise<CommittedBlock> => {
  const { harness, catalogue } = context;
  const nativeTxId = computeMidgardNativeTxId(nativeTx).toString("hex");
  const canonicalCbor = (
    reason === undefined
      ? encodeMidgardNativeTxCanonical
      : encodeMidgardForcedTxCanonical
  )(nativeTx);
  const witnessSetCompactCborHex = encodeMidgardNativeTxWitnessSetCompact(
    deriveMidgardNativeTxWitnessSetCompact(nativeTx.witnessSet),
  ).toString("hex");

  let accepted: CommittedBlock["accepted"];
  let forced: CommittedBlock["forced"];
  let compactCborHex = encodeMidgardNativeTxCompact(nativeTx.compact).toString(
    "hex",
  );
  // The transactions trie carrying the subject. The accepted successor
  // commits it; the predecessor of either shape commits it too, as a valid
  // one-transaction block whose content the proofs never touch.
  const sourceCbor = l2TransactionSourceCbor(nativeTx);
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  await trie.insert(
    Buffer.from(nativeTxId, "hex"),
    Buffer.from(sourceCbor, "hex"),
  );
  const proof = await trie.prove(Buffer.from(nativeTxId, "hex"));
  const transactionsRoot = Buffer.from(trie.hash).toString("hex");
  if (reason === undefined) {
    accepted = {
      transactionsRoot,
      txInclusion: {
        nativeTxId,
        nativeTx: nativeTxFromCoreCompact(nativeTx.compact),
        nativeTxCompactCbor: compactCborHex,
        l2TransactionSourceCbor: sourceCbor,
        transactionsPhasRoot: transactionsRoot,
        txMembershipProof: Data.from(proof.toCBOR().toString("hex"), Proof),
        txMembershipProofCbor: proof.toCBOR().toString("hex"),
      },
    };
  }
  const predecessor = await setupFraudulentBlock({
    funderLucid: harness.funderLucid,
    emulator: harness.emulator,
    contracts: harness.contracts,
    catalogue,
    fixture: {
      transactionsRoot,
      l2TransactionCount: 1n,
      utxosRoot: priorRoot,
      headerDurationMs: EMULATOR_HEADER_CLOCK_HEADROOM_MS,
    },
  });
  const targetStart = emulatorSuccessorHeaderStart({
    predecessorEndTime: predecessor.header.endTime,
    emulator: harness.emulator,
  });
  let header: Header;
  if (reason === undefined) {
    header = {
      ...makeHeader(
        predecessor.header.operatorVkey,
        targetStart,
        await countedTransactionsRoot(transactionsRoot, 1n),
        1n,
      ),
      prevHeaderHash: predecessor.headerHash,
      prevUtxosRoot: priorRoot,
    };
  } else {
    const source = deriveMidgardForcedTxProofSource(
      materializeMidgardForcedTxFromCanonical(nativeTx),
    );
    const leaf: ForcedInclusionTxV1 = {
      tx_id: nativeTxId,
      submitted_source: {
        compact_cbor: source.compactCbor.toString("hex"),
        witness_set_compact_cbor: source.witnessSetCompactCbor.toString("hex"),
        field_preimage_lengths_cbor:
          source.fieldPreimageLengthsCbor.toString("hex"),
      },
      verdict: { ForcedTxInvalid: { reason } },
    };
    compactCborHex = leaf.submitted_source.compact_cbor;
    const key = transitionTraceOutRef("f1");
    const keyBytes = Buffer.from(Data.to(key, OutputReference), "hex");
    const valueBytes = Buffer.from(
      Data.to(leaf as never, ForcedInclusionTxV1Schema as never),
      "hex",
    );
    const counted = await buildCountedRoot(ROOT_DOMAINS.forcedTransactionsV1, [
      { key: keyBytes, value: valueBytes },
    ]);
    const forcedStore = new Store(undefined);
    await forcedStore.ready();
    const forcedTrie = new Trie(forcedStore);
    await forcedTrie.insert(keyBytes, valueBytes);
    const membership: RootMembershipProof<
      OutputReference,
      ForcedInclusionTxV1
    > = {
      domain: ROOT_DOMAINS.forcedTransactionsV1,
      root: counted.root,
      phas_root: counted.phasRoot,
      count: counted.count,
      key,
      value: leaf,
      proof: Data.from(
        (await forcedTrie.prove(keyBytes)).toCBOR().toString("hex"),
        Proof,
      ),
    };
    forced = { leaf, membership, reason };
    // The header's other event commitments must carry the one forced event
    // too (`header_transition_commitments_v1_are_valid`): a rejected forced
    // transaction leaves the ledger at the prior root.
    const eventKey = { ForcedTransactionEventKey: { tx_order_id: key } };
    const countedEntries = async (
      domain: Parameters<typeof buildCountedRoot>[0],
      entries: readonly (readonly [string, string])[],
    ) =>
      await buildCountedRoot(
        domain,
        entries.map(([entryKey, entryValue]) => ({
          key: Buffer.from(entryKey, "hex"),
          value: Buffer.from(entryValue, "hex"),
        })),
      );
    const [transitionRoot, eventRoot, validationRoot] = await Promise.all([
      countedEntries(ROOT_DOMAINS.transitionTrace, [
        transitionTraceDaEntry({
          key: 0n,
          keySchema: Data.Integer() as never,
          value: {
            schema_version: 1n,
            step_index: 0n,
            event_key: eventKey,
            phase: "ForcedTransaction",
            pre_utxos_root: priorRoot,
            post_utxos_root: priorRoot,
          },
          valueSchema: TransitionStepSchema,
        }),
      ]),
      countedEntries(ROOT_DOMAINS.eventToStep, [
        transitionTraceDaEntry({
          key: eventKey,
          keySchema: EventKeySchema,
          value: { step_index: 0n, phase: "ForcedTransaction" },
          valueSchema: EventToStepValueSchema,
        }),
      ]),
      countedEntries(ROOT_DOMAINS.validationTraces, [
        transitionTraceDaEntry({
          key: eventKey,
          keySchema: EventKeySchema,
          value: {
            schema_version: 1n,
            machine_version: 1n,
            trace_root: h32("c1"),
            step_count: 1n,
            initial_state_hash: h32("c2"),
            terminal_state_hash: h32("c3"),
            verdict: "Rejected",
            rejection_code_hash: h32("c4"),
          },
          valueSchema: ValidationTraceDescriptorSchema,
        }),
      ]),
    ]);
    header = {
      ...makeHeader(predecessor.header.operatorVkey, targetStart),
      prevHeaderHash: predecessor.headerHash,
      prevUtxosRoot: priorRoot,
      utxosRoot: priorRoot,
      forcedTransactionsRoot: counted.root,
      transitionTraceRoot: transitionRoot.root,
      eventToStepRoot: eventRoot.root,
      validationTracesRoot: validationRoot.root,
      forcedTransactionCount: 1n,
      totalEventCount: 1n,
      transitionStepCount: 1n,
      validationTraceCount: 1n,
    };
  }
  const target = await submitSuccessorBlockTx({
    lucid: harness.funderLucid,
    emulator: harness.emulator,
    contracts: harness.contracts,
    anchorBlockUnit: predecessor.stateQueueBlockUnit,
    header,
    hubOracle: predecessor.hubOracle,
    scheduler: predecessor.scheduler,
    activeOperatorNode: predecessor.activeOperatorNode,
    activeOperatorNodeUnit: predecessor.activeOperatorNodeUnit,
  });
  return {
    fraudulentBlockOutRef: target.successorOutRef,
    headerHash: target.successorHeaderHash,
    header,
    nativeTx,
    nativeTxId,
    canonicalCbor,
    compactCborHex,
    witnessSetCompactCborHex,
    ...(accepted === undefined ? {} : { accepted }),
    ...(forced === undefined ? {} : { forced }),
  };
};
