import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import { computeMidgardNativeTxId } from "@al-ft/midgard-core";
import { deriveMidgardForcedTxProofSource } from "@al-ft/midgard-core/codec/forced";
import {
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec/forced";
import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import {
  DA_PAYLOAD_VERSION,
  EMPTY_MERKLE_TREE_ROOT,
  encodeDaPayload,
  EventKeySchema,
  EventToStepValueSchema,
  ForcedInclusionTxV1Schema,
  hashBlockHeader,
  OutputReference,
  Proof,
  ROOT_DOMAINS,
  TransitionStepSchema,
  ValidationTraceDescriptorSchema,
} from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerEntryOutputMaterial } from "@al-ft/midgard-validation";
import { Data, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  buildCountedRoot,
  keyValuePhasRootWithCount,
} from "../../src/transition-trace/phas.js";
import { reconstructDaPayload } from "../../src/transition-trace/reconstruct.js";
import { buildForcedTransactionLeafMembershipProof } from "../../src/transition-trace/witnesses.js";
import { publishPlainReferenceScriptUtxo } from "./emulator/reference-scripts.js";
import {
  type ForcedLeafSpec,
  type MeasurementRecorder,
  type OutputReferenceContext,
} from "./output-reference-script-decoding-emulator.commit-accepted-block.js";
import {
  outputReferenceCbor,
  sortedDaEntries,
  transitionTraceRawEntry,
} from "./submit-init-emulator-fixtures.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  h32,
  makeHeader,
  submitSetupTx,
  transitionTraceDaEntry,
  transitionTraceOutRef,
} from "./submit-init-emulator-shared.js";

/**
 * Commits forced leaves under one header: every leaf is an operator rejection
 * carrying its own typed reason, one transition step and event per leaf.
 */
export const commitForcedBlock = async (
  { harness, catalogue }: OutputReferenceContext,
  leaves: readonly ForcedLeafSpec[],
) => {
  const funderCredential = (
    await import("@lucid-evolution/lucid")
  ).getAddressDetails(
    await harness.funderLucid.wallet().address(),
  ).paymentCredential;
  if (funderCredential?.type !== "Key")
    throw new Error("forced fixture funder key absent");
  const now =
    alignUnixTimeToEmulatorSlotBoundary(
      harness.funderLucid,
      harness.emulator.now() + 120_000,
    ) - 1;
  const finalUtxo = transitionTraceRawEntry(
    outputReferenceCbor({ transactionId: h32("01"), outputIndex: 0n }).toString(
      "hex",
    ),
    "a200581d70aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa018200a0",
  );
  const descriptor = buildCanonicalMidgardLedgerEntryOutputMaterial({
    outRef: Buffer.from(finalUtxo[0], "hex"),
    outputCbor: Buffer.from(finalUtxo[1], "hex"),
  }).descriptorCbor;
  const finalRoot = await keyValuePhasRootWithCount([
    { key: Buffer.from(finalUtxo[0], "hex"), value: descriptor },
  ]);
  const built = leaves.map(({ nativeTx, reason }, index) => {
    const txOrderId = transitionTraceOutRef(`f${(index + 1).toString()}`);
    const eventKey = { ForcedTransactionEventKey: { tx_order_id: txOrderId } };
    const source = deriveMidgardForcedTxProofSource(
      materializeMidgardForcedTxFromCanonical(nativeTx),
    );
    const transaction = {
      tx_id: computeMidgardNativeTxId(nativeTx).toString("hex"),
      submitted_source: {
        compact_cbor: source.compactCbor.toString("hex"),
        witness_set_compact_cbor: source.witnessSetCompactCbor.toString("hex"),
        field_preimage_lengths_cbor:
          source.fieldPreimageLengthsCbor.toString("hex"),
      },
      verdict: { ForcedTxInvalid: { reason } },
    } as const;
    return {
      index,
      txOrderId,
      eventKey,
      nativeTx,
      transaction,
      reason,
      forcedEntry: transitionTraceDaEntry({
        key: txOrderId,
        keySchema: OutputReference as never,
        value: transaction,
        valueSchema: ForcedInclusionTxV1Schema,
      }),
      transitionEntry: transitionTraceDaEntry({
        key: BigInt(index),
        keySchema: Data.Integer() as never,
        value: {
          schema_version: 1n,
          step_index: BigInt(index),
          event_key: eventKey,
          phase: "ForcedTransaction",
          pre_utxos_root: EMPTY_MERKLE_TREE_ROOT,
          post_utxos_root: finalRoot.root,
        },
        valueSchema: TransitionStepSchema,
      }),
      eventEntry: transitionTraceDaEntry({
        key: eventKey,
        keySchema: EventKeySchema,
        value: { step_index: BigInt(index), phase: "ForcedTransaction" },
        valueSchema: EventToStepValueSchema,
      }),
      validationEntry: transitionTraceDaEntry({
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
      canonicalHex: encodeMidgardForcedTxCanonical(nativeTx).toString("hex"),
    };
  });
  const counted = async (
    domain: Parameters<typeof buildCountedRoot>[0],
    entries: readonly (readonly [string, string])[],
  ) =>
    await buildCountedRoot(
      domain,
      entries.map(([key, value]) => ({
        key: Buffer.from(key, "hex"),
        value: Buffer.from(value, "hex"),
      })),
    );
  const forcedEntries = built.map((leaf) => leaf.forcedEntry);
  const transitionEntries = built.map((leaf) => leaf.transitionEntry);
  const eventEntries = built.map((leaf) => leaf.eventEntry);
  const validationEntries = built.map((leaf) => leaf.validationEntry);
  const [forcedRoot, transitionRoot, eventRoot, validationRoot] =
    await Promise.all([
      counted(ROOT_DOMAINS.forcedTransactionsV1, forcedEntries),
      counted(ROOT_DOMAINS.transitionTrace, transitionEntries),
      counted(ROOT_DOMAINS.eventToStep, eventEntries),
      counted(ROOT_DOMAINS.validationTraces, validationEntries),
    ]);
  const count = BigInt(leaves.length);
  const counts = {
    withdrawalCount: 0n,
    forcedTransactionCount: count,
    l2TransactionCount: 0n,
    depositCount: 0n,
    totalEventCount: count,
    transitionStepCount: count,
    validationTraceCount: count,
  };
  const header = {
    ...makeHeader(funderCredential.hash, now),
    utxosRoot: finalRoot.root,
    forcedTransactionsRoot: forcedRoot.root,
    transitionTraceRoot: transitionRoot.root,
    eventToStepRoot: eventRoot.root,
    validationTracesRoot: validationRoot.root,
    ...counts,
  };
  const headerHash = await Effect.runPromise(hashBlockHeader(header));
  const payloadEnvelopeCbor = await wrapDaPayload(
    encodeDaPayload({
      version: DA_PAYLOAD_VERSION,
      block_body: {
        header_hash: headerHash,
        header,
        utxos: sortedDaEntries([finalUtxo]),
        withdrawals: [],
        forced_transactions: sortedDaEntries(forcedEntries),
        transactions: [],
        deposits: [],
        transition_trace: sortedDaEntries(transitionEntries),
        event_to_step: sortedDaEntries(eventEntries),
        transaction_preimages: [],
        forced_transaction_preimages: sortedDaEntries(
          built.map((leaf) =>
            transitionTraceRawEntry(leaf.forcedEntry[0], leaf.canonicalHex),
          ),
        ),
        cek_program_material: [],
        validation_traces: sortedDaEntries(validationEntries),
        validation_trace_witnesses: [],
        counts,
      },
    }),
    { mode: "identity" },
  );
  const reconstruction = await reconstructDaPayload({
    payloadEnvelopeCbor,
    expectedHeaderHash: headerHash,
    committedHeader: header,
  });
  const setup = await submitSetupTx({
    lucid: harness.funderLucid,
    contracts: harness.contracts,
    nonceUtxo: harness.nonceUtxo,
    catalogue,
    header,
  });
  const forcedLeaves = [];
  for (const leaf of built) {
    const membership = await buildForcedTransactionLeafMembershipProof({
      reconstruction,
      eventKey: leaf.eventKey,
    });
    forcedLeaves.push({
      nativeTx: leaf.nativeTx,
      transaction: leaf.transaction,
      reason: leaf.reason,
      eventKey: leaf.eventKey,
      membership,
      canonicalCbor: Buffer.from(encodeMidgardForcedTxCanonical(leaf.nativeTx)),
    });
  }
  return { header, headerHash, reconstruction, setup, leaves: forcedLeaves };
};

export type ForcedLeaf = Awaited<
  ReturnType<typeof commitForcedBlock>
>["leaves"][number];

/**
 * A forced root that carries the same leaf but which the header never
 * committed: the membership proof verifies against its own root and the
 * counted-root binding to the header refuses it.
 */
export const foreignForcedMembership = async (
  membership: ForcedLeaf["membership"],
) => {
  const key = Buffer.from(Data.to(membership.key, OutputReference), "hex");
  const value = Buffer.from(
    Data.to(membership.value as never, ForcedInclusionTxV1Schema as never),
    "hex",
  );
  const decoy = Buffer.from(
    Data.to(
      { transactionId: "ab".repeat(32), outputIndex: 7n },
      OutputReference,
    ),
    "hex",
  );
  const root = await buildCountedRoot(ROOT_DOMAINS.forcedTransactionsV1, [
    { key, value },
    { key: decoy, value },
  ]);
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  await trie.insert(key, value);
  await trie.insert(decoy, value);
  return {
    ...membership,
    root: root.root,
    phas_root: root.phasRoot,
    count: root.count,
    proof: Data.from((await trie.prove(key)).toCBOR().toString("hex"), Proof),
  };
};

// ## Stages

export const publishFamilyReferences = async (
  { harness, validators }: OutputReferenceContext,
  recorder: MeasurementRecorder,
  label: string,
) => {
  const references: UTxO[] = [];
  for (const [index, step] of validators.entries()) {
    const published = await publishPlainReferenceScriptUtxo({
      lucid: harness.funderLucid,
      script: step.spendingScript,
      label: `${label}-${index.toString()}`,
    });
    recorder.recordPublication(index, published.publicationMeasurement);
    references.push(published.utxo);
  }
  const certificateReference = (
    await publishPlainReferenceScriptUtxo({
      lucid: harness.funderLucid,
      script: harness.contracts.fieldPreimageCertificate.mintingScript,
      label: `${label}-certificate`,
    })
  ).utxo;
  return { references, certificateReference };
};
