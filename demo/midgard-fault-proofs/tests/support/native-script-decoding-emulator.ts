/**
 * Shared emulator fixtures for the `native-script-decoding` family (#635,
 * #633), offchain plan §8.2 suites 4–7.
 *
 * Three things every decoding emulator scenario needs and none of the
 * existing helpers produce:
 *
 * 1. **A committed block whose transition trace carries the accused step.**
 *    `tests/helpers/canonical-block-evidence-fixture.ts` emits an empty
 *    transition trace, but this family's step-02 opens the event→step leaf
 *    AND the transition step whose `pre_utxos_root` becomes the thread's
 *    `prior_ledger_root`. {@link buildDecodingBlockFixture} assembles the
 *    whole `DaPayload` — counted roots, dense trace, forced leaf and its
 *    DA preimage — the way `tests/transition-trace-challenger.test.ts` does,
 *    but under an emulator-committable header.
 * 2. **A pre-state ledger trie holding the accused outpoint's descriptor.**
 *    {@link buildDecodingLedgerFixture} files a
 *    `MidgardLedgerOutputCommitmentV1` under the §5.3 38-byte out-ref key.
 *    The reference-script facts are supplied rather than derived, because the
 *    whole premise of a direction-A decoding fault is a descriptor the
 *    operator admitted over bytes the canonical builder would have refused.
 * 3. **The six step validators published as reference scripts.** The former
 *    step-03 is split into open-subject, bind-descriptor, and
 *    advance-or-close validators so every applied body remains below the
 *    transaction-size ceiling. No step in this family inline-attaches.
 *
 * The payload fixtures are deliberately tiny (§8.2's "a handful of nodes"):
 * the multi-chunk direction-A item crosses exactly one 4,095-byte chunk
 * boundary and refuses after three primitive steps, so one Scan transaction
 * and one Verdict cover the whole machine route.
 */
import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  buildMidgardBoundedItem,
  buildMidgardLedgerOutputMaterial,
  encodeMidgardNativeScript,
  encodeMidgardTxOutput,
  MIDGARD_LEDGER_OUTPUT_FIELD_INDEX,
  MIDGARD_PROTOCOL_VERSION,
  type MidgardLedgerOutputCommitmentFacts,
  type MidgardLedgerOutputReferenceScriptLanguage,
} from "@al-ft/midgard-core";
import {
  adjudicateMidgardNativeTxFullValidity,
  computeMidgardNativeTxId,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  deriveMidgardNativeTxProofSource,
  deriveMidgardNativeTxProofSourceFromCanonicalCbor,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeMidgardNativeTxCanonical,
  encodeMidgardNativeTxCompact,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core/codec";
import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import * as SDK from "@al-ft/midgard-sdk";
import {
  encodeMidgardTxInputCanonical,
  faultProofStepRedeemerSchema,
  FraudProofComputationThreadRedeemer,
  type MidgardTxInput,
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerOutputMaterial } from "@al-ft/midgard-validation";
import {
  type BuildTxWithRedeemer,
  Data,
  generateEmulatorAccount,
  Lucid,
  type LucidEvolution,
  type Script,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { NativeScriptDecodingContracts } from "../../src/native-script-decoding/contracts.js";
import type { NativeScriptDecodingLedgerTrieHandle } from "../../src/native-script-decoding/evidence.js";
import { nativeScriptDecodingOutpointKey } from "../../src/native-script-decoding/evidence.js";
import {
  NATIVE_SCRIPT_DECODING_PROVER_POLICY_DEFAULTS,
  type NativeScriptDecodingProverDeps,
  type NativeScriptDecodingProverEvent,
  type NativeScriptDecodingProverPolicy,
} from "../../src/native-script-decoding/prover.js";
import {
  type NativeScriptDecodingStepIndex,
  requireNativeScriptDecodingReferenceScript,
} from "../../src/native-script-decoding/submit-common.js";
import {
  type ResolvedProverSigner,
  resolveProverSigner,
} from "../../src/runtime.js";
import { excludeUtxo } from "../../src/spend-input-witness.js";
import type { SubmitStep01TxInclusion } from "../../src/submit-step-01.js";
import {
  nativeTxFromCoreCompact,
  selectFeeInput,
} from "../../src/submit-step-01.js";
import {
  buildCountedRoot,
  keyValuePhasProof,
  keyValuePhasRootWithCount,
} from "../../src/transition-trace/phas.js";
import {
  encodeData,
  reconstructDaPayload,
  type TransitionTraceReconstruction,
} from "../../src/transition-trace/reconstruct.js";
import { computationThreadOutputPredicate } from "../../src/tx-layout.js";
import { witnessMintingPolicyCarriage } from "../../src/witness-reference-scripts.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  funderPaymentKeyHash,
  makeFaultProofEmulatorHarness,
  network as emulatorNetwork,
  publishPlainReferenceScriptUtxo,
  registerChunkedVerifyRewardAccount,
  submitSetupTx,
} from "./submit-init-emulator-shared.js";

// ---------------------------------------------------------------------------
// Reference-script item payloads (§8.2's "handful of nodes")
// ---------------------------------------------------------------------------

/** The single key hash every fixture native script signs under. */
export const DECODING_SIGNER_KEY = Buffer.alloc(28, 0x55);

const SIGNATURE_NODE_HEX = `8200581c${DECODING_SIGNER_KEY.toString("hex")}`;

/** Wrap a payload as the §5.3 versioned tag-0 item (`[0, payload-bytes]`). */
export const decodingItemFromPayload = (payload: Buffer): Buffer => {
  const head =
    payload.length <= 23
      ? Buffer.from([0x40 + payload.length])
      : payload.length < 256
        ? Buffer.from([0x58, payload.length])
        : Buffer.from([
            0x59,
            (payload.length >> 8) & 0xff,
            payload.length & 0xff,
          ]);
  return Buffer.concat([Buffer.from("8200", "hex"), head, payload]);
};

/**
 * `all(sig, <undecodable>)` padded past one chunk boundary: the machine
 * advances three primitive steps (container token, leaf token, frame pop) and
 * refuses the fourth token. Two chunks, so every window the plan carries is
 * the mandatory chunk-plus-next shape.
 */
export const decodingMalformedMultiChunkItem = (): Buffer => {
  const core = Buffer.from(`820182${SIGNATURE_NODE_HEX}820700`, "hex");
  return decodingItemFromPayload(
    Buffer.concat([core, Buffer.alloc(4_100 - core.length, 0)]),
  );
};

/** Maximum field-6 shape: `[item]` is exactly 32,768 bytes. */
export const decodingMalformedMaximumItem = (): Buffer => {
  const core = Buffer.from(`820182${SIGNATURE_NODE_HEX}820700`, "hex");
  return decodingItemFromPayload(
    Buffer.concat([core, Buffer.alloc(32_759 - core.length, 0)]),
  );
};

/** `all(sig)`: canonical, four primitive steps, one chunk. */
export const decodingCanonicalItem = (): Buffer =>
  decodingItemFromPayload(
    encodeMidgardNativeScript({
      type: "all",
      scripts: [{ type: "sig", keyHash: DECODING_SIGNER_KEY }],
    }),
  );

/** A wrapper whose language tag is outside {0, 3, 128}: malformed at bind. */
export const decodingMalformedWrapperItem = (): Buffer =>
  Buffer.from("8201410a", "hex");

/** A tag-3 (Plutus) item: the direction-B descriptor contradiction. */
export const decodingPlutusItem = (): Buffer =>
  Buffer.from("82034401020304", "hex");

// ---------------------------------------------------------------------------
// Pre-state ledger trie
// ---------------------------------------------------------------------------

const LEDGER_OUTPUT_ADDRESS = Buffer.concat([
  Buffer.from([0x60]),
  Buffer.alloc(28, 0x99),
]);

/** A plain key-hash output, the descriptor's carrier for every fixture. */
const fixtureOutputCbor = (): Buffer =>
  encodeMidgardTxOutput({
    address: LEDGER_OUTPUT_ADDRESS,
    value: { lovelace: 5_000_000n, assets: new Map() },
  });

export type DecodingLedgerFixture = {
  readonly descriptorCbor: string;
  readonly rootHex: string;
  readonly trie: NativeScriptDecodingLedgerTrieHandle;
  readonly outpointKey: Buffer;
};

/**
 * Files the accused outpoint's descriptor in a fresh MPF, with the
 * reference-script facts named rather than derived: a direction-A fault is
 * precisely a descriptor whose committed reference-script item the canonical
 * builder would refuse to decode.
 */
export const buildDecodingLedgerFixture = async ({
  txIdHex,
  outputIndex,
  referenceScriptItemBytes,
  referenceScriptLanguage,
  siblings = 1,
}: {
  readonly txIdHex: string;
  readonly outputIndex: number;
  readonly referenceScriptItemBytes: Uint8Array;
  readonly referenceScriptLanguage: Exclude<
    MidgardLedgerOutputReferenceScriptLanguage,
    -1
  >;
  readonly siblings?: number;
}): Promise<DecodingLedgerFixture> => {
  const outputCbor = fixtureOutputCbor();
  const base = buildCanonicalMidgardLedgerOutputMaterial({
    outputIndex,
    outputCbor,
  });
  const item = buildMidgardBoundedItem({
    fieldIndex: MIDGARD_LEDGER_OUTPUT_FIELD_INDEX,
    itemIndex: outputIndex,
    bytes: referenceScriptItemBytes,
  });
  const {
    version: _version,
    outputIndex: _outputIndex,
    totalLength: _totalLength,
    itemCommitment: _itemCommitment,
    ...baseFacts
  } = base.descriptor;
  const facts: MidgardLedgerOutputCommitmentFacts = {
    ...baseFacts,
    referenceScriptLanguage,
    referenceScriptHash: Buffer.alloc(28, 0x5a),
    referenceScriptTotalLength: referenceScriptItemBytes.length,
    referenceScriptItemCommitment: item.commitment,
  };
  const material = buildMidgardLedgerOutputMaterial({
    outputIndex,
    outputCbor,
    facts,
  });
  const outpointKey = nativeScriptDecodingOutpointKey({
    txIdHex,
    outputIndex,
  });
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  await trie.insert(outpointKey, material.descriptorCbor);
  for (let index = 0; index < siblings; index += 1) {
    await trie.insert(
      Buffer.concat([Buffer.alloc(37, 0xee), Buffer.from([index])]),
      Buffer.from([0xd0 + index]),
    );
  }
  const rootHex = (trie.hash as Buffer).toString("hex");
  return {
    descriptorCbor: material.descriptorCbor.toString("hex"),
    rootHex,
    outpointKey,
    trie: {
      rootHex,
      prove: async (target: Buffer) =>
        Buffer.from((await trie.prove(target)).toCBOR()),
    },
  };
};

// ---------------------------------------------------------------------------
// The committed block
// ---------------------------------------------------------------------------

/** A minimal native transaction with the named spend/reference input items. */
export const decodingSubjectTransaction = ({
  spendInputCbors = [],
  referenceInputCbors = [],
  fee = 0n,
}: {
  readonly spendInputCbors?: readonly Buffer[];
  readonly referenceInputCbors?: readonly Buffer[];
  readonly fee?: bigint;
}): MidgardNativeTxFull =>
  materializeMidgardNativeTxFromCanonical({
    version: MIDGARD_NATIVE_TX_VERSION,
    validity: "TxIsValid",
    body: {
      spendInputsPreimageCbor: encodeCbor([...spendInputCbors]),
      referenceInputsPreimageCbor: encodeCbor([...referenceInputCbors]),
      outputsPreimageCbor: EMPTY_CBOR_LIST,
      requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
      requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
      mintPreimageCbor: EMPTY_CBOR_LIST,
      scriptIntegrityHash: EMPTY_NULL_ROOT,
      auxiliaryDataHash: EMPTY_NULL_ROOT,
      fee,
      validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
      validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
      networkId: MIDGARD_NATIVE_NETWORK_ID_NONE,
    },
    witnessSet: {
      addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    },
  });

const entry = (key: Buffer, value: Buffer): SDK.DaPayloadEntry => [
  key.toString("hex"),
  value.toString("hex"),
];

const sorted = (entries: readonly SDK.DaPayloadEntry[]): SDK.DaPayloadEntry[] =>
  [...entries].sort(([left], [right]) =>
    left < right ? -1 : left > right ? 1 : 0,
  );

const bufferEntries = (entries: readonly SDK.DaPayloadEntry[]) =>
  entries.map(([key, value]) => ({
    key: Buffer.from(key, "hex"),
    value: Buffer.from(value, "hex"),
  }));

export type DecodingSubjectSource =
  | { readonly kind: "normal"; readonly nativeTx: MidgardNativeTxFull }
  | {
      readonly kind: "forced";
      readonly nativeTx: MidgardNativeTxFull;
      readonly orderKey: SDK.OutputReference;
      readonly verdict: SDK.OperatorVerdict;
    };

export type DecodingBlockFixture = {
  readonly header: SDK.Header;
  readonly headerHash: string;
  readonly payloadEnvelopeCbor: Buffer;
  readonly reconstruction: TransitionTraceReconstruction;
  readonly nativeTxId: string;
  readonly nativeTxCompactCbor: string;
  /** Direction-A normal-source threads only: the step-01 inclusion evidence. */
  readonly txInclusion: SubmitStep01TxInclusion | null;
  /** Inclusion evidence for every normal transaction committed by the fixture. */
  readonly txInclusions: ReadonlyMap<string, SubmitStep01TxInclusion>;
  readonly forcedOrderKey: SDK.OutputReference | null;
  readonly transactionsPhasRoot: string;
};

/**
 * The committed block the thread disputes: one event (the accused
 * transaction, normal or forced), one dense transition step carrying
 * `priorLedgerRoot` as its `pre_utxos_root`, and the matching event→step and
 * validation-trace leaves so `header_v1_is_valid` admits the header.
 */
export const buildDecodingBlockFixture = async ({
  operatorVkey,
  startTime,
  priorLedgerRoot,
  subject,
  decoyTransactionCount = 0,
  additionalTransactions = [],
}: {
  readonly operatorVkey: string;
  readonly startTime: bigint;
  readonly priorLedgerRoot: string;
  readonly subject: DecodingSubjectSource;
  /**
   * Extra committed L2 transactions, present only to give the header's
   * `transactions_root` more than one leaf: a single-leaf MPF proof has zero
   * steps, and the #545 published-chunk carriage has nothing to publish.
   */
  readonly decoyTransactionCount?: number;
  /** Caller-supplied normal transactions committed beside the subject. */
  readonly additionalTransactions?: readonly MidgardNativeTxFull[];
}): Promise<DecodingBlockFixture> => {
  const canonicalCbor = encodeMidgardNativeTxCanonical(subject.nativeTx);
  const nativeTxId = computeMidgardNativeTxId(subject.nativeTx).toString("hex");
  const compactCbor = encodeMidgardNativeTxCompact(subject.nativeTx.compact);

  let transactions: SDK.DaPayloadEntry[] = [];
  let transactionPreimages: SDK.DaPayloadEntry[] = [];
  let forcedTransactions: SDK.DaPayloadEntry[] = [];
  let forcedTransactionPreimages: SDK.DaPayloadEntry[] = [];
  let eventKey: SDK.EventKey;
  const phase: SDK.TransitionPhase =
    subject.kind === "normal" ? "L2Transaction" : "ForcedTransaction";

  if (subject.kind === "normal") {
    // The DA payload and header commit the same exact canonical
    // `Data(L2TransactionSource)` leaf value.
    const source =
      deriveMidgardNativeTxProofSourceFromCanonicalCbor(canonicalCbor);
    const sourceValue: SDK.L2TransactionSource = {
      tx_id: nativeTxId,
      source: {
        compact_cbor: source.compactCbor.toString("hex"),
        witness_set_compact_cbor: source.witnessSetCompactCbor.toString("hex"),
        field_preimage_lengths_cbor:
          source.fieldPreimageLengthsCbor.toString("hex"),
      },
    };
    transactions = [
      [
        nativeTxId,
        encodeData(
          sourceValue,
          SDK.L2TransactionSourceSchema as never,
        ).toString("hex"),
      ],
    ];
    transactionPreimages = [[nativeTxId, canonicalCbor.toString("hex")]];
    eventKey = { L2TransactionEventKey: { tx_id: nativeTxId } };
  } else {
    // §2.4.3(e): a rejected forced leaf commits the operator-ADJUDICATED
    // source, while the DA preimage stays the submitted canonical bytes. The
    // adjudicated triple is what the thread's `verified_tx_id` and every
    // downstream replay must key off — never the submitted one.
    const source =
      subject.verdict === "ForcedTxValid"
        ? deriveMidgardNativeTxProofSourceFromCanonicalCbor(canonicalCbor)
        : deriveMidgardNativeTxProofSource(
            adjudicateMidgardNativeTxFullValidity(
              decodeMidgardNativeTxFullFromCanonicalCbor(canonicalCbor),
              "TxIsInvalid",
            ),
          );
    const leaf: SDK.ForcedInclusionTxV1 = {
      tx_id: nativeTxId,
      source: {
        compact_cbor: source.compactCbor.toString("hex"),
        witness_set_compact_cbor: source.witnessSetCompactCbor.toString("hex"),
        field_preimage_lengths_cbor:
          source.fieldPreimageLengthsCbor.toString("hex"),
      },
      verdict: subject.verdict,
    };
    const key = encodeData(subject.orderKey, SDK.OutputReference as never);
    forcedTransactions = [
      entry(key, encodeData(leaf, SDK.ForcedInclusionTxV1 as never)),
    ];
    forcedTransactionPreimages = [entry(key, canonicalCbor)];
    eventKey = { ForcedTransactionEventKey: { tx_order_id: subject.orderKey } };
  }

  const normalTransactionsForInclusion: MidgardNativeTxFull[] =
    subject.kind === "normal" ? [subject.nativeTx] : [];

  // Decoys are ordinary committed L2 transactions. They exist only so the
  // transactions trie holds more than one leaf.
  // The placeholder validation-trace descriptor is stamped by the COMMITTED
  // LEAF's verdict, not by what a replay would conclude — the #640
  // convention. A `ForcedTxInvalid` leaf therefore carries `Rejected`, so a
  // direction-B fixture never presents a block that accepts and rejects the
  // same event at once.
  const events: {
    readonly eventKey: SDK.EventKey;
    readonly phase: SDK.TransitionPhase;
    readonly verdict: "Accepted" | "Rejected";
  }[] = [
    {
      eventKey,
      phase,
      verdict:
        subject.kind === "forced" && subject.verdict !== "ForcedTxValid"
          ? "Rejected"
          : "Accepted",
    },
  ];
  const decoys = [
    ...additionalTransactions,
    ...Array.from({ length: decoyTransactionCount }, (_, index) =>
      decodingSubjectTransaction({ fee: BigInt(5_000 + index) }),
    ),
  ];
  for (const decoy of decoys) {
    const decoyCanonical = encodeMidgardNativeTxCanonical(decoy);
    const decoyId = computeMidgardNativeTxId(decoy).toString("hex");
    const decoySource =
      deriveMidgardNativeTxProofSourceFromCanonicalCbor(decoyCanonical);
    transactions = [
      ...transactions,
      [
        decoyId,
        encodeData(
          {
            tx_id: decoyId,
            source: {
              compact_cbor: decoySource.compactCbor.toString("hex"),
              witness_set_compact_cbor:
                decoySource.witnessSetCompactCbor.toString("hex"),
              field_preimage_lengths_cbor:
                decoySource.fieldPreimageLengthsCbor.toString("hex"),
            },
          } satisfies SDK.L2TransactionSource,
          SDK.L2TransactionSourceSchema as never,
        ).toString("hex"),
      ],
    ];
    transactionPreimages = [
      ...transactionPreimages,
      [decoyId, decoyCanonical.toString("hex")],
    ];
    normalTransactionsForInclusion.push(decoy);
    events.push({
      eventKey: { L2TransactionEventKey: { tx_id: decoyId } },
      phase: "L2Transaction",
      verdict: "Accepted",
    });
  }

  const transitionTrace: SDK.DaPayloadEntry[] = [];
  const eventToStep: SDK.DaPayloadEntry[] = [];
  const validationTraces: SDK.DaPayloadEntry[] = [];
  for (const [stepIndex, event] of events.entries()) {
    const step: SDK.TransitionStep = {
      schema_version: SDK.TRANSITION_STEP_SCHEMA_VERSION,
      step_index: BigInt(stepIndex),
      event_key: event.eventKey,
      phase: event.phase,
      pre_utxos_root: priorLedgerRoot,
      post_utxos_root: priorLedgerRoot,
    };
    transitionTrace.push(
      entry(
        encodeData(step.step_index, Data.Integer() as never),
        encodeData(step, SDK.TransitionStepSchema as never),
      ),
    );
    eventToStep.push(
      entry(
        encodeData(event.eventKey, SDK.EventKeySchema as never),
        encodeData(
          {
            step_index: BigInt(stepIndex),
            phase: event.phase,
          } satisfies SDK.EventToStepValue,
          SDK.EventToStepValueSchema as never,
        ),
      ),
    );
    validationTraces.push(
      entry(
        encodeData(event.eventKey, SDK.EventKeySchema as never),
        encodeData(
          {
            schema_version: 1n,
            machine_version: 1n,
            trace_root: "1a".repeat(32),
            step_count: 1n,
            initial_state_hash: "1b".repeat(32),
            terminal_state_hash: "1c".repeat(32),
            verdict: event.verdict,
            rejection_code_hash: "1d".repeat(32),
          } satisfies SDK.ValidationTraceDescriptor,
          SDK.ValidationTraceDescriptorSchema as never,
        ),
      ),
    );
  }

  const utxoRoot = await keyValuePhasRootWithCount([]);
  const roots = {
    withdrawals: await buildCountedRoot(SDK.ROOT_DOMAINS.withdrawals, []),
    forcedTransactions: await buildCountedRoot(
      SDK.ROOT_DOMAINS.forcedTransactionsV1,
      bufferEntries(forcedTransactions),
    ),
    transactions: await buildCountedRoot(
      SDK.ROOT_DOMAINS.transactionsV1,
      bufferEntries(transactions),
    ),
    deposits: await buildCountedRoot(SDK.ROOT_DOMAINS.deposits, []),
    transitionTrace: await buildCountedRoot(
      SDK.ROOT_DOMAINS.transitionTrace,
      bufferEntries(transitionTrace),
    ),
    eventToStep: await buildCountedRoot(
      SDK.ROOT_DOMAINS.eventToStep,
      bufferEntries(eventToStep),
    ),
    validationTraces: await buildCountedRoot(
      SDK.ROOT_DOMAINS.validationTraces,
      bufferEntries(validationTraces),
    ),
  };
  const counts = {
    withdrawalCount: 0n,
    forcedTransactionCount: BigInt(forcedTransactions.length),
    l2TransactionCount: BigInt(transactions.length),
    depositCount: 0n,
    totalEventCount: BigInt(events.length),
    transitionStepCount: BigInt(events.length),
    validationTraceCount: BigInt(events.length),
  };
  const header: SDK.Header = {
    prevUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    utxosRoot: utxoRoot.root,
    withdrawalsRoot: roots.withdrawals.root,
    forcedTransactionsRoot: roots.forcedTransactions.root,
    transactionsRoot: roots.transactions.root,
    depositsRoot: roots.deposits.root,
    transitionTraceRoot: roots.transitionTrace.root,
    eventToStepRoot: roots.eventToStep.root,
    validationTracesRoot: roots.validationTraces.root,
    ...counts,
    startTime,
    endTime: startTime + 1_000n,
    blockSlot: 0n,
    expectedNetworkId: 0n,
    minFeeA: 0n,
    minFeeB: 0n,
    prevHeaderHash: SDK.GENESIS_HEADER_HASH,
    operatorVkey,
    protocolVersion: BigInt(MIDGARD_PROTOCOL_VERSION),
  };
  const headerHash = await Effect.runPromise(SDK.hashBlockHeader(header));
  const payload: SDK.DaPayload = {
    version: SDK.DA_PAYLOAD_VERSION,
    block_body: {
      header_hash: headerHash,
      header,
      utxos: [],
      withdrawals: [],
      forced_transactions: sorted(forcedTransactions),
      transactions: sorted(transactions),
      deposits: [],
      transition_trace: sorted(transitionTrace),
      event_to_step: sorted(eventToStep),
      transaction_preimages: sorted(transactionPreimages),
      forced_transaction_preimages: sorted(forcedTransactionPreimages),
      cek_program_material: [],
      validation_traces: sorted(validationTraces),
      validation_trace_witnesses: [],
      counts,
    },
  };
  const payloadEnvelopeCbor = await wrapDaPayload(
    SDK.encodeDaPayload(payload),
    { mode: "identity" },
  );
  const reconstruction = await reconstructDaPayload({
    payloadEnvelopeCbor,
    expectedHeaderHash: headerHash,
    committedHeader: header,
  });

  const txInclusions = new Map<string, SubmitStep01TxInclusion>();
  for (const nativeTx of normalTransactionsForInclusion) {
    const includedId = computeMidgardNativeTxId(nativeTx).toString("hex");
    const includedCompact = encodeMidgardNativeTxCompact(nativeTx.compact);
    const transactionEntry = transactions.find(([key]) => key === includedId);
    if (transactionEntry === undefined) {
      throw new Error(`Missing retained transaction source for ${includedId}`);
    }
    const includedSourceCbor = Buffer.from(transactionEntry[1], "hex");
    const membership = await keyValuePhasProof(
      { ...roots.transactions, root: roots.transactions.phasRoot },
      Buffer.from(includedId, "hex"),
      includedSourceCbor,
    );
    const proofCbor = Data.to(membership, SDK.Proof);
    txInclusions.set(includedId, {
      nativeTxId: includedId,
      nativeTx: nativeTxFromCoreCompact(nativeTx.compact),
      nativeTxCompactCbor: includedCompact.toString("hex"),
      l2TransactionSourceCbor: includedSourceCbor.toString("hex"),
      transactionsPhasRoot: roots.transactions.phasRoot,
      txMembershipProof: membership,
      txMembershipProofCbor: proofCbor,
    });
  }
  const txInclusion =
    subject.kind === "normal" ? (txInclusions.get(nativeTxId) ?? null) : null;

  return {
    header,
    headerHash,
    payloadEnvelopeCbor,
    reconstruction,
    nativeTxId,
    nativeTxCompactCbor: compactCbor.toString("hex"),
    txInclusion,
    txInclusions,
    forcedOrderKey: subject.kind === "forced" ? subject.orderKey : null,
    transactionsPhasRoot: roots.transactions.phasRoot,
  };
};

// ---------------------------------------------------------------------------
// Harness
// ---------------------------------------------------------------------------

/**
 * The decoding-family harness: the real six-validator chain built from the
 * regenerated blueprint and registered in its canonical production catalogue
 * category.
 */
export const makeDecodingEmulatorHarness = async () => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: {
      realNativeScriptDecoding: true,
      alwaysFraudProofCatalogue: true,
    },
    // The #545 published-chunk carriage withdraws from the merkelized
    // verifier's reward account, which must be registered before any step
    // takes that route.
    registerAdditionalRewardAccounts: registerChunkedVerifyRewardAccount,
  });
  const decoding = harness.contracts.nativeScriptDecoding;
  const category = harness.catalogue.categories.nativeScriptDecoding;
  if (decoding === undefined || category === undefined) {
    throw new Error(
      "Harness did not build the native-script-decoding contracts/category",
    );
  }
  if (
    category.categoryId !==
    SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.nativeScriptDecoding
  ) {
    throw new Error("Unexpected native-script-decoding catalogue category id");
  }
  // The adversarial suite needs a THIRD party — a wallet that is neither the
  // funder nor the prover, and that must never be able to drive or cancel
  // somebody else's thread. It starts empty; `fundDecodingOutsider` fills
  // it once the setup transaction has consumed the harness nonce UTxO.
  const outsider = generateEmulatorAccount({ lovelace: 0n });
  const outsiderLucid = await Lucid(harness.emulator, "Custom");
  outsiderLucid.selectWallet.fromSeed(outsider.seedPhrase);
  const outsiderSigner = resolveProverSigner({
    network: emulatorNetwork,
    walletSeedPhrase: outsider.seedPhrase,
  });
  return { ...harness, decoding, category, outsiderLucid, outsiderSigner };
};

/**
 * Publishes all six custody validators as reference scripts.
 */
export const publishDecodingReferenceScripts = async ({
  lucid,
  contracts,
}: {
  readonly lucid: Parameters<
    typeof publishPlainReferenceScriptUtxo
  >[0]["lucid"];
  readonly contracts: NativeScriptDecodingContracts;
}): Promise<readonly [UTxO, UTxO, UTxO, UTxO, UTxO, UTxO]> => {
  const published: UTxO[] = [];
  for (const [index, step] of contracts.steps.entries()) {
    const script: Script = step.spendingScript;
    const { utxo } = await publishPlainReferenceScriptUtxo({
      lucid,
      script,
      label: `native-script-decoding step-0${(index + 1).toString()}`,
    });
    published.push(utxo);
  }
  return published as unknown as readonly [UTxO, UTxO, UTxO, UTxO, UTxO, UTxO];
};

// ---------------------------------------------------------------------------
// The scenario: a committed block plus the pre-state ledger that resolves the
// accused outpoint, standing on the emulator and ready for Init.
// ---------------------------------------------------------------------------

/** The accused outpoint every scenario files, fixed so ids stay readable. */
export const DECODING_ACCUSED_TX_ID = "ab".repeat(32);

export type DecodingScenarioSource =
  | { readonly kind: "normal" }
  | {
      readonly kind: "forced";
      readonly verdict: SDK.OperatorVerdict;
      readonly orderKey?: SDK.OutputReference;
    };

export type DecodingScenario = {
  readonly ledger: DecodingLedgerFixture;
  readonly block: DecodingBlockFixture;
  readonly setup: Awaited<ReturnType<typeof submitSetupTx>>;
  readonly subjectFieldInputs: readonly MidgardTxInput[];
  /** Where the accused outpoint landed after the canonical §5.3 sort. */
  readonly accusedOrdinal: number;
  readonly accusedSourceKind: bigint;
  readonly referenceScriptItemBytes: Buffer;
};

/**
 * Commits the disputed block on the emulator over a pre-state ledger holding
 * the accused outpoint's descriptor. `accusedSourceKind` picks the §2.5 field
 * the accused ordinal indexes (0 = spend inputs, 1 = reference inputs). With
 * no decoys the accused outpoint sits at ordinal 0 of that field; decoys are
 * sorted in canonically, and `accusedOrdinal` reports where it landed.
 */
export const setupDecodingScenario = async ({
  harness,
  referenceScriptItemBytes,
  referenceScriptLanguage = 0,
  source,
  accusedSourceKind = 1n,
  accusedOutputIndex = 0,
  decoyTransactionCount = 0,
  decoySubjectInputCount = 0,
}: {
  readonly harness: Awaited<ReturnType<typeof makeDecodingEmulatorHarness>>;
  readonly referenceScriptItemBytes: Buffer;
  readonly referenceScriptLanguage?: Exclude<
    MidgardLedgerOutputReferenceScriptLanguage,
    -1
  >;
  readonly source: DecodingScenarioSource;
  readonly accusedSourceKind?: bigint;
  readonly accusedOutputIndex?: number;
  /** Extra committed L2 transactions, so the transactions trie proves in steps. */
  readonly decoyTransactionCount?: number;
  /**
   * Extra fabricated outpoints committed in the subject field beside the
   * accused one, so a test can grow the field's §5.1 preimage past the §8.4
   * tier-1 bound and let size alone select tier-2 carriage.
   */
  readonly decoySubjectInputCount?: number;
}): Promise<DecodingScenario> => {
  const { emulator, funderLucid, contracts, catalogue, nonceUtxo } = harness;
  const ledger = await buildDecodingLedgerFixture({
    txIdHex: DECODING_ACCUSED_TX_ID,
    outputIndex: accusedOutputIndex,
    referenceScriptItemBytes,
    referenceScriptLanguage,
  });
  const accused: MidgardTxInput = {
    tx_id: DECODING_ACCUSED_TX_ID,
    output_index: BigInt(accusedOutputIndex),
  };
  const subjectFieldInputs = [
    accused,
    ...Array.from(
      { length: decoySubjectInputCount },
      (_, index): MidgardTxInput => ({
        tx_id: (index + 1).toString(16).padStart(64, "0"),
        output_index: 0n,
      }),
    ),
  ].sort((left, right) =>
    Buffer.compare(
      encodeMidgardTxInputCanonical(left),
      encodeMidgardTxInputCanonical(right),
    ),
  );
  const accusedOrdinal = subjectFieldInputs.findIndex(
    (input) =>
      input.tx_id === accused.tx_id &&
      input.output_index === accused.output_index,
  );
  const subjectFieldCbors = subjectFieldInputs.map(
    encodeMidgardTxInputCanonical,
  );
  const nativeTx = decodingSubjectTransaction(
    accusedSourceKind === 0n
      ? { spendInputCbors: subjectFieldCbors, fee: 1_000n }
      : { referenceInputCbors: subjectFieldCbors, fee: 1_000n },
  );
  const funderKeyHash = await funderPaymentKeyHash(funderLucid);
  const startTime = BigInt(
    alignUnixTimeToEmulatorSlotBoundary(funderLucid, emulator.now() + 120_000) -
      1,
  );
  const block = await buildDecodingBlockFixture({
    operatorVkey: funderKeyHash,
    startTime,
    priorLedgerRoot: ledger.rootHex,
    decoyTransactionCount,
    subject:
      source.kind === "normal"
        ? { kind: "normal", nativeTx }
        : {
            kind: "forced",
            nativeTx,
            orderKey: source.orderKey ?? {
              transactionId: "cd".repeat(32),
              outputIndex: 0n,
            },
            verdict: source.verdict,
          },
  });
  const setup = await submitSetupTx({
    lucid: funderLucid,
    contracts,
    nonceUtxo,
    catalogue,
    header: block.header,
  });
  return {
    ledger,
    block,
    setup,
    subjectFieldInputs,
    accusedOrdinal,
    accusedSourceKind,
    referenceScriptItemBytes,
  };
};

// ---------------------------------------------------------------------------
// The §4.3 proving core, wired to a scenario
// ---------------------------------------------------------------------------

/** The emulator has no L1 depth or maturity to observe; both gates are off. */
export const DECODING_EMULATOR_PROVER_POLICY: NativeScriptDecodingProverPolicy =
  {
    ...NATIVE_SCRIPT_DECODING_PROVER_POLICY_DEFAULTS,
    minSettlementDepth: 0n,
    maturityGuardFactor: 0,
    maxThreadBudgetLovelace: null,
  };

/**
 * The proving core's dependencies for a scenario: every §4.3 evidence
 * callback is answered from the fixture, so what the core drives on chain is
 * exactly the committed material.
 */
export const decodingProverDeps = ({
  harness,
  scenario,
  referenceScriptItemBytes,
  referenceScriptUtxos,
  journal,
}: {
  readonly harness: Awaited<ReturnType<typeof makeDecodingEmulatorHarness>>;
  readonly scenario: DecodingScenario;
  /** `null` for the routes that never scan an item (§7.2, contradiction). */
  readonly referenceScriptItemBytes: Uint8Array | null;
  readonly referenceScriptUtxos?: NativeScriptDecodingProverDeps["referenceScriptUtxos"];
  readonly journal?: (event: NativeScriptDecodingProverEvent) => void;
}): NativeScriptDecodingProverDeps => ({
  lucid: harness.proverLucid,
  blueprint: harness.realBlueprint,
  network: emulatorNetwork,
  contracts: harness.decoding,
  category: harness.category,
  catalogue: {
    policyId: harness.contracts.fraudProofCatalogue.policyId,
    spendingScriptAddress:
      harness.contracts.fraudProofCatalogue.spendingScriptAddress,
    root: harness.catalogue.root,
  },
  signer: harness.proverSigner,
  evidence: {
    txInclusion: async () => {
      const inclusion = scenario.block.txInclusion;
      if (inclusion === null) {
        throw new Error("this scenario's source is forced; it binds no leaf");
      }
      return inclusion;
    },
    reconstruction: async () => scenario.block.reconstruction,
    subjectTx: async () => ({
      nativeTxCompactCbor: scenario.block.nativeTxCompactCbor,
      subjectFieldInputs: scenario.subjectFieldInputs,
    }),
    descriptor: async () => ({
      descriptorCbor: scenario.ledger.descriptorCbor,
      referenceScriptItemBytes,
    }),
    ledgerTrie: async () => scenario.ledger.trie,
  },
  observations: {},
  journal: journal ?? (() => undefined),
  policy: DECODING_EMULATOR_PROVER_POLICY,
  referenceScriptUtxos,
  witnessReferenceScripts: harness.witnessReferenceScripts,
});

/**
 * Every step's spend redeemer shares the `Cancel` head; the raw builders
 * below never encode a `Continue` through this schema, so the argument
 * schema is irrelevant.
 */
const RawCancelSpendRedeemerSchema = faultProofStepRedeemerSchema(Data.Any());
type RawCancelSpendRedeemer = Data.Static<typeof RawCancelSpendRedeemerSchema>;
const RawCancelSpendRedeemer = asDataType<RawCancelSpendRedeemer>(
  RawCancelSpendRedeemerSchema,
);

/** The thread layout a raw redeemer builder is handed. */
export type RawDecodingStepLayout = {
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
};

/**
 * A test-only raw thread advancement — the same transaction shape the
 * step-02/step-03 submitters build (fee input, thread spend, advanced state
 * paid to `destinationAddress`, carriage read as reference inputs, Q3
 * step-script sourcing), with NONE of their fail-closed pre-checks.
 *
 * The adversarial suite needs this because every attack it exercises is one
 * the honest submitters refuse locally, before anything is paid for. To
 * observe the ON-CHAIN refusal — the check that actually protects an honest
 * operator against a prover who patched their own tooling — the transaction
 * has to be built past those guards. Production code never takes this path.
 */
export const submitRawDecodingStep = async ({
  lucid,
  contracts,
  signer,
  stepIndex,
  threadUtxo,
  threadUnit,
  destinationAddress,
  nextDatumCbor,
  buildRedeemer,
  carriageUtxos = [],
  referenceScriptUtxo,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: NativeScriptDecodingContracts;
  readonly signer: ResolvedProverSigner;
  readonly stepIndex: number;
  readonly threadUtxo: UTxO;
  readonly threadUnit: string;
  readonly destinationAddress: string;
  readonly nextDatumCbor: string;
  readonly buildRedeemer: (layout: RawDecodingStepLayout) => string;
  readonly carriageUtxos?: readonly UTxO[];
  readonly referenceScriptUtxo: UTxO;
}): Promise<string> => {
  signer.selectWallet(lucid);
  const walletUtxos = await lucid.wallet().getUtxos();
  const walletUtxosSansCarriage = carriageUtxos.reduce<readonly UTxO[]>(
    (candidates, utxo) => excludeUtxo(candidates, utxo),
    walletUtxos,
  );
  const feeInput = selectFeeInput(walletUtxosSansCarriage);
  const outputMatches = computationThreadOutputPredicate({
    address: destinationAddress,
    datum: nextDatumCbor,
    unit: threadUnit,
  });
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "raw decoding step");
    return buildRedeemer({
      inputIndex: requireInputIndex(ctx, threadUtxo, "raw decoding step"),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        outputMatches,
        "raw decoding step output",
      ),
    });
  }) satisfies BuildTxWithRedeemer;
  const stepContract = contracts.steps[stepIndex];
  if (stepContract === undefined || stepIndex < 0 || stepIndex > 5) {
    throw new Error(
      `raw decoding step index ${stepIndex.toString()} is invalid`,
    );
  }
  const stepReference = requireNativeScriptDecodingReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: stepContract.spendingScriptHash,
    stepIndex: stepIndex as NativeScriptDecodingStepIndex,
  });

  const withReferences = (() => {
    const base = lucid
      .newTx()
      .collectFrom([feeInput])
      .collectFrom([threadUtxo], redeemer);
    const referenceInputs = [...carriageUtxos, stepReference];
    return referenceInputs.length === 0 ? base : base.readFrom(referenceInputs);
  })();
  const paid = withReferences.pay
    .ToContract(
      destinationAddress,
      { kind: "inline", value: nextDatumCbor },
      { lovelace: threadUtxo.assets.lovelace ?? 0n, [threadUnit]: 1n },
    )
    .addSignerKey(signer.paymentKeyHash);
  const tx = paid;

  const unsigned = await tx.complete({
    localUPLCEval: true,
    ...(carriageUtxos.length === 0
      ? {}
      : { presetWalletInputs: walletUtxosSansCarriage as UTxO[] }),
  });
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);
  return txHash;
};

/**
 * A test-only raw `ct.Cancel` — the cancel submitter's transaction without
 * its "only the named prover can cancel" pre-check, so a third party's
 * attempt reaches the validator's own signature demand.
 */
export const submitRawDecodingCancel = async ({
  lucid,
  contracts,
  signer,
  stepIndex,
  threadUtxo,
  threadUnit,
  threadAssetName,
  referenceScriptUtxo,
  computationThreadReferenceUtxo,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: NativeScriptDecodingContracts;
  readonly signer: ResolvedProverSigner;
  readonly stepIndex: number;
  readonly threadUtxo: UTxO;
  readonly threadUnit: string;
  readonly threadAssetName: string;
  readonly referenceScriptUtxo: UTxO;
  readonly computationThreadReferenceUtxo: UTxO;
}): Promise<string> => {
  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const spendRedeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "raw decoding cancel");
    return Data.to(
      {
        Cancel: {
          input_index: requireInputIndex(
            ctx,
            threadUtxo,
            "raw decoding cancel",
          ),
          computation_thread_mint_redeemer_index: requireMintRedeemerIndex(
            ctx,
            contracts.computationThread.policyId,
            "raw decoding cancel burn",
          ),
        },
      },
      RawCancelSpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const burnRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      contracts.computationThread.policyId,
      "raw decoding cancel burn",
    );
    return Data.to(
      { BurnForCancellation: { burning_token_asset_name: threadAssetName } },
      FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const stepContract = contracts.steps[stepIndex];
  if (stepContract === undefined || stepIndex < 0 || stepIndex > 5) {
    throw new Error(
      `raw decoding cancel step index ${stepIndex.toString()} is invalid`,
    );
  }
  const stepReference = requireNativeScriptDecodingReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: stepContract.spendingScriptHash,
    stepIndex: stepIndex as NativeScriptDecodingStepIndex,
  });
  const computationThreadCarriage = witnessMintingPolicyCarriage({
    script: contracts.computationThread.mintingScript,
    referenceUtxo: computationThreadReferenceUtxo,
    label: "raw decoding cancel computation-thread mint",
  });

  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], spendRedeemer)
    .mintAssets({ [threadUnit]: -1n }, burnRedeemer)
    .addSignerKey(signer.paymentKeyHash)
    .readFrom([stepReference, ...computationThreadCarriage.referenceInputs]);
  const tx = computationThreadCarriage.attach(base);
  const unsigned = await tx.complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);
  return txHash;
};

/**
 * Funds the harness's third-party wallet with plain, token-free outputs so
 * its own transactions can always pay their fee.
 *
 * Deliberately NOT folded into the harness: the funder's opening UTxO is the
 * one-shot nonce the contracts are parameterised by, and the setup
 * transaction has to be the one that spends it.
 */
export const fundDecodingOutsider = async (
  harness: Awaited<ReturnType<typeof makeDecodingEmulatorHarness>>,
): Promise<void> => {
  // Both of the outsider's addresses are funded. `selectWallet.fromSeed`
  // derives the seed's base address while `resolveProverSigner` derives its
  // enterprise address, and the raw drivers re-select through the signer, so
  // funding only the base address strands every transaction the outsider
  // builds after that call.
  const outsiderAddress = await harness.outsiderLucid.wallet().address();
  const funding = await harness.funderLucid
    .newTx()
    .pay.ToAddress(outsiderAddress, { lovelace: 1_000_000_000n })
    .pay.ToAddress(outsiderAddress, { lovelace: 1_000_000_000n })
    .pay.ToAddress(harness.outsiderSigner.address, { lovelace: 1_000_000_000n })
    .pay.ToAddress(harness.outsiderSigner.address, { lovelace: 1_000_000_000n })
    .complete();
  const signed = await funding.sign.withWallet().complete();
  await harness.funderLucid.awaitTx(await signed.submit());
};

/**
 * Asserts a transaction the validator must refuse does not land, and that it
 * died IN THE VALIDATOR rather than in the transaction builder.
 *
 * `localUPLCEval: true` runs the script during `.complete()`, so a validator
 * abort surfaces as an evaluator failure here. The `failed script execution`
 * requirement is what keeps these negatives honest: an offchain builder
 * error — a missing fee input, an unresolvable layout — would otherwise read
 * as a passing security assertion.
 */
export { expectOnchainRefusal } from "./emulator/expect-onchain-refusal.js";
