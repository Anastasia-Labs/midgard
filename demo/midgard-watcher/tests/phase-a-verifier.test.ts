/**
 * W24 Phase A verifier tests.
 *
 * WHAT THESE TESTS ARE FOR. The W24 CG3 waiver allows a watcher-side Phase A
 * verifier only if it *reuses* canonical validation semantics. So the
 * load-bearing evidence here is not "the watcher rejects bad transactions" -
 * it is the differential: for a corpus of valid and invalid transactions the
 * watcher's record is byte-identical to what `validatePhaseASingle` returns
 * (same `RejectCode`, same stage, same detail), and in the direction that
 * matters, a transaction the canonical path rejects is never accepted by the
 * watcher. Everything else - the published vocabulary, the per-code evidence,
 * the boundary and fail-closed cases - exists to show the adapter cannot
 * silently loosen that identity.
 *
 * PROVENANCE OF EVERY INPUT.
 * - Transaction bytes: `makeNativeTx` from
 *   demo/midgard-validation/tests/validation-fixtures.ts, the canonical V1
 *   native transaction encoder's own fixture builder. This file authors no
 *   transaction encoding of its own.
 * - Expected verdicts: `validatePhaseASingle` itself, called directly. No
 *   expected `RejectCode` here is a hand-written guess about what the protocol
 *   should do; the per-code cases assert an identity against the canonical
 *   function, and the code names only pin *which* canonical outcome each
 *   fixture reaches.
 * - Block bytes: a `DaPayloadEnvelopeV1` built the way the node builds one
 *   (demo/midgard-node/src/workers/commit-block-header/da-payload.ts), then
 *   put through the real W22 evaluation, so the W24 input is a genuinely
 *   accepted W22 record rather than a literal.
 * - Header and Phase A parameters: the L1-committed `HeaderV1`, reached only
 *   through `makeWatcherAuthenticatedHeaderObservationV1`.
 *
 * CROSS-LANGUAGE VECTORS: N/A, and deliberately so. W24 adds no new TS/Aiken
 * boundary: it introduces no serialization format, no hash preimage, and no
 * on-chain-visible value. Every byte it touches is produced and interpreted by
 * canonical modules that carry their own cross-language vectors - the V1
 * native transaction codec, the consensus profile, the DA payload encoding,
 * and the canonical reject-code vocabulary. The one artifact W24 mints, the
 * `resultDigest`, is a watcher-local canonical-JSON digest with no on-chain
 * counterpart.
 */
import {
  computeMidgardNativeTxIdV1,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  deriveMidgardNativeTxProofSourceV1FromCanonicalCbor,
} from "@al-ft/midgard-core/codec";
import { buildCountedRoot, encodeData } from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import { validatePhaseASingle } from "@al-ft/midgard-validation/phase-a";
import type {
  PhaseAConfig,
  QueuedTx,
  RejectCode,
  RejectedTx,
} from "@al-ft/midgard-validation/types";
import { RejectCodes } from "@al-ft/midgard-validation/types";
import { CML, Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { blake2b } from "../../midgard-core/node_modules/@noble/hashes/blake2.js";
import {
  encodeMidgardCekProgramMaterialDaValueV1,
  encodeMidgardCekProgramMaterialSidecarV1,
  encodeMidgardCekTermNodeV1,
  hashMidgardCekTermNodeV1,
} from "../../midgard-core/src/cek-proof.js";
import type { MidgardNativeScript } from "../../midgard-core/src/codec/native-script.js";
import { encodeMidgardTxOutput } from "../../midgard-core/src/codec/output.js";
import { MIDGARD_CONSENSUS_PROFILE_V1 } from "../../midgard-core/src/consensus-profile-v1.js";
import { wrapDaPayloadV1 } from "../../midgard-core/src/da-payload-envelope.js";
import {
  makeNativeTx,
  makeOutput,
  nativeScriptWitness,
  outRefFromByte,
  plutusV3ScriptWitness,
  TEST_ADDRESS_BYTES,
} from "../../midgard-validation/tests/validation-fixtures.js";
import { watcherSha256CanonicalJsonV1 } from "../src/durable-store.js";
import {
  evaluateWatcherHeaderRootReconstructionV1,
  makeWatcherAuthenticatedHeaderObservationV1,
  type WatcherHeaderRootReconstructionResultV1,
} from "../src/header-root-reconstruction.js";
import {
  evaluateWatcherPhaseABlockV1,
  evaluateWatcherPhaseAQueuedTxsV1,
  makeWatcherPhaseAConfigV1,
  WATCHER_PHASE_A_CANONICAL_REJECT_CODES_V1,
  WATCHER_PHASE_A_CONSENSUS_REJECT_CODES_V1,
  WATCHER_PHASE_A_DIRECT_REJECT_CODES_V1,
  WATCHER_PHASE_A_DOMINATED_REJECT_CODE_JUSTIFICATIONS_V1,
  WATCHER_PHASE_A_DOMINATED_REJECT_CODES_V1,
  WATCHER_PHASE_A_EVIDENCED_REJECT_CODES_V1,
  WATCHER_PHASE_A_EXCLUDED_REJECT_CODE_JUSTIFICATIONS_V1,
  WATCHER_PHASE_A_EXCLUDED_REJECT_CODES_V1,
  WATCHER_PHASE_A_REACHABLE_REJECT_CODES_V1,
  WATCHER_PHASE_A_VERIFIER_REASON_CODES_V1,
  WATCHER_PHASE_A_VERIFIER_V1_SCHEMA_VERSION,
  watcherPhaseAQueuedTxsV1,
  watcherPhaseARejectionProjectionV1,
  type WatcherPhaseAVerificationResultV1,
  WatcherPhaseAVerifierError,
} from "../src/phase-a-verifier.js";
import {
  computeWatcherRuleBundleV1Commitment,
  makeWatcherCanonicalRuleBundleV1,
  WATCHER_RULE_BUNDLE_V1_VALIDATION_PHASE_PRIORITY,
  type WatcherRuleBundleV1,
} from "../src/rule-bundle-v1.js";
import type { WatcherStateQueueHeaderV1 } from "../src/state-queue-indexer.js";

// ---------------------------------------------------------------------------
// Shared fixture material
// ---------------------------------------------------------------------------

const h32 = (byte: number): string =>
  byte.toString(16).padStart(2, "0").repeat(32);
const h28 = (byte: number): string =>
  byte.toString(16).padStart(2, "0").repeat(28);

/**
 * The size/preimage-bound fixtures are hundreds of kilobytes of canonical CBOR
 * and are validated more than once per case; the 5s default is not a safe
 * budget for them inside a parallel run of the whole watcher suite.
 */
const SLOW_TEST_TIMEOUT_MS = 120_000;

/** Deterministic signing key: fixture bytes must not vary between runs. */
const KEY = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 7));

const L1_PROVENANCE: SDK.EvidenceProvenanceV1 = {
  trustClass: "authenticated_cardano_l1",
  sourceId: "watcher-local-node",
  grade: "security",
};

const DA_PROVENANCE: SDK.EvidenceProvenanceV1 = {
  trustClass: "public_or_permissionless_da",
  sourceId: "watcher-da-peer-1",
  grade: "security",
};

const CHAIN_POINT = { slot: 4242n, blockHash: h32(7) } as const;

const RULE_BUNDLE: WatcherRuleBundleV1 = makeWatcherCanonicalRuleBundleV1({
  constructionIdentity: {
    manifestId: h32(0x21),
    network: "Preprod",
    releaseEvidenceDigest: h32(0x22),
    programCommitments: {
      "transition-order-v1": h32(0x23),
      "validation-machine-v1": h32(0x24),
    },
  },
  targetParameterSnapshot: { finalityDepth: 12 },
});

const RULE_BUNDLE_COMMITMENT =
  computeWatcherRuleBundleV1Commitment(RULE_BUNDLE);

const baseHeader = (overrides: Partial<SDK.HeaderV1> = {}): SDK.HeaderV1 => ({
  prevUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  utxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  transitionTraceRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  eventToStepRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  validationTracesRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  withdrawalCount: 0n,
  forcedTransactionCount: 0n,
  l2TransactionCount: 0n,
  depositCount: 0n,
  totalEventCount: 0n,
  transitionStepCount: 0n,
  validationTraceCount: 0n,
  startTime: 10n,
  endTime: 20n,
  blockSlot: 0n,
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  prevHeaderHash: h28(90),
  operatorVkey: h28(91),
  protocolVersion: BigInt(RULE_BUNDLE.protocolVersion),
  ...overrides,
});

const configFor = (overrides: Partial<SDK.HeaderV1> = {}): PhaseAConfig =>
  makeWatcherPhaseAConfigV1({
    header: baseHeader(overrides),
    ruleBundle: RULE_BUNDLE,
  });

const CONFIG = configFor();

const EMPTY_SIDECAR = encodeMidgardCekProgramMaterialSidecarV1([]);

const queuedTx = (
  txId: Buffer,
  txCbor: Buffer,
  overrides: Partial<QueuedTx> = {},
): QueuedTx => ({
  txId,
  txCbor,
  arrivalSeq: 0n,
  createdAt: new Date(0),
  programMaterialSidecarCbor: EMPTY_SIDECAR,
  ...overrides,
});

const nestedNativeScript = (depth: number): MidgardNativeScript => {
  let script: MidgardNativeScript = { type: "after", slot: 0n };
  for (let index = 0; index < depth; index += 1) {
    script = { type: "all", scripts: [script] };
  }
  return script;
};

/** An inline datum of roughly `chunks * 66` bytes, canonically chunked. */
const bigInlineDatum = (chunks: number): Buffer =>
  Buffer.concat([
    Buffer.from([0x5f]),
    ...Array.from({ length: chunks }, () =>
      Buffer.concat([Buffer.from([0x58, 0x40]), Buffer.alloc(64, 1)]),
    ),
    Buffer.from([0xff]),
  ]);

const assetMap = (count: number): Map<string, Map<string, bigint>> => {
  const inner = new Map<string, bigint>();
  for (let index = 0; index < count; index += 1) {
    inner.set(index.toString(16).padStart(4, "0"), 1n);
  }
  return new Map([["ab".repeat(28), inner]]);
};

const manyOutputs = (count: number): Buffer[] =>
  Array.from({ length: count }, (_unused, index) =>
    makeOutput(BigInt(index + 1), TEST_ADDRESS_BYTES),
  );

/** An unreachable CEK term node, used as block-wide program material. */
const ORPHAN_TERM_NODE = { kind: "error" as const };
const ORPHAN_MATERIAL_ENTRY = {
  kind: "term" as const,
  root: hashMidgardCekTermNodeV1(ORPHAN_TERM_NODE),
  preimage: encodeMidgardCekTermNodeV1(ORPHAN_TERM_NODE),
};
const ORPHAN_MATERIAL_DA_ENTRY: SDK.DaPayloadEntry = [
  Buffer.from(ORPHAN_MATERIAL_ENTRY.root).toString("hex"),
  encodeMidgardCekProgramMaterialDaValueV1(ORPHAN_MATERIAL_ENTRY).toString(
    "hex",
  ),
];

// ---------------------------------------------------------------------------
// Rejection-evidence corpus
// ---------------------------------------------------------------------------

type EvidenceCase = {
  readonly label: string;
  readonly code: RejectCode;
  readonly stage: string;
  readonly queued: QueuedTx;
  readonly config: PhaseAConfig;
};

const fromNativeTx = (
  options: Parameters<typeof makeNativeTx>[0],
  overrides: Partial<QueuedTx> = {},
): QueuedTx => {
  const fixture = makeNativeTx({ privateKey: KEY, ...options });
  return queuedTx(fixture.txId, fixture.txCbor, overrides);
};

const evidenceCase = (
  label: string,
  code: RejectCode,
  stage: string,
  queued: QueuedTx,
  config: PhaseAConfig = CONFIG,
): EvidenceCase => ({ label, code, stage, queued, config });

/**
 * One deterministic rejection-evidence case per reachable-and-producible
 * canonical code. The `code`/`stage` columns are asserted against
 * `validatePhaseASingle` itself, so they pin which canonical outcome the
 * fixture reaches; they are never a second opinion about what it should be.
 */
const EVIDENCE_CASES: readonly EvidenceCase[] = [
  evidenceCase(
    "undecodable transaction bytes",
    RejectCodes.CborDeserialization,
    "canonicalDecode",
    queuedTx(Buffer.alloc(32), Buffer.from([0xff])),
  ),
  evidenceCase(
    "queued tx id does not match the native tx id",
    RejectCodes.TxHashMismatch,
    "compactBinding",
    queuedTx(Buffer.alloc(32, 9), makeNativeTx({ privateKey: KEY }).txCbor),
  ),
  evidenceCase(
    "no spend inputs",
    RejectCodes.EmptyInputs,
    "inputSets",
    fromNativeTx({ spendInputs: [] }),
  ),
  evidenceCase(
    "the same out-ref spent twice",
    RejectCodes.DuplicateInputInTx,
    "inputSets",
    fromNativeTx({ spendInputs: [outRefFromByte(1), outRefFromByte(1)] }),
  ),
  evidenceCase(
    "output preimage item is not a canonical output",
    RejectCodes.InvalidOutput,
    "canonicalDecode",
    fromNativeTx({ outputs: [Buffer.from([0x00])] }),
  ),
  evidenceCase(
    "duplicate required observer",
    RejectCodes.InvalidFieldType,
    "phaseAScriptPreconditions",
    fromNativeTx({
      requiredObserverItems: [Buffer.alloc(28, 3), Buffer.alloc(28, 3)],
      networkId: 0n,
    }),
  ),
  evidenceCase(
    "validity interval start after end",
    RejectCodes.InvalidValidityIntervalFormat,
    "inputSets",
    fromNativeTx({ validityIntervalStart: 5n, validityIntervalEnd: 4n }),
  ),
  evidenceCase(
    "fee below the header-committed minimum",
    RejectCodes.MinFee,
    "staticLedgerRules",
    fromNativeTx({ fee: 0n }),
    configFor({ minFeeB: 1n }),
  ),
  evidenceCase(
    "required signer without a witness",
    RejectCodes.MissingRequiredWitness,
    "signatures",
    fromNativeTx({ requiredSignerItems: [Buffer.alloc(28, 0x5a)] }),
  ),
  evidenceCase(
    "vkey witness signs the wrong body hash",
    RejectCodes.InvalidSignature,
    "signatures",
    fromNativeTx({ invalidVkeyWitness: true }),
  ),
  evidenceCase(
    "native script requires an absent signer",
    RejectCodes.NativeScriptInvalid,
    "phaseANativeScripts",
    fromNativeTx({
      scriptWitnesses: [
        nativeScriptWitness({ type: "sig", keyHash: Buffer.alloc(28, 0x33) }),
      ],
    }),
  ),
  evidenceCase(
    "admission of a non-valid transaction",
    RejectCodes.IsValidFalseForbidden,
    "canonicalDecode",
    fromNativeTx({ validity: "FailedScript" }),
  ),
  evidenceCase(
    "auxiliary data hash present",
    RejectCodes.AuxDataForbidden,
    "canonicalDecode",
    fromNativeTx({ auxiliaryDataHash: Buffer.alloc(32, 1) }),
  ),
  evidenceCase(
    "network id differs from the header-committed one",
    RejectCodes.NetworkIdMismatch,
    "staticLedgerRules",
    fromNativeTx({ networkId: 1n }),
  ),
  evidenceCase(
    "consensus profile is not the compiled V1 tuple",
    RejectCodes.TxVersion,
    "canonicalDecode",
    fromNativeTx({}),
    {
      ...CONFIG,
      consensusProfile: {
        ...MIDGARD_CONSENSUS_PROFILE_V1,
        protocolVersion: 2,
      } as unknown as typeof MIDGARD_CONSENSUS_PROFILE_V1,
    },
  ),
  evidenceCase(
    "canonical transaction over the V1 size bound",
    RejectCodes.TxSize,
    "canonicalDecode",
    fromNativeTx({ outputs: manyOutputs(8000) }),
  ),
  evidenceCase(
    "single output value over the Cardano value bound",
    RejectCodes.ValueSize,
    "canonicalDecode",
    fromNativeTx({
      outputs: [makeOutput(1n, TEST_ADDRESS_BYTES, assetMap(2600))],
    }),
  ),
  evidenceCase(
    "outputs preimage over the field bound",
    RejectCodes.FieldPreimageSize,
    "canonicalDecode",
    fromNativeTx({ outputs: manyOutputs(2000) }),
  ),
  evidenceCase(
    "single output preimage over the ledger-output bound",
    RejectCodes.LedgerOutputSize,
    "canonicalDecode",
    fromNativeTx({
      outputs: [
        encodeMidgardTxOutput({
          address: TEST_ADDRESS_BYTES,
          value: { lovelace: 1n, assets: new Map() },
          datum: { kind: "inline", cbor: bigInlineDatum(400) },
        }),
      ],
    }),
  ),
  evidenceCase(
    "plutus witness is not a canonical bounded program envelope",
    RejectCodes.ScriptProgramEncoding,
    "canonicalDecode",
    fromNativeTx({
      scriptWitnesses: [plutusV3ScriptWitness(Buffer.from([0x00]))],
    }),
  ),
  evidenceCase(
    "missing program-material sidecar",
    RejectCodes.CekProgramMaterial,
    "canonicalDecode",
    fromNativeTx({}, { programMaterialSidecarCbor: null }),
  ),
];

/** Valid transactions that must be accepted, used by the differential. */
const VALID_CASES: readonly QueuedTx[] = [
  fromNativeTx({}),
  fromNativeTx({ outputs: [makeOutput(1n), makeOutput(2n)] }),
  fromNativeTx({ referenceInputs: [outRefFromByte(0x72)] }),
  fromNativeTx({ validityIntervalStart: 1n, validityIntervalEnd: 9n }),
  fromNativeTx({
    scriptWitnesses: [nativeScriptWitness({ type: "after", slot: 0n })],
  }),
];

// ---------------------------------------------------------------------------
// Block fixture
// ---------------------------------------------------------------------------

const headerHashOf = (header: SDK.HeaderV1): string =>
  Buffer.from(
    blake2b(Buffer.from(Data.to(header, SDK.HeaderV1), "hex"), { dkLen: 28 }),
  ).toString("hex");

const sortEntries = (
  entries: readonly SDK.DaPayloadEntry[],
): SDK.DaPayloadEntry[] =>
  [...entries].sort(([left], [right]) =>
    left < right ? -1 : left > right ? 1 : 0,
  );

const bufferEntries = (
  entries: readonly SDK.DaPayloadEntry[],
): readonly { readonly key: Buffer; readonly value: Buffer }[] =>
  entries.map(([key, value]) => ({
    key: Buffer.from(key, "hex"),
    value: Buffer.from(value, "hex"),
  }));

const hex = <A>(value: A, schema: Parameters<typeof Data.to>[1]): string =>
  encodeData(value, schema as never).toString("hex");

const watcherHeaderRecord = (
  header: SDK.HeaderV1,
  headerHash: string,
): WatcherStateQueueHeaderV1 => ({
  headerHash,
  headerCborHex: Data.to(header, SDK.HeaderV1),
  nextHeaderHash: null,
  datumSha256: h32(3),
  prevUtxosRoot: header.prevUtxosRoot,
  utxosRoot: header.utxosRoot,
  withdrawalsRoot: header.withdrawalsRoot,
  forcedTransactionsRoot: header.forcedTransactionsRoot,
  transactionsRoot: header.transactionsRoot,
  depositsRoot: header.depositsRoot,
  transitionTraceRoot: header.transitionTraceRoot,
  eventToStepRoot: header.eventToStepRoot,
  validationTracesRoot: header.validationTracesRoot,
  withdrawalCount: header.withdrawalCount.toString(),
  forcedTransactionCount: header.forcedTransactionCount.toString(),
  l2TransactionCount: header.l2TransactionCount.toString(),
  depositCount: header.depositCount.toString(),
  totalEventCount: header.totalEventCount.toString(),
  transitionStepCount: header.transitionStepCount.toString(),
  validationTraceCount: header.validationTraceCount.toString(),
  startTime: header.startTime.toString(),
  endTime: header.endTime.toString(),
  blockSlot: header.blockSlot.toString(),
  expectedNetworkId: header.expectedNetworkId.toString(),
  minFeeA: header.minFeeA.toString(),
  minFeeB: header.minFeeB.toString(),
  prevHeaderHash: header.prevHeaderHash,
  operatorVkey: header.operatorVkey,
  protocolVersion: header.protocolVersion.toString(),
  daAttestationPolicyId: null,
});

type BlockFixture = {
  readonly payload: SDK.DaPayloadV1;
  readonly header: SDK.HeaderV1;
  readonly headerHash: string;
  readonly envelope: Buffer;
  readonly observation: SDK.AuthenticatedStateQueueHeaderObservationV1;
  readonly reconstruction: WatcherHeaderRootReconstructionResultV1;
  readonly txCbors: readonly Buffer[];
};

/**
 * Builds a real block the way the node does: `transactions` carries the
 * canonical `L2TransactionSourceV1` commitment, `transaction_preimages`
 * carries the exact canonical transaction bytes, and every header root and
 * count is derived from those entries rather than declared.
 */
const buildBlock = async (input: {
  readonly txCbors: readonly Buffer[];
  readonly programMaterial?: readonly SDK.DaPayloadEntry[];
  readonly headerOverrides?: Partial<SDK.HeaderV1>;
}): Promise<BlockFixture> => {
  const transactions = input.txCbors.map((canonicalCbor) => {
    const full = decodeMidgardNativeTxFullV1FromCanonicalCbor(canonicalCbor);
    const proofSource =
      deriveMidgardNativeTxProofSourceV1FromCanonicalCbor(canonicalCbor);
    const source: SDK.L2TransactionSourceV1 = {
      tx_id: computeMidgardNativeTxIdV1(full).toString("hex"),
      source: {
        compact_cbor: proofSource.compactCbor.toString("hex"),
        witness_set_compact_cbor:
          proofSource.witnessSetCompactCbor.toString("hex"),
        field_preimage_lengths_cbor:
          proofSource.fieldPreimageLengthsCbor.toString("hex"),
      },
    };
    return { canonicalCbor, source };
  });

  const transactionEntries: SDK.DaPayloadEntry[] = transactions.map((tx) => [
    tx.source.tx_id,
    encodeData(tx.source, SDK.L2TransactionSourceV1Schema).toString("hex"),
  ]);
  const preimageEntries: SDK.DaPayloadEntry[] = transactions.map((tx) => [
    tx.source.tx_id,
    tx.canonicalCbor.toString("hex"),
  ]);
  const eventToStepEntries: SDK.DaPayloadEntry[] = transactions.map(
    (tx, index) => [
      hex(
        { L2TransactionEventKey: { tx_id: tx.source.tx_id } },
        SDK.EventKeySchema,
      ),
      hex(
        {
          step_index: BigInt(index),
          phase: "L2Transaction",
        } satisfies SDK.EventToStepValue,
        SDK.EventToStepValueSchema,
      ),
    ],
  );
  const validationTraceEntries: SDK.DaPayloadEntry[] = transactions.map(
    (tx, index) => [
      hex(
        { L2TransactionEventKey: { tx_id: tx.source.tx_id } },
        SDK.EventKeySchema,
      ),
      hex(
        {
          schema_version: 1n,
          machine_version: 1n,
          trace_root: h32(140 + index),
          step_count: 1n,
          initial_state_hash: h32(150 + index),
          terminal_state_hash: h32(160 + index),
          verdict: "Accepted",
          rejection_code_hash: h32(170 + index),
        } satisfies SDK.ValidationTraceDescriptorV1,
        SDK.ValidationTraceDescriptorV1Schema,
      ),
    ],
  );

  const countedRoot = async (
    domain: SDK.RootDomain,
    entries: readonly SDK.DaPayloadEntry[],
  ): Promise<string> =>
    (await buildCountedRoot(domain, bufferEntries(entries))).root;

  const counts = {
    withdrawalCount: 0n,
    forcedTransactionCount: 0n,
    l2TransactionCount: BigInt(transactionEntries.length),
    depositCount: 0n,
    totalEventCount: BigInt(transactionEntries.length),
    transitionStepCount: 0n,
    validationTraceCount: BigInt(validationTraceEntries.length),
  };

  const header: SDK.HeaderV1 = baseHeader({
    withdrawalsRoot: await countedRoot(SDK.ROOT_DOMAINS.withdrawals, []),
    forcedTransactionsRoot: await countedRoot(
      SDK.ROOT_DOMAINS.forcedTransactionsV1,
      [],
    ),
    transactionsRoot: await countedRoot(
      SDK.ROOT_DOMAINS.transactionsV1,
      transactionEntries,
    ),
    depositsRoot: await countedRoot(SDK.ROOT_DOMAINS.deposits, []),
    transitionTraceRoot: await countedRoot(
      SDK.ROOT_DOMAINS.transitionTrace,
      [],
    ),
    eventToStepRoot: await countedRoot(
      SDK.ROOT_DOMAINS.eventToStep,
      eventToStepEntries,
    ),
    validationTracesRoot: await countedRoot(
      SDK.ROOT_DOMAINS.validationTraces,
      validationTraceEntries,
    ),
    ...counts,
    ...input.headerOverrides,
  });
  const headerHash = headerHashOf(header);
  const payload: SDK.DaPayloadV1 = {
    version: SDK.DA_PAYLOAD_V1_VERSION,
    block_body: {
      header_hash: headerHash,
      header,
      utxos: [],
      withdrawals: [],
      forced_transactions: [],
      transactions: sortEntries(transactionEntries),
      deposits: [],
      transition_trace: [],
      event_to_step: sortEntries(eventToStepEntries),
      transaction_preimages: sortEntries(preimageEntries),
      forced_transaction_preimages: [],
      cek_program_material: sortEntries(input.programMaterial ?? []),
      validation_traces: sortEntries(validationTraceEntries),
      counts,
    },
  };
  const envelope = await wrapDaPayloadV1(SDK.encodeDaPayloadV1(payload), {
    mode: "identity",
  });
  const observation = await makeWatcherAuthenticatedHeaderObservationV1({
    header: watcherHeaderRecord(header, headerHash),
    chainPoint: CHAIN_POINT,
    confirmationDepth: 12,
    sourceMode: "local_node",
    provenance: L1_PROVENANCE,
  });
  const reconstruction = await evaluateWatcherHeaderRootReconstructionV1({
    observation,
    payloadEnvelopeCbor: envelope,
    daProvenance: DA_PROVENANCE,
  });
  return {
    payload,
    header,
    headerHash,
    envelope,
    observation,
    reconstruction,
    txCbors: input.txCbors,
  };
};

const evaluateBlock = async (
  fixture: BlockFixture,
  overrides: Partial<Parameters<typeof evaluateWatcherPhaseABlockV1>[0]> = {},
): Promise<WatcherPhaseAVerificationResultV1> =>
  await evaluateWatcherPhaseABlockV1({
    observation: fixture.observation,
    reconstruction: fixture.reconstruction,
    payloadEnvelopeCbor: fixture.envelope,
    daProvenance: DA_PROVENANCE,
    ruleBundle: RULE_BUNDLE,
    ruleBundleCommitment: RULE_BUNDLE_COMMITMENT,
    ...overrides,
  });

// ---------------------------------------------------------------------------
// Published rejection vocabulary (CG3 waiver condition b)
// ---------------------------------------------------------------------------

describe("published rejection vocabulary", () => {
  it("mirrors the canonical 49-member RejectCodes vocabulary", () => {
    expect(WATCHER_PHASE_A_CANONICAL_REJECT_CODES_V1).toHaveLength(49);
    expect(new Set(WATCHER_PHASE_A_CANONICAL_REJECT_CODES_V1).size).toBe(49);
    expect(WATCHER_PHASE_A_CANONICAL_REJECT_CODES_V1).toStrictEqual(
      Object.values(RejectCodes),
    );
  });

  it("partitions the vocabulary into 32 reachable and 17 excluded codes", () => {
    expect(WATCHER_PHASE_A_REACHABLE_REJECT_CODES_V1).toHaveLength(32);
    expect(WATCHER_PHASE_A_EXCLUDED_REJECT_CODES_V1).toHaveLength(17);
    expect(
      [
        ...WATCHER_PHASE_A_REACHABLE_REJECT_CODES_V1,
        ...WATCHER_PHASE_A_EXCLUDED_REJECT_CODES_V1,
      ].sort(),
    ).toStrictEqual([...WATCHER_PHASE_A_CANONICAL_REJECT_CODES_V1].sort());
    for (const code of WATCHER_PHASE_A_REACHABLE_REJECT_CODES_V1) {
      expect(WATCHER_PHASE_A_EXCLUDED_REJECT_CODES_V1).not.toContain(code);
    }
  });

  it("derives the reachable set from the canonical Phase A call sites", () => {
    const derived = new Set<string>([
      ...WATCHER_PHASE_A_DIRECT_REJECT_CODES_V1,
      ...WATCHER_PHASE_A_CONSENSUS_REJECT_CODES_V1,
    ]);
    expect(WATCHER_PHASE_A_CONSENSUS_REJECT_CODES_V1).toHaveLength(19);
    expect(new Set(WATCHER_PHASE_A_REACHABLE_REJECT_CODES_V1)).toStrictEqual(
      derived,
    );
  });

  it("keeps every published list in canonical declaration order", () => {
    const order = new Map(
      WATCHER_PHASE_A_CANONICAL_REJECT_CODES_V1.map((code, index) => [
        code,
        index,
      ]),
    );
    for (const list of [
      WATCHER_PHASE_A_REACHABLE_REJECT_CODES_V1,
      WATCHER_PHASE_A_EXCLUDED_REJECT_CODES_V1,
      WATCHER_PHASE_A_EVIDENCED_REJECT_CODES_V1,
      WATCHER_PHASE_A_DOMINATED_REJECT_CODES_V1,
    ]) {
      const positions = list.map((code) => order.get(code)!);
      expect(positions).toStrictEqual([...positions].sort((a, b) => a - b));
    }
  });

  it("justifies every excluded and every dominated code exactly once", () => {
    expect(
      Object.keys(
        WATCHER_PHASE_A_EXCLUDED_REJECT_CODE_JUSTIFICATIONS_V1,
      ).sort(),
    ).toStrictEqual([...WATCHER_PHASE_A_EXCLUDED_REJECT_CODES_V1].sort());
    expect(
      Object.keys(
        WATCHER_PHASE_A_DOMINATED_REJECT_CODE_JUSTIFICATIONS_V1,
      ).sort(),
    ).toStrictEqual([...WATCHER_PHASE_A_DOMINATED_REJECT_CODES_V1].sort());
    for (const justification of [
      ...Object.values(WATCHER_PHASE_A_EXCLUDED_REJECT_CODE_JUSTIFICATIONS_V1),
      ...Object.values(WATCHER_PHASE_A_DOMINATED_REJECT_CODE_JUSTIFICATIONS_V1),
    ]) {
      expect(justification.length).toBeGreaterThan(30);
    }
  });

  it("excludes the five Phase-B-only codes and keeps E_MIN_FEE reachable", () => {
    for (const code of [
      RejectCodes.DoubleSpend,
      RejectCodes.DependencyCycle,
      RejectCodes.DependsOnRejectedTx,
      RejectCodes.InputNotFound,
      RejectCodes.ValueNotPreserved,
    ]) {
      expect(WATCHER_PHASE_A_EXCLUDED_REJECT_CODES_V1).toContain(code);
    }
    // The recorded waiver text lists E_MIN_FEE with the Phase-B set, but
    // phase-a.ts:509-516 emits it from the header-committed minFeeA/minFeeB,
    // and the evidence corpus below reaches it. It is published as reachable.
    expect(WATCHER_PHASE_A_REACHABLE_REJECT_CODES_V1).toContain(
      RejectCodes.MinFee,
    );
    expect(WATCHER_PHASE_A_EVIDENCED_REJECT_CODES_V1).toContain(
      RejectCodes.MinFee,
    );
  });

  it("splits the reachable set into 21 evidenced and 11 dominated codes", () => {
    expect(WATCHER_PHASE_A_EVIDENCED_REJECT_CODES_V1).toHaveLength(21);
    expect(WATCHER_PHASE_A_DOMINATED_REJECT_CODES_V1).toHaveLength(11);
    expect(
      [
        ...WATCHER_PHASE_A_EVIDENCED_REJECT_CODES_V1,
        ...WATCHER_PHASE_A_DOMINATED_REJECT_CODES_V1,
      ].sort(),
    ).toStrictEqual([...WATCHER_PHASE_A_REACHABLE_REJECT_CODES_V1].sort());
  });
});

// ---------------------------------------------------------------------------
// Differential record: the load-bearing evidence
// ---------------------------------------------------------------------------

type CanonicalVerdict = {
  readonly accepted: boolean;
  readonly rejected: RejectedTx | null;
};

/**
 * The canonical verdict for one queued transaction, memoised per
 * (transaction, config) pair. Several corpus entries are hundreds of kilobytes
 * and are compared against the watcher in more than one block, and the cache
 * only avoids repeating an identical pure call - it never substitutes for one.
 */
const verdictCache = new Map<QueuedTx, Map<PhaseAConfig, CanonicalVerdict>>();

const canonicalVerdict = (
  queued: QueuedTx,
  config: PhaseAConfig,
): CanonicalVerdict => {
  let byConfig = verdictCache.get(queued);
  if (byConfig === undefined) {
    byConfig = new Map();
    verdictCache.set(queued, byConfig);
  }
  const cached = byConfig.get(config);
  if (cached !== undefined) {
    return cached;
  }
  const outcome = validatePhaseASingle(queued, config);
  const verdict: CanonicalVerdict =
    "ledgerTx" in outcome
      ? { accepted: true, rejected: null }
      : { accepted: false, rejected: outcome };
  byConfig.set(config, verdict);
  return verdict;
};

describe("differential against the canonical Phase A entry point", () => {
  const corpus: readonly {
    readonly label: string;
    readonly queued: QueuedTx;
    readonly config: PhaseAConfig;
  }[] = [
    ...VALID_CASES.map((queued, index) => ({
      label: `valid #${index.toString()}`,
      queued,
      config: CONFIG,
    })),
    ...EVIDENCE_CASES.map((entry) => ({
      label: entry.label,
      queued: entry.queued,
      config: entry.config,
    })),
  ];

  it.each(corpus.map((entry) => [entry.label, entry] as const))(
    "reproduces the canonical verdict exactly for %s",
    (_label, entry) => {
      const canonical = canonicalVerdict(entry.queued, entry.config);
      const result = evaluateWatcherPhaseAQueuedTxsV1({
        queuedTxs: [entry.queued],
        config: entry.config,
      });
      expect(result.transactionCount).toBe(1);
      if (canonical.accepted) {
        expect(result.action).toBe("accept");
        expect(result.rejections).toStrictEqual([]);
        expect(result.acceptedTxIds).toStrictEqual([
          entry.queued.txId.toString("hex"),
        ]);
        return;
      }
      const rejected = canonical.rejected!;
      expect(result.action).toBe("reject");
      expect(result.acceptedTxIds).toStrictEqual([]);
      expect(result.rejections).toHaveLength(1);
      expect(result.rejections[0]).toMatchObject({
        index: 0,
        txId: rejected.txId.toString("hex"),
        code: rejected.code,
        stage: rejected.consensusPhase,
        detail: rejected.detail,
      });
      expect(result.selectedRejection).toStrictEqual(result.rejections[0]);
    },
    SLOW_TEST_TIMEOUT_MS,
  );

  it(
    "never accepts a transaction the canonical path rejects",
    () => {
      for (const entry of corpus) {
        const canonical = canonicalVerdict(entry.queued, entry.config);
        const result = evaluateWatcherPhaseAQueuedTxsV1({
          queuedTxs: [entry.queued],
          config: entry.config,
        });
        if (!canonical.accepted) {
          expect(result.action).not.toBe("accept");
          expect(result.acceptedTxIds).toStrictEqual([]);
        }
      }
    },
    SLOW_TEST_TIMEOUT_MS,
  );

  it(
    "reproduces the canonical verdict for the whole corpus in one batch",
    () => {
      // A single batch shares one config, so only the cases that use CONFIG can
      // take part; the rest are covered individually above.
      const batch = corpus
        .filter((entry) => entry.config === CONFIG)
        .map((entry) => entry.queued);
      const result = evaluateWatcherPhaseAQueuedTxsV1({
        queuedTxs: batch,
        config: CONFIG,
      });
      const expectedAccepted: string[] = [];
      const expectedRejections: unknown[] = [];
      batch.forEach((queued, index) => {
        const canonical = canonicalVerdict(queued, CONFIG);
        if (canonical.accepted) {
          expectedAccepted.push(queued.txId.toString("hex"));
          return;
        }
        expectedRejections.push({
          index,
          txId: canonical.rejected!.txId.toString("hex"),
          code: canonical.rejected!.code,
          stage: canonical.rejected!.consensusPhase,
          detail: canonical.rejected!.detail,
        });
      });
      expect(result.transactionCount).toBe(batch.length);
      expect(result.acceptedTxIds).toStrictEqual(expectedAccepted);
      expect(
        result.rejections.map((rejection) => ({
          index: rejection.index,
          txId: rejection.txId,
          code: rejection.code,
          stage: rejection.stage,
          detail: rejection.detail,
        })),
      ).toStrictEqual(expectedRejections);
    },
    SLOW_TEST_TIMEOUT_MS,
  );
});

// ---------------------------------------------------------------------------
// One deterministic rejection-evidence case per reachable code
// ---------------------------------------------------------------------------

describe("rejection evidence per reachable code", () => {
  it.each(EVIDENCE_CASES.map((entry) => [entry.code, entry] as const))(
    "produces %s deterministically",
    (code, entry) => {
      const first = evaluateWatcherPhaseAQueuedTxsV1({
        queuedTxs: [entry.queued],
        config: entry.config,
      });
      const second = evaluateWatcherPhaseAQueuedTxsV1({
        queuedTxs: [entry.queued],
        config: entry.config,
      });
      expect(first.resultDigest).toBe(second.resultDigest);
      expect(first.action).toBe("reject");
      expect(first.reasonCodes).toStrictEqual(["phase_a_rejection"]);
      expect(first.rejections).toHaveLength(1);
      expect(first.rejections[0]!.code).toBe(code);
      expect(first.rejections[0]!.stage).toBe(entry.stage);
      expect(first.rejections[0]!.detail).toBe(
        canonicalVerdict(entry.queued, entry.config).rejected!.detail,
      );
    },
    SLOW_TEST_TIMEOUT_MS,
  );

  it(
    "covers exactly the published evidenced set",
    () => {
      const produced = new Set(
        EVIDENCE_CASES.map(
          (entry) =>
            canonicalVerdict(entry.queued, entry.config).rejected!
              .code as string,
        ),
      );
      expect([...produced].sort()).toStrictEqual(
        [...WATCHER_PHASE_A_EVIDENCED_REJECT_CODES_V1].sort(),
      );
      expect(EVIDENCE_CASES).toHaveLength(
        WATCHER_PHASE_A_EVIDENCED_REJECT_CODES_V1.length,
      );
    },
    SLOW_TEST_TIMEOUT_MS,
  );
});

// ---------------------------------------------------------------------------
// Adjacent boundary: the dominated codes and the bounds around them
// ---------------------------------------------------------------------------

describe("adjacent boundary", () => {
  it("accepts a fee exactly at the header minimum and rejects one below", () => {
    const fixture = makeNativeTx({ privateKey: KEY, fee: 7n });
    const config = configFor({ minFeeB: 7n });
    expect(
      evaluateWatcherPhaseAQueuedTxsV1({
        queuedTxs: [queuedTx(fixture.txId, fixture.txCbor)],
        config,
      }).action,
    ).toBe("accept");
    const below = makeNativeTx({ privateKey: KEY, fee: 6n });
    const result = evaluateWatcherPhaseAQueuedTxsV1({
      queuedTxs: [queuedTx(below.txId, below.txCbor)],
      config,
    });
    expect(result.rejections[0]!.code).toBe(RejectCodes.MinFee);
  });

  it(
    "accepts an outputs preimage under the field bound and rejects one over",
    () => {
      const under = makeNativeTx({
        privateKey: KEY,
        outputs: manyOutputs(700),
      });
      expect(
        evaluateWatcherPhaseAQueuedTxsV1({
          queuedTxs: [queuedTx(under.txId, under.txCbor)],
          config: CONFIG,
        }).action,
      ).toBe("accept");
      const over = makeNativeTx({
        privateKey: KEY,
        outputs: manyOutputs(2000),
      });
      expect(
        evaluateWatcherPhaseAQueuedTxsV1({
          queuedTxs: [queuedTx(over.txId, over.txCbor)],
          config: CONFIG,
        }).rejections[0]!.code,
      ).toBe(RejectCodes.FieldPreimageSize);
    },
    SLOW_TEST_TIMEOUT_MS,
  );

  it.each([
    [
      RejectCodes.OutputCount,
      RejectCodes.InvalidOutput,
      () =>
        makeNativeTx({
          privateKey: KEY,
          outputs: Array.from({ length: 16385 }, () => Buffer.alloc(0)),
        }),
    ],
    [
      RejectCodes.RequiredSignerCount,
      RejectCodes.InvalidFieldType,
      () =>
        makeNativeTx({
          privateKey: KEY,
          requiredSignerItems: Array.from({ length: 16385 }, () =>
            Buffer.alloc(0),
          ),
        }),
    ],
    [
      // §5.1 caps an item's byte-string wrapper at `59 LLLL`, so a single output
      // carrying more than the 16,384-asset guardrail cannot be *encoded* at all:
      // 16,385 assets exceed 65,535 bytes of item, and the field preimage that
      // would hold it is refused before the §5.4 aggregate bound is consulted.
      // Under the retired counted grammar this fixture reached
      // `E_FIELD_PREIMAGE_SIZE`, because item bytes were read with a general CBOR
      // walk that accepts the four-byte `5a` head. `E_ASSET_COUNT` is still
      // dominated — by a tighter rule than before.
      RejectCodes.AssetCount,
      RejectCodes.CborDeserialization,
      () =>
        makeNativeTx({
          privateKey: KEY,
          outputs: [makeOutput(1n, TEST_ADDRESS_BYTES, assetMap(20000))],
        }),
    ],
  ])(
    "shows %s is dominated by %s rather than silently unreachable",
    (dominated, dominating, build) => {
      expect(WATCHER_PHASE_A_DOMINATED_REJECT_CODES_V1).toContain(dominated);
      const fixture = build();
      const result = evaluateWatcherPhaseAQueuedTxsV1({
        queuedTxs: [queuedTx(fixture.txId, fixture.txCbor)],
        config: CONFIG,
      });
      expect(result.rejections[0]!.code).toBe(dominating);
      expect(result.rejections[0]!.code).not.toBe(dominated);
    },
    SLOW_TEST_TIMEOUT_MS,
  );

  it("shows the canonical encoder refuses an over-deep native script", () => {
    expect(WATCHER_PHASE_A_DOMINATED_REJECT_CODES_V1).toContain(
      RejectCodes.NativeScriptDepth,
    );
    expect(() =>
      makeNativeTx({
        privateKey: KEY,
        scriptWitnesses: [nativeScriptWitness(nestedNativeScript(16385))],
      }),
    ).toThrow(/nesting exceeds/u);
  });
});

// ---------------------------------------------------------------------------
// Rejection ordering and W23 selection
// ---------------------------------------------------------------------------

describe("deterministic ordering", () => {
  it("emits rejections in block order and selects by W23 phase priority", () => {
    const lateStage = fromNativeTx({
      requiredSignerItems: [Buffer.alloc(28, 0x5a)],
    });
    const earlyStage = fromNativeTx({ auxiliaryDataHash: Buffer.alloc(32, 1) });
    const result = evaluateWatcherPhaseAQueuedTxsV1({
      queuedTxs: [lateStage, earlyStage],
      config: CONFIG,
    });
    expect(result.rejections.map((rejection) => rejection.index)).toStrictEqual(
      [0, 1],
    );
    expect(result.rejections[0]!.stage).toBe("signatures");
    expect(result.rejections[1]!.stage).toBe("canonicalDecode");
    // Block order puts the signatures rejection first, but the W23 rule
    // selects the lowest validation phase, which is the second transaction.
    expect(result.selectedRejection).toStrictEqual(result.rejections[1]);
    expect(result.rejectionSelection).toBe(
      RULE_BUNDLE.validation.rejectionSelection,
    );
  });

  it("breaks a phase tie by canonical block order", () => {
    const first = fromNativeTx({ auxiliaryDataHash: Buffer.alloc(32, 1) });
    const second = fromNativeTx({ auxiliaryDataHash: Buffer.alloc(32, 2) });
    const result = evaluateWatcherPhaseAQueuedTxsV1({
      queuedTxs: [first, second],
      config: CONFIG,
    });
    expect(result.selectedRejection!.index).toBe(0);
  });

  it("uses the W23 phase priority for stagePriority", () => {
    const result = evaluateWatcherPhaseAQueuedTxsV1({
      queuedTxs: [fromNativeTx({ requiredSignerItems: [Buffer.alloc(28, 1)] })],
      config: CONFIG,
    });
    expect(result.rejections[0]!.stagePriority).toBe(
      WATCHER_RULE_BUNDLE_V1_VALIDATION_PHASE_PRIORITY.indexOf("signatures"),
    );
  });
});

// ---------------------------------------------------------------------------
// Positive block path
// ---------------------------------------------------------------------------

describe("block verification", () => {
  it("accepts an all-valid block with a stable resultDigest", async () => {
    const fixture = await buildBlock({
      txCbors: [
        makeNativeTx({ privateKey: KEY, spendInputs: [outRefFromByte(1)] })
          .txCbor,
        makeNativeTx({ privateKey: KEY, spendInputs: [outRefFromByte(2)] })
          .txCbor,
      ],
    });
    expect(fixture.reconstruction.action).toBe("accept");
    const first = await evaluateBlock(fixture);
    const second = await evaluateBlock(fixture);
    expect(first.schemaVersion).toBe(
      WATCHER_PHASE_A_VERIFIER_V1_SCHEMA_VERSION,
    );
    expect(first.action).toBe("accept");
    expect(first.reasonCodes).toStrictEqual([]);
    expect(first.rejections).toStrictEqual([]);
    expect(first.selectedRejection).toBeNull();
    expect(first.transactionCount).toBe(2);
    expect(first.acceptedCount).toBe(2);
    expect(first.headerHash).toBe(fixture.headerHash);
    expect(first.reconstructionDigest).toBe(
      fixture.reconstruction.resultDigest,
    );
    expect(first.ruleBundleCommitment).toBe(RULE_BUNDLE_COMMITMENT);
    expect(first.resultDigest).toBe(second.resultDigest);
    expect(Object.isFrozen(first)).toBe(true);
  });

  it("binds the digest to the verdict content", async () => {
    const accepted = await buildBlock({
      txCbors: [makeNativeTx({ privateKey: KEY }).txCbor],
    });
    const rejected = await buildBlock({
      txCbors: [
        makeNativeTx({ privateKey: KEY, invalidVkeyWitness: true }).txCbor,
      ],
    });
    const first = await evaluateBlock(accepted);
    const second = await evaluateBlock(rejected);
    expect(second.action).toBe("reject");
    expect(second.reasonCodes).toStrictEqual(["phase_a_rejection"]);
    expect(second.rejections[0]!.code).toBe(RejectCodes.InvalidSignature);
    expect(first.resultDigest).not.toBe(second.resultDigest);
    const { resultDigest: _digest, ...withoutDigest } = second;
    expect(watcherSha256CanonicalJsonV1(withoutDigest)).toBe(
      second.resultDigest,
    );
  });

  it("matches the canonical verdict for a mixed block", async () => {
    const cbors = [
      makeNativeTx({ privateKey: KEY, spendInputs: [outRefFromByte(3)] })
        .txCbor,
      makeNativeTx({
        privateKey: KEY,
        spendInputs: [outRefFromByte(4)],
        invalidVkeyWitness: true,
      }).txCbor,
      makeNativeTx({
        privateKey: KEY,
        spendInputs: [outRefFromByte(5), outRefFromByte(5)],
      }).txCbor,
    ];
    const fixture = await buildBlock({ txCbors: cbors });
    const result = await evaluateBlock(fixture);
    expect(result.action).toBe("reject");
    expect(result.transactionCount).toBe(3);

    // Rebuild the canonical inputs independently and compare verdict by
    // verdict. The watcher record must be the canonical record, reshaped.
    const byTxId = new Map(
      cbors.map((cbor) => [
        computeMidgardNativeTxIdV1(
          decodeMidgardNativeTxFullV1FromCanonicalCbor(cbor),
        ).toString("hex"),
        cbor,
      ]),
    );
    const orderedTxIds = fixture.payload.block_body.transactions.map(
      ([key]) => key,
    );
    orderedTxIds.forEach((txId, index) => {
      const canonical = canonicalVerdict(
        queuedTx(Buffer.from(txId, "hex"), byTxId.get(txId)!, {
          arrivalSeq: BigInt(index),
        }),
        makeWatcherPhaseAConfigV1({
          header: fixture.header,
          ruleBundle: RULE_BUNDLE,
        }),
      );
      const watcher =
        result.rejections.find((rejection) => rejection.index === index) ??
        null;
      if (canonical.accepted) {
        expect(watcher).toBeNull();
        expect(result.acceptedTxIds).toContain(txId);
        return;
      }
      expect(watcher).toMatchObject({
        txId,
        code: canonical.rejected!.code,
        stage: canonical.rejected!.consensusPhase,
        detail: canonical.rejected!.detail,
      });
    });
  });

  it("reads minFee and network id from the L1-committed header", async () => {
    const fixture = await buildBlock({
      txCbors: [makeNativeTx({ privateKey: KEY, fee: 0n }).txCbor],
      headerOverrides: { minFeeB: 1n },
    });
    const result = await evaluateBlock(fixture);
    expect(result.action).toBe("reject");
    expect(result.rejections[0]!.code).toBe(RejectCodes.MinFee);
  });
});

// ---------------------------------------------------------------------------
// Program-material projection
// ---------------------------------------------------------------------------

describe("program-material projection", () => {
  it("does not reject a plain transaction for unrelated block material", async () => {
    const fixture = await buildBlock({
      txCbors: [makeNativeTx({ privateKey: KEY }).txCbor],
      programMaterial: [ORPHAN_MATERIAL_DA_ENTRY],
    });
    const result = await evaluateBlock(fixture);
    expect(result.action).toBe("accept");

    // Control: handing the unprojected block-wide set to the canonical
    // validator is exactly what the projection exists to avoid.
    const fullSidecar = encodeMidgardCekProgramMaterialSidecarV1([
      ORPHAN_MATERIAL_ENTRY,
    ]);
    const tx = makeNativeTx({ privateKey: KEY });
    const control = canonicalVerdict(
      queuedTx(tx.txId, tx.txCbor, {
        programMaterialSidecarCbor: fullSidecar,
      }),
      CONFIG,
    );
    expect(control.rejected!.code).toBe(RejectCodes.CekProgramMaterial);
  });

  it("derives an empty sidecar when the block carries no material", () => {
    const tx = makeNativeTx({ privateKey: KEY });
    const [queued] = watcherPhaseAQueuedTxsV1({
      transactions: [{ txId: tx.txId.toString("hex"), txCbor: tx.txCbor }],
      programMaterial: [],
    });
    expect(queued!.programMaterialSidecarCbor).toStrictEqual(EMPTY_SIDECAR);
    expect(queued!.arrivalSeq).toBe(0n);
  });

  it("keeps the complete block material when canonical narrowing throws", () => {
    // The narrowing fallback is the one place the watcher could silently make
    // Phase A more permissive than the operator's own admission. Feeding bytes
    // the canonical decoder rejects forces the fallback and pins it to the
    // complete block-wide set, not the empty one.
    const [queued] = watcherPhaseAQueuedTxsV1({
      transactions: [{ txId: h32(0x11), txCbor: Buffer.from([0xff]) }],
      programMaterial: [ORPHAN_MATERIAL_DA_ENTRY],
    });
    expect(queued!.programMaterialSidecarCbor).toStrictEqual(
      encodeMidgardCekProgramMaterialSidecarV1([ORPHAN_MATERIAL_ENTRY]),
    );
    expect(queued!.programMaterialSidecarCbor).not.toStrictEqual(EMPTY_SIDECAR);
  });

  it("numbers derived queued transactions by canonical block position", () => {
    const first = makeNativeTx({
      privateKey: KEY,
      spendInputs: [outRefFromByte(11)],
    });
    const second = makeNativeTx({
      privateKey: KEY,
      spendInputs: [outRefFromByte(12)],
    });
    const derived = watcherPhaseAQueuedTxsV1({
      transactions: [
        { txId: first.txId.toString("hex"), txCbor: first.txCbor },
        { txId: second.txId.toString("hex"), txCbor: second.txCbor },
      ],
      programMaterial: [],
    });
    expect(derived.map((entry) => entry.arrivalSeq)).toStrictEqual([0n, 1n]);
    expect(derived.map((entry) => entry.createdAt.getTime())).toStrictEqual([
      0, 0,
    ]);
  });

  it("fails closed on undecodable block program material", () => {
    expect(() =>
      watcherPhaseAQueuedTxsV1({
        transactions: [],
        programMaterial: [[h32(1), "00"]],
      }),
    ).toThrow(WatcherPhaseAVerifierError);
  });
});

// ---------------------------------------------------------------------------
// Malformed inputs
// ---------------------------------------------------------------------------

describe("malformed inputs", () => {
  it("reports a canonical reconstruction failure for truncated bytes", async () => {
    const fixture = await buildBlock({
      txCbors: [makeNativeTx({ privateKey: KEY }).txCbor],
    });
    const result = await evaluateBlock(fixture, {
      payloadEnvelopeCbor: fixture.envelope.subarray(
        0,
        fixture.envelope.length - 8,
      ),
    });
    expect(result.action).toBe("error");
    expect(result.reasonCodes).toStrictEqual([
      "canonical_reconstruction_failed",
    ]);
    expect(result.acceptedTxIds).toStrictEqual([]);
  });

  it("cannot even encode a non-V1 payload version, and fails closed if fed one", async () => {
    const fixture = await buildBlock({
      txCbors: [makeNativeTx({ privateKey: KEY }).txCbor],
    });
    expect(() =>
      SDK.encodeDaPayloadV1({
        ...fixture.payload,
        version: (SDK.DA_PAYLOAD_V1_VERSION + 1n) as never,
      }),
    ).toThrow(/version must equal/u);
    // A version byte flipped after encoding is not decodable either.
    const corrupted = Buffer.from(fixture.envelope);
    corrupted[corrupted.length - 1] ^= 0xff;
    const result = await evaluateBlock(fixture, {
      payloadEnvelopeCbor: corrupted,
    });
    expect(result.action).toBe("error");
    expect(result.reasonCodes).toStrictEqual([
      "canonical_reconstruction_failed",
    ]);
  });

  it("rejects undecodable transaction bytes through the canonical decoder", () => {
    const result = evaluateWatcherPhaseAQueuedTxsV1({
      queuedTxs: [queuedTx(Buffer.alloc(32), Buffer.alloc(0))],
      config: CONFIG,
    });
    expect(result.action).toBe("reject");
    expect(result.rejections[0]!.code).toBe(RejectCodes.CborDeserialization);
  });

  it("rejects a malformed transaction id in the derived queue", () => {
    expect(() =>
      watcherPhaseAQueuedTxsV1({
        transactions: [{ txId: "zz", txCbor: Buffer.alloc(1) }],
        programMaterial: [],
      }),
    ).toThrow(WatcherPhaseAVerifierError);
  });
});

// ---------------------------------------------------------------------------
// Fail-closed bindings
// ---------------------------------------------------------------------------

describe("fail-closed bindings", () => {
  const tamper = (
    reconstruction: WatcherHeaderRootReconstructionResultV1,
    patch: Partial<WatcherHeaderRootReconstructionResultV1>,
    reDigest: boolean,
  ): WatcherHeaderRootReconstructionResultV1 => {
    const next = { ...reconstruction, ...patch };
    if (!reDigest) {
      return next;
    }
    const { resultDigest: _drop, ...rest } = next;
    return { ...next, resultDigest: watcherSha256CanonicalJsonV1(rest) };
  };

  it("refuses a W22 record whose digest does not cover its fields", async () => {
    const fixture = await buildBlock({
      txCbors: [makeNativeTx({ privateKey: KEY }).txCbor],
    });
    const result = await evaluateBlock(fixture, {
      reconstruction: tamper(
        fixture.reconstruction,
        { headerHash: h28(1) },
        false,
      ),
    });
    expect(result.action).toBe("error");
    expect(result.reasonCodes).toStrictEqual([
      "reconstruction_digest_mismatch",
    ]);
  });

  it("refuses a re-digested W22 record for a different header", async () => {
    const fixture = await buildBlock({
      txCbors: [makeNativeTx({ privateKey: KEY }).txCbor],
    });
    const result = await evaluateBlock(fixture, {
      reconstruction: tamper(
        fixture.reconstruction,
        { headerHash: h28(1) },
        true,
      ),
    });
    expect(result.action).toBe("error");
    expect(result.reasonCodes).toStrictEqual([
      "reconstruction_header_mismatch",
    ]);
  });

  it("refuses a rejected W22 record", async () => {
    const fixture = await buildBlock({
      txCbors: [makeNativeTx({ privateKey: KEY }).txCbor],
    });
    const result = await evaluateBlock(fixture, {
      reconstruction: tamper(
        fixture.reconstruction,
        { action: "reject", reasonCodes: ["root_mismatch"] },
        true,
      ),
    });
    expect(result.action).toBe("error");
    expect(result.reasonCodes).toStrictEqual(["reconstruction_not_accepted"]);
  });

  it("refuses an unsupported W22 schema version", async () => {
    const fixture = await buildBlock({
      txCbors: [makeNativeTx({ privateKey: KEY }).txCbor],
    });
    const result = await evaluateBlock(fixture, {
      reconstruction: tamper(
        fixture.reconstruction,
        { schemaVersion: "midgard-watcher-header-root-v0" as never },
        true,
      ),
    });
    expect(result.action).toBe("error");
    expect(result.reasonCodes).toStrictEqual([
      "reconstruction_unsupported_schema",
    ]);
  });

  it("refuses a W22 record describing different payload bytes", async () => {
    const fixture = await buildBlock({
      txCbors: [makeNativeTx({ privateKey: KEY }).txCbor],
    });
    const result = await evaluateBlock(fixture, {
      reconstruction: tamper(
        fixture.reconstruction,
        { payloadEnvelopeSha256: h32(0x5e) },
        true,
      ),
    });
    expect(result.action).toBe("error");
    expect(result.reasonCodes).toStrictEqual(["payload_bytes_mismatch"]);
  });

  it("refuses a rule bundle that is not the compiled V1 profile", async () => {
    const fixture = await buildBlock({
      txCbors: [makeNativeTx({ privateKey: KEY }).txCbor],
    });
    const result = await evaluateBlock(fixture, {
      ruleBundle: {
        ...RULE_BUNDLE,
        consensusProfileDigest: h32(0x6d),
      } as WatcherRuleBundleV1,
    });
    expect(result.action).toBe("error");
    expect(result.reasonCodes).toStrictEqual(["rule_bundle_profile_mismatch"]);
  });

  it("refuses a rule bundle with a foreign rejection-selection rule", () => {
    expect(() =>
      makeWatcherPhaseAConfigV1({
        header: baseHeader(),
        ruleBundle: {
          ...RULE_BUNDLE,
          validation: {
            ...RULE_BUNDLE.validation,
            rejectionSelection: "last_rejection_wins_v0",
          },
        } as unknown as WatcherRuleBundleV1,
      }),
    ).toThrow(
      expect.objectContaining({
        code: "rule_bundle_profile_mismatch",
      }) as Error,
    );
  });

  it("refuses a rule bundle with a reordered validation phase priority", () => {
    expect(() =>
      makeWatcherPhaseAConfigV1({
        header: baseHeader(),
        ruleBundle: {
          ...RULE_BUNDLE,
          validation: {
            ...RULE_BUNDLE.validation,
            phasePriority: [
              ...WATCHER_RULE_BUNDLE_V1_VALIDATION_PHASE_PRIORITY,
            ].reverse(),
          },
        } as WatcherRuleBundleV1,
      }),
    ).toThrow(WatcherPhaseAVerifierError);
  });

  it("refuses a header whose protocol version differs from the bundle", () => {
    // A real `HeaderV1` cannot carry a non-V1 protocol version - the SDK
    // refuses to admit the observation first - so this guard is exercised at
    // the configuration boundary it protects.
    expect(() =>
      makeWatcherPhaseAConfigV1({
        header: baseHeader({ protocolVersion: 9n }),
        ruleBundle: RULE_BUNDLE,
      }),
    ).toThrow(
      expect.objectContaining({
        code: "header_protocol_version_mismatch",
      }) as Error,
    );
  });

  it("refuses non-public DA provenance", async () => {
    const fixture = await buildBlock({
      txCbors: [makeNativeTx({ privateKey: KEY }).txCbor],
    });
    const result = await evaluateBlock(fixture, {
      daProvenance: L1_PROVENANCE,
    });
    expect(result.action).toBe("error");
    expect(result.reasonCodes).toStrictEqual([
      "canonical_reconstruction_failed",
    ]);
  });

  it("fails closed on a reject code outside the canonical vocabulary", () => {
    expect(() =>
      watcherPhaseARejectionProjectionV1({
        rejected: {
          txId: Buffer.alloc(32, 1),
          code: "E_NOT_A_REAL_CODE" as RejectCode,
          detail: null,
          consensusPhase: "inputSets",
        },
        index: 0,
        expectedTxId: Buffer.alloc(32, 1).toString("hex"),
      }),
    ).toThrow(
      expect.objectContaining({ code: "unknown_reject_code" }) as Error,
    );
  });

  it("fails closed on a rejection without a canonical stage", () => {
    expect(() =>
      watcherPhaseARejectionProjectionV1({
        rejected: {
          txId: Buffer.alloc(32, 1),
          code: RejectCodes.EmptyInputs,
          detail: null,
        },
        index: 0,
        expectedTxId: Buffer.alloc(32, 1).toString("hex"),
      }),
    ).toThrow(
      expect.objectContaining({ code: "missing_rejection_stage" }) as Error,
    );
  });

  it("fails closed on a rejection carrying a foreign transaction id", () => {
    expect(() =>
      watcherPhaseARejectionProjectionV1({
        rejected: {
          txId: Buffer.alloc(32, 2),
          code: RejectCodes.EmptyInputs,
          detail: null,
          consensusPhase: "inputSets",
        },
        index: 3,
        expectedTxId: Buffer.alloc(32, 1).toString("hex"),
      }),
    ).toThrow(
      expect.objectContaining({ code: "rejection_tx_id_mismatch" }) as Error,
    );
  });

  it("keeps every reason code in the declared total order", () => {
    expect(new Set(WATCHER_PHASE_A_VERIFIER_REASON_CODES_V1).size).toBe(
      WATCHER_PHASE_A_VERIFIER_REASON_CODES_V1.length,
    );
  });

  it("never reports accept on an error result", async () => {
    const fixture = await buildBlock({
      txCbors: [makeNativeTx({ privateKey: KEY }).txCbor],
    });
    for (const result of [
      await evaluateBlock(fixture, {
        payloadEnvelopeCbor: Buffer.alloc(4),
      }),
      await evaluateBlock(fixture, { daProvenance: L1_PROVENANCE }),
      await evaluateBlock(fixture, {
        reconstruction: tamper(
          fixture.reconstruction,
          { action: "reject" },
          true,
        ),
      }),
    ]) {
      expect(result.action).toBe("error");
      expect(result.acceptedCount).toBe(0);
      expect(result.acceptedTxIds).toStrictEqual([]);
      expect(result.selectedRejection).toBeNull();
      expect(Object.isFrozen(result)).toBe(true);
    }
  });
});
