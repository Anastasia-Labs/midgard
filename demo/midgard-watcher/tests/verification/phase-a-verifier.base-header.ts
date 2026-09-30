import {
  encodeMidgardCekProgramMaterialDaValue,
  encodeMidgardCekProgramMaterialSidecar,
  encodeMidgardCekTermNode,
  hashMidgardCekTermNode,
} from "@al-ft/midgard-core/cek-proof";
import type { MidgardNativeScript } from "@al-ft/midgard-core/codec/native-script";
import * as SDK from "@al-ft/midgard-sdk";
import { h28, h32 } from "@al-ft/midgard-test-support/hex";
import {
  makeNativeTx,
  makeOutput,
  TEST_ADDRESS_BYTES,
} from "@al-ft/midgard-validation/tests/validation-fixtures";
import type {
  PhaseAConfig,
  QueuedTx,
  RejectCode,
} from "@al-ft/midgard-validation/types";
import { CML } from "@lucid-evolution/lucid";

import { makeWatcherPhaseAConfig } from "../../src/verification/phase-a-verifier.js";
import {
  computeWatcherRuleBundleCommitment,
  makeWatcherCanonicalRuleBundle,
  type WatcherRuleBundle,
} from "../../src/verification/rule-bundle.js";

// ---------------------------------------------------------------------------
// Shared fixture material
// ---------------------------------------------------------------------------

/**
 * The size/preimage-bound fixtures are hundreds of kilobytes of canonical CBOR
 * and are validated more than once per case; the 5s default is not a safe
 * budget for them inside a parallel run of the whole watcher suite.
 */
export const SLOW_TEST_TIMEOUT_MS = 120_000;

/** Deterministic signing key: fixture bytes must not vary between runs. */
export const KEY = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 7));

export const L1_PROVENANCE: SDK.EvidenceProvenance = {
  trustClass: "authenticated_cardano_l1",
  sourceId: "watcher-local-node",
  grade: "security",
};

export const DA_PROVENANCE: SDK.EvidenceProvenance = {
  trustClass: "public_or_permissionless_da",
  sourceId: "watcher-da-peer-1",
  grade: "security",
};

export const CHAIN_POINT = { slot: 4242n, blockHash: h32(7) } as const;

export const RULE_BUNDLE: WatcherRuleBundle = makeWatcherCanonicalRuleBundle({
  constructionIdentity: {
    manifestId: h32(0x21),
    network: "Preprod",
    blueprintHash: h32(0x22),
    programCommitments: {
      "transition-order-v1": h32(0x23),
      "validation-machine-v1": h32(0x24),
    },
  },
  targetParameterSnapshot: { finalityDepth: 12 },
});

export const RULE_BUNDLE_COMMITMENT =
  computeWatcherRuleBundleCommitment(RULE_BUNDLE);

export const baseHeader = (
  overrides: Partial<SDK.Header> = {},
): SDK.Header => ({
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

export const configFor = (overrides: Partial<SDK.Header> = {}): PhaseAConfig =>
  makeWatcherPhaseAConfig({
    header: baseHeader(overrides),
    ruleBundle: RULE_BUNDLE,
  });

export const CONFIG = configFor();

export const EMPTY_SIDECAR = encodeMidgardCekProgramMaterialSidecar([]);

export const queuedTx = (
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

export const nestedNativeScript = (depth: number): MidgardNativeScript => {
  let script: MidgardNativeScript = { type: "after", slot: 0n };
  for (let index = 0; index < depth; index += 1) {
    script = { type: "all", scripts: [script] };
  }
  return script;
};

/** An inline datum of roughly `chunks * 66` bytes, canonically chunked. */
export const bigInlineDatum = (chunks: number): Buffer =>
  Buffer.concat([
    Buffer.from([0x5f]),
    ...Array.from({ length: chunks }, () =>
      Buffer.concat([Buffer.from([0x58, 0x40]), Buffer.alloc(64, 1)]),
    ),
    Buffer.from([0xff]),
  ]);

export const assetMap = (count: number): Map<string, Map<string, bigint>> => {
  const inner = new Map<string, bigint>();
  for (let index = 0; index < count; index += 1) {
    inner.set(index.toString(16).padStart(4, "0"), 1n);
  }
  return new Map([["ab".repeat(28), inner]]);
};

export const manyOutputs = (count: number): Buffer[] =>
  Array.from({ length: count }, (_unused, index) =>
    makeOutput(BigInt(index + 1), TEST_ADDRESS_BYTES),
  );

/** An unreachable CEK term node, used as block-wide program material. */
const ORPHAN_TERM_NODE = { kind: "error" as const };

export const ORPHAN_MATERIAL_ENTRY = {
  kind: "term" as const,
  root: hashMidgardCekTermNode(ORPHAN_TERM_NODE),
  preimage: encodeMidgardCekTermNode(ORPHAN_TERM_NODE),
};

export const ORPHAN_MATERIAL_DA_ENTRY: SDK.DaPayloadEntry = [
  Buffer.from(ORPHAN_MATERIAL_ENTRY.root).toString("hex"),
  encodeMidgardCekProgramMaterialDaValue(ORPHAN_MATERIAL_ENTRY).toString("hex"),
];

// ---------------------------------------------------------------------------
// Rejection-evidence corpus
// ---------------------------------------------------------------------------

export type EvidenceCase = {
  readonly label: string;
  readonly code: RejectCode;
  readonly stage: string;
  readonly queued: QueuedTx;
  readonly config: PhaseAConfig;
};

export const fromNativeTx = (
  options: Parameters<typeof makeNativeTx>[0],
  overrides: Partial<QueuedTx> = {},
): QueuedTx => {
  const fixture = makeNativeTx({ privateKey: KEY, ...options });
  return queuedTx(fixture.txId, fixture.txCbor, overrides);
};

export const evidenceCase = (
  label: string,
  code: RejectCode,
  stage: string,
  queued: QueuedTx,
  config: PhaseAConfig = CONFIG,
): EvidenceCase => ({ label, code, stage, queued, config });
