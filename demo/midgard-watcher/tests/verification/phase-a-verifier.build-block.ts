import {
  computeMidgardNativeTxId,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  deriveMidgardNativeTxProofSourceFromCanonicalCbor,
} from "@al-ft/midgard-core/codec";
import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { buildCountedRoot, encodeData } from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import { h32 } from "@al-ft/midgard-test-support/hex";
import type {
  PhaseAConfig,
  QueuedTx,
  RejectedTx,
} from "@al-ft/midgard-validation/types";
import { RejectCodes } from "@al-ft/midgard-validation/types";
import { describe, expect, it } from "vitest";

import {
  evaluateWatcherHeaderRootReconstruction,
  makeWatcherAuthenticatedHeaderObservation,
} from "../../src/verification/header-root-reconstruction.js";
import {
  evaluateWatcherPhaseABlock,
  WATCHER_PHASE_A_CANONICAL_REJECT_CODES,
  WATCHER_PHASE_A_CONSENSUS_REJECT_CODES,
  WATCHER_PHASE_A_DIRECT_REJECT_CODES,
  WATCHER_PHASE_A_DOMINATED_REJECT_CODE_JUSTIFICATIONS,
  WATCHER_PHASE_A_DOMINATED_REJECT_CODES,
  WATCHER_PHASE_A_EVIDENCED_REJECT_CODES,
  WATCHER_PHASE_A_EXCLUDED_REJECT_CODE_JUSTIFICATIONS,
  WATCHER_PHASE_A_EXCLUDED_REJECT_CODES,
  WATCHER_PHASE_A_REACHABLE_REJECT_CODES,
  type WatcherPhaseAVerificationResult,
} from "../../src/verification/phase-a-verifier.js";
import {
  baseHeader,
  CHAIN_POINT,
  DA_PROVENANCE,
  L1_PROVENANCE,
  RULE_BUNDLE,
  RULE_BUNDLE_COMMITMENT,
} from "./phase-a-verifier.base-header.js";
import {
  type BlockFixture,
  bufferEntries,
  headerHashOf,
  hex,
  sortEntries,
  watcherHeaderRecord,
} from "./phase-a-verifier.evidence-cases.js";

/**
 * Builds a real block the way the node does: `transactions` carries the
 * canonical `L2TransactionSource` commitment, `transaction_preimages`
 * carries the exact canonical transaction bytes, and every header root and
 * count is derived from those entries rather than declared.
 */
export const buildBlock = async (input: {
  readonly txCbors: readonly Buffer[];
  readonly programMaterial?: readonly SDK.DaPayloadEntry[];
  readonly headerOverrides?: Partial<SDK.Header>;
}): Promise<BlockFixture> => {
  const transactions = input.txCbors.map((canonicalCbor) => {
    const full = decodeMidgardNativeTxFullFromCanonicalCbor(canonicalCbor);
    const proofSource =
      deriveMidgardNativeTxProofSourceFromCanonicalCbor(canonicalCbor);
    const source: SDK.L2TransactionSource = {
      tx_id: computeMidgardNativeTxId(full).toString("hex"),
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
    encodeData(tx.source, SDK.L2TransactionSourceSchema).toString("hex"),
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
        } satisfies SDK.ValidationTraceDescriptor,
        SDK.ValidationTraceDescriptorSchema,
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

  const header: SDK.Header = baseHeader({
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
  const payload: SDK.DaPayload = {
    version: SDK.DA_PAYLOAD_VERSION,
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
      validation_trace_witnesses: [],
      counts,
    },
  };
  const envelope = await wrapDaPayload(SDK.encodeDaPayload(payload), {
    mode: "identity",
  });
  const observation = await makeWatcherAuthenticatedHeaderObservation({
    header: watcherHeaderRecord(header, headerHash),
    chainPoint: CHAIN_POINT,
    confirmationDepth: 12,
    sourceMode: "local_node",
    provenance: L1_PROVENANCE,
  });
  const reconstruction = await evaluateWatcherHeaderRootReconstruction({
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

export const evaluateBlock = async (
  fixture: BlockFixture,
  overrides: Partial<Parameters<typeof evaluateWatcherPhaseABlock>[0]> = {},
): Promise<WatcherPhaseAVerificationResult> =>
  await evaluateWatcherPhaseABlock({
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
  it("mirrors the canonical 50-member RejectCodes vocabulary", () => {
    expect(WATCHER_PHASE_A_CANONICAL_REJECT_CODES).toStrictEqual(
      Object.values(RejectCodes),
    );
  });

  it("partitions the vocabulary into 32 reachable and 18 excluded codes", () => {
    expect(
      [
        ...WATCHER_PHASE_A_REACHABLE_REJECT_CODES,
        ...WATCHER_PHASE_A_EXCLUDED_REJECT_CODES,
      ].sort(),
    ).toStrictEqual([...WATCHER_PHASE_A_CANONICAL_REJECT_CODES].sort());
    for (const code of WATCHER_PHASE_A_REACHABLE_REJECT_CODES) {
      expect(WATCHER_PHASE_A_EXCLUDED_REJECT_CODES).not.toContain(code);
    }
  });

  it("derives the reachable set from the canonical Phase A call sites", () => {
    const derived = new Set<string>([
      ...WATCHER_PHASE_A_DIRECT_REJECT_CODES,
      ...WATCHER_PHASE_A_CONSENSUS_REJECT_CODES,
    ]);
    expect(new Set(WATCHER_PHASE_A_REACHABLE_REJECT_CODES)).toStrictEqual(
      derived,
    );
  });

  it("keeps every published list in canonical declaration order", () => {
    const order = new Map(
      WATCHER_PHASE_A_CANONICAL_REJECT_CODES.map((code, index) => [
        code,
        index,
      ]),
    );
    for (const list of [
      WATCHER_PHASE_A_REACHABLE_REJECT_CODES,
      WATCHER_PHASE_A_EXCLUDED_REJECT_CODES,
      WATCHER_PHASE_A_EVIDENCED_REJECT_CODES,
      WATCHER_PHASE_A_DOMINATED_REJECT_CODES,
    ]) {
      const positions = list.map((code) => order.get(code)!);
      expect(positions).toStrictEqual([...positions].sort((a, b) => a - b));
    }
  });

  it("justifies every excluded and every dominated code exactly once", () => {
    expect(
      Object.keys(WATCHER_PHASE_A_EXCLUDED_REJECT_CODE_JUSTIFICATIONS).sort(),
    ).toStrictEqual([...WATCHER_PHASE_A_EXCLUDED_REJECT_CODES].sort());
    expect(
      Object.keys(WATCHER_PHASE_A_DOMINATED_REJECT_CODE_JUSTIFICATIONS).sort(),
    ).toStrictEqual([...WATCHER_PHASE_A_DOMINATED_REJECT_CODES].sort());
  });

  it("excludes the five Phase-B-only codes and keeps E_MIN_FEE reachable", () => {
    for (const code of [
      RejectCodes.DoubleSpend,
      RejectCodes.DependencyCycle,
      RejectCodes.DependsOnRejectedTx,
      RejectCodes.InputNotFound,
      RejectCodes.ValueNotPreserved,
    ]) {
      expect(WATCHER_PHASE_A_EXCLUDED_REJECT_CODES).toContain(code);
    }
    // The recorded waiver text lists E_MIN_FEE with the Phase-B set, but
    // phase-a.ts:509-516 emits it from the header-committed minFeeA/minFeeB,
    // and the evidence corpus below reaches it. It is published as reachable.
    expect(WATCHER_PHASE_A_REACHABLE_REJECT_CODES).toContain(
      RejectCodes.MinFee,
    );
    expect(WATCHER_PHASE_A_EVIDENCED_REJECT_CODES).toContain(
      RejectCodes.MinFee,
    );
  });

  it("splits the reachable set into 20 evidenced and 12 dominated codes", () => {
    expect(
      [
        ...WATCHER_PHASE_A_EVIDENCED_REJECT_CODES,
        ...WATCHER_PHASE_A_DOMINATED_REJECT_CODES,
      ].sort(),
    ).toStrictEqual([...WATCHER_PHASE_A_REACHABLE_REJECT_CODES].sort());
  });
});

// ---------------------------------------------------------------------------
// Differential record: the load-bearing evidence
// ---------------------------------------------------------------------------

export type CanonicalVerdict = {
  readonly accepted: boolean;
  readonly rejected: RejectedTx | null;
};

/**
 * The canonical verdict for one queued transaction, memoised per
 * (transaction, config) pair. Several corpus entries are hundreds of kilobytes
 * and are compared against the watcher in more than one block, and the cache
 * only avoids repeating an identical pure call - it never substitutes for one.
 */
export const verdictCache = new Map<
  QueuedTx,
  Map<PhaseAConfig, CanonicalVerdict>
>();
