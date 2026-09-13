import { CML } from "@lucid-evolution/lucid";

import type { WatcherNativeBlockAdmission } from "../l1/native-block-admission.js";
import {
  type VerifiedWatcherDeploymentIdentity,
  watcherDeploymentAppliedScriptHashes,
} from "./deployment-identity.js";

export const WATCHER_BLOCK_RELEVANCE_SCHEMA_VERSION =
  "midgard-watcher-block-relevance-v1" as const;

const HEX_28 = /^[0-9a-f]{56}$/u;
const OUT_REF = /^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u;

/**
 * Whether a native block touched anything the watcher tracks. A `quiet`
 * block is processed without any provider request, durable authority
 * transition, or downstream reconciliation. A `touched` block runs the
 * complete authenticated pipeline.
 */
export type WatcherBlockRelevance = "quiet" | "touched";

export type WatcherBlockRelevancePolicy = Readonly<{
  schemaVersion: typeof WATCHER_BLOCK_RELEVANCE_SCHEMA_VERSION;
  /**
   * Payment-credential script hashes whose outputs, and minting policies
   * whose mints or burns, mark a block as touched. Every deployed protocol
   * script belongs here: the state queue, correction lock, fraud-proof,
   * deposit, withdrawal and forced-order scripts, the hub oracle and the
   * user-event certificate policy included.
   */
  trackedScriptHashes: ReadonlySet<string>;
}>;

export const makeWatcherBlockRelevancePolicy = (
  scriptHashes: Iterable<string>,
): WatcherBlockRelevancePolicy => {
  const tracked = new Set<string>();
  for (const hash of scriptHashes) {
    if (!HEX_28.test(hash))
      throw new Error("block relevance policy requires 28-byte script hashes");
    tracked.add(hash);
  }
  if (tracked.size === 0)
    throw new Error("block relevance policy tracks no protocol scripts");
  return Object.freeze({
    schemaVersion: WATCHER_BLOCK_RELEVANCE_SCHEMA_VERSION,
    trackedScriptHashes: tracked,
  });
};

/**
 * The one relevance predicate every watcher component shares: every deployed
 * protocol script credential and minting policy, the hub oracle policy, the
 * user-event certificate policy, and the deposit, withdrawal and forced-order
 * spend credentials and policies. Kupo is configured with the same
 * credentials, so a block this predicate calls quiet is one Kupo would have
 * matched nothing in.
 */
export const makeWatcherDeploymentBlockRelevancePolicy = (input: {
  readonly deploymentIdentity: VerifiedWatcherDeploymentIdentity;
  readonly scripts: Readonly<{
    hub: Readonly<{ policyId: string }>;
    certificatePolicyId: string;
    deposit: Readonly<{ policyId: string; spendScriptHash: string }>;
    withdrawal: Readonly<{ policyId: string; spendScriptHash: string }>;
    forcedOrder: Readonly<{ policyId: string; spendScriptHash: string }>;
  }>;
}): WatcherBlockRelevancePolicy =>
  makeWatcherBlockRelevancePolicy([
    ...Object.values(
      watcherDeploymentAppliedScriptHashes(input.deploymentIdentity),
    ),
    input.scripts.hub.policyId,
    input.scripts.certificatePolicyId,
    ...[
      input.scripts.deposit,
      input.scripts.withdrawal,
      input.scripts.forcedOrder,
    ].flatMap(({ policyId, spendScriptHash }) => [policyId, spendScriptHash]),
  ]);

const scriptHashOfCredential = (
  credential: CML.Credential | undefined,
): string | null => credential?.as_script()?.to_hex() ?? null;

const transactionTouches = (
  transactionCbor: string,
  policy: WatcherBlockRelevancePolicy,
  trackedOutRefs: ReadonlySet<string>,
): boolean => {
  const transaction = CML.Transaction.from_cbor_hex(transactionCbor);
  try {
    const body = transaction.body();
    const inputs = body.inputs();
    for (let index = 0; index < inputs.len(); index += 1) {
      const input = inputs.get(index);
      if (
        trackedOutRefs.has(
          `${input.transaction_id().to_hex()}#${input.index().toString()}`,
        )
      )
        return true;
    }
    const outputs = body.outputs();
    for (let index = 0; index < outputs.len(); index += 1) {
      const hash = scriptHashOfCredential(
        outputs.get(index).address().payment_cred(),
      );
      if (hash !== null && policy.trackedScriptHashes.has(hash)) return true;
    }
    const mint = body.mint();
    if (mint !== undefined) {
      const policies = mint.keys();
      for (let index = 0; index < policies.len(); index += 1) {
        if (policy.trackedScriptHashes.has(policies.get(index).to_hex()))
          return true;
      }
    }
    const withdrawals = body.withdrawals();
    if (withdrawals !== undefined) {
      const accounts = withdrawals.keys();
      for (let index = 0; index < accounts.len(); index += 1) {
        const hash = scriptHashOfCredential(accounts.get(index).payment());
        if (hash !== null && policy.trackedScriptHashes.has(hash)) return true;
      }
    }
    return false;
  } finally {
    transaction.free();
  }
};

/**
 * Classifies a native block from its own bytes. This asks no provider: the
 * native chain-sync already delivered every transaction, and a local scan of
 * outputs, mints, withdrawals and tracked spent outrefs decides in
 * microseconds whether the block can matter. An undecodable transaction is
 * conservatively `touched`, so the authenticated pipeline sees it.
 */
export const classifyWatcherNativeBlock = (input: {
  readonly block: WatcherNativeBlockAdmission;
  readonly policy: WatcherBlockRelevancePolicy;
  /** Currently tracked protocol outrefs (queue nodes, correction lock, ...). */
  readonly trackedOutRefs?: Iterable<string>;
}): WatcherBlockRelevance => {
  const outRefs = new Set<string>();
  for (const outRef of input.trackedOutRefs ?? []) {
    if (!OUT_REF.test(outRef))
      throw new Error("block relevance tracked outref is malformed");
    outRefs.add(outRef);
  }
  for (const transactionCbor of input.block.transactionCbors) {
    let touched: boolean;
    try {
      touched = transactionTouches(transactionCbor, input.policy, outRefs);
    } catch {
      return "touched";
    }
    if (touched) return "touched";
  }
  return "quiet";
};
