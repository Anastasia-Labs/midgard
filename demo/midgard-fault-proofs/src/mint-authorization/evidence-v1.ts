/**
 * `mint-authorization` evidence assembly.
 *
 * Everything a mint-authorization redeemer carries that is not positional
 * (the submitters resolve input/output/reference indices against the
 * transaction they are actually building, following the family submitter
 * pattern):
 *
 * - the committed-claim openings ride the existing transition-trace witness
 *   builders (`buildEventToStepMembershipProof`, `buildIndexedTraceProof`);
 * - the subject's field openings ride the §8.8 door builders in
 *   `field-opening-v1.ts` — nothing here re-implements the door;
 * - what THIS module owns: the reference-input outpoint's trie key (twin of
 *   `encode_midgard_tx_input`) and the pre-state ledger-trie membership
 *   proof behind an injected trie handle (the evidence module never owns
 *   ledger reconstruction).
 *
 * Everything here refuses early what the validator would abort on: a trie
 * whose root is not the thread's `prior_ledger_root`, an outpoint tx id that
 * is not 32 bytes.
 */
import { encodeMidgardSpendInputItem } from "@al-ft/midgard-core";
import type * as SDK from "@al-ft/midgard-sdk";
import { Proof } from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import type { TransitionTraceReconstruction } from "../transition-trace/reconstruct.js";
import {
  buildEventToStepMembershipProof,
  buildIndexedTraceProof,
} from "../transition-trace/witnesses.js";
import { MINT_AUTHORIZATION_CATEGORY_LABEL } from "./contracts-v1.js";

const evidenceError = (message: string): Error =>
  new Error(`${MINT_AUTHORIZATION_CATEGORY_LABEL} evidence: ${message}`);

// ## The scanned reference input's ledger-trie key

/**
 * Twin of `encode_midgard_tx_input` — the §5.3 fixed 38-byte spend-input
 * item IS the ledger trie's key for the outpoint, and field 1's items are
 * spelled in exactly that shape.
 */
export const mintAuthorizationOutpointKey = ({
  txIdHex,
  outputIndex,
}: {
  readonly txIdHex: string;
  readonly outputIndex: number;
}): Buffer => {
  if (!/^[0-9a-f]{64}$/u.test(txIdHex)) {
    throw evidenceError(
      "reference-input outpoint tx id must be 32 bytes of lowercase hex",
    );
  }
  return encodeMidgardSpendInputItem({
    txId: Buffer.from(txIdHex, "hex"),
    outputIndex,
  });
};

// ## Pre-state ledger membership (injected trie handle)

/**
 * The one thing the evidence module needs from the pre-state ledger: its
 * root, and a membership proof per key. Reconstruction stays with the
 * caller (the watcher's block replay, or a test's hand-built trie) — the
 * handle is structural precisely so this package depends on neither.
 */
export type MintAuthorizationLedgerTrieHandle = {
  /** The trie's current root, 32 bytes of hex. */
  readonly rootHex: string;
  /** MPF membership-proof CBOR for the key; must throw when absent. */
  readonly prove: (key: Buffer) => Promise<Buffer>;
};

/**
 * The `ledger_membership_proof` a `ResolveNext` redeemer carries: the MPF
 * proof of the scanned outpoint's descriptor under the thread's committed
 * `prior_ledger_root`. Refuses a trie whose root is not that commitment — a
 * proof from any other tree would abort on-chain.
 */
export const buildMintAuthorizationLedgerMembership = async ({
  trie,
  outpointKey,
  priorLedgerRootHex,
}: {
  readonly trie: MintAuthorizationLedgerTrieHandle;
  readonly outpointKey: Buffer;
  readonly priorLedgerRootHex: string;
}): Promise<SDK.Proof> => {
  const trieRoot = trie.rootHex.toLowerCase();
  const committedRoot = priorLedgerRootHex.toLowerCase();
  if (trieRoot !== committedRoot) {
    throw evidenceError(
      `ledger trie root ${trieRoot} is not the thread's prior_ledger_root ${committedRoot}`,
    );
  }
  const proofCbor = await trie.prove(outpointKey);
  return Data.from(Buffer.from(proofCbor).toString("hex"), Proof);
};

// ## Committed-claim openings (step-02)

/**
 * The two membership proofs a step-02 redeemer opens the committed claim
 * with. `transition_step_membership` is located through the event→step map
 * rather than by a caller-supplied index, so the two proofs cannot name
 * different steps. Only accepted L2 transaction events reach this family —
 * the on-chain step re-checks the leaf's `validity_code`.
 */
export const buildMintAuthorizationStep02Evidence = async ({
  reconstruction,
  eventKey,
}: {
  readonly reconstruction: TransitionTraceReconstruction;
  readonly eventKey: SDK.EventKey;
}): Promise<{
  readonly header: SDK.Header;
  readonly eventToStepMembership: SDK.EventToStepMembershipProof;
  readonly transitionStepMembership: SDK.IndexedTraceProof;
}> => {
  const eventToStepMembership = await buildEventToStepMembershipProof({
    reconstruction,
    eventKey,
  });
  const transitionStepMembership = await buildIndexedTraceProof({
    reconstruction,
    stepIndex: eventToStepMembership.value.step_index,
  });
  return {
    header: reconstruction.header,
    eventToStepMembership,
    transitionStepMembership,
  };
};
