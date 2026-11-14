/**
 * A committed block of one forced transaction the operator rejected, by
 * default for the value it does not preserve, over the predecessor ledger of
 * `event-ledger-committed-steps.ts`. `pre` is the step's committed pre-state
 * root, when it is not the predecessor's.
 */
import { decodeMidgardNativeTxFullFromCanonicalCbor } from "@al-ft/midgard-core";
import type * as SDK from "@al-ft/midgard-sdk";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../../src/evidence/canonical-block-evidence.js";
import { ledgerEntriesRoot, type LedgerEntry } from "../../src/ledger-at.js";
import {
  admitCompleteCanonicalReplayPredecessor,
  type CompleteCanonicalReplay,
  createCompleteCanonicalReplayUnion,
} from "../../src/workflow/complete-replay.js";
import { buildDecodingBlockFixture } from "../support/native-script-decoding-emulator.js";
import {
  authenticatedHeaderObservation,
  buildFixtureTransaction,
  type FixtureTransactionInput,
} from "./canonical-block-evidence-fixture.js";
import { NATIVE, OTHER, SPENT } from "./event-ledger-committed-steps.js";

const ORDER: SDK.OutputReference = {
  transactionId: "bb".repeat(32),
  outputIndex: 0n,
};

export const forcedRejection = async ({
  input,
  reason = "ValueNotPreserved",
  pre,
  predecessor = [NATIVE, SPENT, OTHER],
}: {
  readonly input: FixtureTransactionInput;
  readonly reason?: SDK.RejectionReason;
  readonly pre?: string;
  readonly predecessor?: readonly LedgerEntry[];
}) => {
  const root = await ledgerEntriesRoot(predecessor);
  const fixture = await buildDecodingBlockFixture({
    operatorVkey: "aa".repeat(28),
    startTime: 1_000_000n,
    priorLedgerRoot: pre ?? root,
    prevUtxosRoot: root,
    subject: {
      kind: "forced",
      nativeTx: decodeMidgardNativeTxFullFromCanonicalCbor(
        buildFixtureTransaction(input).canonicalCbor,
      ),
      orderKey: ORDER,
      verdict: { ForcedTxInvalid: { reason } },
    },
  });
  const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
    observation: authenticatedHeaderObservation(fixture),
    payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
    daProvenance: {
      trustClass: "public_or_permissionless_da",
      sourceId: "test/forced-value-rejection",
      grade: "security",
    },
  });
  const context = {
    predecessor: await admitCompleteCanonicalReplayPredecessor({
      currentEvidence: evidence,
      ledgerAt: async (hash) =>
        hash === fixture.header.prevHeaderHash ? predecessor : undefined,
    }),
  };
  return {
    evidence,
    context,
    key: { ForcedTransactionEventKey: { tx_order_id: ORDER } } as SDK.EventKey,
    replay: async (members: readonly CompleteCanonicalReplay[]) =>
      (members.length === 1
        ? members[0]!
        : createCompleteCanonicalReplayUnion(members)
      ).replay(evidence, context),
  };
};
