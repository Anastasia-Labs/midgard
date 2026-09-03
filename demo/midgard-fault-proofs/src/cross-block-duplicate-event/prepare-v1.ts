import * as SDK from "@al-ft/midgard-sdk";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence-v1.js";
import { keyValuePhasProof } from "../transition-trace/phas.js";

export type CrossBlockDuplicateEventKindInput =
  | "deposit"
  | "withdrawal"
  | "forced-transaction";

export type PreparedCrossBlockDuplicateEvent = {
  readonly challengedHeaderHash: string;
  readonly settledHeaderHash: string;
  readonly challengedEvent: SDK.CommittedDuplicateEventProof;
  readonly settledEvent: SDK.CommittedDuplicateEventProof;
  readonly step02State: SDK.CrossBlockDuplicateEventStep02State;
};

export type AuthenticatedSettlementEvidence = {
  readonly observation: SDK.AuthenticatedL1Observation;
  /** Hub-registered settlement policy observed on the authentic hub datum. */
  readonly policyId: string;
  /** The one settlement NFT asset name. */
  readonly assetName: string;
  readonly datum: SDK.SettlementDatum;
  /** False after resolution/burn or rollback; such evidence is inadmissible. */
  readonly live: boolean;
};

const keyEquals = (a: SDK.OutputReference, b: SDK.OutputReference): boolean =>
  a.transactionId === b.transactionId && a.outputIndex === b.outputIndex;

const proofFor = async <K, V>(
  root:
    | CanonicalBlockEvidence["reconstruction"]["rootData"]["deposits"]
    | CanonicalBlockEvidence["reconstruction"]["rootData"]["withdrawals"]
    | CanonicalBlockEvidence["reconstruction"]["rootData"]["forcedTransactions"],
  entry: {
    readonly key: K;
    readonly value: V;
    readonly keyBytes: Buffer;
    readonly valueBytes: Buffer;
  },
): Promise<SDK.RootMembershipProof<K, V>> => ({
  domain: root.domain,
  root: root.root,
  phas_root: root.phasRoot,
  count: root.count,
  key: entry.key,
  value: entry.value,
  proof: await keyValuePhasProof(
    { root: root.phasRoot, count: root.count, entries: root.entries },
    entry.keyBytes,
    entry.valueBytes,
  ),
});

/**
 * Finds one identical L1 event in two independently authenticated canonical
 * block-evidence bundles and builds both counted-root openings. Settlement-NFT
 * authentication remains an L1 submission concern; this pure stage never
 * upgrades retained DA into settlement evidence.
 */
export const prepareCrossBlockDuplicateEvent = async ({
  challenged,
  settled,
  settlement,
  kind,
  eventKey,
}: {
  readonly challenged: CanonicalBlockEvidence;
  readonly settled: CanonicalBlockEvidence;
  readonly settlement: AuthenticatedSettlementEvidence;
  readonly kind: CrossBlockDuplicateEventKindInput;
  readonly eventKey: SDK.OutputReference;
}): Promise<PreparedCrossBlockDuplicateEvent> => {
  SDK.assertSecurityGradeEvidence(challenged.provenance.l1);
  SDK.assertSecurityGradeEvidence(settled.provenance.l1);
  SDK.admitAuthenticatedL1Observation({
    observation: settlement.observation,
  });
  if (challenged.headerHash === settled.headerHash) {
    throw new Error(
      "cross-block-duplicate-event evidence names the same header twice",
    );
  }
  if (!settlement.live) {
    throw new Error(
      "cross-block-duplicate-event settlement NFT is no longer live",
    );
  }
  if (!/^[0-9a-f]{56}$/u.test(settlement.policyId)) {
    throw new Error(
      "cross-block-duplicate-event settlement policy must be 28-byte lowercase hex",
    );
  }
  if (settlement.assetName !== settled.headerHash) {
    throw new Error(
      "cross-block-duplicate-event settlement NFT does not bind the retained-DA header",
    );
  }
  const settledCommittedRoot =
    kind === "deposit"
      ? settled.header.depositsRoot
      : kind === "withdrawal"
        ? settled.header.withdrawalsRoot
        : settled.header.forcedTransactionsRoot;
  const settlementRoot =
    kind === "deposit"
      ? settlement.datum.deposits_root
      : kind === "withdrawal"
        ? settlement.datum.withdrawals_root
        : settlement.datum.forced_transactions_root;
  if (settlementRoot !== settledCommittedRoot) {
    throw new Error(
      "cross-block-duplicate-event settlement datum does not preserve the historical counted root",
    );
  }
  let challengedEvent: SDK.CommittedDuplicateEventProof;
  let settledEvent: SDK.CommittedDuplicateEventProof;
  if (kind === "deposit") {
    const first = challenged.reconstruction.deposits.find((entry) =>
      keyEquals(entry.key, eventKey),
    );
    const second = settled.reconstruction.deposits.find((entry) =>
      keyEquals(entry.key, eventKey),
    );
    if (first === undefined || second === undefined) {
      throw new Error(
        "cross-block-duplicate-event deposit key is absent from one canonical block",
      );
    }
    challengedEvent = {
      CommittedDuplicateDepositV1: {
        membership: await proofFor(
          challenged.reconstruction.rootData.deposits,
          first,
        ),
      },
    };
    settledEvent = {
      CommittedDuplicateDepositV1: {
        membership: await proofFor(
          settled.reconstruction.rootData.deposits,
          second,
        ),
      },
    };
  } else if (kind === "withdrawal") {
    const first = challenged.reconstruction.withdrawals.find((entry) =>
      keyEquals(entry.key, eventKey),
    );
    const second = settled.reconstruction.withdrawals.find((entry) =>
      keyEquals(entry.key, eventKey),
    );
    if (first === undefined || second === undefined) {
      throw new Error(
        "cross-block-duplicate-event withdrawal key is absent from one canonical block",
      );
    }
    challengedEvent = {
      CommittedDuplicateWithdrawalV1: {
        membership: await proofFor(
          challenged.reconstruction.rootData.withdrawals,
          first,
        ),
      },
    };
    settledEvent = {
      CommittedDuplicateWithdrawalV1: {
        membership: await proofFor(
          settled.reconstruction.rootData.withdrawals,
          second,
        ),
      },
    };
  } else {
    const first = challenged.reconstruction.forcedTransactions.find((entry) =>
      keyEquals(entry.key, eventKey),
    );
    const second = settled.reconstruction.forcedTransactions.find((entry) =>
      keyEquals(entry.key, eventKey),
    );
    if (first === undefined || second === undefined) {
      throw new Error(
        "cross-block-duplicate-event forced transaction-order key is absent from one canonical block",
      );
    }
    challengedEvent = {
      CommittedDuplicateForcedTransactionV1: {
        membership: await proofFor(
          challenged.reconstruction.rootData.forcedTransactions,
          first,
        ),
      },
    };
    settledEvent = {
      CommittedDuplicateForcedTransactionV1: {
        membership: await proofFor(
          settled.reconstruction.rootData.forcedTransactions,
          second,
        ),
      },
    };
  }
  const step02State = SDK.crossBlockDuplicateEventStep02State({
    challengedHeaderHash: challenged.headerHash,
    settlementPolicyId: settlement.policyId,
    committedEvent: challengedEvent,
  });
  SDK.assertConfirmedDuplicateEvent({
    state: step02State,
    settledHeaderHash: settled.headerHash,
    settledEvent,
  });
  return {
    challengedHeaderHash: challenged.headerHash,
    settledHeaderHash: settled.headerHash,
    challengedEvent,
    settledEvent,
    step02State,
  };
};
