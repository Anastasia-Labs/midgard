import { isDeepStrictEqual } from "node:util";

import { CML } from "@lucid-evolution/lucid";

import {
  canonicalJsonOrder,
  check,
  exactKeys,
  hexBytes,
  object,
  sha256,
} from "./verify-phase4-pipelined-process-summary.validate-cleanup.mjs";

export const validateLedgerDelta = (reasons, value, label) => {
  if (!exactKeys(reasons, value, ["spent", "produced"], label)) return;
  if (Array.isArray(value.spent)) {
    check(
      reasons,
      value.spent.every(hexBytes) &&
        new Set(value.spent).size === value.spent.length &&
        isDeepStrictEqual(
          value.spent,
          [...value.spent].sort((left, right) => left.localeCompare(right)),
        ),
      `${label}.spent is not canonical`,
    );
  } else {
    reasons.push(`${label}.spent must be an array`);
  }
  if (!Array.isArray(value.produced)) {
    reasons.push(`${label}.produced must be an array`);
    return;
  }
  check(
    reasons,
    canonicalJsonOrder(value.produced),
    `${label}.produced is not canonically sorted`,
  );
  const outrefs = new Set();
  value.produced.forEach((member, index) => {
    const itemLabel = `${label}.produced[${index.toString()}]`;
    if (!exactKeys(reasons, member, ["outref", "output"], itemLabel)) return;
    check(
      reasons,
      hexBytes(member.outref) &&
        hexBytes(member.output) &&
        !outrefs.has(member.outref),
      `${itemLabel} contains a noncanonical or duplicate value`,
    );
    outrefs.add(member.outref);
  });
};

export const logicalDatabaseState = (state) => ({
  activeJournalCount: state.activeJournalCount,
  activeJournal:
    state.activeJournal === null
      ? null
      : {
          headerHash: state.activeJournal.headerHash,
          headerCbor: state.activeJournal.headerCbor,
          journalPayloadIdentity: state.activeJournal.journalPayloadIdentity,
          baseTailHeaderHash: state.activeJournal.baseTailHeaderHash,
          baseTailOutRef: state.activeJournal.baseTailOutRef,
          baseTailDatumCbor: state.activeJournal.baseTailDatumCbor,
          baseRoots: state.activeJournal.baseRoots,
          expectedRoots: state.activeJournal.expectedRoots,
          mpfReplay: state.activeJournal.mpfReplay,
          leaseTokenPresent:
            typeof state.activeJournal.leaseToken === "string" &&
            state.activeJournal.leaseToken.length > 0,
          submittedTxHash: state.activeJournal.submittedTxHash,
          submitted: state.activeJournal.submittedTxHash !== null,
          status: state.activeJournal.status,
          depositCount: state.activeJournal.depositCount,
          mempoolTxCount: state.activeJournal.mempoolTxCount,
        },
  activeLease:
    state.activeLease === null
      ? null
      : {
          holder: state.activeLease.holder,
          status: state.activeLease.status,
          tokenPresent:
            typeof state.activeLease.token === "string" &&
            state.activeLease.token.length > 0,
        },
  deposits: Array.isArray(state.deposits)
    ? state.deposits.map((deposit) => ({
        status: deposit?.status,
        projected: deposit?.projectedHeaderHash !== null,
      }))
    : state.deposits,
  mempool: state.mempool,
  processed: state.processed,
});

export const assertNoJournalBeyondBase = (
  reasons,
  state,
  baseHeaderHash,
  label,
) => {
  check(
    reasons,
    state?.activeJournalCount <= 1,
    `${label} violates the single-active-journal invariant`,
  );
  check(
    reasons,
    state?.activeJournal === null ||
      state?.activeJournal?.headerHash === baseHeaderHash,
    `${label} persisted a journal beyond the submitted base`,
  );
};

export const validatePhasRegistrationTransactionBody = (
  reasons,
  envelope,
  proof,
) => {
  if (
    !exactKeys(
      reasons,
      envelope,
      ["type", "description", "cborHex"],
      "isolation.snapshotPhasRegistrationTransactionBody",
    )
  ) {
    return;
  }
  if (
    !check(
      reasons,
      envelope.type === "Unwitnessed Tx ConwayEra" &&
        typeof envelope.description === "string" &&
        envelope.description.length > 0 &&
        hexBytes(envelope.cborHex),
      "isolation PHAS transaction-body envelope is not exact canonical unsigned CBOR",
    )
  ) {
    return;
  }
  try {
    const cborBytes = Buffer.from(envelope.cborHex, "hex");
    const transaction = CML.Transaction.from_cbor_hex(envelope.cborHex);
    const body = transaction.body();
    const certificates = body.certs();
    const certificate = certificates?.len() === 1 ? certificates.get(0) : null;
    const credential = certificate?.as_stake_registration()?.stake_credential();
    check(
      reasons,
      sha256(cborBytes) === proof?.transactionBody?.cborSha256 &&
        cborBytes.length === proof?.transactionBody?.cborSizeBytes &&
        transaction.to_canonical_cbor_hex() === envelope.cborHex &&
        transaction.witness_set().to_cbor_hex() === "a0" &&
        CML.hash_transaction(body).to_hex() === proof?.registrationTxHash &&
        certificate?.kind() === CML.CertificateKind.StakeRegistration &&
        credential?.kind() === CML.CredentialKind.Script &&
        credential.as_script()?.to_hex() === proof?.scriptHash,
      "isolation PHAS unsigned transaction body does not contain the exact submitted script registration certificate",
    );
  } catch {
    reasons.push(
      "isolation PHAS transaction-body envelope is not valid canonical Cardano CBOR",
    );
  }
};

export const sortedStrings = (value, pattern) =>
  Array.isArray(value) &&
  value.every((entry) => typeof entry === "string" && pattern.test(entry)) &&
  isDeepStrictEqual(
    value,
    [...value].sort((left, right) => left.localeCompare(right)),
  );

export const journalSourceIds = (state) =>
  (Array.isArray(state?.activeJournal?.journalPayloadIdentity?.transactions)
    ? state.activeJournal.journalPayloadIdentity.transactions
    : []
  )
    .flatMap((entry) =>
      object(entry) && typeof entry.sourceId === "string"
        ? [entry.sourceId]
        : [],
    )
    .sort((left, right) => left.localeCompare(right));

export const retainedTransactionIds = (state) =>
  [
    ...new Set(
      [
        ...(Array.isArray(state?.mempool) ? state.mempool : []),
        ...(Array.isArray(state?.processed) ? state.processed : []),
      ].flatMap((entry) =>
        object(entry) && typeof entry.txId === "string" ? [entry.txId] : [],
      ),
    ),
  ].sort((left, right) => left.localeCompare(right));

export const candidateLineMatches = (line, baseHeaderHash) =>
  typeof line === "string" &&
  new RegExp(
    `pipeline_trace phase=candidate_ready[^\\n]*base_header_hash=${baseHeaderHash}(?:\\s|$)`,
    "u",
  ).test(line);
