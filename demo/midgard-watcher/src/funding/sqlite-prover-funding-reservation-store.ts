import { createHash } from "node:crypto";
import { mkdir, realpath, stat } from "node:fs/promises";
import { dirname, isAbsolute, normalize } from "node:path";
import { DatabaseSync } from "node:sqlite";

import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  parseWorkflowFundingAbandonmentHandoff,
  parseWorkflowFundingCompletionHandoff,
  parseWorkflowFundingPreparedTransition,
  parseWorkflowFundingSubmissionHandoff,
  type WorkflowFundingAbandonmentHandoff,
  type WorkflowFundingCompletionHandoff,
  type WorkflowFundingPreparedTransition,
  type WorkflowFundingSubmissionHandoff,
} from "@al-ft/midgard-fault-proofs";
import { CML, coreToTxOutput } from "@lucid-evolution/lucid";

import { watcherCanonicalJson } from "../storage/durable-store.js";
import {
  assertWatcherProverFundingReservationPlan,
  parseWatcherProverFundingReservationRecord,
  type WatcherProverFundingReservationInput,
  type WatcherProverFundingReservationPlan,
  type WatcherProverFundingReservationRecord,
  type WatcherProverFundingReservationStore,
  type WatcherProverFundingReservationTransition,
} from "./prover-funding-reservation.js";

export const WATCHER_SQLITE_PROVER_FUNDING_RESERVATION_STORE =
  "midgard-watcher-sqlite-prover-funding-reservation-store-v1" as const;

const HEX_32 = /^[0-9a-f]{64}$/u;

export type WatcherProverFundingReservationConflict = Readonly<{
  code: "reservation_collision";
  outRef: string;
}>;

const admittedConflicts = new WeakSet<object>();

class ReservationConflictError extends Error {
  readonly conflict: WatcherProverFundingReservationConflict;

  constructor(outRef: string) {
    super("prover funding output is already reserved");
    this.name = "WatcherProductionProverFundingReservationConflictV1";
    this.conflict = Object.freeze({ code: "reservation_collision", outRef });
    admittedConflicts.add(this);
  }
}

export const isWatcherProverFundingReservationConflict = (
  value: unknown,
): value is Error &
  Readonly<{
    conflict: WatcherProverFundingReservationConflict;
  }> => value instanceof Error && admittedConflicts.has(value);

export type WatcherSqliteProverFundingReservationStoreRuntime = Readonly<{
  schemaVersion: typeof WATCHER_SQLITE_PROVER_FUNDING_RESERVATION_STORE;
  store: WatcherProverFundingReservationStore;
  close(): void;
}>;

const canonicalDatabasePath = (value: unknown): string => {
  if (
    typeof value !== "string" ||
    value !== value.trim() ||
    !isAbsolute(value) ||
    normalize(value) !== value ||
    value === "/" ||
    value === "/tmp" ||
    value.startsWith("/tmp/")
  ) {
    throw new Error(
      "prover funding reservation store requires a canonical durable path",
    );
  }
  return value;
};

const identicalInputs = (
  left: readonly WatcherProverFundingReservationInput[],
  right: readonly WatcherProverFundingReservationInput[],
): boolean => watcherCanonicalJson(left) === watcherCanonicalJson(right);

const assertPlanMatchesRecord = (
  plan: WatcherProverFundingReservationPlan,
  record: WatcherProverFundingReservationRecord,
): void => {
  if (
    record.reservationId !== plan.reservationId ||
    record.deploymentFingerprint !== plan.deploymentFingerprint ||
    record.decisionDigest !== plan.decisionDigest ||
    record.policyDigest !== plan.policyDigest ||
    record.reservationBasisDigest !== plan.reservationBasisDigest
  ) {
    throw new Error("prover funding reservation identity mismatch");
  }
};

const nextRecord = (input: {
  readonly current: WatcherProverFundingReservationRecord;
  readonly state?: WatcherProverFundingReservationRecord["state"];
  readonly activeInputs?: readonly WatcherProverFundingReservationInput[];
  readonly pendingTransition?: WatcherProverFundingReservationTransition | null;
  readonly lastConfirmedTransitionDigest?: string | null;
  readonly conflictCode?: WatcherProverFundingReservationRecord["conflictCode"];
}): WatcherProverFundingReservationRecord => {
  const recordInput = Object.freeze({
    reservationId: input.current.reservationId,
    deploymentFingerprint: input.current.deploymentFingerprint,
    decisionDigest: input.current.decisionDigest,
    policyDigest: input.current.policyDigest,
    reservationBasisDigest: input.current.reservationBasisDigest,
    revision: (BigInt(input.current.revision) + 1n).toString(),
    state: input.state ?? input.current.state,
    activeInputs: Object.freeze(
      [...(input.activeInputs ?? input.current.activeInputs)].sort(
        (left, right) => left.outRef.localeCompare(right.outRef),
      ),
    ),
    pendingTransition:
      input.pendingTransition === undefined
        ? input.current.pendingTransition
        : input.pendingTransition,
    lastConfirmedTransitionDigest:
      input.lastConfirmedTransitionDigest === undefined
        ? input.current.lastConfirmedTransitionDigest
        : input.lastConfirmedTransitionDigest,
    conflictCode:
      input.conflictCode === undefined
        ? input.current.conflictCode
        : input.conflictCode,
  });
  return parseWatcherProverFundingReservationRecord({
    ...recordInput,
    recordDigest: computeDeploymentManifestJsonDigest(recordInput),
  });
};

const initialRecord = (
  plan: WatcherProverFundingReservationPlan,
): WatcherProverFundingReservationRecord => {
  const recordInput = Object.freeze({
    reservationId: plan.reservationId,
    deploymentFingerprint: plan.deploymentFingerprint,
    decisionDigest: plan.decisionDigest,
    policyDigest: plan.policyDigest,
    reservationBasisDigest: plan.reservationBasisDigest,
    revision: "0",
    state: "active" as const,
    activeInputs: plan.inputs,
    pendingTransition: null,
    lastConfirmedTransitionDigest: null,
    conflictCode: null,
  });
  return parseWatcherProverFundingReservationRecord({
    ...recordInput,
    recordDigest: computeDeploymentManifestJsonDigest(recordInput),
  });
};

const makeTransition = (input: {
  readonly actionKind: string;
  readonly transactionHash: string;
  readonly transactionBodySha256: string;
  readonly signedTransactionCborHex: string;
  readonly consumedOutRefs: readonly string[];
  readonly producedInputs: readonly WatcherProverFundingReservationInput[];
}): WatcherProverFundingReservationTransition => {
  const transitionInput = Object.freeze({
    actionKind: input.actionKind,
    transactionHash: input.transactionHash,
    transactionBodySha256: input.transactionBodySha256,
    signedTransactionCborHex: input.signedTransactionCborHex,
    consumedOutRefs: Object.freeze([...input.consumedOutRefs].sort()),
    producedInputs: Object.freeze(
      [...input.producedInputs].sort((left, right) =>
        left.outRef.localeCompare(right.outRef),
      ),
    ),
  });
  const transition = Object.freeze({
    ...transitionInput,
    transitionDigest: computeDeploymentManifestJsonDigest(transitionInput),
  });
  const provisional = Object.freeze({
    reservationId: "00".repeat(32),
    deploymentFingerprint: "00".repeat(32),
    decisionDigest: "00".repeat(32),
    policyDigest: "00".repeat(32),
    reservationBasisDigest: "00".repeat(32),
    revision: "0",
    state: "active" as const,
    activeInputs: transition.producedInputs,
    pendingTransition: transition,
    lastConfirmedTransitionDigest: null,
    conflictCode: null,
  });
  parseWatcherProverFundingReservationRecord({
    ...provisional,
    recordDigest: computeDeploymentManifestJsonDigest(provisional),
  });
  return transition;
};

const transactionInputOutRefs = (inputs: {
  readonly len: () => number;
  readonly get: (index: number) => {
    readonly transaction_id: () => { readonly to_hex: () => string };
    readonly index: () => bigint | number;
  };
}): readonly string[] => {
  const outRefs: string[] = [];
  for (let index = 0; index < inputs.len(); index += 1) {
    const input = inputs.get(index);
    outRefs.push(
      `${input.transaction_id().to_hex()}#${input.index().toString()}`,
    );
  }
  return Object.freeze(outRefs.sort());
};

const deriveSignedTransition = ({
  plan,
  activeInputs,
  input,
}: {
  readonly plan: WatcherProverFundingReservationPlan;
  readonly activeInputs: readonly WatcherProverFundingReservationInput[];
  readonly input: Parameters<
    WatcherProverFundingReservationStore["prepareTransition"]
  >[0];
}): Parameters<typeof makeTransition>[0] => {
  if (!/^(?:[0-9a-f]{2})+$/u.test(input.signedTransactionCborHex)) {
    throw new Error("prover transition signed transaction is malformed");
  }
  let transaction: CML.Transaction;
  try {
    transaction = CML.Transaction.from_cbor_hex(input.signedTransactionCborHex);
  } catch {
    throw new Error("prover transition signed transaction is malformed");
  }
  if (transaction.to_cbor_hex() !== input.signedTransactionCborHex) {
    throw new Error(
      "prover transition signed transaction is not a lossless Cardano encoding",
    );
  }
  const body = transaction.body();
  const bodyHash = CML.hash_transaction(body).to_raw_bytes();
  const vkeys = transaction.witness_set().vkeywitnesses();
  let fundingWitness = false;
  for (let index = 0; index < (vkeys?.len() ?? 0); index += 1) {
    const witness = vkeys!.get(index);
    if (
      witness.vkey().hash().to_hex() === plan.fundingPaymentKeyHash &&
      witness.vkey().verify(bodyHash, witness.ed25519_signature())
    ) {
      fundingWitness = true;
    }
  }
  if (!fundingWitness) {
    throw new Error("prover transition lacks its reserved funding witness");
  }
  const transactionHash = CML.hash_transaction(body).to_hex();
  const transactionBodySha256 = createHash("sha256")
    .update(Buffer.from(body.to_cbor_hex(), "hex"))
    .digest("hex");
  const planFunding = new Set(
    activeInputs
      .filter(({ role }) => role === "funding")
      .map(({ outRef }) => outRef),
  );
  const consumedOutRefs = transactionInputOutRefs(body.inputs()).filter(
    (outRef) => planFunding.has(outRef),
  );
  const outputs = body.outputs();
  const producedInputs: WatcherProverFundingReservationInput[] = [];
  for (let index = 0; index < outputs.len(); index += 1) {
    const output = coreToTxOutput(outputs.get(index));
    if (output.address !== plan.walletAddress) continue;
    const lovelace = output.assets.lovelace;
    if (lovelace === undefined || lovelace <= 0n) {
      throw new Error("prover transition wallet output omitted lovelace");
    }
    producedInputs.push(
      Object.freeze({
        outRef: `${transactionHash}#${index.toString()}`,
        role: "funding" as const,
        lovelace: lovelace.toString(),
        assets: Object.freeze(
          Object.entries(output.assets)
            .filter(([unit]) => unit !== "lovelace")
            .sort(([left], [right]) => left.localeCompare(right))
            .map(([unit, quantity]) =>
              Object.freeze({ unit, quantity: quantity.toString() }),
            ),
        ),
      }),
    );
  }
  const derived = Object.freeze({
    actionKind: input.actionKind,
    transactionHash,
    transactionBodySha256,
    signedTransactionCborHex: input.signedTransactionCborHex,
    consumedOutRefs,
    producedInputs: Object.freeze(producedInputs),
  });
  if (
    input.transactionHash !== derived.transactionHash ||
    input.transactionBodySha256 !== derived.transactionBodySha256 ||
    watcherCanonicalJson(input.consumedOutRefs) !==
      watcherCanonicalJson(derived.consumedOutRefs) ||
    watcherCanonicalJson(input.producedInputs) !==
      watcherCanonicalJson(derived.producedInputs)
  ) {
    throw new Error(
      "prover transition output differs from the signed transaction",
    );
  }
  return derived;
};

const openInternal = async (
  input: Readonly<{ path: string; busyTimeoutMs?: number }>,
  assertPlan: (plan: WatcherProverFundingReservationPlan) => void,
): Promise<WatcherSqliteProverFundingReservationStoreRuntime> => {
  const path = canonicalDatabasePath(input.path);
  const directory = dirname(path);
  await mkdir(directory, { recursive: true, mode: 0o700 });
  if ((await realpath(directory)) !== directory) {
    throw new Error("prover funding reservation directory traverses a symlink");
  }
  try {
    if (
      (await stat(path)).isSymbolicLink() ||
      (await realpath(path)) !== path
    ) {
      throw new Error("prover funding reservation path traverses a symlink");
    }
  } catch (error) {
    if ((error as NodeJS.ErrnoException).code !== "ENOENT") throw error;
  }
  const busyTimeoutMs = input.busyTimeoutMs ?? 5_000;
  if (
    !Number.isSafeInteger(busyTimeoutMs) ||
    busyTimeoutMs < 1 ||
    busyTimeoutMs > 120_000
  ) {
    throw new Error("prover funding reservation busy timeout is invalid");
  }

  const database = new DatabaseSync(path, {
    open: true,
    readOnly: false,
    enableForeignKeyConstraints: true,
  });
  database.exec(`
    PRAGMA journal_mode = WAL;
    PRAGMA synchronous = FULL;
    PRAGMA trusted_schema = OFF;
    PRAGMA busy_timeout = ${busyTimeoutMs.toString()};
    CREATE TABLE IF NOT EXISTS watcher_prover_funding_reservation_v1 (
      reservation_id TEXT PRIMARY KEY CHECK (length(reservation_id) = 64),
      record_digest TEXT NOT NULL CHECK (length(record_digest) = 64),
      canonical_json TEXT NOT NULL CHECK (length(canonical_json) > 0)
    ) STRICT;
    CREATE TABLE IF NOT EXISTS watcher_prover_funding_lease_v1 (
      out_ref TEXT PRIMARY KEY,
      reservation_id TEXT NOT NULL,
      lease_phase TEXT NOT NULL CHECK (lease_phase IN ('active', 'pending')),
      role TEXT NOT NULL CHECK (role IN ('funding', 'collateral')),
      FOREIGN KEY (reservation_id)
        REFERENCES watcher_prover_funding_reservation_v1(reservation_id)
        ON DELETE CASCADE
    ) STRICT;
    CREATE TABLE IF NOT EXISTS watcher_prover_funding_handoff_v1 (
      reservation_id TEXT NOT NULL,
      kind TEXT NOT NULL CHECK (kind IN ('submission', 'completion')),
      identity_digest TEXT NOT NULL CHECK (length(identity_digest) = 64),
      record_digest TEXT NOT NULL CHECK (length(record_digest) = 64),
      canonical_json TEXT NOT NULL CHECK (length(canonical_json) > 0),
      PRIMARY KEY (reservation_id, kind, identity_digest),
      FOREIGN KEY (reservation_id)
        REFERENCES watcher_prover_funding_reservation_v1(reservation_id)
        ON DELETE CASCADE
    ) STRICT;
    CREATE UNIQUE INDEX IF NOT EXISTS watcher_prover_funding_completion_handoff_v1
      ON watcher_prover_funding_handoff_v1(reservation_id) WHERE kind = 'completion';
    CREATE TABLE IF NOT EXISTS watcher_prover_funding_abandonment_v1 (
      reservation_id TEXT NOT NULL,
      transition_digest TEXT NOT NULL CHECK(length(transition_digest) = 64),
      record_digest TEXT NOT NULL CHECK(length(record_digest) = 64),
      canonical_json TEXT NOT NULL CHECK(length(canonical_json) > 0),
      acknowledged_revision TEXT,
      PRIMARY KEY (reservation_id, transition_digest),
      FOREIGN KEY (reservation_id) REFERENCES watcher_prover_funding_reservation_v1(reservation_id) ON DELETE CASCADE
    ) STRICT;
    CREATE UNIQUE INDEX IF NOT EXISTS watcher_prover_funding_unacknowledged_abandonment_v1
      ON watcher_prover_funding_abandonment_v1(reservation_id) WHERE acknowledged_revision IS NULL;
    CREATE TABLE IF NOT EXISTS watcher_prover_funding_lineage_v1 (
      reservation_id TEXT NOT NULL,
      action_kind TEXT NOT NULL,
      output_index INTEGER NOT NULL CHECK (output_index >= 0),
      out_ref TEXT NOT NULL UNIQUE,
      resolved_output_cbor_hex TEXT NOT NULL,
      transition_digest TEXT NOT NULL CHECK (length(transition_digest) = 64),
      phase TEXT NOT NULL CHECK (phase IN ('pending', 'confirmed')),
      lineage_digest TEXT NOT NULL CHECK (length(lineage_digest) = 64),
      PRIMARY KEY (reservation_id, transition_digest, output_index),
      FOREIGN KEY (reservation_id)
        REFERENCES watcher_prover_funding_reservation_v1(reservation_id)
        ON DELETE CASCADE
    ) STRICT;
  `);

  const selectAll = database.prepare(`
    SELECT reservation_id, record_digest, canonical_json
    FROM watcher_prover_funding_reservation_v1
    ORDER BY reservation_id ASC
  `);
  const selectOne = database.prepare(`
    SELECT reservation_id, record_digest, canonical_json
    FROM watcher_prover_funding_reservation_v1
    WHERE reservation_id = ?
  `);
  const insertRecord = database.prepare(`
    INSERT INTO watcher_prover_funding_reservation_v1(
      reservation_id, record_digest, canonical_json
    ) VALUES (?, ?, ?)
  `);
  const updateRecord = database.prepare(`
    UPDATE watcher_prover_funding_reservation_v1
    SET record_digest = ?, canonical_json = ?
    WHERE reservation_id = ? AND record_digest = ?
  `);
  const selectAllLeases = database.prepare(`
    SELECT out_ref, reservation_id, lease_phase, role
    FROM watcher_prover_funding_lease_v1
    ORDER BY out_ref ASC
  `);
  const selectLease = database.prepare(`
    SELECT reservation_id
    FROM watcher_prover_funding_lease_v1
    WHERE out_ref = ?
  `);
  const insertLease = database.prepare(`
    INSERT INTO watcher_prover_funding_lease_v1(
      out_ref, reservation_id, lease_phase, role
    ) VALUES (?, ?, ?, ?)
  `);
  const deleteLeases = database.prepare(`
    DELETE FROM watcher_prover_funding_lease_v1
    WHERE reservation_id = ?
  `);
  const insertLineage = database.prepare(`
    INSERT INTO watcher_prover_funding_lineage_v1(
      reservation_id, action_kind, output_index, out_ref,
      resolved_output_cbor_hex, transition_digest, phase, lineage_digest
    ) VALUES (?, ?, ?, ?, ?, ?, 'pending', ?)
  `);
  const confirmLineage = database.prepare(`
    UPDATE watcher_prover_funding_lineage_v1
    SET phase = 'confirmed'
    WHERE reservation_id = ? AND transition_digest = ? AND phase = 'pending'
  `);
  const deletePendingLineage = database.prepare(`
    DELETE FROM watcher_prover_funding_lineage_v1
    WHERE reservation_id = ? AND transition_digest = ? AND phase = 'pending'
  `);
  const selectConfirmedLineage = database.prepare(`
    SELECT reservation_id, action_kind, output_index, out_ref,
           resolved_output_cbor_hex, transition_digest, phase, lineage_digest
    FROM watcher_prover_funding_lineage_v1
    WHERE reservation_id = ? AND out_ref = ?
          AND phase = 'confirmed'
  `);
  const selectAllLineage = database.prepare(`
    SELECT reservation_id, action_kind, output_index, out_ref,
           resolved_output_cbor_hex, transition_digest, phase, lineage_digest
    FROM watcher_prover_funding_lineage_v1
    ORDER BY reservation_id ASC, transition_digest ASC, output_index ASC
  `);

  const insertHandoff = database.prepare(`
    INSERT INTO watcher_prover_funding_handoff_v1(
      reservation_id, kind, identity_digest, record_digest, canonical_json
    ) VALUES (?, ?, ?, ?, ?)
  `);
  const selectHandoff = database.prepare(`
    SELECT reservation_id, kind, identity_digest, record_digest, canonical_json
    FROM watcher_prover_funding_handoff_v1
    WHERE reservation_id = ? AND kind = ? AND identity_digest = ?
  `);
  const selectCompletionHandoff = database.prepare(`
    SELECT reservation_id, kind, identity_digest, record_digest, canonical_json
    FROM watcher_prover_funding_handoff_v1
    WHERE reservation_id = ? AND kind = 'completion'
  `);
  const selectAllHandoffs = database.prepare(`
    SELECT reservation_id, kind, identity_digest, record_digest, canonical_json
    FROM watcher_prover_funding_handoff_v1
    ORDER BY reservation_id ASC, kind ASC, identity_digest ASC
  `);
  const insertAbandonment = database.prepare(`
    INSERT INTO watcher_prover_funding_abandonment_v1(reservation_id, transition_digest, record_digest, canonical_json)
    VALUES (?, ?, ?, ?)
  `);
  const selectUnacknowledgedAbandonment = database.prepare(`
    SELECT * FROM watcher_prover_funding_abandonment_v1 WHERE reservation_id = ? AND acknowledged_revision IS NULL
  `);
  const selectAllAbandonments = database.prepare(
    `SELECT * FROM watcher_prover_funding_abandonment_v1 ORDER BY reservation_id, transition_digest`,
  );
  const acknowledgeAbandonment = database.prepare(`
    UPDATE watcher_prover_funding_abandonment_v1 SET acknowledged_revision = ?
    WHERE reservation_id = ? AND transition_digest = ? AND acknowledged_revision IS NULL
  `);
  type AbandonmentRow = Readonly<{
    reservation_id: unknown;
    transition_digest: unknown;
    record_digest: unknown;
    canonical_json: unknown;
    acknowledged_revision: unknown;
  }>;
  const readAbandonmentRow = (row: AbandonmentRow) => {
    if (
      typeof row.reservation_id !== "string" ||
      !HEX_32.test(row.reservation_id) ||
      typeof row.transition_digest !== "string" ||
      !HEX_32.test(row.transition_digest) ||
      typeof row.record_digest !== "string" ||
      !HEX_32.test(row.record_digest) ||
      typeof row.canonical_json !== "string" ||
      (row.acknowledged_revision !== null &&
        (typeof row.acknowledged_revision !== "string" ||
          !/^(?:0|[1-9][0-9]*)$/u.test(row.acknowledged_revision)))
    )
      throw new Error("prover funding abandonment row is malformed");
    const value: unknown = JSON.parse(row.canonical_json);
    if (
      value === null ||
      typeof value !== "object" ||
      Array.isArray(value) ||
      Object.keys(value).sort().join(",") !== "handoff,transition" ||
      !("handoff" in value) ||
      !("transition" in value) ||
      watcherCanonicalJson(value) !== row.canonical_json ||
      computeDeploymentManifestJsonDigest(value) !== row.record_digest
    )
      throw new Error("prover funding abandonment row digest mismatch");
    const transition = parseWorkflowFundingPreparedTransition(value.transition);
    const handoff = parseWorkflowFundingAbandonmentHandoff(value.handoff);
    if (
      computeDeploymentManifestJsonDigest(transition) !==
        row.transition_digest ||
      handoff.submissionIntent.txHash !== transition.transactionHash ||
      handoff.reconciliation.txHash !== transition.transactionHash
    )
      throw new Error(
        "prover funding abandonment changed signed transaction identity",
      );
    return {
      reservationId: row.reservation_id,
      transitionDigest: row.transition_digest,
      acknowledgedRevision: row.acknowledged_revision,
      transition,
      handoff,
    };
  };
  const unacknowledgedAbandonment = (reservationId: string) => {
    const row = selectUnacknowledgedAbandonment.get(reservationId) as
      | AbandonmentRow
      | undefined;
    return row === undefined ? null : readAbandonmentRow(row);
  };
  type HandoffRow = Readonly<{
    reservation_id: unknown;
    kind: unknown;
    identity_digest: unknown;
    record_digest: unknown;
    canonical_json: unknown;
  }>;
  type SubmissionHandoff = Readonly<{
    kind: "submission";
    reservationId: string;
    identityDigest: string;
    transition: WorkflowFundingPreparedTransition;
    handoff: WorkflowFundingSubmissionHandoff;
  }>;
  type CompletionHandoff = Readonly<{
    kind: "completion";
    reservationId: string;
    identityDigest: string;
    handoff: WorkflowFundingCompletionHandoff;
  }>;
  const readHandoffRow = (
    row: HandoffRow,
  ): SubmissionHandoff | CompletionHandoff => {
    if (
      typeof row.reservation_id !== "string" ||
      !HEX_32.test(row.reservation_id) ||
      typeof row.identity_digest !== "string" ||
      !HEX_32.test(row.identity_digest) ||
      typeof row.record_digest !== "string" ||
      !HEX_32.test(row.record_digest) ||
      typeof row.canonical_json !== "string" ||
      (row.kind !== "submission" && row.kind !== "completion")
    )
      throw new Error("prover funding handoff row is malformed");
    const data: unknown = JSON.parse(row.canonical_json);
    if (
      watcherCanonicalJson(data) !== row.canonical_json ||
      computeDeploymentManifestJsonDigest(data) !== row.record_digest
    )
      throw new Error("prover funding handoff row digest mismatch");
    if (row.kind === "completion") {
      const handoff = parseWorkflowFundingCompletionHandoff(data);
      if (computeDeploymentManifestJsonDigest(handoff) !== row.identity_digest)
        throw new Error("prover funding completion identity mismatch");
      return {
        kind: "completion",
        reservationId: row.reservation_id,
        identityDigest: row.identity_digest,
        handoff,
      };
    }
    if (
      data === null ||
      typeof data !== "object" ||
      Array.isArray(data) ||
      Object.keys(data).sort().join(",") !== "handoff,transition" ||
      !("handoff" in data) ||
      !("transition" in data)
    )
      throw new Error("prover funding submission handoff is malformed");
    const transition = parseWorkflowFundingPreparedTransition(data.transition);
    const handoff = parseWorkflowFundingSubmissionHandoff(data.handoff);
    if (
      computeDeploymentManifestJsonDigest(transition) !== row.identity_digest ||
      handoff.preflight.txHash !== transition.transactionHash ||
      handoff.submissionIntent.txHash !== transition.transactionHash
    )
      throw new Error(
        "prover funding submission handoff differs from signed transaction",
      );
    return {
      kind: "submission",
      reservationId: row.reservation_id,
      identityDigest: row.identity_digest,
      transition,
      handoff,
    };
  };
  const assertHandoffReservation = (
    handoff:
      | WorkflowFundingAbandonmentHandoff
      | WorkflowFundingSubmissionHandoff
      | WorkflowFundingCompletionHandoff,
    record: WatcherProverFundingReservationRecord,
  ) => {
    if (
      handoff.identity.deploymentFingerprint !== record.deploymentFingerprint ||
      handoff.identity.decisionDigest !== record.decisionDigest
    )
      throw new Error(
        "prover funding handoff differs from reservation identity",
      );
  };
  const persistHandoff = (
    record: WatcherProverFundingReservationRecord,
    kind: "submission" | "completion",
    identityDigest: string,
    value: unknown,
  ) => {
    const canonicalJson = watcherCanonicalJson(value);
    const row = {
      reservation_id: record.reservationId,
      kind,
      identity_digest: identityDigest,
      record_digest: computeDeploymentManifestJsonDigest(value),
      canonical_json: canonicalJson,
    };
    assertHandoffReservation(readHandoffRow(row).handoff, record);
    insertHandoff.run(
      record.reservationId,
      kind,
      identityDigest,
      row.record_digest,
      canonicalJson,
    );
  };

  type RecordRow = Readonly<{
    reservation_id: unknown;
    record_digest: unknown;
    canonical_json: unknown;
  }>;
  type LeaseRow = Readonly<{
    out_ref: unknown;
    reservation_id: unknown;
    lease_phase: unknown;
    role: unknown;
  }>;
  type LineageRow = Readonly<{
    reservation_id: unknown;
    action_kind: unknown;
    output_index: unknown;
    out_ref: unknown;
    resolved_output_cbor_hex: unknown;
    transition_digest: unknown;
    phase: unknown;
    lineage_digest: unknown;
  }>;

  const parseLineageRow = (row: LineageRow) => {
    if (
      typeof row.reservation_id !== "string" ||
      !HEX_32.test(row.reservation_id) ||
      typeof row.action_kind !== "string" ||
      !/^[a-z][a-zA-Z0-9_.:-]{0,127}$/u.test(row.action_kind) ||
      !Number.isSafeInteger(row.output_index) ||
      (row.output_index as number) < 0 ||
      typeof row.out_ref !== "string" ||
      !/^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u.test(row.out_ref) ||
      typeof row.resolved_output_cbor_hex !== "string" ||
      !/^(?:[0-9a-f]{2})+$/u.test(row.resolved_output_cbor_hex) ||
      typeof row.transition_digest !== "string" ||
      !HEX_32.test(row.transition_digest) ||
      (row.phase !== "pending" && row.phase !== "confirmed") ||
      typeof row.lineage_digest !== "string" ||
      !HEX_32.test(row.lineage_digest)
    ) {
      throw new Error("prover funding lineage row is malformed");
    }
    const identity = Object.freeze({
      reservationId: row.reservation_id,
      sourceActionKind: row.action_kind,
      sourceOutputIndex: row.output_index as number,
      outRef: row.out_ref,
      resolvedOutputCborHex: row.resolved_output_cbor_hex,
      transitionDigest: row.transition_digest,
    });
    const output = CML.TransactionOutput.from_cbor_hex(
      identity.resolvedOutputCborHex,
    );
    if (
      output.to_canonical_cbor_hex() !== identity.resolvedOutputCborHex ||
      computeDeploymentManifestJsonDigest(identity) !== row.lineage_digest
    ) {
      throw new Error("prover funding lineage row digest mismatch");
    }
    return Object.freeze({ ...identity, phase: row.phase });
  };

  const parseRow = (row: RecordRow): WatcherProverFundingReservationRecord => {
    if (
      typeof row.reservation_id !== "string" ||
      !HEX_32.test(row.reservation_id) ||
      typeof row.record_digest !== "string" ||
      !HEX_32.test(row.record_digest) ||
      typeof row.canonical_json !== "string"
    ) {
      throw new Error("prover funding reservation row is malformed");
    }
    let value: unknown;
    try {
      value = JSON.parse(row.canonical_json);
    } catch {
      throw new Error("prover funding reservation row is malformed");
    }
    const record = parseWatcherProverFundingReservationRecord(value);
    if (
      watcherCanonicalJson(record) !== row.canonical_json ||
      record.reservationId !== row.reservation_id ||
      record.recordDigest !== row.record_digest
    ) {
      throw new Error("prover funding reservation row metadata mismatch");
    }
    return record;
  };

  const readOne = (
    reservationId: string,
  ): WatcherProverFundingReservationRecord | null => {
    const row = selectOne.get(reservationId) as RecordRow | undefined;
    return row === undefined ? null : parseRow(row);
  };

  const writeRecord = (
    currentDigest: string | null,
    next: WatcherProverFundingReservationRecord,
  ): void => {
    const canonicalJson = watcherCanonicalJson(next);
    if (currentDigest === null) {
      insertRecord.run(next.reservationId, next.recordDigest, canonicalJson);
      return;
    }
    const result = updateRecord.run(
      next.recordDigest,
      canonicalJson,
      next.reservationId,
      currentDigest,
    );
    if (result.changes !== 1) {
      throw new Error("prover funding reservation compare-and-swap failed");
    }
  };

  const assertLeaseAvailable = (outRef: string, reservationId: string) => {
    const row = selectLease.get(outRef) as
      | Readonly<{ reservation_id: unknown }>
      | undefined;
    if (row === undefined) return;
    if (row.reservation_id !== reservationId) {
      throw new ReservationConflictError(outRef);
    }
  };

  // An included terminal may have released this exact change output to a
  // later job. Replaying its producer must preserve that job's sole lease.
  const outputTransferredToAnotherReservation = (
    reservationId: string,
    outRef: string,
  ): boolean => {
    const lease = selectLease.get(outRef) as
      | Readonly<{ reservation_id: string }>
      | undefined;
    if (lease === undefined || lease.reservation_id === reservationId)
      return false;
    const lineage = selectConfirmedLineage.get(reservationId, outRef) as
      | LineageRow
      | undefined;
    return (
      lineage !== undefined &&
      parseLineageRow(lineage).outRef === outRef &&
      readOne(lease.reservation_id)?.activeInputs.some(
        (input) => input.outRef === outRef,
      ) === true
    );
  };

  const rebuildLeases = (): void => {
    const records = (selectAll.all() as RecordRow[]).map(parseRow);
    for (const record of records) deleteLeases.run(record.reservationId);
    for (const record of records) {
      for (const value of record.activeInputs)
        insertLease.run(
          value.outRef,
          record.reservationId,
          "active",
          value.role,
        );
    }
    for (const record of records) {
      for (const value of record.pendingTransition?.producedInputs ?? []) {
        if (
          outputTransferredToAnotherReservation(
            record.reservationId,
            value.outRef,
          )
        )
          continue;
        insertLease.run(
          value.outRef,
          record.reservationId,
          "pending",
          value.role,
        );
      }
    }
  };

  const persistPendingLineage = (input: {
    readonly reservationId: string;
    readonly transition: WatcherProverFundingReservationTransition;
    readonly signedTransactionCborHex: string;
  }): number => {
    const transaction = CML.Transaction.from_cbor_hex(
      input.signedTransactionCborHex,
    );
    const outputs = transaction.body().outputs();
    for (let outputIndex = 0; outputIndex < outputs.len(); outputIndex += 1) {
      const identity = Object.freeze({
        reservationId: input.reservationId,
        sourceActionKind: input.transition.actionKind,
        sourceOutputIndex: outputIndex,
        outRef: `${input.transition.transactionHash}#${outputIndex.toString()}`,
        resolvedOutputCborHex: outputs.get(outputIndex).to_canonical_cbor_hex(),
        transitionDigest: input.transition.transitionDigest,
      });
      insertLineage.run(
        identity.reservationId,
        identity.sourceActionKind,
        identity.sourceOutputIndex,
        identity.outRef,
        identity.resolvedOutputCborHex,
        identity.transitionDigest,
        computeDeploymentManifestJsonDigest(identity),
      );
    }
    return outputs.len();
  };

  const recordedSubmissions = (reservationId: string): SubmissionHandoff[] =>
    (selectAllHandoffs.all() as HandoffRow[])
      .filter(
        (row) =>
          row.reservation_id === reservationId && row.kind === "submission",
      )
      .map(readHandoffRow)
      .filter((row): row is SubmissionHandoff => row.kind === "submission");

  const reobservationInputs = (
    reservationId: string,
    transactionHash: string,
  ) => {
    const record = readOne(reservationId);
    if (record === null) throw new Error("prover reservation is missing");
    const candidates = new Map(
      record.activeInputs.map(({ outRef, role }) => [outRef, role]),
    );
    for (const { transition } of recordedSubmissions(reservationId)) {
      if (transition.transactionHash !== transactionHash) continue;
      for (const outRef of transition.consumedOutRefs)
        candidates.set(outRef, "funding");
      const collateral = CML.Transaction.from_cbor_hex(
        transition.signedTransactionCborHex,
      )
        .body()
        .collateral_inputs();
      for (let i = 0; i < (collateral?.len() ?? 0); i++) {
        const ref = collateral!.get(i);
        candidates.set(
          `${ref.transaction_id().to_hex()}#${ref.index()}`,
          "collateral",
        );
      }
    }
    return [...candidates].map(([outRef, role]) => ({ outRef, role }));
  };

  const audit = (): readonly WatcherProverFundingReservationRecord[] => {
    const records = (selectAll.all() as RecordRow[]).map(parseRow);
    const expected = new Map<
      string,
      Readonly<{
        reservationId: string;
        phase: "active" | "pending";
        role: string;
      }>
    >();
    for (const record of records) {
      for (const value of record.activeInputs) {
        if (expected.has(value.outRef))
          throw new Error("prover funding reservation repeats an output lease");
        expected.set(
          value.outRef,
          Object.freeze({
            reservationId: record.reservationId,
            phase: "active",
            role: value.role,
          }),
        );
      }
    }
    for (const record of records) {
      for (const value of record.pendingTransition?.producedInputs ?? []) {
        if (
          outputTransferredToAnotherReservation(
            record.reservationId,
            value.outRef,
          )
        )
          continue;
        if (expected.has(value.outRef)) {
          throw new Error("prover funding reservation repeats an output lease");
        }
        expected.set(
          value.outRef,
          Object.freeze({
            reservationId: record.reservationId,
            phase: "pending",
            role: value.role,
          }),
        );
      }
    }
    const leases = selectAllLeases.all() as LeaseRow[];
    if (leases.length !== expected.size) {
      throw new Error("prover funding reservation lease set mismatch");
    }
    for (const row of leases) {
      if (
        typeof row.out_ref !== "string" ||
        typeof row.reservation_id !== "string" ||
        (row.lease_phase !== "active" && row.lease_phase !== "pending") ||
        (row.role !== "funding" && row.role !== "collateral")
      ) {
        throw new Error("prover funding reservation lease is malformed");
      }
      const value = expected.get(row.out_ref);
      if (
        value === undefined ||
        value.reservationId !== row.reservation_id ||
        value.phase !== row.lease_phase ||
        value.role !== row.role
      ) {
        throw new Error("prover funding reservation lease metadata mismatch");
      }
    }
    const pendingByReservation = new Map(
      records
        .filter(({ pendingTransition }) => pendingTransition !== null)
        .map((record) => [
          record.reservationId,
          record.pendingTransition!.transitionDigest,
        ]),
    );
    for (const row of selectAllLineage.all() as LineageRow[]) {
      const lineage = parseLineageRow(row);
      if (
        lineage.phase === "pending" &&
        pendingByReservation.get(lineage.reservationId) !==
          lineage.transitionDigest
      ) {
        throw new Error(
          "prover funding pending lineage differs from reservation",
        );
      }
    }
    for (const row of selectAllHandoffs.all() as HandoffRow[]) {
      const handoff = readHandoffRow(row);
      const record = records.find(
        ({ reservationId }) => reservationId === handoff.reservationId,
      );
      if (record === undefined)
        throw new Error("prover funding handoff has no reservation");
      assertHandoffReservation(handoff.handoff, record);
      if (handoff.kind === "completion" && record.state !== "released")
        throw new Error(
          "prover funding completion handoff has unreleased inputs",
        );
    }
    for (const row of selectAllAbandonments.all() as AbandonmentRow[]) {
      const abandoned = readAbandonmentRow(row);
      const record = records.find(
        ({ reservationId }) => reservationId === abandoned.reservationId,
      );
      if (record === undefined)
        throw new Error("prover funding abandonment has no reservation");
      assertHandoffReservation(abandoned.handoff, record);
      if (
        abandoned.acknowledgedRevision === null &&
        (record.pendingTransition !== null || record.state === "released")
      )
        throw new Error(
          "unacknowledged funding abandonment overlaps a new transition or release",
        );
      if (
        abandoned.acknowledgedRevision !== null &&
        BigInt(abandoned.acknowledgedRevision) > BigInt(record.revision)
      )
        throw new Error(
          "funding abandonment acknowledgement is ahead of its reservation",
        );
    }
    return Object.freeze(records);
  };

  const transaction = <T>(operation: () => T): T => {
    database.exec("BEGIN IMMEDIATE");
    try {
      const value = operation();
      audit();
      database.exec("COMMIT");
      return value;
    } catch (error) {
      try {
        database.exec("ROLLBACK");
      } catch {
        // Preserve the first storage/authority failure.
      }
      throw error;
    }
  };

  const auditRead = () => {
    database.exec("BEGIN DEFERRED");
    try {
      const records = audit();
      database.exec("COMMIT");
      return records;
    } catch (error) {
      try {
        database.exec("ROLLBACK");
      } catch {
        // Preserve the first storage/authority failure.
      }
      throw error;
    }
  };

  const hasSignedHistory = (reservationId: string): boolean =>
    (selectAllHandoffs.all() as HandoffRow[]).some(
      (row) => row.reservation_id === reservationId,
    ) ||
    (selectAllLineage.all() as LineageRow[]).some(
      (row) => row.reservation_id === reservationId,
    ) ||
    (selectAllAbandonments.all() as AbandonmentRow[]).some(
      (row) => row.reservation_id === reservationId,
    );

  auditRead();

  const store: WatcherProverFundingReservationStore = Object.freeze({
    readAll: async () => auditRead(),
    hasSignedHistory: async ({ reservationId }) => {
      if (!auditRead().some((record) => record.reservationId === reservationId))
        throw new Error("prover funding history reservation is missing");
      return hasSignedHistory(reservationId);
    },
    readAbandonmentHandoff: async ({ reservationId }) => {
      auditRead();
      const abandoned = unacknowledgedAbandonment(reservationId);
      return abandoned === null
        ? null
        : { transition: abandoned.transition, handoff: abandoned.handoff };
    },
    readPendingTransition: async ({ reservationId }) => {
      const current = auditRead().find(
        (record) => record.reservationId === reservationId,
      );
      if (current?.pendingTransition == null) return null;
      const { transitionDigest: _digest, ...transition } =
        current.pendingTransition;
      return transition;
    },
    readPendingHandoff: async ({ reservationId }) => {
      const current = auditRead().find(
        (record) => record.reservationId === reservationId,
      );
      if (current?.pendingTransition == null) return null;
      const row = selectHandoff.get(
        reservationId,
        "submission",
        current.pendingTransition.transitionDigest,
      ) as HandoffRow | undefined;
      if (row === undefined) return null;
      const recovered = readHandoffRow(row);
      if (recovered.kind !== "submission")
        throw new Error("prover funding pending handoff kind mismatch");
      return { transition: recovered.transition, handoff: recovered.handoff };
    },
    readReobservationInputs: async ({ reservationId, transactionHash }) => {
      auditRead();
      return reobservationInputs(reservationId, transactionHash);
    },
    reobserveTransition: async ({
      plan,
      expectedRevision,
      transactionHash,
      inputs,
    }) => {
      assertPlan(plan);
      return transaction(() => {
        const current = readOne(plan.reservationId);
        if (current === null) throw new Error("prover reservation is missing");
        assertPlanMatchesRecord(plan, current);
        if (
          current.revision !== expectedRevision ||
          current.state === "conflict" ||
          unacknowledgedAbandonment(current.reservationId) !== null
        )
          throw new Error("prover reservation reobservation mismatch");
        const completionRow = selectCompletionHandoff.get(
          current.reservationId,
        ) as HandoffRow | undefined;
        if (completionRow !== undefined) {
          const saved = readHandoffRow(completionRow);
          if (
            saved.kind !== "completion" ||
            saved.handoff.completion.kind !== "terminal_included"
          )
            throw new Error("anchored prover reservation cannot be reopened");
        }
        const recorded = recordedSubmissions(current.reservationId).find(
          ({ transition }) => transition.transactionHash === transactionHash,
        );
        if (recorded === undefined)
          throw new Error(
            "reobserved transaction has no signed funding intent",
          );
        const candidates = new Map(
          reobservationInputs(current.reservationId, transactionHash).map(
            ({ outRef, role }) => [outRef, role],
          ),
        );
        const transition = makeTransition(recorded.transition);
        const produced = new Set(
          transition.producedInputs.map(({ outRef }) => outRef),
        );
        for (const value of inputs) {
          if (candidates.get(value.outRef) !== value.role)
            throw new Error("reobserved funding input has no recorded lineage");
          if (!produced.has(value.outRef))
            assertLeaseAvailable(value.outRef, current.reservationId);
        }
        // Only canonical inputs returned by the authenticated wallet source are
        // selected again. Historical signed attempts stay in the handoff table.
        const next = nextRecord({
          current,
          state: "active",
          activeInputs: inputs.filter(({ outRef }) => !produced.has(outRef)),
          pendingTransition: transition,
          conflictCode: null,
        });
        if (current.pendingTransition !== null)
          deletePendingLineage.run(
            current.reservationId,
            current.pendingTransition.transitionDigest,
          );
        const confirmed = selectConfirmedLineage.get(
          current.reservationId,
          `${transition.transactionHash}#0`,
        );
        if (confirmed === undefined)
          persistPendingLineage({
            reservationId: current.reservationId,
            transition,
            signedTransactionCborHex: transition.signedTransactionCborHex,
          });
        database
          .prepare(
            "DELETE FROM watcher_prover_funding_handoff_v1 WHERE reservation_id = ? AND kind = 'completion'",
          )
          .run(current.reservationId);
        writeRecord(current.recordDigest, next);
        rebuildLeases();
        return next;
      });
    },
    readCompletionHandoff: async ({ reservationId }) => {
      auditRead();
      const row = selectCompletionHandoff.get(reservationId) as
        | HandoffRow
        | undefined;
      if (row === undefined) return null;
      const recovered = readHandoffRow(row);
      if (recovered.kind !== "completion")
        throw new Error("prover funding completion handoff kind mismatch");
      return recovered.handoff;
    },
    readConfirmedInput: async ({ reservationId, outRef }) => {
      const row = selectConfirmedLineage.get(reservationId, outRef) as
        | LineageRow
        | undefined;
      if (row === undefined) return null;
      const lineage = parseLineageRow(row);
      return Object.freeze({
        sourceActionKind: lineage.sourceActionKind,
        sourceOutputIndex: lineage.sourceOutputIndex,
        outRef: lineage.outRef,
        resolvedOutputCborHex: lineage.resolvedOutputCborHex,
      });
    },
    releaseUnused: async (expected) => {
      const snapshot = parseWatcherProverFundingReservationRecord(expected);
      return transaction(() => {
        const current = readOne(snapshot.reservationId);
        if (current === null)
          throw new Error("unused prover reservation is missing");
        if (
          current.recordDigest !== snapshot.recordDigest ||
          current.state !== "active" ||
          current.activeInputs.length === 0 ||
          current.pendingTransition !== null ||
          current.lastConfirmedTransitionDigest !== null
        )
          return false;
        // Preparation persists signed bytes before the journal intent. Absence
        // from the journal alone never authorizes reclaiming those inputs.
        if (hasSignedHistory(current.reservationId)) return false;
        const next = nextRecord({ current, activeInputs: [] });
        writeRecord(current.recordDigest, next);
        rebuildLeases();
        return true;
      });
    },
    reserve: async (plan, expectedIdleRevision) => {
      assertPlan(plan);
      return transaction(() => {
        const current = readOne(plan.reservationId);
        if (current !== null) {
          assertPlanMatchesRecord(plan, current);
          if (
            current.revision === "0" &&
            !identicalInputs(current.activeInputs, plan.inputs)
          ) {
            throw new Error("prover funding reservation plan was substituted");
          }
          if (current.state === "released") {
            throw new Error("prover funding reservation was released");
          }
          if (current.state === "conflict") {
            throw new Error("prover funding reservation is conflicted");
          }
          if (expectedIdleRevision !== undefined) {
            if (
              current.revision !== expectedIdleRevision ||
              current.activeInputs.length !== 0 ||
              current.pendingTransition !== null ||
              unacknowledgedAbandonment(current.reservationId) !== null
            )
              throw new Error("prover reservation cannot refresh idle inputs");
            for (const value of plan.inputs)
              assertLeaseAvailable(value.outRef, plan.reservationId);
            const next = nextRecord({ current, activeInputs: plan.inputs });
            writeRecord(current.recordDigest, next);
            rebuildLeases();
            return "reserved" as const;
          }
          return "unchanged" as const;
        }
        if (expectedIdleRevision !== undefined)
          throw new Error("idle prover reservation is missing");
        for (const value of plan.inputs) {
          assertLeaseAvailable(value.outRef, plan.reservationId);
        }
        const record = initialRecord(plan);
        writeRecord(null, record);
        rebuildLeases();
        return "reserved" as const;
      });
    },
    prepareTransition: async (transitionInput) => {
      assertPlan(transitionInput.plan);
      return transaction(() => {
        const current = readOne(transitionInput.plan.reservationId);
        if (current === null) throw new Error("prover reservation is missing");
        assertPlanMatchesRecord(transitionInput.plan, current);
        if (
          current.state !== "active" ||
          current.pendingTransition !== null ||
          unacknowledgedAbandonment(current.reservationId) !== null ||
          current.revision !== transitionInput.expectedRevision
        ) {
          throw new Error("prover reservation cannot prepare transition");
        }
        const activeOutRefs = new Set(
          current.activeInputs.map(({ outRef }) => outRef),
        );
        if (
          transitionInput.consumedOutRefs.some(
            (outRef) => !activeOutRefs.has(outRef),
          )
        ) {
          throw new Error("prover transition consumes an unreserved input");
        }
        const transition = makeTransition(
          deriveSignedTransition({
            plan: transitionInput.plan,
            activeInputs: current.activeInputs,
            input: transitionInput,
          }),
        );
        const { transitionDigest: _digest, ...signedTransition } = transition;
        persistHandoff(current, "submission", transition.transitionDigest, {
          transition: signedTransition,
          handoff: transitionInput.handoff,
        });
        persistPendingLineage({
          reservationId: current.reservationId,
          transition,
          signedTransactionCborHex: transitionInput.signedTransactionCborHex,
        });
        for (const value of transition.producedInputs) {
          assertLeaseAvailable(value.outRef, current.reservationId);
        }
        const next = nextRecord({
          current,
          pendingTransition: transition,
        });
        writeRecord(current.recordDigest, next);
        rebuildLeases();
        return next;
      });
    },
    confirmTransition: async (confirmation) => {
      assertPlan(confirmation.plan);
      return transaction(() => {
        const current = readOne(confirmation.plan.reservationId);
        if (current === null) throw new Error("prover reservation is missing");
        assertPlanMatchesRecord(confirmation.plan, current);
        if (
          current.state !== "active" ||
          current.revision !== confirmation.expectedRevision ||
          !HEX_32.test(confirmation.transactionHash)
        ) {
          throw new Error("prover reservation confirmation mismatch");
        }
        if (current.pendingTransition === null) {
          // SQLite confirmation can commit before its acknowledgement reaches
          // the workflow journal. Reconciliation may acknowledge that exact
          // committed transition again, without rotating leases or revision.
          const row = selectConfirmedLineage.get(
            current.reservationId,
            `${confirmation.transactionHash}#0`,
          ) as LineageRow | undefined;
          if (
            current.lastConfirmedTransitionDigest !== null &&
            current.lastConfirmedTransitionDigest ===
              confirmation.transitionDigest &&
            row !== undefined &&
            parseLineageRow(row).transitionDigest ===
              confirmation.transitionDigest
          )
            return current;
          throw new Error("prover reservation confirmation mismatch");
        }
        if (
          current.pendingTransition.transitionDigest !==
            confirmation.transitionDigest ||
          current.pendingTransition.transactionHash !==
            confirmation.transactionHash
        ) {
          throw new Error("prover reservation confirmation mismatch");
        }
        const consumed = new Set(current.pendingTransition.consumedOutRefs);
        const active = [
          ...current.activeInputs.filter(({ outRef }) => !consumed.has(outRef)),
          ...current.pendingTransition.producedInputs.filter(
            ({ outRef }) =>
              !outputTransferredToAnotherReservation(
                current.reservationId,
                outRef,
              ),
          ),
        ];
        const next = nextRecord({
          current,
          activeInputs: active,
          pendingTransition: null,
          lastConfirmedTransitionDigest:
            current.pendingTransition.transitionDigest,
        });
        const confirmedLineage = confirmLineage.run(
          current.reservationId,
          current.pendingTransition.transitionDigest,
        );
        if (confirmedLineage.changes < 1) {
          const prior = selectConfirmedLineage.get(
            current.reservationId,
            `${confirmation.transactionHash}#0`,
          ) as LineageRow | undefined;
          if (
            prior === undefined ||
            parseLineageRow(prior).transitionDigest !==
              confirmation.transitionDigest
          )
            throw new Error("prover reservation confirmation lacks lineage");
        }
        writeRecord(current.recordDigest, next);
        rebuildLeases();
        return next;
      });
    },
    abandonPendingTransition: async (abandonment) => {
      assertPlan(abandonment.plan);
      return transaction(() => {
        const current = readOne(abandonment.plan.reservationId);
        if (current === null) throw new Error("prover reservation is missing");
        assertPlanMatchesRecord(abandonment.plan, current);
        const handoff = parseWorkflowFundingAbandonmentHandoff(
          abandonment.handoff,
        );
        assertHandoffReservation(handoff, current);
        if (
          current.state !== "active" ||
          current.revision !== abandonment.expectedRevision
        )
          throw new Error("prover reservation abandonment mismatch");
        if (current.pendingTransition === null) {
          const saved = unacknowledgedAbandonment(current.reservationId);
          if (
            saved === null ||
            saved.transitionDigest !== abandonment.transitionDigest ||
            computeDeploymentManifestJsonDigest(saved.handoff) !==
              computeDeploymentManifestJsonDigest(handoff)
          )
            throw new Error("prover reservation abandonment mismatch");
          return current;
        }
        const { transitionDigest, ...transition } = current.pendingTransition;
        if (
          transitionDigest !== abandonment.transitionDigest ||
          handoff.reconciliation.txHash !== transition.transactionHash
        )
          throw new Error("prover reservation abandonment mismatch");
        const value = { transition, handoff };
        insertAbandonment.run(
          current.reservationId,
          transitionDigest,
          computeDeploymentManifestJsonDigest(value),
          watcherCanonicalJson(value),
        );
        const next = nextRecord({ current, pendingTransition: null });
        deletePendingLineage.run(current.reservationId, transitionDigest);
        writeRecord(current.recordDigest, next);
        rebuildLeases();
        return next;
      });
    },
    releaseIdle: async ({ plan, expectedRevision }) => {
      assertPlan(plan);
      return transaction(() => {
        const current = readOne(plan.reservationId);
        if (current === null) throw new Error("prover reservation is missing");
        assertPlanMatchesRecord(plan, current);
        if (current.revision !== expectedRevision)
          throw new Error("prover reservation idle release revision changed");
        if (
          current.state !== "active" ||
          current.activeInputs.length === 0 ||
          current.pendingTransition !== null ||
          unacknowledgedAbandonment(current.reservationId) !== null
        )
          return current;
        // The workflow authority checks that all signed attempts are resolved;
        // this also admits refreshing unsigned inputs spent after confirmation.
        const next = nextRecord({ current, activeInputs: [] });
        writeRecord(current.recordDigest, next);
        rebuildLeases();
        return next;
      });
    },
    acknowledgeAbandonment: async (acknowledgement) => {
      assertPlan(acknowledgement.plan);
      return transaction(() => {
        const current = readOne(acknowledgement.plan.reservationId);
        if (current === null) throw new Error("prover reservation is missing");
        assertPlanMatchesRecord(acknowledgement.plan, current);
        const handoff = parseWorkflowFundingAbandonmentHandoff(
          acknowledgement.handoff,
        );
        assertHandoffReservation(handoff, current);
        const candidates = (selectAllAbandonments.all() as AbandonmentRow[])
          .map(readAbandonmentRow)
          .filter(
            (saved) =>
              saved.reservationId === current.reservationId &&
              saved.transition.transactionHash ===
                handoff.reconciliation.txHash,
          );
        const saved = candidates[0];
        if (
          candidates.length !== 1 ||
          saved === undefined ||
          current.state !== "active" ||
          current.pendingTransition !== null ||
          current.revision !== acknowledgement.expectedRevision ||
          computeDeploymentManifestJsonDigest(saved.handoff) !==
            computeDeploymentManifestJsonDigest(handoff)
        )
          throw new Error(
            "prover reservation abandonment acknowledgement mismatch",
          );
        if (saved.acknowledgedRevision !== null) {
          if (saved.acknowledgedRevision !== current.revision)
            throw new Error(
              "prover reservation abandonment acknowledgement revision changed",
            );
          return current;
        }
        const next = nextRecord({ current });
        if (
          acknowledgeAbandonment.run(
            next.revision,
            current.reservationId,
            saved.transitionDigest,
          ).changes !== 1
        )
          throw new Error(
            "prover reservation abandonment acknowledgement raced",
          );
        writeRecord(current.recordDigest, next);
        rebuildLeases();
        return next;
      });
    },
    markConflict: async (conflict) => {
      assertPlan(conflict.plan);
      return transaction(() => {
        const current = readOne(conflict.plan.reservationId);
        if (current === null) throw new Error("prover reservation is missing");
        assertPlanMatchesRecord(conflict.plan, current);
        if (
          current.state !== "active" ||
          current.revision !== conflict.expectedRevision
        ) {
          throw new Error("prover reservation conflict transition mismatch");
        }
        const next = nextRecord({
          current,
          state: "conflict",
          pendingTransition: null,
          conflictCode: conflict.code,
        });
        if (current.pendingTransition !== null) {
          deletePendingLineage.run(
            current.reservationId,
            current.pendingTransition.transitionDigest,
          );
        }
        writeRecord(current.recordDigest, next);
        rebuildLeases();
        return next;
      });
    },
    release: async (release) => {
      assertPlan(release.plan);
      return transaction(() => {
        const current = readOne(release.plan.reservationId);
        if (current === null) throw new Error("prover reservation is missing");
        assertPlanMatchesRecord(release.plan, current);
        const handoff = parseWorkflowFundingCompletionHandoff(release.handoff);
        assertHandoffReservation(handoff, current);
        if (
          current.revision !== release.expectedRevision ||
          current.pendingTransition !== null ||
          unacknowledgedAbandonment(current.reservationId) !== null
        )
          throw new Error("prover reservation release mismatch");
        const digest = computeDeploymentManifestJsonDigest(handoff);
        if (current.state === "released") {
          const row = selectCompletionHandoff.get(current.reservationId) as
            | HandoffRow
            | undefined;
          if (row !== undefined) {
            const prior = readHandoffRow(row);
            if (
              prior.kind === "completion" &&
              prior.handoff.completion.kind === "terminal_included" &&
              handoff.completion.kind === "completed"
            ) {
              // Anchoring closes an already released execution. Resource use
              // remains independent of how long the finality stamp takes.
              database
                .prepare(
                  "DELETE FROM watcher_prover_funding_handoff_v1 WHERE reservation_id = ? AND kind = 'completion'",
                )
                .run(current.reservationId);
              persistHandoff(current, "completion", digest, handoff);
              return current;
            }
          }
          if (
            row === undefined ||
            readHandoffRow(row).identityDigest !== digest
          )
            throw new Error("prover reservation release handoff mismatch");
          return current;
        }
        if (current.state !== "active")
          throw new Error("prover reservation release mismatch");
        persistHandoff(current, "completion", digest, handoff);
        const next = nextRecord({
          current,
          state: "released",
          activeInputs: [],
          pendingTransition: null,
          conflictCode: null,
        });
        writeRecord(current.recordDigest, next);
        rebuildLeases();
        return next;
      });
    },
  });

  return Object.freeze({
    schemaVersion: WATCHER_SQLITE_PROVER_FUNDING_RESERVATION_STORE,
    store,
    close: () => database.close(),
  });
};

export const openWatcherSqliteProverFundingReservationStore = async (input: {
  readonly path: string;
  readonly busyTimeoutMs?: number;
}): Promise<WatcherSqliteProverFundingReservationStoreRuntime> =>
  await openInternal(input, assertWatcherProverFundingReservationPlan);

/** Test-only storage seam. Production always requires an opaque admitted plan. */
export const unsafeOpenWatcherSqliteProverFundingReservationStoreForTest =
  async (
    input: Readonly<{ path: string; busyTimeoutMs?: number }>,
    unsafeAssertPlanForTest: (
      plan: WatcherProverFundingReservationPlan,
    ) => void,
  ): Promise<WatcherSqliteProverFundingReservationStoreRuntime> =>
    await openInternal(input, unsafeAssertPlanForTest);
