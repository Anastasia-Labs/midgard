import * as SDK from "@al-ft/midgard-sdk";
import { Data as LucidData, toUnit, type UTxO } from "@lucid-evolution/lucid";
import { Data as EffectData, Effect, Option } from "effect";

import * as DepositsDB from "../database/deposits.js";
import * as ForcedTransactionsDB from "../database/forcedTransactions.js";
import { DatabaseError } from "../database/utils/common.js";
import * as WithdrawalsDB from "../database/withdrawals.js";
import { Database, Lucid, MidgardContracts } from "../services/index.js";
import { outRefLabel } from "../tx-context.js";
import { buildAuthenticatedRootFromEncodedEntries } from "../workers/commit-block-header/transition-roots.js";
import { keyValuePhasProof, MpfError } from "../workers/utils/mpf.js";
import { parseEventId } from "./command-utils.js";

export type EventKind = "deposit" | "withdrawal" | "tx-order";

export type EventSettlementProofLookup = {
  readonly kind: EventKind;
  readonly eventId: Buffer;
};

export type EventSettlementProofResolution =
  | {
      readonly kind: "deposit";
      readonly eventId: string;
      readonly headerHash: string;
      readonly settlementRefInput: UTxO;
      readonly settlementDatum: SDK.SettlementDatum;
      readonly root: string;
      readonly proof: SDK.RawRootMembershipProof;
      readonly proofCbor: string;
      readonly status: DepositsDB.Status;
      readonly entry: DepositsDB.Entry;
    }
  | {
      readonly kind: "withdrawal";
      readonly eventId: string;
      readonly headerHash: string;
      readonly settlementRefInput: UTxO;
      readonly settlementDatum: SDK.SettlementDatum;
      readonly root: string;
      readonly proof: SDK.RawRootMembershipProof;
      readonly proofCbor: string;
      readonly status: WithdrawalsDB.Status;
      readonly validity: WithdrawalsDB.Validity | null;
      readonly entry: WithdrawalsDB.Entry;
    }
  | {
      readonly kind: "tx-order";
      readonly eventId: string;
      readonly headerHash: string;
      readonly settlementRefInput: UTxO;
      readonly settlementDatum: SDK.SettlementDatum;
      readonly root: string;
      readonly proof: SDK.RawRootMembershipProof;
      readonly proofCbor: string;
      readonly status: ForcedTransactionsDB.Status;
      readonly validity: SDK.MidgardTxValidity;
      readonly entry: ForcedTransactionsDB.Entry;
    };

export type SerializedEventSettlementProofResolution = {
  readonly kind: EventKind;
  readonly eventId: string;
  readonly headerHash: string;
  readonly settlementOutRef: string;
  readonly root: string;
  readonly proofCbor: string;
  readonly status: string;
  readonly validity?: string | null;
};

export class EventSettlementProofError extends EffectData.TaggedError(
  "EventSettlementProofError",
)<{
  readonly message: string;
  readonly cause: unknown;
}> {}

export const parseEventSettlementProofLookup = ({
  kind,
  eventId,
}: {
  readonly kind: string;
  readonly eventId: string;
}): EventSettlementProofLookup => {
  const normalizedKind = kind.trim().toLowerCase();
  if (
    normalizedKind !== "deposit" &&
    normalizedKind !== "withdrawal" &&
    normalizedKind !== "tx-order"
  ) {
    throw new Error(
      '--kind must be either "deposit", "withdrawal", or "tx-order".',
    );
  }
  return {
    kind: normalizedKind as EventKind,
    eventId: parseEventId(eventId, "--event-id"),
  };
};

const requireProjectedHeaderHash = (
  kind: EventKind,
  eventId: Buffer,
  headerHash: Buffer | null,
): Effect.Effect<Buffer, EventSettlementProofError> =>
  headerHash === null
    ? Effect.fail(
        new EventSettlementProofError({
          message: `${kind} event has not been assigned to a block header`,
          cause: { eventId: eventId.toString("hex") },
        }),
      )
    : Effect.succeed(headerHash);

const fetchSettlementRefInput = (
  headerHash: Buffer,
): Effect.Effect<
  { readonly utxo: UTxO; readonly datum: SDK.SettlementDatum },
  EventSettlementProofError | SDK.LucidError,
  Lucid | MidgardContracts
> =>
  Effect.gen(function* () {
    const { api: lucid } = yield* Lucid;
    const contracts = yield* MidgardContracts;
    const unit = toUnit(
      contracts.settlement.policyId,
      headerHash.toString("hex"),
    );
    const candidates = yield* Effect.tryPromise({
      try: () =>
        lucid.utxosAtWithUnit(contracts.settlement.spendingScriptAddress, unit),
      catch: (cause) =>
        new SDK.LucidError({
          message: "Failed to fetch settlement UTxOs",
          cause,
        }),
    });
    const matches = candidates.filter(
      (utxo) => (utxo.assets[unit] ?? 0n) === 1n,
    );
    if (matches.length !== 1) {
      return yield* Effect.fail(
        new EventSettlementProofError({
          message: "Expected exactly one settlement UTxO for header hash",
          cause: {
            headerHash: headerHash.toString("hex"),
            found: matches.length,
          },
        }),
      );
    }
    const utxo = matches[0]!;
    if (utxo.datum === undefined) {
      return yield* Effect.fail(
        new EventSettlementProofError({
          message: "Settlement UTxO has no inline datum",
          cause: `${utxo.txHash}#${utxo.outputIndex.toString()}`,
        }),
      );
    }
    const datum = yield* Effect.try({
      try: () =>
        LucidData.from(utxo.datum!, SDK.SettlementDatum) as SDK.SettlementDatum,
      catch: (cause) =>
        new EventSettlementProofError({
          message: "Failed to decode settlement datum",
          cause,
        }),
    });
    return { utxo, datum };
  });

const rootProofError = (
  message: string,
  cause: unknown,
): EventSettlementProofError =>
  new EventSettlementProofError({ message, cause });

const buildSourceMembershipProof = ({
  domain,
  expectedRoot,
  entries,
  targetKey,
  label,
}: {
  readonly domain: SDK.RootDomain;
  readonly expectedRoot: string;
  readonly entries: readonly { readonly key: Buffer; readonly value: Buffer }[];
  readonly targetKey: Buffer;
  readonly label: string;
}): Effect.Effect<
  SDK.RawRootMembershipProof,
  EventSettlementProofError | MpfError
> =>
  Effect.gen(function* () {
    const root = yield* buildAuthenticatedRootFromEncodedEntries(
      domain,
      entries,
    );
    if (root.root !== expectedRoot) {
      return yield* Effect.fail(
        rootProofError(
          `Recomputed ${label} root does not match settlement datum`,
          {
            expected: expectedRoot,
            actual: root.root,
            phasRoot: root.phasRoot,
            count: root.count.toString(),
          },
        ),
      );
    }

    const matchingEntry = root.entries.find((entry) =>
      entry.key.equals(targetKey),
    );
    if (matchingEntry === undefined) {
      return yield* Effect.fail(
        rootProofError(`${label} event is not present in projected root`, {
          eventId: targetKey.toString("hex"),
        }),
      );
    }

    const proof = yield* keyValuePhasProof(
      root.entries.map((entry) => entry.key),
      root.entries.map((entry) => entry.value),
      targetKey,
    );
    return {
      domain,
      root: root.root,
      phas_root: root.phasRoot,
      count: root.count,
      key: matchingEntry.key.toString("hex"),
      value: matchingEntry.value.toString("hex"),
      proof,
    };
  });

export const resolveEventSettlementProofProgram = (
  lookup: EventSettlementProofLookup,
): Effect.Effect<
  EventSettlementProofResolution,
  DatabaseError | EventSettlementProofError | MpfError | SDK.LucidError,
  Database | Lucid | MidgardContracts
> =>
  Effect.gen(function* () {
    if (lookup.kind === "deposit") {
      const maybeEntry = yield* DepositsDB.retrieveByEventId(lookup.eventId);
      if (Option.isNone(maybeEntry)) {
        return yield* Effect.fail(
          new EventSettlementProofError({
            message: "Deposit event not found",
            cause: lookup.eventId.toString("hex"),
          }),
        );
      }
      const entry = maybeEntry.value;
      const headerHash = yield* requireProjectedHeaderHash(
        "deposit",
        lookup.eventId,
        entry[DepositsDB.Columns.PROJECTED_HEADER_HASH],
      );
      const settlement = yield* fetchSettlementRefInput(headerHash);
      const entries =
        yield* DepositsDB.retrieveByProjectedHeaderHash(headerHash);
      const proof = yield* buildSourceMembershipProof({
        domain: SDK.ROOT_DOMAINS.deposits,
        expectedRoot: settlement.datum.deposits_root,
        entries: entries.map((row) => ({
          key: row[DepositsDB.Columns.ID],
          value: row[DepositsDB.Columns.INFO],
        })),
        targetKey: lookup.eventId,
        label: "deposit",
      });
      return {
        kind: "deposit",
        eventId: lookup.eventId.toString("hex"),
        headerHash: headerHash.toString("hex"),
        settlementRefInput: settlement.utxo,
        settlementDatum: settlement.datum,
        root: proof.root,
        proof,
        proofCbor: LucidData.to(proof, SDK.RawRootMembershipProof),
        status: entry[DepositsDB.Columns.STATUS],
        entry,
      };
    }

    if (lookup.kind === "withdrawal") {
      const maybeEntry = yield* WithdrawalsDB.retrieveByEventId(lookup.eventId);
      if (Option.isNone(maybeEntry)) {
        return yield* Effect.fail(
          new EventSettlementProofError({
            message: "Withdrawal event not found",
            cause: lookup.eventId.toString("hex"),
          }),
        );
      }
      const entry = maybeEntry.value;
      const headerHash = yield* requireProjectedHeaderHash(
        "withdrawal",
        lookup.eventId,
        entry[WithdrawalsDB.Columns.PROJECTED_HEADER_HASH],
      );
      const entries =
        yield* WithdrawalsDB.retrieveByProjectedHeaderHash(headerHash);
      const keyValues = yield* Effect.forEach(
        entries,
        WithdrawalsDB.toRootKeyValue,
      );
      const settlement = yield* fetchSettlementRefInput(headerHash);
      const proof = yield* buildSourceMembershipProof({
        domain: SDK.ROOT_DOMAINS.withdrawals,
        expectedRoot: settlement.datum.withdrawals_root,
        entries: keyValues,
        targetKey: lookup.eventId,
        label: "withdrawal",
      });
      return {
        kind: "withdrawal",
        eventId: lookup.eventId.toString("hex"),
        headerHash: headerHash.toString("hex"),
        settlementRefInput: settlement.utxo,
        settlementDatum: settlement.datum,
        root: proof.root,
        proof,
        proofCbor: LucidData.to(proof, SDK.RawRootMembershipProof),
        status: entry[WithdrawalsDB.Columns.STATUS],
        validity: entry[WithdrawalsDB.Columns.VALIDITY],
        entry,
      };
    }

    const maybeEntry = yield* ForcedTransactionsDB.retrieveByTxOrderId(
      lookup.eventId,
    );
    if (Option.isNone(maybeEntry)) {
      return yield* Effect.fail(
        new EventSettlementProofError({
          message: "Tx-order event not found",
          cause: lookup.eventId.toString("hex"),
        }),
      );
    }
    const entry = maybeEntry.value;
    const headerHash = yield* requireProjectedHeaderHash(
      "tx-order",
      lookup.eventId,
      entry[ForcedTransactionsDB.Columns.PROJECTED_HEADER_HASH],
    );
    const entries =
      yield* ForcedTransactionsDB.retrieveByProjectedHeaderHash(headerHash);
    const settlement = yield* fetchSettlementRefInput(headerHash);
    const proof = yield* buildSourceMembershipProof({
      domain: SDK.ROOT_DOMAINS.forcedTransactionsV1,
      expectedRoot: settlement.datum.forced_transactions_root,
      entries: entries.map(ForcedTransactionsDB.toRootKeyValue),
      targetKey: lookup.eventId,
      label: "tx-order",
    });
    return {
      kind: "tx-order",
      eventId: lookup.eventId.toString("hex"),
      headerHash: headerHash.toString("hex"),
      settlementRefInput: settlement.utxo,
      settlementDatum: settlement.datum,
      root: proof.root,
      proof,
      proofCbor: LucidData.to(proof, SDK.RawRootMembershipProof),
      status: entry[ForcedTransactionsDB.Columns.STATUS],
      validity: entry[ForcedTransactionsDB.Columns.OPERATOR_VALIDITY],
      entry,
    };
  });

export const serializeEventSettlementProofResolution = (
  resolution: EventSettlementProofResolution,
): SerializedEventSettlementProofResolution => ({
  kind: resolution.kind,
  eventId: resolution.eventId,
  headerHash: resolution.headerHash,
  settlementOutRef: outRefLabel(resolution.settlementRefInput),
  root: resolution.root,
  proofCbor: resolution.proofCbor,
  status: resolution.status,
  ...(resolution.kind === "withdrawal" || resolution.kind === "tx-order"
    ? { validity: resolution.validity }
    : {}),
});
