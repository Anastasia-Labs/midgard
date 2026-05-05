import * as SDK from "@al-ft/midgard-sdk";
import { Data as LucidData, toUnit, type UTxO } from "@lucid-evolution/lucid";
import { Data as EffectData, Effect, Option } from "effect";
import * as DepositsDB from "@/database/deposits.js";
import * as WithdrawalsDB from "@/database/withdrawals.js";
import { DatabaseError } from "@/database/utils/common.js";
import { Database, Lucid, MidgardContracts } from "@/services/index.js";
import {
  keyValuePhasProof,
  keyValuePhasRoot,
  MptError,
} from "@/workers/utils/mpt.js";
import { formatJson, parseEventId } from "@/commands/withdrawal-utils.js";

export type EventKind = "deposit" | "withdrawal";

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
      readonly proof: SDK.Proof;
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
      readonly proof: SDK.Proof;
      readonly proofCbor: string;
      readonly status: WithdrawalsDB.Status;
      readonly validity: WithdrawalsDB.Validity | null;
      readonly entry: WithdrawalsDB.Entry;
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
  if (normalizedKind !== "deposit" && normalizedKind !== "withdrawal") {
    throw new Error('--kind must be either "deposit" or "withdrawal".');
  }
  return {
    kind: normalizedKind,
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

export const resolveEventSettlementProofProgram = (
  lookup: EventSettlementProofLookup,
): Effect.Effect<
  EventSettlementProofResolution,
  DatabaseError | EventSettlementProofError | MptError | SDK.LucidError,
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
      const entries =
        yield* DepositsDB.retrieveByProjectedHeaderHash(headerHash);
      const keys = entries.map((row) => row[DepositsDB.Columns.ID]);
      const values = entries.map((row) => row[DepositsDB.Columns.INFO]);
      const root = yield* keyValuePhasRoot(keys, values);
      const proof = yield* keyValuePhasProof(keys, values, lookup.eventId);
      const settlement = yield* fetchSettlementRefInput(headerHash);
      if (settlement.datum.deposits_root !== root) {
        return yield* Effect.fail(
          new EventSettlementProofError({
            message: "Recomputed deposit root does not match settlement datum",
            cause: {
              expected: settlement.datum.deposits_root,
              actual: root,
            },
          }),
        );
      }
      return {
        kind: "deposit",
        eventId: lookup.eventId.toString("hex"),
        headerHash: headerHash.toString("hex"),
        settlementRefInput: settlement.utxo,
        settlementDatum: settlement.datum,
        root,
        proof,
        proofCbor: LucidData.to(proof, SDK.Proof),
        status: entry[DepositsDB.Columns.STATUS],
        entry,
      };
    }

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
    const root = yield* keyValuePhasRoot(
      keyValues.map((keyValue) => keyValue.key),
      keyValues.map((keyValue) => keyValue.value),
    );
    const proof = yield* keyValuePhasProof(
      keyValues.map((keyValue) => keyValue.key),
      keyValues.map((keyValue) => keyValue.value),
      lookup.eventId,
    );
    const settlement = yield* fetchSettlementRefInput(headerHash);
    if (settlement.datum.withdrawals_root !== root) {
      return yield* Effect.fail(
        new EventSettlementProofError({
          message: "Recomputed withdrawal root does not match settlement datum",
          cause: {
            expected: settlement.datum.withdrawals_root,
            actual: root,
          },
        }),
      );
    }
    return {
      kind: "withdrawal",
      eventId: lookup.eventId.toString("hex"),
      headerHash: headerHash.toString("hex"),
      settlementRefInput: settlement.utxo,
      settlementDatum: settlement.datum,
      root,
      proof,
      proofCbor: LucidData.to(proof, SDK.Proof),
      status: entry[WithdrawalsDB.Columns.STATUS],
      validity: entry[WithdrawalsDB.Columns.VALIDITY],
      entry,
    };
  });

export const serializeEventSettlementProofResolution = (
  resolution: EventSettlementProofResolution,
): SerializedEventSettlementProofResolution => ({
  kind: resolution.kind,
  eventId: resolution.eventId,
  headerHash: resolution.headerHash,
  settlementOutRef: `${resolution.settlementRefInput.txHash}#${resolution.settlementRefInput.outputIndex.toString()}`,
  root: resolution.root,
  proofCbor: resolution.proofCbor,
  status: resolution.status,
  ...(resolution.kind === "withdrawal"
    ? { validity: resolution.validity }
    : {}),
});

export const formatEventSettlementProofResolution = (
  resolution: EventSettlementProofResolution,
): string => formatJson(serializeEventSettlementProofResolution(resolution));
