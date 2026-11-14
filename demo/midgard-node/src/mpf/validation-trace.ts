/**
 * Validation-trace member construction and event-key set validation.
 *
 * P1 commits a placeholder descriptor per transaction event and retains no
 * validation-machine state.
 */

import * as SDK from "@al-ft/midgard-sdk";
import {
  placeholderValidationTraceDescriptorV1,
  validationTransactionCommitmentV1,
} from "@al-ft/midgard-validation";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import * as PendingBlockFinalizationsDB from "../database/pendingBlockFinalizations.js";
import { DatabaseError } from "../database/utils/common.js";
import { MpfError } from "./errors.js";
import {
  eventKeyCbor,
  type ValidationTraceTransactionInput,
} from "./trace-events.js";

export type ValidationTraceBuildInput = {
  readonly transactions: readonly ValidationTraceTransactionInput[];
};

export type RetainedValidationTraceMember = {
  readonly eventKey: SDK.EventKey;
  readonly keyCbor: Buffer;
  readonly valueCbor: Buffer;
  readonly value: SDK.ValidationTraceDescriptor;
};

export type ValidationTraceBuildResult = {
  readonly validationTracesRoot: string;
  readonly validationTraceMembers: readonly RetainedValidationTraceMember[];
  readonly validationTraceCount: number;
};

export const buildPlaceholderValidationTraceMembers = (
  input: ValidationTraceBuildInput,
): Effect.Effect<
  readonly RetainedValidationTraceMember[],
  DatabaseError | MpfError
> =>
  Effect.forEach(input.transactions, (transaction) =>
    Effect.gen(function* () {
      const keyCbor = yield* eventKeyCbor(transaction.eventKey);
      const value = yield* Effect.try({
        try: () =>
          SDK.validationTraceDescriptorDataFromCore(
            placeholderValidationTraceDescriptorV1(
              {
                eventKeyCbor: keyCbor,
                transactionCommitment: validationTransactionCommitmentV1({
                  canonicalTransactionCbor:
                    transaction.canonicalTransactionCbor,
                  sourceKind: transaction.sourceKind,
                }),
              },
              transaction.verdict,
              transaction.rejectionReason,
            ),
          ),
        catch: (cause) =>
          new DatabaseError({
            table: PendingBlockFinalizationsDB.tableName,
            message: "Failed to derive a placeholder validation descriptor",
            cause,
          }),
      });
      return {
        eventKey: transaction.eventKey,
        keyCbor,
        valueCbor: Buffer.from(
          LucidData.to(value, SDK.ValidationTraceDescriptor),
          "hex",
        ),
        value,
      };
    }),
  );

export const validateValidationTraceEventKeySet = ({
  expectedEventKeys,
  transitionEventKeyCbors,
  members,
}: {
  readonly expectedEventKeys: readonly SDK.EventKey[];
  readonly transitionEventKeyCbors: ReadonlySet<string>;
  readonly members: readonly Pick<
    RetainedValidationTraceMember,
    "eventKey" | "keyCbor"
  >[];
}): Effect.Effect<void, DatabaseError | MpfError> =>
  Effect.gen(function* () {
    const expected = new Set<string>();
    for (const eventKey of expectedEventKeys) {
      const keyHex = (yield* eventKeyCbor(eventKey)).toString("hex");
      if (expected.has(keyHex)) {
        return yield* Effect.fail(
          new DatabaseError({
            table: PendingBlockFinalizationsDB.tableName,
            message:
              "Validation trace inputs contain a duplicate canonical event key",
            cause: `event_key_cbor=${keyHex}`,
          }),
        );
      }
      expected.add(keyHex);
    }
    if (members.length !== expected.size) {
      return yield* Effect.fail(
        new DatabaseError({
          table: PendingBlockFinalizationsDB.tableName,
          message:
            "Validation trace provider returned the wrong descriptor count",
          cause: `expected=${expected.size.toString()},actual=${members.length.toString()}`,
        }),
      );
    }

    const seen = new Set<string>();
    for (const member of members) {
      const keyHex = member.keyCbor.toString("hex");
      if (
        seen.has(keyHex) ||
        !expected.has(keyHex) ||
        !transitionEventKeyCbors.has(keyHex) ||
        !member.keyCbor.equals(yield* eventKeyCbor(member.eventKey))
      ) {
        return yield* Effect.fail(
          new DatabaseError({
            table: PendingBlockFinalizationsDB.tableName,
            message:
              "Validation trace provider returned a duplicate, foreign, or non-canonical event key",
            cause: `event_key_cbor=${keyHex}`,
          }),
        );
      }
      seen.add(keyHex);
    }
    if (seen.size !== expected.size) {
      return yield* Effect.fail(
        new DatabaseError({
          table: PendingBlockFinalizationsDB.tableName,
          message:
            "Validation trace provider omitted a required canonical event key",
          cause: `expected=${expected.size.toString()},actual=${seen.size.toString()}`,
        }),
      );
    }
  });
