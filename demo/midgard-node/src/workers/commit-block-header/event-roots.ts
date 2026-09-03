import {
  MIDGARD_CONSENSUS_PROFILE_V1,
  type MidgardConsensusProfileV1,
} from "@al-ft/midgard-core/consensus-profile-v1";
import { Effect, Option } from "effect";

import {
  DepositsDB,
  ForcedTransactionsDB,
  WithdrawalsDB,
} from "../../database/index.js";
import type { DatabaseError } from "../../database/utils/common.js";
import type { MpfError } from "../utils/mpf.js";
import { buildAuthenticatedRootFromEncodedEntries } from "./transition-roots.js";

export const resolveDepositsRoot = (
  depositEntries: readonly DepositsDB.Entry[],
): Effect.Effect<Option.Option<string>, MpfError, never> =>
  Effect.gen(function* () {
    if (depositEntries.length <= 0) {
      return Option.none();
    }
    const eventIds = depositEntries.map(
      (entry) => entry[DepositsDB.Columns.ID],
    );
    const eventInfos = depositEntries.map(
      (entry) => entry[DepositsDB.Columns.INFO],
    );
    const root = yield* buildAuthenticatedRootFromEncodedEntries(
      "DepositsRootDomain",
      eventIds.map((key, index) => ({
        key,
        value: eventInfos[index]!,
      })),
    );
    return Option.some(root.root);
  });

export const resolveWithdrawalsRoot = (
  withdrawalEntries: readonly WithdrawalsDB.Entry[],
): Effect.Effect<Option.Option<string>, MpfError | DatabaseError, never> =>
  Effect.gen(function* () {
    if (withdrawalEntries.length <= 0) {
      return Option.none();
    }
    const keyValues = yield* Effect.forEach(
      withdrawalEntries,
      WithdrawalsDB.toRootKeyValue,
    );
    const root = yield* buildAuthenticatedRootFromEncodedEntries(
      "WithdrawalsRootDomain",
      keyValues,
    );
    return Option.some(root.root);
  });

export const resolveForcedTransactionsRoot = (
  forcedTransactionEntries: readonly ForcedTransactionsDB.Entry[],
  _consensusProfile: MidgardConsensusProfileV1 = MIDGARD_CONSENSUS_PROFILE_V1,
): Effect.Effect<Option.Option<string>, MpfError, never> =>
  Effect.gen(function* () {
    if (forcedTransactionEntries.length <= 0) {
      return Option.none();
    }
    const root = yield* buildAuthenticatedRootFromEncodedEntries(
      "ForcedTransactionsV1RootDomain",
      forcedTransactionEntries.map(ForcedTransactionsDB.toRootKeyValue),
    );
    return Option.some(root.root);
  });
