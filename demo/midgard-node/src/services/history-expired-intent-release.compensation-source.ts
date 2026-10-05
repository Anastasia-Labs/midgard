import { Effect, Option } from "effect";

import type { HistoryRecoveryIntent } from "../database/eventHistoryRecoveryPlans.js";
import { retainedPreparedRecoveryPlan } from "../database/eventHistoryRecoveryPlans.js";
import * as Pending from "../database/pendingBlockFinalizations.js";
import { eventHistoryCanonicalJson } from "../l1-event-history-source.js";
import {
  journalAbandonment,
  SignedIntentReplacementIntegrityError,
} from "./canonical-journal-recovery.js";
import type { ReplacedBlockRevivalInput } from "./history-expired-intent-release.prepare-replaced-block-revival.js";
import { displacementIdentity } from "./history-expired-intent-release.recover-displacement.js";
import {
  anyActiveJournal,
  C,
  sha,
} from "./history-expired-intent-release.table.js";
export type DisplacementCompensationSource = Readonly<{
  recoveryId: string;
  originalRecoveryId: string;
  originalIntent: HistoryRecoveryIntent;
}>;

export const loadCompensationSource = (
  input: ReplacedBlockRevivalInput,
  getSource: () => DisplacementCompensationSource,
) =>
  Effect.gen(function* () {
    const source = getSource();
    const original = source.originalIntent;
    const integrity = (message: string) =>
      Effect.fail(
        new SignedIntentReplacementIntegrityError(original.headerHash, message),
      );
    const retained = yield* retainedPreparedRecoveryPlan(input.binding.digest);
    if (retained?.kind === "displaced_block_revival") {
      if (
        retained.recoveryId !== source.recoveryId ||
        retained.recoveryId !== source.originalRecoveryId ||
        eventHistoryCanonicalJson(retained.displacementIntent) !==
          eventHistoryCanonicalJson(original)
      )
        return yield* integrity(
          "Original displacement changed before compensation",
        );
    } else if (retained?.kind === "displacement_compensation") {
      if (
        retained.recoveryId !== source.recoveryId ||
        retained.intent.originalRecoveryId !== source.originalRecoveryId ||
        eventHistoryCanonicalJson(retained.intent.originalIntent) !==
          eventHistoryCanonicalJson(original)
      )
        return yield* integrity("Compensation original provenance changed");
    } else
      return yield* integrity(
        "Compensation no longer owns a prepared operation",
      );
    const records: Pending.Record[] = [];
    for (const header of [
      original.headerHash,
      ...original.displacedHeaderHashes!,
    ]) {
      const found = yield* Pending.retrieveByHeaderHash(
        Buffer.from(header, "hex"),
        true,
      );
      if (Option.isNone(found))
        return yield* integrity(`Compensation lost retained journal ${header}`);
      records.push(found.value);
    }
    const [winner, ...displaced] = records;
    if (
      winner === undefined ||
      winner[C.STATUS] !== Pending.Status.Abandoned ||
      journalAbandonment(winner) !== "replacement" ||
      displaced.some(
        (record) => record[C.STATUS] !== Pending.Status.Finalized,
      ) ||
      records.some(
        (record) =>
          record[C.DEPLOYMENT_MANIFEST_ID] !== input.checkpoint.manifestId,
      ) ||
      displacementIdentity(winner, displaced) !== original.journalDigest ||
      original.targetRoot !== winner[C.BASE_UTXOS_ROOT] ||
      original.signedTransactionHash !==
        winner[C.INTENDED_TX_HASH]?.toString("hex") ||
      winner[C.SIGNED_TX_CBOR] == null ||
      original.signedTransactionCborSha256 !== sha(winner[C.SIGNED_TX_CBOR]!)
    )
      return yield* integrity(
        "Compensation journals no longer match the original operation",
      );
    if (yield* anyActiveJournal)
      return yield* integrity("An active journal conflicts with compensation");
    let parent = winner[C.BASE_TAIL_HEADER_HASH];
    let root = original.targetRoot;
    for (const record of displaced) {
      if (
        !record[C.BASE_TAIL_HEADER_HASH].equals(parent) ||
        record[C.BASE_UTXOS_ROOT] !== root ||
        record.nativeMpfReplay?.baseRoot.toString("hex") !== root ||
        record.nativeMpfReplay?.candidateRoot.toString("hex") !==
          record[C.EXPECTED_UTXOS_ROOT]
      )
        return yield* integrity(
          "Compensation requires one contiguous retained native chain",
        );
      parent = record[C.HEADER_HASH];
      root = record[C.EXPECTED_UTXOS_ROOT];
    }
    if (
      original.expectedRoot !== root &&
      original.expectedRoot !== original.targetRoot
    )
      return yield* integrity(
        "Original displacement expected root is outside its retained chain",
      );
    return { retained, winner, displaced };
  });
