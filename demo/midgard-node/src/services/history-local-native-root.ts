import { assertDeploymentMarkerMatches } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Data } from "@lucid-evolution/lucid";
import { Effect, Option } from "effect";

import { SIGNED_INTENT_RELEASE_RECOVERY_DOMAIN } from "../database/eventHistoryRecoveryPlans.prepare-history-recovery-plan.js";
import * as Pending from "../database/pendingBlockFinalizations.js";
import { eventHistoryCanonicalJson } from "../l1-event-history-source.js";
import { sha256 } from "../sha256.js";
import { MAX_LANDED_MERGE_CATCH_UP } from "../transactions/state-queue/merge-to-confirmed-state.finalize-confirmed-merge-program.js";
import {
  revalidateForeignCommitBase,
  type VerifiedForeignCommitBase,
} from "../workers/commit-block-header.verify-foreign-base.js";
import { decodeHeader } from "../workers/commit-block-header/da-payload.verify-payload-commitments.js";
import { signedIntentReplacementDigest } from "./canonical-journal-recovery.js";
import {
  journalIdentity,
  signedCommitNode,
} from "./history-expired-intent-release.signed-commit-node.js";
import {
  ContractDeploymentIdentity,
  MidgardContracts,
} from "./midgard-contracts.js";

type Input = {
  readonly base: VerifiedForeignCommitBase;
  readonly durableRoot: string;
  readonly ownerBinarySha256: string;
};

/** An applied release can retain its parent root even after that parent's
 * local journal was pruned. Its exact signed journal and immutable applied
 * plan still bind the parent datum, hash and native replay base. */
const releasedIntentParent = (input: Input) =>
  Effect.gen(function* () {
    const identity = yield* ContractDeploymentIdentity;
    if (identity.deploymentMarker === undefined) return false;
    const manifestId = identity.deploymentMarker.manifestId;
    const contracts = yield* MidgardContracts;
    const sql = yield* SqlClient.SqlClient;
    const plans = yield* sql<{
      intent: string;
      recovery_id: Buffer;
      header_hash: Buffer;
    }>`
    SELECT intent,recovery_id,header_hash FROM event_history_recovery_plans
    WHERE binding_digest = ${Buffer.from(input.base.history.coverage.bindingDigest, "hex")}
      AND manifest_id = ${Buffer.from(manifestId, "hex")}
      AND state = 'applied' ORDER BY updated_at DESC,recovery_id DESC LIMIT 1`;
    const plan = plans[0];
    if (plan === undefined) return false;
    const intent = yield* Effect.try(
      () =>
        JSON.parse(plan.intent) as { domain?: unknown; expectedRoot?: unknown },
    );
    if (intent?.domain !== SIGNED_INTENT_RELEASE_RECOVERY_DOMAIN) return false;
    const journal = yield* Pending.retrieveByHeaderHash(plan.header_hash);
    if (Option.isNone(journal)) return false;
    const record = journal.value;
    const C = Pending.Columns;
    const replay = record.nativeMpfReplay;
    const signed = record[C.SIGNED_TX_CBOR];
    const intended = record[C.INTENDED_TX_HASH];
    if (
      record[C.STATUS] !== Pending.Status.Abandoned ||
      signed == null ||
      intended == null ||
      record[C.CONSENSUS_PROFILE_ID] !== identity.consensusProfile.profileId ||
      record[C.CORRECTION_TRANSITION_DIGEST] !==
        signedIntentReplacementDigest(record) ||
      record[C.BASE_UTXOS_ROOT] !== input.durableRoot ||
      replay === undefined ||
      replay.ownerBinarySha256.toString("hex") !== input.ownerBinarySha256 ||
      replay.baseRoot.toString("hex") !== input.durableRoot ||
      replay.candidateRoot.toString("hex") !== record[C.EXPECTED_UTXOS_ROOT] ||
      (intent.expectedRoot !== input.durableRoot &&
        intent.expectedRoot !== record[C.EXPECTED_UTXOS_ROOT])
    )
      return false;
    yield* Effect.try(() =>
      assertDeploymentMarkerMatches(
        identity.deploymentMarker!,
        {
          schemaVersion: record[C.DEPLOYMENT_MARKER_SCHEMA_VERSION],
          manifestId: record[C.DEPLOYMENT_MANIFEST_ID],
        },
        "released intent native parent",
      ),
    );
    const payload = eventHistoryCanonicalJson({
      domain: SIGNED_INTENT_RELEASE_RECOVERY_DOMAIN,
      bindingDigest: input.base.history.coverage.bindingDigest,
      manifestId,
      headerHash: record[C.HEADER_HASH].toString("hex"),
      signedTransactionHash: intended.toString("hex"),
      signedTransactionCborSha256: sha256(signed).toString("hex"),
      targetRoot: input.durableRoot,
      journalDigest: journalIdentity(record),
      expectedRoot: intent.expectedRoot,
    });
    if (
      payload !== plan.intent ||
      !sha256(Buffer.from(payload)).equals(plan.recovery_id)
    )
      return false;
    const signedNode = yield* signedCommitNode(record, contracts);
    const signedHeader = yield* SDK.getHeaderFromStateQueueDatum(
      signedNode.node.datum,
    );
    const parentHash = record[C.BASE_TAIL_HEADER_HASH].toString("hex");
    const parent = yield* Effect.try(() =>
      SDK.linkedListDatumToNodeView(
        Data.from(record[C.BASE_TAIL_DATUM_CBOR], SDK.LinkedListDatum),
        SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + parentHash,
      ),
    );
    const header = yield* SDK.getHeaderFromStateQueueDatum(parent);
    if (
      signedHeader.prevHeaderHash !== parentHash ||
      signedHeader.prevUtxosRoot !== input.durableRoot ||
      signedHeader.utxosRoot !== record[C.EXPECTED_UTXOS_ROOT] ||
      (yield* SDK.hashBlockHeader(header)) !== parentHash ||
      header.utxosRoot !== input.durableRoot ||
      header.prevHeaderHash !== input.base.headerHash ||
      header.prevUtxosRoot !== input.base.root ||
      parentHash === input.base.headerHash ||
      input.base.importedBlocks.some((block) => block.headerHash === parentHash)
    )
      return false;
    yield* revalidateForeignCommitBase(input.base);
    return true;
  });

/** A removed local suffix remains owned by its correction/signed disposition.
 * This only identifies that obligation; it never restores a root or authorizes
 * Ready. The history observer must collect the required expiry/finality proof.
 */
export const retainedLocalNativeRoot = (input: Input) =>
  Effect.gen(function* () {
    const { base, durableRoot } = input;
    if (base.importedBlocks.some((block) => block.kind === "foreign"))
      return false;
    const identity = yield* ContractDeploymentIdentity;
    if (identity.deploymentMarker === undefined) return false;
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{ header_hash: Buffer }>`SELECT header_hash
      FROM pending_block_finalizations
      WHERE expected_utxos_root = ${durableRoot}
        AND status IN ('finalized','abandoned')
      ORDER BY block_end_time DESC, created_at DESC LIMIT 1`;
    const row = rows[0];
    if (row === undefined) return yield* releasedIntentParent(input);
    let hash = row.header_hash.toString("hex");
    let root = durableRoot;
    const seen = new Set<string>();
    while (root !== base.root) {
      if (
        seen.size >= MAX_LANDED_MERGE_CATCH_UP ||
        seen.has(hash) ||
        hash === base.headerHash ||
        base.importedBlocks.some((block) => block.headerHash === hash)
      )
        return false;
      seen.add(hash);
      const journal = yield* Pending.retrieveByHeaderHash(
        Buffer.from(hash, "hex"),
      );
      if (Option.isNone(journal)) return false;
      const record = journal.value;
      yield* Effect.try(() =>
        assertDeploymentMarkerMatches(
          identity.deploymentMarker!,
          {
            schemaVersion:
              record[Pending.Columns.DEPLOYMENT_MARKER_SCHEMA_VERSION],
            manifestId: record[Pending.Columns.DEPLOYMENT_MANIFEST_ID],
          },
          "retained local native root",
        ),
      );
      const header = yield* decodeHeader(record);
      const replay = record.nativeMpfReplay;
      if (
        (record[Pending.Columns.STATUS] !== Pending.Status.Finalized &&
          record[Pending.Columns.STATUS] !== Pending.Status.Abandoned) ||
        record[Pending.Columns.CONSENSUS_PROFILE_ID] !==
          identity.consensusProfile.profileId ||
        (yield* SDK.hashBlockHeader(header)) !== hash ||
        header.utxosRoot !== root ||
        record[Pending.Columns.EXPECTED_UTXOS_ROOT] !== root ||
        header.prevHeaderHash !==
          record[Pending.Columns.BASE_TAIL_HEADER_HASH].toString("hex") ||
        header.prevUtxosRoot !== record[Pending.Columns.BASE_UTXOS_ROOT] ||
        replay === undefined ||
        replay.ownerBinarySha256.toString("hex") !== input.ownerBinarySha256 ||
        replay.baseRoot.toString("hex") !== header.prevUtxosRoot ||
        replay.candidateRoot.toString("hex") !== root
      )
        return false;
      hash = header.prevHeaderHash;
      root = header.prevUtxosRoot;
    }
    if (hash !== base.headerHash) return false;
    yield* revalidateForeignCommitBase(base);
    return true;
  });
