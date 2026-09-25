import { createHash, randomUUID } from "node:crypto";

import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Data, toUnit } from "@lucid-evolution/lucid";
import { Effect, Option, Ref } from "effect";

import * as Authority from "../../src/database/eventHistoryAuthority.js";
import * as Journal from "../../src/database/eventHistoryJournal.js";
import { materializeCanonicalHistory } from "../../src/database/eventHistoryMaterialization.js";
import {
  decodeBoundEventHistoryLedgerSnapshot,
  eventHistoryCanonicalJson,
  eventHistoryGenesisLosslessSha256,
  type EventHistorySourceBinding,
  HISTORY_GENESIS_DIGEST_ALGORITHM,
} from "../../src/l1-event-history-source.js";
import type { LedgerSnapshotOutput } from "../../src/l1-ledger-snapshot.js";
import { ogmiosEndpointIdentitySha256 } from "../../src/local-ledger-slot.js";
import { BatchSql } from "../../src/services/database.js";
import {
  type EventHistoryOwner,
  makeEventHistoryOwner,
} from "../../src/services/event-history-owner.js";
import type { Globals } from "../../src/services/globals.js";
import type { MempoolLedgerCacheService } from "../../src/services/mempool-ledger-cache.js";
import { testDatabaseName } from "../test-env.js";
import { makeRecordedHistoryTransport } from "./history-source-owner-emulator.js";
import { loadRealMidgardContractsForTest } from "./real-midgard-contracts.js";

const hash = (text: string) => createHash("sha256").update(text).digest("hex");

/** Pool/queue model only. The retained empty paired snapshot and source binding
 * are explicit model inputs, not applied initialization or a deployment claim.
 * The real owner still authenticates each low-level socket, reintersects and
 * recaptures that source, reconciles SQL, reloads the supplied cache and gates
 * actual producer lifetimes. No new SQL pools or alternate runProducer exist.
 */
export const makePoolIsolationHistoryOwner = (input: {
  readonly globals: Globals;
  readonly cache: MempoolLedgerCacheService;
}) =>
  Effect.gen(function* () {
    const batchSql = yield* BatchSql;
    const scoped = Effect.gen(function* () {
      const [database] = yield* batchSql<{
        name: string;
      }>`SELECT current_database() AS name`;
      if (database?.name !== testDatabaseName())
        throw new Error(
          "Pool history fixture requires its isolated worker database",
        );
      if (Option.isSome(yield* Authority.retrieve))
        throw new Error("Pool history fixture cannot adopt an existing owner");
      if ((yield* Ref.get(input.globals.EVENT_HISTORY_OWNER)) !== undefined)
        throw new Error(
          "Pool history fixture cannot replace another runtime owner",
        );

      const contracts = yield* Effect.tryPromise(() =>
        loadRealMidgardContractsForTest({
          txHash: hash("pool-isolation-modeled-contract-nonce"),
          outputIndex: 0,
        }),
      );
      const histories = SDK.requireEventHistoryContracts(contracts);
      const hubDatumCbor = Data.to(
        yield* SDK.makeHubOracleDatum(contracts),
        SDK.HubOracleDatum,
      );
      const hubUnit = toUnit(
        contracts.hubOracle.policyId,
        SDK.HUB_ORACLE_ASSET_NAME,
      );
      const deployments = {
        deposit: SDK.eventHistoryDeploymentFromContracts(histories.deposit),
        withdrawal: SDK.eventHistoryDeploymentFromContracts(
          histories.withdrawal,
        ),
      };
      const root = Data.to(
        {
          position: "Root",
          next: null,
          protected_until: 0n,
          payload: "RootContent",
        },
        SDK.EventHistoryNode,
      );
      const outputs: LedgerSnapshotOutput[] = Object.values(deployments).map(
        (deployment, outputIndex) => ({
          txHash: hash("pool-isolation-modeled-root-output"),
          outputIndex,
          address: deployment.address,
          assets: { lovelace: 3_000_000n, [deployment.policyId]: 1n },
          datum: root,
          hasReferenceScript: false,
        }),
      );
      outputs.push({
        txHash: hash("pool-isolation-modeled-hub-output"),
        outputIndex: 0,
        address: contracts.hubOracle.spendingScriptAddress,
        assets: { lovelace: 3_000_000n, [hubUnit]: 1n },
        datum: hubDatumCbor,
        hasReferenceScript: false,
      });
      const transport = makeRecordedHistoryTransport({
        publications: new Map(),
        batches: [
          { observations: [], observedSlot: 100, observedHeight: 1, outputs },
        ],
        genesis: {
          scope: "explicit pool-isolation source model; no applied ledger",
          initializationTxHash: hash("pool-isolation-modeled-initialization"),
        },
      });
      const genesis = {
        scope: "explicit pool-isolation source model; no applied ledger",
        initializationTxHash: hash("pool-isolation-modeled-initialization"),
      };
      const facts: Omit<EventHistorySourceBinding, "digest"> = {
        manifestId: hash("pool-isolation-modeled-manifest"),
        network: "Preprod" as const,
        endpointIdentitySha256: ogmiosEndpointIdentitySha256(
          transport.options.ogmiosUrl,
        ),
        genesisAlgorithm: HISTORY_GENESIS_DIGEST_ALGORITHM,
        genesisSha256: eventHistoryGenesisLosslessSha256(genesis),
        hubAddress: contracts.hubOracle.spendingScriptAddress,
        hubUnit,
        hubDatumCbor,
        deployments,
      };
      const binding: EventHistorySourceBinding = {
        ...facts,
        digest: hash(
          eventHistoryCanonicalJson({
            domain: "midgard-node-history-source-v1",
            ...facts,
          }),
        ),
      };
      const point = transport.points[0]!.point;
      const capture = yield* decodeBoundEventHistoryLedgerSnapshot(
        {
          point: { id: point.id, slot: point.slot },
          addresses: [
            binding.hubAddress,
            ...Object.values(deployments).flatMap((deployment) => [
              deployment.address,
              deployment.retentionAddress,
            ]),
          ],
          outputs,
        },
        binding,
      );
      const existing =
        yield* batchSql`SELECT 1 FROM event_history_cursor WHERE binding_digest = ${Buffer.from(binding.digest, "hex")}`;
      if (existing.length !== 0)
        throw new Error(
          "Pool history fixture cannot replace an existing journal binding",
        );
      const ownerToken = randomUUID();
      const seedToken = yield* Authority.acquire({
        deploymentIdentity: binding.manifestId,
        ownerToken,
        leaseDurationMs: 60_000,
      });
      const lifecycle: { owner: EventHistoryOwner | undefined } = {
        owner: undefined,
      };
      let cleaned = false;
      const close = Effect.uninterruptible(
        Effect.gen(function* () {
          if (cleaned) return;
          const owner = lifecycle.owner;
          if (owner !== undefined) yield* owner.close;
          else yield* Authority.release(seedToken);
          yield* Ref.update(input.globals.EVENT_HISTORY_OWNER, (current) =>
            current === owner ? undefined : current,
          );
          transport.close();
          yield* batchSql.withTransaction(
            Effect.gen(function* () {
              const [current] = yield* batchSql<{
                generation: string;
                state: string;
              }>`SELECT generation::text, state FROM event_history_authority WHERE singleton = true AND deployment_identity = ${Buffer.from(binding.manifestId, "hex")} AND owner_token = ${ownerToken}::uuid FOR UPDATE`;
              if (current === undefined || current.state !== "suspended")
                throw new Error(
                  "Pool history cleanup requires its own released authority",
                );
              const digest = Buffer.from(binding.digest, "hex");
              // Exact helper binding only; no CASCADE and no unrelated event rows.
              yield* batchSql`DELETE FROM event_history_l2_ledger_receipts WHERE binding_digest = ${digest}`;
              yield* batchSql`DELETE FROM event_history_replay_receipts WHERE binding_digest = ${digest}`;
              yield* batchSql`DELETE FROM event_history_block_applications WHERE binding_digest = ${digest}`;
              yield* batchSql`DELETE FROM event_history_live_outputs WHERE binding_digest = ${digest}`;
              yield* batchSql`DELETE FROM event_history_incarnations WHERE binding_digest = ${digest}`;
              yield* batchSql`DELETE FROM event_history_cursor WHERE binding_digest = ${digest}`;
              const deleted =
                yield* batchSql`DELETE FROM event_history_authority WHERE singleton = true AND deployment_identity = ${Buffer.from(binding.manifestId, "hex")} AND owner_token = ${ownerToken}::uuid AND generation = ${current.generation}::bigint AND state = 'suspended' RETURNING singleton`;
              if (deleted.length !== 1)
                throw new Error("Pool history cleanup authority changed");
            }),
          );
          cleaned = true;
        }),
      );
      yield* Effect.addFinalizer(() => close.pipe(Effect.orDie));
      const receipt =
        "Explicit modeled empty paired history bootstrap for isolated pool semantics; no applied L1 claim";
      yield* Authority.withRecovery(
        seedToken,
        Journal.seed({
          binding,
          capture,
          height: point.height,
          originReceipt: receipt,
          originReceiptDigest: hash(receipt),
          incarnations: [],
        }),
      );
      yield* Authority.release(seedToken);
      const owner = yield* makeEventHistoryOwner({
        binding,
        histories,
        ownerToken,
        cache: input.cache,
        slotToUnixTime: (slot) => slot * 1000,
        transport: transport.options,
        heartbeatIntervalMs: 1000,
        leaseDurationMs: 60_000,
        rollbackHorizon: 2160,
        retainedPointLimit: 16,
        maximumReceiptBytes: 16 * 1024 * 1024,
        reconcile: (change) => materializeCanonicalHistory(change, "Preprod"),
      });
      lifecycle.owner = owner;
      yield* owner.awaitReady;
      yield* Ref.set(input.globals.EVENT_HISTORY_OWNER, owner);
      return {
        owner,
        close: close.pipe(
          Effect.provideService(SqlClient.SqlClient, batchSql),
          Effect.orDie,
        ),
      };
    });
    return yield* scoped.pipe(
      Effect.provideService(SqlClient.SqlClient, batchSql),
    );
  });
