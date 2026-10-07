import { heightAtDepth } from "@al-ft/midgard-l1-follower/heads";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Data, type UTxO } from "@lucid-evolution/lucid";
import { Cause, Deferred, Effect, Ref, Schedule } from "effect";

import * as Journal from "../database/eventHistoryJournal.js";
import {
  makeEventHistorySourceBinding,
  readBoundRecoveryLedgerSnapshot,
} from "../l1-event-history-source.js";
import {
  type AcquiredLedgerSnapshot,
  LEDGER_SCAN_TIMEOUT_MS,
} from "../l1-ledger-snapshot.js";
import {
  ContractDeploymentIdentity,
  Globals,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "../services/index.js";
import {
  clearLivenessIncident,
  HaltSource,
  raiseLivenessIncident,
} from "../services/liveness-halt.js";
import { resolveOwnOperatorKeyHashProgram } from "../transactions/operators/takeover.js";

export type OperatorMembershipState =
  | "unknown"
  | "active"
  | "awaiting_activation"
  | "removal_pending"
  | "removed";
export type OperatorMembership = "active" | "registered" | "retired" | "absent";

/** Exact protocol assets exclude permissionless donations from bounded reads. */
export const relevantMembershipAsset = (
  assetName: string,
  rootAssetName: string,
  nodePrefix?: string,
): boolean =>
  assetName === rootAssetName ||
  (nodePrefix !== undefined && assetName.startsWith(nodePrefix));

/** A complete acquired ledger image, rather than filtered indexer absence,
 * proves directory membership. Validate every authenticated node and linkage. */
export const decodeOperatorMembership = (
  ledger: AcquiredLedgerSnapshot,
  contracts: Pick<
    SDK.OperatorDirectoryValidators,
    "registeredOperators" | "activeOperators" | "retiredOperators"
  >,
  ownKey: string,
): Effect.Effect<OperatorMembership, Error> =>
  Effect.gen(function* () {
    const memberships: OperatorMembership[] = [];
    for (const [name, validator, prefix, rootAssetName, schema] of [
      [
        "active",
        contracts.activeOperators,
        SDK.ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX,
        SDK.ACTIVE_OPERATORS_ROOT_ASSET_NAME,
        SDK.ActiveOperatorDatum,
      ],
      [
        "registered",
        contracts.registeredOperators,
        SDK.REGISTERED_OPERATOR_NODE_ASSET_NAME_PREFIX,
        SDK.REGISTERED_OPERATORS_ROOT_ASSET_NAME,
        SDK.RegisteredOperatorDatum,
      ],
      [
        "retired",
        contracts.retiredOperators,
        SDK.RETIRED_OPERATOR_NODE_ASSET_NAME_PREFIX,
        SDK.RETIRED_OPERATORS_ROOT_ASSET_NAME,
        SDK.RetiredOperatorDatum,
      ],
    ] as const) {
      const nodes = new Map<string, SDK.LinkedListNodeView>();
      for (const output of ledger.outputs.filter(
        (entry) => entry.address === validator.spendingScriptAddress,
      )) {
        const units = Object.keys(output.assets).filter((unit) =>
          unit.startsWith(validator.policyId),
        );
        if (units.length === 0) continue;
        if (output.hasReferenceScript || output.datumHash !== undefined)
          throw new Error(`Malformed ${name} directory witness`);
        const unit = units[0]!;
        const assetName = unit.slice(56);
        if (
          units.length !== 1 ||
          output.assets[unit] !== 1n ||
          Object.keys(output.assets).filter((key) => key !== "lovelace")
            .length !== 1 ||
          (assetName !== rootAssetName && !assetName.startsWith(prefix))
        )
          throw new Error(`Malformed ${name} directory authentication asset`);
        const node = yield* SDK.getLinkedListNodeViewFromUTxO(
          output as UTxO,
        ).pipe(
          Effect.mapError(
            (cause) =>
              new Error(`Malformed ${name} directory datum`, { cause }),
          ),
        );
        const key = node.key === "Empty" ? "root" : node.key.Key.key;
        if (
          nodes.has(key) ||
          (node.key === "Empty") !== (assetName === rootAssetName)
        )
          throw new Error(`Duplicate or mismatched ${name} directory node`);
        if (node.key !== "Empty") {
          if (
            !new RegExp(
              `^[0-9a-f]{${name === "registered" ? 16 : 56}}$`,
              "u",
            ).test(key)
          )
            throw new Error(`Malformed ${name} directory key`);
          const datum = yield* Effect.try(() =>
            Data.castFrom(node.data as never, schema),
          );
          if (
            (name === "registered"
              ? (datum as SDK.RegisteredOperatorDatum).operator
              : key) === ownKey
          )
            memberships.push(name);
        }
        nodes.set(key, node);
      }
      const root = nodes.get("root");
      if (root === undefined) throw new Error(`Missing ${name} directory root`);
      let next = root.next;
      const visited = new Set(["root"]);
      while (next !== "Empty") {
        const key = next.Key.key;
        const node = nodes.get(key);
        if (node === undefined || visited.has(key))
          throw new Error(`Incomplete or cyclic ${name} directory`);
        visited.add(key);
        next = node.next;
      }
      if (visited.size !== nodes.size)
        throw new Error(`Disconnected ${name} directory node`);
    }
    if (memberships.length > 1)
      throw new Error("Operator occupies multiple directory lists");
    return memberships[0] ?? "absent";
  });

/** A registered key has not activated yet. An absent key is removal only
 * after authenticated past activity; retirement is direct removal evidence. */
export const classifyOperatorMembership = (
  membership: OperatorMembership,
  previouslyActive: boolean,
): OperatorMembershipState =>
  membership === "active"
    ? "active"
    : membership === "retired" || previouslyActive
      ? "removed"
      : membership === "registered"
        ? "awaiting_activation"
        : "unknown";

/** Records outside retained ancestry are unknown even when older than anchor. */
export const operatorActivityIsRetained = (
  recorded:
    | { readonly active_block_hash: Buffer; readonly active_block_slot: string }
    | undefined,
  binding: Parameters<typeof Journal.retains>[0],
  checkpoint: Parameters<typeof Journal.retains>[1],
) =>
  recorded === undefined
    ? Effect.succeed(false)
    : Journal.retains(binding, checkpoint, {
        id: recorded.active_block_hash.toString("hex"),
        slot: Number(recorded.active_block_slot),
      });

/** The liveness reasons authenticated removal raises under
 * `HaltSource.operatorMembership`, which holds every operator duty (see
 * `FIBER_HALT_SOURCES`). No other membership state holds duties. */
export const OPERATOR_MEMBERSHIP_HALT_REASONS = {
  removal_pending: "operator_removal_pending_finality",
  removed: "operator_removed_from_active_set",
} as const satisfies Partial<Record<OperatorMembershipState, string>>;

/** Removal is published only after finality; active/pending reflect the
 * authenticated head. Current absence after authenticated activity holds the
 * operator duties from the moment it is seen; confirmed removal keeps them
 * held for good and signals shutdown. Only an authenticated `active` or
 * `awaiting_activation` lifts a pending removal: `unknown` (say, once the
 * retained proof of past activity ages out) never does. */
export const publishOperatorMembership = (
  state: OperatorMembershipState,
): Effect.Effect<void, never, Globals> =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    const current = yield* Ref.get(globals.OPERATOR_MEMBERSHIP);
    if (
      current === "removed" ||
      (current === "removal_pending" && state === "unknown")
    )
      return;
    yield* Ref.set(globals.OPERATOR_MEMBERSHIP, state);
    if (state === "removed" || state === "removal_pending") {
      yield* raiseLivenessIncident(
        globals,
        HaltSource.operatorMembership,
        OPERATOR_MEMBERSHIP_HALT_REASONS[state],
        state === "removed"
          ? "the operator left the active set at a finalized point; duties stay held and the node shuts down"
          : "the operator is absent from the active set at the authenticated head; duties are held until the finalized point decides",
      );
    } else {
      yield* clearLivenessIncident(globals, HaltSource.operatorMembership);
    }
    if (state === "removed") {
      yield* Effect.logError(
        "Operator removed from the active set; disabling duties and shutting down without re-registration.",
      );
      yield* Deferred.succeed(globals.OPERATOR_REMOVAL_SHUTDOWN, undefined);
    }
  });

/** Confirm at a retained canonical ancestor with the deployment's finality
 * depth. The history generation guard fences the head check and every
 * publication; the finalized point is re-proved canonical before its scan's
 * verdict is published. */
export const operatorMembershipTick = Effect.gen(function* () {
  const globals = yield* Globals;
  if ((yield* Ref.get(globals.OPERATOR_MEMBERSHIP)) === "removed") return;
  const config = yield* NodeConfig;
  const contracts = yield* MidgardContracts;
  const identity = yield* ContractDeploymentIdentity;
  const lucid = yield* Lucid;
  const sql = yield* SqlClient.SqlClient;
  const owner = yield* Ref.get(globals.EVENT_HISTORY_OWNER);
  if (
    owner === undefined ||
    identity.manifest === undefined ||
    identity.manifestId === undefined
  )
    throw new Error("Authenticated membership authority unavailable");
  const manifestId = identity.manifestId;
  const key = yield* resolveOwnOperatorKeyHashProgram(
    lucid.operatorMainAddress,
  );
  const binding = yield* makeEventHistorySourceBinding({
    contracts,
    identity,
    network: config.NETWORK,
    expectedGenesisLosslessSha256: config.L1_HISTORY_GENESIS_LOSSLESS_SHA256,
  });
  const finalized = yield* owner.runProducer((_token, guard) =>
    Effect.gen(function* () {
      const checkpoint = yield* Journal.loadCurrent(binding);
      if (checkpoint === null)
        throw new Error("Membership history checkpoint unavailable");
      // The block one deeper than cd, as before the heads module (N6 owns
      // the move to a level).
      const targetHeight = heightAtDepth(
        checkpoint.head.height,
        identity.manifest!.l1Finality.confirmationDepth + 1,
      );

      const previous = yield* sql<{
        active_block_hash: Buffer;
        active_block_slot: string;
        active_block_height: string;
      }>`SELECT active_block_hash, active_block_slot::text, active_block_height::text FROM operator_membership_observations WHERE manifest_id = ${Buffer.from(manifestId, "hex")} AND operator_key = ${Buffer.from(key, "hex")}`;
      const recorded = previous[0];
      // Only retained canonical ancestry proves past activity. An orphan marker
      // can age below the anchor, so age alone never authenticates it.
      const observationCanonical = yield* operatorActivityIsRetained(
        recorded,
        binding,
        checkpoint,
      );
      const addresses = [
        contracts.activeOperators.spendingScriptAddress,
        contracts.registeredOperators.spendingScriptAddress,
        contracts.retiredOperators.spendingScriptAddress,
      ];
      // The indexer supplies candidates only. Acquire every candidate on the
      // genesis-authenticated socket, then follow all authenticated root links.
      // A stale root, missing successor or spent candidate fails closed.
      const scopes = [
        [
          binding.hubAddress,
          binding.hubUnit.slice(0, 56),
          binding.hubUnit.slice(56),
          undefined,
        ],
        [
          addresses[0]!,
          contracts.activeOperators.policyId,
          SDK.ACTIVE_OPERATORS_ROOT_ASSET_NAME,
          SDK.ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX,
        ],
        [
          addresses[1]!,
          contracts.registeredOperators.policyId,
          SDK.REGISTERED_OPERATORS_ROOT_ASSET_NAME,
          SDK.REGISTERED_OPERATOR_NODE_ASSET_NAME_PREFIX,
        ],
        [
          addresses[2]!,
          contracts.retiredOperators.policyId,
          SDK.RETIRED_OPERATORS_ROOT_ASSET_NAME,
          SDK.RETIRED_OPERATOR_NODE_ASSET_NAME_PREFIX,
        ],
      ] as const;
      const candidates = yield* Effect.all(
        scopes.map(([address, policyId, rootAssetName, nodePrefix]) =>
          SDK.utxosAtByNFTPolicyId(lucid.api, address, policyId).pipe(
            Effect.map((entries) =>
              entries
                .filter(({ assetName }) =>
                  relevantMembershipAsset(assetName, rootAssetName, nodePrefix),
                )
                .map(({ utxo }) => utxo),
            ),
          ),
        ),
        { concurrency: "unbounded" },
      ).pipe(
        Effect.timeoutFail({
          duration: config.L1_PROVIDER_PREFLIGHT_TIMEOUT_MS,
          onTimeout: () => new Error("Membership candidates timed out"),
        }),
      );
      const currentCapture = yield* Effect.tryPromise({
        try: (signal) =>
          readBoundRecoveryLedgerSnapshot({
            binding,
            at: checkpoint.head,
            ogmiosUrl: config.L1_OGMIOS_KEY,
            addresses,
            outputReferences: candidates
              .flat()
              .map(({ txHash, outputIndex }) => ({ txHash, outputIndex })),
            timeoutMs: config.L1_PROVIDER_PREFLIGHT_TIMEOUT_MS,
            signal,
          }),
        catch: (cause) => cause,
      });
      const currentMembership = yield* decodeOperatorMembership(
        currentCapture.ledger,
        contracts,
        key,
      );
      yield* guard;
      if (currentMembership === "active") {
        // Refresh while active so ordinary restarts retain a recent proof.
        yield* sql`INSERT INTO operator_membership_observations (manifest_id, operator_key, active_block_hash, active_block_slot, active_block_height) VALUES (${Buffer.from(manifestId, "hex")}, ${Buffer.from(key, "hex")}, ${Buffer.from(checkpoint.head.id, "hex")}, ${checkpoint.head.slot}, ${checkpoint.head.height}) ON CONFLICT (manifest_id, operator_key) DO UPDATE SET active_block_hash = EXCLUDED.active_block_hash, active_block_slot = EXCLUDED.active_block_slot, active_block_height = EXCLUDED.active_block_height`;
        yield* guard;
        yield* Ref.set(globals.OPERATOR_MEMBERSHIP_MISSING_HEIGHT, undefined);
        yield* publishOperatorMembership("active");
        return undefined;
      }
      // A pending removal was raised on retained proof of that activity. Once
      // the record ages below the anchor it is final, not orphaned (an orphan
      // inside the retained window still reads as unproven), so a pending
      // removal of an absent key keeps it as proof and goes on to the
      // finalized point instead of waiting for evidence that cannot return.
      const activityProven =
        observationCanonical ||
        (currentMembership === "absent" &&
          (yield* Ref.get(globals.OPERATOR_MEMBERSHIP)) === "removal_pending" &&
          recorded !== undefined &&
          Number(recorded.active_block_slot) < checkpoint.anchor.slot);
      if (!activityProven && currentMembership !== "retired") {
        yield* Ref.set(globals.OPERATOR_MEMBERSHIP_MISSING_HEIGHT, undefined);
        yield* publishOperatorMembership(
          currentMembership === "registered"
            ? "awaiting_activation"
            : "unknown",
        );
        return undefined;
      }
      // Pause duties on authoritative current absence. Wait until this absence
      // point itself is finalized before the expensive historical capture.
      const missingHeight =
        (yield* Ref.get(globals.OPERATOR_MEMBERSHIP_MISSING_HEIGHT)) ??
        checkpoint.head.height;
      yield* Ref.set(globals.OPERATOR_MEMBERSHIP_MISSING_HEIGHT, missingHeight);
      yield* publishOperatorMembership("removal_pending");
      if (targetHeight < missingHeight) return undefined;
      const points =
        targetHeight === checkpoint.anchor.height
          ? [checkpoint.anchor]
          : yield* sql<{
              id: string;
              slot: string;
            }>`SELECT encode(block_hash, 'hex') AS id, block_slot::text AS slot FROM event_history_block_applications WHERE binding_digest = ${Buffer.from(binding.digest, "hex")} AND canonical AND block_height = ${targetHeight}`;
      if (points.length !== 1)
        throw new Error("Finalized membership point is not retained");
      return {
        at: { id: points[0]!.id, slot: Number(points[0]!.slot) },
        previouslyActive:
          activityProven &&
          Number(recorded!.active_block_height) <= targetHeight,
      };
    }),
  );
  if (finalized === undefined) return;
  // The full scan runs outside the producer section, so history recovery
  // never drains behind it. It reads the ledger at one exact block; that block
  // still being canonical afterwards keeps the recorded activity behind it
  // canonical too, so the scan's verdict stands.
  const capture = yield* Effect.tryPromise({
    try: (signal) =>
      readBoundRecoveryLedgerSnapshot({
        binding,
        at: finalized.at,
        ogmiosUrl: config.L1_OGMIOS_KEY,
        addresses: [
          contracts.activeOperators.spendingScriptAddress,
          contracts.registeredOperators.spendingScriptAddress,
          contracts.retiredOperators.spendingScriptAddress,
        ],
        timeoutMs: LEDGER_SCAN_TIMEOUT_MS,
        signal,
      }),
    catch: (cause) => cause,
  });
  const membership = yield* decodeOperatorMembership(
    capture.ledger,
    contracts,
    key,
  );
  yield* owner.runProducer((_token, guard) =>
    Effect.gen(function* () {
      const checkpoint = yield* Journal.loadCurrent(binding);
      if (
        checkpoint === null ||
        !(yield* Journal.retains(binding, checkpoint, finalized.at))
      )
        throw new Error("Finalized membership point is no longer retained");
      yield* guard;
      yield* publishOperatorMembership(
        classifyOperatorMembership(membership, finalized.previouslyActive) ===
          "removed"
          ? "removed"
          : "removal_pending",
      );
    }),
  );
}).pipe(
  Effect.catchAllCause((cause) =>
    Cause.isInterrupted(cause)
      ? Effect.interrupt
      : // Unavailable, stale, rolled-back or malformed evidence is neither
        // removal nor its absence: the last authenticated state stands, and
        // the next tick retries.
        Effect.logWarning(
          "Operator membership could not be authenticated; keeping the last authenticated state",
          cause,
        ),
  ),
);

export const operatorMembershipFiber = operatorMembershipTick.pipe(
  Effect.repeat(Schedule.spaced("60 seconds")),
);

/** Runs `runtime` until removal is confirmed, then keeps the process up
 * briefly so readiness reports the removal before it ends. A removed operator
 * never re-registers. Removal itself holds the duties through
 * `HaltSource.operatorMembership`; this only ends the process. */
export const untilOperatorRemoved = <A, E, R>(
  runtime: Effect.Effect<A, E, R>,
): Effect.Effect<void, E, R | Globals> =>
  Effect.flatMap(Globals, (globals) =>
    Effect.raceFirst(
      Effect.asVoid(runtime),
      Deferred.await(globals.OPERATOR_REMOVAL_SHUTDOWN).pipe(
        Effect.zipRight(Effect.sleep("1 second")),
      ),
    ),
  );
