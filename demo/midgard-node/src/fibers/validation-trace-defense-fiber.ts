/**
 * The defending operator's fiber: every tick it surveys the validation-trace
 * cases on our blocks and plays what they call for, earliest deadline first
 * (see `validation-trace-defense/tick.ts`). It plays with its own Lucid,
 * the operator wallet with its defense pocket selected, so no other fiber's
 * wallet switch moves under it. It never fails: a failed tick is recorded in
 * the node's health and retried at the next one.
 */
import { MIDGARD_VALIDATION_DISPUTE_MAX_VALIDITY_RANGE_MS } from "@al-ft/midgard-core";
import * as LE from "@lucid-evolution/lucid";
import { Effect, Schedule } from "effect";

import {
  ContractDeploymentIdentity,
  Database,
  deploymentL1SlotConfig,
  Globals,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "../services/index.js";
import type { IntentJournal } from "../services/intent-journal.js";
import { defensePocketAddress } from "../services/intent-journal.tracked-set.js";
import {
  fetchReferenceScriptUtxosProgram,
  referenceScriptByName,
} from "../transactions/reference-scripts.fetch-reference-script-utxos-program.js";
import { selectNodeWallet } from "../transactions/utils.wallet-view.js";
import type {
  ValidationTraceDefensePort,
  ValidationTraceDefenseReferences,
} from "../validation-trace-defense/deployment.js";
import {
  validationTraceDefenseStep,
  validationTraceDefenseTick,
} from "../validation-trace-defense/tick.js";

/** The defender's tick: a quarter of the longest move validity range. */
export const VALIDATION_TRACE_DEFENSE_TICK_MS = Math.floor(
  MIDGARD_VALIDATION_DISPUTE_MAX_VALIDITY_RANGE_MS / 4,
);

/** How often a held Defend looks for its landing. */
const POLL_MS = 1_000;

/** The defender's port over the node's services. */
const makePort = Effect.gen(function* () {
  const lucidService = yield* Lucid;
  const nodeConfig = yield* NodeConfig;
  const contracts = yield* MidgardContracts;
  const identity = yield* ContractDeploymentIdentity;
  const shared = lucidService.api.config();
  const lucid = yield* Effect.tryPromise({
    try: () =>
      LE.Lucid(shared.provider, nodeConfig.NETWORK, {
        ...(shared.slotConfig === undefined
          ? {}
          : { slotConfig: shared.slotConfig }),
      }),
    catch: (cause) =>
      new Error("validation-trace defense: its Lucid could not be built", {
        cause,
      }),
  });
  selectNodeWallet(lucid, nodeConfig.L1_OPERATOR_SEED_PHRASE, {
    defensePocket: true,
  });
  const ourVkh = LE.getAddressDetails(lucidService.operatorMainAddress)
    .paymentCredential?.hash;
  if (ourVkh === undefined)
    return yield* Effect.fail(
      new Error("the operator address has no payment credential"),
    );
  const dispute = contracts.fraudProofContracts.validationTraceDispute;
  const roles = [
    ["case", "V1 validation-trace case", dispute.case.spendingScript],
    ["game", "V1 validation-trace game", dispute.game.spendingScript],
    ["timeout", "V1 validation-trace timeout", dispute.timeout.spendingScript],
    [
      "boundary",
      "V1 validation-trace boundary",
      dispute.boundary.spendingScript,
    ],
    [
      "threadExpiry",
      "V1 validation-trace thread expiry",
      dispute.threadExpiry.withdrawalScript,
    ],
    [
      "activeOperators",
      "active-operators spending",
      contracts.activeOperators.spendingScript,
    ],
  ] as const;
  const references = fetchReferenceScriptUtxosProgram(
    lucid,
    lucidService.referenceScriptsAddress,
    roles.map(([, name, script]) => ({ name, script })),
    contracts.referenceScriptAuth,
  ).pipe(
    Effect.flatMap((resolved) =>
      Effect.try(
        () =>
          Object.fromEntries(
            roles.map(([key, name]) => [
              key,
              referenceScriptByName(resolved, name),
            ]),
          ) as ValidationTraceDefenseReferences,
      ),
    ),
    Effect.mapError(
      (cause) =>
        new Error("validation-trace defense: references unresolved", {
          cause,
        }),
    ),
  );
  const port: ValidationTraceDefensePort = {
    lucid,
    network: nodeConfig.NETWORK,
    dispute,
    activeOperatorsPolicyId: contracts.activeOperators.policyId,
    hubOraclePolicyId: contracts.hubOracle.policyId,
    ourVkh,
    baseAddress: lucidService.operatorMainAddress,
    pocketAddress: defensePocketAddress(nodeConfig),
    l1SlotConfig: deploymentL1SlotConfig(identity),
    references,
    lease: {
      ttlMs: nodeConfig.STATE_QUEUE_MUTATION_LEASE_TTL_MS,
      renewIntervalMs: nodeConfig.STATE_QUEUE_MUTATION_LEASE_RENEW_INTERVAL_MS,
    },
    tickMs: VALIDATION_TRACE_DEFENSE_TICK_MS,
    pollMs: POLL_MS,
    cache: new Map(),
  };
  return port;
});

/**
 * The defender, ticking on `schedule`. Its port is built at its first tick
 * and kept; a tick before it can be built fails like any other.
 */
export const validationTraceDefenseFiber = (
  schedule: Schedule.Schedule<number>,
): Effect.Effect<
  void,
  never,
  | Lucid
  | MidgardContracts
  | ContractDeploymentIdentity
  | Database
  | Globals
  | NodeConfig
  | IntentJournal
> =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    yield* Effect.logInfo("Validation-trace defense fiber started.");
    let port: ValidationTraceDefensePort | undefined;
    const portOnce = Effect.suspend(() =>
      port === undefined
        ? Effect.tap(makePort, (made) =>
            Effect.sync(() => {
              port = made;
            }),
          )
        : Effect.succeed(port),
    );
    yield* Effect.repeat(
      validationTraceDefenseStep(
        Effect.flatMap(portOnce, validationTraceDefenseTick).pipe(
          Effect.withSpan("validation-trace-defense-fiber"),
        ),
        globals.VALIDATION_TRACE_DEFENSE_HEALTH,
      ),
      schedule,
    );
  });
