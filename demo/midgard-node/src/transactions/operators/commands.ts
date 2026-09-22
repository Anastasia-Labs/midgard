/**
 * Service-level entry points for the operator lifecycle CLI verbs and the
 * `/operator/status` endpoint. Each command selects the operator wallet, reads
 * the deployment economics, and delegates to the pure-ish programs in this
 * directory. Local refusals and funding shortfalls surface as
 * `OperatorCommandRefusal` so the CLI can print one line and exit non-zero
 * instead of dumping a fiber trace.
 */
import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import { Lucid, MidgardContracts, NodeConfig } from "../../services/index.js";
import {
  activateOperatorProgram,
  activateRegisteredOperatorProgram,
  deregisterOperatorProgram,
  OperatorRegistrationRefusal,
  registerOperatorProgram,
} from "../register-active-operator.js";
import {
  configuredOperatorEconomicsProgram,
  OperatorExitRefusal,
  recoverOperatorBondProgram,
  retireOperatorProgram,
  slashDuplicateOperatorProgram,
} from "./exit.js";
import { OperatorFundingShortfall } from "./funding-preflight.js";
import { operatorStatusProgram, type OperatorStatusReport } from "./status.js";
import {
  planTakeoverProgram,
  resolveOwnOperatorKeyHashProgram,
  submitInactivityStrikeProgram,
} from "./takeover.js";

/**
 * A refusal the CLI prints verbatim. Wraps the local refusals of the
 * underlying programs so callers match one class.
 */
export class OperatorCommandRefusal extends Error {
  override readonly name = "OperatorCommandRefusal";
}

export const isOperatorCommandRefusal = (
  error: unknown,
): error is
  | OperatorCommandRefusal
  | OperatorExitRefusal
  | OperatorRegistrationRefusal
  | OperatorFundingShortfall =>
  error instanceof OperatorCommandRefusal ||
  error instanceof OperatorExitRefusal ||
  error instanceof OperatorRegistrationRefusal ||
  error instanceof OperatorFundingShortfall;

const OPERATOR_KEY_HASH_HEX = /^[0-9a-f]{56}$/;

export const parseOperatorKeyHashOption = (value: string): string => {
  const normalized = value.trim().toLowerCase();
  if (!OPERATOR_KEY_HASH_HEX.test(normalized)) {
    throw new Error(
      "--operator-key-hash must be a 28-byte payment key hash in hex (56 hex characters)",
    );
  }
  return normalized;
};

/**
 * What every verb starts from: the operator wallet selected, the deployment
 * economics, and that wallet's operator key.
 */
const operatorCommandContext = Effect.gen(function* () {
  const lucid = yield* Lucid;
  const contracts = yield* MidgardContracts;
  const economics = yield* configuredOperatorEconomicsProgram;
  yield* lucid.switchToOperatorsMainWallet;
  const ownOperatorKeyHash = yield* resolveOwnOperatorKeyHashProgram(lucid.api);
  return { lucid, contracts, economics, ownOperatorKeyHash };
});

// ---------------------------------------------------------------------------
// Status
// ---------------------------------------------------------------------------

export const operatorStatusCommand = (input: {
  readonly operatorKeyHash?: string;
}): Effect.Effect<
  OperatorStatusReport,
  Error,
  Lucid | MidgardContracts | NodeConfig
> =>
  Effect.gen(function* () {
    const nodeConfig = yield* NodeConfig;
    const { lucid, contracts, ownOperatorKeyHash } =
      yield* operatorCommandContext;
    return yield* operatorStatusProgram(lucid.api, contracts, {
      operatorKeyHash: input.operatorKeyHash ?? ownOperatorKeyHash,
      watchdog: {
        enabled: nodeConfig.OPERATOR_WATCHDOG_ENABLED,
        patienceMs: nodeConfig.OPERATOR_WATCHDOG_PATIENCE_MS,
      },
    });
  });

// ---------------------------------------------------------------------------
// Entry: register / activate / deregister
// ---------------------------------------------------------------------------

export const registerOperatorCommand = Effect.gen(function* () {
  const { lucid, contracts, economics, ownOperatorKeyHash } =
    yield* operatorCommandContext;
  const result = yield* registerOperatorProgram(
    lucid.api,
    contracts,
    economics.requiredBondLovelace,
    lucid.referenceScriptsApi,
    lucid.referenceScriptsAddress,
  );
  return { ...result, operatorKeyHash: ownOperatorKeyHash };
});

export const activateOperatorCommand = (input: {
  readonly operatorKeyHash?: string;
}) =>
  Effect.gen(function* () {
    const { lucid, contracts, economics, ownOperatorKeyHash } =
      yield* operatorCommandContext;
    const operatorKeyHash = input.operatorKeyHash ?? ownOperatorKeyHash;
    const result =
      operatorKeyHash === ownOperatorKeyHash
        ? yield* activateOperatorProgram(
            lucid.api,
            contracts,
            economics.requiredBondLovelace,
            lucid.referenceScriptsApi,
            lucid.referenceScriptsAddress,
          )
        : yield* activateRegisteredOperatorProgram(
            lucid.api,
            contracts,
            economics.requiredBondLovelace,
            operatorKeyHash,
            lucid.referenceScriptsApi,
            lucid.referenceScriptsAddress,
          );
    return { activateTxHash: result.activateTxHash, operatorKeyHash };
  });

export const deregisterOperatorCommand = Effect.gen(function* () {
  const { lucid, contracts, economics, ownOperatorKeyHash } =
    yield* operatorCommandContext;
  const result = yield* deregisterOperatorProgram(
    lucid.api,
    contracts,
    economics.requiredBondLovelace,
    lucid.referenceScriptsApi,
    lucid.referenceScriptsAddress,
  );
  return { ...result, operatorKeyHash: ownOperatorKeyHash };
});

// ---------------------------------------------------------------------------
// Exit: retire / recover / force-retire / slash duplicate
// ---------------------------------------------------------------------------

export const retireOperatorCommand = Effect.gen(function* () {
  const { lucid, contracts, economics, ownOperatorKeyHash } =
    yield* operatorCommandContext;
  return yield* retireOperatorProgram(
    lucid.api,
    contracts,
    lucid.referenceScriptsAddress,
    { operatorKeyHash: ownOperatorKeyHash, mode: "voluntary", economics },
    { label: "retire-operator" },
  );
});

export const recoverOperatorBondCommand = Effect.gen(function* () {
  const { lucid, contracts, ownOperatorKeyHash } =
    yield* operatorCommandContext;
  return yield* recoverOperatorBondProgram(
    lucid.api,
    contracts,
    lucid.referenceScriptsAddress,
    { operatorKeyHash: ownOperatorKeyHash },
    { label: "recover-operator-bond" },
  );
});

export const forceRetireOperatorCommand = (input: {
  readonly operatorKeyHash: string;
}) =>
  Effect.gen(function* () {
    const { lucid, contracts, economics } = yield* operatorCommandContext;
    return yield* retireOperatorProgram(
      lucid.api,
      contracts,
      lucid.referenceScriptsAddress,
      {
        operatorKeyHash: input.operatorKeyHash,
        mode: "forced-inactivity",
        economics,
      },
      { label: "force-retire-operator" },
    );
  });

export const slashDuplicateOperatorCommand = (input: {
  readonly operatorKeyHash: string;
}) =>
  Effect.gen(function* () {
    const { lucid, contracts, economics } = yield* operatorCommandContext;
    return yield* slashDuplicateOperatorProgram(
      lucid.api,
      contracts,
      lucid.referenceScriptsAddress,
      { operatorKeyHash: input.operatorKeyHash, economics },
      { label: "slash-duplicate-operator" },
    );
  });

// ---------------------------------------------------------------------------
// Stalled-operator recovery by hand
// ---------------------------------------------------------------------------

const describeNotYet = (
  plan: Extract<SDK.InactivityTakeoverPlan, { kind: "not-yet" }>,
): string => {
  const waitMs = plan.thresholdMs - plan.nowMs;
  return `Operator ${plan.currentOperator} holds the shift and cannot be struck yet: the inactivity threshold (${plan.thresholdSource}) is ${new Date(Number(plan.thresholdMs)).toISOString()}, ${Math.max(0, Math.ceil(Number(waitMs) / 1000)).toString()} s from now`;
};

export const strikeInactiveOperatorCommand = Effect.gen(function* () {
  const { lucid, contracts } = yield* operatorCommandContext;
  const planning = yield* planTakeoverProgram(lucid.api, contracts);
  const plan = planning.plan;
  switch (plan.kind) {
    case "no-shift":
      return yield* Effect.fail(
        new OperatorCommandRefusal(
          "The scheduler names no operator; there is no shift to strike",
        ),
      );
    case "not-yet":
      return yield* Effect.fail(
        new OperatorCommandRefusal(describeNotYet(plan)),
      );
    case "blocked":
      return yield* Effect.fail(
        new OperatorCommandRefusal(
          `No strike can be built against ${plan.currentOperator}: ${plan.reason} (${plan.detail})`,
        ),
      );
    case "strikes-exhausted":
      return yield* Effect.fail(
        new OperatorCommandRefusal(
          `Operator ${plan.currentOperator} already carries ${plan.inactivityStrikes.toString()} of ${plan.maxInactivityStrikes.toString()} strikes; run force-retire-operator --operator-key-hash ${plan.currentOperator} instead`,
        ),
      );
    case "ready":
      return yield* submitInactivityStrikeProgram(
        lucid.api,
        contracts,
        lucid.referenceScriptsAddress,
        { ...planning, plan },
        { label: "strike-inactive-operator" },
      );
  }
});
