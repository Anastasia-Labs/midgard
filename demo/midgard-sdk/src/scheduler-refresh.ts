import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  toUnit,
  type TxBuilder,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { normalizeRootIndefiniteArrayEncoding } from "@/cbor.js";
import type { AuthenticatedValidator } from "@/common.js";
import {
  SCHEDULER_ASSET_NAME,
  type SchedulerDatum,
  SchedulerDatum as SchedulerDatumSchema,
  SchedulerError,
  type SchedulerSpendRedeemer,
  SchedulerSpendRedeemer as SchedulerSpendRedeemerSchema,
} from "@/scheduler.js";
import { completeOptionsWithLocalEval } from "@/tx-completion.js";
import {
  requireInputIndex,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
} from "@/tx-context-redeemer.js";

export type SchedulerRefreshNodeWitness = {
  readonly utxo: UTxO;
};

export type SchedulerRefreshWitnessSelection =
  | {
      readonly kind: "Advance";
      readonly activeNode: SchedulerRefreshNodeWitness;
    }
  | {
      readonly kind: "AppointFirst";
      readonly activeNode: SchedulerRefreshNodeWitness;
      readonly registeredWitnessNode: SchedulerRefreshNodeWitness;
    }
  | {
      readonly kind: "Rewind";
      readonly activeNode: SchedulerRefreshNodeWitness;
      readonly activeRootNode: SchedulerRefreshNodeWitness;
      readonly registeredWitnessNode: SchedulerRefreshNodeWitness;
    };

export type SchedulerRefreshLayout =
  | {
      readonly kind: "Advance";
      readonly schedulerInputIndex: bigint;
      readonly schedulerOutputIndex: bigint;
      readonly activeNodeRefInputIndex: bigint;
    }
  | {
      readonly kind: "AppointFirst";
      readonly schedulerInputIndex: bigint;
      readonly schedulerOutputIndex: bigint;
      readonly activeNodeRefInputIndex: bigint;
      readonly registeredWitnessRefInputIndex: bigint;
    }
  | {
      readonly kind: "Rewind";
      readonly schedulerInputIndex: bigint;
      readonly schedulerOutputIndex: bigint;
      readonly activeRootRefInputIndex: bigint;
      readonly activeTailRefInputIndex: bigint;
      readonly registeredWitnessRefInputIndex: bigint;
    };

export type BuildSchedulerRefreshTxConfig = {
  readonly lucid: LucidEvolution;
  readonly scheduler: AuthenticatedValidator;
  readonly operatorKeyHash: string;
  readonly presetWalletInputs?: readonly UTxO[];
  readonly schedulerInput: UTxO;
  readonly refreshedDatum: SchedulerDatum;
  readonly validFrom: bigint;
  readonly validTo: bigint;
  readonly selection: SchedulerRefreshWitnessSelection;
  readonly schedulerSpendingScriptRef?: UTxO;
};

export type SchedulerRefreshTxResult = {
  readonly tx: TxSignBuilder;
  readonly layout: SchedulerRefreshLayout;
  readonly schedulerSpendRedeemerCbor: string;
  readonly refreshedDatumCbor: string;
};

export const encodeSchedulerDatumForChain = (datum: SchedulerDatum): string =>
  // Older deployed scheduler validators on preprod expect the root constructor
  // array in definite form. Lucid emits an indefinite root array here, so
  // normalize before publishing scheduler outputs on-chain.
  normalizeRootIndefiniteArrayEncoding(
    Data.to(datum as never, SchedulerDatumSchema as never),
  );

const schedulerError = (message: string, cause: unknown): SchedulerError =>
  new SchedulerError({ message, cause });

const failScheduler = (
  message: string,
  cause: unknown,
): Effect.Effect<never, SchedulerError> =>
  Effect.fail(schedulerError(message, cause));

const safeTimeNumber = (value: bigint, label: string): number => {
  if (value < 0n || value > BigInt(Number.MAX_SAFE_INTEGER)) {
    throw schedulerError(
      `${label} is outside the safe Lucid time range`,
      value.toString(),
    );
  }
  return Number(value);
};

const referenceInputsForSelection = (
  selection: SchedulerRefreshWitnessSelection,
): readonly UTxO[] => {
  switch (selection.kind) {
    case "Advance":
      return [selection.activeNode.utxo];
    case "AppointFirst":
      return [selection.activeNode.utxo, selection.registeredWitnessNode.utxo];
    case "Rewind":
      return [
        selection.activeNode.utxo,
        selection.activeRootNode.utxo,
        selection.registeredWitnessNode.utxo,
      ];
  }
};

const deriveSchedulerRefreshLayoutFromRedeemerContext = ({
  config,
  ctx,
  refreshedDatumCbor,
  schedulerWitnessUnit,
}: {
  readonly config: BuildSchedulerRefreshTxConfig;
  readonly ctx: Parameters<BuildTxWithRedeemer>[0];
  readonly refreshedDatumCbor: string;
  readonly schedulerWitnessUnit: string;
}): SchedulerRefreshLayout => {
  requireOwnSpendPurpose(ctx, config.schedulerInput, "scheduler refresh");
  const schedulerInputIndex = requireInputIndex(
    ctx,
    config.schedulerInput,
    "scheduler refresh",
  );
  const schedulerOutputIndex = requireUniqueOutputIndex(
    ctx.outputs,
    (output) =>
      output.address === config.scheduler.spendingScriptAddress &&
      output.datum === refreshedDatumCbor &&
      (output.assets[schedulerWitnessUnit] ?? 0n) === 1n,
    "scheduler refresh",
  );

  switch (config.selection.kind) {
    case "Advance":
      return {
        kind: "Advance",
        schedulerInputIndex,
        schedulerOutputIndex,
        activeNodeRefInputIndex: requireReferenceInputIndex(
          ctx,
          config.selection.activeNode.utxo,
          "scheduler refresh active node",
        ),
      };
    case "AppointFirst":
      return {
        kind: "AppointFirst",
        schedulerInputIndex,
        schedulerOutputIndex,
        activeNodeRefInputIndex: requireReferenceInputIndex(
          ctx,
          config.selection.activeNode.utxo,
          "scheduler refresh active node",
        ),
        registeredWitnessRefInputIndex: requireReferenceInputIndex(
          ctx,
          config.selection.registeredWitnessNode.utxo,
          "scheduler refresh registered witness",
        ),
      };
    case "Rewind":
      return {
        kind: "Rewind",
        schedulerInputIndex,
        schedulerOutputIndex,
        activeRootRefInputIndex: requireReferenceInputIndex(
          ctx,
          config.selection.activeRootNode.utxo,
          "scheduler refresh active root",
        ),
        activeTailRefInputIndex: requireReferenceInputIndex(
          ctx,
          config.selection.activeNode.utxo,
          "scheduler refresh active tail",
        ),
        registeredWitnessRefInputIndex: requireReferenceInputIndex(
          ctx,
          config.selection.registeredWitnessNode.utxo,
          "scheduler refresh registered witness",
        ),
      };
  }
};

const encodeSchedulerRefreshRedeemer = (
  layout: SchedulerRefreshLayout,
): string => {
  const base = {
    scheduler_input_index: layout.schedulerInputIndex,
    scheduler_output_index: layout.schedulerOutputIndex,
  };
  const redeemer: SchedulerSpendRedeemer =
    layout.kind === "Advance"
      ? {
          ...base,
          advancing_approach: {
            GoToNextDueToEndOfShift: {
              new_shifts_operator_node_ref_input_index:
                layout.activeNodeRefInputIndex,
            },
          },
        }
      : layout.kind === "AppointFirst"
        ? {
            ...base,
            advancing_approach: {
              AppointFirstOperator: {
                new_shifts_operator_node_ref_input_index:
                  layout.activeNodeRefInputIndex,
                registered_element_ref_input_index:
                  layout.registeredWitnessRefInputIndex,
              },
            },
          }
        : {
            ...base,
            advancing_approach: {
              RewindDueToEndOfShift: {
                active_operators_root_ref_input_index:
                  layout.activeRootRefInputIndex,
                active_operators_last_node_ref_input_index:
                  layout.activeTailRefInputIndex,
                registered_element_ref_input_index:
                  layout.registeredWitnessRefInputIndex,
              },
            },
          };
  return Data.to(redeemer as never, SchedulerSpendRedeemerSchema as never);
};

export const buildSchedulerRefreshTx = (
  config: BuildSchedulerRefreshTxConfig,
  schedulerSpendRedeemer: BuildTxWithRedeemer | string,
): TxBuilder => {
  const refreshedDatumCbor = encodeSchedulerDatumForChain(
    config.refreshedDatum,
  );
  const witnessReferenceInputs = referenceInputsForSelection(config.selection);
  const referenceInputs =
    config.schedulerSpendingScriptRef === undefined
      ? witnessReferenceInputs
      : [...witnessReferenceInputs, config.schedulerSpendingScriptRef];
  const tx = config.lucid
    .newTx()
    .validFrom(safeTimeNumber(config.validFrom, "scheduler refresh validFrom"))
    .validTo(safeTimeNumber(config.validTo, "scheduler refresh validTo"))
    .readFrom([...referenceInputs])
    .collectFrom([config.schedulerInput], schedulerSpendRedeemer)
    .pay.ToContract(
      config.scheduler.spendingScriptAddress,
      {
        kind: "inline",
        value: refreshedDatumCbor,
      },
      config.schedulerInput.assets,
    )
    .addSignerKey(config.operatorKeyHash);
  return config.schedulerSpendingScriptRef === undefined
    ? tx.attach.Script(config.scheduler.spendingScript)
    : tx;
};

export const buildUnsignedSchedulerRefreshTxProgram = (
  config: BuildSchedulerRefreshTxConfig,
): Effect.Effect<SchedulerRefreshTxResult, SchedulerError> =>
  Effect.gen(function* () {
    const refreshedDatumCbor = yield* Effect.try({
      try: () => encodeSchedulerDatumForChain(config.refreshedDatum),
      catch: (cause) =>
        schedulerError("Failed to encode refreshed scheduler datum", cause),
    });
    const schedulerWitnessUnit = toUnit(
      config.scheduler.policyId,
      SCHEDULER_ASSET_NAME,
    );
    let layout: SchedulerRefreshLayout | undefined;
    let schedulerSpendRedeemerCbor: string | undefined;
    let callbackCount = 0;
    const schedulerSpendRedeemer = ((ctx) => {
      callbackCount += 1;
      const resolvedLayout = deriveSchedulerRefreshLayoutFromRedeemerContext({
        config,
        ctx,
        refreshedDatumCbor,
        schedulerWitnessUnit,
      });
      const redeemerCbor = encodeSchedulerRefreshRedeemer(resolvedLayout);
      if (
        schedulerSpendRedeemerCbor !== undefined &&
        schedulerSpendRedeemerCbor !== redeemerCbor
      ) {
        throw schedulerError(
          "BuildTxWithRedeemer resolved inconsistent scheduler refresh redeemers",
          {
            callback_count: callbackCount.toString(),
            previous_redeemer_cbor: schedulerSpendRedeemerCbor,
            next_redeemer_cbor: redeemerCbor,
          },
        );
      }
      layout = resolvedLayout;
      schedulerSpendRedeemerCbor = redeemerCbor;
      return redeemerCbor;
    }) satisfies BuildTxWithRedeemer;

    yield* Effect.tryPromise({
      try: () =>
        buildSchedulerRefreshTx(config, schedulerSpendRedeemer).complete(
          completeOptionsWithLocalEval({
            presetWalletInputs: config.presetWalletInputs,
          }),
        ),
      catch: (cause) =>
        schedulerError(`Failed to build scheduler refresh tx: ${cause}`, cause),
    });
    if (callbackCount < 1) {
      return yield* failScheduler(
        "BuildTxWithRedeemer did not resolve scheduler refresh redeemer",
        `callback_count=${callbackCount.toString()}`,
      );
    }
    if (layout === undefined || schedulerSpendRedeemerCbor === undefined) {
      return yield* failScheduler(
        "BuildTxWithRedeemer did not resolve scheduler refresh redeemer",
        "missing scheduler refresh redeemer callback",
      );
    }
    const resolvedLayout = layout;
    const resolvedSchedulerSpendRedeemerCbor = schedulerSpendRedeemerCbor;
    const tx = yield* Effect.tryPromise({
      try: () =>
        buildSchedulerRefreshTx(
          config,
          resolvedSchedulerSpendRedeemerCbor,
        ).complete(
          completeOptionsWithLocalEval({
            presetWalletInputs: config.presetWalletInputs,
          }),
        ),
      catch: (cause) =>
        schedulerError(
          `Failed to rebuild scheduler refresh tx: ${cause}`,
          cause,
        ),
    });
    return {
      tx,
      layout: resolvedLayout,
      schedulerSpendRedeemerCbor: resolvedSchedulerSpendRedeemerCbor,
      refreshedDatumCbor,
    };
  });
