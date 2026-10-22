import { type MidgardNativeTxFull } from "@al-ft/midgard-core/codec";

import {
  BuilderInvariantError,
  ProviderCapabilityError,
} from "../core/errors.js";
import { outRefLabel } from "../core/out-ref.js";
import {
  outputAddressPaymentScriptHash,
  outputAddressProtected,
} from "../core/output.js";
import type { ExUnits, Redeemer } from "../core/scripts.js";
import type {
  MidgardProvider,
  MidgardRedeemerBudget,
} from "../provider/types.js";
import type { BuilderState } from "./context.js";
import { nativeInputOutRefs } from "./imported-tx.normalize-resolved-spend-inputs.js";
import {
  effectiveMints,
  redeemerIntentKey,
} from "./script-witnesses.collect-known-script-sources.js";
import { RedeemerTags } from "./script-witnesses.known-script-source.js";

/** Rounds of draft-and-evaluate before giving up on stable budgets. */
const MAX_REDEEMER_EVALUATION_ROUNDS = 4;

type ExUnitsByIntent = ReadonlyMap<string, ExUnits>;

const missing = (redeemer: Redeemer | undefined): boolean =>
  redeemer !== undefined && redeemer.exUnits === undefined;

export const hasRedeemerWithoutExUnits = (state: BuilderState): boolean =>
  state.scripts.spendRedeemers.some(({ redeemer }) => missing(redeemer)) ||
  state.scripts.mints.some(({ redeemer }) => missing(redeemer)) ||
  state.scripts.observers.some(({ redeemer }) => missing(redeemer)) ||
  state.scripts.receiveRedeemers.some(({ redeemer }) => missing(redeemer));

const fill = <R extends Redeemer | undefined>(
  redeemer: R,
  key: string,
  units: ExUnitsByIntent,
): R =>
  redeemer !== undefined && redeemer.exUnits === undefined
    ? { ...redeemer, exUnits: units.get(key) ?? { mem: 0n, steps: 0n } }
    : redeemer;

/**
 * The state with every redeemer that has no exUnits given the units recorded
 * for its intent, or zero. Redeemers with exUnits are kept as they are.
 */
const withRedeemerExUnits = (
  state: BuilderState,
  units: ExUnitsByIntent,
): BuilderState => ({
  ...state,
  scripts: {
    ...state.scripts,
    spendRedeemers: state.scripts.spendRedeemers.map((intent) => ({
      ...intent,
      redeemer: fill(
        intent.redeemer,
        redeemerIntentKey("spend", outRefLabel(intent)),
        units,
      ),
    })),
    mints: state.scripts.mints.map((intent) => ({
      ...intent,
      redeemer: fill(
        intent.redeemer,
        redeemerIntentKey("mint", intent.policyId),
        units,
      ),
    })),
    observers: state.scripts.observers.map((intent) => ({
      ...intent,
      redeemer: fill(
        intent.redeemer,
        redeemerIntentKey("observe", intent.scriptHash),
        units,
      ),
    })),
    receiveRedeemers: state.scripts.receiveRedeemers.map((intent) => ({
      ...intent,
      redeemer: fill(
        intent.redeemer,
        redeemerIntentKey("receive", intent.scriptHash),
        units,
      ),
    })),
  },
});

/**
 * Maps evaluated budgets back to the redeemer intents of `state`, using the
 * pointer order deriveScriptWitnesses assigns: spend by the draft's
 * sorted inputs, mint by effective policy id, observe and receive by sorted
 * script hash.
 */
const exUnitsByIntent = (
  state: BuilderState,
  draft: MidgardNativeTxFull,
  budgets: readonly MidgardRedeemerBudget[],
): ExUnitsByIntent => {
  const spends = nativeInputOutRefs(draft).map((outRef) =>
    redeemerIntentKey("spend", outRefLabel(outRef)),
  );
  const mints = effectiveMints(state.scripts.mints).map(({ policyId }) =>
    redeemerIntentKey("mint", policyId),
  );
  const observers = [
    ...new Set(state.scripts.observers.map(({ scriptHash }) => scriptHash)),
  ]
    .sort()
    .map((scriptHash) => redeemerIntentKey("observe", scriptHash));
  const receivers = [
    ...new Set(
      state.outputs.flatMap((output) => {
        const scriptHash = outputAddressProtected(output.address)
          ? outputAddressPaymentScriptHash(output.address)
          : undefined;
        return scriptHash === undefined ? [] : [scriptHash];
      }),
    ),
  ]
    .sort()
    .map((scriptHash) => redeemerIntentKey("receive", scriptHash));
  const keysByTag = new Map<number, readonly string[]>([
    [RedeemerTags.Spend, spends],
    [RedeemerTags.Mint, mints],
    [RedeemerTags.Reward, observers],
    [RedeemerTags.Receive, receivers],
  ]);
  const units = new Map<string, ExUnits>();
  for (const budget of budgets) {
    const key =
      budget.index <= BigInt(Number.MAX_SAFE_INTEGER)
        ? keysByTag.get(budget.tag)?.[Number(budget.index)]
        : undefined;
    if (key === undefined || units.has(key)) {
      throw new BuilderInvariantError(
        "Evaluated redeemer does not match the draft",
        `tag=${budget.tag.toString()} index=${budget.index.toString()}`,
      );
    }
    units.set(key, { mem: budget.memory, steps: budget.steps });
  }
  return units;
};

/** Whether every evaluated budget fits the units the draft declared. */
const budgetsFitDeclared = (
  declared: ExUnitsByIntent,
  evaluated: ExUnitsByIntent,
): boolean =>
  [...evaluated].every(([key, units]) => {
    const given = declared.get(key);
    return (
      given !== undefined &&
      units.mem <= given.mem &&
      units.steps <= given.steps
    );
  });

/** The per-intent maximum of two budget maps. */
const maxExUnits = (
  left: ExUnitsByIntent,
  right: ExUnitsByIntent,
): ExUnitsByIntent => {
  const merged = new Map(left);
  for (const [key, units] of right) {
    const given = merged.get(key);
    merged.set(
      key,
      given === undefined
        ? units
        : {
            mem: units.mem > given.mem ? units.mem : given.mem,
            steps: units.steps > given.steps ? units.steps : given.steps,
          },
    );
  }
  return merged;
};

/**
 * `state` with every redeemer that has no exUnits given the budget the
 * provider evaluates for it. Drafts are rebuilt with the evaluated budgets
 * until every script fits what its draft declares, since fee and change can
 * move with the declared units.
 */
export const evaluateMissingExUnits = async (
  state: BuilderState,
  provider: MidgardProvider,
  completeDraft: (draft: BuilderState) => Promise<{
    readonly tx: MidgardNativeTxFull;
    readonly txHex: string;
  }>,
): Promise<BuilderState> => {
  let declared: ExUnitsByIntent = new Map();
  for (let round = 0; round < MAX_REDEEMER_EVALUATION_ROUNDS; round += 1) {
    const candidate = withRedeemerExUnits(state, declared);
    const draft = await completeDraft(candidate);
    // The draft is built first so that a malformed transaction reports its
    // own error rather than the provider's.
    if (provider.evaluateTx === undefined) {
      throw new ProviderCapabilityError(
        "/tx/evaluate",
        "Midgard provider cannot evaluate redeemers; give every redeemer exUnits",
      );
    }
    const evaluated = exUnitsByIntent(
      state,
      draft.tx,
      await provider.evaluateTx(draft.txHex),
    );
    if (round > 0 && budgetsFitDeclared(declared, evaluated)) {
      return candidate;
    }
    declared = maxExUnits(declared, evaluated);
  }
  throw new BuilderInvariantError(
    "Evaluated redeemer budgets did not settle",
    `rounds=${MAX_REDEEMER_EVALUATION_ROUNDS.toString()}`,
  );
};
