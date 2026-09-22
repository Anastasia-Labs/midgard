import {
  type LucidEvolution,
  type Network,
  type Script,
  type TxSignBuilder,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { scriptRewardAddress } from "./cardano-addresses.js";
import { LucidError, UnspecifiedNetworkError } from "./errors.js";
import {
  getUnappliedScript,
  parseFaultProofBlueprint,
} from "./fraud-proof/contracts/blueprint.js";
import { completeTxWithLocalUPLCEvalProgram } from "./tx-completion.js";

export const PHAS_MEMBERSHIP_WITHDRAWAL_VALIDATOR_TITLE =
  "phas.membership.withdraw";

export type PhasMembershipBlueprint = {
  readonly validators: readonly {
    readonly title: string;
    readonly compiledCode: string;
    /**
     * The blueprint's declared parameter list, carried so the loader can
     * refuse to deploy `compiledCode` bare the moment the validator grows a
     * parameter (#610). Absent means the validator declares none.
     */
    readonly parameters?: readonly unknown[];
  }[];
};

export type PhasMembershipIdentity = {
  readonly rewardAddress: string;
  readonly scriptHash: string;
};

export type BuiltPhasMembershipRewardRegistrationTx = {
  readonly tx: TxSignBuilder;
  readonly rewardAddress: string;
  readonly scriptHash: string;
};

export type PhasMembershipRewardRegistrationBuildError =
  | LucidError
  | UnspecifiedNetworkError;

export const parsePhasMembershipBlueprint = (
  value: unknown,
): PhasMembershipBlueprint => {
  if (typeof value !== "object" || value === null) {
    throw new Error("PHAS membership blueprint must be a JSON object");
  }

  const validators = (value as { readonly validators?: unknown }).validators;
  if (!Array.isArray(validators)) {
    throw new Error("PHAS membership blueprint must contain validators[]");
  }

  return {
    validators: validators.map((validator, index) => {
      if (typeof validator !== "object" || validator === null) {
        throw new Error(`validators[${index}] must be an object`);
      }
      const candidate = validator as {
        readonly title?: unknown;
        readonly compiledCode?: unknown;
        readonly parameters?: unknown;
      };
      if (typeof candidate.title !== "string") {
        throw new Error(`validators[${index}].title must be a string`);
      }
      if (
        typeof candidate.compiledCode !== "string" ||
        candidate.compiledCode.length === 0
      ) {
        throw new Error(
          `validators[${index}].compiledCode must be a non-empty string`,
        );
      }
      if (
        candidate.parameters !== undefined &&
        !Array.isArray(candidate.parameters)
      ) {
        throw new Error(
          `validators[${index}].parameters must be an array when present`,
        );
      }
      return {
        title: candidate.title,
        compiledCode: candidate.compiledCode,
        ...(candidate.parameters === undefined
          ? {}
          : { parameters: candidate.parameters as readonly unknown[] }),
      };
    }),
  };
};

export const phasMembershipWithdrawalScriptFromBlueprint = (
  blueprint: PhasMembershipBlueprint,
): Script => ({
  type: "PlutusV3",
  script: getUnappliedScript(
    parseFaultProofBlueprint(blueprint),
    PHAS_MEMBERSHIP_WITHDRAWAL_VALIDATOR_TITLE,
  ),
});

export const phasMembershipRewardAddress = (
  network: Network,
  script: Script,
): string => scriptRewardAddress(network, script);

export const phasMembershipIdentity = (
  network: Network,
  script: Script,
): PhasMembershipIdentity => ({
  rewardAddress: phasMembershipRewardAddress(network, script),
  scriptHash: validatorToScriptHash(script),
});

export const buildPhasMembershipRewardRegistrationTxProgram = (
  lucid: LucidEvolution,
  config: { readonly script: Script },
): Effect.Effect<
  BuiltPhasMembershipRewardRegistrationTx,
  PhasMembershipRewardRegistrationBuildError
> =>
  Effect.gen(function* () {
    const network = lucid.config().network;
    if (network === undefined) {
      return yield* Effect.fail(
        new UnspecifiedNetworkError({
          message:
            "Cannot build PHAS membership reward-account registration transaction without a configured Lucid network",
          cause: "lucid.config().network is undefined",
        }),
      );
    }

    const identity = phasMembershipIdentity(network, config.script);
    const txBuilder = yield* Effect.try({
      try: () => lucid.newTx().register.Stake(identity.rewardAddress),
      catch: (cause) =>
        new LucidError({
          message:
            "Failed to build PHAS membership reward-account registration transaction",
          cause,
        }),
    });
    const tx = yield* completeTxWithLocalUPLCEvalProgram(
      txBuilder,
      (cause) =>
        new LucidError({
          message:
            "Failed to complete PHAS membership reward-account registration transaction",
          cause,
        }),
    );
    return {
      tx,
      rewardAddress: identity.rewardAddress,
      scriptHash: identity.scriptHash,
    };
  });
