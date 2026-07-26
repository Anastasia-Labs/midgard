import { compareOutRefs } from "@al-ft/midgard-core/out-ref";
import {
  type Assets,
  CML,
  coreToTxOutput,
  Data,
  fromText,
  type LucidEvolution,
  mintingPolicyToId,
  type Script,
  toUnit,
  type TxBuilder,
  type TxSignBuilder,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { MintingValidator } from "@/common.js";
import { LucidError } from "@/common.js";
import { StateQueueError } from "@/state-queue.js";
import { isPlainPositiveAdaOnlyUtxo } from "@/tx-output-utils.js";

export const REFERENCE_SCRIPT_AUTH_TIMELOCK_MS = 4 * 60 * 60 * 1000;
export const REFERENCE_SCRIPT_AUTH_MIN_REMAINING_MS = 90 * 60 * 1000;

export const REFERENCE_SCRIPT_AUTH_TOKEN_NAMES = {
  "reference-script-auth minting": "ReferenceScriptAuthMint",
  "hub-oracle minting": "HubOracleMint",
  "da-params-governor spending": "DaParamsGovernorSpend",
  "da-params-governor minting": "DaParamsGovernorMint",
  "da-attestation spending": "DaAttestationSpend",
  "da-attestation minting": "DaAttestationMint",
  "scheduler spending": "SchedulerSpend",
  "scheduler minting": "SchedulerMint",
  "state-queue spending": "StateQueueSpend",
  "state-queue minting": "StateQueueMint",
  "registered-operators spending": "RegisteredOperatorsSpend",
  "registered-operators minting": "RegisteredOperatorsMint",
  "active-operators spending": "ActiveOperatorsSpend",
  "active-operators minting": "ActiveOperatorsMint",
  "retired-operators spending": "RetiredOperatorsSpend",
  "retired-operators minting": "RetiredOperatorsMint",
  "fraud-proof-catalogue minting": "FraudProofCatalogueMint",
  "deposit minting": "DepositMint",
  "deposit spending": "DepositSpend",
  "withdrawal minting": "WithdrawalMint",
  "withdrawal spending": "WithdrawalSpend",
  "settlement minting": "SettlementMint",
  "membership proof withdrawal": "MembershipProofWithdraw",
  "reserve spending": "ReserveSpend",
  "reserve observer": "ReserveObserver",
  "payout spending": "PayoutSpend",
  "payout minting": "PayoutMint",
  "V1 transaction-field preimage publication": "V1TxFieldPreimageSpend",
  "V1 transaction-field receipt": "V1TxFieldReceiptSpend",
  "V1 transaction-field receipt minting": "V1TxFieldReceiptMint",
  "V1 immutable CEK program-material publication":
    "V1CekProgramMaterialSpend",
  "V1 validation-trace dispute": "V1ValidationTraceDispute",
  "V1 validation-trace source": "V1ValidationTraceSource",
  "V1 validation-trace game": "V1ValidationTraceGame",
  "V1 validation-trace boundary": "V1ValidationTraceBoundary",
  "V1 validation-trace timeout": "V1ValidationTraceTimeout",
  "V1 validation-trace award": "V1ValidationTraceAward",
} as const;

export type ReferenceScriptAuthTokenTarget =
  keyof typeof REFERENCE_SCRIPT_AUTH_TOKEN_NAMES;

export type ReferenceScriptAuthPolicy = MintingValidator & {
  readonly expiresAtSlot: number;
  readonly expiresAtUnixTime: number;
  readonly timelockDurationMs: number;
};

export type ReferenceScriptAuthPolicyRef = Pick<
  ReferenceScriptAuthPolicy,
  "policyId"
>;

export type ReferenceScriptAuthMintingPolicy = MintingValidator & {
  readonly expiresAtUnixTime?: number;
};

export type ReferenceScriptAuthDeadlineDiagnostic = {
  readonly scopeName: string;
  readonly targetNames: readonly string[];
  readonly nowMs: number;
  readonly expiresAtUnixTime?: number;
  readonly remainingMs?: number;
  readonly minRemainingMs: number;
};

export type ReferenceScriptAuthPolicyDeploymentInfo = {
  readonly policyId: string;
  readonly nativeScript: {
    readonly type: "Native";
    readonly cborHex: string;
    readonly expiresAtSlot: number;
    readonly expiresAtUnixTime: number;
    readonly timelockDurationMs: number;
  };
  readonly tokenNames: Readonly<Record<ReferenceScriptAuthTokenTarget, string>>;
  readonly postTimelockAudit: {
    readonly required: true;
    readonly rule: string;
  };
};

export type ReferenceScriptTarget = {
  readonly name: string;
  readonly script: Script;
};

export type ReferenceScriptResolved = {
  readonly name: string;
  readonly utxo: UTxO;
};

export type ReferenceScriptWalletReplenishmentTxParams = {
  readonly lucid: LucidEvolution;
  readonly selectedFundingInputs: readonly UTxO[];
  readonly referenceScriptAddress: string;
  readonly topUpAmount: bigint;
};

export type ReferenceScriptPublicationTxParams = {
  readonly lucid: LucidEvolution;
  readonly selectedFundingInputs: readonly UTxO[];
  readonly walletAddress: string;
  readonly referenceScriptsAddress: string;
  readonly missingTargets: readonly ReferenceScriptTarget[];
  readonly authPolicy: ReferenceScriptAuthMintingPolicy;
};

export type ReferenceScriptPublicationLayout = {
  readonly localReferenceOutputs: ReadonlyMap<string, Omit<UTxO, "txHash">>;
  readonly walletOutputs: readonly Omit<UTxO, "txHash">[];
};

export type BuiltReferenceScriptPublicationTx = {
  readonly tx: TxSignBuilder;
  readonly layout: ReferenceScriptPublicationLayout;
};

type TxCompleteOptions = NonNullable<Parameters<TxBuilder["complete"]>[0]>;

export const SCRIPT_REF_OUTPUT_LOVELACE = 4_000_000n;
export const SCRIPT_REF_PUBLICATION_FUNDING_BUFFER_LOVELACE = 10_000_000n;

export const referenceScriptAuthTokenNameText = (
  targetName: string,
): string => {
  const tokenName =
    REFERENCE_SCRIPT_AUTH_TOKEN_NAMES[
      targetName as ReferenceScriptAuthTokenTarget
    ];
  if (tokenName === undefined) {
    throw new Error(`Missing reference-script auth token name: ${targetName}`);
  }
  return tokenName;
};

export const referenceScriptAuthTokenName = (targetName: string): string =>
  fromText(referenceScriptAuthTokenNameText(targetName));

export const referenceScriptAuthUnit = (
  policyId: string,
  targetName: string,
): string => toUnit(policyId, referenceScriptAuthTokenName(targetName));

export const createReferenceScriptAuthPolicy = (
  lucid: LucidEvolution,
  nowMs: number = Date.now(),
  timelockDurationMs: number = REFERENCE_SCRIPT_AUTH_TIMELOCK_MS,
): ReferenceScriptAuthPolicy => {
  const expiresAtUnixTime = nowMs + timelockDurationMs;
  const expiresAtSlot = lucid.unixTimeToSlot(expiresAtUnixTime);
  const nativeScript = CML.NativeScript.new_script_invalid_hereafter(
    BigInt(expiresAtSlot),
  );
  const mintingScript: Script = {
    type: "Native",
    script: nativeScript.to_cbor_hex(),
  };
  return {
    mintingScriptCBOR: mintingScript.script,
    policyId: mintingPolicyToId(mintingScript),
    mintingScript,
    expiresAtSlot,
    expiresAtUnixTime,
    timelockDurationMs,
  };
};

export const referenceScriptAuthPolicyFromDeploymentInfo = (
  deploymentInfo: ReferenceScriptAuthPolicyDeploymentInfo,
): ReferenceScriptAuthPolicy => ({
  mintingScriptCBOR: deploymentInfo.nativeScript.cborHex,
  mintingScript: {
    type: "Native",
    script: deploymentInfo.nativeScript.cborHex,
  },
  policyId: deploymentInfo.policyId,
  expiresAtSlot: deploymentInfo.nativeScript.expiresAtSlot,
  expiresAtUnixTime: deploymentInfo.nativeScript.expiresAtUnixTime,
  timelockDurationMs: deploymentInfo.nativeScript.timelockDurationMs,
});

export const referenceScriptAuthRemainingMs = (
  policy: ReferenceScriptAuthMintingPolicy,
  nowMs: number,
): number | undefined =>
  policy.expiresAtUnixTime === undefined
    ? undefined
    : policy.expiresAtUnixTime - nowMs;

export class ReferenceScriptAuthDeadlineError extends Error {
  readonly diagnostic: ReferenceScriptAuthDeadlineDiagnostic;

  constructor(diagnostic: ReferenceScriptAuthDeadlineDiagnostic) {
    const remaining =
      diagnostic.remainingMs === undefined
        ? "missing"
        : diagnostic.remainingMs.toString();
    super(
      [
        `Reference-script auth policy is not safe to use for ${diagnostic.scopeName}`,
        `now_ms=${diagnostic.nowMs.toString()}`,
        `expires_at_unix_time=${
          diagnostic.expiresAtUnixTime === undefined
            ? "missing"
            : diagnostic.expiresAtUnixTime.toString()
        }`,
        `remaining_ms=${remaining}`,
        `min_remaining_ms=${diagnostic.minRemainingMs.toString()}`,
        `targets=${diagnostic.targetNames.join(",")}`,
      ].join("; "),
    );
    this.name = "ReferenceScriptAuthDeadlineError";
    this.diagnostic = diagnostic;
  }
}

export const assertReferenceScriptAuthMinimumRemaining = ({
  policy,
  nowMs,
  minRemainingMs,
  scopeName,
  targetNames,
}: {
  readonly policy: ReferenceScriptAuthMintingPolicy;
  readonly nowMs: number;
  readonly minRemainingMs: number;
  readonly scopeName: string;
  readonly targetNames: readonly string[];
}): void => {
  if (!Number.isSafeInteger(minRemainingMs) || minRemainingMs <= 0) {
    throw new Error(
      "REFERENCE_SCRIPT_AUTH_MIN_REMAINING_MS must be a positive safe integer",
    );
  }
  const remainingMs = referenceScriptAuthRemainingMs(policy, nowMs);
  if (remainingMs === undefined || remainingMs <= minRemainingMs) {
    throw new ReferenceScriptAuthDeadlineError({
      scopeName,
      targetNames,
      nowMs,
      expiresAtUnixTime: policy.expiresAtUnixTime,
      remainingMs,
      minRemainingMs,
    });
  }
};

export const referenceScriptAuthPolicyDeploymentInfo = (
  policy: ReferenceScriptAuthPolicy,
): ReferenceScriptAuthPolicyDeploymentInfo => {
  if (policy.mintingScript.type !== "Native") {
    throw new Error("Reference-script auth policy must be a native script");
  }
  return {
    policyId: policy.policyId,
    nativeScript: {
      type: "Native",
      cborHex: policy.mintingScript.script,
      expiresAtSlot: policy.expiresAtSlot,
      expiresAtUnixTime: policy.expiresAtUnixTime,
      timelockDurationMs: policy.timelockDurationMs,
    },
    tokenNames: REFERENCE_SCRIPT_AUTH_TOKEN_NAMES,
    postTimelockAudit: {
      required: true,
      rule: "After the timelock expires, verify there is exactly one role token under this policy for every listed token name before treating the deployment as production-ready.",
    },
  };
};

export const referenceScriptRoleAssets = (
  target: ReferenceScriptTarget,
  authPolicy: ReferenceScriptAuthPolicyRef,
): Assets => ({
  lovelace: SCRIPT_REF_OUTPUT_LOVELACE,
  [referenceScriptAuthUnit(authPolicy.policyId, target.name)]: 1n,
});

export const isSameScriptRef = (
  left: Script | null | undefined,
  right: Script,
): boolean => {
  if (left === undefined || left === null || left.type !== right.type) {
    return false;
  }
  try {
    return validatorToScriptHash(left) === validatorToScriptHash(right);
  } catch {
    return false;
  }
};

export const hasReferenceScriptAuthRole = (
  utxo: UTxO,
  target: ReferenceScriptTarget,
  authPolicy: ReferenceScriptAuthPolicyRef,
): boolean =>
  utxo.assets[referenceScriptAuthUnit(authPolicy.policyId, target.name)] === 1n;

export const referenceScriptPublicationFundingTarget = (
  missingTargetCount: number,
): bigint =>
  SCRIPT_REF_OUTPUT_LOVELACE * (BigInt(missingTargetCount) + 1n) +
  SCRIPT_REF_PUBLICATION_FUNDING_BUFFER_LOVELACE;

const lovelaceOf = (utxo: UTxO): bigint => utxo.assets.lovelace ?? 0n;

const isPlainAdaOnlyUtxo = isPlainPositiveAdaOnlyUtxo;

export const orderReferenceScriptFundingUtxos = (
  utxos: readonly UTxO[],
): readonly UTxO[] =>
  [...utxos].sort((left, right) => {
    const leftIsPlain = isPlainAdaOnlyUtxo(left);
    const rightIsPlain = isPlainAdaOnlyUtxo(right);
    if (leftIsPlain !== rightIsPlain) {
      return leftIsPlain ? -1 : 1;
    }
    const leftLovelace = lovelaceOf(left);
    const rightLovelace = lovelaceOf(right);
    if (leftLovelace === rightLovelace) {
      return compareOutRefs(left, right);
    }
    return leftLovelace > rightLovelace ? -1 : 1;
  });

export const selectReferenceScriptFundingUtxos = (
  utxos: readonly UTxO[],
  targetLovelace: bigint,
): readonly UTxO[] => {
  if (targetLovelace <= 0n) {
    return [];
  }
  const selected: UTxO[] = [];
  let covered = 0n;
  for (const utxo of orderReferenceScriptFundingUtxos(utxos)) {
    if (!isPlainAdaOnlyUtxo(utxo)) {
      continue;
    }
    selected.push(utxo);
    covered += lovelaceOf(utxo);
    if (covered >= targetLovelace) {
      return selected;
    }
  }
  return [];
};

const completeWithSelectedFundingInputs = (
  selectedFundingInputs: readonly UTxO[],
): TxCompleteOptions => ({
  coinSelection: false,
  localUPLCEval: true,
  presetWalletInputs: [...selectedFundingInputs],
});

export const incompleteReferenceScriptWalletReplenishmentTxProgram = ({
  lucid,
  selectedFundingInputs,
  referenceScriptAddress,
  topUpAmount,
}: ReferenceScriptWalletReplenishmentTxParams): Effect.Effect<
  TxBuilder,
  LucidError
> =>
  Effect.try({
    try: () =>
      lucid
        .newTx()
        .collectFrom([...selectedFundingInputs])
        .pay.ToAddress(referenceScriptAddress, { lovelace: topUpAmount }),
    catch: (cause) =>
      new LucidError({
        message: `Failed to build reference-script wallet replenishment transaction: ${String(cause)}`,
        cause,
      }),
  });

export const completeReferenceScriptWalletReplenishmentTxProgram = (
  params: ReferenceScriptWalletReplenishmentTxParams,
): Effect.Effect<TxSignBuilder, LucidError> =>
  Effect.gen(function* () {
    const tx =
      yield* incompleteReferenceScriptWalletReplenishmentTxProgram(params);
    return yield* Effect.tryPromise({
      try: () =>
        tx.complete(
          completeWithSelectedFundingInputs(params.selectedFundingInputs),
        ),
      catch: (cause) =>
        new LucidError({
          message: `Failed to complete reference-script wallet replenishment transaction: ${String(cause)}`,
          cause,
        }),
    });
  });

export const incompleteReferenceScriptPublicationTxProgram = ({
  lucid,
  selectedFundingInputs,
  walletAddress,
  referenceScriptsAddress,
  missingTargets,
  authPolicy,
}: ReferenceScriptPublicationTxParams): Effect.Effect<TxBuilder, LucidError> =>
  Effect.try({
    try: () => {
      const roleMintAssets: Assets = {};
      for (const target of missingTargets) {
        roleMintAssets[
          referenceScriptAuthUnit(authPolicy.policyId, target.name)
        ] = 1n;
      }
      let tx = lucid.newTx().collectFrom([...selectedFundingInputs]);
      tx =
        authPolicy.mintingScript.type === "Native"
          ? tx.mintAssets(roleMintAssets)
          : tx.mintAssets(roleMintAssets, Data.void());
      tx = tx.attach.MintingPolicy(authPolicy.mintingScript);
      if (authPolicy.expiresAtUnixTime !== undefined) {
        tx = tx.validTo(authPolicy.expiresAtUnixTime - 1);
      }
      tx = tx.pay.ToAddressWithData(walletAddress, undefined, {
        lovelace: SCRIPT_REF_OUTPUT_LOVELACE,
      });
      for (const target of missingTargets) {
        tx = tx.pay.ToAddressWithData(
          referenceScriptsAddress,
          undefined,
          referenceScriptRoleAssets(target, authPolicy),
          target.script,
        );
      }
      return tx;
    },
    catch: (cause) =>
      new LucidError({
        message: `Failed to build reference-script publication transaction for ${missingTargets
          .map(({ name }) => name)
          .join(", ")}: ${String(cause)}`,
        cause,
      }),
  });

export const resolveReferenceScriptPublicationLayout = (
  tx: TxSignBuilder,
  params: Pick<
    ReferenceScriptPublicationTxParams,
    | "walletAddress"
    | "referenceScriptsAddress"
    | "missingTargets"
    | "authPolicy"
  >,
): Effect.Effect<ReferenceScriptPublicationLayout, StateQueueError> =>
  Effect.try({
    try: () => {
      const publicationOutputs = tx.toTransaction().body().outputs();
      const localReferenceOutputs = new Map<string, Omit<UTxO, "txHash">>();
      const walletOutputs: Omit<UTxO, "txHash">[] = [];
      for (
        let outputIndex = 0;
        outputIndex < publicationOutputs.len();
        outputIndex += 1
      ) {
        const output = coreToTxOutput(publicationOutputs.get(outputIndex));
        if (output.address === params.walletAddress) {
          walletOutputs.push({
            outputIndex,
            address: output.address,
            assets: output.assets,
            datum: output.datum ?? undefined,
            datumHash: output.datumHash ?? undefined,
            scriptRef: output.scriptRef ?? undefined,
          });
        }
        if (output.address !== params.referenceScriptsAddress) {
          continue;
        }
        if (output.scriptRef === undefined) {
          continue;
        }
        const matchingTarget = params.missingTargets.find(
          (target) =>
            !localReferenceOutputs.has(target.name) &&
            isSameScriptRef(output.scriptRef, target.script) &&
            output.assets[
              referenceScriptAuthUnit(params.authPolicy.policyId, target.name)
            ] === 1n,
        );
        if (matchingTarget === undefined) {
          continue;
        }
        localReferenceOutputs.set(matchingTarget.name, {
          outputIndex,
          address: output.address,
          assets: output.assets,
          datum: output.datum ?? undefined,
          datumHash: output.datumHash ?? undefined,
          scriptRef: output.scriptRef,
        });
      }
      return {
        localReferenceOutputs,
        walletOutputs,
      };
    },
    catch: (cause) =>
      new StateQueueError({
        message: "Failed to resolve reference-script publication layout",
        cause,
      }),
  });

export const completeReferenceScriptPublicationTxProgram = (
  params: ReferenceScriptPublicationTxParams,
): Effect.Effect<
  BuiltReferenceScriptPublicationTx,
  LucidError | StateQueueError
> =>
  Effect.gen(function* () {
    const tx = yield* incompleteReferenceScriptPublicationTxProgram(params);
    const unsigned = yield* Effect.tryPromise({
      try: () =>
        tx.complete(
          completeWithSelectedFundingInputs(params.selectedFundingInputs),
        ),
      catch: (cause) =>
        new LucidError({
          message: `Failed to complete reference-script publication transaction for ${params.missingTargets
            .map(({ name }) => name)
            .join(", ")}: ${String(cause)}`,
          cause,
        }),
    });
    const layout = yield* resolveReferenceScriptPublicationLayout(
      unsigned,
      params,
    );
    return { tx: unsigned, layout };
  });
