import { compareOutRefs } from "@al-ft/midgard-core/out-ref";
import {
  type Assets,
  coreToTxOutput,
  Data,
  type Script,
  type TxBuilder,
  type TxSignBuilder,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { LucidError } from "./common.js";
import {
  type BuiltReferenceScriptPublicationTx,
  REFERENCE_SCRIPT_PUBLICATION_VALIDITY_MS,
  type ReferenceScriptAuthPolicyRef,
  referenceScriptAuthUnit,
  type ReferenceScriptPublicationLayout,
  type ReferenceScriptPublicationTxParams,
  type ReferenceScriptTarget,
  type ReferenceScriptWalletReplenishmentTxParams,
  SCRIPT_REF_OUTPUT_LOVELACE,
  SCRIPT_REF_PUBLICATION_FUNDING_BUFFER_LOVELACE,
  type TxCompleteOptions,
} from "./reference-scripts.create-reference-script-auth-policy.js";
import { StateQueueError } from "./state-queue.js";
import { isPlainPositiveAdaOnlyUtxo } from "./tx-output-utils.js";

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
      const publicationDeadline =
        lucid.slotToUnixTime(lucid.currentSlot()) +
        REFERENCE_SCRIPT_PUBLICATION_VALIDITY_MS;
      tx = tx.validTo(publicationDeadline);
      if (authPolicy.expiresAtUnixTime !== undefined) {
        // Bound by the native authority's slot, including a non-aligned planned
        // lifetime. Subtracting one millisecond can remain in its expiry slot.
        tx = tx.validTo(
          Math.min(
            publicationDeadline,
            lucid.slotToUnixTime(
              lucid.unixTimeToSlot(authPolicy.expiresAtUnixTime) - 1,
            ),
          ),
        );
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
