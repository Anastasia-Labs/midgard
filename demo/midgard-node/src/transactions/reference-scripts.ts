import * as SDK from "@al-ft/midgard-sdk";
import {
  coreToTxOutput,
  type LucidEvolution,
  type Script,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { formatUnknownError } from "@/error-format.js";
import { compareOutRefs, outRefLabel } from "@/tx-context.js";
import {
  handleSignSubmit,
  TxSignError,
  TxSubmitError,
} from "@/transactions/utils.js";

export type ReferenceScriptTarget = {
  readonly name: string;
  readonly script: Script;
};

export type ReferenceScriptResolved = {
  readonly name: string;
  readonly utxo: UTxO;
};

export type ReferenceScriptPublication = ReferenceScriptResolved;

const SCRIPT_REF_OUTPUT_LOVELACE = 4_000_000n;
const SCRIPT_REF_PUBLICATION_FUNDING_BUFFER_LOVELACE = 10_000_000n;
const REFERENCE_SCRIPT_WALLET_WORKING_CAPITAL_LOVELACE = 50_000_000n;
const REFERENCE_SCRIPT_PUBLICATION_BALANCE_INSUFFICIENT_PATTERN =
  /UTxO Balance Insufficient/i;
const REFERENCE_SCRIPT_PUBLICATION_TX_SIZE_EXCEEDED_PATTERN =
  /Max transaction size of \d+ exceeded\. Found: \d+/i;
const REFERENCE_SCRIPT_PUBLICATION_BALANCE_GAP_PATTERN =
  /Inputs:\s*Value\s*\{\s*coin:\s*(\d+)[\s\S]*?Outputs:\s*Value\s*\{\s*coin:\s*(\d+)/i;
const WALLET_OWN_ADDRESS_REFRESH_MAX_RETRIES = 6;
const WALLET_OWN_ADDRESS_REFRESH_RETRY_DELAY = "1 second";
const WALLET_OUTREF_RECONCILE_MAX_RETRIES = 4;
const WALLET_OUTREF_RECONCILE_RETRY_DELAY = "750 millis";

export const REFERENCE_SCRIPT_COMMAND_NAMES = [
  "node-runtime",
  "protocol-init",
  "hub-oracle",
  "state-queue",
  "scheduler",
  "registered-operators",
  "active-operators",
  "retired-operators",
  "deposit",
  "settlement",
] as const;

export type ReferenceScriptCommandName =
  (typeof REFERENCE_SCRIPT_COMMAND_NAMES)[number];

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

export const fetchReferenceScriptUtxosProgram = (
  lucid: LucidEvolution,
  referenceScriptsAddress: string,
  targets: readonly ReferenceScriptTarget[],
): Effect.Effect<readonly ReferenceScriptResolved[], SDK.StateQueueError> =>
  Effect.gen(function* () {
    const referenceScriptUtxos = yield* Effect.tryPromise({
      try: () => lucid.utxosAt(referenceScriptsAddress),
      catch: (cause) =>
        new SDK.StateQueueError({
          message: `Failed to fetch reference-script UTxOs at ${referenceScriptsAddress}`,
          cause,
        }),
    });
    return yield* Effect.forEach(targets, (target) =>
      Effect.gen(function* () {
        const resolved = [...referenceScriptUtxos]
          .filter((utxo) => isSameScriptRef(utxo.scriptRef, target.script))
          .sort(compareOutRefs)[0];
        if (resolved === undefined) {
          return yield* Effect.fail(
            new SDK.StateQueueError({
              message: "Missing reference script",
              cause: `${target.name} at ${referenceScriptsAddress}`,
            }),
          );
        }
        return {
          name: target.name,
          utxo: resolved,
        };
      }),
    );
  }).pipe(
    Effect.mapError((cause) =>
      cause instanceof SDK.StateQueueError
        ? cause
        : new SDK.StateQueueError({
            message: "Failed to resolve required reference scripts",
            cause,
          }),
    ),
  );

export const referenceScriptByName = (
  resolved: readonly ReferenceScriptResolved[],
  name: string,
): UTxO => {
  const found = resolved.find((candidate) => candidate.name === name);
  if (found === undefined) {
    throw new Error(`Missing resolved reference script: ${name}`);
  }
  return found.utxo;
};

export const describeReferenceScriptInputs = (
  utxos: readonly UTxO[],
): string => utxos.map(outRefLabel).join(",");

const isReferenceScriptPublicationTxTooLarge = (cause: unknown): boolean =>
  REFERENCE_SCRIPT_PUBLICATION_TX_SIZE_EXCEEDED_PATTERN.test(
    formatUnknownError(cause),
  );

const isReferenceScriptPublicationBalanceInsufficient = (
  cause: unknown,
): boolean =>
  REFERENCE_SCRIPT_PUBLICATION_BALANCE_INSUFFICIENT_PATTERN.test(
    formatUnknownError(cause),
  );

const resolveReferenceScriptPublicationAdditionalFunding = (
  cause: unknown,
): bigint | undefined => {
  const match =
    REFERENCE_SCRIPT_PUBLICATION_BALANCE_GAP_PATTERN.exec(
      formatUnknownError(cause),
    );
  if (match === null) {
    return undefined;
  }
  const inputs = BigInt(match[1]);
  const outputs = BigInt(match[2]);
  if (outputs <= inputs) {
    return undefined;
  }
  return outputs - inputs + SCRIPT_REF_PUBLICATION_FUNDING_BUFFER_LOVELACE;
};

const lovelaceOf = (utxo: UTxO): bigint => utxo.assets.lovelace ?? 0n;

export const utxoOutRefKey = (utxo: UTxO): string =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

const filterPlainWalletUtxos = (utxos: readonly UTxO[]): readonly UTxO[] =>
  utxos.filter((utxo) => utxo.scriptRef === undefined);

const sumWalletLovelace = (utxos: readonly UTxO[]): bigint =>
  utxos.reduce((total, utxo) => total + lovelaceOf(utxo), 0n);

const resolveReferenceScriptPublicationFundingTarget = (
  missingTargetCount: number,
): bigint =>
  SCRIPT_REF_OUTPUT_LOVELACE * (BigInt(missingTargetCount) + 1n) +
  SCRIPT_REF_PUBLICATION_FUNDING_BUFFER_LOVELACE;

const orderWalletFundingUtxos = (utxos: readonly UTxO[]): readonly UTxO[] =>
  [...utxos].sort((left, right) => {
    const leftIsPlain = left.scriptRef === undefined;
    const rightIsPlain = right.scriptRef === undefined;
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

export const selectWalletFundingUtxos = (
  utxos: readonly UTxO[],
  targetLovelace: bigint,
): readonly UTxO[] => {
  const sorted = orderWalletFundingUtxos(utxos);
  const selected: UTxO[] = [];
  let covered = 0n;
  for (const utxo of sorted) {
    selected.push(utxo);
    covered += lovelaceOf(utxo);
    if (covered >= targetLovelace) {
      break;
    }
  }
  return selected;
};

const mergeWalletUtxosPreservingScriptRefs = (
  liveUtxos: readonly UTxO[],
  cachedUtxos: readonly UTxO[],
): readonly UTxO[] => {
  const cachedByOutRef = new Map(
    cachedUtxos.map((utxo) => [utxoOutRefKey(utxo), utxo]),
  );
  return liveUtxos.map((utxo) => {
    if (utxo.scriptRef !== undefined) {
      return utxo;
    }
    const cached = cachedByOutRef.get(utxoOutRefKey(utxo));
    if (cached?.scriptRef === undefined) {
      return utxo;
    }
    return {
      ...utxo,
      scriptRef: cached.scriptRef,
    };
  });
};

const reconcileLiveWalletUtxos = (
  lucid: LucidEvolution,
  utxos: readonly UTxO[],
): Effect.Effect<readonly UTxO[], SDK.StateQueueError> =>
  Effect.gen(function* () {
    if (utxos.length === 0) {
      return [];
    }
    const uniqueOutRefs = Array.from(
      new Map(
        utxos.map((utxo) => [
          utxoOutRefKey(utxo),
          {
            txHash: utxo.txHash,
            outputIndex: utxo.outputIndex,
          },
        ]),
      ).values(),
    );
    let live: readonly UTxO[] | null = null;
    let lastCause: unknown = null;
    for (
      let attempt = 0;
      attempt < WALLET_OUTREF_RECONCILE_MAX_RETRIES;
      attempt += 1
    ) {
      const liveAttempt = yield* Effect.either(
        Effect.tryPromise({
          try: () => lucid.utxosByOutRef(uniqueOutRefs),
          catch: (cause) => cause,
        }),
      );
      if (liveAttempt._tag === "Right") {
        live = liveAttempt.right;
        break;
      }
      lastCause = liveAttempt.left;
      if (attempt + 1 < WALLET_OUTREF_RECONCILE_MAX_RETRIES) {
        yield* Effect.logWarning(
          `Wallet UTxO out-ref reconciliation failed (attempt ${(attempt + 1).toString()}/${WALLET_OUTREF_RECONCILE_MAX_RETRIES.toString()}); retrying in ${WALLET_OUTREF_RECONCILE_RETRY_DELAY}. cause=${String(lastCause)}`,
        );
        yield* Effect.sleep(WALLET_OUTREF_RECONCILE_RETRY_DELAY);
      }
    }
    if (live === null) {
      yield* Effect.logWarning(
        `Wallet UTxO out-ref reconciliation exhausted retries; using wallet snapshot as fallback. attempts=${WALLET_OUTREF_RECONCILE_MAX_RETRIES.toString()},last_cause=${String(lastCause)}`,
      );
      return utxos;
    }
    if (utxos.length > 0 && live.length === 0) {
      yield* Effect.logWarning(
        "Wallet UTxO out-ref reconciliation returned zero live entries from a non-empty snapshot; keeping wallet snapshot to avoid false empty-input failures.",
      );
      return utxos;
    }
    return mergeWalletUtxosPreservingScriptRefs(live, utxos);
  });

const fetchReconciledWalletUtxos = (
  lucid: LucidEvolution,
  failureMessage: string,
): Effect.Effect<readonly UTxO[], SDK.StateQueueError> =>
  Effect.gen(function* () {
    const walletUtxosRaw = yield* Effect.tryPromise({
      try: () => lucid.wallet().getUtxos(),
      catch: (cause) =>
        new SDK.StateQueueError({
          message: failureMessage,
          cause,
        }),
    });
    return yield* reconcileLiveWalletUtxos(lucid, walletUtxosRaw);
  });

const refreshWalletUtxosFromOwnAddress = (
  lucid: LucidEvolution,
  {
    scopeName,
    failureMessage,
    minimumPlainBalance,
  }: {
    readonly scopeName: string;
    readonly failureMessage: string;
    readonly minimumPlainBalance?: bigint;
  },
): Effect.Effect<readonly UTxO[], SDK.StateQueueError> =>
  Effect.gen(function* () {
    const walletAddress = yield* Effect.tryPromise({
      try: () => lucid.wallet().address(),
      catch: (cause) =>
        new SDK.StateQueueError({
          message: `Failed to resolve wallet address while refreshing ${scopeName}`,
          cause,
        }),
    });
    const cachedWalletUtxos = yield* Effect.tryPromise({
      try: () => lucid.wallet().getUtxos(),
      catch: () => [] as readonly UTxO[],
    }).pipe(Effect.catchAll(() => Effect.succeed([] as readonly UTxO[])));

    let refreshedWalletUtxos: readonly UTxO[] | null = null;
    let lastCause: unknown = null;
    for (
      let attempt = 0;
      attempt < WALLET_OWN_ADDRESS_REFRESH_MAX_RETRIES;
      attempt += 1
    ) {
      const atAddressAttempt = yield* Effect.either(
        Effect.tryPromise({
          try: () => lucid.utxosAt(walletAddress),
          catch: (cause) => cause,
        }),
      );
      if (atAddressAttempt._tag === "Right") {
        const mergedWalletUtxos = mergeWalletUtxosPreservingScriptRefs(
          atAddressAttempt.right,
          cachedWalletUtxos,
        );
        yield* Effect.sync(() => lucid.overrideUTxOs([...mergedWalletUtxos]));
        const plainBalance = sumWalletLovelace(
          filterPlainWalletUtxos(mergedWalletUtxos),
        );
        if (
          minimumPlainBalance === undefined ||
          plainBalance >= minimumPlainBalance
        ) {
          refreshedWalletUtxos = mergedWalletUtxos;
          break;
        }
        lastCause = `wallet_address=${walletAddress},plain_balance=${plainBalance.toString()},required_plain_balance=${minimumPlainBalance.toString()}`;
      } else {
        lastCause = atAddressAttempt.left;
      }

      if (attempt + 1 < WALLET_OWN_ADDRESS_REFRESH_MAX_RETRIES) {
        yield* Effect.logWarning(
          `Wallet own-address refresh for ${scopeName} did not reach the required state (attempt ${(attempt + 1).toString()}/${WALLET_OWN_ADDRESS_REFRESH_MAX_RETRIES.toString()}); retrying in ${WALLET_OWN_ADDRESS_REFRESH_RETRY_DELAY}. cause=${String(lastCause)}`,
        );
        yield* Effect.sleep(WALLET_OWN_ADDRESS_REFRESH_RETRY_DELAY);
      }
    }

    if (refreshedWalletUtxos !== null) {
      return refreshedWalletUtxos;
    }
    return yield* Effect.fail(
      new SDK.StateQueueError({
        message: failureMessage,
        cause: lastCause ?? `wallet_address=${walletAddress}`,
      }),
    );
  });

export const resolveSpendableWalletUtxos = (
  lucid: LucidEvolution,
  excludedOutRefKeys: ReadonlySet<string>,
): Effect.Effect<readonly UTxO[], SDK.StateQueueError> =>
  Effect.gen(function* () {
    const walletUtxos = yield* fetchReconciledWalletUtxos(
      lucid,
      "Failed to fetch wallet UTxOs for transaction input preset",
    );
    // Reference-script publications must remain available for `.readFrom(...)`.
    return filterPlainWalletUtxos(walletUtxos).filter(
      (utxo) => !excludedOutRefKeys.has(utxoOutRefKey(utxo)),
    );
  });

const resolveLiveWalletUtxo = (
  lucid: LucidEvolution,
  utxo: UTxO,
): Effect.Effect<UTxO | undefined, SDK.StateQueueError> =>
  Effect.gen(function* () {
    const resolved = yield* Effect.tryPromise({
      try: () =>
        lucid.utxosByOutRef([
          {
            txHash: utxo.txHash,
            outputIndex: utxo.outputIndex,
          },
        ]),
      catch: (cause) =>
        new SDK.StateQueueError({
          message:
            "Failed to resolve wallet UTxO by out-ref while validating script reference",
          cause,
        }),
    });
    const live = resolved.find(
      (candidate) =>
        candidate.txHash === utxo.txHash &&
        candidate.outputIndex === utxo.outputIndex,
    );
    if (live === undefined) {
      return undefined;
    }
    if (live.scriptRef === undefined && utxo.scriptRef !== undefined) {
      return {
        ...live,
        scriptRef: utxo.scriptRef,
      };
    }
    return live;
  });

const resolveExistingReferenceScriptPublication = (
  lucid: LucidEvolution,
  walletUtxos: readonly UTxO[],
  target: ReferenceScriptTarget,
): Effect.Effect<ReferenceScriptPublication | undefined, SDK.StateQueueError> =>
  Effect.gen(function* () {
    const existingCandidates = walletUtxos
      .filter((utxo) => isSameScriptRef(utxo.scriptRef, target.script))
      .sort(compareOutRefs)
      .reverse();
    for (const existingCandidate of existingCandidates) {
      const existing = yield* resolveLiveWalletUtxo(lucid, existingCandidate);
      if (existing !== undefined && isSameScriptRef(existing.scriptRef, target.script)) {
        return {
          name: target.name,
          utxo: existing,
        };
      }
    }
    return undefined;
  });

const ensureReferenceScriptWalletWorkingCapital = (
  fundingLucid: LucidEvolution,
  referenceScriptsLucid: LucidEvolution,
  scopeName: string,
  requiredPlainBalance: bigint,
): Effect.Effect<
  void,
  SDK.StateQueueError | SDK.LucidError | TxSignError | TxSubmitError
> =>
  Effect.gen(function* () {
    const referenceScriptWalletUtxos = yield* refreshWalletUtxosFromOwnAddress(
      referenceScriptsLucid,
      {
        scopeName: `${scopeName} reference scripts`,
        failureMessage:
          `Failed to fetch wallet UTxOs while preparing ${scopeName} reference scripts`,
      },
    );
    const currentPlainBalance = sumWalletLovelace(
      filterPlainWalletUtxos(referenceScriptWalletUtxos),
    );
    const targetPlainBalance =
      requiredPlainBalance > REFERENCE_SCRIPT_WALLET_WORKING_CAPITAL_LOVELACE
        ? requiredPlainBalance
        : REFERENCE_SCRIPT_WALLET_WORKING_CAPITAL_LOVELACE;
    if (currentPlainBalance >= targetPlainBalance) {
      return;
    }

    const referenceScriptAddress = yield* Effect.tryPromise({
      try: () => referenceScriptsLucid.wallet().address(),
      catch: (cause) =>
        new SDK.StateQueueError({
          message: `Failed to resolve reference-script wallet address while preparing ${scopeName} reference scripts`,
          cause,
        }),
    });
    const fundingAddress = yield* Effect.tryPromise({
      try: () => fundingLucid.wallet().address(),
      catch: (cause) =>
        new SDK.StateQueueError({
          message: `Failed to resolve funding wallet address while preparing ${scopeName} reference scripts`,
          cause,
        }),
    });
    if (fundingAddress === referenceScriptAddress) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            `Reference-script wallet plain balance is below the required working-capital floor while preparing ${scopeName} reference scripts`,
          cause: `plain_balance=${currentPlainBalance.toString()},required=${targetPlainBalance.toString()},wallet_address=${referenceScriptAddress},reason=same-wallet-funding-would-risk-scriptref-spend`,
        }),
      );
    }

    const topUpAmount = targetPlainBalance - currentPlainBalance;
    const fundingInputs = yield* resolveSpendableWalletUtxos(
      fundingLucid,
      new Set<string>(),
    );
    if (fundingInputs.length === 0) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            `No operator wallet funding UTxOs available to replenish ${scopeName} reference scripts`,
          cause: `reference_script_wallet=${referenceScriptAddress},required_top_up=${topUpAmount.toString()}`,
        }),
      );
    }
    const selectedFundingInputs = selectWalletFundingUtxos(
      fundingInputs,
      topUpAmount + SCRIPT_REF_PUBLICATION_FUNDING_BUFFER_LOVELACE,
    );
    if (selectedFundingInputs.length === 0) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            `Failed to select operator wallet funding UTxOs to replenish ${scopeName} reference scripts`,
          cause: `reference_script_wallet=${referenceScriptAddress},required_top_up=${topUpAmount.toString()}`,
        }),
      );
    }

    yield* Effect.logInfo(
      `Replenishing reference-script wallet for ${scopeName}: current_plain_balance=${currentPlainBalance.toString()},target_plain_balance=${targetPlainBalance.toString()},top_up_amount=${topUpAmount.toString()},reference_script_wallet=${referenceScriptAddress}`,
    );
    const unsigned = yield* Effect.tryPromise({
      try: () =>
        fundingLucid
          .newTx()
          .collectFrom([...selectedFundingInputs])
          .pay.ToAddress(referenceScriptAddress, {
            lovelace: topUpAmount,
          })
          .complete({
            coinSelection: false,
            localUPLCEval: true,
            presetWalletInputs: [...selectedFundingInputs],
          }),
      catch: (cause) =>
        new SDK.LucidError({
          message: `Failed to build reference-script wallet replenishment transaction for ${scopeName}: ${String(cause)}`,
          cause,
        }),
    });
    const txHash = yield* handleSignSubmit(fundingLucid, unsigned);
    yield* refreshWalletUtxosFromOwnAddress(referenceScriptsLucid, {
      scopeName: `${scopeName} reference scripts after replenishment`,
      failureMessage:
        `Failed to refresh reference-script wallet after replenishing ${scopeName} reference scripts`,
      minimumPlainBalance: targetPlainBalance,
    });
    yield* Effect.logInfo(
      `Reference-script wallet replenishment confirmed for ${scopeName}: txHash=${txHash},top_up_amount=${topUpAmount.toString()}`,
    );
  });

const publishMissingReferenceScriptTargets = (
  lucid: LucidEvolution,
  operatorAddress: string,
  walletUtxos: readonly UTxO[],
  fundingCandidateUtxos: readonly UTxO[],
  missingTargets: readonly ReferenceScriptTarget[],
): Effect.Effect<
  readonly ReferenceScriptPublication[],
  SDK.StateQueueError | SDK.LucidError | TxSignError | TxSubmitError
> =>
  Effect.gen(function* () {
    const orderedFundingCandidates = orderWalletFundingUtxos(
      fundingCandidateUtxos,
    );
    let selectedFundingInputs = selectWalletFundingUtxos(
      fundingCandidateUtxos,
      resolveReferenceScriptPublicationFundingTarget(missingTargets.length),
    );
    if (selectedFundingInputs.length === 0) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Failed to select non-reference wallet UTxOs for reference-script publication",
          cause: missingTargets.map(({ name }) => name).join(","),
        }),
      );
    }
    let nextFundingCandidateIndex = selectedFundingInputs.length;
    const [txHash, localReferenceOutputs, walletOutputs] = yield* Effect.gen(
      function* () {
        while (true) {
          yield* Effect.sync(() => lucid.overrideUTxOs([...selectedFundingInputs]));
          const buildAttempt = yield* Effect.either(
            Effect.gen(function* () {
              let tx = lucid.newTx().collectFrom([...selectedFundingInputs]);
              tx = tx.pay.ToAddressWithData(operatorAddress, undefined, {
                lovelace: SCRIPT_REF_OUTPUT_LOVELACE,
              });
              for (const target of missingTargets) {
                tx = tx.pay.ToAddressWithData(
                  operatorAddress,
                  undefined,
                  { lovelace: SCRIPT_REF_OUTPUT_LOVELACE },
                  target.script,
                );
              }
              const unsigned = yield* Effect.tryPromise({
                try: () =>
                  tx.complete({
                    coinSelection: false,
                    localUPLCEval: true,
                    presetWalletInputs: [...selectedFundingInputs],
                  }),
                catch: (cause) =>
                  new SDK.LucidError({
                    message: `Failed to build reference-script publication transaction for ${missingTargets
                      .map(({ name }) => name)
                      .join(", ")}: ${String(cause)}`,
                    cause,
                  }),
              });
              const publicationTx = unsigned.toTransaction();
              const publicationOutputs = publicationTx.body().outputs();
              const localReferenceOutputs = new Map<
                string,
                Omit<UTxO, "txHash">
              >();
              const walletOutputs: Omit<UTxO, "txHash">[] = [];
              for (
                let outputIndex = 0;
                outputIndex < publicationOutputs.len();
                outputIndex += 1
              ) {
                const output = coreToTxOutput(publicationOutputs.get(outputIndex));
                if (output.address !== operatorAddress) {
                  continue;
                }
                walletOutputs.push({
                  outputIndex,
                  address: output.address,
                  assets: output.assets,
                  datum: output.datum ?? undefined,
                  datumHash: output.datumHash ?? undefined,
                  scriptRef: output.scriptRef ?? undefined,
                });
                if (output.scriptRef === undefined) {
                  continue;
                }
                const matchingTarget = missingTargets.find(
                  (target) =>
                    !localReferenceOutputs.has(target.name) &&
                    isSameScriptRef(output.scriptRef, target.script),
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
              const txHash = yield* handleSignSubmit(lucid, unsigned);
              return [txHash, localReferenceOutputs, walletOutputs] as const;
            }),
          ).pipe(
            Effect.ensuring(
              Effect.sync(() => lucid.overrideUTxOs([...walletUtxos])),
            ),
          );

          if (buildAttempt._tag === "Right") {
            return buildAttempt.right;
          }
          if (
            !isReferenceScriptPublicationBalanceInsufficient(buildAttempt.left) ||
            nextFundingCandidateIndex >= orderedFundingCandidates.length
          ) {
            return yield* Effect.fail(buildAttempt.left);
          }

          const nextFundingInput =
            orderedFundingCandidates[nextFundingCandidateIndex];
          nextFundingCandidateIndex += 1;
          selectedFundingInputs = [
            ...selectedFundingInputs,
            nextFundingInput,
          ];
          yield* Effect.logWarning(
            `Reference-script publication for ${missingTargets
              .map(({ name }) => name)
              .join(", ")} needed more wallet funding than the seed estimate; retrying with ${selectedFundingInputs.length.toString()} funding input(s).`,
          );
        }
      },
    );
    const restoredWalletUtxos = [
      ...walletUtxos.filter(
        (utxo) =>
          !selectedFundingInputs.some(
            (selected) =>
              selected.txHash === utxo.txHash &&
              selected.outputIndex === utxo.outputIndex,
          ),
      ),
      ...walletOutputs.map((output) => ({
        txHash,
        ...output,
      })),
    ];
    yield* Effect.sync(() => lucid.overrideUTxOs(restoredWalletUtxos));
    return yield* Effect.forEach(missingTargets, (target) =>
      Effect.gen(function* () {
        const localReferenceOutput = localReferenceOutputs.get(target.name);
        if (localReferenceOutput === undefined) {
          return yield* Effect.fail(
            new SDK.StateQueueError({
              message:
                "Reference-script publication transaction did not contain the expected script-ref output",
              cause: `${target.name},txHash=${txHash}`,
            }),
          );
        }
        const localReferenceUtxo: UTxO = {
          txHash,
          ...localReferenceOutput,
        };
        const liveReference = yield* Effect.either(
          resolveLiveWalletUtxo(lucid, localReferenceUtxo),
        );
        if (
          liveReference._tag === "Right" &&
          liveReference.right !== undefined &&
          isSameScriptRef(liveReference.right.scriptRef, target.script)
        ) {
          return {
            name: target.name,
            utxo: liveReference.right,
          };
        }
        if (
          liveReference._tag === "Right" &&
          liveReference.right !== undefined
        ) {
          return {
            name: target.name,
            utxo: {
              ...liveReference.right,
              scriptRef: target.script,
            },
          };
        }
        return {
          name: target.name,
          utxo: {
            ...localReferenceUtxo,
            scriptRef: target.script,
          },
        };
      }),
    );
  });

export const nodeRuntimeReferenceScriptTargets = (
  contracts: SDK.MidgardValidators,
): readonly ReferenceScriptTarget[] => [
  {
    name: "hub-oracle minting",
    script: contracts.hubOracle.mintingScript,
  },
  {
    name: "scheduler spending",
    script: contracts.scheduler.spendingScript,
  },
  {
    name: "scheduler minting",
    script: contracts.scheduler.mintingScript,
  },
  {
    name: "state-queue spending",
    script: contracts.stateQueue.spendingScript,
  },
  {
    name: "state-queue minting",
    script: contracts.stateQueue.mintingScript,
  },
  {
    name: "registered-operators spending",
    script: contracts.registeredOperators.spendingScript,
  },
  {
    name: "registered-operators minting",
    script: contracts.registeredOperators.mintingScript,
  },
  {
    name: "active-operators spending",
    script: contracts.activeOperators.spendingScript,
  },
  {
    name: "active-operators minting",
    script: contracts.activeOperators.mintingScript,
  },
  {
    name: "retired-operators spending",
    script: contracts.retiredOperators.spendingScript,
  },
  {
    name: "retired-operators minting",
    script: contracts.retiredOperators.mintingScript,
  },
  {
    name: "fraud-proof-catalogue minting",
    script: contracts.fraudProofCatalogue.mintingScript,
  },
  {
    name: "deposit minting",
    script: contracts.deposit.mintingScript,
  },
  {
    name: "settlement minting",
    script: contracts.settlement.mintingScript,
  },
];

export const referenceScriptTargetsByCommand = (
  contracts: SDK.MidgardValidators,
): Readonly<Record<ReferenceScriptCommandName, readonly ReferenceScriptTarget[]>> => ({
  "node-runtime": nodeRuntimeReferenceScriptTargets(contracts),
  "protocol-init": [
    {
      name: "hub-oracle minting",
      script: contracts.hubOracle.mintingScript,
    },
    {
      name: "scheduler minting",
      script: contracts.scheduler.mintingScript,
    },
    {
      name: "state-queue minting",
      script: contracts.stateQueue.mintingScript,
    },
    {
      name: "registered-operators minting",
      script: contracts.registeredOperators.mintingScript,
    },
    {
      name: "active-operators minting",
      script: contracts.activeOperators.mintingScript,
    },
    {
      name: "retired-operators minting",
      script: contracts.retiredOperators.mintingScript,
    },
    {
      name: "fraud-proof-catalogue minting",
      script: contracts.fraudProofCatalogue.mintingScript,
    },
  ],
  "hub-oracle": [
    {
      name: "hub-oracle minting",
      script: contracts.hubOracle.mintingScript,
    },
  ],
  "state-queue": [
    {
      name: "state-queue spending",
      script: contracts.stateQueue.spendingScript,
    },
    {
      name: "state-queue minting",
      script: contracts.stateQueue.mintingScript,
    },
  ],
  scheduler: [
    {
      name: "scheduler spending",
      script: contracts.scheduler.spendingScript,
    },
    {
      name: "scheduler minting",
      script: contracts.scheduler.mintingScript,
    },
  ],
  "registered-operators": [
    {
      name: "registered-operators spending",
      script: contracts.registeredOperators.spendingScript,
    },
    {
      name: "registered-operators minting",
      script: contracts.registeredOperators.mintingScript,
    },
  ],
  "active-operators": [
    {
      name: "active-operators spending",
      script: contracts.activeOperators.spendingScript,
    },
    {
      name: "active-operators minting",
      script: contracts.activeOperators.mintingScript,
    },
  ],
  "retired-operators": [
    {
      name: "retired-operators spending",
      script: contracts.retiredOperators.spendingScript,
    },
    {
      name: "retired-operators minting",
      script: contracts.retiredOperators.mintingScript,
    },
  ],
  deposit: [
    {
      name: "deposit minting",
      script: contracts.deposit.mintingScript,
    },
  ],
  settlement: [
    {
      name: "settlement minting",
      script: contracts.settlement.mintingScript,
    },
  ],
});

export const ensureReferenceScriptTargetsProgram = (
  referenceScriptsLucid: LucidEvolution,
  scopeName: string,
  targets: readonly ReferenceScriptTarget[],
  fundingLucid: LucidEvolution = referenceScriptsLucid,
): Effect.Effect<
  readonly ReferenceScriptPublication[],
  SDK.StateQueueError | SDK.LucidError | TxSignError | TxSubmitError
> =>
  Effect.gen(function* () {
    const fetchReferenceScriptWalletUtxos = (): Effect.Effect<
      readonly UTxO[],
      SDK.StateQueueError
    > =>
      refreshWalletUtxosFromOwnAddress(
        referenceScriptsLucid,
        {
          scopeName: `${scopeName} reference scripts`,
          failureMessage:
            `Failed to fetch wallet UTxOs while resolving ${scopeName} reference scripts`,
        },
      );

    const publishMissingTargetsInBatches = (
      missingTargets: readonly ReferenceScriptTarget[],
    ): Effect.Effect<
      readonly ReferenceScriptPublication[],
      SDK.StateQueueError | SDK.LucidError | TxSignError | TxSubmitError
    > =>
      Effect.gen(function* () {
        if (missingTargets.length === 0) {
          return [];
        }
        let requiredPlainBalance =
          resolveReferenceScriptPublicationFundingTarget(missingTargets.length);
        while (true) {
          yield* ensureReferenceScriptWalletWorkingCapital(
            fundingLucid,
            referenceScriptsLucid,
            scopeName,
            requiredPlainBalance,
          );
          const walletUtxos = yield* fetchReferenceScriptWalletUtxos();
          const operatorAddress = yield* Effect.tryPromise({
            try: () => referenceScriptsLucid.wallet().address(),
            catch: (cause) =>
              new SDK.StateQueueError({
                message: `Failed to resolve wallet address while creating ${scopeName} reference scripts`,
                cause,
              }),
          });
          if (walletUtxos.length === 0) {
            return yield* Effect.fail(
              new SDK.StateQueueError({
                message:
                  `No wallet UTxOs available while publishing ${scopeName} reference scripts`,
                cause: "wallet-has-no-live-utxos",
              }),
            );
          }
          const plainFundingCandidateUtxos = filterPlainWalletUtxos(walletUtxos);
          if (plainFundingCandidateUtxos.length === 0) {
            return yield* Effect.fail(
              new SDK.StateQueueError({
                message:
                  `No plain wallet UTxOs available while publishing ${scopeName} reference scripts`,
                cause: "wallet-has-no-plain-utxos",
              }),
            );
          }
          const publishAttempt = yield* Effect.either(
            publishMissingReferenceScriptTargets(
              referenceScriptsLucid,
              operatorAddress,
              walletUtxos,
              plainFundingCandidateUtxos,
              missingTargets,
            ),
          );
          if (publishAttempt._tag === "Right") {
            return publishAttempt.right;
          }
          if (
            isReferenceScriptPublicationBalanceInsufficient(publishAttempt.left)
          ) {
            const additionalFunding =
              resolveReferenceScriptPublicationAdditionalFunding(
                publishAttempt.left,
              );
            if (additionalFunding !== undefined) {
              requiredPlainBalance =
                sumWalletLovelace(plainFundingCandidateUtxos) +
                additionalFunding;
              yield* Effect.logWarning(
                `Reference-script publication for ${scopeName} needed more dedicated-wallet funding than the current working-capital floor; retrying after replenishing an additional ${additionalFunding.toString()} lovelace.`,
              );
              continue;
            }
          }
          if (
            missingTargets.length === 1 ||
            !isReferenceScriptPublicationTxTooLarge(publishAttempt.left)
          ) {
            return yield* Effect.fail(publishAttempt.left);
          }
          const splitIndex = Math.ceil(missingTargets.length / 2);
          const leftTargets = missingTargets.slice(0, splitIndex);
          const rightTargets = missingTargets.slice(splitIndex);
          yield* Effect.logWarning(
            `Reference-script publication for ${scopeName} exceeded max tx size; retrying in smaller batches: left=[${leftTargets
              .map(({ name }) => name)
              .join(", ")}], right=[${rightTargets
              .map(({ name }) => name)
              .join(", ")}]`,
          );
          const leftPublications = yield* publishMissingTargetsInBatches(
            leftTargets,
          );
          const rightPublications = yield* publishMissingTargetsInBatches(
            rightTargets,
          );
          return [...leftPublications, ...rightPublications];
        }
      });

    const walletUtxos = yield* fetchReferenceScriptWalletUtxos();
    const existingPublications = yield* Effect.forEach(targets, (target) =>
      resolveExistingReferenceScriptPublication(
        referenceScriptsLucid,
        walletUtxos,
        target,
      ),
    );
    const existingByName = new Map(
      existingPublications
        .filter(
          (
            publication,
          ): publication is ReferenceScriptPublication => publication !== undefined,
        )
        .map((publication) => [publication.name, publication]),
    );
    const missingTargets = targets.filter(
      (target) => !existingByName.has(target.name),
    );
    let createdByName = new Map<string, ReferenceScriptPublication>();
    if (missingTargets.length > 0) {
      createdByName = new Map(
        (
          yield* publishMissingTargetsInBatches(missingTargets)
        ).map((publication) => [publication.name, publication]),
      );
    }
    const resolvedPublications: ReferenceScriptPublication[] = [];
    for (const target of targets) {
      const publication =
        existingByName.get(target.name) ?? createdByName.get(target.name);
      if (publication === undefined) {
        return yield* Effect.fail(
          new SDK.StateQueueError({
            message: "Missing resolved reference script publication",
            cause: `${scopeName}:${target.name}`,
          }),
        );
      }
      resolvedPublications.push(publication);
    }
    return resolvedPublications;
  });

export const deployReferenceScriptCommandProgram = (
  referenceScriptsLucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  commandName: ReferenceScriptCommandName,
  fundingLucid: LucidEvolution = referenceScriptsLucid,
): Effect.Effect<
  readonly ReferenceScriptPublication[],
  SDK.StateQueueError | SDK.LucidError | TxSignError | TxSubmitError
> =>
  ensureReferenceScriptTargetsProgram(
    referenceScriptsLucid,
    commandName,
    referenceScriptTargetsByCommand(contracts)[commandName],
    fundingLucid,
  );

export const ensureNodeRuntimeReferenceScriptsProgram = (
  referenceScriptsLucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  fundingLucid: LucidEvolution = referenceScriptsLucid,
): Effect.Effect<
  readonly ReferenceScriptPublication[],
  SDK.StateQueueError | SDK.LucidError | TxSignError | TxSubmitError
> =>
  ensureReferenceScriptTargetsProgram(
    referenceScriptsLucid,
    "node-runtime",
    nodeRuntimeReferenceScriptTargets(contracts),
    fundingLucid,
  );

export const resolveReferenceScriptTargetsProgram = (
  referenceScriptsLucid: LucidEvolution,
  scopeName: string,
  targets: readonly ReferenceScriptTarget[],
): Effect.Effect<readonly ReferenceScriptPublication[], SDK.StateQueueError> =>
  Effect.gen(function* () {
    const referenceScriptsAddress = yield* Effect.tryPromise({
      try: () => referenceScriptsLucid.wallet().address(),
      catch: (cause) =>
        new SDK.StateQueueError({
          message: `Failed to resolve reference-script wallet address while resolving ${scopeName} reference scripts`,
          cause,
        }),
    });
    return yield* fetchReferenceScriptUtxosProgram(
      referenceScriptsLucid,
      referenceScriptsAddress,
      targets,
    );
  });

export const verifyNodeRuntimeReferenceScriptsProgram = (
  lucid: LucidEvolution,
  referenceScriptsAddress: string,
  contracts: SDK.MidgardValidators,
): Effect.Effect<readonly ReferenceScriptPublication[], SDK.StateQueueError> =>
  Effect.gen(function* () {
    const targets = nodeRuntimeReferenceScriptTargets(contracts);
    const referenceScriptUtxos = yield* Effect.tryPromise({
      try: () => lucid.utxosAt(referenceScriptsAddress),
      catch: (cause) =>
        new SDK.StateQueueError({
          message: `Failed to fetch node-runtime reference-script UTxOs at ${referenceScriptsAddress}`,
          cause,
        }),
    });
    const resolved: ReferenceScriptPublication[] = [];
    const missing: string[] = [];
    for (const target of targets) {
      const utxo = [...referenceScriptUtxos]
        .filter((candidate) => isSameScriptRef(candidate.scriptRef, target.script))
        .sort(compareOutRefs)[0];
      if (utxo === undefined) {
        missing.push(target.name);
      } else {
        resolved.push({ name: target.name, utxo });
      }
    }
    if (missing.length > 0) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message: "Missing node-runtime reference scripts",
          cause: `address=${referenceScriptsAddress};missing=[${missing.join(",")}]`,
        }),
      );
    }
    return resolved;
  });
