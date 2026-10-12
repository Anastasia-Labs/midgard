import * as SDK from "@al-ft/midgard-sdk";
import {
  activateLayoutToLogString,
  nodeKeyEquals,
  orderedNotMemberWitness,
  registerLayoutToLogString,
} from "@al-ft/midgard-sdk";
import { LucidEvolution, toUnit } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  type IntentJournal,
  journaledIntent,
  openPlan,
} from "../services/intent-journal.js";
import { alignedUnixTimeStrictlyAfter } from "../workers/utils/commit-end-time.js";
import {
  OperatorFundingShortfall,
  requireOperatorFundingProgram,
} from "./operators/funding-preflight.js";
import {
  referenceScriptTargetsByCommand,
  resolveReferenceScriptTargetsProgram,
  resolveSpendableWalletUtxos,
  selectWalletFundingUtxos,
  utxoOutRefKey,
} from "./reference-scripts.js";
import {
  ACTIVATION_VALIDITY_WINDOW_MS,
  ACTIVATION_WALLET_FUNDING_TARGET_LOVELACE,
  decodeHubOracleDatum,
  describeUnknownValue,
  fetchHubOracleRefInput,
  fetchNodeSet,
  getOperatorKeyHash,
  linkPointsToKey,
  type OperatorLifecycleMode,
  type OperatorLifecycleTxHashes,
  REGISTERED_ACTIVATION_DELAY_MS,
  REGISTERED_SET_REFRESH_MAX_RETRIES,
  REGISTERED_SET_REFRESH_RETRY_DELAY,
  registeredNodeMatchesOperator,
  summarizeNodeSetForDiagnostics,
  summarizeOnChainScriptFailure,
} from "./register-active-operator.fetch-hub-oracle-ref-input.js";
import {
  describePosixTime,
  OperatorRegistrationRefusal,
  type PermissionlessActivation,
  toLifecycleResult,
} from "./register-active-operator.to-lifecycle-result.js";
import { canActivateRegisteredOperatorImmediately } from "./register-active-operator/activation.js";
import {
  alignUnixTimeMsToSlotBoundary,
  currentTimeMsForLucidOrEmulatorFallback,
  resolveL1NowMsOrRefuse,
} from "./register-active-operator/clock.js";
import {
  handleSignSubmit,
  TxConfirmError,
  TxSignError,
  TxSubmitError,
} from "./utils.js";

export const operatorLifecycleProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  requiredBondLovelace: bigint,
  mode: OperatorLifecycleMode,
  referenceScriptsLucid: LucidEvolution = lucid,
  referenceScriptsAddress?: string,
  permissionlessActivation?: PermissionlessActivation,
): Effect.Effect<
  OperatorLifecycleTxHashes,
  | SDK.StateQueueError
  | SDK.LucidError
  | OperatorFundingShortfall
  | TxConfirmError
  | TxSignError
  | TxSubmitError,
  IntentJournal
> =>
  Effect.gen(function* () {
    if (permissionlessActivation !== undefined && mode !== "activate-only") {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Permissionless operator activation supports only the activate-only lifecycle mode",
          cause: mode,
        }),
      );
    }
    // S5: one plan for the lifecycle, opened before its first L1 read: the
    // hub oracle read here feeds every step.
    const plan = yield* openPlan;
    const operatorKeyHash =
      permissionlessActivation?.operatorKeyHash ??
      (yield* getOperatorKeyHash(lucid));
    const usesWallClockTime = lucid.config().network === "Custom";
    const hubOracleRefInput = yield* fetchHubOracleRefInput(lucid, contracts);
    const hubOracleDatum = yield* decodeHubOracleDatum(hubOracleRefInput);
    yield* Effect.logInfo(
      `Hub oracle policies: registered=${hubOracleDatum.registered_operators},active=${hubOracleDatum.active_operators},retired=${hubOracleDatum.retired_operators}`,
    );
    const policyMismatches: string[] = [];
    if (
      hubOracleDatum.registered_operators !==
      contracts.registeredOperators.policyId
    ) {
      policyMismatches.push(
        `registered(hub=${hubOracleDatum.registered_operators},contracts=${contracts.registeredOperators.policyId})`,
      );
    }
    if (
      hubOracleDatum.active_operators !== contracts.activeOperators.policyId
    ) {
      policyMismatches.push(
        `active(hub=${hubOracleDatum.active_operators},contracts=${contracts.activeOperators.policyId})`,
      );
    }
    if (
      hubOracleDatum.retired_operators !== contracts.retiredOperators.policyId
    ) {
      policyMismatches.push(
        `retired(hub=${hubOracleDatum.retired_operators},contracts=${contracts.retiredOperators.policyId})`,
      );
    }
    if (policyMismatches.length > 0) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Hub oracle policy ids do not match configured contract policy ids; activation would be invalid with mismatched policies",
          cause: policyMismatches.join(","),
        }),
      );
    }
    // `activate-only` must reuse the same published active/registered reference
    // scripts that `register-only` established, otherwise activation can build
    // against a different reference-input layout than registration prepared.
    const operatorReferenceTargets = [
      ...referenceScriptTargetsByCommand(contracts)["registered-operators"],
      ...referenceScriptTargetsByCommand(contracts)["active-operators"],
    ] as const;
    const operatorReferenceScriptRefs =
      mode === "deregister-only"
        ? yield* resolveReferenceScriptTargetsProgram(
            referenceScriptsLucid,
            "registered-operators",
            referenceScriptTargetsByCommand(contracts)["registered-operators"],
            contracts.referenceScriptAuth,
            referenceScriptsAddress,
          )
        : yield* resolveReferenceScriptTargetsProgram(
            referenceScriptsLucid,
            "operator lifecycle",
            operatorReferenceTargets,
            contracts.referenceScriptAuth,
            referenceScriptsAddress,
          );
    const registeredOperatorScriptRefs = operatorReferenceScriptRefs.filter(
      ({ name }) => name.startsWith("registered-operators "),
    );
    const activeOperatorScriptRefs = operatorReferenceScriptRefs.filter(
      ({ name }) => name.startsWith("active-operators "),
    );
    const lifecycleScriptRefs = [
      ...registeredOperatorScriptRefs,
      ...activeOperatorScriptRefs,
    ];
    const lifecycleScriptRefOutRefs = new Set(
      lifecycleScriptRefs.map(({ utxo }) => utxoOutRefKey(utxo)),
    );

    const registeredNodes = yield* fetchNodeSet(
      lucid,
      contracts.registeredOperators.spendingScriptAddress,
      contracts.registeredOperators.policyId,
    );
    const activeNodes = yield* fetchNodeSet(
      lucid,
      contracts.activeOperators.spendingScriptAddress,
      contracts.activeOperators.policyId,
    );
    const retiredNodes = yield* fetchNodeSet(
      lucid,
      contracts.retiredOperators.spendingScriptAddress,
      contracts.retiredOperators.policyId,
    );

    yield* Effect.logInfo(
      `Operator set snapshot: registered(policy=${contracts.registeredOperators.policyId},address=${contracts.registeredOperators.spendingScriptAddress},count=${registeredNodes.length.toString()}),active(policy=${contracts.activeOperators.policyId},address=${contracts.activeOperators.spendingScriptAddress},count=${activeNodes.length.toString()}),retired(policy=${contracts.retiredOperators.policyId},address=${contracts.retiredOperators.spendingScriptAddress},count=${retiredNodes.length.toString()})`,
    );
    yield* Effect.logInfo(
      `Retired set keys: ${retiredNodes
        .map(({ datum }) =>
          datum.key === "Empty" ? "Empty" : datum.key.Key.key,
        )
        .join(",")}`,
    );

    const existingActiveNodes = activeNodes.filter(({ datum }) =>
      nodeKeyEquals(datum, operatorKeyHash),
    );
    if (existingActiveNodes.length > 0) {
      yield* Effect.logInfo(
        `Operator ${operatorKeyHash} is already active; skipping operator lifecycle step for mode=${mode}.`,
      );
      return yield* toLifecycleResult({
        registerTxHash: null,
        activateTxHash: null,
        deregisterTxHash: null,
      });
    }

    let currentRegisteredNodes = registeredNodes;
    let registerTxHash: string | null = null;
    let activateTxHash: string | null = null;
    let deregisterTxHash: string | null = null;

    const existingRegisteredNodes = registeredNodes.filter(({ datum }) =>
      registeredNodeMatchesOperator(datum, operatorKeyHash),
    );
    if (existingRegisteredNodes.length > 1) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Found multiple registered-operator nodes for the current operator key",
          cause: operatorKeyHash,
        }),
      );
    }
    const resumeActivationFromExistingRegistration =
      existingRegisteredNodes.length === 1 && mode === "register-and-activate";

    if (existingRegisteredNodes.length === 1) {
      if (mode === "register-only") {
        yield* Effect.logInfo(
          `Operator ${operatorKeyHash} is already registered; skipping register-only flow.`,
        );
        return yield* toLifecycleResult({
          registerTxHash: null,
          activateTxHash: null,
          deregisterTxHash: null,
        });
      }

      if (mode === "register-and-activate") {
        yield* Effect.logInfo(
          `Operator ${operatorKeyHash} is already registered but not active; resuming with activation without deregistration.`,
        );
      } else if (mode === "deregister-only") {
        const existingRegisteredNode = existingRegisteredNodes[0];
        const existingRegisteredAnchor = currentRegisteredNodes.find(
          ({ datum }) =>
            linkPointsToKey(datum, existingRegisteredNode.datum.key),
        );
        if (existingRegisteredAnchor === undefined) {
          return yield* Effect.fail(
            new SDK.StateQueueError({
              message:
                "Registered-operators anchor node for existing registration was not found",
              cause: operatorKeyHash,
            }),
          );
        }
        yield* Effect.logWarning(
          `Operator ${operatorKeyHash} is registered; executing deregister-only lifecycle step.`,
        );

        const registeredNodeUnit = toUnit(
          contracts.registeredOperators.policyId,
          existingRegisteredNode.assetName,
        );
        const updatedRegisteredAnchorDatumAfterDeregister: SDK.LinkedListNodeView =
          {
            ...existingRegisteredAnchor.datum,
            next: existingRegisteredNode.datum.next,
          };
        yield* requireOperatorFundingProgram(lucid, {
          label: "deregister-operator",
          lockedLovelace: 0n,
        });
        const spendableWalletUtxosForDeregister =
          yield* resolveSpendableWalletUtxos(lucid, lifecycleScriptRefOutRefs);
        const deregisterUnsignedTx = yield* Effect.tryPromise({
          try: () =>
            SDK.buildDeregisterRegisteredOperatorTx({
              lucid,
              contracts,
              operatorKeyHash,
              registeredOperatorScriptRefs,
              registeredNode: existingRegisteredNode,
              registeredAnchor: existingRegisteredAnchor,
              registeredNodeUnit,
              updatedRegisteredAnchorDatum:
                updatedRegisteredAnchorDatumAfterDeregister,
            }).complete({
              localUPLCEval: true,
              presetWalletInputs: [...spendableWalletUtxosForDeregister],
            }),
          catch: (cause) =>
            new SDK.LucidError({
              message:
                "Failed to build operator deregistration transaction during operator lifecycle flow",
              cause,
            }),
        });
        const deregisterSubmitResult = yield* Effect.either(
          handleSignSubmit(
            lucid,
            deregisterUnsignedTx,
            journaledIntent(
              "deregister",
              `deregister:${operatorKeyHash}`,
              plan,
            ),
          ),
        );
        if (deregisterSubmitResult._tag === "Left") {
          return yield* Effect.fail(deregisterSubmitResult.left);
        }
        deregisterTxHash = deregisterSubmitResult.right;
        let registrationCleared = false;
        for (
          let attempt = 0;
          attempt < REGISTERED_SET_REFRESH_MAX_RETRIES;
          attempt += 1
        ) {
          currentRegisteredNodes = yield* fetchNodeSet(
            lucid,
            contracts.registeredOperators.spendingScriptAddress,
            contracts.registeredOperators.policyId,
          );
          const remainingRegistrationNodes = currentRegisteredNodes.filter(
            ({ datum }) =>
              registeredNodeMatchesOperator(datum, operatorKeyHash),
          );
          if (remainingRegistrationNodes.length === 0) {
            registrationCleared = true;
            break;
          }
          if (remainingRegistrationNodes.length > 1) {
            return yield* Effect.fail(
              new SDK.StateQueueError({
                message:
                  "Found multiple registered-operator nodes after deregistration step",
                cause: operatorKeyHash,
              }),
            );
          }
          if (attempt + 1 < REGISTERED_SET_REFRESH_MAX_RETRIES) {
            yield* Effect.sleep(REGISTERED_SET_REFRESH_RETRY_DELAY);
          }
        }
        if (!registrationCleared) {
          return yield* Effect.fail(
            new SDK.StateQueueError({
              message:
                "Deregistration step did not clear the operator node from registered set",
              cause: operatorKeyHash,
            }),
          );
        }
      }
    }

    const postRefreshRegisteredNodes = currentRegisteredNodes.filter(
      ({ datum }) => registeredNodeMatchesOperator(datum, operatorKeyHash),
    );
    if (postRefreshRegisteredNodes.length > 1) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Found multiple registered-operator nodes after registration refresh",
          cause: operatorKeyHash,
        }),
      );
    }
    if (mode === "deregister-only") {
      if (postRefreshRegisteredNodes.length === 0) {
        return yield* toLifecycleResult({
          registerTxHash: null,
          activateTxHash: null,
          deregisterTxHash,
        });
      }
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Deregister-only flow expected the operator node to be removed from registered set",
          cause: operatorKeyHash,
        }),
      );
    }

    if (mode === "activate-only" || resumeActivationFromExistingRegistration) {
      if (postRefreshRegisteredNodes.length === 1) {
        yield* Effect.logInfo(
          `${mode} flow found pre-existing registration node for operator ${operatorKeyHash}.`,
        );
      } else {
        return yield* Effect.fail(
          new OperatorRegistrationRefusal({
            message: `${mode} flow found no registered node for operator ${operatorKeyHash}; run register-operator first`,
            cause: operatorKeyHash,
          }),
        );
      }
    } else if (postRefreshRegisteredNodes.length === 1) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Registration refresh did not clear existing operator node from registered set",
          cause: operatorKeyHash,
        }),
      );
    }

    if (
      postRefreshRegisteredNodes.length === 0 &&
      mode !== "activate-only" &&
      !resumeActivationFromExistingRegistration
    ) {
      // On-chain, `RegisterOperator` proves non-membership of the active and
      // retired lists only — never of the registered list — so a second
      // registration for the same key is a transaction the ledger accepts and
      // `SlashDuplicateOperator` then punishes by taking the bond. Refuse
      // locally, naming the membership that already exists, before spending
      // anything. This also covers the retired list, which the skip checks
      // above do not look at.
      yield* Effect.try({
        try: () =>
          SDK.assertOperatorNotInDirectory(
            {
              registered: currentRegisteredNodes,
              active: activeNodes,
              retired: retiredNodes,
            },
            operatorKeyHash,
          ),
        catch: (cause) =>
          new OperatorRegistrationRefusal({
            message:
              cause instanceof Error
                ? cause.message
                : `Operator ${operatorKeyHash} is already in the operator directory`,
            cause,
          }),
      });

      const registeredRootNode = currentRegisteredNodes.find(
        ({ datum }) => datum.key === "Empty",
      );
      if (registeredRootNode === undefined) {
        return yield* Effect.fail(
          new SDK.StateQueueError({
            message: "Registered-operators root node is missing",
            cause: `policy=${contracts.registeredOperators.policyId}`,
          }),
        );
      }

      const activeNotMemberWitness = activeNodes.find(({ datum }) =>
        orderedNotMemberWitness(datum, operatorKeyHash),
      );
      if (activeNotMemberWitness === undefined) {
        return yield* Effect.fail(
          new SDK.StateQueueError({
            message:
              "Failed to find active-operators witness node proving non-membership for registration",
            cause: operatorKeyHash,
          }),
        );
      }
      const retiredNotMemberWitness = retiredNodes.find(({ datum }) =>
        orderedNotMemberWitness(datum, operatorKeyHash),
      );
      if (retiredNotMemberWitness === undefined) {
        return yield* Effect.fail(
          new SDK.StateQueueError({
            message:
              "Failed to find retired-operators witness node proving non-membership for registration",
            cause: operatorKeyHash,
          }),
        );
      }

      const registerBuildTime = currentTimeMsForLucidOrEmulatorFallback(lucid);
      const registerValidTo = alignUnixTimeMsToSlotBoundary(
        lucid,
        registerBuildTime + ACTIVATION_VALIDITY_WINDOW_MS,
      );
      const registrationTime =
        registerValidTo - 1n + REGISTERED_ACTIVATION_DELAY_MS;
      const registrationNodeKey =
        SDK.posixTimeToRegisteredNodeKey(registrationTime);
      const prependedNodeDatum: SDK.LinkedListNodeView = {
        key: { Key: { key: registrationNodeKey } },
        next: registeredRootNode.datum.next,
        data: SDK.encodeRegisteredOperatorDatumValue(
          operatorKeyHash,
        ) as SDK.LinkedListNodeView["data"],
      };
      const updatedRegisteredRootDatum: SDK.LinkedListNodeView = {
        ...registeredRootNode.datum,
        next: { Key: { key: registrationNodeKey } },
      };

      const registeredNodeUnit = toUnit(
        contracts.registeredOperators.policyId,
        SDK.REGISTERED_OPERATOR_NODE_ASSET_NAME_PREFIX + registrationNodeKey,
      );
      const registerMintAssets = {
        [registeredNodeUnit]: 1n,
      };
      yield* requireOperatorFundingProgram(lucid, {
        label: "register-operator",
        lockedLovelace: requiredBondLovelace,
        feeHeadroomLovelace: ACTIVATION_WALLET_FUNDING_TARGET_LOVELACE,
      });
      const spendableWalletUtxosForRegister =
        yield* resolveSpendableWalletUtxos(lucid, lifecycleScriptRefOutRefs);
      const registerFundingInputs = selectWalletFundingUtxos(
        spendableWalletUtxosForRegister,
        requiredBondLovelace + ACTIVATION_WALLET_FUNDING_TARGET_LOVELACE,
      );
      if (registerFundingInputs.length === 0) {
        return yield* Effect.fail(
          new SDK.StateQueueError({
            message:
              "Failed to select wallet funding UTxOs for registration transaction",
            cause: operatorKeyHash,
          }),
        );
      }
      const prependedNodeAssets = {
        lovelace: requiredBondLovelace,
        [registeredNodeUnit]: 1n,
      };
      /**
       * Builds the registration transaction for an active operator.
       */
      let registerLayout: SDK.RegisterRedeemerLayout | undefined;
      const registerTxConfigBase = {
        lucid,
        contracts,
        operatorKeyHash,
        registeredOperatorScriptRefs,
        hubOracleRefInput,
        activeNotMemberWitness,
        retiredNotMemberWitness,
        registeredRootNode,
        registerFundingInputs,
        registerMintAssets,
        prependedNodeDatum,
        prependedNodeAssets,
        updatedRegisteredRootDatum,
        registerValidTo,
      };
      const mkRegisterTx = (layout?: SDK.RegisterRedeemerLayout) =>
        SDK.buildRegisterOperatorTx({
          ...registerTxConfigBase,
          layout,
          onLayout: (resolvedLayout) => {
            registerLayout = resolvedLayout;
          },
        });

      yield* Effect.logInfo(
        [
          "Register witnesses:",
          `hub=${hubOracleRefInput.txHash}#${hubOracleRefInput.outputIndex.toString()}`,
          `active=${activeNotMemberWitness.utxo.txHash}#${activeNotMemberWitness.utxo.outputIndex.toString()}:${activeNotMemberWitness.assetName}`,
          `retired=${retiredNotMemberWitness.utxo.txHash}#${retiredNotMemberWitness.utxo.outputIndex.toString()}:${retiredNotMemberWitness.assetName}`,
          `valid_to=${registerValidTo.toString()}`,
          `registration_time=${registrationTime.toString()}`,
          `prepended_node_datum=${SDK.encodeLinkedListNodeView(prependedNodeDatum)}`,
        ].join(" "),
      );
      yield* Effect.tryPromise({
        try: () =>
          mkRegisterTx().complete({
            localUPLCEval: true,
            presetWalletInputs: [...registerFundingInputs],
          }),
        catch: (cause) =>
          new SDK.LucidError({
            message: [
              "Failed to build operator registration transaction with final redeemer context.",
              `cause=${String(cause)}`,
            ].join(" "),
            cause,
          }),
      });
      if (registerLayout === undefined) {
        return yield* Effect.fail(
          new SDK.StateQueueError({
            message:
              "BuildTxWithRedeemer did not resolve operator register layout",
            cause: operatorKeyHash,
          }),
        );
      }
      const resolvedRegisterLayout = registerLayout;
      yield* Effect.logInfo(
        `Using register redeemer layout: ${registerLayoutToLogString(resolvedRegisterLayout)}`,
      );
      const registerUnsignedTx = yield* Effect.tryPromise({
        try: () =>
          mkRegisterTx(resolvedRegisterLayout).complete({
            localUPLCEval: true,
            presetWalletInputs: [...registerFundingInputs],
          }),
        catch: (cause) =>
          new SDK.LucidError({
            message: [
              "Failed to rebuild operator registration transaction with resolved redeemer context.",
              `cause=${String(cause)}`,
            ].join(" "),
            cause,
          }),
      });
      registerTxHash = yield* handleSignSubmit(
        lucid,
        registerUnsignedTx,
        journaledIntent("register", `register:${operatorKeyHash}`, plan),
        {
          label: "operator registration",
          requiredOutputIndexes: [
            Number(resolvedRegisterLayout.prependedNodeOutputIndex),
            Number(resolvedRegisterLayout.anchorNodeOutputIndex),
          ],
        },
      );
      let refreshedRegisteredNodeSet = false;
      for (
        let attempt = 0;
        attempt < REGISTERED_SET_REFRESH_MAX_RETRIES;
        attempt += 1
      ) {
        currentRegisteredNodes = yield* fetchNodeSet(
          lucid,
          contracts.registeredOperators.spendingScriptAddress,
          contracts.registeredOperators.policyId,
        );
        const operatorNodeVisible = currentRegisteredNodes.some(({ datum }) =>
          registeredNodeMatchesOperator(datum, operatorKeyHash),
        );
        if (operatorNodeVisible) {
          refreshedRegisteredNodeSet = true;
          break;
        }
        if (attempt + 1 < REGISTERED_SET_REFRESH_MAX_RETRIES) {
          yield* Effect.sleep(REGISTERED_SET_REFRESH_RETRY_DELAY);
        }
      }
      if (!refreshedRegisteredNodeSet) {
        const diagnostics = {
          operatorKeyHash,
          nodeCount: currentRegisteredNodes.length,
          nodes: summarizeNodeSetForDiagnostics(currentRegisteredNodes),
        };
        yield* Effect.logWarning(
          `Registered set refresh diagnostics: ${describeUnknownValue(diagnostics)}`,
        );
        return yield* Effect.fail(
          new SDK.StateQueueError({
            message:
              "Registered set refresh did not expose the operator node after successful registration",
            cause: describeUnknownValue(diagnostics),
          }),
        );
      }
    }

    if (mode === "register-only") {
      return yield* toLifecycleResult({
        registerTxHash,
        activateTxHash: null,
        deregisterTxHash,
      });
    }

    const registeredNode = currentRegisteredNodes.find(({ datum }) =>
      registeredNodeMatchesOperator(datum, operatorKeyHash),
    );
    if (registeredNode === undefined) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Operator registration node was not found after registration step",
          cause: operatorKeyHash,
        }),
      );
    }
    const registeredAnchor = currentRegisteredNodes.find(({ datum }) =>
      linkPointsToKey(datum, registeredNode.datum.key),
    );
    if (registeredAnchor === undefined) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Registered-operators anchor node for activation was not found",
          cause: operatorKeyHash,
        }),
      );
    }

    const activeInsertionAnchor = activeNodes.find(({ datum }) =>
      orderedNotMemberWitness(datum, operatorKeyHash),
    );
    if (activeInsertionAnchor === undefined) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Failed to find active-operators ordered insertion anchor for activation",
          cause:
            "No active-operators node proves strict ordered non-membership for the operator key",
        }),
      );
    }
    const retiredNotMemberWitnessForActivate = retiredNodes.find(({ datum }) =>
      orderedNotMemberWitness(datum, operatorKeyHash),
    );
    if (retiredNotMemberWitnessForActivate === undefined) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Failed to find retired-operators witness node proving non-membership for activation",
          cause: operatorKeyHash,
        }),
      );
    }

    const activationTime = SDK.registeredNodeKeyToPosixTime(
      registeredNode.datum.key,
    );
    if (activationTime === undefined) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Failed to decode registered-operator activation time from registration node key",
          cause: describeUnknownValue(registeredNode.datum.key),
        }),
      );
    }
    const immediateActivation = canActivateRegisteredOperatorImmediately(
      activeInsertionAnchor,
      registeredNode.datum,
      contracts.activeOperators,
    );
    const initialNow = yield* resolveL1NowMsOrRefuse(lucid, "activation");
    if (!immediateActivation && initialNow < activationTime) {
      // `activate-only` is an operator verb: refuse now and name the time,
      // like the other time-gated verbs, instead of holding the process.
      if (mode === "activate-only") {
        return yield* Effect.fail(
          new OperatorRegistrationRefusal({
            message: `Operator ${operatorKeyHash} cannot be activated before its activation time ${describePosixTime(activationTime)}; the chain time is ${describePosixTime(initialNow)}`,
            cause: {
              activationTime: activationTime.toString(),
              nowMs: initialNow.toString(),
            },
          }),
        );
      }
      // `register-and-activate` has just registered, so its activation time
      // cannot have arrived yet: wait for it on networks whose clock the
      // node can read exactly.
      if (!usesWallClockTime) {
        const waitMs = activationTime - initialNow + 1_000n;
        yield* Effect.logInfo(
          `Waiting ${waitMs.toString()}ms until operator activation time (ledger_now=${initialNow.toString()},activation_time=${activationTime.toString()})`,
        );
        yield* Effect.sleep(Number(waitMs));
      }
    }
    const resolveActivationValidityWindow = (): {
      readonly validFrom: bigint;
      readonly validTo?: bigint;
    } => {
      const currentTime = currentTimeMsForLucidOrEmulatorFallback(lucid);
      const backdatedTime = currentTime - 60_000n;
      const lowerBoundTarget = immediateActivation
        ? backdatedTime > 0n
          ? backdatedTime
          : 0n
        : activationTime;
      // Ordinary activation remains bounded by the registration maturity time.
      // The earliest registration may restore an authenticated empty active set
      // immediately; backdate that lower bound to avoid provider slot races.
      // On custom networks (emulator), avoid an upper bound because wall-clock
      // slot estimation can drift from the emulator ledger tip during long
      // candidate retries and cause false "slot range" submit failures.
      const alignedActivationTime = alignUnixTimeMsToSlotBoundary(
        lucid,
        lowerBoundTarget,
      );
      const validFrom =
        alignedActivationTime >= lowerBoundTarget
          ? alignedActivationTime
          : BigInt(
              alignedUnixTimeStrictlyAfter(lucid, Number(lowerBoundTarget)),
            );
      if (usesWallClockTime) {
        return { validFrom };
      }
      const upperBoundBase = currentTime + ACTIVATION_VALIDITY_WINDOW_MS;
      const validTo = alignUnixTimeMsToSlotBoundary(
        lucid,
        upperBoundBase > validFrom
          ? upperBoundBase
          : validFrom + ACTIVATION_VALIDITY_WINDOW_MS,
      );
      return { validFrom, validTo };
    };

    const registeredNodeUnit = toUnit(
      contracts.registeredOperators.policyId,
      registeredNode.assetName,
    );
    const activeNodeUnit = toUnit(
      contracts.activeOperators.policyId,
      SDK.ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX + operatorKeyHash,
    );
    const transferredOperatorAssets = {
      ...registeredNode.utxo.assets,
      [registeredNodeUnit]: 0n,
      [activeNodeUnit]: 1n,
    };
    delete transferredOperatorAssets[registeredNodeUnit];

    yield* requireOperatorFundingProgram(lucid, {
      label: "activate-operator",
      lockedLovelace: 0n,
      feeHeadroomLovelace: ACTIVATION_WALLET_FUNDING_TARGET_LOVELACE,
    });
    const spendableWalletUtxosForActivation =
      yield* resolveSpendableWalletUtxos(lucid, lifecycleScriptRefOutRefs);
    const activationFundingInputs = selectWalletFundingUtxos(
      spendableWalletUtxosForActivation,
      ACTIVATION_WALLET_FUNDING_TARGET_LOVELACE,
    );
    if (activationFundingInputs.length === 0) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Failed to select wallet funding UTxOs for activation transaction",
          cause: operatorKeyHash,
        }),
      );
    }
    const activationCompleteOptions = (options: {
      readonly localUPLCEval: boolean;
    }) => ({
      ...options,
      presetWalletInputs: [...activationFundingInputs],
    });

    const updatedRegisteredAnchorDatum: SDK.LinkedListNodeView = {
      ...registeredAnchor.datum,
      next: registeredNode.datum.next,
    };
    /**
     * Builds the activation transaction for a registered operator.
     */
    let activateLayout: SDK.ActivateRedeemerLayout | undefined;
    const mkActivateTx = (layout?: SDK.ActivateRedeemerLayout) => {
      const { validFrom, validTo } = resolveActivationValidityWindow();
      return SDK.buildActivateOperatorTx({
        lucid,
        contracts,
        operatorKeyHash,
        registeredOperatorScriptRefs,
        activeOperatorScriptRefs,
        hubOracleRefInput,
        retiredNotMemberWitness: retiredNotMemberWitnessForActivate,
        registeredNode,
        registeredAnchor,
        activeInsertionAnchor,
        activationFundingInputs,
        validFrom,
        validTo,
        registeredNodeUnit,
        activeNodeUnit,
        transferredOperatorAssets,
        updatedRegisteredAnchorDatum,
        requireOperatorSignature: permissionlessActivation === undefined,
        layout,
        onLayout: (layout) => {
          activateLayout = layout;
        },
      });
    };

    yield* Effect.tryPromise({
      try: () =>
        mkActivateTx().complete(
          activationCompleteOptions({ localUPLCEval: true }),
        ),
      catch: (cause) =>
        new SDK.LucidError({
          message: `Failed to build activation transaction with final redeemer context: ${String(cause)}`,
          cause,
        }),
    });
    if (activateLayout === undefined) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "BuildTxWithRedeemer did not resolve operator activate layout",
          cause: operatorKeyHash,
        }),
      );
    }
    const resolvedActivateLayout = activateLayout;
    yield* Effect.logInfo(
      `Using activate redeemer layout: ${activateLayoutToLogString(resolvedActivateLayout)}`,
    );
    const activationUnsignedTx = yield* Effect.tryPromise({
      try: () =>
        mkActivateTx(resolvedActivateLayout).complete(
          activationCompleteOptions({ localUPLCEval: true }),
        ),
      catch: (cause) =>
        new SDK.LucidError({
          message: `Failed to rebuild activation transaction with resolved redeemer context: ${String(cause)}`,
          cause,
        }),
    });
    const activateSubmitResult = yield* Effect.either(
      handleSignSubmit(
        lucid,
        activationUnsignedTx,
        journaledIntent("activate", `activate:${operatorKeyHash}`, plan),
        {
          label: "operator activation",
          requiredOutputIndexes: [
            Number(
              resolvedActivateLayout.activeOperatorsInsertedNodeOutputIndex,
            ),
            Number(resolvedActivateLayout.activeOperatorsAnchorNodeOutputIndex),
            Number(
              resolvedActivateLayout.registeredOperatorsAnchorNodeOutputIndex,
            ),
          ],
        },
      ),
    );
    if (activateSubmitResult._tag === "Left") {
      const onChainFailureSummary = summarizeOnChainScriptFailure(
        activateSubmitResult.left.cause,
      );
      if (onChainFailureSummary !== null) {
        yield* Effect.logWarning(
          `Activation submission on-chain failure summary: ${onChainFailureSummary}`,
        );
      }
      return yield* Effect.fail(activateSubmitResult.left);
    }
    activateTxHash = activateSubmitResult.right;

    return yield* toLifecycleResult({
      registerTxHash,
      activateTxHash,
      deregisterTxHash,
    });
  });
