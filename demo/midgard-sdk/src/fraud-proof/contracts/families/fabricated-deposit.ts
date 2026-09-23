import { Data, Network } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  AddressData,
  addressDataFromBech32,
  AuthenticatedValidator,
  MintingValidator,
  SpendingValidator,
} from "../../../common.js";
import { applyEventHistoryRetentionValidator } from "../../../user-events/history-data.js";
import { type EventHistoryPayloadBounds } from "../../../user-events/history-payload.js";
import {
  applyBlueprintParams,
  type FaultProofBlueprint,
  makeSpendingValidator,
  tryBuild,
} from "../blueprint.js";
import { buildSharedFaultProofContracts } from "../shared.js";
import {
  type BuildFaultProofContractsParams,
  type FraudProofChain,
} from "../types.js";

export const FABRICATED_DEPOSIT_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/fabricated_deposit/step_01.main.spend",
  step02: "fraud_proofs/fabricated_deposit/step_02.main.spend",
  step03: "fraud_proofs/fabricated_deposit/step_03.main.spend",
  step04: "fraud_proofs/fabricated_deposit/step_04.main.spend",
} as const;

/**
 * Q39 `fabricated-deposit`: a committed `deposits_root` leaf that is not the
 * authentic L1 deposit event pair.
 */
export type FabricatedDepositFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fabricatedDeposit: FraudProofChain & {
    readonly history: EventHistoryPayloadBounds & {
      readonly retentionAddress: string;
    };
    readonly steps: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
  };
};

export type BuildFabricatedDepositFaultProofContractsParams =
  BuildFaultProofContractsParams & {
    readonly eventHistoryBounds: EventHistoryPayloadBounds;
  };

export const buildFabricatedDepositChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
  eventHistoryBounds,
}: {
  readonly eventHistoryBounds: EventHistoryPayloadBounds;
  readonly blueprint: FaultProofBlueprint;
  readonly network: Network;
  readonly hubOraclePolicyId: string;
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fraudProofTokenAddressData: Data;
}): Effect.Effect<
  FabricatedDepositFaultProofContracts["fabricatedDeposit"],
  Error
> =>
  Effect.gen(function* () {
    const retention = yield* tryBuild(
      "Failed to apply deposit history retention",
      () =>
        applyEventHistoryRetentionValidator(
          blueprint,
          network,
          hubOraclePolicyId,
          "Deposit",
        ),
    );
    const retentionAddress = yield* addressDataFromBech32(retention.address);
    if (
      eventHistoryBounds.inlineLimitBytes <= 0n ||
      eventHistoryBounds.maxPayloadBytes <
        eventHistoryBounds.inlineLimitBytes ||
      eventHistoryBounds.maxPayloadNodes <= 0n
    ) {
      return yield* Effect.fail(
        new Error("History proofs require explicit measured payload bounds"),
      );
    }
    const step04 = yield* tryBuild(
      "Failed to build fabricated-deposit step 04",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            FABRICATED_DEPOSIT_FAULT_PROOF_TITLES.step04,
            [
              fraudProof.policyId,
              fraudProofTokenAddressData,
              computationThread.policyId,
            ],
          ),
        ),
    );

    const step03 = yield* tryBuild(
      "Failed to build fabricated-deposit step 03",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            FABRICATED_DEPOSIT_FAULT_PROOF_TITLES.step03,
            [step04.spendingScriptHash, computationThread.policyId],
          ),
        ),
    );

    const step02 = yield* tryBuild(
      "Failed to build fabricated-deposit step 02",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            FABRICATED_DEPOSIT_FAULT_PROOF_TITLES.step02,
            [
              step03.spendingScriptHash,
              computationThread.policyId,
              hubOraclePolicyId,
              Data.from(Data.to(retentionAddress, AddressData)),
              eventHistoryBounds.inlineLimitBytes,
              eventHistoryBounds.maxPayloadBytes,
              eventHistoryBounds.maxPayloadNodes,
            ],
          ),
        ),
    );

    const step01 = yield* tryBuild(
      "Failed to build fabricated-deposit step 01",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            FABRICATED_DEPOSIT_FAULT_PROOF_TITLES.step01,
            [
              step02.spendingScriptHash,
              computationThread.policyId,
              hubOraclePolicyId,
            ],
          ),
        ),
    );

    return {
      history: { ...eventHistoryBounds, retentionAddress: retention.address },
      firstStep: step01,
      steps: [step01, step02, step03, step04],
    };
  });

export const buildFabricatedDepositFaultProofContracts = (
  params: BuildFabricatedDepositFaultProofContractsParams,
): Effect.Effect<FabricatedDepositFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const fabricatedDeposit = yield* buildFabricatedDepositChain({
      ...params,
      ...shared,
    });
    return {
      computationThread: shared.computationThread,
      fraudProof: shared.fraudProof,
      fabricatedDeposit,
    };
  });
