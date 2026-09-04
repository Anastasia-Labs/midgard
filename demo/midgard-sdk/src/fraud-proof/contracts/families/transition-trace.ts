import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data, Network } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  AuthenticatedValidator,
  MintingValidator,
  SpendingValidator,
} from "../../../common.js";
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

export const TRANSITION_TRACE_FAULT_PROOF_TITLES = {
  route: "fraud_proofs/transition_trace/route_v1.main.spend",
  control: "fraud_proofs/transition_trace/control_v1.main.spend",
  source: "fraud_proofs/transition_trace/source_v1.main.spend",
  withdrawal: "fraud_proofs/transition_trace/withdrawal_v1.main.spend",
  forced: "fraud_proofs/transition_trace/forced_v1.main.spend",
  accepted: "fraud_proofs/transition_trace/accepted_transaction_v1.main.spend",
  deposit: "fraud_proofs/transition_trace/deposit_v1.main.spend",
  l1Event: "fraud_proofs/transition_trace/l1_event_v1.main.spend",
  duplicate: "fraud_proofs/transition_trace/duplicate_v1.main.spend",
} as const;

export type TransitionTraceFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly transitionTrace: FraudProofChain & {
    readonly route: SpendingValidator;
    readonly finals: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
    readonly steps: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
  };
};

export type BuildTransitionTraceFaultProofContractsParams =
  BuildFaultProofContractsParams;

export const buildTransitionTraceChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
}: {
  readonly blueprint: FaultProofBlueprint;
  readonly network: Network;
  readonly hubOraclePolicyId: string;
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fraudProofTokenAddressData: Data;
}): Effect.Effect<
  TransitionTraceFaultProofContracts["transitionTrace"],
  Error
> =>
  Effect.gen(function* () {
    const finalSpecs = [
      ["control", false],
      ["source", false],
      ["withdrawal", false],
      ["forced", false],
      ["accepted", false],
      ["deposit", true],
      ["l1Event", true],
      ["duplicate", false],
    ] as const;
    const builtFinals: SpendingValidator[] = [];
    for (const [name, needsHub] of finalSpecs) {
      builtFinals.push(
        yield* tryBuild(
          `Failed to build transition-trace ${name} final validator`,
          () =>
            makeSpendingValidator(
              network,
              applyBlueprintParams(
                blueprint,
                TRANSITION_TRACE_FAULT_PROOF_TITLES[name],
                [
                  computationThread.policyId,
                  fraudProof.policyId,
                  fraudProofTokenAddressData,
                  ...(needsHub ? [hubOraclePolicyId] : []),
                ],
              ),
            ),
        ),
      );
    }
    const finals = [
      builtFinals[0]!,
      builtFinals[1]!,
      builtFinals[2]!,
      builtFinals[3]!,
      builtFinals[4]!,
      builtFinals[5]!,
      builtFinals[6]!,
      builtFinals[7]!,
    ] as const;
    if (
      new Set(finals.map(({ spendingScriptHash }) => spendingScriptHash))
        .size !== finals.length
    ) {
      return yield* Effect.fail(
        new Error("Transition-trace final validator hashes must be distinct"),
      );
    }
    const finalHashesSchema = Data.Array(Data.Bytes());
    type FinalHashes = Data.Static<typeof finalHashesSchema>;
    const FinalHashes = asDataType<FinalHashes>(finalHashesSchema);
    const finalHashesData = Data.from(
      Data.to(
        finals.map(({ spendingScriptHash }) => spendingScriptHash),
        FinalHashes,
      ),
    );
    const route = yield* tryBuild(
      "Failed to build transition-trace route validator",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            TRANSITION_TRACE_FAULT_PROOF_TITLES.route,
            [finalHashesData, computationThread.policyId],
          ),
        ),
    );

    return {
      firstStep: route,
      route,
      finals,
      steps: [route, ...finals],
    };
  });

export const buildTransitionTraceFaultProofContracts = (
  params: BuildTransitionTraceFaultProofContractsParams,
): Effect.Effect<TransitionTraceFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const transitionTrace = yield* buildTransitionTraceChain({
      ...params,
      ...shared,
    });
    return {
      computationThread: shared.computationThread,
      fraudProof: shared.fraudProof,
      transitionTrace,
    };
  });
