import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data, Network } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  AuthenticatedValidator,
  MintingValidator,
  SpendingValidator,
  WithdrawalValidator,
} from "../../../common.js";
import {
  applyBlueprintParams,
  type FaultProofBlueprint,
  makeSpendingValidator,
  makeWithdrawalValidator,
  tryBuild,
} from "../blueprint.js";
import { buildSharedFaultProofContracts } from "../shared.js";
import {
  type BuildFaultProofContractsParams,
  type FraudProofChain,
} from "../types.js";

/**
 * The route-index domain `validators/fraud-proofs/transition-trace/route-v1.ak`
 * dispatches over: one entry per final validator, in
 * {@link TRANSITION_TRACE_FAULT_PROOF_TITLES} final order.
 */
export const TRANSITION_TRACE_ROUTE_FINAL_COUNT = 8;

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

export const TRANSITION_TRACE_YIELD_TITLES = {
  l2Scan: "fraud_proofs/transition_trace/output_scan.scan_output.withdraw",
  l2Value: "fraud_proofs/transition_trace/output_value.value_output.withdraw",
  depositScan: "fraud_proofs/transition_trace/output_scan.scan_output.withdraw",
  depositValue:
    "fraud_proofs/transition_trace/deposit_value.value_output.withdraw",

  l2Open:
    "fraud_proofs/transition_trace/accepted_transaction_yields.l2_open.withdraw",
  l2Summaries:
    "fraud_proofs/transition_trace/output_summaries.summaries.withdraw",
  l2Assembly: "fraud_proofs/transition_trace/output_assembly.assembly.withdraw",
  depositReplay:
    "fraud_proofs/transition_trace/accepted_transaction_yields.l2_replay.withdraw",
  depositAssembly:
    "fraud_proofs/transition_trace/output_assembly.assembly.withdraw",
  l2Replay:
    "fraud_proofs/transition_trace/accepted_transaction_yields.l2_replay.withdraw",
  claimStructure:
    "fraud_proofs/transition_trace/accepted_transaction_yields.claim_structure.withdraw",
  claimSource:
    "fraud_proofs/transition_trace/accepted_transaction_yields.claim_source.withdraw",
  claimEndpoints:
    "fraud_proofs/transition_trace/accepted_transaction_yields.claim_endpoints.withdraw",
  depositProjection:
    "fraud_proofs/transition_trace/deposit_yields.projection.withdraw",
  depositSummaries:
    "fraud_proofs/transition_trace/deposit_summaries.summaries.withdraw",
} as const;

export type TransitionTraceFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly transitionTrace: FraudProofChain & {
    readonly route: SpendingValidator;
    readonly yields: Readonly<
      Record<keyof typeof TRANSITION_TRACE_YIELD_TITLES, WithdrawalValidator>
    >;
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
  BuildFaultProofContractsParams & {
    readonly referenceScriptAuthPolicyId: string;
  };

export const buildTransitionTraceChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  referenceScriptAuthPolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
}: {
  readonly blueprint: FaultProofBlueprint;
  readonly network: Network;
  readonly hubOraclePolicyId: string;
  readonly referenceScriptAuthPolicyId: string;
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
      ["deposit", false],
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
                  ...(name === "accepted" || name === "deposit"
                    ? [referenceScriptAuthPolicyId]
                    : needsHub
                      ? [hubOraclePolicyId]
                      : []),
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
    // `route_v1` selects a final validator by route index over this list and
    // used to re-check `list.length == 8` on every execution. Deployment
    // parameterization is trusted on chain, so the cardinality is pinned here.
    const finalCount: number = finals.length;
    if (finalCount !== TRANSITION_TRACE_ROUTE_FINAL_COUNT) {
      return yield* Effect.fail(
        new Error(
          `Transition-trace route dispatches over ${TRANSITION_TRACE_ROUTE_FINAL_COUNT.toString()} ` +
            `final validators but ${finalCount.toString()} were deployed`,
        ),
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

    const buildYield = (
      name: keyof typeof TRANSITION_TRACE_YIELD_TITLES,
      parameters: Data[],
    ) =>
      tryBuild(`Failed to build transition-trace ${name} yield`, () =>
        makeWithdrawalValidator(
          applyBlueprintParams(
            blueprint,
            TRANSITION_TRACE_YIELD_TITLES[name],
            parameters,
          ),
        ),
      );
    const acceptedHash = finals[4].spendingScriptHash;
    const depositHash = finals[5].spendingScriptHash;
    const l2Open = yield* buildYield("l2Open", [acceptedHash]);
    const depositProjection = yield* buildYield("depositProjection", [
      depositHash,
      hubOraclePolicyId,
    ]);
    const l2Summaries = yield* buildYield("l2Summaries", [acceptedHash]);
    const depositSummaries = yield* buildYield("depositSummaries", [
      depositHash,
    ]);
    const yields = {
      l2Scan: yield* buildYield("l2Scan", [acceptedHash]),
      l2Value: yield* buildYield("l2Value", [acceptedHash]),
      depositScan: yield* buildYield("depositScan", [depositHash]),
      depositValue: yield* buildYield("depositValue", [depositHash]),

      l2Open,
      l2Summaries,
      l2Assembly: yield* buildYield("l2Assembly", [acceptedHash]),
      l2Replay: yield* buildYield("l2Replay", [acceptedHash]),
      claimStructure: yield* buildYield("claimStructure", [acceptedHash]),
      claimSource: yield* buildYield("claimSource", [acceptedHash]),
      claimEndpoints: yield* buildYield("claimEndpoints", [acceptedHash]),
      depositProjection,
      depositSummaries,
      depositReplay: yield* buildYield("depositReplay", [depositHash]),
      depositAssembly: yield* buildYield("depositAssembly", [depositHash]),
    };
    return {
      firstStep: route,
      yields,
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
