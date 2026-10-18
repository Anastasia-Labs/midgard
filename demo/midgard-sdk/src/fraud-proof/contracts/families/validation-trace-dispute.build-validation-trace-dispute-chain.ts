import { type L1SlotConfig } from "@al-ft/midgard-core/l1-slot-config";
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { type OutRefLike } from "@al-ft/midgard-core/out-ref";
import { Data, Network } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { MintingValidator, SpendingValidator } from "../../../common.js";
import {
  applyBlueprintParams,
  applyThreadHolderParams,
  declaredParameters,
  deriveValidationTraceDeploymentId,
  type FaultProofBlueprint,
  getBlueprintValidator,
  getUnappliedScript,
  makeSpendingValidator,
  makeWithdrawalValidator,
  tryBuild,
} from "../blueprint.js";
import {
  buildCekContextTail,
  completeCekContextStages,
} from "../cek-context.js";
import { buildCekCoreStages, cekCoreEntryHashes } from "../cek-core.js";
import {
  VALIDATION_TRACE_DISPUTE_STEP_COUNT,
  VALIDATION_TRACE_RESOLVER_COUNT,
} from "../titles.js";
import {
  buildCekRedeemerItemStages,
  buildScriptSourcesRedeemerItemStages,
} from "./shared-redeemer-item.js";
import { buildValidationCaseContracts } from "./validation-trace-dispute.case-contracts.js";
import { type ValidationTraceDisputeFaultProofContracts } from "./validation-trace-dispute.types.js";
import { VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES as validationTraceTitles } from "./validation-trace-dispute.validation-trace-dispute-fault-proof-titles.js";
import { VALIDATION_TRACE_SEMANTIC_RESOLVER_GROUP_SIZES } from "./validation-trace-dispute.validation-trace-semantic-resolver-group-sizes.js";

export const buildValidationTraceDisputeChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  fraudProofCataloguePolicyId,
  computationThread,
  fieldPreimageCertificatePolicyId,
  scriptDecodePolicyId,
  referenceScriptAuthPolicyId,
  l1SlotConfig,
  disputeBondLovelace,
  caseConfigInitializationNonce,
}: {
  readonly blueprint: FaultProofBlueprint;
  readonly network: Network;
  readonly hubOraclePolicyId: string;
  readonly fraudProofCataloguePolicyId: string;
  readonly computationThread: MintingValidator;
  readonly fieldPreimageCertificatePolicyId: string;
  readonly scriptDecodePolicyId: string;
  readonly referenceScriptAuthPolicyId: string;
  readonly l1SlotConfig: L1SlotConfig;
  readonly disputeBondLovelace: bigint;
  readonly caseConfigInitializationNonce: OutRefLike;
}): Effect.Effect<
  ValidationTraceDisputeFaultProofContracts["validationTraceDispute"],
  Error
> =>
  Effect.gen(function* () {
    // The case and its hand-off scripts. From Defend on, a thread carries a
    // token of the case policy, so every validator a thread passes through
    // takes the case policy as its thread policy. Only the opener and the
    // source still hold computation-thread tokens.
    const caseContracts = yield* tryBuild(
      "Failed to build validation-case contracts",
      () =>
        buildValidationCaseContracts({
          blueprint,
          network,
          hubOraclePolicyId,
          computationThreadPolicyId: computationThread.policyId,
          disputeBondLovelace,
          caseConfigInitializationNonce,
        }),
    );
    const { caseConfig, threadExpiry, award } = caseContracts;
    const caseValidator = caseContracts.case;
    const threadPolicyId = caseValidator.policyId;
    // Every thread holder of the resolution chain releases its thread
    // through the expiry script, which it takes as its last parameter.
    const applyHolder = (title: string, params: readonly Data[]): string =>
      applyThreadHolderParams(
        blueprint,
        title,
        params,
        threadExpiry.withdrawalScriptHash,
      );

    const disputeBond = yield* tryBuild(
      "Failed to build validation-dispute bond validator",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(blueprint, validationTraceTitles.disputeBond, [
            threadPolicyId,
          ]),
        ),
    );

    const cekCoreStages = yield* tryBuild(
      "Failed to build bounded CEK core stages",
      () =>
        buildCekCoreStages(
          blueprint,
          network,
          award.spendingScriptHash,
          threadPolicyId,
          threadExpiry.withdrawalScriptHash,
        ),
    );
    const deploymentId = deriveValidationTraceDeploymentId(
      fraudProofCataloguePolicyId,
    );
    const cekContextTail = yield* tryBuild(
      "Failed to build CEK context return stages",
      () =>
        buildCekContextTail(
          blueprint,
          network,
          award.spendingScriptHash,
          threadPolicyId,
          fieldPreimageCertificatePolicyId,
          l1SlotConfig,
          threadExpiry.withdrawalScriptHash,
        ),
    );
    const cekContextItemStages = yield* tryBuild(
      "Failed to build shared CEK item stages",
      () =>
        buildCekRedeemerItemStages({
          blueprint,
          network,
          computationThreadPolicyId: threadPolicyId,
          threadExpiryScriptHash: threadExpiry.withdrawalScriptHash,
          deploymentId,
          returnScriptHash: cekContextTail.itemReturn.spendingScriptHash,
        }),
    );
    const cekContextStages = yield* tryBuild(
      "Failed to complete CEK context stages",
      () =>
        completeCekContextStages(
          blueprint,
          network,
          cekContextTail,
          cekContextItemStages.entry.spendingScriptHash,
          threadPolicyId,
          threadExpiry.withdrawalScriptHash,
        ),
    );
    const sharedItem = yield* tryBuild(
      "Failed to build shared ScriptSources redeemer item chain",
      () =>
        buildScriptSourcesRedeemerItemStages({
          blueprint,
          network,
          computationThreadPolicyId: threadPolicyId,
          threadExpiryScriptHash: threadExpiry.withdrawalScriptHash,
          deploymentId,
          awardScriptHash: award.spendingScriptHash,
        }),
    );
    const scriptSourcesStageOneRedeemerStages = {
      envelope: sharedItem.entry,
      traversalNormalizer: sharedItem.traversalNormalizer,
      outerNormalizer: sharedItem.outerNormalizer,
      sourceAuthenticator: sharedItem.sourceAuthenticator,
      executors: sharedItem.executors,
      foldMapExecutor: sharedItem.executors[0]!,
      finalizeFrameExecutor: sharedItem.executors[1]!,
      settlement: sharedItem.settlement,
    } as const;

    const semanticTitles = Object.values(validationTraceTitles.semantics);
    const proofItem = yield* tryBuild(
      "Failed to build validation-trace proof-item validator",
      () =>
        makeSpendingValidator(
          network,
          getUnappliedScript(blueprint, validationTraceTitles.proofItem),
        ),
    );
    const canonicalDecodeItemSettlement = yield* tryBuild(
      "Failed to build validation-trace canonical item settlement",
      () =>
        makeSpendingValidator(
          network,
          applyHolder(
            validationTraceTitles.canonicalDecodeItemStages.settlement,
            [award.spendingScriptHash, threadPolicyId],
          ),
        ),
    );
    const canonicalDecodeItemProof = yield* tryBuild(
      "Failed to build validation-trace canonical item proof verifier",
      () =>
        makeSpendingValidator(
          network,
          applyHolder(validationTraceTitles.canonicalDecodeItemStages.proof, [
            canonicalDecodeItemSettlement.spendingScriptHash,
            threadPolicyId,
          ]),
        ),
    );
    const canonicalDecodeItemObserve = yield* tryBuild(
      "Failed to build validation-trace canonical item observer",
      () =>
        makeSpendingValidator(
          network,
          applyHolder(validationTraceTitles.canonicalDecodeItemStages.observe, [
            canonicalDecodeItemProof.spendingScriptHash,
            threadPolicyId,
            proofItem.spendingScriptHash,
            fieldPreimageCertificatePolicyId,
          ]),
        ),
    );
    const canonicalDecodeItemSource = yield* tryBuild(
      "Failed to build validation-trace canonical item source binder",
      () =>
        makeSpendingValidator(
          network,
          applyHolder(validationTraceTitles.canonicalDecodeItemStages.source, [
            canonicalDecodeItemObserve.spendingScriptHash,
            threadPolicyId,
          ]),
        ),
    );
    const canonicalDecodeItemStages = {
      source: canonicalDecodeItemSource,
      observe: canonicalDecodeItemObserve,
      proof: canonicalDecodeItemProof,
      settlement: canonicalDecodeItemSettlement,
    } as const;
    /**
     * Every parameter any semantic resolver declares, keyed by the name the
     * compiler recorded for it.
     *
     * The semantic family is deployed by iterating a title list, so the loop
     * cannot carry a hand-written argument list per title without drifting from
     * what the validators declare — which is precisely how the ten resolvers
     * that gained `field_preimage_certificate_policy_id` in #592 kept being
     * deployed with two arguments and became always-succeeds scripts (#605).
     * Resolving each declared parameter BY NAME makes the blueprint the only
     * authority on both the count and the order: a resolver that grows a
     * parameter is served automatically if the name is known, and refused
     * loudly if it is not. A count-only rule could not work here anyway — the
     * canonical-decode item resolver declares a parameter set that is
     * different entirely, not `award_script_hash` plus one (three names
     * before #620's transition-only subtraction dropped
     * `proof_item_script_hash`, two after).
     */
    const semanticResolverParameterValues = new Map<string, Data>([
      ["award_script_hash", award.spendingScriptHash],
      ["arm_script_hashes", cekCoreEntryHashes(cekCoreStages)],
      [
        "cek_context_control_script_hash",
        cekContextStages.control.spendingScriptHash,
      ],
      ["reference_script_auth_policy_id", referenceScriptAuthPolicyId],
      ["computation_thread_policy_id", threadPolicyId],
      ["thread_expiry_script_hash", threadExpiry.withdrawalScriptHash],
      [
        "field_preimage_certificate_policy_id",
        fieldPreimageCertificatePolicyId,
      ],
      [
        "source_binder_script_hash",
        canonicalDecodeItemSource.spendingScriptHash,
      ],
      ["proof_item_script_hash", proofItem.spendingScriptHash],
    ]);
    const semanticResolverParams = (title: string): readonly Data[] =>
      declaredParameters(getBlueprintValidator(blueprint, title)).map(
        (parameter) => {
          const value = semanticResolverParameterValues.get(parameter.title);
          if (value === undefined) {
            throw new Error(
              `Semantic resolver "${title}" declares parameter ` +
                `"${parameter.title}", which this deployment builder has no value ` +
                "for. Add it to the semantic-resolver parameter set rather than " +
                "deploying the resolver under-applied (#609).",
            );
          }
          return value;
        },
      );
    const builtSemanticResolvers: SpendingValidator[] = [];
    for (const [index, title] of semanticTitles.entries()) {
      builtSemanticResolvers.push(
        yield* tryBuild(
          `Failed to build validation-trace semantic resolver ${index.toString()}`,
          () =>
            makeSpendingValidator(
              network,
              applyBlueprintParams(
                blueprint,
                title,
                semanticResolverParams(title),
              ),
            ),
        ),
      );
    }
    if (builtSemanticResolvers.length !== 82) {
      return yield* Effect.fail(
        new Error("Validation-trace semantic resolver set is incomplete"),
      );
    }
    const baseSemanticResolvers = [
      builtSemanticResolvers[0]!,
      builtSemanticResolvers[1]!,
      builtSemanticResolvers[2]!,
      builtSemanticResolvers[3]!,
      builtSemanticResolvers[4]!,
      builtSemanticResolvers[5]!,
      builtSemanticResolvers[6]!,
      builtSemanticResolvers[7]!,
      builtSemanticResolvers[8]!,
      builtSemanticResolvers[9]!,
      builtSemanticResolvers[10]!,
      builtSemanticResolvers[11]!,
      builtSemanticResolvers[12]!,
      builtSemanticResolvers[13]!,
      builtSemanticResolvers[14]!,
      builtSemanticResolvers[15]!,
      builtSemanticResolvers[16]!,
      builtSemanticResolvers[17]!,
      builtSemanticResolvers[18]!,
      builtSemanticResolvers[19]!,
      builtSemanticResolvers[20]!,
      builtSemanticResolvers[21]!,
      builtSemanticResolvers[22]!,
      builtSemanticResolvers[23]!,
      builtSemanticResolvers[24]!,
      builtSemanticResolvers[25]!,
      builtSemanticResolvers[26]!,
      builtSemanticResolvers[27]!,
      builtSemanticResolvers[28]!,
      builtSemanticResolvers[29]!,
      builtSemanticResolvers[30]!,
      builtSemanticResolvers[31]!,
      builtSemanticResolvers[32]!,
      builtSemanticResolvers[33]!,
      builtSemanticResolvers[34]!,
      builtSemanticResolvers[35]!,
      builtSemanticResolvers[36]!,
      builtSemanticResolvers[37]!,
      builtSemanticResolvers[38]!,
      builtSemanticResolvers[39]!,
      builtSemanticResolvers[40]!,
      builtSemanticResolvers[41]!,
      builtSemanticResolvers[42]!,
      builtSemanticResolvers[43]!,
      builtSemanticResolvers[44]!,
      builtSemanticResolvers[45]!,
      builtSemanticResolvers[46]!,
      builtSemanticResolvers[47]!,
      builtSemanticResolvers[48]!,
      builtSemanticResolvers[49]!,
      builtSemanticResolvers[50]!,
      builtSemanticResolvers[51]!,
      builtSemanticResolvers[52]!,
      builtSemanticResolvers[53]!,
      builtSemanticResolvers[54]!,
      builtSemanticResolvers[55]!,
      builtSemanticResolvers[56]!,
      builtSemanticResolvers[57]!,
      builtSemanticResolvers[58]!,
      builtSemanticResolvers[59]!,
      builtSemanticResolvers[60]!,
      builtSemanticResolvers[61]!,
      builtSemanticResolvers[62]!,
      builtSemanticResolvers[63]!,
      builtSemanticResolvers[64]!,
      builtSemanticResolvers[65]!,
      builtSemanticResolvers[66]!,
      builtSemanticResolvers[67]!,
      builtSemanticResolvers[68]!,
      builtSemanticResolvers[69]!,
      builtSemanticResolvers[70]!,
      builtSemanticResolvers[71]!,
      builtSemanticResolvers[72]!,
      builtSemanticResolvers[73]!,
      builtSemanticResolvers[74]!,
      builtSemanticResolvers[75]!,
      builtSemanticResolvers[76]!,
      builtSemanticResolvers[77]!,
      builtSemanticResolvers[78]!,
      builtSemanticResolvers[79]!,
      builtSemanticResolvers[80]!,
      builtSemanticResolvers[81]!,
    ] as const;
    const semanticResolvers = [
      ...baseSemanticResolvers,
      sharedItem.entry,
    ] as const;
    const scriptSourcesStageTwoAdvance = yield* tryBuild(
      "Failed to build ScriptSources stage_two_advance yield",
      () =>
        makeWithdrawalValidator(
          applyHolder(
            validationTraceTitles.yields.scriptSourcesStageTwoAdvance,
            [
              [
                builtSemanticResolvers[
                  semanticTitles.indexOf(
                    validationTraceTitles.semantics.scriptSourcesNonOutput,
                  )
                ]!.spendingScriptHash,
              ],
            ],
          ),
        ),
    );
    const scriptSourcesStageThreeReplay = yield* tryBuild(
      "Failed to build ScriptSources stage_three_replay yield",
      () =>
        makeWithdrawalValidator(
          applyHolder(
            validationTraceTitles.yields.scriptSourcesStageThreeReplay,
            [
              [
                builtSemanticResolvers[
                  semanticTitles.indexOf(
                    validationTraceTitles.semantics.scriptSourcesNonOutput,
                  )
                ]!.spendingScriptHash,
              ],
            ],
          ),
        ),
    );
    const scriptSourcesStageThreeFinish = yield* tryBuild(
      "Failed to build ScriptSources stage_three_finish yield",
      () =>
        makeWithdrawalValidator(
          applyHolder(
            validationTraceTitles.yields.scriptSourcesStageThreeFinish,
            [
              [
                builtSemanticResolvers[
                  semanticTitles.indexOf(
                    validationTraceTitles.semantics.scriptSourcesNonOutput,
                  )
                ]!.spendingScriptHash,
              ],
            ],
          ),
        ),
    );
    const scriptSourcesStageFourBegin = yield* tryBuild(
      "Failed to build ScriptSources stage_four_begin yield",
      () =>
        makeWithdrawalValidator(
          applyHolder(
            validationTraceTitles.yields.scriptSourcesStageFourBegin,
            [
              [
                builtSemanticResolvers[
                  semanticTitles.indexOf(
                    validationTraceTitles.semantics.scriptSourcesNonOutput,
                  )
                ]!.spendingScriptHash,
              ],
              fieldPreimageCertificatePolicyId,
            ],
          ),
        ),
    );
    const scriptSourcesStageFourFinish = yield* tryBuild(
      "Failed to build ScriptSources stage_four_finish yield",
      () =>
        makeWithdrawalValidator(
          applyHolder(
            validationTraceTitles.yields.scriptSourcesStageFourFinish,
            [
              [
                builtSemanticResolvers[
                  semanticTitles.indexOf(
                    validationTraceTitles.semantics.scriptSourcesNonOutput,
                  )
                ]!.spendingScriptHash,
              ],
            ],
          ),
        ),
    );
    const scriptSourcesStageSixBeginPolicy = yield* tryBuild(
      "Failed to build ScriptSources stage_six_begin_policy yield",
      () =>
        makeWithdrawalValidator(
          applyHolder(
            validationTraceTitles.yields.scriptSourcesStageSixBeginPolicy,
            [
              [
                builtSemanticResolvers[
                  semanticTitles.indexOf(
                    validationTraceTitles.semantics.scriptSourcesNonOutput,
                  )
                ]!.spendingScriptHash,
              ],
              fieldPreimageCertificatePolicyId,
            ],
          ),
        ),
    );
    const scriptSourcesStageSixFoldAsset = yield* tryBuild(
      "Failed to build ScriptSources stage_six_fold_asset yield",
      () =>
        makeWithdrawalValidator(
          applyHolder(
            validationTraceTitles.yields.scriptSourcesStageSixFoldAsset,
            [
              [
                builtSemanticResolvers[
                  semanticTitles.indexOf(
                    validationTraceTitles.semantics.scriptSourcesNonOutput,
                  )
                ]!.spendingScriptHash,
              ],
            ],
          ),
        ),
    );
    const scriptSourcesStageSixFinish = yield* tryBuild(
      "Failed to build ScriptSources stage_six_finish yield",
      () =>
        makeWithdrawalValidator(
          applyHolder(
            validationTraceTitles.yields.scriptSourcesStageSixFinish,
            [
              [
                builtSemanticResolvers[
                  semanticTitles.indexOf(
                    validationTraceTitles.semantics.scriptSourcesNonOutput,
                  )
                ]!.spendingScriptHash,
              ],
            ],
          ),
        ),
    );
    const scriptSourcesObserverItem = yield* tryBuild(
      "Failed to build observer yield",
      () =>
        makeWithdrawalValidator(
          applyHolder(validationTraceTitles.yields.scriptSourcesObserverItem, [
            builtSemanticResolvers[
              semanticTitles.indexOf(
                validationTraceTitles.semantics.scriptSourcesStageSevenObserver,
              )
            ]!.spendingScriptHash,
            fieldPreimageCertificatePolicyId,
          ]),
        ),
    );
    const scriptSourcesObserverBound = yield* tryBuild(
      "Failed to build observer yield",
      () =>
        makeWithdrawalValidator(
          applyHolder(validationTraceTitles.yields.scriptSourcesObserverBound, [
            builtSemanticResolvers[
              semanticTitles.indexOf(
                validationTraceTitles.semantics.scriptSourcesStageSevenObserver,
              )
            ]!.spendingScriptHash,
          ]),
        ),
    );
    const scriptSourcesRedeemerDescriptor = yield* tryBuild(
      "Failed to build redeemer descriptor yield",
      () =>
        makeWithdrawalValidator(
          applyHolder(
            validationTraceTitles.yields.scriptSourcesRedeemerDescriptor,
            [
              [
                builtSemanticResolvers[
                  semanticTitles.indexOf(
                    validationTraceTitles.semantics.scriptSourcesStageTenMatch,
                  )
                ]!.spendingScriptHash,
                builtSemanticResolvers[
                  semanticTitles.indexOf(
                    validationTraceTitles.semantics
                      .scriptSourcesStageTenMismatch,
                  )
                ]!.spendingScriptHash,
                builtSemanticResolvers[
                  semanticTitles.indexOf(
                    validationTraceTitles.semantics
                      .scriptSourcesStageTwelveRedeemer,
                  )
                ]!.spendingScriptHash,
              ],
            ],
          ),
        ),
    );
    const stepDispatcherHashes = [
      builtSemanticResolvers[
        semanticTitles.indexOf(
          validationTraceTitles.semantics.scriptSourcesOutputProofStep,
        )
      ]!.spendingScriptHash,
      builtSemanticResolvers[
        semanticTitles.indexOf(
          validationTraceTitles.semantics.resolveInputsMembershipStep,
        )
      ]!.spendingScriptHash,
    ];
    const finalizeDispatcherHashes = [
      builtSemanticResolvers[
        semanticTitles.indexOf(
          validationTraceTitles.semantics.scriptSourcesOutputProofFinalize,
        )
      ]!.spendingScriptHash,
      builtSemanticResolvers[
        semanticTitles.indexOf(
          validationTraceTitles.semantics.resolveInputsMembershipFinalize,
        )
      ]!.spendingScriptHash,
    ];
    const ledgerOutputProofStructure = yield* tryBuild(
      "Failed to build ledger-output-proof structure yield",
      () =>
        makeWithdrawalValidator(
          applyHolder(validationTraceTitles.yields.ledgerOutputProofStructure, [
            stepDispatcherHashes,
          ]),
        ),
    );
    const ledgerOutputProofValue = yield* tryBuild(
      "Failed to build ledger-output-proof value yield",
      () =>
        makeWithdrawalValidator(
          applyHolder(validationTraceTitles.yields.ledgerOutputProofValue, [
            stepDispatcherHashes,
          ]),
        ),
    );
    const ledgerOutputProofDatumFoldMap = yield* tryBuild(
      "Failed to build ledger-output-proof datum fold-map yield",
      () =>
        makeWithdrawalValidator(
          applyHolder(
            validationTraceTitles.yields.ledgerOutputProofDatumFoldMap,
            [stepDispatcherHashes],
          ),
        ),
    );
    const ledgerOutputProofDatumFinalizeFrame = yield* tryBuild(
      "Failed to build ledger-output-proof datum finalize-frame yield",
      () =>
        makeWithdrawalValidator(
          applyHolder(
            validationTraceTitles.yields.ledgerOutputProofDatumFinalizeFrame,
            [stepDispatcherHashes],
          ),
        ),
    );
    const ledgerOutputProofDatumHeadScalar = yield* tryBuild(
      "Failed to build ledger-output-proof datum head-scalar yield",
      () =>
        makeWithdrawalValidator(
          applyHolder(
            validationTraceTitles.yields.ledgerOutputProofDatumHeadScalar,
            [stepDispatcherHashes],
          ),
        ),
    );
    const ledgerOutputProofDatumAttachInteger = yield* tryBuild(
      "Failed to build ledger-output-proof datum attach-integer yield",
      () =>
        makeWithdrawalValidator(
          applyHolder(
            validationTraceTitles.yields.ledgerOutputProofDatumAttachInteger,
            [stepDispatcherHashes],
          ),
        ),
    );
    const ledgerOutputProofDatumFoldList = yield* tryBuild(
      "Failed to build ledger-output-proof datum fold-list yield",
      () =>
        makeWithdrawalValidator(
          applyHolder(
            validationTraceTitles.yields.ledgerOutputProofDatumFoldList,
            [stepDispatcherHashes],
          ),
        ),
    );
    const ledgerOutputProofDatumAdvanceInteger = yield* tryBuild(
      "Failed to build ledger-output-proof datum advance-integer yield",
      () =>
        makeWithdrawalValidator(
          applyHolder(
            validationTraceTitles.yields.ledgerOutputProofDatumAdvanceInteger,
            [stepDispatcherHashes],
          ),
        ),
    );
    const ledgerOutputProofReferenceScript = yield* tryBuild(
      "Failed to build ledger-output-proof reference-script yield",
      () =>
        makeWithdrawalValidator(
          applyHolder(
            validationTraceTitles.yields.ledgerOutputProofReferenceScript,
            [stepDispatcherHashes],
          ),
        ),
    );
    const ledgerOutputProofScriptHash = yield* tryBuild(
      "Failed to build ledger-output-proof script-hash yield",
      () =>
        makeWithdrawalValidator(
          applyHolder(
            validationTraceTitles.yields.ledgerOutputProofScriptHash,
            [stepDispatcherHashes],
          ),
        ),
    );
    const ledgerOutputProofNativeScript = yield* tryBuild(
      "Failed to build ledger-output-proof native-script yield",
      () =>
        makeWithdrawalValidator(
          applyHolder(
            validationTraceTitles.yields.ledgerOutputProofNativeScript,
            [stepDispatcherHashes],
          ),
        ),
    );
    const ledgerOutputProofStructureAssets = yield* tryBuild(
      "Failed to build ledger-output-proof structure assets yield",
      () =>
        makeWithdrawalValidator(
          applyHolder(
            validationTraceTitles.yields.ledgerOutputProofStructureAssets,
            [stepDispatcherHashes],
          ),
        ),
    );
    const ledgerOutputProofStructureOptional = yield* tryBuild(
      "Failed to build ledger-output-proof structure optional yield",
      () =>
        makeWithdrawalValidator(
          applyHolder(
            validationTraceTitles.yields.ledgerOutputProofStructureOptional,
            [stepDispatcherHashes],
          ),
        ),
    );
    const ledgerOutputProofStructureFinish = yield* tryBuild(
      "Failed to build ledger-output-proof structure finish yield",
      () =>
        makeWithdrawalValidator(
          applyHolder(
            validationTraceTitles.yields.ledgerOutputProofStructureFinish,
            [stepDispatcherHashes],
          ),
        ),
    );
    const ledgerOutputProofDatumHeadSequence = yield* tryBuild(
      "Failed to build ledger-output-proof datum head-sequence yield",
      () =>
        makeWithdrawalValidator(
          applyHolder(
            validationTraceTitles.yields.ledgerOutputProofDatumHeadSequence,
            [stepDispatcherHashes],
          ),
        ),
    );
    const ledgerOutputProofDatumHeadMap = yield* tryBuild(
      "Failed to build ledger-output-proof datum head-map yield",
      () =>
        makeWithdrawalValidator(
          applyHolder(
            validationTraceTitles.yields.ledgerOutputProofDatumHeadMap,
            [stepDispatcherHashes],
          ),
        ),
    );
    const ledgerOutputProofDatumHeadLargeConstructor = yield* tryBuild(
      "Failed to build ledger-output-proof datum head-large-constructor yield",
      () =>
        makeWithdrawalValidator(
          applyHolder(
            validationTraceTitles.yields
              .ledgerOutputProofDatumHeadLargeConstructor,
            [stepDispatcherHashes],
          ),
        ),
    );
    const ledgerOutputProofDatumAttachBytes = yield* tryBuild(
      "Failed to build ledger-output-proof datum attach-bytes yield",
      () =>
        makeWithdrawalValidator(
          applyHolder(
            validationTraceTitles.yields.ledgerOutputProofDatumAttachBytes,
            [stepDispatcherHashes],
          ),
        ),
    );
    const ledgerOutputProofDatumAdvanceBytes = yield* tryBuild(
      "Failed to build ledger-output-proof datum advance-bytes yield",
      () =>
        makeWithdrawalValidator(
          applyHolder(
            validationTraceTitles.yields.ledgerOutputProofDatumAdvanceBytes,
            [stepDispatcherHashes],
          ),
        ),
    );
    const ledgerOutputProofDatumFinish = yield* tryBuild(
      "Failed to build ledger-output-proof datum finish yield",
      () =>
        makeWithdrawalValidator(
          applyHolder(
            validationTraceTitles.yields.ledgerOutputProofDatumFinish,
            [stepDispatcherHashes],
          ),
        ),
    );
    const ledgerOutputProofDatumLargeConstructor = yield* tryBuild(
      "Failed to build ledger-output-proof datum large-constructor yield",
      () =>
        makeWithdrawalValidator(
          applyHolder(
            validationTraceTitles.yields.ledgerOutputProofDatumLargeConstructor,
            [stepDispatcherHashes],
          ),
        ),
    );
    const ledgerOutputProofDatumLargeFields = yield* tryBuild(
      "Failed to build ledger-output-proof datum large-fields yield",
      () =>
        makeWithdrawalValidator(
          applyHolder(
            validationTraceTitles.yields.ledgerOutputProofDatumLargeFields,
            [stepDispatcherHashes],
          ),
        ),
    );
    const ledgerOutputProofDatumClose = yield* tryBuild(
      "Failed to build ledger-output-proof datum close yield",
      () =>
        makeWithdrawalValidator(
          applyHolder(
            validationTraceTitles.yields.ledgerOutputProofDatumClose,
            [stepDispatcherHashes],
          ),
        ),
    );
    const ledgerOutputProofSpan = yield* tryBuild(
      "Failed to build ledger-output-proof span yield",
      () =>
        makeWithdrawalValidator(
          applyHolder(validationTraceTitles.yields.ledgerOutputProofSpan, [
            stepDispatcherHashes,
          ]),
        ),
    );
    const ledgerOutputProofScalarInteger = yield* tryBuild(
      "Failed to build ledger-output-proof scalar-integer yield",
      () =>
        makeWithdrawalValidator(
          applyHolder(
            validationTraceTitles.yields.ledgerOutputProofScalarInteger,
            [stepDispatcherHashes],
          ),
        ),
    );
    const ledgerOutputProofScalarBytes = yield* tryBuild(
      "Failed to build ledger-output-proof scalar-bytes yield",
      () =>
        makeWithdrawalValidator(
          applyHolder(
            validationTraceTitles.yields.ledgerOutputProofScalarBytes,
            [stepDispatcherHashes],
          ),
        ),
    );
    const ledgerOutputDescriptorScanFacts = yield* tryBuild(
      "Failed to build ledger-output-descriptor scan-facts yield",
      () =>
        makeWithdrawalValidator(
          applyHolder(
            validationTraceTitles.yields.ledgerOutputDescriptorScanFacts,
            [finalizeDispatcherHashes],
          ),
        ),
    );
    const ledgerOutputDescriptorReferenceScript = yield* tryBuild(
      "Failed to build ledger-output-descriptor reference-script yield",
      () =>
        makeWithdrawalValidator(
          applyHolder(
            validationTraceTitles.yields.ledgerOutputDescriptorReferenceScript,
            [finalizeDispatcherHashes],
          ),
        ),
    );
    const ledgerOutputDescriptorDatumSummary = yield* tryBuild(
      "Failed to build ledger-output-descriptor datum-summary yield",
      () =>
        makeWithdrawalValidator(
          applyHolder(
            validationTraceTitles.yields.ledgerOutputDescriptorDatumSummary,
            [finalizeDispatcherHashes],
          ),
        ),
    );
    const ledgerOutputDescriptorValueSummary = yield* tryBuild(
      "Failed to build ledger-output-descriptor value-summary yield",
      () =>
        makeWithdrawalValidator(
          applyHolder(
            validationTraceTitles.yields.ledgerOutputDescriptorValueSummary,
            [finalizeDispatcherHashes],
          ),
        ),
    );
    const phaseANativeItemNative = yield* tryBuild(
      "Failed to build phase-A item native yield",
      () =>
        makeWithdrawalValidator(
          applyHolder(validationTraceTitles.yields.phaseANativeItemNative, [
            builtSemanticResolvers[
              semanticTitles.indexOf(
                validationTraceTitles.semantics.phaseANativeScriptsItem,
              )
            ]!.spendingScriptHash,
            award.spendingScriptHash,
            fieldPreimageCertificatePolicyId,
          ]),
        ),
    );
    const phaseANativeItemForeign = yield* tryBuild(
      "Failed to build phase-A item foreign yield",
      () =>
        makeWithdrawalValidator(
          applyHolder(validationTraceTitles.yields.phaseANativeItemForeign, [
            builtSemanticResolvers[
              semanticTitles.indexOf(
                validationTraceTitles.semantics.phaseANativeScriptsItem,
              )
            ]!.spendingScriptHash,
            award.spendingScriptHash,
            fieldPreimageCertificatePolicyId,
          ]),
        ),
    );
    const selectionDispatcherHash =
      builtSemanticResolvers[
        semanticTitles.indexOf(
          validationTraceTitles.semantics.cekExecutionSelection,
        )
      ]!.spendingScriptHash;
    const cekSelectionAuthenticate = yield* tryBuild(
      "Failed to build CEK selection authenticate yield",
      () =>
        makeWithdrawalValidator(
          applyHolder(validationTraceTitles.yields.cekSelectionAuthenticate, [
            selectionDispatcherHash,
          ]),
        ),
    );
    const cekSelectionSuccessor = yield* tryBuild(
      "Failed to build CEK selection successor yield",
      () =>
        makeWithdrawalValidator(
          applyHolder(validationTraceTitles.yields.cekSelectionSuccessor, [
            selectionDispatcherHash,
            scriptDecodePolicyId,
          ]),
        ),
    );
    const valueAndMintAssetFold = yield* tryBuild(
      "Failed to build validation-trace asset-fold yield",
      () =>
        makeWithdrawalValidator(
          applyHolder(validationTraceTitles.yields.valueAndMintAssetFold, [
            builtSemanticResolvers[
              semanticTitles.indexOf(
                validationTraceTitles.semantics.valueAndMintReplayAsset,
              )
            ]!.spendingScriptHash,
            builtSemanticResolvers[
              semanticTitles.indexOf(
                validationTraceTitles.semantics.valueAndMintOutputAsset,
              )
            ]!.spendingScriptHash,
            builtSemanticResolvers[
              semanticTitles.indexOf(
                validationTraceTitles.semantics.valueAndMintMintAsset,
              )
            ]!.spendingScriptHash,
          ]),
        ),
    );
    const semanticResolverGroups = [
      [semanticResolvers[0], semanticResolvers[1]],
      [semanticResolvers[2]],
      [semanticResolvers[3]],
      [semanticResolvers[4], semanticResolvers[5]],
      [
        semanticResolvers[6],
        semanticResolvers[7],
        semanticResolvers[8],
        semanticResolvers[9],
      ],
      [
        semanticResolvers[10],
        semanticResolvers[11],
        semanticResolvers[12],
        semanticResolvers[13],
        semanticResolvers[14],
        semanticResolvers[15],
        semanticResolvers[16],
        semanticResolvers[17],
        semanticResolvers[18],
        semanticResolvers[19],
        semanticResolvers[20],
        semanticResolvers[21],
        semanticResolvers[22],
        semanticResolvers[23],
      ],
      [semanticResolvers[24], semanticResolvers[25]],
      [
        semanticResolvers[26],
        semanticResolvers[27],
        semanticResolvers[28],
        semanticResolvers[29],
        semanticResolvers[30],
        semanticResolvers[31],
      ],
      [
        semanticResolvers[32],
        semanticResolvers[33],
        semanticResolvers[34],
        semanticResolvers[35],
        semanticResolvers[36],
        semanticResolvers[37],
        semanticResolvers[38],
        semanticResolvers[39],
        semanticResolvers[40],
        semanticResolvers[41],
        semanticResolvers[42],
        semanticResolvers[43],
        semanticResolvers[44],
        semanticResolvers[45],
        semanticResolvers[46],
        semanticResolvers[47],
        semanticResolvers[48],
        semanticResolvers[49],
        semanticResolvers[50],
        semanticResolvers[51],
        semanticResolvers[52],
        semanticResolvers[53],
        semanticResolvers[54],
        semanticResolvers[55],
        semanticResolvers[56],
        semanticResolvers[57],
        semanticResolvers[58],
        semanticResolvers[59],
        semanticResolvers[82],
      ],
      [semanticResolvers[60], semanticResolvers[61], semanticResolvers[62]],
      [
        semanticResolvers[63],
        semanticResolvers[64],
        semanticResolvers[65],
        semanticResolvers[66],
      ],
      [
        semanticResolvers[67],
        semanticResolvers[68],
        semanticResolvers[69],
        semanticResolvers[70],
      ],
      [
        semanticResolvers[71],
        semanticResolvers[72],
        semanticResolvers[73],
        semanticResolvers[74],
        semanticResolvers[75],
        semanticResolvers[76],
        semanticResolvers[77],
        semanticResolvers[78],
        semanticResolvers[79],
        semanticResolvers[80],
        semanticResolvers[81],
      ],
    ] as const;
    const semanticResolverHashesSchema = Data.Array(Data.Bytes());
    type SemanticResolverHashes = Data.Static<
      typeof semanticResolverHashesSchema
    >;
    const SemanticResolverHashes = asDataType<SemanticResolverHashes>(
      semanticResolverHashesSchema,
    );

    const prepareTitles = Object.entries(
      validationTraceTitles.prepares,
    ) as readonly (readonly [
      keyof typeof VALIDATION_TRACE_SEMANTIC_RESOLVER_GROUP_SIZES,
      string,
    ])[];
    const builtPrepareResolvers: SpendingValidator[] = [];
    for (const [index, [phase, title]] of prepareTitles.entries()) {
      const expectedGroupSize =
        VALIDATION_TRACE_SEMANTIC_RESOLVER_GROUP_SIZES[phase];
      const groupSize = semanticResolverGroups[index]!.length;
      if (groupSize !== expectedGroupSize) {
        return yield* Effect.fail(
          new Error(
            `Validation-trace prepare resolver "${title}" routes into ` +
              `${expectedGroupSize.toString()} semantic resolver(s) on chain ` +
              `but ${groupSize.toString()} were deployed for it`,
          ),
        );
      }
      const semanticResolverHashesData = Data.from(
        Data.to(
          semanticResolverGroups[index]!.map(
            ({ spendingScriptHash }) => spendingScriptHash,
          ),
          SemanticResolverHashes,
        ),
      ) as Data;
      builtPrepareResolvers.push(
        yield* tryBuild(
          `Failed to build validation-trace prepare resolver ${index.toString()}`,
          () =>
            makeSpendingValidator(
              network,
              applyHolder(title, [semanticResolverHashesData, threadPolicyId]),
            ),
        ),
      );
    }
    if (builtPrepareResolvers.length !== VALIDATION_TRACE_RESOLVER_COUNT) {
      return yield* Effect.fail(
        new Error("Validation-trace prepare resolver set is incomplete"),
      );
    }
    const prepareResolvers = [
      builtPrepareResolvers[0]!,
      builtPrepareResolvers[1]!,
      builtPrepareResolvers[2]!,
      builtPrepareResolvers[3]!,
      builtPrepareResolvers[4]!,
      builtPrepareResolvers[5]!,
      builtPrepareResolvers[6]!,
      builtPrepareResolvers[7]!,
      builtPrepareResolvers[8]!,
      builtPrepareResolvers[9]!,
      builtPrepareResolvers[10]!,
      builtPrepareResolvers[11]!,
      builtPrepareResolvers[12]!,
    ] as const;

    const resolvers = [
      prepareResolvers[0],
      prepareResolvers[1],
      prepareResolvers[2],
      prepareResolvers[3],
      prepareResolvers[4],
      prepareResolvers[5],
      prepareResolvers[6],
      prepareResolvers[7],
      prepareResolvers[8],
      prepareResolvers[9],
      prepareResolvers[10],
      prepareResolvers[11],
      prepareResolvers[12],
    ] as const;
    if (
      new Set(resolvers.map(({ spendingScriptHash }) => spendingScriptHash))
        .size !== VALIDATION_TRACE_RESOLVER_COUNT
    ) {
      return yield* Effect.fail(
        new Error("Validation-trace resolver hashes must be distinct"),
      );
    }
    // `boundary_v1` dispatches on a one-step resolver index over this list and
    // used to re-check its length against `resolver_count` per execution; the
    // deployed list is trusted on chain, so the cardinality is pinned here.
    const resolverCount: number = resolvers.length;
    if (resolverCount !== VALIDATION_TRACE_RESOLVER_COUNT) {
      return yield* Effect.fail(
        new Error(
          `Validation-trace boundary routes over ${VALIDATION_TRACE_RESOLVER_COUNT.toString()} ` +
            `one-step resolvers but ${resolverCount.toString()} were deployed`,
        ),
      );
    }
    const resolverHashesSchema = Data.Array(Data.Bytes());
    type ResolverHashes = Data.Static<typeof resolverHashesSchema>;
    const ResolverHashes = asDataType<ResolverHashes>(resolverHashesSchema);
    const resolverHashesData = Data.from(
      Data.to(
        resolvers.map(({ spendingScriptHash }) => spendingScriptHash),
        ResolverHashes,
      ),
    );

    const boundary = yield* tryBuild(
      "Failed to build validation-trace boundary validator",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(blueprint, validationTraceTitles.boundary, [
            resolverHashesData,
            award.spendingScriptHash,
            threadPolicyId,
            threadExpiry.withdrawalScriptHash,
          ]),
        ),
    );
    const timeout = yield* tryBuild(
      "Failed to build validation-trace timeout validator",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(blueprint, validationTraceTitles.timeout, [
            threadPolicyId,
            threadExpiry.withdrawalScriptHash,
          ]),
        ),
    );
    const game = yield* tryBuild(
      "Failed to build validation-trace midpoint game validator",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(blueprint, validationTraceTitles.game, [
            boundary.spendingScriptHash,
            timeout.spendingScriptHash,
            threadPolicyId,
            threadExpiry.withdrawalScriptHash,
          ]),
        ),
    );
    const source = yield* tryBuild(
      "Failed to build validation-trace source validator",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(blueprint, validationTraceTitles.source, [
            caseValidator.spendingScriptHash,
            disputeBond.spendingScriptHash,
            disputeBondLovelace,
            computationThread.policyId,
          ]),
        ),
    );
    const dispute = yield* tryBuild(
      "Failed to build validation-trace dispute opener",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(blueprint, validationTraceTitles.dispute, [
            source.spendingScriptHash,
            computationThread.policyId,
            hubOraclePolicyId,
          ]),
        ),
    );

    const steps: [typeof dispute, ...(typeof dispute)[]] = [
      dispute,
      source,
      game,
      boundary,
      timeout,
      award,
      proofItem,
      ...semanticResolvers,
      sharedItem.traversalNormalizer,
      sharedItem.outerNormalizer,
      sharedItem.sourceAuthenticator,
      ...sharedItem.executors,
      sharedItem.settlement,
      ...Object.values(cekContextStages),
      cekContextItemStages.entry,
      cekContextItemStages.settlement,
      ...Object.values(canonicalDecodeItemStages),
      ...prepareResolvers,
    ];
    if (steps.length !== VALIDATION_TRACE_DISPUTE_STEP_COUNT) {
      return yield* Effect.fail(
        new Error(
          `Validation-trace dispute composed ${steps.length.toString()} deployed steps, ` +
            `expected VALIDATION_TRACE_DISPUTE_STEP_COUNT=${VALIDATION_TRACE_DISPUTE_STEP_COUNT.toString()}`,
        ),
      );
    }
    return {
      firstStep: dispute,
      steps,
      opener: dispute,
      source,
      game,
      boundary,
      timeout,
      award,
      disputeBond,
      case: caseValidator,
      caseConfig,
      threadExpiry,
      proofItem,
      cekCoreStages,
      cekContextStages,
      cekContextItemStages,
      canonicalDecodePrepare: prepareResolvers[0],
      canonicalDecodeItemStages,
      scriptSourcesStageOneRedeemerStages,
      prepareResolvers,
      semanticResolvers,
      yields: {
        scriptSourcesStageTwoAdvance,
        scriptSourcesStageThreeReplay,
        scriptSourcesStageThreeFinish,
        scriptSourcesStageFourBegin,
        scriptSourcesStageFourFinish,
        scriptSourcesStageSixBeginPolicy,
        scriptSourcesStageSixFoldAsset,
        scriptSourcesStageSixFinish,
        scriptSourcesObserverItem,
        scriptSourcesObserverBound,
        scriptSourcesRedeemerDescriptor,
        ledgerOutputProofStructure,
        ledgerOutputProofValue,
        ledgerOutputProofDatumFoldMap,
        ledgerOutputProofDatumFinalizeFrame,
        ledgerOutputProofDatumHeadScalar,
        ledgerOutputProofDatumAttachInteger,
        ledgerOutputProofDatumFoldList,
        ledgerOutputProofDatumAdvanceInteger,
        ledgerOutputProofReferenceScript,
        ledgerOutputProofScriptHash,
        ledgerOutputProofNativeScript,
        ledgerOutputProofStructureAssets,
        ledgerOutputProofStructureOptional,
        ledgerOutputProofStructureFinish,
        ledgerOutputProofDatumHeadSequence,
        ledgerOutputProofDatumHeadMap,
        ledgerOutputProofDatumHeadLargeConstructor,
        ledgerOutputProofDatumAttachBytes,
        ledgerOutputProofDatumAdvanceBytes,
        ledgerOutputProofDatumFinish,
        ledgerOutputProofDatumLargeConstructor,
        ledgerOutputProofDatumLargeFields,
        ledgerOutputProofDatumClose,
        ledgerOutputProofSpan,
        ledgerOutputProofScalarInteger,
        ledgerOutputProofScalarBytes,
        ledgerOutputDescriptorScanFacts,
        ledgerOutputDescriptorReferenceScript,
        ledgerOutputDescriptorDatumSummary,
        ledgerOutputDescriptorValueSummary,
        phaseANativeItemNative,
        phaseANativeItemForeign,
        valueAndMintAssetFold,
        cekSelectionAuthenticate,
        cekSelectionSuccessor,
      },
      resolvers,
    };
  });
