/**
 * The CEK context's finalization (`cek-context-finalization.ak` and the
 * assembly steps of `cek-context-step.ak`): the context parts, the TxInfo
 * assembly, the final context, and the seed the execution starts from.
 */
import {
  buildMidgardValidationMerkleMembership,
  decodeMidgardLedgerOutputCommitment,
} from "@al-ft/midgard-core";

import {
  composeMidgardCekContextSummary,
  hashMidgardCekContextPartsControl,
  hashMidgardCekFinalContextControl,
  hashMidgardCekTxInfoAssemblyControl,
  type MidgardCekContextControl,
  type MidgardCekContextPartsControl,
  type MidgardCekFinalContextControl,
  type MidgardCekRedeemerContextControl,
  type MidgardCekTxInfoAssemblyControl,
} from "../cek-context.js";
import {
  emptyMidgardCekDataListSummary,
  prependMidgardCekDataListSummary,
  summarizeMidgardCekSmallConstrData,
} from "../script-context-proof.js";
import { type CekContextRun } from "./cek-producer.context-step.js";
import {
  exactDescriptorSummary,
  outRefSummary,
  sameSummary,
} from "./trace-descriptor-summary.js";

/** Stages 10 to 13 and the seed. A PlutusV3 spend binds its script info to
 * the spent input's descriptor. Returns the seeded control. */
export const seedCekContext = (
  run: CekContextRun,
  start: MidgardCekContextControl,
  redeemerControl: MidgardCekRedeemerContextControl,
): MidgardCekContextControl => {
  const { parts } = run;
  const { purpose } = run.execution;
  const spendIndex = Number(purpose.purposeIndex);
  const spendItem =
    run.languageTag === 3 && purpose.purposeKind === 0
      ? run.prepared.resolutionScheduleNodes[spendIndex]
      : undefined;
  const spendDescriptorCbor =
    spendItem === undefined
      ? undefined
      : run.prepared.ledgerDescriptorState.get(spendItem.key.toString("hex"));
  if (spendItem !== undefined && spendDescriptorCbor === undefined) {
    throw new Error(
      "CEK spend finalization lost its authenticated ledger descriptor",
    );
  }
  const scriptInfo =
    spendItem === undefined
      ? parts.scriptInfo
      : summarizeMidgardCekSmallConstrData(
          1n,
          prependMidgardCekDataListSummary(
            outRefSummary(spendItem.key),
            prependMidgardCekDataListSummary(
              exactDescriptorSummary(
                decodeMidgardLedgerOutputCommitment(spendDescriptorCbor!)
                  .cardanoSpendDatum,
              ),
              emptyMidgardCekDataListSummary(),
            ),
          ),
        );
  if (!sameSummary(scriptInfo, parts.scriptInfo)) {
    throw new Error(
      "CEK descriptor-derived script info differs from the evaluated context",
    );
  }
  const partsControl: MidgardCekContextPartsControl = {
    redeemerItems: redeemerControl.mapItems,
    redeemer: redeemerControl.currentRedeemer,
    scriptInfo,
  };
  run.at(
    start,
    spendItem === undefined
      ? { kind: "cekContextFinalize", redeemerControl }
      : {
          kind: "cekContextFinalizeSpend",
          redeemerControl,
          itemIndex: spendIndex,
          key: spendItem.key,
          descriptorCbor: spendDescriptorCbor!,
          siblings: buildMidgardValidationMerkleMembership(
            run.resolvedLeaves,
            spendIndex,
          ).siblings,
        },
  );
  let control: MidgardCekContextControl = {
    ...start,
    stage: 11,
    redeemerContextControlHash: hashMidgardCekContextPartsControl(partsControl),
  };
  run.at(control, { kind: "cekContextAssemble", control: partsControl });
  const assemblyControl: MidgardCekTxInfoAssemblyControl = {
    tailFields: parts.tailFields,
    redeemer: parts.redeemer,
    scriptInfo,
  };
  control = {
    ...control,
    stage: 12,
    redeemerContextControlHash:
      hashMidgardCekTxInfoAssemblyControl(assemblyControl),
  };
  run.at(control, { kind: "cekTxInfoFinalize", control: assemblyControl });
  const finalControl: MidgardCekFinalContextControl = {
    txInfo: parts.txInfo,
    redeemer: parts.redeemer,
    scriptInfo: parts.scriptInfo,
  };
  control = {
    ...control,
    stage: 13,
    redeemerContextControlHash: hashMidgardCekFinalContextControl(finalControl),
  };
  if (
    !sameSummary(composeMidgardCekContextSummary(finalControl), parts.context)
  ) {
    throw new Error("CEK final context composition differs from evaluation");
  }
  run.at(control, { kind: "cekContextSeed", control: finalControl });
  return control;
};
