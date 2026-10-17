import {
  encodeMidgardTxInputCanonical,
  FraudProofComputationThreadStepDatum,
  NATIVE_SCRIPT_DECODING_CLASS_PENDING,
  NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_ACCEPTANCE,
  NATIVE_SCRIPT_DECODING_SOURCE_KIND_NORMAL,
  OutputReference,
} from "@al-ft/midgard-sdk";
import { Data, toUnit } from "@lucid-evolution/lucid";

import { outRefLabel } from "../runtime.js";
import {
  assertNativeScriptDecodingFindingProvable,
  type NativeScriptDecodingFinding,
  NativeScriptDecodingProvability,
} from "./finding.js";
import {
  type DriveState,
  locateNativeScriptDecodingThread,
  type NativeScriptDecodingProofOutcome,
  type NativeScriptDecodingProverDeps,
  type NativeScriptDecodingProverEvent,
  requireDatum,
} from "./prover.locate-native-script-decoding-thread.js";
import {
  coreDirectionOf,
  remainingTxCount,
  toError,
} from "./prover.remaining-tx-count.js";
import {
  buildNativeScriptDecodingScanPlan,
  NativeScriptDecodingPlanRoutes,
  type NativeScriptDecodingScanPlan,
} from "./scan-plan.js";
import { nativeScriptDecodingSubmitError } from "./submit-common.js";
import { submitNativeScriptDecodingInit } from "./submit-native-script-decoding-init.js";
import {
  submitNativeScriptDecodingStep01BindNormal,
  submitNativeScriptDecodingStep01RecordForced,
} from "./submit-native-script-decoding-step-01.js";
import { submitNativeScriptDecodingStep02 } from "./submit-native-script-decoding-step-02.js";
import {
  submitNativeScriptDecodingStep03AdvanceOrCloseClose,
  submitNativeScriptDecodingStep03AdvanceOrCloseSegment,
  submitNativeScriptDecodingStep03BindDescriptor,
  submitNativeScriptDecodingStep03OpenSubject,
} from "./submit-native-script-decoding-step-03.js";
import { submitNativeScriptDecodingStep04 } from "./submit-native-script-decoding-step-04.js";

// ## The core

export const runNativeScriptDecodingProver = async (
  finding: NativeScriptDecodingFinding,
  deps: NativeScriptDecodingProverDeps,
): Promise<NativeScriptDecodingProofOutcome> => {
  const { lucid, contracts, policy, signer } = deps;
  const referenceScriptUtxos = deps.referenceScriptUtxos;
  if (
    referenceScriptUtxos?.step01 === undefined ||
    referenceScriptUtxos.step02 === undefined ||
    referenceScriptUtxos.step03OpenSubject === undefined ||
    referenceScriptUtxos.step03BindDescriptor === undefined ||
    referenceScriptUtxos.step03AdvanceOrClose === undefined ||
    referenceScriptUtxos.step04 === undefined
  ) {
    throw nativeScriptDecodingSubmitError(
      "production proving requires authenticated reference-script UTxOs for all six custody validators.",
    );
  }
  const headerHash = finding.headerHash;
  const journal = async (
    event: Omit<NativeScriptDecodingProverEvent, "headerHash">,
  ) => {
    await deps.journal({ ...event, headerHash });
  };
  const refused = async (
    refusal: "classification" | "policy" | "duplicate" | "alreadyProven",
    reason: string,
  ): Promise<NativeScriptDecodingProofOutcome> => {
    await journal({
      phase: "outcome",
      message: `refused (${refusal}): ${reason}`,
    });
    return { kind: "refused", refusal, reason };
  };
  const stalled = async (
    reason: string,
    threadOutRef: string | null,
    cause: unknown,
  ): Promise<NativeScriptDecodingProofOutcome> => {
    await journal({
      phase: "outcome",
      message: `STALLED: ${reason}`,
      threadOutRef: threadOutRef ?? undefined,
    });
    return { kind: "stalled", reason, threadOutRef, cause };
  };

  // 1. The non-negotiable §3.2/3.3 boundary: classification gates proving
  //    regardless of policy.
  try {
    assertNativeScriptDecodingFindingProvable(finding);
  } catch (cause) {
    return refused("classification", toError(cause).message);
  }

  const assetName = `${deps.category.categoryId}${headerHash}`;
  const threadUnit = toUnit(contracts.computationThread.policyId, assetName);
  const fraudProofUnit = toUnit(contracts.fraudProof.policyId, assetName);

  // 2. Idempotence fast-path: a fraud-proof token for this header already
  //    sits at the fraud-proof address.
  const provenAlready = await lucid.utxosAtWithUnit(
    contracts.fraudProof.spendingScriptAddress,
    fraudProofUnit,
  );
  if (provenAlready.length > 0) {
    return refused(
      "alreadyProven",
      `fraud-proof token ${fraudProofUnit} already exists at ${outRefLabel(provenAlready[0]!)}.`,
    );
  }

  // 3. §7.1: locate any live thread for this asset name.
  const position = await locateNativeScriptDecodingThread({
    lucid,
    contracts,
    threadUnit,
  });
  if (position.step !== "none") {
    const datum = Data.from(
      requireDatum(position.threadUtxo),
      FraudProofComputationThreadStepDatum,
    );
    if (datum.fraud_prover !== signer.paymentKeyHash) {
      // §3.4 dedup: a live third-party thread is sound; duplicating it
      // only wastes fees.
      return refused(
        "duplicate",
        `a live thread at ${outRefLabel(position.threadUtxo)} names fraud prover ${datum.fraud_prover}, not this wallet.`,
      );
    }
  }

  // 4. Route preparation. For the machine route, the plan is re-derived up
  //    front: the budget arithmetic and the §7.1 mid-loop boundary search
  //    both need it.
  let plan: NativeScriptDecodingScanPlan | null = null;
  let descriptorCbor: string | null = null;
  let referenceScriptItemBytes: Uint8Array | null = null;
  const needsDescriptorBinding =
    finding.provability !==
    NativeScriptDecodingProvability.OutOfDomainAccusation;
  try {
    if (needsDescriptorBinding) {
      const resolved = await deps.evidence.descriptor(finding);
      descriptorCbor = resolved.descriptorCbor;
      referenceScriptItemBytes = resolved.referenceScriptItemBytes;
      if (
        finding.provability === NativeScriptDecodingProvability.MachineRoute
      ) {
        if (referenceScriptItemBytes === null) {
          throw nativeScriptDecodingSubmitError(
            "the machine route scans the reference-script item; the descriptor evidence carries no item bytes.",
          );
        }
        plan = buildNativeScriptDecodingScanPlan({
          itemBytes: referenceScriptItemBytes,
          direction: coreDirectionOf(finding),
        });
        if (
          plan.route === NativeScriptDecodingPlanRoutes.DescriptorContradiction
        ) {
          throw nativeScriptDecodingSubmitError(
            "the re-derived plan routes to a descriptor contradiction — the finding's machine-route classification does not match the evidence.",
          );
        }
      }
    }
  } catch (cause) {
    // Before Init this is a classification/evidence refusal; with a live
    // thread it is a stall the operator must see.
    return position.step === "none"
      ? refused("classification", toError(cause).message)
      : stalled(
          `route preparation failed on a live thread: ${toError(cause).message}`,
          outRefLabel(position.threadUtxo),
          cause,
        );
  }

  // 5. Map the on-chain position onto the drive cursor.
  let cursor: DriveState;
  if (position.step === "none") {
    cursor = { at: "init" };
  } else if (position.step === "step01" || position.step === "step02") {
    cursor = {
      at: position.step,
      threadOutRef: outRefLabel(position.threadUtxo),
    };
  } else if (position.step === "step04") {
    cursor = {
      at: "step04",
      threadOutRef: outRefLabel(position.threadUtxo),
    };
  } else if (position.step === "step03OpenSubject") {
    cursor = {
      at: "openSubject",
      threadOutRef: outRefLabel(position.threadUtxo),
    };
  } else if (position.step === "step03BindDescriptor") {
    cursor = {
      at: "bindDescriptor",
      threadOutRef: outRefLabel(position.threadUtxo),
    };
  } else {
    if (position.step !== "step03AdvanceOrClose") {
      throw nativeScriptDecodingSubmitError(
        `unhandled thread position ${String(position.step)}`,
      );
    }
    const threadOutRef = outRefLabel(position.threadUtxo);
    const state = position.state;
    if (state.refusal_class !== NATIVE_SCRIPT_DECODING_CLASS_PENDING) {
      return stalled(
        `thread at AdvanceOrClose carries closed class ${state.refusal_class.toString()} instead of paying step-04.`,
        threadOutRef,
        null,
      );
    }
    if (
      plan === null ||
      plan.route !== NativeScriptDecodingPlanRoutes.Machine
    ) {
      return stalled(
        "the thread is at AdvanceOrClose but local evidence derives no machine plan.",
        threadOutRef,
        null,
      );
    }
    const committed = state.machine_state_hash;
    const segmentIndex = plan.segments.findIndex(
      (segment) => segment.controlBefore.hashHex === committed,
    );
    if (segmentIndex >= 0) {
      cursor = { at: "advanceOrClose", threadOutRef, segmentIndex };
    } else if (plan.verdict.control?.hashHex === committed) {
      cursor = { at: "close", threadOutRef };
    } else {
      return stalled(
        `no plan boundary hashes to committed machine state ${committed}.`,
        threadOutRef,
        null,
      );
    }
  }

  // 6. Policy gates. Settlement depth and maturity gate Init; the fee
  //    budget gates Init and is re-checked as the loop progresses.
  const totalTxCount = remainingTxCount({ at: "init" }, finding, plan);
  const consumedBeforeThisRun =
    totalTxCount - remainingTxCount(cursor, finding, plan);
  const requireBudget = (
    at: DriveState,
    submittedThisRun: number,
  ): string | null => {
    if (policy.maxThreadBudgetLovelace === null) {
      return null;
    }
    const projected =
      BigInt(
        consumedBeforeThisRun +
          submittedThisRun +
          remainingTxCount(at, finding, plan),
      ) * policy.assumedFeePerTxLovelace;
    return projected > policy.maxThreadBudgetLovelace
      ? `projected thread cost ${projected.toString()} lovelace exceeds the ${policy.maxThreadBudgetLovelace.toString()} cap.`
      : null;
  };
  if (cursor.at === "init") {
    if (policy.minSettlementDepth > 0n) {
      if (deps.observations.settlementDepthOf === undefined) {
        throw nativeScriptDecodingSubmitError(
          "the policy gates on settlement depth but deps carry no settlementDepthOf observer.",
        );
      }
      const depth = await deps.observations.settlementDepthOf(
        finding.fraudulentBlockOutRef,
      );
      if (depth < policy.minSettlementDepth) {
        return refused(
          "policy",
          `the faulted block sits at depth ${depth.toString()}, under the ${policy.minSettlementDepth.toString()} settlement gate.`,
        );
      }
    }
    if (policy.maturityGuardFactor > 0) {
      if (deps.observations.remainingMaturityMs === undefined) {
        throw nativeScriptDecodingSubmitError(
          "the policy gates on remaining maturity but deps carry no remainingMaturityMs observer.",
        );
      }
      const remainingMs = await deps.observations.remainingMaturityMs(
        finding.fraudulentBlockOutRef,
      );
      const predictedMs = totalTxCount * policy.assumedMillisPerTx;
      if (remainingMs < policy.maturityGuardFactor * predictedMs) {
        return refused(
          "policy",
          `remaining maturity ${remainingMs.toString()}ms is under ${policy.maturityGuardFactor.toString()}× the predicted ${predictedMs.toString()}ms serial duration.`,
        );
      }
    }
    const overBudget = requireBudget(cursor, 0);
    if (overBudget !== null) {
      return refused("policy", overBudget);
    }
  }

  // 7. Drive. Every submitter is idempotent-by-reconstruction; an
  //    unexpected abort stalls loudly with the thread's position.
  const txHashes: string[] = [];
  const submittedThisRun = () => txHashes.length;
  let currentOutRef: string | null =
    cursor.at === "init" ? null : cursor.threadOutRef;
  try {
    while (true) {
      // §4.3: the budget is re-checked as the loop progresses. A breach
      // mid-thread stalls (the thread is real; cancellation is explicit).
      if (cursor.at !== "init") {
        const overBudget = requireBudget(cursor, txHashes.length);
        if (overBudget !== null) {
          return stalled(
            `budget breached mid-thread: ${overBudget}`,
            currentOutRef,
            null,
          );
        }
      }
      switch (cursor.at) {
        case "init": {
          const result = await submitNativeScriptDecodingInit({
            lucid,
            blueprint: deps.blueprint,
            network: deps.network,
            contracts,
            category: deps.category,
            catalogue: deps.catalogue,
            signer,
            fraudulentBlockOutRef: finding.fraudulentBlockOutRef,
            fraudulentHeaderHash: headerHash,
            witnessReferenceScripts: deps.witnessReferenceScripts,
          });
          txHashes.push(result.txHash);
          currentOutRef = result.nextThreadOutRef;
          await journal({
            phase: "init",
            message: "thread minted",
            txHash: result.txHash,
            threadOutRef: currentOutRef,
          });
          cursor = { at: "step01", threadOutRef: currentOutRef };
          break;
        }
        case "step01": {
          const shared = {
            lucid,
            contracts,
            categoryId: deps.category.categoryId,
            signer,
            threadOutRef: cursor.threadOutRef,
            referenceScriptUtxo: referenceScriptUtxos.step01,
            witnessReferenceScripts: deps.witnessReferenceScripts,
          };
          const result =
            finding.sourceKind === NATIVE_SCRIPT_DECODING_SOURCE_KIND_NORMAL
              ? await submitNativeScriptDecodingStep01BindNormal({
                  ...shared,
                  blueprint: deps.blueprint,
                  network: deps.network,
                  stateQueueBlockOutRef: finding.fraudulentBlockOutRef,
                  txInclusion: await deps.evidence.txInclusion(finding),
                  publishedProofChunks:
                    (await deps.evidence.publishedProofChunks?.(finding)) ??
                    undefined,
                })
              : await submitNativeScriptDecodingStep01RecordForced({
                  ...shared,
                  direction: finding.direction,
                });
          txHashes.push(result.txHash);
          currentOutRef = result.nextThreadOutRef;
          await journal({
            phase: "step01",
            message: "source bound",
            txHash: result.txHash,
            threadOutRef: currentOutRef,
          });
          cursor = { at: "step02", threadOutRef: currentOutRef };
          break;
        }
        case "step02": {
          const isDirectionA =
            finding.direction ===
            NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_ACCEPTANCE;
          const result = await submitNativeScriptDecodingStep02({
            lucid,
            contracts,
            categoryId: deps.category.categoryId,
            signer,
            threadOutRef: cursor.threadOutRef,
            reconstruction: await deps.evidence.reconstruction(finding),
            forcedOrderKey:
              finding.event.kind === "forcedEvent"
                ? Data.from(finding.event.orderKeyCbor, OutputReference)
                : undefined,
            chosenOutpoint: isDirectionA
              ? {
                  sourceKind: finding.accusedOutpointSourceKind,
                  cursor: finding.accusedOutpointCursor,
                }
              : undefined,
            referenceScriptUtxo: referenceScriptUtxos.step02,
          });
          txHashes.push(result.txHash);
          currentOutRef = result.nextThreadOutRef;
          await journal({
            phase: "step02",
            message: "committed claims opened",
            txHash: result.txHash,
            threadOutRef: currentOutRef,
          });
          cursor = { at: "openSubject", threadOutRef: currentOutRef };
          break;
        }
        case "openSubject": {
          const namesField =
            (finding.accusedOutpointSourceKind === 0n ||
              finding.accusedOutpointSourceKind === 1n) &&
            finding.accusedOutpointCursor >= 0n;
          const subject = namesField
            ? await deps.evidence.subjectTx(finding)
            : null;
          const result = await submitNativeScriptDecodingStep03OpenSubject({
            lucid,
            contracts,
            categoryId: deps.category.categoryId,
            signer,
            threadOutRef: cursor.threadOutRef,
            nativeTxCompactCbor: subject?.nativeTxCompactCbor,
            subjectFieldInputs: subject?.subjectFieldInputs,
            publishCarriage: deps.publishCarriage,
            referenceScriptUtxo: referenceScriptUtxos.step03OpenSubject,
          });
          txHashes.push(result.txHash);
          const nextOutRef = result.nextThreadOutRef;
          currentOutRef = nextOutRef;
          const opened = needsDescriptorBinding;
          await journal({
            phase: "openSubject",
            message: opened
              ? "accused outpoint opened"
              : "out-of-domain accusation closed",
            txHash: result.txHash,
            threadOutRef: nextOutRef,
          });
          cursor = opened
            ? { at: "bindDescriptor", threadOutRef: nextOutRef }
            : { at: "step04", threadOutRef: nextOutRef };
          break;
        }
        case "bindDescriptor": {
          if (descriptorCbor === null) {
            throw nativeScriptDecodingSubmitError(
              "BindDescriptor reached without descriptor evidence.",
            );
          }
          const subject = await deps.evidence.subjectTx(finding);
          const accused =
            subject.subjectFieldInputs[Number(finding.accusedOutpointCursor)];
          if (accused === undefined) {
            throw nativeScriptDecodingSubmitError(
              "BindDescriptor cannot recover the outpoint opened on-chain.",
            );
          }
          const outpointKeyCbor = Buffer.from(
            encodeMidgardTxInputCanonical(accused),
          ).toString("hex");
          const result = await submitNativeScriptDecodingStep03BindDescriptor({
            lucid,
            contracts,
            categoryId: deps.category.categoryId,
            signer,
            threadOutRef: cursor.threadOutRef,
            outpointKeyCbor,
            descriptorCbor,
            ledgerTrie: await deps.evidence.ledgerTrie(finding),
            plan: plan ?? undefined,
            referenceScriptItemBytes: referenceScriptItemBytes ?? undefined,
            referenceScriptUtxo: referenceScriptUtxos.step03BindDescriptor,
          });
          txHashes.push(result.txHash);
          const nextOutRef = result.nextThreadOutRef;
          currentOutRef = nextOutRef;
          const machinePlan =
            plan?.route === NativeScriptDecodingPlanRoutes.Machine
              ? plan
              : null;
          await journal({
            phase: "bindDescriptor",
            message:
              machinePlan === null
                ? "descriptor bound and closed"
                : "descriptor bound, machine committed",
            txHash: result.txHash,
            threadOutRef: nextOutRef,
          });
          cursor =
            machinePlan === null
              ? { at: "step04", threadOutRef: nextOutRef }
              : machinePlan.segments.length > 0
                ? {
                    at: "advanceOrClose",
                    threadOutRef: nextOutRef,
                    segmentIndex: 0,
                  }
                : { at: "close", threadOutRef: nextOutRef };
          break;
        }
        case "advanceOrClose": {
          if (plan === null || referenceScriptItemBytes === null) {
            throw nativeScriptDecodingSubmitError(
              "AdvanceOrClose reached without a machine plan.",
            );
          }
          const segment = plan.segments[cursor.segmentIndex];
          if (segment === undefined) {
            throw nativeScriptDecodingSubmitError(
              `plan has no segment ${cursor.segmentIndex.toString()}.`,
            );
          }
          const result =
            await submitNativeScriptDecodingStep03AdvanceOrCloseSegment({
              lucid,
              contracts,
              categoryId: deps.category.categoryId,
              signer,
              threadOutRef: cursor.threadOutRef,
              segment,
              referenceScriptItemBytes,
              referenceScriptUtxo: referenceScriptUtxos.step03AdvanceOrClose,
            });
          txHashes.push(result.txHash);
          const nextOutRef = result.nextThreadOutRef;
          currentOutRef = nextOutRef;
          const closed =
            result.destinationAddress ===
            contracts.steps[5].spendingScriptAddress;
          await journal({
            phase: "advanceOrClose",
            message: closed
              ? "exact terminal closed"
              : `segment ${(cursor.segmentIndex + 1).toString()}/${plan.segments.length.toString()} advanced`,
            txHash: result.txHash,
            threadOutRef: nextOutRef,
          });
          cursor = closed
            ? { at: "step04", threadOutRef: nextOutRef }
            : cursor.segmentIndex + 1 < plan.segments.length
              ? {
                  at: "advanceOrClose",
                  threadOutRef: nextOutRef,
                  segmentIndex: cursor.segmentIndex + 1,
                }
              : { at: "close", threadOutRef: nextOutRef };
          break;
        }
        case "close": {
          if (plan === null) {
            throw nativeScriptDecodingSubmitError(
              "AdvanceOrClose close reached without a plan.",
            );
          }
          const result =
            await submitNativeScriptDecodingStep03AdvanceOrCloseClose({
              lucid,
              contracts,
              categoryId: deps.category.categoryId,
              signer,
              threadOutRef: cursor.threadOutRef,
              verdict: plan.verdict,
              referenceScriptItemBytes:
                plan.verdict.window === null
                  ? undefined
                  : (referenceScriptItemBytes ?? undefined),
              referenceScriptUtxo: referenceScriptUtxos.step03AdvanceOrClose,
            });
          txHashes.push(result.txHash);
          const nextOutRef = result.nextThreadOutRef;
          currentOutRef = nextOutRef;
          await journal({
            phase: "close",
            message: "scan claim closed",
            txHash: result.txHash,
            threadOutRef: nextOutRef,
          });
          cursor = { at: "step04", threadOutRef: nextOutRef };
          break;
        }
        case "step04": {
          const result = await submitNativeScriptDecodingStep04({
            lucid,
            contracts,
            categoryId: deps.category.categoryId,
            signer,
            threadOutRef: cursor.threadOutRef,
            referenceScriptUtxo: referenceScriptUtxos.step04,
            witnessReferenceScripts: deps.witnessReferenceScripts,
          });
          txHashes.push(result.txHash);
          await journal({
            phase: "step04",
            message: `fraud-proof token minted (${submittedThisRun().toString()} txs this run)`,
            txHash: result.txHash,
            threadOutRef: result.fraudProofOutRef,
          });
          await journal({ phase: "outcome", message: "proven" });
          return {
            kind: "proven",
            fraudProofUnit: result.fraudProofUnit,
            fraudProofOutRef: result.fraudProofOutRef,
            txHashes,
          };
        }
      }
    }
  } catch (cause) {
    return stalled(
      `unexpected abort at ${cursor.at}: ${toError(cause).message}`,
      currentOutRef,
      cause,
    );
  }
};
