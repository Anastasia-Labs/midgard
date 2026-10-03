import { outRefLabel } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, toUnit } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  buildCountedRoot,
  buildTransitionFaultProof,
  detectTransitionTraceFaults,
  resolveTransitionTraceDeploymentContracts,
  rootCountProof,
  submitTransitionTraceProof,
  transitionTraceFinalIndex,
} from "../src/index.js";
import {
  alignedHeaderStart,
  removeAndAssertPermanentProof,
} from "./submit-init-emulator-transition-trace-subvariants.remove-and-assert-permanent-proof.js";
import {
  firstThreadUtxo,
  makeHarness,
  reconstruct,
  setupChallenge,
  withdrawalInfo,
} from "./submit-init-emulator-transition-trace-subvariants.setup-challenge.js";
import {
  funderPaymentKeyHash,
  makeHeader,
  network,
  transitionTraceDaEntry,
  transitionTraceOutRef,
} from "./support/submit-init-emulator-shared.js";

type ShortRoot = "withdrawals" | "transitionTrace" | "eventToStep";

const counted = async (
  domain: SDK.RootDomain,
  entries: readonly SDK.DaPayloadEntry[],
) =>
  await buildCountedRoot(
    domain,
    entries.map(([key, value]) => ({
      key: Buffer.from(key, "hex"),
      value: Buffer.from(value, "hex"),
    })),
  );

/**
 * A withdrawal-only block whose header counts are `count` everywhere. Every
 * member list holds `count` entries except `short`, which keeps only the
 * first, so exactly that counted root embeds a count the header disagrees
 * with. The header stays valid: the counts still sum, the step count still
 * equals the total, and every positive count has a non-empty root.
 */
const buildCountBlock = async ({
  operator,
  startTime,
  count,
  short,
}: {
  readonly operator: string;
  readonly startTime: number;
  readonly count: bigint;
  readonly short?: ShortRoot;
}) => {
  const events = Array.from({ length: Number(count) }, (_, index) => {
    const withdrawalId = transitionTraceOutRef((0x61 + index).toString(16));
    const eventKey: SDK.EventKey = {
      WithdrawalEventKey: { withdrawal_id: withdrawalId },
    };
    const stepIndex = BigInt(index);
    return {
      withdrawal: [
        Data.to(withdrawalId, SDK.OutputReference),
        SDK.committedWithdrawalValueBytes(
          withdrawalInfo("IncorrectWithdrawalSignature"),
        ),
      ] satisfies SDK.DaPayloadEntry,
      step: transitionTraceDaEntry({
        key: stepIndex,
        keySchema: Data.Integer() as never,
        value: {
          schema_version: 1n,
          step_index: stepIndex,
          event_key: eventKey,
          phase: "Withdrawal",
          pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
          post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
        } satisfies SDK.TransitionStep,
        valueSchema: SDK.TransitionStepSchema,
      }),
      mapping: transitionTraceDaEntry({
        key: eventKey,
        keySchema: SDK.EventKeySchema,
        value: {
          step_index: stepIndex,
          phase: "Withdrawal",
        } satisfies SDK.EventToStepValue,
        valueSchema: SDK.EventToStepValueSchema,
      }),
    };
  });
  const members = (root: ShortRoot, entries: SDK.DaPayloadEntry[]) =>
    root === short ? entries.slice(0, 1) : entries;
  const withdrawals = members(
    "withdrawals",
    events.map(({ withdrawal }) => withdrawal),
  );
  const transitionTrace = members(
    "transitionTrace",
    events.map(({ step }) => step),
  );
  const eventToStep = members(
    "eventToStep",
    events.map(({ mapping }) => mapping),
  );
  const [withdrawalsRoot, traceRoot, mappingRoot] = await Promise.all([
    counted(SDK.ROOT_DOMAINS.withdrawals, withdrawals),
    counted(SDK.ROOT_DOMAINS.transitionTrace, transitionTrace),
    counted(SDK.ROOT_DOMAINS.eventToStep, eventToStep),
  ]);
  const header: SDK.Header = {
    ...makeHeader(operator, startTime),
    withdrawalsRoot: withdrawalsRoot.root,
    transitionTraceRoot: traceRoot.root,
    eventToStepRoot: mappingRoot.root,
    withdrawalCount: count,
    totalEventCount: count,
    transitionStepCount: count,
  };
  const reconstruction = await reconstruct({
    header,
    withdrawals,
    transitionTrace,
    eventToStep,
  });
  return { header, reconstruction };
};

/** Lands the block through the production state queue, with no bypass. */
const landCountBlock = async ({
  count,
  short,
}: {
  readonly count: bigint;
  readonly short?: ShortRoot;
}) => {
  const context = await makeHarness();
  const { harness, publications, transitionTraceReferenceScripts } = context;
  const block = await buildCountBlock({
    operator: await funderPaymentKeyHash(harness.funderLucid),
    startTime: await alignedHeaderStart(harness),
    count,
    ...(short === undefined ? {} : { short }),
  });
  const lifecycle = await setupChallenge({
    harness,
    publications,
    transitionTraceReferenceScripts,
    header: block.header,
  });
  expect(lifecycle.setup.headerHash).toBe(
    await Effect.runPromise(SDK.hashBlockHeader(block.header)),
  );
  return { ...context, block, lifecycle };
};

type Landed = Awaited<ReturnType<typeof landCountBlock>>;

const submit = async (landed: Landed, proof: SDK.TransitionFaultProof) =>
  await submitTransitionTraceProof({
    lucid: landed.harness.proverLucid,
    blueprint: landed.harness.realBlueprint,
    deploymentInfo: landed.lifecycle.deploymentInfo,
    network,
    signer: landed.harness.proverSigner,
    threadOutRef: outRefLabel(
      await firstThreadUtxo({
        harness: landed.harness,
        init: landed.lifecycle.init,
      }),
    ),
    proof,
    witnessReferenceScripts: landed.harness.witnessReferenceScripts,
    awaitConfirmation: true,
  });

/** The final-0 count check refused: no fraud-proof token, block retained. */
const expectRefusedAtFinalZero = async (
  landed: Landed,
  proof: SDK.TransitionFaultProof,
) => {
  expect(transitionTraceFinalIndex(proof)).toBe(0);
  const refusal = await submit(landed, proof).then(
    () => undefined,
    (error: unknown) => String(error),
  );
  // The final transaction's only script spend is the routed thread.
  expect(refusal).toMatch(/failed script execution Spend\[\d+\]/u);
  const { harness, lifecycle } = landed;
  const resolved = await resolveTransitionTraceDeploymentContracts({
    blueprint: harness.realBlueprint,
    deploymentInfo: lifecycle.deploymentInfo,
    network,
    requireFraudProofSpend: true,
  });
  await expect(
    harness.proverLucid.utxosAtWithUnit(
      resolved.contracts.transitionTrace.finals[0]!.spendingScriptAddress,
      lifecycle.init.computationThreadUnit,
    ),
  ).resolves.toHaveLength(1);
  await expect(
    harness.proverLucid.utxosAtWithUnit(
      resolved.contracts.fraudProof.spendingScriptAddress,
      toUnit(
        resolved.contracts.fraudProof.policyId,
        lifecycle.init.computationThreadAssetName,
      ),
    ),
  ).resolves.toHaveLength(0);
  await expect(
    harness.funderLucid.utxosAtWithUnit(
      harness.contracts.stateQueue.spendingScriptAddress,
      lifecycle.setup.stateQueueBlockUnit,
    ),
  ).resolves.toHaveLength(1);
};

describe("transition-trace root-count route lifecycle", () => {
  it.each([
    ["withdrawals", "withdrawals_root_count", "SourceRootCountMismatch"],
    [
      "transitionTrace",
      "transition_trace_root_count",
      "TransitionTraceRootCountMismatch",
    ],
    ["eventToStep", "event_to_step_root_count", "EventToStepRootCountMismatch"],
  ] as const)(
    "proves a %s root embedding fewer members than the header count",
    async (short, invariant, arm) => {
      const landed = await landCountBlock({ count: 2n, short });
      const counts = (
        await detectTransitionTraceFaults(landed.block.reconstruction)
      ).filter(({ kind }) => kind === "countFault");
      expect(counts.map((detection) => detection.invariant)).toEqual([
        invariant,
      ]);
      const [detection] = counts;
      if (detection === undefined || !detection.buildable)
        throw new Error("count detection is not buildable");
      if (!("CountFault" in detection.fault))
        throw new Error("count detection is not a CountFault");
      const witness = detection.fault.CountFault.witness;
      if (typeof witness === "string" || !(arm in witness))
        throw new Error(`count detection is not ${arm}`);
      expect(transitionTraceFinalIndex(detection.proof)).toBe(0);

      const proofResult = await submit(landed, detection.proof);
      await removeAndAssertPermanentProof({
        harness: landed.harness,
        setup: landed.lifecycle.setup,
        deploymentInfo: landed.lifecycle.deploymentInfo,
        proofResult,
      });
    },
    300_000,
  );

  // Each negative isolates one conjunct of the SourceRootCountMismatch arm
  // against an honest header: a forged count differs from the header but no
  // longer opens the root; the honest count opens it but equals the header.
  it.each([
    ["a forged count that does not open the root", 2n],
    ["an honest count equal to the header", 1n],
  ] as const)(
    "refuses a root-count proof with %s",
    async (_name, proofCount) => {
      const landed = await landCountBlock({ count: 1n });
      const { reconstruction } = landed.block;
      expect(
        (await detectTransitionTraceFaults(reconstruction)).filter(
          ({ kind }) => kind === "countFault",
        ),
      ).toEqual([]);
      const proof = buildTransitionFaultProof({
        reconstruction,
        fault: SDK.countFault({
          SourceRootCountMismatch: {
            proof: {
              ...rootCountProof(reconstruction.rootData.withdrawals),
              count: proofCount,
            },
          },
        }),
      });
      await expectRefusedAtFinalZero(landed, proof);
    },
    300_000,
  );
});
