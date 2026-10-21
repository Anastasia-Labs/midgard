/**
 * Our case verified by another party and defended by nobody, under the
 * prover funding permit, on the Open scene's emulator, and the permit's walk
 * over the case close the funded watcher builds for it. The journey records
 * the raw L1 view from before the scene, so the convicted block's removal
 * can observe it, and captures every close the watcher builds with the
 * authority its builder granted. A test can present the close's authority
 * checks another policy, permit state, lineage or spent case and bond, or
 * walk the close as the runtime bound does.
 */
import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import type { TxSigned, UTxO } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import { VALIDATION_TRACE_CASE_OPEN } from "../../src/validation-trace-dispute/adapter.js";
import {
  parseValidationTraceDisputeArtifact,
  type ValidationTraceDisputeArtifact,
} from "../../src/validation-trace-dispute/artifact.js";
import { createWorkflowActuationPermitController } from "../../src/workflow/actuation-permit.js";
import { validationDisputeFunding } from "../../src/workflow/funding-reservation-permit.assert-validation-dispute-funding.js";
import { exactResolvedOutputCbor } from "../../src/workflow/funding-reservation-permit.create-workflow-funding-reservation-permit.js";
import {
  assertValidationDisputeCloseBinding,
  assertValidationDisputeCloseCase,
  assertValidationDisputeCloseInputs,
  grantValidationDisputeCloseAuthority,
  readValidationDisputeCloseAuthority,
  type ValidationDisputeCloseAuthority,
} from "../../src/workflow/funding-reservation-permit.validation-dispute-close-authority.js";
import type { JournalJsonObject } from "../../src/workflow/journal.js";
import type { FraudProofWorkflowAction } from "../../src/workflow/orchestrator.js";
import { workflowPreflightTransaction } from "../../src/workflow/transaction-boundary.js";
import { recordCrossBlockRawEmulator } from "./cross-block-raw-emulator.js";
import { drainProver } from "./installed-prover-wallet.js";
import {
  outRefOf,
  verifiedByAnotherParty,
} from "./validation-trace-dispute-foreign-verify.js";
import {
  confirmedMoves,
  recordedOutput,
} from "./validation-trace-dispute-funded-moves.js";
import {
  type AdapterWrap,
  type FundedDispute,
  openFundedDispute,
  turnUntil,
} from "./validation-trace-dispute-funded-runner.js";
import {
  entriesOf,
  FUNDED,
} from "./validation-trace-dispute-funded-verify-source.js";
import {
  type Scene,
  setUpScene,
} from "./validation-trace-dispute-open-scene.js";
import {
  type RecordedRemoval,
  recordedRemoval,
} from "./validation-trace-dispute-removal-l1.js";

const CATEGORY = "validationTraceDispute" as const;

/** A close the watcher built, as its builder evaluated it. */
export type CapturedClose = Readonly<{
  signed: TxSigned;
  authority: ValidationDisputeCloseAuthority;
  action: FraudProofWorkflowAction;
}>;

export type ForeignClose = Readonly<{
  recorder: ReturnType<typeof recordCrossBlockRawEmulator>;
  removal: RecordedRemoval;
  scene: Scene;
  dispute: FundedDispute;
  verified: Awaited<ReturnType<typeof verifiedByAnotherParty>>;
  /** The pending claim our confirmed Open left. */
  opener: string;
  artifact: ValidationTraceDisputeArtifact;
  closes: CapturedClose[];
  /** Captures each close the watcher builds into `closes`. */
  capture: AdapterWrap;
}>;

/**
 * A fresh scene whose funded watcher opened our claim, another party
 * verified it first, and the next process holds the case open.
 */
export const openForeignVerifiedCase = async (): Promise<ForeignClose> => {
  // Before the scene, so the raw L1 view has the whole history.
  const recorder = recordCrossBlockRawEmulator();
  const scene = await setUpScene();
  const removal = await recordedRemoval(scene, recorder);
  await drainProver(scene.h as never, FUNDED);
  const dispute = await openFundedDispute(scene, { removal: removal.removal });
  const verified = await verifiedByAnotherParty(scene, dispute);
  await dispute.restart();
  const opened = await turnUntil(dispute, VALIDATION_TRACE_CASE_OPEN, {
    limit: 40,
  });
  expect(opened.result.kind, opened.history.join("\n")).toBe("pending");
  const stages = confirmedMoves(opened.result);
  expect(stages.map(({ stage }) => stage)).toStrictEqual(["init", "open"]);
  const prepared = entriesOf(opened.result).find(
    ({ event }) => event.kind === "prepared",
  );
  if (prepared?.event.kind !== "prepared") throw new Error("no artifact");
  const closes: CapturedClose[] = [];
  return {
    recorder,
    removal,
    scene,
    dispute,
    verified,
    opener: stages[1]!.outputs[0]!,
    artifact: parseValidationTraceDisputeArtifact(
      prepared.event.artifact.familyArtifact as JournalJsonObject,
    ),
    closes,
    capture: (adapter) => ({
      ...adapter,
      preflight: async (context) => {
        const preflight = await adapter.preflight(context);
        const signed = workflowPreflightTransaction(preflight);
        const authority =
          signed === undefined
            ? null
            : readValidationDisputeCloseAuthority(signed);
        if (authority !== null)
          closes.push({ signed: signed!, authority, action: context.action });
        return preflight;
      },
    }),
  };
};

/** The checks of the close's authority over the watcher's first close. */
export const closeAuthorityChecks = (journey: ForeignClose) => {
  const { scene, dispute, verified, opener, closes } = journey;
  const closed = () => closes[0]!;
  const terms = (): never =>
    ({
      caseAddress: scene.chain.case.spendingScriptAddress,
      casePolicyId: scene.chain.case.policyId,
      bondAddress: scene.chain.disputeBond.spendingScriptAddress,
    }) as never;
  const policy = (validationDispute: unknown): never =>
    ({
      category: CATEGORY,
      deploymentFingerprint: closed().authority.deploymentFingerprint,
      fundingPaymentKeyHash: scene.h.proverSigner.paymentKeyHash,
      validationDispute,
    }) as never;
  /** The reservation lineage of the pending claim our Open left. */
  const openLineage = () => {
    const claim = recordedOutput(scene, opener);
    return {
      sourceActionKind: "open",
      sourceOutputIndex: claim.outputIndex,
      outRef: opener,
      resolvedOutputCborHex: exactResolvedOutputCbor(claim),
    };
  };
  /** The permit state the watcher's turn held while building the close. */
  const state = (
    overrides: Record<string, unknown> = {},
    lineage: unknown = openLineage(),
  ): never =>
    ({
      category: CATEGORY,
      currentActionKind: "close",
      currentActionDigest: computeDeploymentManifestJsonDigest(closed().action),
      actuationPermit: createWorkflowActuationPermitController({
        decision: dispute.decision,
        rollbackGeneration: "0",
      }).permit,
      snapshot: { rollbackGeneration: "0" },
      port: {
        resolveConfirmedInput: async ({ outRef }: { outRef: string }) =>
          outRef === opener ? lineage : null,
      },
      ...overrides,
    }) as never;
  const bind = ({
    authority = closed().authority,
    withoutTerms = false,
    policyCategory = CATEGORY as string,
    overrides = {},
    action = closed().action,
    headerHash = closed().authority.headerHash,
  }: {
    readonly authority?: ValidationDisputeCloseAuthority;
    /** A policy that carries no dispute terms. */
    readonly withoutTerms?: boolean;
    readonly policyCategory?: string;
    readonly overrides?: Record<string, unknown>;
    readonly action?: FraudProofWorkflowAction;
    readonly headerHash?: string;
  } = {}) =>
    assertValidationDisputeCloseBinding({
      authority,
      policy: {
        ...(policy(withoutTerms ? undefined : terms()) as object),
        category: policyCategory,
      } as never,
      state: state(overrides),
      action,
      headerHash,
    });
  const readCase = (lineage: unknown) =>
    assertValidationDisputeCloseCase({
      authority: closed().authority,
      lineage,
      fundingPaymentKeyHash: scene.h.proverSigner.paymentKeyHash,
    });
  /** The spent case and bond made by `edit`, with an authority over them. */
  const presentInputs = (
    edit: (spent: { caseInput: UTxO; bond: UTxO }) => {
      caseInput: UTxO;
      bond: UTxO;
    },
    authority: Partial<ValidationDisputeCloseAuthority> = {},
  ) => {
    const { caseInput, bond } = edit({
      caseInput: verified.caseOutput,
      bond: verified.bond,
    });
    return assertValidationDisputeCloseInputs({
      authority: {
        ...closed().authority,
        caseResolvedOutputCborHex: exactResolvedOutputCbor(caseInput),
        bondResolvedOutputCborHex: exactResolvedOutputCbor(bond),
        ...authority,
      },
      terms: terms(),
      caseId: readCase(openLineage()),
      caseInput,
      bond,
    });
  };
  const spentInputs = () =>
    new Map<string, UTxO>([
      [outRefOf(verified.caseOutput), verified.caseOutput],
      [outRefOf(verified.bond), verified.bond],
    ]);
  /** The close's bytes as another TxSigned, granted its authority again. */
  const regranted = async () => {
    const signed = await scene.h.proverLucid
      .fromTx(closed().signed.toTransaction().to_cbor_hex())
      .complete();
    const { authority } = closed();
    grantValidationDisputeCloseAuthority({
      signed,
      deploymentFingerprint: authority.deploymentFingerprint,
      headerHash: authority.headerHash,
      caseOpenerOutRef: authority.caseOpenerOutRef,
      caseInput: verified.caseOutput,
      bond: verified.bond,
    });
    return signed;
  };
  /**
   * Walks the close as the runtime bound does over `inputs`: its spent
   * inputs, each contract input's lineage, and the protocol top-up.
   */
  const walk = async ({
    signed = closed().signed,
    slash = false,
    lineage = openLineage() as unknown,
    inputs = spentInputs(),
    topup = 0n,
  } = {}) => {
    const opened = await validationDisputeFunding({
      signed,
      slash,
      policy: policy(terms()),
      state: state({}, lineage),
      action: closed().action,
    });
    if (opened === null) throw new Error("the close carries no authority");
    opened.admitOpener(inputs);
    const lineages = [...inputs].map(([outRef, input]) =>
      opened.admitContractInput(outRef, input),
    );
    opened.refuseProtocolTopup(topup);
    return lineages;
  };
  return {
    closed,
    openLineage,
    bind,
    readCase,
    presentInputs,
    spentInputs,
    regranted,
    walk,
  };
};

/** `utxo` holding another token of `policyId` in place of its own. */
export const otherToken = (utxo: UTxO, policyId: string): UTxO => {
  const unit = Object.keys(utxo.assets).find((key) =>
    key.startsWith(policyId),
  )!;
  const { [unit]: _, ...rest } = utxo.assets;
  const other = unit.slice(0, -2) + (unit.endsWith("00") ? "01" : "00");
  return { ...utxo, assets: { ...rest, [other]: 1n } };
};
