/** The prover funding permit refuses a validation-trace dispute VerifySource
 * whose authority and outputs agree with each other but not with the
 * deployment's dispute terms: a bond above the deployment-bound bond, or the
 * bond or case paid to, and bound at, another dispute contract. It refuses
 * an authority naming another bond marker or case token than the claim's,
 * another pending claim, or the claim's output other than it is, and a
 * second case output, or a bond or case output without its own token. The
 * workflow stalls with the refusal after Init and Open. Called on what the
 * builder evaluated, the authority's checks refuse an action other than the
 * one the reservation began, and a claim for another header.
 *
 * The category is not installed until its defender exists; this file
 * stands in for that install so the production classifier mints the
 * objective's actuation permit. */
import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import {
  assetsToValue,
  CML,
  coreToTxOutput,
  Data,
  type UTxO,
} from "@lucid-evolution/lucid";
import { beforeEach, describe, expect, it, vi } from "vitest";

import { VALIDATION_TRACE_CASE_OPEN } from "../src/validation-trace-dispute/adapter.js";
import {
  assertValidationDisputeFundingAuthorityBinding,
  assertValidationDisputeOpener,
  readValidationDisputeFundingAuthority,
  type ValidationDisputeFundingAuthority,
} from "../src/workflow/funding-reservation-permit.assert-validation-dispute-funding.js";
import { exactResolvedOutputCbor } from "../src/workflow/funding-reservation-permit.create-workflow-funding-reservation-permit.js";
import type { FraudProofWorkflowAction } from "../src/workflow/orchestrator.js";
import { parseOwnOutRef } from "../src/workflow/own-outputs.js";
import { workflowPreflightTransaction } from "../src/workflow/transaction-boundary.js";
import { drainProver } from "./support/installed-prover-wallet.js";
import {
  openFundedDispute,
  turnUntil,
} from "./support/validation-trace-dispute-funded-runner.js";
import {
  confirmedStages,
  FLOOR,
  FUNDED,
  isVerifySource,
  TIMEOUT,
} from "./support/validation-trace-dispute-funded-verify-source.js";
import {
  BOND_LOVELACE,
  type Scene,
  setUpScene,
} from "./support/validation-trace-dispute-open-scene.js";
import {
  bondIndex,
  caseIndex,
  changeIndex,
  presentVerifySource,
  type VerifySourcePresentation,
  withAddress,
  withCoin,
} from "./support/validation-trace-dispute-tampered-verify-source.js";

// Stands in for the family's install, which waits on its defender.
vi.mock("../src/workflow/installed-categories.js", async (importOriginal) => {
  const actual =
    await importOriginal<
      typeof import("../src/workflow/installed-categories.js")
    >();
  return {
    ...actual,
    isInstalledFraudProofCategory: (
      category: Parameters<typeof actual.isInstalledFraudProofCategory>[0],
    ) =>
      category === "validationTraceDispute" ||
      actual.isInstalledFraudProofCategory(category),
  };
});

let scene: Scene;

const BINDING =
  "validation dispute funding authority differs from its exact VerifySource action or deployment terms";

const ANOTHER = "00".repeat(32);

/** Another proof-thread contract the deployment's roster governs. */
const anotherContract = () => scene.chain.source.spendingScriptAddress;

/** `unit` with its last byte changed: another token of the same policy. */
const anotherUnit = (unit: string) =>
  unit.slice(0, -2) + (unit.endsWith("00") ? "01" : "00");

/** `output` holding `assets` instead of its own. */
const withAssets = (
  output: CML.TransactionOutput,
  assets: Record<string, bigint>,
) =>
  CML.TransactionOutput.new(
    output.address(),
    assetsToValue(assets),
    output.datum() ?? null,
    output.script_ref() ?? null,
  );

/** The bond's or the case's own token moved from it to the change. */
const tokenToChange =
  (kind: "bond" | "case") =>
  (
    outputs: CML.TransactionOutput[],
    addresses: { bond: string; case: string; wallet: string },
  ) => {
    const at =
      kind === "bond"
        ? bondIndex(outputs, addresses.bond)
        : caseIndex(outputs, addresses.case);
    const change = changeIndex(outputs, addresses.wallet);
    const { assets } = coreToTxOutput(outputs[at]!);
    // The bond marker and the case token are both the case policy's.
    const own = Object.keys(assets).filter((unit) =>
      unit.startsWith(scene.chain.case.policyId),
    );
    if (own.length !== 1) throw new Error(`the ${kind} carries no own token`);
    const { [own[0]!]: _, ...rest } = assets;
    return outputs.map((output, index) =>
      index === at
        ? withAssets(output, rest)
        : index === change
          ? withAssets(output, {
              ...coreToTxOutput(output).assets,
              [own[0]!]: 1n,
            })
          : output,
    );
  };

describe("validation-trace dispute VerifySource bound to other dispute terms", () => {
  // Each case disputes the scene's failing block from a fresh deployment,
  // its prover wallet split into a collateral output and plain funding.
  beforeEach(async () => {
    scene = await setUpScene();
  }, TIMEOUT);

  it.each([
    {
      name: "an authority and a bond one lovelace over the deployment-bound bond",
      present: (): VerifySourcePresentation => ({
        outputs: (
          outputs: CML.TransactionOutput[],
          addresses: { bond: string; wallet: string },
        ) => {
          const bond = bondIndex(outputs, addresses.bond);
          const change = changeIndex(outputs, addresses.wallet);
          return outputs.map((output, index) =>
            index === bond
              ? withCoin(output, output.amount().coin() + 1n)
              : index === change
                ? withCoin(output, output.amount().coin() - 1n)
                : output,
          );
        },
        authority: (grant) => ({
          ...grant,
          bond: { ...grant.bond, lovelace: grant.bond.lovelace + 1n },
        }),
      }),
      refusal: `${BINDING} (bond)`,
    },
    {
      name: "an authority and a bond at another dispute contract",
      present: (): VerifySourcePresentation => ({
        outputs: (
          outputs: CML.TransactionOutput[],
          addresses: { bond: string },
        ) => {
          const bond = bondIndex(outputs, addresses.bond);
          return outputs.map((output, index) =>
            index === bond ? withAddress(output, anotherContract()) : output,
          );
        },
        authority: (grant) => ({
          ...grant,
          bond: { ...grant.bond, address: anotherContract() },
        }),
      }),
      refusal: `${BINDING} (bond address)`,
    },
    {
      name: "an authority and a case at another dispute contract",
      present: (): VerifySourcePresentation => ({
        outputs: (
          outputs: CML.TransactionOutput[],
          addresses: { case: string },
        ) => {
          const at = caseIndex(outputs, addresses.case);
          return outputs.map((output, index) =>
            index === at ? withAddress(output, anotherContract()) : output,
          );
        },
        authority: (grant) => ({
          ...grant,
          caseOutput: { ...grant.caseOutput, address: anotherContract() },
        }),
      }),
      refusal: `${BINDING} (case address)`,
    },
    {
      name: "an authority naming another bond marker",
      present: (): VerifySourcePresentation => ({
        authority: (grant) => ({
          ...grant,
          bond: { ...grant.bond, unit: anotherUnit(grant.bond.unit) },
        }),
      }),
      refusal: `${BINDING} (bond unit)`,
    },
    {
      name: "an authority naming another case token",
      present: (): VerifySourcePresentation => ({
        authority: (grant) => ({
          ...grant,
          caseOutput: {
            ...grant.caseOutput,
            unit: anotherUnit(grant.caseOutput.unit),
          },
        }),
      }),
      refusal: `${BINDING} (case unit)`,
    },
    {
      name: "an authority over the claim's output holding another lovelace",
      present: (): VerifySourcePresentation => ({
        opener: (opener) => ({
          ...opener,
          assets: {
            ...opener.assets,
            lovelace: (opener.assets.lovelace ?? 0n) + 1n,
          },
        }),
      }),
      refusal: `${BINDING} (opener output)`,
    },
    {
      name: "an authority over another pending claim",
      present: (): VerifySourcePresentation => ({
        opener: (opener) => ({ ...opener, txHash: ANOTHER }),
      }),
      refusal: `${BINDING} (opener)`,
    },
    {
      name: "a second case output",
      present: (): VerifySourcePresentation => ({
        outputs: (outputs, addresses) => {
          const at = outputs[caseIndex(outputs, addresses.case)]!;
          const change = changeIndex(outputs, addresses.wallet);
          return [
            ...outputs.map((output, index) =>
              index === change
                ? withCoin(output, output.amount().coin() - at.amount().coin())
                : output,
            ),
            at,
          ];
        },
      }),
      refusal:
        "validation dispute VerifySource must pay exactly one bond and one case output: bonds=1 cases=2",
    },
    {
      name: "the bond marker paid to the change",
      present: (): VerifySourcePresentation => ({
        outputs: tokenToChange("bond"),
      }),
      refusal:
        "validation dispute bond output differs from its exact marker and datum",
    },
    {
      name: "the case token paid to the change",
      present: (): VerifySourcePresentation => ({
        outputs: tokenToChange("case"),
      }),
      refusal:
        "validation dispute case output differs from its exact case and thread tokens",
    },
  ])(
    "refuses VerifySource with $name",
    async ({ present, refusal }) => {
      await drainProver(scene.h as never, FUNDED);
      const dispute = await openFundedDispute(scene);
      try {
        const { result, history } = await turnUntil(
          dispute,
          VALIDATION_TRACE_CASE_OPEN,
          { wrap: presentVerifySource(scene, present()) },
        );
        expect(result.kind, history.join("\n")).toBe("stalled");
        if (result.kind !== "stalled") return;
        expect(result.reason).toContain("funding reservation failed for");
        expect(result.reason).toContain(refusal);
        expect(confirmedStages(result)).toStrictEqual(["init", "open"]);
      } finally {
        await dispute.close();
      }
    },
    TIMEOUT,
  );

  /** What the builder evaluated: its authority, pending claim and action. */
  const evaluated = async () => {
    await drainProver(scene.h as never, FUNDED);
    const dispute = await openFundedDispute(scene);
    let seen:
      | Readonly<{
          authority: ValidationDisputeFundingAuthority;
          opener: UTxO;
          action: FraudProofWorkflowAction;
        }>
      | undefined;
    try {
      await turnUntil(dispute, VALIDATION_TRACE_CASE_OPEN, {
        wrap: (adapter) => ({
          ...adapter,
          preflight: async (context) => {
            const preflight = await adapter.preflight(context);
            if (!isVerifySource(context.action) || seen !== undefined)
              return preflight;
            const authority = readValidationDisputeFundingAuthority(
              workflowPreflightTransaction(preflight)!,
            )!;
            const [opener] = await scene.h.emulator.getUtxosByOutRef([
              parseOwnOutRef(authority.openerOutRef),
            ]);
            seen = { authority, opener: opener!, action: context.action };
            return preflight;
          },
        }),
      });
    } finally {
      await dispute.close();
    }
    if (seen === undefined) throw new Error("VerifySource was never built");
    return seen;
  };

  const terms = () => ({
    bondLovelace: BOND_LOVELACE.toString(),
    caseFloorLovelace: FLOOR.toString(),
    bondAddress: scene.chain.disputeBond.spendingScriptAddress,
    caseAddress: scene.chain.case.spendingScriptAddress,
    casePolicyId: scene.chain.case.policyId,
  });

  /** The binding checked against the action the reservation began. */
  const bind = (
    { authority, action }: Awaited<ReturnType<typeof evaluated>>,
    presented: FraudProofWorkflowAction,
  ) =>
    assertValidationDisputeFundingAuthorityBinding({
      authority,
      policy: {
        category: "validationTraceDispute",
        deploymentFingerprint: authority.deploymentFingerprint,
        validationDispute: terms(),
      } as never,
      state: {
        category: "validationTraceDispute",
        currentActionKind: "verify_source",
        currentActionDigest: computeDeploymentManifestJsonDigest(action),
      } as never,
      action: presented,
      headerHash: authority.headerHash,
    });

  /**
   * The claim checked against an authority derived from `claimed`, a claim
   * naming `header`, so that only the header can differ.
   */
  const derive = (
    { authority, opener }: Awaited<ReturnType<typeof evaluated>>,
    header: string | null,
  ) => {
    const pending = Data.from(opener.datum!, SDK.PendingValidationClaimDatum);
    const state = pending.data!;
    const claimed: UTxO = {
      ...opener,
      datum: Data.to(
        {
          ...pending,
          data: {
            ...state,
            challenged_header_hash: header ?? authority.headerHash,
          },
        },
        SDK.PendingValidationClaimDatum,
      ),
    };
    const caseId = SDK.validationCaseId(
      header ?? authority.headerHash,
      state.claim.event_to_step_membership.key,
      opener,
    );
    return assertValidationDisputeOpener({
      authority: {
        ...authority,
        openerResolvedOutputCborHex: exactResolvedOutputCbor(claimed),
        caseUnit: scene.chain.case.policyId + caseId,
        bondUnit:
          scene.chain.case.policyId + SDK.validationCaseBondMarkerName(caseId),
        bondDatumCborHex: Data.to(
          {
            challenged_header_hash: header ?? authority.headerHash,
            fraud_prover: pending.fraud_prover,
            case_id: caseId,
          },
          SDK.DisputeBond,
        ),
      },
      terms: terms(),
      opener: claimed,
      fundingPaymentKeyHash: scene.h.proverSigner.paymentKeyHash,
    });
  };

  it.each([
    {
      name: "admits the action the reservation began",
      check: (seen: Awaited<ReturnType<typeof evaluated>>) =>
        bind(seen, seen.action),
      refusal: null,
    },
    {
      name: "refuses an action other than the one the reservation began",
      check: (seen: Awaited<ReturnType<typeof evaluated>>) =>
        bind(seen, { ...seen.action, actionId: `${seen.action.actionId}'` }),
      refusal: `${BINDING} (action digest)`,
    },
    {
      name: "admits the claim for the authority's header",
      check: (seen: Awaited<ReturnType<typeof evaluated>>) =>
        derive(seen, null),
      refusal: null,
    },
    {
      name: "refuses a claim for another header than the authority's",
      check: (seen: Awaited<ReturnType<typeof evaluated>>) =>
        derive(seen, "00".repeat(seen.authority.headerHash.length / 2)),
      refusal: `${BINDING} (challenged header)`,
    },
  ])(
    "$name",
    async ({ check, refusal }) => {
      const seen = await evaluated();
      if (refusal === null) expect(() => check(seen)).not.toThrow();
      else expect(() => check(seen)).toThrow(refusal);
    },
    TIMEOUT,
  );
});
