/** The staging authority the prover funding permit admits our staging of
 * a staged builtin step's operands, and its spend back, by: bound to its
 * action, header and deployment, and carried alone, never beside another
 * funding authority (a fraud slash, a VerifySource, a thread move or a case
 * close). */
import { createHash } from "node:crypto";

import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
  type TxSigned,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { grantValidationDisputeFundingAuthority } from "../src/workflow/funding-reservation-permit.assert-validation-dispute-funding.js";
import { grantValidationDisputeCloseAuthority } from "../src/workflow/funding-reservation-permit.validation-dispute-close-authority.js";
import { grantValidationDisputeMoveAuthority } from "../src/workflow/funding-reservation-permit.validation-dispute-move-authority.js";
import {
  assertValidationDisputeStagingBinding,
  grantValidationDisputeStagingAuthority,
  VALIDATION_DISPUTE_STAGING_BINDING,
  type ValidationDisputeStagingAuthority,
  validationDisputeStagingFunding,
  type ValidationDisputeStagingShape,
} from "../src/workflow/funding-reservation-permit.validation-dispute-staging-authority.js";
import {
  ADA,
  NEXT,
  policy,
  reclaimShape,
  RESERVE,
  RUN,
  STAGED_OUT_REF,
  stagedOutput,
  stagingShape,
  theirs,
  THREAD_OUT_REF,
} from "./support/validation-trace-dispute-staging-funding.js";

const BESIDE =
  "funding transaction carries a validation dispute staging authority beside another funding authority";
const identity = { deploymentFingerprint: "fingerprint", headerHash: "header" };
const payer = generateEmulatorAccount({ lovelace: 100n * ADA });
const { kind: _kind, ...stagingRun } = stagingShape() as Extract<
  ValidationDisputeStagingShape,
  { kind: "stage_operands" }
>;

describe("the staging authority's binding", () => {
  const run = (datum: string) =>
    createHash("sha256").update(Buffer.from(datum, "hex")).digest("hex");
  const stageAction = {
    actionId: "validation-trace-stage:run:0",
    input: { stage: "stage_operands", run: run(RUN) },
  } as never;
  const reclaimAction = {
    actionId: `validation-trace-reclaim-staging:${STAGED_OUT_REF}`,
    input: { stage: "reclaim_staging", consumes: STAGED_OUT_REF },
  } as never;
  const authorityOf = (
    shape: ValidationDisputeStagingShape,
  ): ValidationDisputeStagingAuthority =>
    ({
      category: "validationTraceDispute",
      deploymentFingerprint: "fingerprint",
      headerHash: "header",
      shape,
    }) as never;
  const bind = (
    shape: ValidationDisputeStagingShape,
    action: never,
    {
      kind = shape.kind as string,
      headerHash = "header",
      fingerprint = "fingerprint",
    } = {},
  ) =>
    assertValidationDisputeStagingBinding({
      authority: authorityOf(shape),
      policy: {
        ...(policy as object),
        deploymentFingerprint: fingerprint,
      } as never,
      state: {
        category: "validationTraceDispute",
        currentActionKind: kind,
        currentActionDigest: computeDeploymentManifestJsonDigest(action),
      },
      action,
      headerHash,
    });

  it("binds a staging to its action's run, and a spend back to the output its action spends, under this header and deployment", () => {
    expect(bind(stagingShape(), stageAction)).toMatchObject({
      walletMoveReserveLovelace: RESERVE.toString(),
    });
    bind(reclaimShape, reclaimAction);
  });

  it("refuses a staging authority for another run, another action, header or deployment", () => {
    for (const [refused, names] of [
      [() => bind(stagingShape({ datumCborHex: NEXT }), stageAction), "run"],
      [
        () =>
          bind(stagingShape({ remainingDatumCborHexes: ["00"] }), stageAction),
        "run",
      ],
      [() => bind(stagingShape({ walletPaidHops: -1 }), stageAction), "run"],
      [() => bind(stagingShape(), reclaimAction), "action kind, run"],
      [
        () => bind(stagingShape(), stageAction, { headerHash: "other" }),
        "header hash",
      ],
      [
        () => bind(stagingShape(), stageAction, { fingerprint: "other" }),
        "deployment fingerprint",
      ],
      [
        () =>
          bind(
            { ...reclaimShape, outRef: `${"ab".repeat(32)}#1` } as never,
            reclaimAction,
          ),
        "staging output",
      ],
    ] as const)
      expect(refused).toThrow(
        `${VALIDATION_DISPUTE_STAGING_BINDING} (${names})`,
      );
  });
});

/** A signed transaction of ours granted a staging authority. */
const staged = async (): Promise<TxSigned> => {
  const lucid = await Lucid(new Emulator([payer]), "Custom");
  lucid.selectWallet.fromSeed(payer.seedPhrase);
  const signed = await (
    await lucid
      .newTx()
      .pay.ToAddress(theirs.address, { lovelace: 2n * ADA })
      .complete()
  ).sign
    .withWallet()
    .complete();
  grantValidationDisputeStagingAuthority({
    signed,
    ...identity,
    shape: stagingRun,
  });
  return signed;
};

describe("the prover funding permit over our staging beside another funding authority", () => {
  it.each([
    { name: "a fraud slash", slash: true, beside: () => {} },
    {
      name: "a VerifySource authority",
      slash: false,
      beside: (signed: TxSigned) =>
        grantValidationDisputeFundingAuthority({
          signed,
          ...identity,
          opener: stagedOutput,
          bond: {
            address: stagedOutput.address,
            unit: "",
            lovelace: 0n,
            datumCborHex: "",
          },
          caseOutput: {
            address: stagedOutput.address,
            unit: "",
            floorLovelace: 0n,
            lovelace: 0n,
          },
        }),
    },
    {
      name: "a thread move authority",
      slash: false,
      beside: (signed: TxSigned) =>
        grantValidationDisputeMoveAuthority({
          signed,
          kind: "timeout",
          ...identity,
          caseOpenerOutRef: THREAD_OUT_REF,
          defenderVkh: "00".repeat(28),
          thread: stagedOutput,
        }),
    },
    {
      name: "a case close authority",
      slash: false,
      beside: (signed: TxSigned) =>
        grantValidationDisputeCloseAuthority({
          signed,
          ...identity,
          caseOpenerOutRef: THREAD_OUT_REF,
          caseInput: stagedOutput,
          bond: stagedOutput,
        }),
    },
  ])("refuses a staging that also carries $name", async ({ slash, beside }) => {
    const signed = await staged();
    beside(signed);
    await expect(
      validationDisputeStagingFunding({
        signed,
        slash,
        policy: {} as never,
        state: {} as never,
        action: {} as never,
      }),
    ).rejects.toThrow(BESIDE);
  });

  it("passes a staging under its own authority alone on to its binding", async () => {
    await expect(
      validationDisputeStagingFunding({
        signed: await staged(),
        slash: false,
        policy: {} as never,
        state: {} as never,
        action: {} as never,
      }),
    ).rejects.not.toThrow(BESIDE);
  });
});
